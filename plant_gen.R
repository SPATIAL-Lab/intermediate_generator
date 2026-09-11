library(openxlsx)

age_range_Ma <- c(0, 541)
out_dir <- "output_plant"
template_files <- c(stomata="templates/stomata_IntermediateTemplate.xlsx",
                    konrad="templates/stomataKonrad_IntermediateTemplate.xlsx")
proxy <- "plant"
output_prefix <- "stomata(Konrad)?"
age_divisor <- 1
dir.create(out_dir, showWarnings=FALSE, recursive=TRUE)
source("generator_checks.R", local=TRUE)

clean <- text_value
norm <- header_key
number <- num
same <- function(x, y) {
  a <- number(x); b <- number(y)
  (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) &
    (tolower(trimws(x)) == tolower(trimws(y)) | (is.finite(a) & is.finite(b) & abs(a-b) <= 1e-8*pmax(1,abs(a),abs(b)))))
}
join <- function(x) { x <- unique(clean(x)); x <- x[!is.na(x)]; if(length(x)) paste(x,collapse="; ") else NA_character_ }
study_id <- function(f) sub("_p[0-9.]+$", "", sub("^stomata-[^_]+_", "", source_key(f)))
method_id <- function(f) sub("^stomata-([^_]+)_.*", "\\1", basename(f))

templates <- lapply(template_files, function(f) {
  x <- read.xlsx(f, "data4PSM", colNames=FALSE, skipEmptyRows=FALSE, skipEmptyCols=FALSE)
  h <- which(vapply(x[[1]], function(v) identical(v, "sample"), logical(1)))
  if(length(h)!=1) stop("Cannot identify template headers: ",f)
  list(file=f, header=h, fields=as.character(unlist(x[h,],use.names=FALSE)),
       defaults=if(nrow(x)>h) clean(unlist(x[h+1,],use.names=FALSE)) else NULL)
})
base_names <- templates$stomata$fields

extra <- c("method", "publication_year", "species", "formation", "stratigraphic_level", "location", "age_scale",
           "SD", "eSD", "SD_error_type", "SI", "eSI", "SI_error_type", "ED", "eED", "ED_error_type",
           "leaf_count", "calibration_species", "calibration_equation", "SR_standardization", "age_revision")
fields <- c(base_names, extra)
map <- c(publication_year="^publicationyear$", sample="^samplenamea?$", doi="^doi$", lat="^modernlatitude", lon="^modernlong[ti]*tude",
         age_mean="^agema$", age_min="^ageuncertaintyyoungma$", age_max="^ageuncertaintyoldma$",
         age_notes="^howwasagedetermined", family="^family$", genus="^genus$", species="^species$",
         formation="^geologicformation$", stratigraphic_level="^stratigraphiclevel$", location="^location$",
         age_scale="^agescale", age_revision="^specifyreasonforagerevision",
         gen_notes="^(generalnotes|notes|remarks|comments)$",
         SD="^(sample)?meanstomataldensity", eSD="^sderror", SI="^(sample)?meanstomatalindex", eSI="^sierror",
         ED="^meanepidermaldensity", eED="^ederror", leaf_count="^numberofleaves",
         calibration_species="^(moderncalibrationspecies|nearestlivingequivalentspecies)$",
         calibration_equation="^moderncalibrationregressionequation", SR_standardization="^standardization")
franks <- base_names[16:62]
map <- c(map, setNames(paste0("^",norm(franks),"$"),franks))

primary <- c("Dab", "Dad", "GCLab", "GCLad", "GCWab", "GCWad", "d13Cp", "SD", "SI", "ED")
measure <- c(primary, "eDab", "eDad", "eGCLab", "eGCLad", "eGCWab", "eGCWad", "ed13Cp", "eSD", "eSI", "eED")
identity <- c("sample", "family", "genus", "species", "formation")

read_plant <- function(f) {
  raw <- read_source(f)
  source_rows <- attr(raw,"rows"); sheet <- attr(raw,"sheet")
  headers <- attr(raw,"headers")
  keep <- usable_rows(raw)
  raw <- raw[keep,,drop=FALSE];source_rows<-source_rows[keep]
  if(!nrow(raw))return(NULL)
  nm <- names(raw); rows <- seq_len(nrow(raw))
  method <- method_id(f)
  d <- as.data.frame(matrix(NA_character_,length(rows),length(fields)),stringsAsFactors=FALSE); names(d)<-fields
  for(k in names(map)) {
    cols <- which(grepl(map[[k]],nm))
    if(k=="doi" && length(cols)>1) cols<-cols[1]
    if(k!="gen_notes" && length(cols)>1)cols<-cols[1]
    if(!length(cols))next
    if(k=="gen_notes") d[[k]]<-apply(raw[rows,cols,drop=FALSE],1,join) else d[[k]]<-clean(raw[rows,cols])
    if(k %in% c("eSD","eSI","eED")) d[[paste0(sub("^e","",k),"_error_type")]]<-headers[cols]
  }
  loose <- which(is.na(nm) | nm=="")
  if(length(loose)) {
    notes <- raw[,loose,drop=FALSE]
    notes[] <- lapply(notes,function(x)ifelse(is.na(number(x)),clean(x),NA_character_))
    d$gen_notes <- mapply(function(a,b)join(c(a,b)),d$gen_notes,apply(notes,1,join))
  }
  for(i in which(tolower(field(raw,"^fixeda$"))=="yes"))
    d$gen_notes[i] <- join(c(d$gen_notes[i],"Source used fixed photosynthesis (fixed_A=yes); A0 is the reported net photosynthetic rate."))
  age_errors <- match(c("ageuncertaintyposka", "ageuncertaintynegka"),nm)
  if(!anyNA(age_errors)) {
    age <- number(d$age_mean); old <- number(d$age_max); young <- number(d$age_min)
    pos <- number(raw[rows,age_errors[1]])/1000
    neg <- number(raw[rows,age_errors[2]])/1000
    offsets <- which((age < young | age > old | young > old) & old >= 0 & young >= 0 &
                     abs(old-pos)<1e-8 & abs(young-neg)<1e-8)
    d$age_max[offsets] <- as.character(age[offsets]+old[offsets])
    d$age_min[offsets] <- as.character(age[offsets]-young[offsets])
    for(i in offsets) d$age_notes[i] <- join(c(d$age_notes[i],
      "Age uncertainty offsets corroborated by product ka errors; converted to absolute Ma bounds."))
  }
  age_assumed <- rep(FALSE,nrow(d))
  if(grepl("^product_",dirname(f))) {
    revised_age <- num(field(raw,"^ageka$"))/1000
    use <- !is.na(revised_age)
    d$age_mean[use] <- as.character(revised_age[use])
    for(i in which(use))d$age_notes[i] <- join(c(d$age_notes[i],field(raw,"^specifyreasonforagerevision$")[i]))
    pos<-num(field(raw,"^ageuncertaintyposka$"))/1000
    neg<-num(field(raw,"^ageuncertaintynegka$"))/1000
    bounds<-!is.na(d$age_min)|!is.na(d$age_max)
    d$age_min[bounds & !is.na(neg)]<-as.character(num(d$age_mean[bounds & !is.na(neg)])-neg[bounds & !is.na(neg)])
    d$age_max[bounds & !is.na(pos)]<-as.character(num(d$age_mean[bounds & !is.na(pos)])+pos[bounds & !is.na(pos)])
    age_assumed<-!bounds & (!is.na(pos)|!is.na(neg))
    d$age_2s[age_assumed]<-as.character((pos[age_assumed]+neg[age_assumed])/2)

  }
  d$method<-method
  d$archive_sheet<-basename(f)
  d$name_person<-"Harper and Giulivi";d$email_person<-"dustin.t.harper@utah.edu; claudiag@ldeo.columbia.edu"
  d$plant_grp<-vapply(seq_len(nrow(d)),function(i) {x<-c(d$family[i],d$genus[i]); x<-x[!is.na(x)]; if(length(x))paste(x,collapse=" / ") else NA_character_},character(1))
  for(k in c("eSD","eSI","eED")) {
    explicit<-which(grepl("^[0-9.]+[[:space:];]*(sd|s[.]d[.]|sem|s[.]e[.]m[.])$",d[[k]],ignore.case=TRUE))
    if(length(explicit)) {
      d[[paste0(sub("^e","",k),"_error_type")]][explicit]<-ifelse(grepl("sem|s[.]e",d[[k]][explicit],ignore.case=TRUE),"1 s.e.m.","1 s.d.")
      d[[k]][explicit]<-sub("[[:space:];]*[sS].*$","",d[[k]][explicit])
    }
  }
  if(method %in% c("sd","si","sr")) {
    ok<-which(is.finite(number(d$SD)))
    d$Dab[ok]<-as.character(number(d$SD[ok])*1e6)
    d$eDab[ok]<-as.character(number(d$eSD[ok])*1e6)
    for(i in ok) {
      d$N_eDab[i]<-join(c(d$SD_error_type[i],if(!is.na(d$leaf_count[i]))paste(d$leaf_count[i],"leaves")))
      d$gen_notes[i]<-join(c(d$gen_notes[i],"Stomatal density treated as abaxial; density and uncertainty converted from mm^-2 to m^-2."))
    }
  }
  observed <- rowSums(as.data.frame(lapply(d[primary],function(x)is.finite(number(x)))))>0
  if(grepl("^konrad",method)) {
    physical <- c(Dab="^stomataldensitysd", GCLab="^stomatalporelength", GCWab="^stomatalporedepth", d13Cp="^d13cplantmaterial")
    scale <- c(Dab=1e6, GCLab=1e-6, GCWab=1e-6, d13Cp=1)
    defaults <- templates$konrad$defaults
    names(defaults) <- templates$konrad$fields
    fixed <- names(defaults)[!is.na(defaults) & !grepl("<|2 sigma uncertainty",defaults)]
    source_group <- d$plant_grp
    fixed <- setdiff(fixed,"gen_notes")
    for(k in fixed) d[[k]] <- defaults[[k]]
    other_model <- grepl("does not use.*Konrad|different.*gas.exchange",d$gen_notes,ignore.case=TRUE)
    d$plant_grp[other_model] <- source_group[other_model]
    observed <- rep(FALSE,nrow(d))
    for(k in names(physical)) {
      co <- which(grepl(physical[[k]],nm))
      if(!length(co))next
      co <- co[1]
      observed <- observed | is.finite(number(clean(raw[rows,co])))
      d[[k]] <- as.character(number(clean(raw[rows,co]))*scale[[k]])
      d[[paste0("e",k)]] <- as.character(number(clean(raw[rows,co+1]))*scale[[k]]/2)
      d[[paste0("N_e",k)]] <- ifelse(is.na(d[[paste0("e",k)]]),NA,"1 sigma; source 2 sigma divided by 2")
    }
    for(i in seq_len(nrow(d))) {
      d$sample[i] <- join(c(d$sample[i],d$location[i],d$formation[i],d$stratigraphic_level[i],d$species[i]))
      d$gen_notes[i] <- join(c(d$gen_notes[i],"Pore length stored in GCLab (s1=1); pore depth stored in GCWab (s2=1). Density converted to m^-2; lengths to m; measured 2 sigma errors converted to 1 sigma."))
    }
    person <- field(raw,"^nameofpersonenteringproductdata$")
    email <- field(raw,"^emailofpersonenteringproductdata$")
    d$name_person[!is.na(person)] <- person[!is.na(person)]
    d$email_person[!is.na(email)] <- email[!is.na(email)]
  }
  d$.file<-basename(f);d$.row<-source_rows;d$.sheet<-sheet;d$.study<-study_id(f)
  d$.age_assumed <- age_assumed
  d$.observed <- observed
  d$.template<-if(grepl("^konrad",method))"konrad" else "stomata"
  d
}

# Select sources, then apply the method hierarchy within each study.
src_files <- source_files(proxy)
dat<-do.call(rbind,lapply(src_files,read_plant))
rownames(dat)<-NULL
message(sum(!dat$.observed)," CO2-only rows excluded (no fossil measurements)")

eligible<-rep(FALSE,nrow(dat))
for(st in unique(dat$.study)) {
  ids<-which(dat$.study==st)
  methods<-method_id(src_files[study_id(src_files)==st])
  priority<-if("franks"%in%methods)c("franks","sd") else if(any(methods%in%c("sd","si","sr")))c("sd","si","sr") else c("konrad-fom","konrad-rom")
  eligible[ids]<-dat$method[ids]%in%priority & dat$.observed[ids]
}
for(sf in unique(dat$.file[eligible])) {
  ix<-which(dat$.file==sf & eligible)
  eligible[ix]<-select_age(dat$age_mean[ix],dat$.row[ix])
}
d <- dat[eligible,,drop=FALSE]
m <- data.frame(file=d$.file,source_row=d$.row,sample=d$sample,study=d$.study,method=d$method)
identity_match <- function(i,j) {
  if(d$.template[i]!=d$.template[j])return(FALSE)
  same <- function(a,b)!is.na(a) && !is.na(b) && isTRUE(same_value(a,b))
  id <- same(d$sample[i],d$sample[j])
  if(d$.study[i]!=d$.study[j]) {
    reference <- (!is.na(d$gen_notes[i]) && !is.na(d$doi[j]) && grepl(d$doi[j],d$gen_notes[i],fixed=TRUE)) ||
                 (!is.na(d$gen_notes[j]) && !is.na(d$doi[i]) && grepl(d$doi[i],d$gen_notes[j],fixed=TRUE))
    return(id && reference)
  }
  if(id)return(TRUE)
  if(!is.na(d$sample[i]) && !is.na(d$sample[j]))return(FALSE)
  sum(vapply(c("family","genus","species","formation"),function(k)same(d[[k]][i],d[[k]][j]),logical(1)))>=2
}
same_value <- same
year <- number(d$publication_year)
rank <- match(d$method,c("franks","sd","si","sr","konrad-fom","konrad-rom"))
compared <- setdiff(fields,c("sample","doi","archive_sheet","name_person","email_person","gen_notes","age_notes",
                            "method","publication_year","age_revision","calibration_species","calibration_equation","SR_standardization","SD_error_type","SI_error_type","ED_error_type",grep("^N_",fields,value=TRUE)))
identity_pool <- function(j)which(d$.study==d$.study[j] | (!is.na(d$sample[j]) & d$sample==d$sample[j]))
resolved <- resolve_duplicates(d[,fields],m,identity_match,primary,compared,c("gen_notes","age_notes"),
                               order(year,d$.study,rank,d$.file,d$.row,na.last=TRUE),identity_pool)
result <- d[resolved$keep,,drop=FALSE]
result[,fields] <- resolved$data[resolved$keep,fields]

numeric_fields<-c("lat","lon","age_mean","age_2s","age_min","age_max",setdiff(franks, c(grep("^N_",franks,value=TRUE),"fixed_A")),
                  "SD","eSD","SI","eSI","ED","eED")
for(sf in unique(result$.file)) {
  ix<-which(result$.file==sf)
  assumed<-ix[result$.age_assumed[ix]]
  report_issue("age uncertainty",result$.row[assumed],result$age_2s[assumed],
               "Assumed 2 sigma; product age uncertainty type not stated")
  for(k in numeric_fields) {
    bad<-ix[!is.na(result[[k]][ix]) & !is.finite(number(result[[k]][ix]))]
    if(length(bad)) {
      for(j in bad)result$gen_notes[j]<-join(c(result$gen_notes[j],paste0(k,": ",result[[k]][j])))
      result[[k]][bad]<-NA_character_
    }
  }
  for(k in c("SD","SI","ED")) {
    type<-norm(result[[paste0(k,"_error_type")]][ix]);value<-result[[paste0("e",k)]][ix]
    bad<-ix[!is.na(value) & !grepl("1(sem|sd|standarddeviation)",type)]
    report_issue(paste0(k," uncertainty"),result$.row[bad],result[[paste0(k,"_error_type")]][bad],"Uncertainty type needs review; raw magnitude retained")
  }
  if(result$.template[ix[1]]=="konrad") {
    for(k in c("Dab","GCLab","GCWab","d13Cp")) {
      bad <- ix[!is.na(result[[k]][ix]) & is.na(result[[paste0("e",k)]][ix])]
      report_issue(paste0("e",k),result$.row[bad],NA_character_,"Measurement present; uncertainty missing in source")
    }
  }
  x<-result[ix,base_names];names(x)<-int2col(1:63)
  check_output(x,result$.row[ix],intersect(int2col(which(base_names %in% numeric_fields)),names(x)),character())
}
problems<-problems[problems$problem!="No primary proxy measurement mapped",]
for(i in seq_len(nrow(result))) {
  sf<-result$.file[i]
  if(!is.na(number(result$SI[i])) && (number(result$SI[i])<0 || number(result$SI[i])>100))
    report_issue("stomatal index",result$.row[i],result$SI[i],"Source SI outside 0–100%; check SD/SI column assignment")
}
for(k in numeric_fields)result[[k]]<-number(result[[k]])

write_book <- function(x, file, kind) {
  template <- templates[[kind]]
  wb<-loadWorkbook(template$file)
  if(kind=="stomata")writeData(wb,"data4PSM",t(rep("",length(template$fields))),startRow=1,colNames=FALSE)
  else writeData(wb,"data4PSM",t(rep("",length(template$fields))),startRow=template$header+1,colNames=FALSE)
  if(kind=="konrad") {
    normal <- grepl("grein_2011",x$.file)
    if(any(normal) && !all(same(x$age_mean[normal]-x$age_min[normal],x$age_max[normal]-x$age_mean[normal])))stop("Grein age bounds are not symmetric")
    x$age_2s[normal] <- x$age_mean[normal]-x$age_min[normal]
    x$age_mean[!normal & !is.na(x$age_min) & !is.na(x$age_max)] <- NA_real_
    x$age_min[normal] <- x$age_max[normal] <- NA_real_
  }
  writeData(wb,"data4PSM",x[,template$fields,drop=FALSE],startRow=template$header+1,colNames=FALSE,keepNA=FALSE)
  saveWorkbook(wb,file,overwrite=TRUE)
}
for(kind in c("stomata","konrad")) {
  prefix <- if(kind=="konrad")"stomataKonrad" else "stomata"
  for(st in unique(result$.study[result$.template==kind])) {
    ix<-which(result$.study==st & result$.template==kind)
    out_file<-file.path(out_dir,paste0(prefix,"_Intermediate_",st,".xlsx"))
    write_book(result[ix,,drop=FALSE],out_file,kind)
    current_outputs<-c(current_outputs,out_file)
  }
  ix <- which(result$.template==kind)
  if(length(ix)) {
    combined<-file.path(out_dir,paste0(prefix,"_Intermediate_combined.xlsx"))
    write_book(result[ix,,drop=FALSE],combined,kind)
    current_outputs<-c(current_outputs,combined)
  }
}
write_duplicates(resolved$report)
finish_reports()
message(nrow(result)," plant intermediate rows from ",length(unique(result$.study))," studies")
