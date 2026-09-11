library(openxlsx)

age_range_Ma <- c(0, 66)
source_dir <- "data_plant"
out_dir <- "output_plant"
template_files <- c(stomata="templates/stomata_IntermediateTemplate.xlsx",
                    konrad="templates/stomataKonrad_IntermediateTemplate.xlsx")
proxy <- "plant"
output_prefix <- "stomata(Konrad)?"
age_divisor <- 1
dir.create(out_dir, showWarnings=FALSE, recursive=TRUE)
source("generator_checks.R", local=TRUE)

clean <- function(x) {
  x <- trimws(as.character(x))
  x[tolower(x) %in% c("", "na", "n/a", "nan", "null", "not reported")] <- NA_character_
  x
}
norm <- function(x) tolower(gsub("[^[:alnum:]]", "", x))
number <- function(x) suppressWarnings(as.numeric(x))
same <- function(x, y) {
  a <- number(x); b <- number(y)
  (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) &
    (tolower(trimws(x)) == tolower(trimws(y)) | (is.finite(a) & is.finite(b) & abs(a-b) <= 1e-8*pmax(1,abs(a),abs(b)))))
}
join <- function(x) { x <- unique(clean(x)); x <- x[!is.na(x)]; if(length(x)) paste(x,collapse="; ") else NA_character_ }
study_id <- function(f) sub("_p[0-9.]+$", "", sub("^stomata-[^_]+_", "", tools::file_path_sans_ext(basename(f))))
method_id <- function(f) sub("^stomata-([^_]+)_.*", "\\1", basename(f))

templates <- lapply(template_files, function(f) {
  x <- read.xlsx(f, "data4PSM", colNames=FALSE, skipEmptyRows=FALSE, skipEmptyCols=FALSE)
  h <- which(vapply(x[[1]], function(v) identical(v, "sample"), logical(1)))
  if(length(h)!=1) stop("Cannot identify template headers: ",f)
  list(file=f, header=h, fields=as.character(unlist(x[h,],use.names=FALSE)),
       defaults=if(nrow(x)>h) clean(unlist(x[h+1,],use.names=FALSE)) else NULL)
})
base_names <- templates$stomata$fields
stopifnot(all(templates$konrad$fields %in% base_names))

extra <- c("method", "species", "formation", "stratigraphic_level", "location", "age_scale",
           "SD", "eSD", "SD_error_type", "SI", "eSI", "SI_error_type", "ED", "eED", "ED_error_type",
           "leaf_count", "calibration_species", "calibration_equation", "SR_standardization", "age_revision")
fields <- c(base_names, extra)
stopifnot(!anyDuplicated(fields))
map <- c(sample="^samplenamea?$", doi="^doi$", lat="^modernlatitude", lon="^modernlong[ti]*tude",
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
  sheets <- getSheetNames(f)
  wanted <- c(franks="leafgasexchangefranks", sd="stomataldensity", si="stomatalindex", sr="stomatalratio",
              `konrad-fom`="leafgasexchangefom", `konrad-rom`="leafgasexchangerom")
  method <- method_id(f)
  sheet <- which(norm(sheets) == wanted[method])
  if (!length(sheet) && method=="sd") sheet <- which(norm(sheets)%in%c("stomatalindex","sdsi"))
  if (length(sheet)!=1) stop("Unrecognized plant method sheet: ",f)
  sheet <- sheets[sheet]
  raw <- read.xlsx(f, sheet, colNames=FALSE, skipEmptyCols=FALSE, skipEmptyRows=FALSE)
  h <- which(apply(raw,1,function(x) any(norm(x)=="proxy",na.rm=TRUE)))
  if(length(h)!=1) stop("Cannot identify header: ",f)
  headers <- clean(unlist(raw[h,],use.names=FALSE)); nm <- norm(headers)
  rows <- which(seq_len(nrow(raw))>h & rowSums(!is.na(raw))>0)
  d <- as.data.frame(matrix(NA_character_,length(rows),length(fields)),stringsAsFactors=FALSE); names(d)<-fields
  for(k in names(map)) {
    cols <- which(grepl(map[[k]],nm))
    if(k=="doi" && length(cols)>1) cols<-cols[1]
    if(length(cols)>1 && k!="gen_notes") stop("Ambiguous header for ",k,": ",f)
    if(!length(cols))next
    if(k=="gen_notes") d[[k]]<-apply(raw[rows,cols,drop=FALSE],1,join) else d[[k]]<-clean(raw[rows,cols])
    if(k %in% c("eSD","eSI","eED")) d[[paste0(sub("^e","",k),"_error_type")]]<-headers[cols]
  }
  if(!any(grepl("^agema$",nm)))stop("Missing Age (Ma) header: ",f)
  if(!any(nm=="nameofpersonenteringproductdata"))stop("Expected a plant product workbook: ",f)
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
    if(!any(grepl("meanstomatal",nm)))stop("Missing measurement headers: ",f)
    ok<-which(is.finite(number(d$SD)))
    d$Dab[ok]<-as.character(number(d$SD[ok])*1e6)
    d$eDab[ok]<-as.character(number(d$eSD[ok])*1e6)
    for(i in ok) {
      d$N_eDab[i]<-join(c(d$SD_error_type[i],if(!is.na(d$leaf_count[i]))paste(d$leaf_count[i],"leaves")))
      d$gen_notes[i]<-join(c(d$gen_notes[i],"Stomatal density treated as abaxial; density and uncertainty converted from mm^-2 to m^-2."))
    }
  }
  if(method=="franks" && !any(nm=="dab"))stop("Missing Franks measurement headers: ",f)
  observed <- rowSums(as.data.frame(lapply(d[primary],function(x)is.finite(number(x)))))>0
  if(grepl("^konrad",method)) {
    physical <- c(Dab="^stomataldensitysd", GCLab="^stomatalporelength", GCWab="^stomatalporedepth", d13Cp="^d13cplantmaterial")
    scale <- c(Dab=1e6, GCLab=1e-6, GCWab=1e-6, d13Cp=1)
    defaults <- templates$konrad$defaults
    names(defaults) <- templates$konrad$fields
    fixed <- names(defaults)[!is.na(defaults) & !grepl("<|2 sigma uncertainty",defaults)]
    for(k in fixed) d[[k]] <- defaults[[k]]
    observed <- rep(FALSE,nrow(d))
    for(k in names(physical)) {
      co <- which(grepl(physical[[k]],nm))
      if(length(co)!=1 || co==length(nm) || nm[co+1]!="2suncertainty")stop("Unexpected Konrad measurement/error headers: ",f," / ",k)
      if((k=="Dab" && !grepl("1mm2",nm[co])) || (k%in%c("GCLab","GCWab") && !grepl("µm|μm",nm[co])))stop("Unexpected Konrad units: ",f," / ",headers[co])
      observed <- observed | is.finite(number(clean(raw[rows,co])))
      d[[k]] <- as.character(number(clean(raw[rows,co]))*scale[[k]])
      d[[paste0("e",k)]] <- as.character(number(clean(raw[rows,co+1]))*scale[[k]]/2)
      d[[paste0("N_e",k)]] <- ifelse(is.na(d[[paste0("e",k)]]),NA,"1 sigma; source 2 sigma divided by 2")
    }
    for(i in seq_len(nrow(d))) {
      d$sample[i] <- join(c(d$sample[i],d$location[i],d$formation[i],d$stratigraphic_level[i],d$species[i]))
      d$gen_notes[i] <- join(c(d$gen_notes[i],"Pore length stored in GCLab (s1=1); pore depth stored in GCWab (s2=1). Density converted to m^-2; lengths to m; measured 2 sigma errors converted to 1 sigma."))
    }
    person <- which(nm=="nameofpersonenteringproductdata")
    email <- which(nm=="emailofpersonenteringproductdata")
    d$name_person <- clean(raw[rows,person]);d$email_person <- clean(raw[rows,email])
  }
  d$.file<-basename(f);d$.row<-rows;d$.sheet<-sheet;d$.study<-study_id(f)
  d$.observed <- observed
  d$.template<-if(grepl("^konrad",method))"konrad" else "stomata"
  d
}

# Match within a study, using taxonomy and measurements to resolve repeated sample names.
candidates <- function(row, d) {
  ok <- rep(TRUE,nrow(d))
  for(k in c("doi", identity)) {
    a<-row[[k]];b<-d[[k]]
    if(!is.na(a)) ok<-ok & (is.na(b)|same(a,b))
  }
  if(!is.na(row$sample)) ok<-ok & !is.na(d$sample) & same(row$sample,d$sample)
  else {
    evidence<-rep(0L,nrow(d))
    for(k in c("family","genus","species","formation")) if(!is.na(row[[k]])) evidence<-evidence+(!is.na(d[[k]]) & same(row[[k]],d[[k]]))
    ok<-ok & evidence>=2
  }
  for(k in c("age_mean","stratigraphic_level")) {
    if(!is.na(row[[k]]))ok<-ok & (is.na(d[[k]])|same(row[[k]],d[[k]]))
  }
  has_measurement<-function(x) rowSums(as.data.frame(lapply(x[primary],function(v) is.finite(number(v)))))>0
  if(has_measurement(row))ok<-ok & has_measurement(d)
  else return(integer())
  ix<-which(ok)
  {
    for(k in measure) {
      a<-row[[k]];b<-d[[k]][ix]
      if(!is.na(a))ix<-ix[is.na(b)|same(a,b)]
    }
  }
  if(length(ix)>1) {
    for(k in c("calibration_species","calibration_equation","SR_standardization")) {
      if(!is.na(row[[k]])) {
        exact<-!is.na(d[[k]][ix]) & same(row[[k]],d[[k]][ix])
        ix<-if(any(exact))ix[exact] else ix[is.na(d[[k]][ix])]
      }
    }
  }
  ix
}
src_files<-list.files(source_dir,pattern="\\.xlsx$",full.names=TRUE)
src_files<-src_files[!grepl("^~\\$",basename(src_files))]
if(!length(src_files))stop("No plant product workbooks")
keys <- paste(vapply(src_files,method_id,character(1)),vapply(src_files,study_id,character(1)))
if(anyDuplicated(keys))stop("Keep only one product version per method/study in data_plant")
dat<-do.call(rbind,lapply(src_files,read_plant))
rownames(dat)<-NULL
message(sum(!dat$.observed)," CO2-only product rows excluded (no fossil measurements)")

eligible<-rep(FALSE,nrow(dat))
for(st in unique(dat$.study)) {
  ids<-which(dat$.study==st)
  priority<-if("franks"%in%dat$method[ids])c("franks","sd") else if(any(dat$method[ids]%in%c("sd","si","sr")))c("sd","si","sr") else c("konrad-fom","konrad-rom")
  for(i in ids[!dat$method[ids]%in%priority]) {
    sf<-dat$.file[i];report_action("method",dat$.row[i],dat$method[i],"Not selected by study method priority")
  }
  eligible[ids]<-dat$method[ids]%in%priority & dat$.observed[ids]
}
for(sf in unique(dat$.file[eligible])) {
  ix<-which(dat$.file==sf & eligible)
  eligible[ix]<-select_age(dat$age_mean[ix],dat$.row[ix])
}
result<-dat[FALSE,];membership<-integer(nrow(dat))
ambiguous <- data.frame(file_1=character(), source_row_1=integer(), file_2=character(), source_row_2=integer(), sample=character(), status=character())
for(st in unique(dat$.study)) {
  ids<-which(dat$.study==st & eligible)
  ids<-ids[order(match(dat$method[ids],c("franks","sd","si","sr","konrad-fom","konrad-rom")))]
  for(i in ids) {
    row<-dat[i,];sf<-row$.file
    ix<-which(result$.study==st)
    match_rows<-candidates(row,result[ix,,drop=FALSE])
    ix<-ix[match_rows]
    # A shared ID with different measurements is a distinct record.
    if(length(ix)==1) {
      j<-ix
      for(k in fields) {
        if(is.na(result[[k]][j]) && !is.na(row[[k]])) {result[[k]][j]<-row[[k]]}
      }
      result$gen_notes[j]<-join(c(result$gen_notes[j],row$gen_notes))
      membership[i]<-j
      report_action("sample",row$.row,row$sample,"Matching measurements merged; lower-priority data fill blanks; source rows recorded in plant_duplicate_candidates.csv")
    } else {
      result<-rbind(result,row);membership[i]<-nrow(result)
      if(length(ix)>1) ambiguous <- rbind(ambiguous, data.frame(
        file_1=result$.file[ix], source_row_1=result$.row[ix], file_2=row$.file,
        source_row_2=row$.row, sample=row$sample,
        status="ambiguous: both retained; human review needed"))
    }
  }
}

duplicates<-data.frame(removed_file=character(),removed_row=integer(),retained_file=character(),retained_row=integer(),sample=character())
removed<-integer()
for(i in seq_len(nrow(result))) {
  if(is.na(result$sample[i]))next
  possible<-which(result$.study!=result$.study[i] & !is.na(result$sample) & same(result$sample[i],result$sample))
  possible<-setdiff(possible,removed)
  for(j in possible) {
    if(is.na(result$gen_notes[i]) || is.na(result$doi[j]) || !grepl(result$doi[j],result$gen_notes[i],fixed=TRUE))next
    if(result$.template[i]!=result$.template[j])next
    keys<-c("family","genus","species","age_mean",measure)
    if(!all(same(unlist(result[i,keys]),unlist(result[j,keys]))))next
    removed<-c(removed,i);membership[membership==i]<-j
    duplicates<-rbind(duplicates,data.frame(removed_file=result$.file[i],removed_row=result$.row[i],retained_file=result$.file[j],retained_row=result$.row[j],sample=result$sample[i]))
    result$gen_notes[j]<-join(c(result$gen_notes[j],result$gen_notes[i]))
    sf<-result$.file[i];report_action("duplicate",result$.row[i],result$sample[i],paste("Repeated measurements; original source retained:",result$.file[j],result$.row[j]))
    break
  }
}
keep<-setdiff(seq_len(nrow(result)),removed)
membership<-match(membership,keep,nomatch=0)
result<-result[keep,,drop=FALSE]


numeric_fields<-c("lat","lon","age_mean","age_2s","age_min","age_max",setdiff(franks, c(grep("^N_",franks,value=TRUE),"fixed_A")),
                  "SD","eSD","SI","eSI","ED","eED")
for(sf in unique(dat$.file)) {
  ix<-which(dat$.file==sf & eligible & dat$method!="franks" & is.finite(number(dat$Dab)))
  report_action("Dab",dat$.row[ix],dat$Dab[ix],"SD and error converted to m^-2; assumed abaxial surface recorded in notes")
}
for(sf in unique(result$.file)) {
  ix<-which(result$.file==sf)
  for(k in numeric_fields) {
    bad<-ix[!is.na(result[[k]][ix]) & !is.finite(number(result[[k]][ix]))]
    if(length(bad)) {
      report_action(k,result$.row[bad],result[[k]][bad],"Text moved to notes; numeric cell left blank")
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
      report_issue(paste0("e",k),result$.row[bad],NA_character_,"Measurement present; uncertainty missing in product")
    }
  }
  x<-result[ix,base_names];names(x)<-int2col(1:63)
  check_output(x,result$.row[ix],intersect(int2col(which(base_names %in% numeric_fields)),names(x)),character())
}
problems<-problems[problems$problem!="No primary proxy measurement mapped",]
for(i in seq_len(nrow(result))) {
  sf<-result$.file[i]
  if(all(is.na(result[i,c("Dab","Dad","GCLab","GCLad","GCWab","GCWad","d13Cp")])))report_issue("measurement",result$.row[i],NA_character_,"No primary proxy measurement mapped")
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
    x$age_mean[!normal] <- NA_real_
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
candidates <- ambiguous
for(j in seq_len(nrow(result))) {
  ids <- which(membership==j)
  other <- ids[!(dat$.file[ids]==result$.file[j] & dat$.row[ids]==result$.row[j])]
  if(length(other)) candidates <- rbind(candidates, data.frame(
    file_1=result$.file[j], source_row_1=result$.row[j], file_2=dat$.file[other],
    source_row_2=dat$.row[other], sample=dat$sample[other],
    status="record 1 retained; record 2 removed/merged"))
}
write_duplicates(unique(candidates))
finish_reports()
message(nrow(result)," plant intermediate rows from ",length(unique(result$.study))," studies")
