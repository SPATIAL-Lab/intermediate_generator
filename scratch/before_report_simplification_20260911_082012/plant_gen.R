library(openxlsx)

age_range_Ma <- c(0, 23.03)
source_dir <- "data_plant"
product_dir <- "product_plant"
out_dir <- "output_plant"
template_file <- "templates/stomata_IntermediateTemplate.xlsx"
template_sheet <- "data4PSM"
proxy <- "plant"
output_prefix <- "stomata"
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

base_names <- as.character(read.xlsx(template_file, template_sheet, rows=4, colNames=FALSE)[1,1:63])
extra <- c("method", "species", "formation", "stratigraphic_level", "age_scale", "SD", "eSD", "SD_error_type",
           "SI", "eSI", "SI_error_type", "ED", "eED", "ED_error_type", "stomata_count", "epidermal_count",
           "leaf_count", "counts_per_leaf", "counting_method", "counting_area", "calibration_species",
           "calibration_equation", "calibration_error", "NLE_SD_SI", "SR_standardization",
           "d13Ca", "ed13Ca", "N_ed13Ca", "fixed_A", "b", "gamma", "temp",
           "published_CO2", "published_CO2_low", "published_CO2_high", "CO2_type", "CO2_range_type",
           "CO2_distribution", "product_sheet", "product_category", "product_category_reason",
           "age_revision", "CO2_revision", "age_quarantined", "age_superseded_by", "CO2_superseded_by")
fields <- c(base_names, extra)
stopifnot(!anyDuplicated(fields))
map <- c(sample="^samplename$", doi="^doi$", lat="^modernlatitude", lon="^modernlong[ti]*tude",
         age_mean="^agema$", age_min="^ageuncertaintyyoungma$", age_max="^ageuncertaintyoldma$",
         age_notes="^howwasagedetermined", family="^family$", genus="^genus$", species="^species$",
         formation="^geologicformation$", stratigraphic_level="^stratigraphiclevel$", age_scale="^agescale",
         gen_notes="^(generalnotes|notes|remarks|comments)$",
         SD="^(sample)?meanstomataldensity", eSD="^sderror", SI="^(sample)?meanstomatalindex", eSI="^sierror",
         ED="^meanepidermaldensity", eED="^ederror", stomata_count="^stomata$", epidermal_count="^epidermalcells$",
         leaf_count="^numberofleaves", counts_per_leaf="^numberofstomatalcounts", counting_method="^countingmethod",
         counting_area="^countingbox", calibration_species="^(moderncalibrationspecies|nearestlivingequivalentspecies)$",
         calibration_equation="^moderncalibrationregressionequation", calibration_error="^calibrationerror",
         NLE_SD_SI="^nlesdsivalue$", SR_standardization="^standardization",
         published_CO2="^(estimatedatmosphericco2concentrationppm|reportedmeanco2(ppm)?)$",
         published_CO2_low="^(co2lowppm|reportedco2uncertaintylow)$",
         published_CO2_high="^(co2highppm|reportedco2uncertaintyhigh)$", CO2_type="^co2type$",
         CO2_range_type="^(whatistheco2range|whatistheuncertaintyrange)", CO2_distribution="^whatisthedistribution",
         product_category="^proxycategory", product_category_reason="^specifyreasonforchoice",
         age_revision="^specifyreasonforagerevision", CO2_revision="^specifyreasonforco2revision",
         age_quarantined="^agedataquarantined", age_superseded_by="^agedatasuperseded", CO2_superseded_by="^co2datasuperseded")
franks <- c(base_names[16:62], "d13Ca", "ed13Ca", "N_ed13Ca", "fixed_A", "b", "gamma", "temp")
map <- c(map, setNames(paste0("^",norm(franks),"$"),franks))
primary <- c("Dab", "Dad", "GCLab", "GCLad", "GCWab", "GCWad", "d13Cp", "SD", "SI", "ED")
measure <- c(primary, "eDab", "eDad", "eGCLab", "eGCLad", "eGCWab", "eGCWad", "ed13Cp", "eSD", "eSI", "eED")
identity <- c("sample", "family", "genus", "species", "formation")
comparison <- data.frame(file=character(), source_row=integer(), product=character(), product_row=integer(),
                         field=character(), archive_value=character(), product_value=character(), action=character())

read_plant <- function(f, product=FALSE) {
  sheets <- getSheetNames(f)
  wanted <- c(franks="leafgasexchangefranks", sd="stomataldensity", si="stomatalindex", sr="stomatalratio")
  method <- method_id(f)
  sheet <- which(norm(sheets) == wanted[method])
  if (!length(sheet) && basename(f)%in%c("stomata-sd_liang_2022a_p1.0.xlsx","stomata-sd_stults_2011_p1.0.xlsx","stomata-sr_steinthorsdottir_2021_p1.0.xlsx")) sheet <- which(norm(sheets)%in%c("stomatalindex","sdsi"))
  if (length(sheet)!=1) stop("Unrecognized plant method sheet: ",f)
  sheet <- sheets[sheet]
  raw <- read.xlsx(f, sheet, colNames=FALSE, skipEmptyCols=FALSE, skipEmptyRows=FALSE)
  h <- which(apply(raw,1,function(x) any(norm(x)=="proxy",na.rm=TRUE)))
  if(length(h)!=1) stop("Cannot identify header: ",f)
  headers <- clean(unlist(raw[h,],use.names=FALSE)); nm <- norm(headers)
  rows <- which(seq_len(nrow(raw))>h & rowSums(!is.na(raw))>0)
  d <- as.data.frame(matrix(NA_character_,length(rows),length(fields)),stringsAsFactors=FALSE); names(d)<-fields
  origins <- d
  for(k in names(map)) {
    cols <- which(grepl(map[[k]],nm))
    if(k=="doi" && length(cols)>1) cols<-cols[1]
    if(length(cols)>1 && k!="gen_notes") stop("Ambiguous header for ",k,": ",f)
    if(!length(cols))next
    if(k=="gen_notes") d[[k]]<-apply(raw[rows,cols,drop=FALSE],1,join) else d[[k]]<-clean(raw[rows,cols])
    origins[[k]]<-ifelse(is.na(d[[k]]),NA,paste0(basename(f),"#",sheet,"!",int2col(cols[1]),rows))
    if(k %in% c("eSD","eSI","eED")) d[[paste0(sub("^e","",k),"_error_type")]]<-headers[cols]
  }
  if(!any(grepl("^agema$",nm)))stop("Missing Age (Ma) header: ",f)
  if(!any(grepl(if(method=="franks") "^dab$" else "meanstomatal",nm)))stop("Missing measurement headers: ",f)
  d$method<-method
  archive_name<-sub("_p[0-9.]+\\.xlsx$",".xlsx",basename(f))
  d$archive_sheet<-clean(archive_links$url[match(archive_name,archive_links$file)])
  d$product_sheet<-if(product) paste0("https://www.ncei.noaa.gov/pub/data/paleo/climate_forcing/trace_gases/Paleo-pCO2/product_files/",basename(f)) else NA_character_
  d$name_person<-"Harper and Giulivi";d$email_person<-"dustin.t.harper@utah.edu; claudiag@ldeo.columbia.edu"
  d$plant_grp<-vapply(seq_len(nrow(d)),function(i) {x<-c(d$family[i],d$genus[i]); x<-x[!is.na(x)]; if(length(x))paste(x,collapse=" / ") else NA_character_},character(1))
  for(k in c("eSD","eSI","eED")) {
    explicit<-which(grepl("^[0-9.]+[[:space:];]*(sd|s[.]d[.]|sem|s[.]e[.]m[.])$",d[[k]],ignore.case=TRUE))
    if(length(explicit)) {
      d[[paste0(sub("^e","",k),"_error_type")]][explicit]<-ifelse(grepl("sem|s[.]e",d[[k]][explicit],ignore.case=TRUE),"1 s.e.m.","1 s.d.")
      d[[k]][explicit]<-sub("[[:space:];]*[sS].*$","",d[[k]][explicit])
    }
  }
  if(method!="franks") {
    ok<-which(is.finite(number(d$SD)))
    d$Dab[ok]<-as.character(number(d$SD[ok])*1e6)
    d$eDab[ok]<-as.character(number(d$eSD[ok])*1e6)
    origins$Dab[ok]<-origins$SD[ok];origins$eDab[ok]<-origins$eSD[ok]
    for(i in ok) {
      d$N_eDab[i]<-join(c(d$SD_error_type[i],if(!is.na(d$leaf_count[i]))paste(d$leaf_count[i],"leaves")))
      origins$N_eDab[i]<-join(c(origins$SD[i],origins$leaf_count[i]))
      d$gen_notes[i]<-join(c(d$gen_notes[i],"Stomatal density treated as abaxial; density and uncertainty converted from mm^-2 to m^-2."))
    }
  }
  d$.file<-basename(f);d$.row<-rows;d$.sheet<-sheet;d$.study<-study_id(f)
  list(data=d,origins=origins)
}

# Match within a study, using taxonomy and measurements to resolve repeated sample names.
candidates <- function(row, d, product=FALSE) {
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
  if(!product) for(k in c("age_mean","stratigraphic_level")) {
    if(!is.na(row[[k]]))ok<-ok & (is.na(d[[k]])|same(row[[k]],d[[k]]))
  }
  has_measurement<-function(x) rowSums(as.data.frame(lapply(x[primary],function(v) is.finite(number(v)))))>0
  if(has_measurement(row))ok<-ok & has_measurement(d)
  else if(!product)return(integer())
  ix<-which(ok)
  if(length(ix)>1 || !product) {
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
record_change <- function(a,p,k,action) {
  comparison <<- rbind(comparison,data.frame(file=a$.file,source_row=a$.row,product=p$.file,product_row=p$.row,
    field=k,archive_value=as.character(a[[k]]),product_value=as.character(p[[k]]),action=action))
}

src_files<-list.files(source_dir,pattern="\\.xlsx$",full.names=TRUE)
src_files<-src_files[!grepl("^~\\$",basename(src_files))]
if(!length(src_files))stop("No plant source workbooks")
if(!file.exists(file.path(product_dir,"archive_links.csv")))stop("Run refresh_plant_products.R first")
archive_links<-read.csv(file.path(product_dir,"archive_links.csv"),stringsAsFactors=FALSE)
if(!all(basename(src_files)%in%archive_links$file))stop("New source files: run refresh_plant_products.R first")
manifest_file<-file.path(product_dir,"manifest.csv")
if(!file.exists(manifest_file))stop("Run refresh_plant_products.R first")
manifest<-read.csv(manifest_file,stringsAsFactors=FALSE)
if(nrow(manifest)) {
  hashes<-unname(tools::md5sum(file.path(product_dir,manifest$file)))
  if(anyNA(hashes)||any(hashes!=manifest$md5))stop("Plant product cache changed; refresh manifest before running")
}
all_data<-list();all_origins<-list();coverage<-list()
for(sf in src_files) {
  if(!method_id(sf)%in%c("franks","sd","si","sr")) {
    report_action("method",NA_integer_,method_id(sf),"Outside Franks/SD/SI/SR priority; not generated")
    next
  }
  a<-read_plant(sf);d<-a$data;o<-a$origins
  products<-manifest$file[sub("_p[0-9.]+\\.xlsx$",".xlsx",manifest$file)==basename(sf)]
  coverage[[length(coverage)+1]]<-data.frame(file=basename(sf),product=join(products),status=if(length(products))"compared" else "no corresponding product in cached NOAA inventory")
  if(length(products)>1)stop("Multiple cached versions: ",sf)
  if(length(products)) {
    p<-read_plant(file.path(product_dir,products),TRUE)
    used<-integer()
    for(i in seq_len(nrow(d))) {
      ix<-candidates(d[i,],p$data,TRUE)
      if(length(ix)!=1 || ix%in%used) {
        report_action("product match",d$.row[i],join(p$data$.row[ix]),"No unique product match; archive retained")
        next
      }
      j<-ix;used<-c(used,j);pr<-p$data[j,];d$product_sheet[i]<-pr$product_sheet
      for(k in c(base_names, "species", "formation", "stratigraphic_level", "age_scale", "age_revision")) {
        if(k %in% c("archive_sheet","name_person","email_person","plant_grp","method") || is.na(pr[[k]]) || same(d[[k]][i],pr[[k]]))next
        age_revision<-grepl("^age_|^stratigraphic_level$",k) && !is.na(pr$age_revision)
        use<-is.na(d[[k]][i]) || age_revision || grepl("^(product_|age_revision|CO2_revision|age_quarantined|age_superseded|CO2_superseded)",k)
        action<-if(use)"product addition/revision used" else "unexplained difference; archive retained"
        record_change(d[i,],pr,k,action)
        if(k=="gen_notes") {d[[k]][i]<-join(c(d[[k]][i],pr[[k]]));o[[k]][i]<-join(c(o[[k]][i],p$origins[[k]][j]));next}
        if(use) {d[[k]][i]<-pr[[k]];o[[k]][i]<-p$origins[[k]][j]}
      }
    }
    new<-setdiff(seq_len(nrow(p$data)),used)
    for(j in new) {
      pr<-p$data[j,]
      if(all(is.na(pr[primary]))) {report_action("product row",NA_integer_,paste(products,pr$.row),"CO2 summary without fossil measurements; not added as a sample");next}
      ix<-candidates(pr,d,TRUE)
      if(length(ix)) {report_issue("product match",d$.row[ix],paste(products,pr$.row),"Ambiguous product sample match; product row not added");next}
      d<-rbind(d,pr);o<-rbind(o,p$origins[j,])
      report_action("product row",NA_integer_,paste(products,pr$.row),"Additional measured product sample retained")
    }
  }
  all_data[[length(all_data)+1]]<-d;all_origins[[length(all_origins)+1]]<-o
}
# Include extra product methods only for studies present in the source folder.
studies<-unique(vapply(src_files,study_id,character(1)))
for(pf in manifest$file) {
  if(!study_id(pf)%in%studies || sub("_p[0-9.]+\\.xlsx$",".xlsx",pf)%in%basename(src_files))next
  sf<-pf;p<-read_plant(file.path(product_dir,pf),TRUE)
  all_data[[length(all_data)+1]]<-p$data;all_origins[[length(all_origins)+1]]<-p$origins
}
dat<-do.call(rbind,all_data);origins<-do.call(rbind,all_origins)
rownames(dat)<-rownames(origins)<-NULL

eligible<-rep(FALSE,nrow(dat))
for(st in unique(dat$.study)) {
  ids<-which(dat$.study==st)
  priority<-if("franks"%in%dat$method[ids])c("franks","sd") else c("sd","si","sr")
  for(i in ids[!dat$method[ids]%in%priority]) {
    sf<-dat$.file[i];report_action("method",dat$.row[i],dat$method[i],"Not selected by study method priority")
  }
  eligible[ids]<-dat$method[ids]%in%priority
}
for(sf in unique(dat$.file[eligible])) {
  ix<-which(dat$.file==sf & eligible)
  eligible[ix]<-select_age(dat$age_mean[ix],dat$.row[ix])
}
# Report unresolved product differences only for records in the selected age range/methods.
for(i in which(comparison$action=="unexplained difference; archive retained" & comparison$field!="gen_notes")) {
  z<-comparison[i,];ix<-which(dat$.file==z$file & dat$.row==z$source_row & eligible)
  if(length(ix)) {sf<-z$file;report_issue(z$field,z$source_row,paste(z$archive_value,"vs",z$product_value,"in",z$product,z$product_row),"Unexplained product difference; archive retained")}
}

method_data<-dat[eligible,,drop=FALSE]
result<-dat[FALSE,];result_origins<-origins[FALSE,];membership<-integer(nrow(dat))
for(st in unique(dat$.study)) {
  ids<-which(dat$.study==st & eligible)
  ids<-ids[order(match(dat$method[ids],c("franks","sd","si","sr")))]
  for(i in ids) {
    row<-dat[i,];sf<-row$.file
    ix<-which(result$.study==st)
    match_rows<-candidates(row,result[ix,,drop=FALSE])
    ix<-ix[match_rows]
    # A shared ID with different measurements is a distinct record.
    if(length(ix)==1) {
      j<-ix
      for(k in fields) {
        if(is.na(result[[k]][j]) && !is.na(row[[k]])) {result[[k]][j]<-row[[k]];result_origins[[k]][j]<-origins[[k]][i]}
      }
      result$gen_notes[j]<-join(c(result$gen_notes[j],row$gen_notes))
      result_origins$gen_notes[j]<-join(c(result_origins$gen_notes[j],origins$gen_notes[i]))
      membership[i]<-j
      report_action("sample",row$.row,row$sample,"Matching measurements merged; lower-priority data fill blanks; source rows recorded in plant_record_metadata.csv")
    } else {
      result<-rbind(result,row);result_origins<-rbind(result_origins,origins[i,]);membership[i]<-nrow(result)
      if(length(ix)>1)report_issue("sample match",row$.row,row$sample,"Multiple possible sample matches; record retained separately")
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
    keys<-c("family","genus","species","age_mean",measure)
    if(!all(same(unlist(result[i,keys]),unlist(result[j,keys]))))next
    removed<-c(removed,i);membership[membership==i]<-j
    duplicates<-rbind(duplicates,data.frame(removed_file=result$.file[i],removed_row=result$.row[i],retained_file=result$.file[j],retained_row=result$.row[j],sample=result$sample[i]))
    result$gen_notes[j]<-join(c(result$gen_notes[j],result$gen_notes[i]))
    result_origins$gen_notes[j]<-join(c(result_origins$gen_notes[j],result_origins$gen_notes[i]))
    sf<-result$.file[i];report_action("duplicate",result$.row[i],result$sample[i],paste("Repeated measurements; original source retained:",result$.file[j],result$.row[j]))
    break
  }
}
keep<-setdiff(seq_len(nrow(result)),removed)
membership<-match(membership,keep,nomatch=0)
result<-result[keep,,drop=FALSE];result_origins<-result_origins[keep,,drop=FALSE]
write.csv(duplicates,file.path(out_dir,"plant_duplicate_removals.csv"),row.names=FALSE,na="")

numeric_fields<-c("lat","lon","age_mean","age_2s","age_min","age_max",setdiff(franks, c(grep("^N_",franks,value=TRUE),"fixed_A")),
                  "SD","eSD","SI","eSI","ED","eED","stomata_count","epidermal_count","published_CO2","published_CO2_low","published_CO2_high")
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
  x<-result[ix,base_names];names(x)<-int2col(1:63)
  check_output(x,result$.row[ix],intersect(int2col(which(base_names %in% numeric_fields)),names(x)),character())
}
# Additional method measurements also satisfy the primary-measurement check.
problems<-problems[problems$problem!="No primary proxy measurement mapped",]
for(i in seq_len(nrow(result))) {
  sf<-result$.file[i]
  if(all(is.na(result[i,c("Dab","Dad","GCLab","GCLad","GCWab","GCWad","d13Cp")])))report_issue("measurement",result$.row[i],NA_character_,"No primary proxy measurement mapped")
}
for(k in numeric_fields)result[[k]]<-number(result[[k]])

write_book <- function(x, file) {
  wb<-loadWorkbook(template_file)
  writeData(wb,template_sheet,t(rep("",63)),startRow=1,colNames=FALSE)
  writeData(wb,template_sheet,x[,base_names,drop=FALSE],startRow=5,colNames=FALSE,keepNA=FALSE)
  saveWorkbook(wb,file,overwrite=TRUE)
}
for(st in unique(result$.study)) {
  ix<-which(result$.study==st)
  out_file<-file.path(out_dir,paste0("stomata_Intermediate_",st,".xlsx"))
  write_book(result[ix,,drop=FALSE],out_file)
  current_outputs<-c(current_outputs,out_file)
  files<-unique(dat$.file[dat$.study==st]);study_outputs[files]<-out_file
}
combined<-file.path(out_dir,"stomata_Intermediate_combined.xlsx")
write_book(result,combined);current_outputs<-c(current_outputs,combined)
for(sf in unique(comparison$file)) {
  changes<-comparison[comparison$file==sf & comparison$action=="product addition/revision used",]
  for(r in unique(changes$source_row))report_action("product update",r,join(changes$field[changes$source_row==r]),"Documented product additions/revisions used; values recorded in plant_product_comparison.csv")
}
write.csv(comparison,file.path(out_dir,"plant_product_comparison.csv"),row.names=FALSE,na="")
write.csv(do.call(rbind,coverage),file.path(out_dir,"plant_product_coverage.csv"),row.names=FALSE,na="")
metadata<-dat[,c(".file",".sheet",".row",".study","method","sample","age_mean")]
metadata$combined_row<-ifelse(membership>0,membership+4L,NA_integer_)
metadata$included<-eligible
write.csv(metadata,file.path(out_dir,"plant_record_metadata.csv"),row.names=FALSE,na="")
provenance<-data.frame(combined_row=integer(),field=character(),source=character())
for(j in seq_len(nrow(result))) {
  present<-which(!is.na(result_origins[j,1:63]))
  provenance<-rbind(provenance,data.frame(combined_row=j+4L,field=fields[present],source=unlist(result_origins[j,present],use.names=FALSE)))
}
provenance$conversion<-ifelse(provenance$field%in%c("Dab","eDab") & grepl("^stomata-(sd|si|sr)_",provenance$source),"mm^-2 to m^-2: multiply by 1e6",NA_character_)
write.csv(provenance,file.path(out_dir,"plant_value_sources.csv"),row.names=FALSE,na="")
finish_reports()
message(nrow(result)," plant intermediate rows from ",length(unique(result$.study))," studies")
