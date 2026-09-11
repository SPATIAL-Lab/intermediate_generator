library(openxlsx)

age_range_Ma <- c(0, 541)
proxy <- output_prefix <- "boron"
age_divisor <- 1000
out_dir <- "output_boron"
template_file <- "templates/boron_IntermediateTemplate.xlsx"
template_sheet <- "data4PSM"
source("generator_checks.R", local=TRUE)
dir.create(out_dir,showWarnings=FALSE)
src_files <- source_files(proxy)
generated_data <- metadata <- list()
as_chr <- text_value

# Replicate means and keys for repeated measurements.
replicates <- function(x, key=FALSE) {
  x <- sub("[[:space:]]*\\(.*$","",text_value(x))
  values <- num(strsplit(ifelse(is.na(x),"",x),",")[[1]])
  if(!length(values) || anyNA(values))return(if(key)NA_character_ else NA_real_)
  if(key)paste(sort(values),collapse=",") else mean(values)
}

# Map the selected product, or the archive when no product exists.
for(sf in src_files) {
  message("Reading: ",basename(sf))
  dat <- read_source(sf)
  source_rows <- attr(dat,"rows"); source_sheet <- attr(dat,"sheet")
  age_raw <- num(field(dat,"^2sageuncertaintyposka$",-2))
  age <- num(field(dat,"^ageka$"))
  age[is.na(age)] <- age_raw[is.na(age)]
  keep <- select_rows(dat,age,source_rows)
  dat <- dat[keep,,drop=FALSE];source_rows<-source_rows[keep];age<-age[keep]/1000
  if(!nrow(dat))next
  n <- nrow(dat)
  get <- function(pattern,offset=0)field(dat,pattern,offset)
  val <- function(pattern,offset=0) {
    raw <- get(pattern,offset); x <- num(raw)
    bad <- which(!is.na(raw)&is.na(x))
    report_issue(pattern,source_rows[bad],raw[bad],"Non-numeric measurement; left blank")
    x
  }
  ids <- lapply(c("^sitedsdp","^hole$","^core$","^section$","^topcm$","^bottomcm$"),get)
  sample <- apply(as.data.frame(ids),1,function(x)if(all(is.na(x)))NA_character_ else paste(ifelse(is.na(x),"",x),collapse="_"))
  ap <- num(get("^2sageuncertaintyposka$"))/1000
  an <- num(get("^2sageuncertaintynegka$"))/1000
  at <- get("^2sageuncertaintyposka$",-1)
  if(grepl("^product_",dirname(sf))) {
    sp <- num(get("^ageuncertaintyposka$"))/1000; sn <- num(get("^ageuncertaintynegka$"))/1000
    changed <- which((!is.na(sp)&(is.na(ap)|abs(sp-ap)>1e-8)) | (!is.na(sn)&(is.na(an)|abs(sn-an)>1e-8)))
    at[changed] <- NA_character_
    ap[!is.na(sp)]<-sp[!is.na(sp)];an[!is.na(sn)]<-sn[!is.na(sn)]
    # Unchanged detailed errors retain their explicit 2s convention.
    blank <- is.na(at)&!seq_len(n)%in%changed
    at[blank]<-"2sd"
  } else at[is.na(at)]<-"2sd"
  at <- product_age_type(at,ap,an,grepl("^product_",dirname(sf)))
  age_unc <- error_2s(age,ap,an,at,"age uncertainty")
  d11B <- val("^d11Baverageofreplicates$")
  d11B_unc <- val("^d11Baverageofreplicates$",2)
  boron_unc <- error_2s(d11B,d11B_unc,type=get("^d11Baverageofreplicates$",1),field_name="d11B uncertainty",default="2sd")
  internal <- num(get("^2seofreplicates"))
  bad <- which(internal>boron_unc$error)
  report_issue("d11B uncertainty",source_rows[bad],d11B_unc[bad],"Reported uncertainty smaller than replicate 2SE")
  mg_raw <- get("^mgcammolmolaverageofreplicates$")
  mg <- num(mg_raw); reps_raw <- get("^mgcaofindividualreplicate")
  reps <- vapply(reps_raw,replicates,numeric(1)); use<-is.na(mg)&!is.na(reps);mg[use]<-reps[use]
  mp <- num(get("^2smguncertaintypos")); mn <- num(get("^2smgcauncertaintyneg"))
  mt <- get("^mgcammolmolaverageofreplicates$",1)
  supplied <- ifelse(is.na(mp),mn,mp)
  symmetric <- which(grepl("setvalue.*3",header_key(mt)) & xor(is.na(mp),is.na(mn)) & abs(supplied-.03*mg)<1e-8)
  mp[symmetric]<-mn[symmetric]<-supplied[symmetric]
  mg_unc <- error_2s(mg,mp,mn,mt,"Mg/Ca uncertainty",default="2sd")
  for(label in c("d11B", "Mg/Ca")) {
    bounds <- if(label=="d11B")boron_unc else mg_unc
    rows <- which(!is.na(bounds$min) | !is.na(bounds$max))
    report_issue(paste(label,"uncertainty"),source_rows[rows],
      paste(bounds$min[rows],bounds$max[rows],sep="; "),
      "Reported range cannot be mapped to the template error field without a distribution assumption; left blank")
  }
  temp <- val("^temperaturec$")
  temp_unc <- error_2s(temp,val("^2stemperatureposc$"),val("^2stemperaturenegc$"),get("^temperaturec$",1),"temperature uncertainty",default="2sd")
  # Compare raw Mg/Ca where an alternate scenario repeats the same sample.
  alt_tabs <- grep("alt.*scenario",getSheetNames(sf),value=TRUE,ignore.case=TRUE)
  sample_key <- function(d)do.call(paste,c(lapply(c("^sitedsdp","^coredepthbelowseafloor",
    "^(taxonanalyzed|foraminiferspeciesanalyzed)$","^d11baverageofreplicates$"),function(p)field(d,p)),sep="|"))
  for(tab in alt_tabs) {
    alt <- read_source(sf,tabs=tab)
    akey <- sample_key(alt); key <- sample_key(dat)
    match_row <- match(key,akey)
    unique_key <- !duplicated(akey) & !duplicated(akey,fromLast=TRUE)
    alt_mg <- num(field(alt,"^mgcammolmolaverageofreplicates$"))[match_row]
    bad <- which(unique_key[match_row] & !is.na(mg) & !is.na(alt_mg) & abs(mg-alt_mg)>1e-8)
    report_issue("Mg/Ca",source_rows[bad],paste(mg[bad],alt_mg[bad],sep="; "),
      paste0("Main and alternate-scenario tabs disagree for the same sample; main value retained. Alternate tab: ",tab))
  }
  notes <- apply(dat[,grep("^additionalnotes",names(dat)),drop=FALSE],1,join_notes)
  for(i in seq_len(n)) {
    text <- c(mg_raw[i],get("^2smgcauncertaintyneg")[i],reps_raw[i])
    text <- text[!is.na(text)&is.na(num(text))]
    notes[i] <- join_notes(notes[i],if(length(text))paste0("Mg/Ca: ",text),
      if(i%in%symmetric)"Mg/Ca error treated as symmetric using the stated +/-3%.")
  }
  out_df <- data.frame(A=sample,B=get("^doi$"),C=basename(sf),D="Harper and Giulivi",
    E="dustin.t.harper@utah.edu; claudiag@ldeo.columbia.edu",
    F=val("^modernlatitude"),G=val("^modernlongitude"),H=age,I=age_unc$error,J=age_unc$min,K=age_unc$max,
    L=mapply(join_notes,get("^referencetoagemodel"),get("^specifyreasonforagerevision$")),
    M=get("^(foraminiferspeciesanalyzed|taxonanalyzed)$"),N=get("^(foraminifersizeclass|samplesizeclass)"),O=get("^(numberofforaminiferashells|numberofspecimenspersample)"),
    P=get("^analyticalmethod"),Q=d11B,R=boron_unc$error,S=mg,T=mg_unc$error,
    U=temp,V=temp_unc$error,W=temp_unc$min,X=temp_unc$max,Y=notes)
  file_id <- sub("^boron_isotopes_","",source_key(sf))
  study_outputs[basename(sf)] <- file.path(out_dir,paste0("boron_Intermediate_",file_id,".xlsx"))
  generated_data[[basename(sf)]] <- out_df
  metadata[[basename(sf)]] <- data.frame(file=basename(sf),sheet=source_sheet,source_row=source_rows,
    publication_year=num(get("^publicationyear$")),sample=sample,site=get("^sitedsdp"),
    depth_m=num(get("^coredepthbelowseafloor")),composite_depth_m=num(get("^corecompositedepthbelowseafloor")),
    lat=out_df$F,lon=out_df$G,age_Ma=age,species=out_df$M,
    d11B_replicates=vapply(get("^d11Bofindividualreplicate"),replicates,character(1),key=TRUE),
    MgCa_replicates=vapply(reps_raw,replicates,character(1),key=TRUE),replicate_2SE=internal)
}

# Resolve repeated samples and write individual and combined sheets.
if(length(generated_data)) {
  d <- do.call(rbind,generated_data); m <- do.call(rbind,metadata)
  key <- function(x)gsub("[^a-z0-9_]","",sub("^(dsdp|odp|iodp)[[:space:]]*","",tolower(as_chr(x))))
  samples <- key(m$sample); sites <- key(m$site)
  identity_pool <- function(j) {
    id <- !is.na(samples[j]) & !is.na(samples) & samples==samples[j]
    depth <- !is.na(sites[j]) & !is.na(sites) & sites==sites[j] & abs(m$depth_m-m$depth_m[j])<=1e-6
    site <- abs(m$lat-m$lat[j])<=.01 & abs(m$lon-m$lon[j])<=.01
    which(site & (id | depth))
  }
  identity_match <- function(i,j)TRUE
  extra <- c("depth_m","composite_depth_m","d11B_replicates","MgCa_replicates","replicate_2SE")
  d <- cbind(d,m[extra])
  resolved <- resolve_duplicates(d,m,identity_match,c("Q","S","U"),
    c("H","I","J","K",LETTERS[13:24],extra),c("L","Y"),
    order(m$publication_year,m$file,m$source_row,na.last=TRUE),identity_pool)
  write_resolved(resolved$data[,setdiff(names(d),extra)],m,resolved$keep,c("Q","S","U"))
  write_duplicates(resolved$report)
}
finish_reports()
