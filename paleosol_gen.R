library(openxlsx)

age_range_Ma <- c(0, 541)
proxy <- output_prefix <- "paleosol"
age_divisor <- 1
out_dir <- "output_paleosol"
template_file <- "templates/paleosol_IntermediateTemplate.xlsx"
template_sheet <- "data4PSM"
source("generator_checks.R",local=TRUE)
dir.create(out_dir,showWarnings=FALSE)
src_files <- source_files(proxy)
generated_data <- screen_data <- list()

# Match the measurement headers shared by the old and new layouts.
map <- c(A="^sampleid$",B="^doi$",F="^(modern|site)latitude",G="^(modern|site)longitude",
  M="^paleosolnumber$",N="^soiltexture",O="^soilhorizonfororganicmatter",P="^organicmattertype$",
  Q="^d13ccc(pdb)?$",R="^d13cccuncertainty",S="^d13coccludedom(pdb)?$",T="^d13coccludedomuncertainty",
  U="^d13cbulkpaleosolom(pdb)?$",V="^d13cbulkpaleosolomuncertainty",W="^d13cenamel(pdb)?$",X="^d13cenameluncertainty",
  Y="^d18occ(pdb)?$",Z="^d18occuncertainty",AA="^temperatureofcalciumcarbonateformation",
  AC="^meanannualprecipitation",AE="^notes$")
numeric_cols <- c("F","G","H","I","J","K",LETTERS[17:26],"AA","AB","AC","AD")

for(sf in src_files) {
  message("Reading: ",basename(sf))
  dat <- read_source(sf);source_rows<-attr(dat,"rows");source_sheet<-attr(dat,"sheet")
  age <- num(field(dat,"^age(ma)?$"))
  summary_age <- num(field(dat,"^ageka$"))/1000
  product <- grepl("^product_",dirname(sf))
  if(product)age[!is.na(summary_age)]<-summary_age[!is.na(summary_age)]
  keep <- select_rows(dat,age,source_rows)
  dat<-dat[keep,,drop=FALSE];source_rows<-source_rows[keep];age<-age[keep]
  if(!nrow(dat))next
  n<-nrow(dat); get<-function(pattern,offset=0)field(dat,pattern,offset)
  out_df <- as.data.frame(matrix(NA_character_,n,31));names(out_df)<-int2col(1:31)
  for(co in names(map))out_df[[co]]<-get(map[[co]])
  out_df$C<-basename(sf);out_df$D<-"Harper and Giulivi"
  out_df$E<-"dustin.t.harper@utah.edu; claudiag@ldeo.columbia.edu"
  out_df$H<-age
  out_df$L<-mapply(join_notes,get("^(referencesforage|notesonagedetermination)$"),get("^specifyreasonforagerevision$"))
  lo<-num(get("^agemin(ma)?$"));hi<-num(get("^agemax(ma)?$"))
  reversed<-which(lo>hi & age>=hi & age<=lo)
  if(length(reversed)) {
    old<-lo[reversed];lo[reversed]<-hi[reversed];hi[reversed]<-old
    for(i in reversed)out_df$L[i]<-join_notes(out_df$L[i],"Reversed age bounds put in younger-to-older order.")
  }
  ap<-num(get("^ageuncertaintypositive"));an<-num(get("^ageuncertaintynegative"))
  at<-get("^typeofuncertaintyonage")
  if(product) {
    sp<-num(get("^ageuncertaintyposka$"))/1000;sn<-num(get("^ageuncertaintynegka$"))/1000
    # Older products store absolute age bounds; shift them with a revised central age.
    has_bounds<-!is.na(lo)|!is.na(hi)
    lo[has_bounds & !is.na(sn)]<-age[has_bounds & !is.na(sn)]-sn[has_bounds & !is.na(sn)]
    hi[has_bounds & !is.na(sp)]<-age[has_bounds & !is.na(sp)]+sp[has_bounds & !is.na(sp)]
    use<-!has_bounds & (!is.na(sp)|!is.na(sn))
    ap[use]<-sp[use];an[use]<-sn[use]
  }
  at<-product_age_type(at,ap,an,product)
  unc<-error_2s(age,ap,an,at,"age uncertainty")
  out_df$I<-unc$error;out_df$J<-ifelse(is.na(lo),unc$min,lo);out_df$K<-ifelse(is.na(hi),unc$max,hi)
  # Respired carbon is the fallback when neither organic pool was measured.
  use<-is.na(out_df$S)&is.na(out_df$U) & any(grepl("^otherd13crapproach$",names(dat)))
  out_df$U[use]<-get("^d13cr(pdb)?$")[use]
  out_df$V[use]<-get("^d13cruncertainty")[use]
  out_df$AE<-mapply(join_notes,out_df$AE,get("^additionalpublicationsthatarepartofthedataset$"))
  for(co in c("AB","AD")) {
    prefix<-if(co=="AB")"temperature" else "map"
    pos<-grep(paste0("^",prefix,"uncertainty"),names(dat))[1]
    value<-if(is.na(pos))rep(NA_real_,n) else num(sub("^[[:space:]]*(±|\\+/-)[[:space:]]*","",dat[[pos]]))
    explicit<-!is.na(pos) && grepl("2s",names(dat)[pos])
    type<-if(explicit)rep("2sd",n) else get(paste0("^",prefix,"uncertainty"),2)
    if(!explicit && (is.na(pos) || !grepl("typeof.*uncertainty",names(dat)[pos+2])))type<-rep(NA_character_,n)
    other<-num(sub("^[[:space:]]*(±|\\+/-)[[:space:]]*","",get(paste0("^other",prefix,"uncertainty"))))
    use<-is.na(value)&!is.na(other)
    value[use]<-other[use];type[use]<-get(paste0("^other",prefix,"uncertainty"),1)[use]
    out_df[[co]]<-error_2s(num(out_df[[if(co=="AB")"AA" else "AC"]]),value,type=type,field_name=paste(prefix,"uncertainty"))$error
  }
  for(co in numeric_cols) {
    raw<-text_value(out_df[[co]]);value<-num(raw);bad<-which(!is.na(raw)&is.na(value))
    report_issue(co,source_rows[bad],raw[bad],"Non-numeric measurement; left blank")
    out_df[[co]]<-value
  }
  file_id<-sub("^paleosol_","",source_key(sf))
  study_outputs[basename(sf)]<-file.path(out_dir,paste0("paleosol_Intermediate_",file_id,".xlsx"))
  generated_data[[basename(sf)]]<-out_df
  screen_data[[basename(sf)]]<-data.frame(file=basename(sf),sheet=source_sheet,source_row=source_rows,
    publication_year=num(get("^publicationyear$")),sample=out_df$A,lat=out_df$F,lon=out_df$G,age_Ma=age,
    formation=get("^rockformationname$"),depth_m=num(get("^stratigraphiclevel")))
}

# Apply the shared duplicate rules before writing.
if(length(generated_data)) {
  d<-do.call(rbind,generated_data);m<-do.call(rbind,screen_data)
  identity_match<-function(i,j) {
    close<-function(a,b,tol=1e-6)!is.na(a)&&!is.na(b)&&abs(a-b)<=tol
    same<-function(a,b)!is.na(a)&&!is.na(b)&&tolower(trimws(a))==tolower(trimws(b))
    site<-close(m$lat[i],m$lat[j],.01)&&close(m$lon[i],m$lon[j],.01)
    site && (same(m$sample[i],m$sample[j]) || (same(m$formation[i],m$formation[j])&&close(m$depth_m[i],m$depth_m[j])))
  }
  d<-cbind(d,m[c("formation","depth_m")])
  resolved<-resolve_duplicates(d,m,identity_match,c("Q","S","U","W","Y"),
    c("F","G","H","I","J","K",LETTERS[13:26],"AA","AB","AC","AD","formation","depth_m"),c("L","AE"),
    order(m$publication_year,m$file,m$source_row,na.last=TRUE))
  write_resolved(resolved$data[,int2col(1:31)],m,resolved$keep,c("Q","S","U","W","Y"))
  write_duplicates(resolved$report)
}
finish_reports()
