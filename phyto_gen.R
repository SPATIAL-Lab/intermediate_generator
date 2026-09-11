library(openxlsx)

age_range_Ma <- c(0, 541)
proxy <- output_prefix <- "phyto"
age_divisor <- 1000
out_dir <- "output_phyto"
template_file <- "templates/phyto_IntermediateTemplate.xlsx"
template_sheet <- "data4PSM"
source("generator_checks.R",local=TRUE)
dir.create(out_dir,showWarnings=FALSE)
src_files <- source_files(proxy)
generated_data <- metadata <- list()
as_chr <- text_value

# Published fields and the revised inputs supplied in product sheets.
inputs <- list(
  organic=c("organicd13cmeasuredpermil","organicd13cmeasureduncertaintypermil","organicd13cmeasureduncertaintypermil","organicd13cmeasureduncertaintytype"),
  temp=c("sstdegc","sstdegcuncertaintypos","sstdegcuncertaintyneg","sstdegcuncertaintytype"),
  phosphate=c("phosphateumolkg","phosphateumolkguncertaintypos","phosphateumolkguncertaintyneg","phosphateunertaintytype"),
  lith=c("vsalithsizeum","vsalithsizeuncertaintyposum","vsalithsizeuncertaintynegum","vsalithsizeuncertaintytype"))
revised <- list(
  organic=c("organicd13cmeasuredpermiluse","organicd13cmeasureduncertaintypermiluse","organicd13cmeasureduncertaintypermiluse","organicd13cmeasureduncertaintypermiltypeuse"),
  temp=c("sstdegcuse","sstdegcuncertaintyuse","sstdegcuncertaintyuse","sstdegcuncertaintytypeuse"),
  phosphate=c("phosphateumolkguse","phosphateumolkguncertaintyuse","phosphateumolkguncertaintyuse","phosphateumolkguncertaintytypeuse"),
  lith=c("vsalithsizeumuse","vsalithsizeuncertaintyumuse","vsalithsizeuncertaintyumuse","vsalithsizeuncertaintyumtypeuse"))

for(sf in src_files) {
  message("Reading: ",basename(sf))
  dat <- read_source(sf);source_rows<-attr(dat,"rows");source_sheet<-attr(dat,"sheet")
  age <- num(field(dat,"^agekabp$"))
  keep <- select_rows(dat,age,source_rows)
  dat<-dat[keep,,drop=FALSE];source_rows<-source_rows[keep];age<-age[keep]/1000
  if(!nrow(dat))next
  n<-nrow(dat);product<-grepl("^product_",dirname(sf))
  get<-function(name)field(dat,paste0("^",name,"$"))
  value<-function(x,field_name) {
    raw<-text_value(x);raw<-sub(" +note:.*$","",raw,ignore.case=TRUE)
    x<-num(raw);bad<-which(!is.na(raw)&is.na(x))
    report_issue(field_name,source_rows[bad],raw[bad],"Non-numeric measurement; left blank")
    x
  }
  groups<-list();used<-list()
  for(k in names(inputs)) {
    v<-lapply(inputs[[k]],get); published<-v
    use<-if(product)!is.na(get(revised[[k]][1])) else rep(FALSE,n)
    for(j in 1:4)v[[j]][use]<-get(revised[[k]][j])[use]
    if(k=="temp") {
      alt<-is.na(v[[1]]) & !is.na(get("sstotherdegc"))
      for(j in 1:4)v[[j]][alt]<-get(c("sstotherdegc","sstotherdegcuncertaintypos","sstotherdegcuncertaintyneg","sstotherdegcuncertaintytype")[j])[alt]
    }
    if(k=="organic") {
      misplaced<-is.na(v[[4]]) & grepl("^[12](std|sem|sd|se)$",header_key(get("organicd13cmeasureduncertaintyn")))
      v[[4]][misplaced]<-get("organicd13cmeasureduncertaintyn")[misplaced]
    }
    if(k=="temp" && grepl("rae_2021",sf))v[[4]][which(header_key(v[[4]])=="5to975percentile")]<-"2.5 to 97.5 percentile"
    unclear <- integer()
    if(k=="lith") {
      normal <- which(use & header_key(v[[4]])=="normal")
      same_error <- abs(num(v[[2]])-num(published[[2]]))<1e-8 &
        abs(num(v[[3]])-num(published[[3]]))<1e-8
      labelled <- grepl("^[12](sd|std|se|sem|sigma)$",header_key(published[[4]]))
      carry <- intersect(normal,which(same_error & labelled))
      v[[4]][carry] <- published[[4]][carry]
      unclear <- setdiff(normal,carry)
    }
    mean<-value(v[[1]],k)
    unc<-error_2s(mean,value(v[[2]],paste(k,"uncertainty")),value(v[[3]],paste(k,"uncertainty")),v[[4]],paste(k,"uncertainty"))
    if(length(unclear)) {
      unc$error[unclear] <- NA_real_
      report_issue("lith uncertainty",source_rows[unclear],v[[4]][unclear],
        "Revised normal uncertainty has no recoverable sigma count; left blank")
    }
    groups[[k]]<-cbind(mean,unc);used[[k]]<-use
  }
  ap<-num(get("ageuncertaintyposka"))/1000;an<-num(get("ageuncertaintynegka"))/1000
  at<-product_age_type(get("ageuncertaintydistribution"),ap,an,product)
  age_unc<-error_2s(age,ap,an,at,"age uncertainty")
  radius<-value(get("vsacellradiusum"),"cell radius")
  if(product) {
    use<-!is.na(get("vsacellradiusumuse"))
    radius[use]<-value(get("vsacellradiusumuse"),"cell radius")[use]
  }
  temp_source<-get("sstdegcsource")
  temp_source[used$temp]<-get("sstdegcsourceuse")[used$temp]
  sample<-get("fullsamplenameinsource");id<-get("uniqueid")
  sample[is.na(sample)]<-id[is.na(sample)]
  out_df<-data.frame(A=sample,B=get("doi"),C=basename(sf),D="Harper and Giulivi",
    E="dustin.t.harper@utah.edu; claudiag@ldeo.columbia.edu",F=num(field(dat,"^(sitelatitude|modernlatitude)")),G=num(field(dat,"^(sitelongitude|modernlongitude)")),
    H=age,I=age_unc$error,J=age_unc$min,K=age_unc$max,
    L=mapply(join_notes,get("agemodelreference"),get("agemodelnotes"),get("agerecalculatedreason")),M=get("organicd13cmaterialname"),
    N=groups$organic$mean,O=groups$organic$error,P=groups$organic$min,Q=groups$organic$max,
    R=groups$temp$mean,S=groups$temp$error,T=groups$temp$min,U=groups$temp$max,V=temp_source,
    W=groups$phosphate$mean,X=groups$phosphate$error,Y=groups$phosphate$min,Z=groups$phosphate$max,AA=get("phosphatereference"),
    AB=groups$lith$mean,AC=groups$lith$error,AD=groups$lith$min,AE=groups$lith$max,AF=radius,AG=get("vsacellradiusmethod"),
    AH=get("includeparentuniqueid"),AI=get("includechilduniqueid"),AJ=get("compilationnotes"))
  file_id<-sub("^phytoplankton_","",source_key(sf))
  study_outputs[basename(sf)]<-file.path(out_dir,paste0("phyto_Intermediate_",file_id,".xlsx"))
  generated_data[[basename(sf)]]<-out_df
  metadata[[basename(sf)]]<-data.frame(file=basename(sf),sheet=source_sheet,source_row=source_rows,
    unique_id=id,sample_name=sample,parent_id=out_df$AH,child_id=out_df$AI,
    publication_year=num(get("publicationyear")),age_Ma=age,lat=out_df$F,lon=out_df$G,material=out_df$M)
}

# Parent/child links and repeated IDs identify pairs for the shared duplicate rules.
if(length(generated_data)) {
  d <- do.call(rbind,generated_data); m <- do.call(rbind,metadata)
  id_key <- function(x)sub("^algae_","phytoplankton_",tolower(as_chr(x)))
  ids <- id_key(m$unique_id); samples <- as_chr(m$sample_name)
  links <- lapply(seq_len(nrow(m)),function(i)id_key(unlist(strsplit(paste(m$parent_id[i],m$child_id[i],sep=";"),"[;,|]"))))
  linked_pairs <- do.call(rbind,lapply(seq_along(links),function(i) {
    hit <- which(!is.na(ids) & ids %in% links[[i]])
    if(length(hit))cbind(i,hit) else NULL
  }))
  identity_pool <- function(j) {
    same_id <- !is.na(ids[j]) & !is.na(ids) & ids==ids[j]
    same_sample <- !is.na(samples[j]) & !is.na(samples) & samples==samples[j] &
      abs(m$lat-m$lat[j])<=.01 & abs(m$lon-m$lon[j])<=.01
    linked <- if(length(linked_pairs))c(linked_pairs[linked_pairs[,1]==j,2],linked_pairs[linked_pairs[,2]==j,1]) else integer()
    unique(c(which(same_id | same_sample),linked))
  }
  identity_match <- function(i,j)TRUE
  resolved <- resolve_duplicates(d,m,identity_match,c("N","R","W","AB","AF"),
    c("F","G","H","I","J","K",LETTERS[13:26],"AA","AB","AC","AD","AE","AF"),c("L","AG","AJ"),
    order(m$publication_year,m$file,m$source_row,na.last=TRUE),identity_pool)
  write_resolved(resolved$data,m,resolved$keep,c("N","R","W","AB","AF"))
  write_duplicates(resolved$report)
}
finish_reports()
