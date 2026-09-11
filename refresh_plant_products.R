product_dir <- "product_plant"
source_dir <- "data_plant"
base_url <- "https://www.ncei.noaa.gov/pub/data/paleo/climate_forcing/trace_gases/Paleo-pCO2/"
dir.create(product_dir,showWarnings=FALSE,recursive=TRUE)
files <- list.files(source_dir,pattern="\\.xlsx$")
files <- files[!grepl("^~\\$",files)]
study <- function(x) sub("_p[0-9.]+$","",sub("^stomata-[^_]+_","",tools::file_path_sans_ext(x)))
links <- function(url) {
  index <- tempfile(fileext=".html")
  download.file(url,index,mode="wb",quiet=TRUE)
  text <- paste(readLines(index,warn=FALSE),collapse="\n")
  matches <- regmatches(text,gregexpr('href="[^"]+\\.xlsx"',text))[[1]]
  sub('"$','',sub('^href="','',matches))
}
archive_files <- links(base_url)
products <- links(paste0(base_url,"product_files/"))
products <- products[grepl("^stomata-(franks|sd|si|sr)_",products) & study(products)%in%study(files)]
keys <- sub("_p[0-9.]+\\.xlsx$","",products)
latest <- character()
for(key in unique(keys)) {
  x <- products[keys==key]
  version <- sub(".*_p([0-9.]+)\\.xlsx$","\\1",x)
  best <- 1L
  for(i in seq_along(x))if(utils::compareVersion(version[i],version[best])>0)best<-i
  latest <- c(latest,x[best])
}
manifest <- data.frame(file=character(),url=character(),md5=character(),retrieved=character())
for(file in latest) {
  url <- paste0(base_url,"product_files/",file)
  tmp <- tempfile(fileext=".xlsx");download.file(url,tmp,mode="wb",quiet=TRUE)
  dest <- file.path(product_dir,file)
  if(file.exists(dest) && tools::md5sum(dest)!=tools::md5sum(tmp)) {
    old <- file.path(product_dir,"previous_versions",format(Sys.time(),"%Y%m%d_%H%M%S"))
    dir.create(old,recursive=TRUE,showWarnings=FALSE)
    if(!file.copy(dest,file.path(old,file)))stop("Could not preserve previous product: ",file)
  }
  if(!file.copy(tmp,dest,overwrite=TRUE))stop("Could not save product: ",file)
  manifest <- rbind(manifest,data.frame(file=file,url=url,md5=unname(tools::md5sum(dest)),retrieved=as.character(Sys.Date())))
}
write.csv(manifest,file.path(product_dir,"manifest.csv"),row.names=FALSE)
all_files <- unique(c(files,sub("_p[0-9.]+\\.xlsx$",".xlsx",latest)))
archive_links <- data.frame(file=all_files,url=ifelse(all_files%in%archive_files,paste0(base_url,all_files),NA_character_))
write.csv(archive_links,file.path(product_dir,"archive_links.csv"),row.names=FALSE,na="")
message(length(latest)," product files cached; run plant_gen.R to regenerate outputs")
