stopifnot(is.numeric(age_range_Ma), length(age_range_Ma) == 2, !anyNA(age_range_Ma),
          age_range_Ma[1] <= age_range_Ma[2])

problems <- data.frame(file=character(), source_row=integer(), field=character(),
                       value=character(), problem=character())
age_screen <- data.frame(file=character(), source_row=integer(), age_Ma=numeric(),
                         min_age_Ma=numeric(), max_age_Ma=numeric(), status=character())
current_outputs <- character()
study_outputs <- character()

missing_value <- function(x) {
  is.na(x) | tolower(trimws(as.character(x))) %in% c("", "na", "nan", "null", "not reported")
}

report_issue <- function(field, rows, value, problem) {
  if (!length(rows)) return(invisible(NULL))
  problems <<- unique(rbind(problems, data.frame(file=basename(sf), source_row=rows,
                                                field=field, value=as.character(value), problem=problem)))
}

select_age <- function(raw, source_rows) {
  age <- suppressWarnings(as.numeric(as.character(raw))) / age_divisor
  keep <- is.finite(age) & age >= age_range_Ma[1] & age <= age_range_Ma[2]
  status <- ifelse(!is.finite(age), "missing or invalid age", ifelse(keep, "included", "outside age range"))
  age_screen <<- rbind(age_screen, data.frame(file=basename(sf), source_row=source_rows,
    age_Ma=age, min_age_Ma=age_range_Ma[1], max_age_Ma=age_range_Ma[2], status=status))
  report_issue("age", source_rows[!is.finite(age)], raw[!is.finite(age)],
               "Missing or invalid age; excluded from age-filtered outputs")
  keep
}

check_output <- function(x, source_rows, numeric_cols, measurement_cols) {
  for (co in numeric_cols) {
    raw <- x[[co]]
    value <- suppressWarnings(as.numeric(as.character(raw)))
    bad <- !missing_value(raw) & !is.finite(value)
    report_issue(co, source_rows[bad], raw[bad], "Non-numeric or non-finite output value")
  }
  for (co in c("A", "B", "F", "G")) {
    bad <- missing_value(x[[co]])
    report_issue(co, source_rows[bad], x[[co]][bad], paste("Missing", c(A="sample ID", B="DOI", F="latitude", G="longitude")[[co]]))
  }
  for (co in c("F", "G")) {
    value <- suppressWarnings(as.numeric(as.character(x[[co]])))
    bad <- which(abs(value) > if (co == "F") 90 else 180)
    report_issue(co, source_rows[bad], x[[co]][bad], "Coordinate outside valid range")
  }
  bad <- which(rowSums(!as.data.frame(lapply(x[measurement_cols], missing_value))) == 0)
  report_issue("measurement", source_rows[bad], rep(NA_character_, length(bad)),
               "No primary proxy measurement mapped")
  lo <- suppressWarnings(as.numeric(as.character(x$J)))
  hi <- suppressWarnings(as.numeric(as.character(x$K)))
  age <- suppressWarnings(as.numeric(as.character(x$H)))
  bad <- which(lo > hi | age < lo | age > hi)
  report_issue("age bounds", source_rows[bad], paste(lo[bad], age[bad], hi[bad], sep="; "),
               "Age bounds reversed or do not contain central age")

}

write_duplicates <- function(candidates) {
  if (nrow(candidates)) write.csv(candidates,
    file.path(out_dir, paste0(proxy, "_duplicate_candidates.csv")), row.names=FALSE, na="")
  duplicate_count <<- nrow(candidates)
}
duplicate_count <- 0L

finish_reports <- function() {
  issues_file <- file.path(out_dir, paste0(proxy, "_data_issues.csv"))
  duplicate_file <- file.path(out_dir, paste0(proxy, "_duplicate_candidates.csv"))
  if (nrow(problems)) write.csv(problems, issues_file, row.names=FALSE, na="")
  reports <- list.files(out_dir, pattern="\\.csv$", full.names=TRUE)
  generated <- grepl(paste0("^(", proxy, "_|", output_prefix, "_Intermediate_)"), basename(reports))
  keep <- c(if(nrow(problems)) issues_file, if(duplicate_count) duplicate_file)
  old <- setdiff(reports[generated], keep)
  if (length(old) && !all(file.remove(old))) stop("Could not remove stale reports")
  old <- list.files(out_dir, pattern=paste0("^", output_prefix, "_Intermediate_.*\\.xlsx$"), full.names=TRUE)
  old <- setdiff(old, current_outputs)
  if (length(old) && !all(file.remove(old))) stop("Could not remove stale outputs")
  message(sum(age_screen$status == "included"), " rows included; ",
          sum(age_screen$status != "included"), " excluded; ", nrow(problems), " data issues")
}

# Shared duplicate decisions; each generator supplies its sample-identity evidence.
resolve_duplicates <- function(d, m, identity_match, measurements, compare, notes, order_rows, identity_pool=NULL) {
  equal <- function(a,b) {
    x <- suppressWarnings(as.numeric(as.character(a))); y <- suppressWarnings(as.numeric(as.character(b)))
    if(is.finite(x) && is.finite(y)) return(abs(x-y)<=1e-8*pmax(1,abs(x),abs(y)))
    tolower(trimws(as.character(a)))==tolower(trimws(as.character(b)))
  }
  overlap <- function(i,j,cols) {
    present <- !vapply(d[i,cols,drop=FALSE],missing_value,logical(1)) &
               !vapply(d[j,cols,drop=FALSE],missing_value,logical(1))
    cols[present]
  }
  conflicts <- function(i,j,cols) {
    cols <- overlap(i,j,cols)
    any(vapply(cols,function(k)!equal(d[[k]][i],d[[k]][j]),logical(1)))
  }
  pairs <- data.frame(record=integer(),linked_record=integer(),status=character())
  retained <- integer(); owner <- seq_len(nrow(d))
  for(j in order_rows) {
    possible <- if(is.null(identity_pool)) retained else intersect(retained,identity_pool(j))
    possible <- possible[vapply(possible,function(i)isTRUE(identity_match(i,j)),logical(1))]
    possible <- possible[!vapply(possible,function(i)conflicts(i,j,measurements),logical(1))]
    clear <- possible[vapply(possible,function(i)
      length(overlap(i,j,measurements))>0 && !conflicts(i,j,compare),logical(1))]
    if(length(clear)==1 && length(possible)==1) {
      i <- clear
      fill <- names(d)[vapply(d[i,,drop=FALSE],missing_value,logical(1)) &
                       !vapply(d[j,,drop=FALSE],missing_value,logical(1))]
      for(k in fill)d[[k]][i]<-d[[k]][j]
      for(k in notes) {
        value <- unique(as.character(c(d[[k]][i],d[[k]][j])))
        value <- value[!missing_value(value)]
        d[[k]][i] <- if(length(value))paste(value,collapse="; ") else NA_character_
      }
      owner[j] <- i
      pairs <- rbind(pairs,data.frame(record=i,linked_record=j,
        status="record 1 retained; record 2 removed/merged"))
    } else {
      if(length(possible)) pairs <- rbind(pairs,data.frame(record=possible,linked_record=j,
        status="ambiguous: both retained; human review needed"))
      retained <- c(retained,j)
    }
  }
  # Resolve report endpoints after later merges so statuses describe final outputs.
  if(nrow(pairs)) {
    ambiguous <- grepl("^ambiguous",pairs$status)
    pairs$record[ambiguous] <- owner[pairs$record[ambiguous]]
    pairs$linked_record[ambiguous] <- owner[pairs$linked_record[ambiguous]]
    pairs <- unique(pairs)
  }
  left <- m[pairs$record,,drop=FALSE]; right <- m[pairs$linked_record,,drop=FALSE]
  names(left)<-paste0(names(left),"_1");names(right)<-paste0(names(right),"_2")
  list(data=d,metadata=m,keep=sort(retained),owner=owner,
       report=data.frame(left,right,status=pairs$status,row.names=NULL))
}

write_resolved <- function(d, m, keep, measurements) {
  checked <- c("Missing sample ID","Missing DOI","Missing latitude","Missing longitude",
               "Coordinate outside valid range","No primary proxy measurement mapped",
               "Age bounds reversed or do not contain central age","Archive link not yet assigned",
               "Non-numeric or non-finite output value")
  problems <<- problems[!problems$problem %in% checked,]
  for(f in unique(m$file[keep])) {
    sf <<- f
    rows <- keep[m$file[keep]==f]
    check_output(d[rows,,drop=FALSE],m$source_row[rows],names(d)[vapply(d,is.numeric,logical(1))],measurements)
  }
  current_outputs <<- character()
  write_one <- function(x,file) {
    wb <- openxlsx::loadWorkbook(template_file)
    openxlsx::writeData(wb,template_sheet,matrix("",1,ncol(x)),startRow=1,colNames=FALSE)
    openxlsx::writeData(wb,template_sheet,x,startRow=5,colNames=FALSE,keepNA=TRUE)
    openxlsx::saveWorkbook(wb,file,overwrite=TRUE)
    current_outputs <<- c(current_outputs,file)
  }
  for(f in unique(m$file[keep]))write_one(d[keep[m$file[keep]==f],,drop=FALSE],study_outputs[[f]])
  if(length(keep))write_one(d[keep,,drop=FALSE],file.path(out_dir,paste0(output_prefix,"_Intermediate_combined.xlsx")))
}

# Prefer the latest product for each study and method.
source_aliases <- character()
source_key <- function(f) {
  x <- sub("_p[0-9.]+$","",tools::file_path_sans_ext(basename(f)))
  matched <- x%in%names(source_aliases)
  x[matched] <- source_aliases[x[matched]]
  x
}
source_files <- function(proxy) {
  source_aliases <<- character()
  files <- function(kind) {
    x<-list.files(paste0(kind,"_",proxy),"\\.xlsx$",full.names=TRUE)
    x[!grepl("^~\\$",basename(x))]
  }
  archives<-files("archive");products<-files("product")
  if(!length(c(archives,products)))stop("No source workbooks for ",proxy)
  versions<-sub(".*_p([0-9.]+)\\.xlsx$","\\1",products)
  versions[!grepl("^[0-9.]+$",versions)]<-"0"
  if(length(products))products<-products[order(package_version(versions),decreasing=TRUE)]
  products<-products[!duplicated(source_key(products))]
  a<-archives[!source_key(archives)%in%source_key(products)]
  p<-products[!source_key(products)%in%source_key(archives)]
  # A unique DOI/method match handles renamed studies without a filename list.
  identity<-function(f) {
    d<-read_source(f,preview=TRUE)
    doi<-tolower(text_value(field(d,"^doi$")))
    doi<-doi[!is.na(doi)][1]
    doi<-sub(";[[:space:]]*(10[.]|https?://).*","",doi)
    doi<-sub("^(https?://(dx\\.)?doi.org/|doi:)","",doi)
    method<-if(proxy=="plant")sub("_.*","",source_key(f)) else proxy
    if(is.na(doi))NA_character_ else paste(method,utils::URLdecode(doi))
  }
  if(length(a)&&length(p)) {
    aid<-vapply(a,identity,character(1));pid<-vapply(p,identity,character(1))
    for(i in seq_along(p)) {
      hit<-which(!is.na(aid)&aid==pid[i])
      if(length(hit)==1 && sum(pid==pid[i],na.rm=TRUE)==1)
        source_aliases[source_key(p[i])] <<- source_key(a[hit])
    }
  }
  sort(c(products,archives[!source_key(archives)%in%source_key(products)]))
}
text_value <- function(x) {
  x <- trimws(gsub("\u00a0"," ",as.character(x),fixed=TRUE))
  x[tolower(x)%in%c("","na","n/a","nan","null","not reported","-","–","—")] <- NA_character_
  x
}
header_key <- function(x)tolower(gsub("[^[:alnum:]]","",x))
read_source <- function(f,preview=FALSE,tabs=getSheetNames(f)) {
  for(sheet in tabs) {
    top <- read.xlsx(f,sheet,rows=1:6,colNames=FALSE,skipEmptyRows=FALSE,skipEmptyCols=FALSE)
    if(is.null(top))next
    score <- apply(top,1,function(x)sum(header_key(x)%in%c("proxy","doi","publicationyear","firstauthorlastname","agekabp","sampleid")))
    if(max(score)<2)next
    h <- which.max(score)
    raw <- read.xlsx(f,sheet,rows=if(preview)1:12 else NULL,colNames=FALSE,skipEmptyRows=FALSE,skipEmptyCols=FALSE)
    names(raw) <- header_key(unlist(raw[h,],use.names=FALSE))
    rows <- seq_len(nrow(raw))
    keep <- rows>h & rowSums(!is.na(raw))>0
    age_col <- grep("^age(ma)?$",names(raw))[1]
    if(!is.na(age_col) && h<nrow(raw) && header_key(raw[h+1,age_col])%in%c("ma","ka"))keep[h+1]<-FALSE
    if("proxy"%in%names(raw))keep <- keep & !header_key(raw[["proxy"]])%in%c("proxy","proxytype")
    if("firstauthorlastname"%in%names(raw))keep <- keep & header_key(raw[["firstauthorlastname"]])!="firstauthorlastname"
    d <- as.data.frame(lapply(raw[keep,,drop=FALSE],text_value),check.names=FALSE)
    names(d)<-names(raw)
    attr(d,"rows")<-rows[keep];attr(d,"sheet")<-sheet
    attr(d,"headers")<-text_value(unlist(raw[h,],use.names=FALSE))
    return(d)
  }
  stop("No data table in ",basename(f))
}
field <- function(d, pattern, offset=0) {
  col <- grep(pattern,names(d),ignore.case=TRUE)[1]+offset
  if(is.na(col) || col>ncol(d))rep(NA_character_,nrow(d)) else d[[col]]
}
num <- function(x)suppressWarnings(as.numeric(gsub("−","-",text_value(x),fixed=TRUE)))
join_notes <- function(...) {
  x <- unique(text_value(unlist(list(...))))
  x <- x[!is.na(x)]
  if(length(x))paste(x,collapse="; ") else NA_character_
}

# Convert labelled errors to the 2-sigma fields or uniform bounds.
error_2s <- function(mean,pos,neg=pos,type,field_name,default=NA_character_) {
  type <- header_key(type)
  type[is.na(type)|type==""] <- header_key(default)
  divisor <- rep(if(is.na(default))NA_real_ else 2,length(mean))
  divisor[grepl("^1(sd|se|std|sem|sigma)|^1standard",type)]<-1
  divisor[grepl("^2(sd|se|std|sem|sigma)|^2xstandard|^2standard|normal|^setvalue",type)]<-2
  divisor[grepl("5to95|95.*5percent",type)]<-qnorm(.95)
  divisor[grepl("25to975|97525|95ci|95confidence",type)]<-qnorm(.975)
  uniform <- grepl("uniform|range",type) & !grepl("normal",type)
  present <- !is.na(pos)|!is.na(neg)
  bad <- which(present & (pos<0 | neg<0))
  report_issue(field_name,source_rows[bad],paste(pos[bad],neg[bad]),"Negative uncertainty; left blank")
  pos[bad]<-neg[bad]<-NA_real_
  unknown <- which(present & is.na(divisor) & !uniform)
  report_issue(field_name,source_rows[unknown],type[unknown],"Uncertainty type missing or unclear; left blank")
  partial <- which(xor(is.na(pos),is.na(neg)))
  report_issue(field_name,source_rows[partial],paste(pos[partial],neg[partial]),"Only one uncertainty side available")
  data.frame(error=ifelse(uniform,NA_real_,(pos+neg)/divisor),
    min=ifelse(uniform,mean-neg,NA_real_),max=ifelse(uniform,mean+pos,NA_real_))
}

# Keep corrected ages; exclude unresolved quarantine or explicit exclusion flags.
usable_rows <- function(d) {
  truth <- function(x)tolower(text_value(x))%in%c("true","yes","1")
  age_bad <- truth(field(d,"^age(data)?quarantined"))
  revised <- truth(field(d,"^age(data)?recalculated")) &
    !is.na(num(field(d,"^ageka(bp)?$")))
  excluded <- tolower(field(d,"^includetf$"))%in%c("false","no","0") |
    truth(field(d,"^(exclude|unusable)(tf)?$"))
  keep <- !(excluded | (age_bad & !revised))
  if(any(!keep))message(sum(!keep)," unusable source rows excluded")
  keep
}

product_age_type <- function(type,pos,neg,product) {
  assumed <- which(product & missing_value(type) & (!is.na(pos)|!is.na(neg)))
  report_issue("age uncertainty",source_rows[assumed],paste(pos[assumed],neg[assumed],sep="; "),
               "Assumed 2 sigma; product age uncertainty type not stated")
  type[assumed]<-"2sd"
  type
}

select_rows <- function(d,age,rows) {
  keep<-usable_rows(d)
  if(any(keep))keep[keep]<-select_age(age[keep],rows[keep])
  keep
}
