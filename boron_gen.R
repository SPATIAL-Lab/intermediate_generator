# age range in Ma (youngest, oldest); c(-Inf, Inf) includes all dated samples
age_range_Ma <- c(0, 23.03)

library(openxlsx)

# paths
template_file <- file.path("templates", "boron_IntermediateTemplate.xlsx")
template_sheet <- "data4PSM"
source_dir <- "data_boron"
out_dir <- "output_boron"
archive_dir <- "https://www.ncei.noaa.gov/pub/data/paleo/climate_forcing/trace_gases/Paleo-pCO2/"
archive_files <- paste0("boron_isotopes_", c(
  "anderson_2024", "badger_2013", "bartoli_2011", "brown_2022", "chalk_2017",
  "clarkson_2015", "delavega_2020", "dyez_2018", "foster_2008", "foster_2012",
  "greenop_2014", "greenop_2019", "guillermic_2022", "henehan_2013",
  "hoenisch_2005", "hoenisch_2009", "martinez-boti_2015", "pearson_2000",
  "pearson_2009", "seki_2010", "sosdian_2018", "stap_2016"), ".xlsx")

dir.create(out_dir, showWarnings=FALSE, recursive=TRUE)
proxy <- "boron"
output_prefix <- "boron"
age_divisor <- 1000
source("generator_checks.R", local=TRUE)
src_files <- list.files(source_dir, "\\.xlsx$", full.names=TRUE)
src_files <- src_files[!grepl("^~\\$", basename(src_files))]
if (!length(src_files)) stop("No source workbooks found")
if (any(!basename(src_files) %in% archive_files)) stop("Verify new NOAA archive filenames first")

generated_data <- metadata <- list()

# helpers
as_chr <- function(x) {
  x <- trimws(as.character(x))
  x[tolower(x) %in% c("", "na", "n/a", "nan", "null", "not reported")] <- NA_character_
  x
}
as_num <- function(x) suppressWarnings(as.numeric(as_chr(x)))

numeric_col <- function(col) {
  raw <- as_chr(dat[[col2int(col)]])
  value <- as_num(raw)
  bad <- which(!is.na(raw) & !is.finite(value))
  if (!col %in% c("CC","CF"))
    report_issue(col, source_rows[bad], raw[bad], "Non-numeric entry omitted from numeric output; original retained here")
  value[!is.finite(value)] <- NA_real_
  value
}

replicate_mean <- function(x) {
  x <- sub("[[:space:]]*\\(.*$", "", as_chr(x))
  if (is.na(x)) return(NA_real_)
  parts <- as_chr(strsplit(x, ",", fixed=TRUE)[[1]])
  values <- as_num(parts)
  if (any(!is.na(parts) & !is.finite(values)) || all(is.na(values))) return(NA_real_)
  mean(values, na.rm=TRUE)
}

uncertainty <- function(mean, pos, neg, type, field, bounds=TRUE) {
  type <- tolower(as_chr(type))
  uniform <- grepl("range|uniform", type) & !grepl("normal", type)
  divisor <- rep(2, length(mean)) # archive magnitude columns explicitly specify 2s
  divisor[grepl("^1 *(sd|se|std|sem|sigma)", type)] <- 1
  divisor[grepl("5 *to *95", type)] <- qnorm(.95)
  divisor[grepl("2[.]5 *to *97[.]5|97[.]5/2[.]5", type)] <- qnorm(.975)
  present <- !is.na(pos) | !is.na(neg)
  bad <- which(present & (pos < 0 | neg < 0))
  report_issue(field, source_rows[bad], paste(pos[bad], neg[bad], sep="; "), "Negative uncertainty magnitude; output left blank")
  pos[bad] <- neg[bad] <- NA_real_
  partial <- which(xor(is.na(pos), is.na(neg)))
  report_issue(field, source_rows[partial], paste(pos[partial], neg[partial], sep="; "), "Only one uncertainty side available")
  blank <- which(present & is.na(type))
  report_action(field, source_rows[blank], rep(NA_character_, length(blank)), "Used explicit 2s magnitude headers; distribution label absent")
  if (!bounds) {
    rows <- which(present & uniform)
    report_issue(field, source_rows[rows], type[rows], "Range cannot be represented in this template uncertainty column; left blank")
  }
  data.frame(error=ifelse(uniform, NA_real_, (pos+neg)/divisor),
             min=ifelse(uniform & bounds, mean-neg, NA_real_),
             max=ifelse(uniform & bounds, mean+pos, NA_real_))
}

replicate_key <- function(x) {
  x <- sub("[[:space:]]*\\(.*$", "", as_chr(x))
  if (is.na(x)) return(NA_character_)
  parts <- as_chr(strsplit(x, ",", fixed=TRUE)[[1]])
  values <- as_num(parts)
  if (any(!is.na(parts) & !is.finite(values))) return(tolower(x))
  if (all(is.na(values))) NA_character_ else paste(sort(values[!is.na(values)]), collapse=",")
}

# mapping
for (sf in src_files) {
  message("Reading: ", basename(sf))
  sheets <- getSheetNames(sf)
  hit <- which(tolower(trimws(sheets)) %in% c("boron isotopes", "4. boron isotopes"))
  if (length(hit) != 1) stop("Check boron data sheet in: ", sf)
  source_sheet <- sheets[hit]
  dat <- read.xlsx(sf, sheet=source_sheet, startRow=3, skipEmptyRows=FALSE, skipEmptyCols=FALSE)
  expected <- c("Age.(ka)", "d11B.(‰,.average.of.replicates)", "temperature.(°C)",
                "Mg/Ca.(mmol/mol,.average.of.replicates)")
  if (!identical(names(dat)[c(33,57,76,81)], expected)) {
    report_issue("headers", NA_integer_, paste(names(dat)[c(33,57,76,81)], collapse="; "),
                 "Unrecognized boron layout; generation stopped")
    stop("Check boron layout in: ", sf)
  }
  keep <- rowSums(!as.data.frame(lapply(dat, missing_value))) > 0
  source_rows <- which(keep)+3L
  dat <- dat[keep, , drop=FALSE]
  age_only <- rowSums(!as.data.frame(lapply(dat[-33], missing_value))) == 0
  report_action("AG", source_rows[age_only], dat[[33]][age_only], "Age-only row with no sample metadata or measurements; excluded")
  source_rows <- source_rows[!age_only]
  dat <- dat[!age_only, , drop=FALSE]
  keep <- select_age(dat[[33]], source_rows)
  source_rows <- source_rows[keep]
  dat <- dat[keep, , drop=FALSE]
  if (!nrow(dat)) next
  n <- nrow(dat)

  id_parts <- lapply(dat[15:20], as_chr)
  sample <- apply(as.data.frame(id_parts), 1, function(x) {
    if (all(is.na(x))) NA_character_ else paste(ifelse(is.na(x), "", x), collapse="_")
  })
  age <- numeric_col("AG")/1000
  age_unc <- uncertainty(age, numeric_col("AI")/1000, numeric_col("AJ")/1000, dat[[34]], "AH")
  d11B <- numeric_col("BE")
  d11B_unc <- numeric_col("BG")
  boron_unc <- uncertainty(d11B, d11B_unc, d11B_unc, dat[[58]], "BF", FALSE)
  internal <- as_num(dat[[56]])
  rows <- which(internal > boron_unc$error)
  report_issue("BG", source_rows[rows], d11B_unc[rows], "Reported uncertainty smaller than replicate 2SE; review internal versus external uncertainty")

  mg <- numeric_col("CC")
  reps <- vapply(dat[[80]], replicate_mean, numeric(1))
  use <- which(is.na(mg) & !is.na(reps))
  mg[use] <- reps[use]

  mg_pos <- numeric_col("CE")
  mg_neg <- numeric_col("CF")
  type <- gsub("[[:space:]]", "", tolower(as_chr(dat[[82]])))
  supplied <- ifelse(is.na(mg_pos), mg_neg, mg_pos)
  symmetric <- type %in% c("setvalue,+/-3%", "setvalue,±3%") &
    xor(is.na(mg_pos), is.na(mg_neg)) & !is.na(mg) &
    !is.na(supplied) & abs(supplied - .03*mg) < 1e-8
  mg_pos[symmetric] <- mg_neg[symmetric] <- supplied[symmetric]
  mg_unc <- uncertainty(mg, mg_pos, mg_neg, dat[[82]], "CD", FALSE)
  temp <- numeric_col("BX")
  temp_unc <- uncertainty(temp, numeric_col("BZ"), numeric_col("CA"), dat[[77]], "BY")

  note_cols <- lapply(dat[c(80,81,84)],as_chr)
  note_cols[[1]][!is.na(reps) & !grepl("\\(",note_cols[[1]])] <- NA_character_
  for (j in 2:3) note_cols[[j]][!is.na(as_num(note_cols[[j]]))] <- NA_character_
  notes <- vapply(seq_len(n),function(i) {
    extra <- vapply(note_cols,function(x) x[i],character(1))
    extra <- unique(extra[!is.na(extra)])
    x <- c(as_chr(dat[[173]][i]),if (length(extra)) paste0("Mg/Ca: ",extra),
           if (symmetric[i]) "Mg/Ca uncertainty: missing side filled using the archive's symmetric +/-3% specification.")
    x <- x[!is.na(x)]
    if (length(x)) paste(x,collapse="; ") else NA_character_
  },character(1))

  out_df <- data.frame(
    A=sample, B=as_chr(dat[[4]]), C=paste0(archive_dir, basename(sf)),
    D="Harper and Giulivi", E="dustin.t.harper@utah.edu; claudiag@ldeo.columbia.edu",
    F=numeric_col("Y"), G=numeric_col("Z"), H=age, I=age_unc$error, J=age_unc$min, K=age_unc$max,
    L=as_chr(dat[[39]]), M=as_chr(dat[[47]]), N=as_chr(dat[[48]]), O=as_chr(dat[[49]]),
    P=as_chr(dat[[53]]), Q=d11B, R=boron_unc$error, S=mg, T=mg_unc$error,
    U=temp, V=temp_unc$error, W=temp_unc$min, X=temp_unc$max, Y=notes)
  class(out_df$C) <- "hyperlink"
  check_output(out_df, source_rows, c("F","G","H","I","J","K","Q","R","S","T","U","V","W","X"), "Q")

  file_id <- sub("^boron_isotopes_", "", tools::file_path_sans_ext(basename(sf)))
  out_file <- file.path(out_dir, paste0("boron_Intermediate_", file_id, ".xlsx"))
  study_outputs[basename(sf)] <- out_file
  generated_data[[basename(sf)]] <- out_df
  metadata[[basename(sf)]] <- data.frame(file=basename(sf), sheet=source_sheet, source_row=source_rows,
    publication_year=as_num(dat[[3]]), sample=sample, site=as_chr(dat[[15]]), depth_m=as_num(dat[[21]]),
    composite_depth_m=as_num(dat[[22]]), lat=out_df$F, lon=out_df$G, age_Ma=age, species=out_df$M, shell_size=out_df$N,
    d11B_replicates=vapply(dat[[54]],replicate_key,character(1)),
    MgCa_replicates=vapply(dat[[80]],replicate_key,character(1)),
    replicate_2SE=internal, published_pCO2=as_num(dat[[156]]),
    d11B=d11B, d11B_2s=boron_unc$error, MgCa=mg, MgCa_2s=mg_unc$error, temperature=temp,
    source_notes=as_chr(dat[[173]]), MgCa_from_replicates=seq_len(n) %in% use)
}

# remove redundant records and combine
redundant <- function(d, m, i, j) {
  if (is.na(d$Q[i]) || is.na(d$Q[j]) || is.na(d$M[i]) || is.na(d$M[j])) return(FALSE)
  same <- function(a, b) {
    if (is.na(b)) return(TRUE)
    if (is.na(a)) return(FALSE)
    if (is.numeric(a)) isTRUE(all.equal(a,b,tolerance=1e-8)) else
      identical(tolower(as_chr(a)),tolower(as_chr(b)))
  }
  cols <- c("H","I","J","K","M","N","O","P",LETTERS[17:24])
  all(vapply(cols,function(co) same(d[[co]][i],d[[co]][j]),logical(1))) &&
    all(vapply(c("d11B_replicates","MgCa_replicates","replicate_2SE"),
               function(co) same(m[[co]][i],m[[co]][j]),logical(1)))
}

pairs <- data.frame(record=integer(), linked_record=integer())
if (length(generated_data)) {
  all_data <- do.call(rbind, generated_data)
  m <- do.call(rbind, metadata)
  m$retained <- TRUE
  original <- seq_len(nrow(m))
  key <- function(x) gsub("[^a-z0-9_]", "", sub("^(dsdp|odp|iodp)[[:space:]]*", "", tolower(as_chr(x))))
  sample_key <- key(m$sample)
  site_key <- key(m$site)
  order_rows <- order(m$publication_year,m$file,m$source_row,na.last=TRUE)
  for (ii in seq_len(length(order_rows)-1L)) {
    i <- order_rows[ii]
    if (!m$retained[i]) next
    j <- order_rows[seq.int(ii+1L,length(order_rows))]
    same_id <- !is.na(sample_key[i]) & sample_key[i] == sample_key[j]
    same_depth <- !is.na(site_key[i]) & site_key[i] == site_key[j] & abs(m$depth_m[i]-m$depth_m[j]) < 1e-6
    same_site <- abs(m$lat[i]-m$lat[j]) <= .01 & abs(m$lon[i]-m$lon[j]) <= .01
    for (k in j[which(m$retained[j] & same_site & (same_id | same_depth))]) {
      depth_conflict <- any(abs(c(m$depth_m[i]-m$depth_m[k],
                                  m$composite_depth_m[i]-m$composite_depth_m[k])) > 1e-6, na.rm=TRUE)
      if (!depth_conflict && redundant(all_data,m,i,k)) {
        pairs <- rbind(pairs,data.frame(record=i,linked_record=k))
        m$retained[k] <- FALSE
        original[k] <- i
      }
    }
  }
  m$retained_file <- m$file[original]
  m$retained_source_row <- m$source_row[original]
  m$intermediate_row <- m$combined_row <- NA_integer_
  m$combined_row[m$retained] <- seq_len(sum(m$retained))+4L
  for (file in names(generated_data)) {
    rows <- which(m$file==file & m$retained)
    removed <- which(m$file==file & !m$retained)
    sf <- file
    report_action("duplicate",m$source_row[removed],
      paste(m$retained_file[removed],m$retained_source_row[removed],sep=":"),
      "Redundant record removed; original retained with no loss of mapped inputs")
    if (!length(rows)) next
    m$intermediate_row[rows] <- seq_along(rows)+4L
    out_df <- all_data[rows, , drop=FALSE]
    class(out_df$C) <- "hyperlink"
    out_file <- study_outputs[[file]]
    wb <- loadWorkbook(template_file)
    writeData(wb,template_sheet,matrix("",1,25),startRow=1,colNames=FALSE)
    writeData(wb,template_sheet,out_df,startRow=5,colNames=FALSE,keepNA=TRUE)
    saveWorkbook(wb,out_file,overwrite=TRUE)
    current_outputs <- c(current_outputs,out_file)
  }
  combined <- all_data[m$retained, , drop=FALSE]
  class(combined$C) <- "hyperlink"
  wb <- loadWorkbook(template_file)
  writeData(wb,template_sheet,matrix("",1,25),startRow=1,colNames=FALSE)
  writeData(wb,template_sheet,combined,startRow=5,colNames=FALSE,keepNA=TRUE)
  combined_file <- file.path(out_dir,"boron_Intermediate_combined.xlsx")
  saveWorkbook(wb,combined_file,overwrite=TRUE)
  current_outputs <- c(current_outputs,combined_file)
  left <- m[pairs$record, ]; right <- m[pairs$linked_record, ]
  names(left) <- paste0(names(left),"_1"); names(right) <- paste0(names(right),"_2")
  candidates <- data.frame(action=rep("removed redundant record",nrow(pairs)),left,right,row.names=NULL)
} else {
  candidates <- data.frame(action=character(),file_1=character(),source_row_1=integer(),file_2=character(),source_row_2=integer())
}
candidates$status <- rep("record 1 retained; record 2 removed", nrow(candidates))
write_duplicates(candidates)
finish_reports()
message(nrow(candidates)," redundant records removed")
