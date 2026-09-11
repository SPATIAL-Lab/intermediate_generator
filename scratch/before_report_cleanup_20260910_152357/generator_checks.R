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
  write.csv(problems, file.path(out_dir, paste0(proxy, "_data_issues.csv")), row.names=FALSE, na="")
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
  bad <- which(x$C == "please enter value manually")
  report_issue("C", source_rows[bad], x$C[bad], "Archive link not yet assigned")
}

check_uncertainty <- function(dat, type_col, value_cols, known) {
  type <- gsub("[[:space:]]", "", tolower(as.character(dat[[type_col]])))
  present <- rowSums(!as.data.frame(lapply(dat[value_cols], missing_value))) > 0
  bad <- which(present & !type %in% known)
  report_issue(paste0(int2col(type_col), ": ", names(dat)[type_col]), source_rows[bad], dat[[type_col]][bad],
               "Uncertainty magnitude present; type missing or not handled by this mapping")
}

finish_reports <- function() {
  write.csv(problems, file.path(out_dir, paste0(proxy, "_data_issues.csv")), row.names=FALSE, na="")
  write.csv(age_screen, file.path(out_dir, paste0(proxy, "_age_screen.csv")), row.names=FALSE, na="")
  for (file in names(study_outputs)) {
    dest <- sub("\\.xlsx$", "_data_issues.csv", study_outputs[[file]])
    write.csv(problems[problems$file == file, ], dest, row.names=FALSE, na="")
  }
  old <- list.files(out_dir, pattern=paste0("^", output_prefix, "_Intermediate_.*\\.(xlsx|csv)$"), full.names=TRUE)
  current <- c(current_outputs, sub("\\.xlsx$", "_data_issues.csv", unname(study_outputs)))
  old <- setdiff(old, current)
  if (length(old)) {
    archive <- file.path(out_dir, "previous_outputs", format(Sys.time(), "%Y%m%d_%H%M%OS6"))
    dir.create(archive, recursive=TRUE)
    if (!all(file.rename(old, file.path(archive, basename(old))))) stop("Could not archive stale outputs")
  }
  message(sum(age_screen$status == "included"), " rows included; ",
          sum(age_screen$status != "included"), " excluded; ", nrow(problems), " data issues")
}
