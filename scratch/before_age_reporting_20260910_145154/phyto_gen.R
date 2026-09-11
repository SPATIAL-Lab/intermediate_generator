library(openxlsx)

# paths 
template_file <- file.path("templates", "phyto_IntermediateTemplate.xlsx")
template_sheet <- "data4PSM"
source_dir <- "data_phyto"
source_sheet <- "Data"
out_dir <- "output_phyto"
archive_dir <- "https://www.ncei.noaa.gov/pub/data/paleo/climate_forcing/trace_gases/Paleo-pCO2/"
archive_files <- c(
  "phytoplankton_andersen_1999.xlsx",
  "phytoplankton_badger_2013a.xlsx",
  "phytoplankton_badger_2013b.xlsx",
  "phytoplankton_badger_2019.xlsx",
  "phytoplankton_bae_2015.xlsx",
  "phytoplankton_bolton_2016.xlsx",
  "phytoplankton_jasper_1990.xlsx",
  "phytoplankton_jasper_1994.xlsx",
  "phytoplankton_mejia_2017.xlsx",
  "phytoplankton_pagani_1999a.xlsx",
  "phytoplankton_pagani_1999b.xlsx",
  "phytoplankton_pagani_2000.xlsx",
  "phytoplankton_pagani_2010.xlsx",
  "phytoplankton_pagani_2011.xlsx",
  "phytoplankton_palmer_2010.xlsx",
  "phytoplankton_rae_2021.xlsx",
  "phytoplankton_seki_2010.xlsx",
  "phytoplankton_super_2018.xlsx",
  "phytoplankton_witkowski_2018.xlsx",
  "phytoplankton_zhang_2013.xlsx",
  "phytoplankton_zhang_2017.xlsx",
  "phytoplankton_zhang_2019.xlsx",
  "phytoplankton_zhang_2020.xlsx"
)

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

src_files <- list.files(source_dir, pattern = "\\.xlsx$", full.names = TRUE)
src_files <- src_files[!grepl("^~\\$", basename(src_files))]
src_files <- src_files[file.exists(src_files)]

if (any(!basename(src_files) %in% archive_files)) stop("Verify new NOAA archive filenames first")
if (!length(src_files)) stop("No .xlsx files found in: ", source_dir)
if (!file.exists(template_file)) stop("Template not found: ", template_file)

metadata <- list()
issues <- list()
generated_data <- list()

# helpers
as_num <- function(x) suppressWarnings(as.numeric(as_chr(x)))

as_chr <- function(x) {
  x <- trimws(as.character(x))
  x[tolower(x) %in% c("", "na", "nan", "null", "not reported")] <- NA_character_
  x
}

clean_type <- function(x) {
  x <- tolower(as_chr(x))
  gsub("[[:space:]]", "", sub(" *\\(assigned value\\)$", "", x))
}

uncertainty <- function(mean, pos, neg, type, sigma = 2) {
  type <- clean_type(type)
  divisor <- rep(NA_real_, length(type))
  divisor[type %in% c("1std", "1sem", "1sd", "1se")] <- 1
  divisor[type %in% c("2std", "2sem", "2sd", "2se", "normal", "normaldistribution", "setvalue")] <- 2
  divisor[type == "5to95percentile" & !is.na(type)] <- qnorm(0.95)
  divisor[type == "2.5to97.5percentile" & !is.na(type)] <- qnorm(0.975)
  uniform <- type %in% c("uniform", "range", "uniformdistribution")
  data.frame(error = sigma * (pos + neg) / (2 * divisor),
             min = ifelse(uniform, mean - neg, NA_real_),
             max = ifelse(uniform, mean + pos, NA_real_))
}

fields <- c(
  unique_id = "unique_id",
  sample_name = "full_sample_name_in_source",
  doi = "doi",
  site_lat = "site_latitude",
  site_lon = "site_longitude",
  age = "age_ka_bp",
  age_pos = "age_uncertainty_pos_ka",
  age_neg = "age_uncertainty_neg_ka",
  age_dist = "age_uncertainty_distribution",
  age_ref = "age_model_reference",
  age_notes = "age_model_notes",
  organic_material = "organic_d13C_material_name",
  organic_d13C = "organic_d13C_measured_permil",
  organic_unc = "organic_d13C_measured_uncertainty_permil",
  organic_unc_type = "organic_d13C_measured_uncertainty_type",
  sst = "SST_degC",
  sst_pos = "SST_degC_uncertainty_pos",
  sst_neg = "SST_degC_uncertainty_neg",
  sst_unc_type = "SST_degC_uncertainty_type",
  sst_source = "SST_degC_source",
  phosphate = "phosphate_umol_kg",
  phosphate_pos = "phosphate_umol_kg_uncertainty_pos",
  phosphate_neg = "phosphate_umol_kg_uncertainty_neg",
  phosphate_unc_type = "phosphate_unertainty_type",
  phosphate_ref = "phosphate_reference",
  vsa_lith = "VSA_lith_size_um",
  vsa_pos = "VSA_lith_size_uncertainty_pos_um",
  vsa_neg = "VSA_lith_size_uncertainty_neg_um",
  vsa_unc_type = "VSA_lith_size_uncertainty_type",
  vsa_cell_radius = "VSA_cell_radius_um",
  vsa_cell_radius_method = "VSA_cell_radius_method",
  include_parent = "include_parent_unique_id",
  include_child = "include_child_unique_id",
  compilation_notes = "compilation_notes"
)

numeric_fields <- c("site_lat", "site_lon", "age", "age_pos", "age_neg",
                    "organic_d13C", "organic_unc", "sst", "sst_pos", "sst_neg",
                    "phosphate", "phosphate_pos", "phosphate_neg", "vsa_lith",
                    "vsa_pos", "vsa_neg", "vsa_cell_radius")
uncertainties <- list(age_dist = c("age_pos", "age_neg"),
                      organic_unc_type = "organic_unc",
                      sst_unc_type = c("sst_pos", "sst_neg"),
                      phosphate_unc_type = c("phosphate_pos", "phosphate_neg"),
                      vsa_unc_type = c("vsa_pos", "vsa_neg"))

for (sf in src_files) {

  message("Reading: ", basename(sf))

  dat <- read.xlsx(sf, sheet = source_sheet, startRow = 3, colNames = TRUE,
                   skipEmptyCols = FALSE, skipEmptyRows = FALSE)
  keep <- apply(dat, 1, function(r) any(!is.na(r) & trimws(as.character(r)) != ""))
  source_rows <- which(keep) + 3L
  dat <- dat[keep, , drop = FALSE]
  if (!nrow(dat)) next

  n <- nrow(dat)

  names(dat)[names(dat) == "Modern.Latitude.(decimal.degree,.south.negative)"] <- "site_latitude"
  names(dat)[names(dat) == "Modern.Longitude.(decimal.degree,.west.negative)"] <- "site_longitude"
  doi_cols <- which(names(dat) == "doi")
  if (length(doi_cols) > 1) {
    for (j in doi_cols[-1]) {
      if (!identical(as_chr(dat[[doi_cols[1]]]), as_chr(dat[[j]])))
        stop("Conflicting DOI columns in: ", sf)
      names(dat)[j] <- paste0("doi_reference_", j)
    }
  }
  required <- c(unname(fields), "include_TF", "include_explanation")
  if (any(!required %in% names(dat)) || anyDuplicated(names(dat)[names(dat) %in% required]))
    stop("Missing or duplicated phytoplankton headers in: ", sf)
  src_col <- function(x) dat[[fields[[x]]]]
  file_id <- sub("^phytoplankton_", "", tools::file_path_sans_ext(basename(sf)))
  out_file <- file.path(out_dir, paste0("phyto_Intermediate_", file_id, ".xlsx"))

  add_issue <- function(field, rows, problem) {
    if (length(rows)) issues[[length(issues) + 1L]] <<- data.frame(
      file = basename(sf), source_row = source_rows[rows], field = field,
      value = as.character(dat[[fields[[field]]]][rows]), problem = problem)
  }
  for (field in numeric_fields) {
    raw <- as_chr(src_col(field))
    note <- which(!is.na(raw) & grepl("^[+-]?[0-9.]+ +note:", raw))
    add_issue(field, note, "Numeric value read before note; original text retained here")
    raw[note] <- sub(" +note:.*", "", raw[note])
    bad <- which(!is.na(raw) & is.na(as_num(raw)))
    if (length(bad)) stop("Non-numeric ", field, " in ", sf, " rows ", paste(source_rows[bad], collapse = ", "))
    dat[[fields[[field]]]] <- as_num(raw)
  }
  type_field <- fields[["organic_unc_type"]]
  n_field <- "organic_d13C_measured_uncertainty_n"
  if (n_field %in% names(dat)) {
    misplaced <- is.na(as_chr(dat[[type_field]])) &
      clean_type(dat[[n_field]]) %in% c("1std", "1sem", "2std", "2sem", "uniform", "range")
    dat[[type_field]][misplaced] <- dat[[n_field]][misplaced]
  }
  if (basename(sf) == "phytoplankton_rae_2021.xlsx") {
    corrected <- which(clean_type(dat$SST_degC_uncertainty_type) == "5to97.5percentile")
    dat$SST_degC_uncertainty_type[corrected] <- "2.5 to 97.5 percentile"
  }
  for (field in names(uncertainties)) {
    type <- clean_type(src_col(field))
    present <- rowSums(!is.na(dat[unname(fields[uncertainties[[field]]])])) > 0
    known <- type %in% c("1std", "1sem", "1sd", "1se", "2std", "2sem", "2sd", "2se",
                        "normal", "normaldistribution", "setvalue", "uniform", "range",
                        "uniformdistribution", "5to95percentile", "2.5to97.5percentile")
    add_issue(field, which(present & !known), "Uncertainty magnitude present but its scale/distribution is unspecified")
  }
  wb <- loadWorkbook(template_file)

  blank_row1 <- as.data.frame(matrix("", nrow = 1, ncol = 36))
  writeData(wb, template_sheet, blank_row1,
            startRow = 1, startCol = 1, colNames = FALSE, rowNames = FALSE)

  # mapping 
  unique_id <- as_chr(src_col("unique_id"))
  sample_name <- as_chr(src_col("sample_name"))
  colA <- unique_id
  use_sample_name <- (is.na(unique_id) | trimws(as.character(unique_id)) == "") &
    (!is.na(sample_name) & trimws(as.character(sample_name)) != "")
  colA[use_sample_name] <- sample_name[use_sample_name]

  colB <- src_col("doi")

  colC <- rep(paste0(archive_dir, basename(sf)), n)

  colD <- rep("Harper and Giulivi", n)

  colE <- rep("dustin.t.harper@utah.edu; claudiag@ldeo.columbia.edu", n)

  colF <- src_col("site_lat")

  colG <- src_col("site_lon")

  colH <- as_num(src_col("age")) / 1e3

  age_unc <- uncertainty(colH, src_col("age_pos") / 1e3,
                         src_col("age_neg") / 1e3, src_col("age_dist"))
  colI <- age_unc$error
  colJ <- age_unc$min
  colK <- age_unc$max

  AK <- as_chr(src_col("age_ref"))
  AL <- as_chr(src_col("age_notes"))
  colL <- ifelse(is.na(AK), AL, ifelse(is.na(AL), AK, paste(AK, AL, sep = "; ")))

  colM <- src_col("organic_material")

  colN <- src_col("organic_d13C")

  organic_unc <- uncertainty(colN, src_col("organic_unc"), src_col("organic_unc"),
                             src_col("organic_unc_type"))
  colO <- organic_unc$error
  colP <- organic_unc$min
  colQ <- organic_unc$max

  colR <- src_col("sst")
  sst_unc <- uncertainty(colR, src_col("sst_pos"), src_col("sst_neg"), src_col("sst_unc_type"))
  colS <- sst_unc$error
  colT <- sst_unc$min
  colU <- sst_unc$max

  colV <- src_col("sst_source")

  colW <- src_col("phosphate")
  phosphate_unc <- uncertainty(colW, src_col("phosphate_pos"), src_col("phosphate_neg"),
                               src_col("phosphate_unc_type"))
  colX <- phosphate_unc$error
  colY <- phosphate_unc$min
  colZ <- phosphate_unc$max

  colAA <- src_col("phosphate_ref")

  colAB <- src_col("vsa_lith")
  lith_unc <- uncertainty(colAB, src_col("vsa_pos"), src_col("vsa_neg"),
                          src_col("vsa_unc_type"))
  colAC <- lith_unc$error
  colAD <- lith_unc$min
  colAE <- lith_unc$max

  colAF <- src_col("vsa_cell_radius")

  colAG <- src_col("vsa_cell_radius_method")

  colAH <- as_chr(src_col("include_parent"))

  colAI <- as_chr(src_col("include_child"))

  colAJ <- src_col("compilation_notes")

  out_df <- data.frame(
    A  = colA,  B  = colB,  C  = colC,  D  = colD,  E  = colE,
    F  = colF,  G  = colG,  H  = colH,  I  = colI,  J  = colJ,
    K  = colK,  L  = colL,  M  = colM,  N  = colN,  O  = colO,
    P  = colP,  Q  = colQ,  R  = colR,  S  = colS,  T  = colT,
    U  = colU,  V  = colV,  W  = colW,  X  = colX,  Y  = colY,
    Z  = colZ,  AA = colAA, AB = colAB, AC = colAC, AD = colAD,
    AE = colAE, AF = colAF, AG = colAG, AH = colAH, AI = colAI,
    AJ = colAJ,
    stringsAsFactors = FALSE
  )

  class(out_df$C) <- "hyperlink"

  writeData(wb, template_sheet, out_df,
            startRow = 5, startCol = 1,
            colNames = FALSE, rowNames = FALSE, keepNA = TRUE)

  saveWorkbook(wb, out_file, overwrite = TRUE)

  generated_data[[basename(sf)]] <- out_df
  metadata[[basename(sf)]] <- data.frame(
    file = basename(sf), source_row = source_rows, intermediate_row = seq_len(n) + 4L,
    unique_id = unique_id, sample_name = sample_name, parent_id = colAH, child_id = colAI,
    include_TF = as_chr(dat$include_TF), include_explanation = as_chr(dat$include_explanation),
    publication_year = as_num(gsub("[^0-9]", "", as.character(dat$publication_year))),
    age_Ma = colH, lat = colF, lon = colG, material = colM,
    d13Corg = colN, tempC = colR,
    compilation_notes = as_chr(colAJ), stringsAsFactors = FALSE)

}

# combine intermediate sheets
combined_file <- file.path(out_dir, "phyto_Intermediate_combined.xlsx")

if (length(generated_data)) {

  combined_dat <- do.call(rbind, generated_data)
  class(combined_dat$C) <- "hyperlink"

  wb_combined <- loadWorkbook(template_file)

  blank_row1 <- as.data.frame(matrix("", nrow = 1, ncol = 36))
  writeData(wb_combined, template_sheet, blank_row1,
            startRow = 1, startCol = 1, colNames = FALSE, rowNames = FALSE)

  writeData(wb_combined, template_sheet, combined_dat,
            startRow = 5, startCol = 1,
            colNames = FALSE, rowNames = FALSE, keepNA = TRUE)

  saveWorkbook(wb_combined, combined_file, overwrite = TRUE)
}

# duplicate metadata
id_key <- function(x) sub("^algae_", "phytoplankton_", tolower(as_chr(x)))

link_report <- function(m) {
  key <- id_key(m$unique_id)
  links <- list()
  for (field in c("parent_id", "child_id")) for (i in which(!is.na(m[[field]]))) {
    targets <- as_chr(unlist(strsplit(m[[field]][i], "[;,|]")))
    for (target in targets[!is.na(targets)]) {
      hit <- which(!is.na(key) & key == id_key(target))
      status <- if (!length(hit)) "not in batch" else if (length(hit) > 1) "ambiguous ID" else if (hit == i) "self link" else "matched"
      if (!length(hit)) hit <- NA_integer_
      for (j in hit) links[[length(links) + 1L]] <- data.frame(
        record = i, linked_record = j,
        file = m$file[i], source_row = m$source_row[i], unique_id = m$unique_id[i],
        field = field, linked_id = target, status = status,
        linked_file = m$file[j], linked_source_row = m$source_row[j],
        matched_id = m$unique_id[j],
        age_difference_Ma = abs(m$age_Ma[i] - m$age_Ma[j]),
        latitude_difference = abs(m$lat[i] - m$lat[j]),
        longitude_difference = abs(m$lon[i] - m$lon[j]),
        direction_review = if (is.na(j)) NA else if (field == "parent_id")
          m$publication_year[j] > m$publication_year[i] else
          m$publication_year[j] < m$publication_year[i],
        include_TF = m$include_TF[i],
        linked_include_TF = m$include_TF[j], explanation = m$include_explanation[i],
        linked_explanation = m$include_explanation[j], stringsAsFactors = FALSE)
    }
  }
  if (length(links)) do.call(rbind, links) else data.frame(
    record=integer(), linked_record=integer(),
    file=character(), source_row=integer(), unique_id=character(), field=character(),
    linked_id=character(), status=character(), linked_file=character(),
    linked_source_row=integer(), matched_id=character(), age_difference_Ma=numeric(),
    latitude_difference=numeric(), longitude_difference=numeric(), direction_review=logical(), include_TF=character(),
    linked_include_TF=character(), explanation=character(), linked_explanation=character())
}

same_measurements <- function(d, i, j) {
  if (is.na(i) || is.na(j) || i == j) return(FALSE)
  cols <- c("M", LETTERS[14:21], LETTERS[23:26], "AB", "AC", "AD", "AE", "AF")
  if (is.na(d$N[i]) && is.na(d$AB[i])) return(FALSE)
  all(vapply(cols, function(co) {
    a <- d[[co]][i]; b <- d[[co]][j]
    if (is.numeric(d[[co]])) isTRUE(all.equal(a, b, tolerance = 1e-8)) else
      identical(as_chr(a), as_chr(b))
  }, logical(1)))
}

if (length(metadata)) {
  m <- do.call(rbind, metadata)
  m$combined_row <- seq_len(nrow(m)) + 4L
  key <- id_key(m$unique_id)
  repeated <- !is.na(key) & (duplicated(key) | duplicated(key, fromLast = TRUE))
  links <- link_report(m)
  links$same_measurements <- mapply(function(i, j) same_measurements(combined_dat, i, j),
                                    links$record, links$linked_record)
  pairs <- links[which(links$same_measurements), c("record", "linked_record")]
  for (id in unique(key[repeated])) {
    rows <- which(!is.na(key) & key == id)
    p <- t(combn(rows, 2))
    keep <- apply(p, 1, function(r) same_measurements(combined_dat, r[1], r[2]))
    if (any(keep)) pairs <- rbind(pairs, data.frame(record = p[keep, 1], linked_record = p[keep, 2]))
  }
  if (nrow(pairs)) pairs <- unique(data.frame(record = pmin(pairs$record, pairs$linked_record),
                                             linked_record = pmax(pairs$record, pairs$linked_record)))
  left <- m[pairs$record, ]; right <- m[pairs$linked_record, ]
  names(left) <- paste0(names(left), "_1"); names(right) <- paste0(names(right), "_2")
  candidates <- data.frame(left, right, row.names = NULL)
  write.csv(m, file.path(out_dir, "phyto_record_metadata.csv"), row.names = FALSE, na = "")
  write.csv(candidates, file.path(out_dir, "phyto_duplicate_candidates.csv"), row.names = FALSE, na = "")
  write.csv(links[setdiff(names(links), c("record", "linked_record"))], file.path(out_dir, "phyto_parent_child_links.csv"), row.names = FALSE, na = "")
  problems <- if (length(issues)) unique(do.call(rbind, issues)) else data.frame(
    file=character(), source_row=integer(), field=character(), value=character(), problem=character())
  write.csv(problems, file.path(out_dir, "phyto_data_issues.csv"), row.names = FALSE, na = "")
  message(nrow(m), " rows; ", nrow(candidates), " measurement-matched candidate pairs; ",
          nrow(links), " link records; ", nrow(problems), " data issues")
}

cat("Done. Wrote outputs to: ", normalizePath(out_dir), "\n", sep = "")

if (file.exists(combined_file)) {
  cat("Combined file: ", normalizePath(combined_file), "\n", sep = "")
}


