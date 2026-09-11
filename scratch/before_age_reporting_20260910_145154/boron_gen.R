###############################################################################
#
# - Uses templates/boron_IntermediateTemplate.xlsx
# - For each xlsx in data_boron/:
#     * loads template workbook
#     * blanks out row 1; preserves formatting
#     * keeps headers in rows 2-4 intact
#     * writes data starting at row 5
#     * saves output_boron/boron_Intermediate_<author><year>.xlsx
# - Combines all intermediate data generated during the current run into:
#     * output_boron/boron_Intermediate_combined.xlsx
#
# Requires: openxlsx
###############################################################################

library(openxlsx)

###############################################################################
# paths 
###############################################################################
template_file <- file.path("templates", "boron_IntermediateTemplate.xlsx")
template_sheet <- "data4PSM"

source_dir <- "data_boron"
source_sheet <- "boron isotopes"

out_dir <- "output_boron"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

src_files <- list.files(source_dir, pattern = "\\.xlsx$", full.names = TRUE)
src_files <- src_files[!grepl("^~\\$", basename(src_files))]
src_files <- src_files[file.exists(src_files)]

if (!length(src_files)) stop("No .xlsx files found in: ", source_dir)
if (!file.exists(template_file)) stop("Template not found: ", template_file)

generated_files <- character()
generated_data <- list()

###############################################################################
# helpers 
###############################################################################
xl_col <- function(x) {
  x <- toupper(gsub("[^A-Z]", "", x))
  if (!nzchar(x)) return(NA_integer_)
  chars <- strsplit(x, "", fixed = TRUE)[[1]]
  idx <- 0L
  for (ch in chars) idx <- idx * 26L + (utf8ToInt(ch) - utf8ToInt("A") + 1L)
  idx
}

get_col <- function(dat, let) {
  ii <- xl_col(let)
  if (is.na(ii) || ii < 1L || ii > ncol(dat)) return(rep(NA, nrow(dat)))
  dat[[ii]]
}

as_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

as_chr <- function(x) {
  y <- as.character(x)
  y[y %in% c("NA", "NaN", "NULL")] <- NA_character_
  y
}

norm_type <- function(x) {
  z <- tolower(trimws(as.character(x)))
  z %in% c("normal distribution", "set value", "5 to 95 percentile", "normal")
}

unif_type <- function(x) {
  z <- tolower(trimws(as.character(x)))
  z %in% c("uniform distribution", "range", "uniform")
}

###############################################################################
# main loop
###############################################################################
for (sf in src_files) {
  
  message("Reading: ", basename(sf))
  
  # Source data
  sheets <- getSheetNames(sf)
  source_sheet_i <- if (source_sheet %in% sheets) {
    source_sheet
  } else if ("4. boron isotopes" %in% sheets) {
    "4. boron isotopes"
  } else {
    stop("Could not find boron isotope sheet in: ", sf)
  }
  
  dat <- read.xlsx(sf, sheet = source_sheet_i, startRow = 3, colNames = TRUE)
  
  # drop completely empty rows
  keep <- apply(dat, 1, function(r) any(!is.na(r) & trimws(as.character(r)) != ""))
  dat <- dat[keep, , drop = FALSE]
  if (!nrow(dat)) next
  
  n <- nrow(dat)
  
  # output name parts
  author <- if ("first_author_last_name" %in% names(dat)) as.character(dat$first_author_last_name[1]) else ""
  year <- if ("publication_year" %in% names(dat)) as.character(dat$publication_year[1]) else ""
  
  author <- gsub("[^A-Za-z0-9]+", "_", author)
  year <- gsub("[^0-9]+", "", year)
  if (!nzchar(author)) author <- gsub("[^A-Za-z0-9]+", "_", tools::file_path_sans_ext(basename(sf)))
  
  out_file <- file.path(out_dir, paste0("boron_Intermediate_", author, year, ".xlsx"))
  
  # load template
  wb <- loadWorkbook(template_file)
  
  # remove mapping notes 
  blank_row1 <- as.data.frame(matrix("", nrow = 1, ncol = 25))
  writeData(wb, template_sheet, blank_row1, startRow = 1, startCol = 1,
            colNames = FALSE, rowNames = FALSE)
  
  ###############################################################################
  # mapping A:Y
  ###############################################################################
  # A = concatenate O,P,Q,R,S,T with underscores between P&Q, Q&R, R&S, S&T
  O <- as_chr(get_col(dat, "O"))
  P <- as_chr(get_col(dat, "P"))
  Q <- as_chr(get_col(dat, "Q"))
  R <- as_chr(get_col(dat, "R"))
  Sx <- as_chr(get_col(dat, "S"))
  Tx <- as_chr(get_col(dat, "T"))
  colA <- paste0(O, P, "_", Q, "_", R, "_", Sx, "_", Tx)
  
  colB <- get_col(dat, "D")
  colC <- rep("please enter value manually", n)
  colD <- rep("Harper and Giulivi", n)
  colE <- rep("dustin.t.harper@utah.edu; claudiag@ldeo.columbia.edu", n)
  colF <- get_col(dat, "Y")
  colG <- get_col(dat, "Z")
  colH <- get_col(dat, "AG")
  
  AH <- get_col(dat, "AH")
  AI <- as_num(get_col(dat, "AI"))
  AJ <- as_num(get_col(dat, "AJ"))
  AG <- as_num(get_col(dat, "AG"))
  
  colI <- rep(NA_real_, n)
  ii <- norm_type(AH)
  colI[ii] <- (AI[ii] + AJ[ii]) / 2
  
  colJ <- rep(NA_real_, n)
  jj <- unif_type(AH)
  colJ[jj] <- AG[jj] - AJ[jj]
  
  colK <- rep(NA_real_, n)
  colK[jj] <- AG[jj] + AI[jj]
  
  colL <- get_col(dat, "AM")
  colM <- get_col(dat, "AU")
  colN <- get_col(dat, "AV")
  colO2 <- get_col(dat, "AW")
  colP2 <- get_col(dat, "BA")
  colQ2 <- get_col(dat, "BE")
  colR2 <- get_col(dat, "BG")
  
  CC <- get_col(dat, "CC")
  CB <- get_col(dat, "CB")
  colS <- CC
  flagS <- (is.na(CC) | trimws(as.character(CC)) == "") &
    (!is.na(CB) & trimws(as.character(CB)) != "")
  
  CE <- as_num(get_col(dat, "CE"))
  CF <- as_num(get_col(dat, "CF"))
  colT <- (CE + CF) / 2
  
  colU <- get_col(dat, "BX")
  
  BY <- get_col(dat, "BY")
  BXn <- as_num(get_col(dat, "BX"))
  BZ <- as_num(get_col(dat, "BZ"))
  CA <- as_num(get_col(dat, "CA"))
  
  colV <- rep(NA_real_, n)
  vv <- norm_type(BY)
  colV[vv] <- (BZ[vv] + CA[vv]) / 2
  
  colW <- rep(NA_real_, n)
  ww <- unif_type(BY)
  colW[ww] <- BXn[ww] - CA[ww]
  
  colX <- rep(NA_real_, n)
  colX[ww] <- BXn[ww] + BZ[ww]
  
  colY <- get_col(dat, "FQ")
  
  out_df <- data.frame(
    A = colA, B = colB, C = colC, D = colD, E = colE,
    F = colF, G = colG, H = colH/1000, I = colI/1000, J = colJ/1000,
    K = colK/1000, L = colL, M = colM, N = colN, O = colO2,
    P = colP2, Q = colQ2, R = colR2, S = colS, T = colT,
    U = colU, V = colV, W = colW, X = colX, Y = colY,
    stringsAsFactors = FALSE
  )
  
  # headers are rows 2-4 in the template -> data start at row 5
  writeData(wb, template_sheet, out_df,
            startRow = 5, startCol = 1,
            colNames = FALSE, rowNames = FALSE, keepNA = TRUE)
  
  # Flag Column S red (S = 19)
  if (any(flagS)) {
    red <- createStyle(fontColour = "#FF0000")
    addStyle(
      wb, template_sheet, red,
      rows = which(flagS) + 4,  # first data row is 5
      cols = 19,
      gridExpand = FALSE, stack = TRUE
    )
  }
  
  saveWorkbook(wb, out_file, overwrite = TRUE)
  
  generated_files <- c(generated_files, out_file)
  generated_data[[basename(out_file)]] <- out_df
}

###############################################################################
# combine intermediate sheets
###############################################################################
combined_file <- file.path(out_dir, "boron_Intermediate_combined.xlsx")

if (length(generated_data)) {
  
  combined_dat <- do.call(rbind, generated_data)
  names(combined_dat) <- paste0("V", seq_len(ncol(combined_dat)))
  
  wb_combined <- loadWorkbook(template_file)
  
  blank_row1 <- as.data.frame(matrix("", nrow = 1, ncol = 25))
  writeData(wb_combined, template_sheet, blank_row1,
            startRow = 1, startCol = 1,
            colNames = FALSE, rowNames = FALSE)
  
  writeData(wb_combined, template_sheet, combined_dat,
            startRow = 5, startCol = 1,
            colNames = FALSE, rowNames = FALSE, keepNA = TRUE)
  
  saveWorkbook(wb_combined, combined_file, overwrite = TRUE)
}

cat("Done. Wrote outputs to: ", normalizePath(out_dir), "\n", sep = "")

if (file.exists(combined_file)) {
  cat("Combined file: ", normalizePath(combined_file), "\n", sep = "")
}


