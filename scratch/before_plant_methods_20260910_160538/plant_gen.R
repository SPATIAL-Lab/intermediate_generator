# age range in Ma (youngest, oldest); c(-Inf, Inf) includes all dated samples
age_range_Ma <- c(0, 23.03)

###############################################################################
#
# - Uses templates/stomata_IntermediateTemplate.xlsx
# - For each xlsx in data_plant/:
#     * loads template workbook
#     * BLANKS OUT row 1; preserves formatting
#     * keeps headers in rows 2-4 intact
#     * writes data starting at row 5
#     * saves output_plant/stomata_Intermediate_<author><year>.xlsx
#
# Optional for Column M: taxize (fallback works without it)
###############################################################################

library(openxlsx)
#library(taxize)

###############################################################################
# paths
###############################################################################
template_file <- file.path("templates", "stomata_IntermediateTemplate.xlsx")
template_sheet <- "data4PSM"         

source_dir <- "data_plant"
source_sheet <- "leaf gas-exchange_Franks"    

out_dir <- "output_plant"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
proxy <- "plant"
output_prefix <- "stomata"
age_divisor <- 1
source("generator_checks.R", local = TRUE)

src_files <- list.files(source_dir, pattern = "\\.xlsx$", full.names = TRUE)
src_files <- src_files[!grepl("^~\\$", basename(src_files))]
if (!length(src_files)) stop("No .xlsx files found in: ", source_dir)
if (!file.exists(template_file)) stop("Template not found: ", template_file)

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

as_chr <- function(x) {
  y <- as.character(x)
  y[y %in% c("NA", "NaN", "NULL")] <- NA_character_
  y
}

is_missing <- function(x) {
  z <- trimws(as.character(x))
  is.na(x) | is.na(z) | z == "" | toupper(z) == "NA"
}

# Column M: plant group name resolver
# Uses taxize if installed; otherwise returns "Family / Genus" fallback.
make_group_name <- function(family, genus) {
  fam <- trimws(as_chr(family))
  gen <- trimws(as_chr(genus))
  
  out <- rep(NA_character_, length(fam))
  
  # fallback first 
  both <- !is_missing(fam) & !is_missing(gen)
  out[both] <- paste0(fam[both], " / ", gen[both])
  only_f <- !is_missing(fam) &  is_missing(gen)
  out[only_f] <- fam[only_f]
  only_g <- is_missing(fam) & !is_missing(gen)
  out[only_g] <- gen[only_g]
  
  # if taxize is available, try to improve labels
  if (requireNamespace("taxize", quietly = TRUE)) {
    # Best effort: build a "Genus species" query using Genus only
    try({
      # attempt to standardize genus names via GNR 
      g <- gen
      ok <- !is_missing(g)
      if (any(ok)) {
        res <- taxize::gnr_resolve(names = unique(g[ok]), best_match_only = TRUE)
        if (nrow(res)) {
          m <- match(g, res$user_supplied_name)
          repl <- res$matched_name2[m]
          good <- !is.na(repl) & nzchar(repl)
          out[ok & good] <- paste0(fam[ok & good], " / ", repl[ok & good])
        }
      }
    }, silent = TRUE)
  }
  
  out
}

###############################################################################
# main loop
###############################################################################
for (sf in src_files) {
  
  if (!source_sheet %in% getSheetNames(sf)) {
    report_action("sheet", NA_integer_, paste(getSheetNames(sf), collapse="; "),
                 "Unsupported plant method; no Franks sheet, workbook not generated")
    next
  }
  # Source data: assuming headers on row 3 
  dat <- read.xlsx(sf, sheet = source_sheet, startRow = 3, colNames = TRUE,
                   skipEmptyCols = FALSE, skipEmptyRows = FALSE)
  
  # drop completely empty rows
  keep <- apply(dat, 1, function(r) any(!is.na(r) & trimws(as.character(r)) != ""))
  source_rows <- which(keep) + 3L
  dat <- dat[keep, , drop = FALSE]
  if (!nrow(dat)) next
  
  if (!identical(names(dat)[22], "Age.(Ma)")) {
    report_issue("age header", NA_integer_, names(dat)[22], "Unexpected age column; generation stopped")
    stop("Check age column in: ", sf)
  }
  age_keep <- select_age(dat[[22]], source_rows)
  dat <- dat[age_keep, , drop = FALSE]
  source_rows <- source_rows[age_keep]
  if (!nrow(dat)) next

  n <- nrow(dat)
  
  # output name parts
  author <- if ("first_author_last_name" %in% names(dat)) as.character(dat$first_author_last_name[1]) else ""
  year <- if ("publication_year"      %in% names(dat)) as.character(dat$publication_year[1])      else ""
  
  author <- iconv(author, to = "ASCII//TRANSLIT")
  author <- gsub("[^A-Za-z0-9]+", "_", author)
  year <- gsub("[^0-9]+", "", year)
  if (!nzchar(author)) author <- gsub("[^A-Za-z0-9]+", "_", tools::file_path_sans_ext(basename(sf)))
  
  file_id <- tools::file_path_sans_ext(basename(sf))
  file_id <- iconv(file_id, to = "ASCII//TRANSLIT")
  file_id <- gsub("[^A-Za-z0-9]+", "_", file_id)
  
  out_file <- file.path(out_dir, paste0("stomata_Intermediate_", file_id, ".xlsx"))
  
  wb <- loadWorkbook(template_file)
  
  # mapping row 1 
  blank_row1 <- as.data.frame(matrix("", nrow = 1, ncol = 63))
  writeData(wb, template_sheet, blank_row1,
            startRow = 1, startCol = 1, colNames = FALSE, rowNames = FALSE)
  
  # 
  # mapping
  colA <- get_col(dat, "O")
  colB <- get_col(dat, "D")
  colC <- rep("please enter value manually", n)
  colD <- rep("Harper and Giulivi", n)
  colE <- rep("dustin.t.harper@utah.edu; claudiag@ldeo.columbia.edu", n)
  colF <- get_col(dat, "AA")
  colG <- get_col(dat, "AB")
  colH <- get_col(dat, "V")
  colI <- rep(NA, n)     # repeat NA
  colJ <- get_col(dat, "X")
  colK <- get_col(dat, "W")
  colL <- get_col(dat, "Z")
  
  # M = plant group name from family (P) and genus (Q)
  colM  <- make_group_name(get_col(dat, "P"), get_col(dat, "Q"))
  
  colN <- get_col(dat, "P")
  colO <- get_col(dat, "Q")
  colP <- get_col(dat, "AM")
  colQ <- get_col(dat, "AN")
  colR <- get_col(dat, "AO")
  colS <- get_col(dat, "AP")
  colT <- get_col(dat, "AQ")
  colU <- get_col(dat, "AR")
  colV <- get_col(dat, "AS")
  colW <- get_col(dat, "AT")
  colX <- get_col(dat, "AU")
  colY <- get_col(dat, "AV")
  colZ <- get_col(dat, "AW")
  colAA <- get_col(dat, "AX")
  colAB <- get_col(dat, "AY")
  colAC <- get_col(dat, "AZ")
  colAD <- get_col(dat, "BA")
  colAE <- get_col(dat, "BB")
  colAF <- get_col(dat, "BC")
  colAG <- get_col(dat, "BD")
  colAH <- get_col(dat, "BE")
  colAI <- get_col(dat, "BF")
  colAJ <- get_col(dat, "BG")
  colAK <- get_col(dat, "BK")
  colAL <- get_col(dat, "CL")
  colAM <- get_col(dat, "BL")
  colAN <- get_col(dat, "BM")
  colAO <- get_col(dat, "BN")
  colAP <- get_col(dat, "BO")
  colAQ <- get_col(dat, "BP")
  colAR <- get_col(dat, "BQ")
  colAS <- get_col(dat, "BR")
  colAT <- get_col(dat, "BS")
  colAU <- get_col(dat, "BT")
  colAV <- get_col(dat, "BU")
  colAW <- get_col(dat, "BV")
  colAX <- get_col(dat, "BW")
  colAY <- get_col(dat, "BX")
  colAZ <- get_col(dat, "BY")
  colBA <- get_col(dat, "BZ")
  colBB <- get_col(dat, "CA")
  colBC <- get_col(dat, "CB")
  colBD <- get_col(dat, "CC")
  colBE <- get_col(dat, "CD")
  colBF <- get_col(dat, "CE")
  colBG <- get_col(dat, "CF")
  colBH <- get_col(dat, "CG")
  colBI <- get_col(dat, "CH")
  colBJ <- get_col(dat, "CI")
  colBK <- rep("no notes :)", n)
  
  out_df <- data.frame(
    A=colA, B=colB, C=colC, D=colD, E=colE, F=colF, G=colG, H=colH, I=colI, J=colJ,
    K=colK, L=colL, M=colM, N=colN, O=colO, P=colP, Q=colQ, R=colR, S=colS, T=colT,
    U=colU, V=colV, W=colW, X=colX, Y=colY, Z=colZ, AA=colAA, AB=colAB, AC=colAC,
    AD=colAD, AE=colAE, AF=colAF, AG=colAG, AH=colAH, AI=colAI, AJ=colAJ, AK=colAK,
    AL=colAL, AM=colAM, AN=colAN, AO=colAO, AP=colAP, AQ=colAQ, AR=colAR, AS=colAS,
    AT=colAT, AU=colAU, AV=colAV, AW=colAW, AX=colAX, AY=colAY, AZ=colAZ, BA=colBA,
    BB=colBB, BC=colBC, BD=colBD, BE=colBE, BF=colBF, BG=colBG, BH=colBH, BI=colBI,
    BJ=colBJ, BK=colBK,
    stringsAsFactors = FALSE
  )
  
  # headers are rows 2-4 in the template -> data start at row 5
  check_output(out_df, source_rows, c("F", "G", "H", "I", "J", "K", int2col(setdiff(16:62, c(seq(18, 36, 3), seq(41, 62, 3))))), c("P", "S", "V", "Y", "AB", "AE", "AH"))

  writeData(wb, template_sheet, out_df,
            startRow = 5, startCol = 1,
            colNames = FALSE, rowNames = FALSE, keepNA = TRUE)
  
  if (out_file %in% current_outputs) stop("Output filename collision: ", out_file)
  saveWorkbook(wb, out_file, overwrite = TRUE)
  current_outputs <- c(current_outputs, out_file)
  study_outputs[basename(sf)] <- out_file
}

finish_reports()

cat("Done. Wrote outputs to: ", normalizePath(out_dir), "\n", sep = "")
