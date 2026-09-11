library(openxlsx)

# paths 
template_file <- file.path("templates", "paleosol_IntermediateTemplate.xlsx")
template_sheet <- "data4PSM"

source_dir <- "data_paleosol"
source_sheet <- "paleosol data"
archive_dir <- "https://www.ncei.noaa.gov/pub/data/paleo/climate_forcing/trace_gases/Paleo-pCO2/"
archive_files <- c("paleosol_cotton_2012.xlsx", "paleosol_da_2015.xlsx",
                   "paleosol_da_2019.xlsx", "paleosol_ji_2018.xlsx")

out_dir <- "output_paleosol"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

src_files <- list.files(source_dir, pattern = "\\.xlsx$", full.names = TRUE)
src_files <- src_files[!grepl("^~\\$", basename(src_files))]
if (!length(src_files)) stop("No .xlsx files found in: ", source_dir)
if (!file.exists(template_file)) stop("Template not found: ", template_file)
if (any(!basename(src_files) %in% archive_files))
  stop("Check NOAA archive filenames for: ",
       paste(basename(src_files)[!basename(src_files) %in% archive_files], collapse = ", "))

# helpers 
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

is_missing <- function(x) {
  z <- trimws(as.character(x))
  is.na(x) | is.na(z) | z == "" | toupper(z) == "NA"
}

is_norm <- function(x) {
  z <- tolower(trimws(as.character(x)))
  z %in% c("normal distribution", "set value", "5 to 95 percentile", "normal")
}
is_unif <- function(x) {
  z <- tolower(trimws(as.character(x)))
  z %in% c("uniform distribution", "range", "uniform")
}

# duplicate screening
find_duplicates <- function(x, site_tolerance = 0.01, isotope_tolerance = 1e-6) {
  same <- function(a, b) !is_missing(a) & !is_missing(b) & a == b
  close <- function(a, b, tolerance) !is.na(a) & !is.na(b) & abs(a - b) <= tolerance
  pairs <- data.frame(i = integer(), j = integer(), reason = character())
  if (nrow(x) > 1) for (i in seq_len(nrow(x) - 1)) {
    j <- seq.int(i + 1, nrow(x))
    site <- close(x$lat[i], x$lat[j], site_tolerance) &
            close(x$lon[i], x$lon[j], site_tolerance)
    id <- same(x$sample[i], x$sample[j])
    isotope <- close(x$d13Ccc[i], x$d13Ccc[j], isotope_tolerance) &
      (close(x$d13Com_occluded[i], x$d13Com_occluded[j], isotope_tolerance) |
       close(x$d13Com_bulk[i], x$d13Com_bulk[j], isotope_tolerance))
    depth <- same(x$formation[i], x$formation[j]) &
             close(x$depth_m[i], x$depth_m[j], 1e-6)
    for (k in which(site & (id | isotope | depth))) {
      reason <- paste(c("sample ID", "carbonate + organic isotopes", "formation + depth")
                      [c(id[k], isotope[k], depth[k])], collapse = "; ")
      pairs <- rbind(pairs, data.frame(i = i, j = j[k], reason = reason))
    }
  }
  left <- x[pairs$i, , drop = FALSE]
  right <- x[pairs$j, , drop = FALSE]
  names(left) <- paste0(names(left), "_1")
  names(right) <- paste0(names(right), "_2")
  data.frame(reason = pairs$reason, left, right, row.names = NULL)
}

# main loop 
generated_data <- list()
screen_data <- list()
for (sf in src_files) {

  sheets <- getSheetNames(sf)
  source_sheet_i <- if (source_sheet %in% sheets) {
    source_sheet
  } else if ("paleosol" %in% sheets) {
    "paleosol"
  } else if ("paleosols" %in% sheets) {
    "paleosols"
  } else {
    stop("Could not find paleosol data sheet in: ", sf)
  }

  dat <- read.xlsx(sf, sheet = source_sheet_i, startRow = 3, colNames = TRUE,
                   skipEmptyCols = FALSE, skipEmptyRows = FALSE)

  keep <- apply(dat, 1, function(r) any(!is.na(r) & trimws(as.character(r)) != ""))
  source_rows <- which(keep) + 3L
  dat <- dat[keep, , drop = FALSE]
  if (!nrow(dat)) next

  n <- nrow(dat)

  author <- if ("first_author_last_name" %in% names(dat)) as.character(dat$first_author_last_name[1]) else ""
  year   <- if ("publication_year"      %in% names(dat)) as.character(dat$publication_year[1])      else ""

  author <- iconv(author, to = "ASCII//TRANSLIT")
  author <- gsub("[^A-Za-z0-9]+", "_", author)

  year   <- gsub("[^0-9]+", "", year)
  if (!nzchar(author)) author <- gsub("[^A-Za-z0-9]+", "_", tools::file_path_sans_ext(basename(sf)))

  out_file <- file.path(out_dir, paste0("paleosol_Intermediate_", author, year, ".xlsx"))

  wb <- loadWorkbook(template_file)

  blank_row1 <- as.data.frame(matrix("", nrow = 1, ncol = 31))
  writeData(wb, template_sheet, blank_row1,
            startRow = 1, startCol = 1, colNames = FALSE, rowNames = FALSE)

  # mapping
  early_layout <- all(c("Sample.ID", "Age.(Ma)", "Age_max.(Ma)", "Age_min.(Ma)",
                     "Temperature.of.calcium.carbonate.formation.(°C)", "Notes") ==
                   names(dat)[c(17, 24, 25, 26, 88, 134)])
  if (is.na(early_layout)) early_layout <- FALSE

  if (early_layout) {
    # Ji, Cotton and Da 2015
    cols <- c(A="Q", B="D", F="S", G="T", H="X", J="Z", K="Y", L="AA",
              N="DK", O="DN", P="DP", Q="AB", R="AC", S="AF", T="AG",
              U="AI", V="AJ", W="AM", X="AN", Y="AQ", Z="AR",
              AA="CJ", AB="CL", AC="CU", AD="CW", AE="ED")
    for (co in names(cols)) assign(paste0("col", co), get_col(dat, cols[[co]]))
    colA <- as_chr(colA)
    colC <- rep(paste0(archive_dir, basename(sf)), n)
    colD <- rep("Harper and Giulivi", n)
    colE <- rep("dustin.t.harper@utah.edu; claudiag@ldeo.columbia.edu", n)
    colI <- rep(NA_real_, n)
    colM <- rep(NA, n)

    # Temperature and MAP uncertainty to 2s
    for (co in c("AB", "AD")) {
      type_col <- if (co == "AB") "CN" else "CY"
      type <- gsub("[[:space:]]", "", tolower(as.character(get_col(dat, type_col))))
      value <- as_num(get(paste0("col", co)))
      one <- type %in% c("1sd", "1se", "1std", "1sem")
      two <- type %in% c("2sd", "2se", "2std", "2sem")
      if (any(!is.na(value) & !one & !two))
        stop("Unrecognized ", type_col, " uncertainty in: ", sf)
      value[one] <- value[one] * 2
      assign(paste0("col", co), value)
    }
  } else {
    later_layout <- all(c("Sample.ID", "Age.(Ma)", "paleosol.number",
                          "Temperature.of.calcium.carbonate.formation.(°C)",
                          "Soil.texture.(Grain.size)") ==
                        names(dat)[c(16, 26, 32, 93, 130)])
    if (!isTRUE(later_layout)) stop("Unrecognized paleosol layout in: ", sf)

    colA <- get_col(dat, "P")

    colB <- get_col(dat, "D")

    colC <- rep(paste0(archive_dir, basename(sf)), n)

    colD <- rep("Harper and Giulivi", n)

    colE <- rep("dustin.t.harper@utah.edu; claudiag@ldeo.columbia.edu", n)

    colF <- get_col(dat, "R")

    colG <- get_col(dat, "S")

    colH <- get_col(dat, "Z")

    ACtype <- get_col(dat, "AC")
    Z  <- as_num(get_col(dat, "Z"))
    AA <- as_num(get_col(dat, "AA"))
    AB <- as_num(get_col(dat, "AB"))

    colI <- rep(NA_real_, n)
    ii <- is_norm(ACtype)
    colI[ii] <- (AA[ii] + AB[ii]) / 2

    colJ <- rep(NA_real_, n)
    jj <- is_unif(ACtype)
    colJ[jj] <- Z[jj] - AB[jj]

    colK <- rep(NA_real_, n)
    colK[jj] <- Z[jj] + AA[jj]

    colL <- get_col(dat, "AD")

    colM <- get_col(dat, "AF")

    colN <- get_col(dat, "DZ")

    colO <- get_col(dat, "EK")

    colP <- get_col(dat, "EM")

    colQ <- get_col(dat, "AG")

    colR <- get_col(dat, "AH")

    colS <- get_col(dat, "AK")

    colT <- get_col(dat, "AL")

    ANsrc <- get_col(dat, "AN")
    AKsrc <- get_col(dat, "AK")
    BYsrc <- get_col(dat, "BY")

    colU <- ANsrc
    condU <- is_missing(ANsrc) & is_missing(AKsrc)
    colU[condU] <- BYsrc[condU]

    AOsrc <- get_col(dat, "AO")
    ALsrc <- get_col(dat, "AL")
    CCsrc <- get_col(dat, "CC")

    colV <- AOsrc
    condV <- is_missing(AOsrc) & is_missing(ALsrc)
    colV[condV] <- CCsrc[condV]

    colW <- get_col(dat, "AR")

    colX <- get_col(dat, "AS")

    colY <- get_col(dat, "AV")

    colZ <- get_col(dat, "AW")

    colAA <- get_col(dat, "CO")

    CSsrc <- get_col(dat, "CS")
    CUsrc <- get_col(dat, "CU")
    colAB <- CSsrc

    useCU <- is_missing(CSsrc) & !is_missing(CUsrc)
    colAB[useCU] <- CUsrc[useCU]

    colAC <- get_col(dat, "DE")

    colAD <- get_col(dat, "DH")

    notes_col <- which(names(dat) == "Notes")
    if (length(notes_col) != 1) stop("Could not identify notes column in: ", sf)
    colAE <- as_chr(dat[[notes_col]])

  }

  out_df <- data.frame(
    A  = colA,  B  = colB,  C  = colC,  D  = colD,  E  = colE,
    F  = colF,  G  = colG,  H  = colH,  I  = colI,  J  = colJ,
    K  = colK,  L  = colL,  M  = colM,  N  = colN,  O  = colO,
    P  = colP,  Q  = colQ,  R  = colR,  S  = colS,  T  = colT,
    U  = colU,  V  = colV,  W  = colW,  X  = colX,  Y  = colY,
    Z  = colZ,  AA = colAA, AB = colAB, AC = colAC, AD = colAD,
    AE = colAE,
    stringsAsFactors = FALSE
  )

  for (co in c("F", "G", "H", "I", "J", "K", LETTERS[17:26], "AA", "AB", "AC", "AD")) {
    x <- as.character(out_df[[co]])
    x[tolower(trimws(x)) == "not reported" & !is.na(x)] <- NA_character_
    value <- as_num(x)
    if (any(!is_missing(x) & is.na(value)))
      stop("Non-numeric value in output column ", co, " in: ", sf)
    out_df[[co]] <- value
  }
  class(out_df$C) <- "hyperlink"

  writeData(wb, template_sheet, out_df,
            startRow = 5, startCol = 1,
            colNames = FALSE, rowNames = FALSE, keepNA = TRUE)

  saveWorkbook(wb, out_file, overwrite = TRUE)
  generated_data[[basename(sf)]] <- out_df
  screen_data[[basename(sf)]] <- data.frame(
    file = basename(sf), sheet = source_sheet_i, source_row = source_rows,
    intermediate_row = seq_len(n) + 4L, sample = trimws(as_chr(colA)),
    lat = out_df$F, lon = out_df$G, age_Ma = out_df$H,
    formation = trimws(as_chr(get_col(dat, if (early_layout) "R" else "Q"))),
    depth_m = as_num(get_col(dat, if (early_layout) "W" else "Y")),
    d13Ccc = out_df$Q, d13Com_occluded = out_df$S, d13Com_bulk = out_df$U,
    stringsAsFactors = FALSE
  )
}

# combine
if (length(generated_data)) {
  combined_dat <- do.call(rbind, generated_data)
  class(combined_dat$C) <- "hyperlink"
  wb <- loadWorkbook(template_file)
  writeData(wb, template_sheet, blank_row1,
            startRow = 1, colNames = FALSE, rowNames = FALSE)
  writeData(wb, template_sheet, combined_dat,
            startRow = 5, colNames = FALSE, rowNames = FALSE, keepNA = TRUE)
  saveWorkbook(wb, file.path(out_dir, "paleosol_Intermediate_combined.xlsx"),
               overwrite = TRUE)
}

if (length(screen_data)) {
  candidates <- find_duplicates(do.call(rbind, screen_data))
  write.csv(candidates, file.path(out_dir, "paleosol_duplicate_candidates.csv"),
            row.names = FALSE, na = "")
  message("Duplicate candidate pairs: ", nrow(candidates))
}

cat("Done. Wrote outputs to: ", normalizePath(out_dir), "\n", sep = "")
