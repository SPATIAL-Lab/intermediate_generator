####################################################################################################
##### Translate Phanerozoic sample ages between GTS2012 and GTS2020
#####
##### 
##### Hard-code full Phanerozoic stage tables for each timescale.
##### Build a shared boundary grid between the two timescales.
##### Translate old ages with piecewise linear interpolation between shared boundary ages.
##### Re-classify translated ages on the new timescale.
##### Depth-based interpolation fallback for samples that could not be translated directly from old age.
####################################################################################################



####################################################################################################
##### Hard-coded Phanerozoic timescale tables
####################################################################################################

build_gts2020 <- function() {
  
  stage <- c(
    "Meghalayan", "Northgrippian", "Greenlandian",
    "Late Pleistocene", "Chibanian", "Calabrian", "Gelasian",
    "Piacenzian", "Zanclean",
    "Messinian", "Tortonian", "Serravallian", "Langhian", "Burdigalian", "Aquitanian",
    "Chattian", "Rupelian",
    "Priabonian", "Bartonian", "Lutetian", "Ypresian",
    "Thanetian", "Selandian", "Danian",
    "Maastrichtian", "Campanian", "Santonian", "Coniacian", "Turonian", "Cenomanian",
    "Albian", "Aptian", "Barremian", "Hauterivian", "Valanginian", "Berriasian",
    "Tithonian", "Kimmeridgian", "Oxfordian",
    "Callovian", "Bathonian", "Bajocian", "Aalenian",
    "Toarcian", "Pliensbachian", "Sinemurian", "Hettangian",
    "Rhaetian", "Norian", "Carnian",
    "Ladinian", "Anisian",
    "Olenekian", "Induan",
    "Changhsingian", "Wuchiapingian",
    "Capitanian", "Wordian", "Roadian",
    "Kungurian", "Artinskian", "Sakmarian", "Asselian",
    "Gzhelian", "Kasimovian", "Moscovian", "Bashkirian",
    "Serpukhovian", "Visean", "Tournaisian",
    "Famennian", "Frasnian",
    "Givetian", "Eifelian",
    "Emsian", "Pragian", "Lochkovian",
    "Pridoli",
    "Ludfordian", "Gorstian",
    "Homerian", "Sheinwoodian",
    "Telychian", "Aeronian", "Rhuddanian",
    "Hirnantian", "Katian", "Sandbian",
    "Darriwilian", "Dapingian",
    "Floian", "Tremadocian",
    "Age 10", "Jiangshanian", "Paibian",
    "Guzhangian", "Drumian", "Wuliuan",
    "Age 4", "Age 3", "Age 2", "Fortunian"
  )
  
  base_ma <- c(
    0.004250, 0.008236, 0.01170,
    0.129, 0.774, 1.80, 2.58,
    3.60, 5.335,
    7.25, 11.625, 13.82, 15.99, 20.45, 23.04,
    27.29, 33.90,
    37.71, 41.03, 48.07, 56.00,
    59.24, 61.66, 66.04,
    72.17, 83.65, 85.70, 89.39, 93.90, 100.50,
    113.20, 121.40, 126.50, 132.60, 137.70, 143.10,
    149.24, 154.80, 161.50,
    165.30, 168.20, 170.90, 174.70,
    184.20, 192.90, 199.46, 201.36,
    205.74, 227.30, 237.00,
    241.46, 246.70,
    249.88, 251.90,
    254.24, 259.55,
    264.34, 269.21, 274.37,
    283.30, 290.51, 293.52, 298.89,
    303.68, 307.02, 315.15, 323.40,
    330.34, 346.73, 359.30,
    371.10, 378.90,
    385.30, 394.30,
    410.50, 412.40, 419.00,
    422.73,
    425.01, 426.74,
    430.62, 432.93,
    438.59, 440.49, 443.07,
    445.21, 452.75, 458.18,
    469.42, 471.26,
    477.08, 486.85,
    491.00, 494.20, 497.00,
    500.50, 504.50, 509.00,
    514.50, 521.00, 529.00, 538.80
  )
  
  period <- c(
    rep("Quaternary", 7),
    rep("Neogene", 8),
    rep("Paleogene", 9),
    rep("Cretaceous", 12),
    rep("Jurassic", 11),
    rep("Triassic", 7),
    rep("Permian", 9),
    rep("Carboniferous", 7),
    rep("Devonian", 7),
    rep("Silurian", 8),
    rep("Ordovician", 7),
    rep("Cambrian", 10)
  )
  
  series_epoch <- c(
    "Holocene", "Holocene", "Holocene",
    "Pleistocene", "Pleistocene", "Pleistocene", "Pleistocene",
    
    "Pliocene", "Pliocene",
    "Miocene", "Miocene", "Miocene", "Miocene", "Miocene", "Miocene",
    
    "Oligocene", "Oligocene",
    "Eocene", "Eocene", "Eocene", "Eocene",
    "Paleocene", "Paleocene", "Paleocene",
    
    "Late Cretaceous", "Late Cretaceous", "Late Cretaceous", "Late Cretaceous", "Late Cretaceous", "Late Cretaceous",
    "Early Cretaceous", "Early Cretaceous", "Early Cretaceous", "Early Cretaceous", "Early Cretaceous", "Early Cretaceous",
    
    "Late Jurassic", "Late Jurassic", "Late Jurassic",
    "Middle Jurassic", "Middle Jurassic", "Middle Jurassic", "Middle Jurassic",
    "Early Jurassic", "Early Jurassic", "Early Jurassic", "Early Jurassic",
    
    "Late Triassic", "Late Triassic", "Late Triassic",
    "Middle Triassic", "Middle Triassic",
    "Early Triassic", "Early Triassic",
    
    "Lopingian", "Lopingian",
    "Guadalupian", "Guadalupian", "Guadalupian",
    "Cisuralian", "Cisuralian", "Cisuralian", "Cisuralian",
    
    "Pennsylvanian", "Pennsylvanian", "Pennsylvanian", "Pennsylvanian",
    "Mississippian", "Mississippian", "Mississippian",
    
    "Late Devonian", "Late Devonian",
    "Middle Devonian", "Middle Devonian",
    "Early Devonian", "Early Devonian", "Early Devonian",
    
    "Pridoli",
    "Ludlow", "Ludlow",
    "Wenlock", "Wenlock",
    "Llandovery", "Llandovery", "Llandovery",
    
    "Late Ordovician", "Late Ordovician", "Late Ordovician",
    "Middle Ordovician", "Middle Ordovician",
    "Early Ordovician", "Early Ordovician",
    
    "Furongian", "Furongian", "Furongian",
    "Miaolingian", "Miaolingian", "Miaolingian",
    "Series 2", "Series 2", "Series 2", "Terreneuvian"
  )
  
  out <- data.frame(
    period = period,
    series_epoch = series_epoch,
    stage = stage,
    base_ma = base_ma,
    stringsAsFactors = FALSE
  )
  
  out$top_ma <- c(0, head(out$base_ma, -1))
  
  # boundary_id is what gets used in the shared control-point mapping
  out$boundary_id <- out$stage
  out$boundary_id[out$stage == "Greenlandian"]     <- "Holocene"
  out$boundary_id[out$stage == "Late Pleistocene"] <- "Late Pleistocene"
  
  out
}


build_gts2012 <- function() {
  
  stage <- c(
    "Holocene", "Upper Pleistocene", "Calabrian", "Gelasian",
    "Piacenzian", "Zanclean",
    "Messinian", "Tortonian", "Serravallian", "Langhian", "Burdigalian", "Aquitanian",
    "Chattian", "Rupelian",
    "Priabonian", "Bartonian", "Lutetian", "Ypresian",
    "Thanetian", "Selandian", "Danian",
    "Maastrichtian", "Campanian", "Santonian", "Coniacian", "Turonian", "Cenomanian",
    "Albian", "Aptian", "Barremian", "Hauterivian", "Valanginian", "Berriasian",
    "Tithonian", "Kimmeridgian", "Oxfordian",
    "Callovian", "Bathonian", "Bajocian", "Aalenian",
    "Toarcian", "Pliensbachian", "Sinemurian", "Hettangian",
    "Rhaetian", "Norian", "Carnian",
    "Ladinian", "Anisian", "Olenekian", "Induan",
    "Changhsingian", "Wuchiapingian",
    "Capitanian", "Wordian", "Roadian",
    "Kungurian", "Artinskian", "Sakmarian", "Asselian",
    "Gzhelian", "Kasimovian", "Moscovian", "Bashkirian",
    "Serpukhovian", "Visean", "Tournaisian",
    "Famennian", "Frasnian",
    "Givetian", "Eifelian",
    "Emsian", "Pragian", "Lochkovian",
    "Pridoli",
    "Ludfordian", "Gorstian",
    "Homerian", "Sheinwoodian",
    "Telychian", "Aeronian", "Rhuddanian",
    "Hirnantian", "Katian", "Sandbian",
    "Darriwilian", "Dapingian",
    "Floian", "Tremadocian",
    "Age 10", "Jiangshanian", "Paibian",
    "Guzhangian", "Drumian", "Wuliuan",
    "Age 4", "Age 3", "Age 2", "Fortunian"
  )
  
  base_ma <- c(
    0.0118, 0.126, 1.806, 2.59,
    3.60, 5.333,
    7.246, 11.63, 13.82, 15.97, 20.43, 23.03,
    28.09, 33.89,
    37.80, 41.20, 47.82, 55.96,
    59.24, 61.61, 66.04,
    72.05, 83.64, 86.26, 89.77, 93.90, 100.50,
    112.95, 126.30, 130.77, 133.88, 140.18, 145.01,
    152.06, 157.30, 163.50,
    166.10, 168.30, 170.30, 174.10,
    182.70, 190.80, 199.30, 201.30,
    209.46, 228.35, 237.00,
    241.50, 247.06, 250.01, 252.16,
    254.20, 259.81,
    265.14, 268.80, 272.30,
    279.33, 290.06, 295.53, 298.88,
    303.67, 306.99, 315.16, 323.23,
    330.92, 346.73, 358.94,
    372.24, 382.69,
    387.72, 393.25,
    407.57, 410.78, 419.20,
    422.96,
    425.57, 427.36,
    430.45, 433.35,
    438.49, 440.77, 443.83,
    445.16, 452.97, 458.36,
    467.25, 469.96,
    477.72, 485.37,
    489.50, 494.00, 497.00,
    500.50, 504.50, 509.00,
    514.00, 521.00, 529.00, 541.00
  )
  
  period <- c(
    rep("Quaternary", 4),
    rep("Neogene", 8),
    rep("Paleogene", 9),
    rep("Cretaceous", 12),
    rep("Jurassic", 11),
    rep("Triassic", 7),
    rep("Permian", 9),
    rep("Carboniferous", 7),
    rep("Devonian", 7),
    rep("Silurian", 8),
    rep("Ordovician", 7),
    rep("Cambrian", 10)
  )
  
  series_epoch <- c(
    "Holocene", "Pleistocene", "Pleistocene", "Pleistocene",
    
    "Pliocene", "Pliocene",
    "Miocene", "Miocene", "Miocene", "Miocene", "Miocene", "Miocene",
    
    "Oligocene", "Oligocene",
    "Eocene", "Eocene", "Eocene", "Eocene",
    "Paleocene", "Paleocene", "Paleocene",
    
    "Late Cretaceous", "Late Cretaceous", "Late Cretaceous", "Late Cretaceous", "Late Cretaceous", "Late Cretaceous",
    "Early Cretaceous", "Early Cretaceous", "Early Cretaceous", "Early Cretaceous", "Early Cretaceous", "Early Cretaceous",
    
    "Late Jurassic", "Late Jurassic", "Late Jurassic",
    "Middle Jurassic", "Middle Jurassic", "Middle Jurassic", "Middle Jurassic",
    "Early Jurassic", "Early Jurassic", "Early Jurassic", "Early Jurassic",
    
    "Late Triassic", "Late Triassic", "Late Triassic",
    "Middle Triassic", "Middle Triassic",
    "Early Triassic", "Early Triassic",
    
    "Lopingian", "Lopingian",
    "Guadalupian", "Guadalupian", "Guadalupian",
    "Cisuralian", "Cisuralian", "Cisuralian", "Cisuralian",
    
    "Pennsylvanian", "Pennsylvanian", "Pennsylvanian", "Pennsylvanian",
    "Mississippian", "Mississippian", "Mississippian",
    
    "Late Devonian", "Late Devonian",
    "Middle Devonian", "Middle Devonian",
    "Early Devonian", "Early Devonian", "Early Devonian",
    
    "Pridoli",
    "Ludlow", "Ludlow",
    "Wenlock", "Wenlock",
    "Llandovery", "Llandovery", "Llandovery",
    
    "Late Ordovician", "Late Ordovician", "Late Ordovician",
    "Middle Ordovician", "Middle Ordovician",
    "Early Ordovician", "Early Ordovician",
    
    "Furongian", "Furongian", "Furongian",
    "Miaolingian", "Miaolingian", "Miaolingian",
    "Series 2", "Series 2", "Series 2", "Terreneuvian"
  )
  
  out <- data.frame(
    period = period,
    series_epoch = series_epoch,
    stage = stage,
    base_ma = base_ma,
    stringsAsFactors = FALSE
  )
  
  out$top_ma <- c(0, head(out$base_ma, -1))
  
  out$boundary_id <- out$stage
  out$boundary_id[out$stage == "Holocene"]          <- "Holocene"
  out$boundary_id[out$stage == "Upper Pleistocene"] <- "Late Pleistocene"
  
  out
}


build_timescale_library <- function() {
  list(
    GTS2012 = build_gts2012(),
    GTS2020 = build_gts2020()
  )
}


####################################################################################################
##### Helpers
####################################################################################################

get_timescale <- function(timescale_name, ts_library) {
  
  if (!timescale_name %in% names(ts_library)) {
    stop("Unknown timescale: ", timescale_name)
  }
  
  ts_library[[timescale_name]]
}


check_input_lengths <- function(old_age, depth) {
  
  if (length(old_age) != length(depth)) {
    stop("'old_age' and 'depth' must have the same length.")
  }
}


check_phanerozoic_range <- function(age) {
  
  out_of_range <- !is.na(age) & (age < 0 | age > 541)
  
  if (any(out_of_range)) {
    warning("Some ages lie outside the Phanerozoic 0-541 Ma range and will return NA.")
  }
}


make_boundary_table <- function(ts) {
  
  data.frame(
    boundary_id = c("top", ts$boundary_id),
    age_ma = c(0, ts$base_ma),
    stringsAsFactors = FALSE
  )
}


make_shared_boundary_table <- function(old_ts, new_ts) {
  
  old_b <- make_boundary_table(old_ts)
  new_b <- make_boundary_table(new_ts)
  
  shared_ids <- intersect(old_b$boundary_id, new_b$boundary_id)
  
  old_b <- old_b[old_b$boundary_id %in% shared_ids, , drop = FALSE]
  new_b <- new_b[new_b$boundary_id %in% shared_ids, , drop = FALSE]
  
  merged <- merge(
    old_b,
    new_b,
    by = "boundary_id",
    suffixes = c("_old", "_new"),
    sort = FALSE
  )
  
  merged <- merged[order(merged$age_ma_old), , drop = FALSE]
  rownames(merged) <- NULL
  
  merged
}


classify_age <- function(age, ts) {
  
  n <- length(age)
  
  out <- data.frame(
    period = rep(NA_character_, n),
    series_epoch = rep(NA_character_, n),
    stage = rep(NA_character_, n),
    stringsAsFactors = FALSE
  )
  
  if (n == 0) {
    return(out)
  }
  
  for (i in seq_len(n)) {
    
    if (is.na(age[i])) {
      next
    }
    
    hit <- which(age[i] > ts$top_ma & age[i] <= ts$base_ma)
    
    # special case for exact present-day age
    if (length(hit) == 0 && age[i] == 0) {
      hit <- 1L
    }
    
    # special case for exact 541 Ma Fortunian base
    if (length(hit) == 0 && age[i] == max(ts$base_ma)) {
      hit <- which.max(ts$base_ma)
    }
    
    if (length(hit) == 1L) {
      out$period[i]       <- ts$period[hit]
      out$series_epoch[i] <- ts$series_epoch[hit]
      out$stage[i]        <- ts$stage[hit]
    }
  }
  
  out
}


translate_age_from_boundaries <- function(age, shared_boundaries) {
  
  if (is.na(age)) {
    return(NA_real_)
  }
  
  # No extrapolation beyond shared boundary range
  if (age < min(shared_boundaries$age_ma_old) || age > max(shared_boundaries$age_ma_old)) {
    return(NA_real_)
  }
  
  approx(
    x = shared_boundaries$age_ma_old,
    y = shared_boundaries$age_ma_new,
    xout = age,
    method = "linear",
    rule = 1
  )$y
}


fill_by_depth_fallback <- function(new_age, depth) {
  
  out <- new_age
  
  good <- which(!is.na(out) & !is.na(depth))
  bad  <- which(is.na(out) & !is.na(depth))
  
  if (length(good) < 2L || length(bad) == 0L) {
    return(out)
  }
  
  ord_good <- order(depth[good])
  
  x_good <- depth[good][ord_good]
  y_good <- out[good][ord_good]
  
  # Collapse duplicate depths if they exist
  keep <- !duplicated(x_good)
  x_good <- x_good[keep]
  y_good <- y_good[keep]
  
  if (length(x_good) < 2L) {
    return(out)
  }
  
  y_bad <- approx(
    x = x_good,
    y = y_good,
    xout = depth[bad],
    method = "linear",
    rule = 1
  )$y
  
  out[bad] <- y_bad
  
  out
}


####################################################################################################
##### Main translation function
####################################################################################################

translate_timescale_age <- function(old_age,
                                    depth,
                                    old_timescale = "GTS2012",
                                    new_timescale = "GTS2020",
                                    allow_depth_fallback = TRUE,
                                    ts_library = build_timescale_library()) {
  
  check_input_lengths(old_age = old_age, depth = depth)
  check_phanerozoic_range(old_age)
  
  old_ts <- get_timescale(old_timescale, ts_library)
  new_ts <- get_timescale(new_timescale, ts_library)
  
  shared_boundaries <- make_shared_boundary_table(old_ts, new_ts)
  
  new_age_direct <- vapply(
    X = old_age,
    FUN = translate_age_from_boundaries,
    FUN.VALUE = numeric(1),
    shared_boundaries = shared_boundaries
  )
  
  new_age <- new_age_direct
  
  age_source <- rep("direct_age_translation", length(old_age))
  age_source[is.na(new_age_direct)] <- NA_character_
  
  if (allow_depth_fallback) {
    new_age_filled <- fill_by_depth_fallback(new_age = new_age_direct, depth = depth)
    
    used_fallback <- is.na(new_age_direct) & !is.na(new_age_filled)
    
    new_age[used_fallback] <- new_age_filled[used_fallback]
    age_source[used_fallback] <- "depth_fallback"
  }
  
  old_class <- classify_age(age = old_age, ts = old_ts)
  new_class <- classify_age(age = new_age, ts = new_ts)
  
  out <- data.frame(
    depth = depth,
    old_age = old_age,
    old_period = old_class$period,
    old_series_epoch = old_class$series_epoch,
    old_stage = old_class$stage,
    new_age = new_age,
    new_period = new_class$period,
    new_series_epoch = new_class$series_epoch,
    new_stage = new_class$stage,
    age_source = age_source,
    stringsAsFactors = FALSE
  )
  
  out
}


####################################################################################################
##### Example with dummy data
####################################################################################################

# Synthetic section mostly spanning the Barremian-Aptian interval,where the GTS2012 -> GTS2020 
# change is fairly noticeable. Two ages are intentionally missing to demonstrate depth fallback.

dummy_depth <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45)

dummy_old_age <- c(
  129.8,
  128.4,
  127.1,
  NA,
  124.4,
  123.2,
  NA,
  120.9,
  119.4,
  118.1
)

result <- translate_timescale_age(
  old_age = dummy_old_age,
  depth = dummy_depth,
  old_timescale = "GTS2012",
  new_timescale = "GTS2020",
  allow_depth_fallback = TRUE
)

print(result, row.names = FALSE)


####################################################################################################
##### plot
####################################################################################################

plot_translation_demo <- function(result_object) {
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  
  par(mfrow = c(1, 2))
  
  plot(
    result_object$old_age,
    result_object$depth,
    pch = 16,
    xlab = "Old age (Ma)",
    ylab = "Depth",
    main = "Input ages"
  )
  axis(3)
  box()
  
  plot(
    result_object$new_age,
    result_object$depth,
    pch = 16,
    xlab = "Translated age (Ma)",
    ylab = "Depth",
    main = "Translated ages"
  )
  axis(3)
  box()
}

plot_translation_demo(result)


