####################################################################################################
##### Translate Phanerozoic sample ages between timescales
#####
##### 
##### Build a shared boundary grid between timescales.
##### Translate old ages with piecewise linear interpolation between shared boundary ages.
##### Re-classify translated ages on the new timescale.
##### Depth-based interpolation fallback for samples that could not be translated directly from old age.
####################################################################################################



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

translate_gts_age <- function(old_age,
                              depth,
                              old_timescale = "GTS2012",
                              new_timescale = "GTS2020",
                              allow_depth_fallback = TRUE,
                              ts_library = NULL) {
  
  if (is.null(ts_library)) {
    ts_library <- build_timescale_library()
  }
  
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

