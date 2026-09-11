####################################################################################################
##### Translate Phanerozoic sample ages between timescales
#####
##### Functions for translating non-radiometric ages between timescales while preserving
##### radiometric ages as fixed numerical ages across timescales.
####################################################################################################


get_shared_boundaries <- function(old_ts, new_ts) {
  shared_id <- intersect(old_ts$boundary_id, new_ts$boundary_id)
  
  old_match <- match(shared_id, old_ts$boundary_id)
  new_match <- match(shared_id, new_ts$boundary_id)
  
  out <- data.frame(
    boundary_id = shared_id,
    old_ma = old_ts$base_ma[old_match],
    new_ma = new_ts$base_ma[new_match],
    stringsAsFactors = FALSE
  )
  
  out <- out[order(out$old_ma), , drop = FALSE]
  row.names(out) <- NULL
  out
}


translate_numeric_ages <- function(age, old_ts, new_ts) {
  shared <- get_shared_boundaries(old_ts, new_ts)
  
  out <- rep(NA_real_, length(age))
  is_ok <- !is.na(age)
  
  if (!any(is_ok)) {
    return(out)
  }
  
  x <- shared$old_ma
  y <- shared$new_ma
  
  xmin <- min(x)
  xmax <- max(x)
  in_range <- is_ok & age >= xmin & age <= xmax
  
  if (any(in_range)) {
    out[in_range] <- approx(
      x = x,
      y = y,
      xout = age[in_range],
      method = "linear",
      ties = "ordered"
    )$y
  }
  
  out
}


classify_gts_age <- function(age, ts) {
  
  out <- data.frame(
    period = rep(NA_character_, length(age)),
    series_epoch = rep(NA_character_, length(age)),
    stage = rep(NA_character_, length(age)),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(age)) {
    
    if (is.na(age[i])) next
    
    hit <- which(age[i] >= ts$top_ma & age[i] < ts$base_ma)
    
    if (length(hit) == 0) {
      
      # Handle exact present-day value
      if (age[i] == 0) {
        hit <- 1
      } else if (age[i] == max(ts$base_ma)) {
        # Handle exact oldest boundary value by assigning it to the oldest stage
        hit <- nrow(ts)
      } else {
        next
      }
    }
    
    hit <- hit[1]
    
    out$period[i] <- ts$period[hit]
    out$series_epoch[i] <- ts$series_epoch[hit]
    out$stage[i] <- ts$stage[hit]
  }
  
  out
}


translate_gts_age <- function(old_age,
                              depth = NULL,
                              old_timescale = "GTS2012",
                              new_timescale = "GTS2020",
                              allow_depth_fallback = TRUE,
                              radiometric_flag = NULL) {
  
  lib <- build_timescale_library()
  
  if (!old_timescale %in% names(lib)) {
    stop("old_timescale not found in timescale library.")
  }
  
  if (!new_timescale %in% names(lib)) {
    stop("new_timescale not found in timescale library.")
  }
  
  old_ts <- lib[[old_timescale]]
  new_ts <- lib[[new_timescale]]
  
  n <- length(old_age)
  
  if (is.null(radiometric_flag)) {
    radiometric_flag <- rep(FALSE, n)
  }
  
  if (length(radiometric_flag) != n) {
    stop("radiometric_flag must have the same length as old_age.")
  }
  
  radiometric_flag <- as.logical(radiometric_flag)
  
  if (!is.null(depth) && length(depth) != n) {
    stop("depth must have the same length as old_age.")
  }
  
  translated_age <- rep(NA_real_, n)
  translation_method <- rep(NA_character_, n)
  
  is_nonmissing <- !is.na(old_age)
  is_radiometric <- is_nonmissing & radiometric_flag
  is_stratigraphic <- is_nonmissing & !radiometric_flag
  
  if (any(is_radiometric)) {
    translated_age[is_radiometric] <- old_age[is_radiometric]
    translation_method[is_radiometric] <- "radiometric_fixed"
  }
  
  if (any(is_stratigraphic)) {
    translated_age[is_stratigraphic] <- translate_numeric_ages(
      age = old_age[is_stratigraphic],
      old_ts = old_ts,
      new_ts = new_ts
    )
    
    translated_ok <- is_stratigraphic & !is.na(translated_age)
    translation_method[translated_ok] <- "timescale_translated"
  }
  
  needs_fallback <- !radiometric_flag & is.na(translated_age)
  
  if (allow_depth_fallback && any(needs_fallback)) {
    if (is.null(depth)) {
      warning("Some non-radiometric ages could not be translated, but depth is NULL so fallback was skipped.")
    } else {
      fit <- !needs_fallback & !is.na(depth) & !is.na(translated_age)
      pred <- needs_fallback & !is.na(depth)
      
      if (sum(fit) >= 2 && any(pred)) {
        translated_fill <- approx(
          x = depth[fit],
          y = translated_age[fit],
          xout = depth[pred],
          method = "linear",
          rule = 1,
          ties = "ordered"
        )$y
        
        translated_age[pred] <- translated_fill
        used_fallback <- pred & !is.na(translated_age)
        translation_method[used_fallback] <- "depth_fallback"
      }
    }
  }
  
  old_class <- classify_gts_age(old_age, old_ts)
  new_class <- classify_gts_age(translated_age, new_ts)
  
  out <- data.frame(
    depth = if (is.null(depth)) rep(NA_real_, n) else depth,
    old_age = old_age,
    translated_age = translated_age,
    radiometric_flag = radiometric_flag,
    old_period = old_class$period,
    old_series_epoch = old_class$series_epoch,
    old_stage = old_class$stage,
    new_period = new_class$period,
    new_series_epoch = new_class$series_epoch,
    new_stage = new_class$stage,
    translation_method = translation_method,
    stringsAsFactors = FALSE
  )
  
  out
}


plot_translation_demo <- function(result) {
  ok <- !is.na(result$old_age) & !is.na(result$translated_age)
  
  if (!any(ok)) {
    stop("No paired old_age / translated_age values available to plot.")
  }
  
  xlim <- range(c(result$old_age[ok], result$translated_age[ok]), na.rm = TRUE)
  ylim <- xlim
  
  plot(
    result$old_age[ok],
    result$translated_age[ok],
    pch = ifelse(result$radiometric_flag[ok], 17, 16),
    xlab = "Input age (Ma)",
    ylab = "Output age (Ma)",
    main = "Timescale translation demo",
    xlim = xlim,
    ylim = ylim
  )
  
  abline(0, 1, lty = 2)
  
  legend(
    "topleft",
    legend = c("stratigraphic", "radiometric"),
    pch = c(16, 17),
    bty = "n"
  )
}
