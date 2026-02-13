# helper functions used by 'extract_image_features()' to post-process extracted
# features and derive method recommendations
#
# includes numeric safety utilities, table assembly helpers and heuristic
# decision logic
#
# no exported functions
#
#
# .ex_safe_num()
# safely coerce numeric values by replacing NA, NaN or infinite values with
# defined fallback values
.ex_safe_num <- function(x,
                         fallback = 0) {
  # replace invalid numeric values with fallback
  if(is.na(x) || is.nan(x) || is.infinite(x)) {
    fallback
  } else {
    as.numeric(x)
  }
}
#
#
# .ex_round_numeric_df()
# round all numeric columns of a data fram to a specified number of decimal
# digits
.ex_round_numeric_df <- function(df,
                                 digits) {
  # round all columns except first (assumed identifier column)
  df[ ,-1] <- lapply(df[, -1], function(x) {
    round(as.numeric(x), digits)})
  df
}
#
#
# .ex_build_properties_table()
# assemble the final properties table by merging feature blocks derived from
# different analysis stages
.ex_build_properties_table <- function(norm_names,
                                       color_info,
                                       hist_stats,
                                       results_round) {
  # build base properties table (color + histogram features)
  properties <- data.frame(Image_name = norm_names,
                           Color_mode = color_info[ ,"mode"],
                           GS_Diff = as.numeric(color_info[ ,"diff_mean"]),
                           round(hist_stats, 3),
                           stringsAsFactors = FALSE)

  # merge with gradient/frequency features
  # left join on image name (preserve all base rows)
  merge(properties,
        results_round,
        by.x = "Image_name",
        by.y = "image_name",
        all.x = TRUE)
}
#
#
# .ex_choose_method()
# determine a recommended analysis method for each image based on extracted
# feature values and a weighted voting heuristic
.ex_choose_method <- function(df) {

  # apply decision logic row-wise
  res <- apply(df, 1, function(row) {
    # initialize weighted vote counters
    votes <- c(BiopixR = 0, Cellpose = 0)
    reasons <- character(0)

    # safely extract numeric feature values
    skewness <- .ex_safe_num(row["skewness"])
    kurtosis <- .ex_safe_num(row["kurtosis"])
    mean_grad <- .ex_safe_num(row["mean_grad"])
    median_grad <- .ex_safe_num(row["median_grad"])
    mean_power <- .ex_safe_num(row["mean_power"])

    # Feature-based voting rules
    # thresholds are empirically chosen heuristics

    # Skewness
    if(abs(skewness) > 1.5) {
      votes["BiopixR"] <- votes["BiopixR"] + 1.5
      reasons <- c(reasons, "Skewness -> BiopixR")
    } else {
      votes["Cellpose"] <- votes["Cellpose"] + 1.5
      reasons <- c(reasons, "Skewness -> Cellpose")
    }

    # Kurtosis
    if(kurtosis > 7) {
      votes["BiopixR"] <- votes["BiopixR"] + 1.5
      reasons <- c(reasons, "Kurtosis -> BiopixR")
    } else {
      votes["Cellpose"] <- votes["Cellpose"] + 1.5
      reasons <- c(reasons, "Kurtosis -> Cellpose")
    }

    # Mean gradient magnitude
    # mean_grad
    if(mean_grad < 0.03 || mean_grad > 1.0) {
      votes["BiopixR"] <- votes["BiopixR"] + 1
      reasons <- c(reasons, "mean_grad -> BiopixR")
    } else {
      votes["Cellpose"] <- votes["Cellpose"] + 1
      reasons <- c(reasons, "mean_grad -> Cellpose")
    }

    # Median gradient magnitude
    # median_grad
    if(median_grad < 0.01 || median_grad > 0.09) {
      votes["BiopixR"] <- votes["BiopixR"] + 1
      reasons <- c(reasons, "median_grad -> BiopixR")
    } else {
      votes["Cellpose"] <- votes["Cellpose"] + 1
      reasons <- c(reasons, "median_grad -> Cellpose")
    }


    # Mean power (frequency domain)
    # mean_power
    if(mean_power < 100000) {
      votes["BiopixR"] <- votes["BiopixR"] + 1.5
      reasons <- c(reasons, "mean_power -> BiopixR")
    } else {
      votes["Cellpose"] <- votes["Cellpose"] + 1.5
      reasons <- c(reasons, "mean_power -> Cellpose")
    }

    # Final decision: method with higher total vote
    # (ties default to Cellpose)
    method <- if(votes["BiopixR"] > votes["Cellpose"]) {
      "BiopixR"
    } else {"Cellpose"}

    c(method = method,
      reason = paste(reasons, collapse = ", "))
  })

  # convert row-wise apply result to data frame
  data.frame(Recommended_method = res["method", ],
             Decision_justification = res["reason", ],
             stringsAsFactors = FALSE)
}
