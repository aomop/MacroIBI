# Helper function to add ordinal suffixes to dates rendered in outputs, because why the hell not?
add_ordinal_suffix <- function(date_input, style = NULL) {
  # Normalize to Date class
  if (inherits(date_input, "Date")) {
    dates <- date_input
  } else if (is.character(date_input)) {
    if (is.null(style)) {
      stop("If `date_input` is character, a `style` format must be provided.", call. = FALSE)
    }
    
    dates <- tryCatch(
      as.Date(date_input, format = style),
      error = function(e) {
        stop("Failed to parse one or more date strings. Check the `style` format and values.", call. = FALSE)
      }
    )
    
    if (anyNA(dates)) {
      stop("Some dates could not be parsed with the given `style` format.", call. = FALSE)
    }
  } else {
    stop("`date_input` must be a Date or character vector.", call. = FALSE)
  }
  
  day_numbers <- as.integer(format(dates, "%d"))
  
  suffixes <- ifelse(day_numbers %in% c(11, 12, 13), "th",
                     ifelse(day_numbers %% 10 == 1, "st",
                            ifelse(day_numbers %% 10 == 2, "nd",
                                   ifelse(day_numbers %% 10 == 3, "rd", "th"))))
  
  base_dates <- format(dates, "%B %d, %Y")
  
  # Replace the "24," part with "24th," using simple string replacement
  final <- mapply(function(txt, day, suff) {
    sub(sprintf("%02d,", day), sprintf("%d%s,", day, suff), txt, fixed = TRUE)
  }, txt = base_dates, day = day_numbers, suff = suffixes, USE.NAMES = FALSE)
  
  return(final)
}

# --------------------------------------------------------------------
# Helper Function: Safe Reactive Value Retrieval
# --------------------------------------------------------------------
# Safely retrieves a reactive value, providing a default value in case of errors or null values.
safe_reactive_value <- function(reactive_val, default = 0) {
  tryCatch({
    val <- reactive_val()
    if (!is.null(val)) val else default
  }, error = function(e) {
    default  # Return default if an error occurs
  })
}

# --------------------------------------------------------------------
# Helper Function: Calculate Metric Scores
# --------------------------------------------------------------------
# Calculates metric scores based on provided values and scaling factors.
# Supports standard and inverse calculations for different types of metrics.
calculate_metric_score <- function(values, min_values, max_values, scale_factor = 10, inverse = FALSE) {
  ifelse(
    !is.na(values) & !is.na(min_values) & !is.na(max_values) & (max_values - min_values != 0),
    if (inverse) {
      # Inverse calculation for "Increase" metrics
      scale_factor - ((values - min_values) / (max_values - min_values) * scale_factor)
    } else {
      # Standard calculation
      ((values - min_values) / (max_values - min_values) * scale_factor)
    },
    NA  # Return NA for invalid or missing values
  )
}

# --------------------------------------------------------------------
# Function: Calculate Corixids Ratio
# --------------------------------------------------------------------
# Calculates the ratio of Corixids to total counts of beetles and bugs as a specific metric.
calculate_corixids_ratio <- function(selected_genera, group_totals, taxonomy) {
  tryCatch({
    # Step 1: Get beetle and bug counts
    count_beetles <- safe_reactive_value(group_totals[["section_2"]])
    count_bugs <- safe_reactive_value(group_totals[["section_4"]])
    
    # Step 2: Extract Corixoidea taxa from the taxonomy table
    corixids_taxa <- taxonomy %>%
      filter(Family == "Corixidae") %>%
      pull(taxon)
    
    # Step 3: Retrieve selected genera data for section 4
    
    selected_data <- selected_genera[["section_4"]]()
    if (is.list(selected_data$data) & length(selected_data$data) == 0) {
      return(0)
    }
    
    # Step 4: Sum Corixoidea counts from the selected genera data
    count_corixids <- sum(sapply(selected_data$data, function(row) {
      tryCatch({
        if (row[["taxon"]] %in% corixids_taxa) {
          dipnet1 <- row[["dipnet1"]]
          dipnet2 <- row[["dipnet2"]]
          
          # Sum the dipnet counts
          sum(dipnet1, dipnet2, na.rm = TRUE)
        } else {
          0
        }
      }, error = function(e) {
        print(paste("Error processing row:", e$message))
        0
      })
    }), na.rm = TRUE)
    
    # Step 5: Calculate ratio
    total_beetles_bugs <- count_beetles + count_bugs
    if (total_beetles_bugs > 0) {
      ratio <- count_corixids / total_beetles_bugs
      ratio
    } else {
      0
    }
  }, error = function(e) {
    print(paste("Error in calculating Corixid metric:", e$message))
    0
  })
}

# Helper function to avoid troublesome characters when using latex.
escape_latex <- function(x) {
  x %>%
    gsub("\\\\", "\\\\textbackslash{}", ., fixed = TRUE) %>%
    gsub("([&_#$%{}])", "\\\\\\1", ., perl = TRUE) %>%
    gsub("~", "\\\\textasciitilde{}", ., fixed = TRUE) %>%
    gsub("\\^", "\\\\textasciicircum{}", ., fixed = TRUE) %>%
    gsub("'", "’", ., fixed = TRUE)  # Replace apostrophe with proper typographic one
}