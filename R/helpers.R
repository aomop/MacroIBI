#' Evaluate a reactive or numeric value safely
#'
#' @param x A numeric value or a reactive returning numeric output.
#' @return A numeric value; `0` when evaluation fails.
#' @keywords internal
reactive_handler <- function(x) {
  if (is.reactive(x)) {
    tryCatch({
      val <- x()
      if (!is.null(val) && is.numeric(val)) val else 0
    }, error = function(e) {
      0
    })
  } else {
    if (is.numeric(x)) return(x)
    0
  }
}

#' Add ordinal suffix to a date
#'
#' @param date_input A `Date` or character vector.
#' @param style Optional format string when `date_input` is character.
#' @return A character vector with ordinal suffixes applied.
#' @keywords internal
add_ordinal_suffix <- function(date_input, style = NULL) {
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
        ifelse(day_numbers %% 10 == 3, "rd", "th")
      )
    )
  )

  base_dates <- format(dates, "%B %d, %Y")

  mapply(function(txt, day, suff) {
    sub(sprintf("%02d,", day), sprintf("%d%s,", day, suff), txt, fixed = TRUE)
  }, txt = base_dates, day = day_numbers, suff = suffixes, USE.NAMES = FALSE)
}

#' Safely retrieve a reactive value
#'
#' @param reactive_val A reactive expression.
#' @param default Default value if evaluation fails.
#' @return The reactive value or `default` when unavailable.
#' @keywords internal
safe_reactive_value <- function(reactive_val, default = 0) {
  tryCatch({
    val <- reactive_val()
    if (!is.null(val)) val else default
  }, error = function(e) {
    default
  })
}

#' Calculate a metric score
#'
#' @param values Metric values.
#' @param min_values Minimum values for scaling.
#' @param max_values Maximum values for scaling.
#' @param scale_factor Scaling factor.
#' @param inverse Whether to invert the scale.
#' @return Numeric scores.
#' @keywords internal
calculate_metric_score <- function(values, min_values, max_values, scale_factor = 10, inverse = FALSE) {
  ifelse(
    !is.na(values) & !is.na(min_values) & !is.na(max_values) & (max_values - min_values != 0),
    if (inverse) {
      scale_factor - ((values - min_values) / (max_values - min_values) * scale_factor)
    } else {
      ((values - min_values) / (max_values - min_values) * scale_factor)
    },
    NA
  )
}

#' Calculate the Corixids ratio metric
#'
#' @param selected_genera Reactive values of selected taxa.
#' @param group_totals Reactive values of group totals.
#' @param taxonomy Taxonomy data frame.
#' @return Numeric ratio.
#' @keywords internal
calculate_corixids_ratio <- function(selected_genera, group_totals, taxonomy) {
  tryCatch({
    count_beetles <- safe_reactive_value(group_totals[["section_2"]])
    count_bugs <- safe_reactive_value(group_totals[["section_4"]])

    corixids_taxa <- dplyr::pull(dplyr::filter(taxonomy, .data$Family == "Corixidae"), .data$taxon)

    selected_data <- selected_genera[["section_4"]]()
    if (is.list(selected_data$data) & length(selected_data$data) == 0) {
      return(0)
    }

    count_corixids <- sum(sapply(selected_data$data, function(row) {
      tryCatch({
        if (row[["taxon"]] %in% corixids_taxa) {
          dipnet1 <- row[["dipnet1"]]
          dipnet2 <- row[["dipnet2"]]
          sum(dipnet1, dipnet2, na.rm = TRUE)
        } else {
          0
        }
      }, error = function(e) {
        0
      })
    }), na.rm = TRUE)

    total_beetles_bugs <- count_beetles + count_bugs
    if (total_beetles_bugs > 0) {
      count_corixids / total_beetles_bugs
    } else {
      0
    }
  }, error = function(e) {
    0
  })
}

#' Escape LaTeX characters
#'
#' @param x Character vector to escape.
#' @return Escaped character vector.
#' @keywords internal
escape_latex <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x, fixed = TRUE)
  x <- gsub("([&_#$%{}])", "\\\\\\1", x, perl = TRUE)
  x <- gsub("~", "\\\\textasciitilde{}", x, fixed = TRUE)
  x <- gsub("\\^", "\\\\textasciicircum{}", x, fixed = TRUE)
  x <- gsub("'", "\\\\textquotesingle{}", x, fixed = TRUE)
}
