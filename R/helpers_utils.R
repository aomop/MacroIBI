# Metadata columns stamped onto every autosave/export CSV
METADATA_COLS <- c("Title", "Date", "schema_version")

# Prefix shared by all taxon-section module IDs (e.g. "section_1", "section_2")
SECTION_PREFIX <- "section_"

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

#' Sanitize group ID for stable use in file names/IDs
#' @keywords internal
sanitize_group_id <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' Internal: packages used in report templates
#'
#' @name report_deps
#' 
#' @description
#'  These packages are used inside the R Markdown templates in inst/.
#' This block exists so R CMD check sees that the Imports are actually used.
#'
#' @keywords internal
#' @import ggplot2
#' @import grid
#' @import gridExtra
#' @import kableExtra
#' @import knitr
#' @import tidyr
NULL
