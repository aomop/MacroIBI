#' Refresh Taxonomy Data
#'
#' Reads the most recent built taxonomy CSV from a directory and saves it as a
#' dated RDS file in the package's extdata directory. These CSVs are produced by
#' the companion \href{https://github.com/aomop/macro-taxonomy}{macro-taxonomy}
#' pipeline, which writes them to its \code{data/output/} directory.
#'
#' "Most recent" is determined by the date in the filename, not by file
#' modification time, so the result does not depend on checkout or copy order.
#'
#' @param input_dir Character. Path to a directory containing built taxonomy
#'   CSV files named \code{taxonomy_<YYYYMMDD>.csv}. Required; there is no
#'   default.
#' @param output_path Character. Path to the output directory where the RDS
#'   file will be saved. Defaults to \code{"inst/extdata/"}.
#'
#' @return Invisibly returns the taxonomy data frame. Called for its side effect
#'   of writing an RDS file named \code{taxonomy_<YYYY-MM-DD>.rds} to
#'   \code{output_path}. The date in the output filename is taken from the
#'   source CSV, so re-running on an older CSV does not mislabel it as current.
#'
#' @examples
#' \dontrun{
#' # Refresh from the macro-taxonomy pipeline's output directory
#' refresh_taxonomy(
#'   input_dir   = "path/to/macro-taxonomy/data/output",
#'   output_path = "inst/extdata/"
#' )
#' }
#'
#' @export
refresh_taxonomy <- function(
    input_dir = NULL,
    output_path = "inst/extdata/"
    ){
  if (is.null(input_dir)) {
    stop(
      "No `input_dir` specified. Please provide the path to your taxonomy CSV directory.\n",
      "Example: refresh_taxonomy(input_dir = \"path/to/macro-taxonomy/data/output\")",
      call. = FALSE
    )
  }

  # Match only the pipeline's own output. The previous pattern ("*.csv$") was a
  # malformed regex that matched any CSV in the directory, so an unrelated file
  # could be adopted as the taxonomy.
  files <- list.files(
    path = input_dir,
    pattern = "^taxonomy_\\d{8}\\.csv$",
    full.names = TRUE
  )

  if (length(files) == 0) {
    stop(
      sprintf(
        "No taxonomy_<YYYYMMDD>.csv files found at %s",
        input_dir
      ),
      call. = FALSE
    )
  }

  # Order by the date in the filename rather than mtime, which is not preserved
  # by git checkouts or file copies.
  file_dates <- as.Date(
    sub("^taxonomy_(\\d{8})\\.csv$", "\\1", basename(files)),
    format = "%Y%m%d"
  )

  latest_index     <- which.max(file_dates)
  latest_file_path <- files[latest_index]
  latest_date      <- file_dates[latest_index]

  latest_taxonomy <- utils::read.csv(latest_file_path)

  # Name the output for the source CSV's date, not today's, so a rebuild from an
  # older CSV is not mislabelled as current.
  output_file <- file.path(
    output_path,
    sprintf("taxonomy_%s.rds", format(latest_date, "%Y-%m-%d"))
  )

  saveRDS(latest_taxonomy, output_file)

  invisible(latest_taxonomy)
  }
