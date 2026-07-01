#' Refresh Taxonomy Data
#'
#' Reads the most recently modified CSV file from a directory of built taxonomy
#' files and saves it as a dated RDS file in the package's extdata directory.
#'
#' @param input_dir Character. Path to the directory containing built taxonomy
#'   CSV files. Defaults to the local TAXONOMY project output directory.
#' @param output_path Character. Path to the output directory where the RDS
#'   file will be saved. Defaults to \code{"inst/extdata/"}.
#'
#' @return Invisibly returns the taxonomy data frame. Called for its side effect
#'   of writing an RDS file named \code{taxonomy_<YYYY-MM-DD>.rds} to
#'   \code{output_path}.
#'
#' @note \code{input_dir} has no default. You must supply the path to a
#'   directory containing built taxonomy CSV files.
#'
#' @examples
#' \dontrun{
#' # Refresh taxonomy from a custom directory
#' refresh_taxonomy(
#'   input_dir = "path/to/taxonomy/csv/files",
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
      "Example: refresh_taxonomy(input_dir = \"path/to/taxonomy/csvs\")",
      call. = FALSE
    )
  }

  files <- list.files(
    path = input_dir,
    pattern = "*.csv$",
    full.names = TRUE
  )
  
  if(length(files) == 0){
    stop(sprintf("No built taxonomy files found at %s", input_dir))
  }
  
  file_details <- file.info(files)
  
  latest_file_path <- rownames(file_details)[which.max(file_details$mtime)]
  
  latest_taxonomy <- utils::read.csv(latest_file_path)
  
  saveRDS(latest_taxonomy, paste0(output_path, sprintf("taxonomy_%s.rds", Sys.Date())))

  invisible(latest_taxonomy)
  }
