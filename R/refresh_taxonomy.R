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
#' @return Invisibly returns \code{NULL}. Called for its side effect of writing
#'   an RDS file named \code{taxonomy_<YYYY-MM-DD>.rds} to \code{output_path}.
#'
#' @export
refresh_taxonomy <- function(
    input_dir = "C:/Users/61296/Documents/repos/Miscelaneous/TAXONOMY/data/output",
    output_path = "inst/extdata/"
    ){
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
  
  latest_taxonomy <- read.csv(latest_file_path)
  
  saveRDS(latest_taxonomy, paste0(output_path, sprintf("taxonomy_%s.rds", Sys.Date())))

  invisible(latest_taxonomy)
  }
