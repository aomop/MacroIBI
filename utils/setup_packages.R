cat("Checking and installing required packages...\n")

# Install BiocManager if needed
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

# List of CRAN packages
cran_packages <- c(
  "rmarkdown", "shiny", "shinyjs", "shinybusy", "bslib",
  "viridis", "DT", "shinyTree", "webshot", "htmltools",
  "htmlwidgets", "magick", "tidyverse", "uuid", "anytime"
)

# List of BioC packages
bioc_packages <- c("ggtree", "treeio", "phytools", "ape")

# GitHub-only packages
github_packages <- list(
  cheerfulgif = "aomop/cheerfulgif"
)

# Install missing CRAN
install_cran <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(paste0("Installing ", pkg, " from CRAN...\n"))
    install.packages(pkg)
  }
}

# Install missing BioC
install_bioc <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(paste0("Installing ", pkg, " from Bioconductor...\n"))
    BiocManager::install(pkg, ask = FALSE)
  }
}

# Install missing GitHub packages
#install_github <- function(pkg, repo) {
#  if (!requireNamespace(pkg, quietly = TRUE)) {
#    cat(paste0("Installing ", pkg, " from GitHub (", repo, ")...\n"))
#    devtools::install_github(repo)
#  }
#}

# Perform installations
invisible(lapply(cran_packages, install_cran))
invisible(lapply(bioc_packages, install_bioc))
#invisible(mapply(install_github, names(github_packages), github_packages))

cat("All required packages are installed.\n")
