setwd("C:\\Users\\UTENTE\\OneDrive - ISTAT\\Desktop\\RJDopenCruncher\\RJDProcessor")
# setwd("C:\\Users\\alessandro.piovani\\Desktop\\github_repo\\RJDProcessor\\RJDProcessor\\")

# install.packages("C:\\Users\\UTENTE\\OneDrive - ISTAT\\Desktop\\RJDopenCruncher\\RJDProcessor_1.2.1.tar.gz", repos = NULL, type = "source")


# Make sure to have all the necessary packages
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
if (!requireNamespace("roxygen2", quietly = TRUE)) {
  install.packages("roxygen2")
}


# Load devtools and roxygen2 packages
library(devtools)
library(roxygen2)

#devtools::load_all("R/import_and_interface_definition.R")

# Create the documentation
devtools::document()

# Build the package
devtools::build()

# Check the package
check()

devtools::build_manual(pkg = "C:\\Users\\UTENTE\\OneDrive - ISTAT\\Desktop\\RJDopenCruncher\\RJDProcessor")
# devtools::build_manual(pkg = "C:\\Users\\alessandro.piovani\\Desktop\\github_repo\\RJDProcessor\\RJDProcessor\\")


install.packages("C:\\Users\\UTENTE\\OneDrive - ISTAT\\Desktop\\RJDopenCruncher\\RJDProcessor_1.2.1.tar.gz", type="source", repos = NULL)
# install.packages("C:\\Users\\alessandro.piovani\\Desktop\\github_repo\\RJDProcessor\\RJDProcessor_1.2.1.tar.gz", type="source", repos = NULL)
