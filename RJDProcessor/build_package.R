setwd("C:\\Users\\UTENTE\\Desktop\\RJDopenCruncher\\RJDProcessor")

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

devtools::build_manual(pkg = "C:\\Users\\UTENTE\\Desktop\\RJDopenCruncher\\RJDProcessor")

#C:\Users\UTENTE\AppData\Local\Programs\MiKTeX\miktex\bin\x64\pdflatex.exe
