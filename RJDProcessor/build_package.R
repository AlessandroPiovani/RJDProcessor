setwd("C:\\Users\\UTENTE\\Desktop\\RJDopenCruncher\\RJDProcessor")

# Assicurati di avere i pacchetti necessari
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
if (!requireNamespace("roxygen2", quietly = TRUE)) {
  install.packages("roxygen2")
}


# Carica il pacchetto devtools
library(devtools)
library(roxygen2)

#devtools::load_all("R/import_and_interface_definition.R")

# Genera la documentazione
devtools::document()

# Costruisci il pacchetto
devtools::build()

# Controlla il pacchetto
check()

devtools::build_manual(pkg = "C:\\Users\\UTENTE\\Desktop\\RJDopenCruncher\\RJDProcessor")

#C:\Users\UTENTE\AppData\Local\Programs\MiKTeX\miktex\bin\x64\pdflatex.exe
