source("utility_functions.R")

library("RJDemetra")

spc <- RJDemetra::tramoseats_spec(spec="RSA3")

regarima_spec <- simplify_leaves(spc)
seats_spec    <- simplify_leaves(spc)

