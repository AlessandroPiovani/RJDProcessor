Download the package and install it:
  * install.packages("your_local_path\\RJDProcessor_0.1.0.tar.gz", repos = NULL, type = "source")
  * library(RJDProcessor)

Test it using the orchestrator R scripts and data sources in test folder

If you want to work on source code (e.g for debugging to fix some bugs or test some extensions or customizations):
  * uncomment the source and library instructions in import_and_interface_definitions.R and source the script in your code, e.g. source("import_and_interface_definitions.R") in an orchestrator or in a custom script; 
  * finally run your scripts without library(RJDProcessor).

Build the package with build_package.R script:
  * if new source files are created, add them to the "Collate" field in the DESCRIPTION file;
  * if new scode requires external libraries which are not present, add them to the "Imports" field in the DESCRIPTION file.

