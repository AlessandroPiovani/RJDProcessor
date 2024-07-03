Download the package and **install** it:
  * _install.packages("your_local_path\\RJDProcessor_0.1.0.tar.gz", repos = NULL, type = "source")_
  * _library(RJDProcessor)_

**Test** it using the orchestrator R scripts and data sources in test folder

If you want to **work on source code** (e.g for debugging to fix some bugs or test some extensions or customizations):
  * uncomment the source and library instructions in _import_and_interface_definitions.R_ and source the script in your code, e.g. _source("import_and_interface_definitions.R")_ in an orchestrator or in a custom script; 
  * finally run your scripts without _library(RJDProcessor)_.

**Build** the package with build_package.R script:
  * if new source files are created, add them to the _Collate_ field in the _DESCRIPTION_ file;
  * if new scode requires external libraries which are not present, add them to the _Imports_ field in the _DESCRIPTION_ file.

