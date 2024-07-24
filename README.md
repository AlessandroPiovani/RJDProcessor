<h1 align="center"><b>RJDProcessor</b></h1>
</br>

<h2><b>Description</b></h1>
<p align="justify">JDemetra+ is the officially recommended seasonal adjustment software for scholars and practitioners affiliated with the European Central Bank and Statistical systems. It is composed of many tools: a graphical user interface (GUI) that can be used together with a cruncher for mass production and R libraries, organized in an ecosystem called rjdverse. In a production environment where multiple time series need to be treated together, rjdverse functions require hardcoding new model specifications and refreshing the data one series at a time. Working in R is also useful for producing non-standard reports.</p>

<p align="justify">Another crucial aspect, both for the GUI and rjdverse, is the management of model specifications. Currently, data and specifications are stored together in a structure called workspace, a collection of verbose XML files organized hierarchically in filesystem directories. This structure does not perfectly meet the needs of some organizations that may wish to treat model specifications as data, similar to how they handle time series. Treating specifications as independent objects that can be dynamically associated with their respective data would facilitate model comparison, revision checking, and simplify their storage and retrieval in production databases. In response to these needs, the RJDProcessor library offers the possibility to define and command the overall process of seasonal adjustment (acquisition, processing, storage, and automation) and not only the seasonal adjustment of the data. </p>

<p align="justify">RJDProcessor integrates the packages of the rjdverse into a fully R-based production pipeline, ready to be used but also easily extendable by methodologists. RJDProcessor's modular organization, with a clear division of responsibilities, makes the software easy to maintain and extend. Its object-based architecture allows users to adapt it to their production lines, either by writing custom DataReaders that respect the suggested interface or by using the predefined ones. Model specifications are represented through clearly readable JSON objects and can be dynamically linked with data.</p>

<p align="justify">Processing of multiple time series is possible by storing their specifications in JSON files, and interoperability with other JDemetra+ software is guaranteed because RJDProcessor can read workspaces and is able to produce them as an output.</p>

<p align="justify">RJDProcessor also offers tools to migrate from TS+ software (Gomez and Maravall, Bank of Spain) to JDemetra+/rjdverse.</p>

<p align="justify">So far RJDPrecessor supports only TRAMO-SEATS. The architecture also supports X13-ARIMA-SEATS but it is not implemented yet (coming soon) </p>


<h2><b>Documentation</b></h1>

See the files in **Documentation folder** to understand how to use the package, starting from **RJDemetra_tools_for_statistical_production.pdf**

<h2><b>Install</b></h2>

Download the package and install it:
  * **_install.packages("your_local_path\\RJDProcessor_0.1.0.tar.gz", repos = NULL, type = "source")_**
  * **_library(RJDProcessor)_**

<h2><b>Test</b></h2>

Test the package using the **orchestrator R scripts and data sources in test folder**

<h2><b>Work on/with source code</b></h2>

If you want to work on source code (e.g for debugging to fix some bugs or test some extensions or customizations):
  * uncomment the source and library instructions in **_import_and_interface_definitions.R_** and source the script in your code, e.g. **_source("import_and_interface_definitions.R")_** in an orchestrator or in a custom script; 
  * finally run your scripts without **_library(RJDProcessor)_**.

<h2><b>Build</b></h2>

Build the package with **build_package.R** script:
  * if new source files are created, add them to the **_Collate_** field in the **DESCRIPTION** file;
  * if new scode requires external libraries which are not present, add them to the **_Imports_** field in the **DESCRIPTION** file.

