OHDSI Evaluation of Population-Level Estimation Methods
=======================================================

<img src="https://img.shields.io/badge/Study%20Status-Results%20Available-yellow.svg" alt="Study Status: Results Available">

- Analytics use case(s): **Population-Level Estimation**
- Study type: **Methods Research**
- Tags: **-**
- Study lead: **Martijn Schuemie**
- Study start date: **November 1, 2018**
- Study end date: **August 1, 2019**
- Protocol: [**Word file**](https://github.com/ohdsi-studies/MethodsLibraryPleEvaluation/blob/master/extras/OHDSI%20Protocol%20Method%20Evaluation.docx)
- Publications: **Paper under review**
- Results explorer: **[MethodEvalViewer](https://data.ohdsi.org/MethodEvalViewer/)**

This study aims to evaluate the performance of verious methods in the OHDSI methods library. 

How to run
==========
1. Make sure that you have Java installed. If you don't have Java already intalled on your computed (on most computers it already is installed), go to java.com to get the latest version. (If you have trouble building with rJava below, be sure on Windows that your Path variable includes the path to jvm.dll (Windows Button --> type "path" --> Edit Environmental Variables --> Edit PATH variable, add to end ;C:/Program Files/Java/jre/bin/server) or wherever it is on your system.)

2. In R, use the following code to install the study package and its dependencies:

	```r
	install.packages("devtools")
	library(devtools)
    install_github("ohdsi/Cyclops", ref = "v2.0.1")
    install_github("ohdsi/FeatureExtraction", ref = "v2.1.5")
    install_github("ohdsi/EmpiricalCalibration", ref = "v1.4.0")
    install_github("ohdsi/CohortMethod", ref = "v3.0.1")
    install_github("ohdsi/SelfControlledCaseSeries", ref = "v1.4.0")
    install_github("ohdsi/CaseControl", ref = "v1.6.0")
    install_github("ohdsi/SelfControlledCohort", ref = "v1.5.0")
    install_github("ohdsi/CaseCrossover", ref = "v1.1.0")
    install_github("ohdsi/MethodEvaluation", ref = "v1.0.0")
	install_github("ohdsi-studies/MethodsLibraryPleEvaluation")
	```

3. Once installed, you can execute the study by modifying and using the following code:

	```r
	library(MethodsLibraryPleEvaluation)
	
	# Optional: specify where the temporary files (used by the ff package) will be created:
    options(fftempdir = "c:/FFtemp")

    # Maximum number of cores to be used:
    maxCores <- parallel::detectCores()

    # Details for connecting to the server:
	connectionDetails <- createConnectionDetails(dbms = "postgresql",
												 user = "joe",
												 password = "secret",
												 server = "myserver")
												 
    # The name of the database schema where the CDM data can be found:
    cdmDatabaseSchema <- "cdm_truven_ccae_v778.dbo"
    
    # The name of the database
    databaseName <- "CCAE"
    
    # The name of the database schema and tables where the study-specific cohorts will be instantiated:
    outcomeDatabaseSchema <- "scratch.dbo"
    outcomeTable <- "mschuemi_ohdsi_hois_ccae"
    nestingCohortDatabaseSchema <- "scratch.dbo"
    nestingCohortTable <- "mschuemi_ohdsi_nesting_ccae"

    # For Oracle: define a schema that can be used to emulate temp tables:
    oracleTempSchema <- NULL
    
    # Output folder:
    outputFolder <- "c:/MethodsLibraryPleEvaluation_ccae"

	execute <- function(connectionDetails = connectionDetails,
                        cdmDatabaseSchema = cdmDatabaseSchema,
                        oracleTempSchema = oracleTempSchema,
                        outcomeDatabaseSchema = outcomeDatabaseSchema,
                        outcomeTable = outcomeTable,
                        nestingCohortDatabaseSchema = nestingCohortDatabaseSchema,
                        nestingCohortTable = nestingCohortTable,
                        outputFolder = outputFolder,
                        databaseName = databaseName,
                        maxCores = maxCores,
                        cdmVersion = cdmVersion,
                        createNegativeControlCohorts = TRUE,
                        synthesizePositiveControls = TRUE,
                        runCohortMethod = TRUE,
                        runSelfControlledCaseSeries = TRUE,
                        runSelfControlledCohort = TRUE,
                        runCaseControl = TRUE,
                        runCaseCrossover = TRUE,
                        packageResults = TRUE)
	```

	* For details on how to configure```createConnectionDetails``` in your environment type this for help:
	
    	```r
    	?createConnectionDetails
    	```

	* ```cdmDatabaseSchema``` should specify the schema name where your patient-level data in OMOP CDM format resides. Note that for SQL Server, this should include both the database and schema name, for example 'cdm_data.dbo'.

	* ```oracleTempSchema``` should be used in Oracle to specify a schema where the user has write priviliges for storing temporary tables.

4. You can view the results using a Shiny app:

    ```r
    exportFolder <- file.path(outputFolder, "export")
    MethodEvaluation::launchMethodEvaluationApp(exportFolder)
    ```
   
