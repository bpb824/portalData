# portalData
Bryan's repo for storing exploration and analysis tools and results used for PORTAL data quality project for the Oregon Department of Transportation and Portland State University. 

Tools are written in R, outputs in various forms such as: images, documents, shiny apps, etc. 

Requires database credentials to use. The file "db_credentials-template.json" should be appropriately edited (with database credentials) and renamed to "db_credentials.json" in order to be loaded by the scripts. PORTAL database credentials can be obtained from [Kristen Tufte](mailto:tufte@pdx.edu)

.rds files not included in repository as some are very large. These are data inputs and outputs for scripts. A zip folder of this material is stored on an ODOT Region 1 planning unit folder. Contact [Chi Mai](mailto:Chi.Mai@odot.state.or.us) for access. 

**File Directory Guide**
* **_data**: This folder contains all data inputs and outputs from scripts
* **_graphicResources**: This folder contains some source images used in outputs.
* **_results**: This folder contains outputs from scripts, such as plots (in the /img subfolder) and html outputs (in the /web subfolder)
* **_scripts**: This folder contains the scripts used to conduct the analyes and prepare data for Shiny web apps.
* **_shiny**: This folder contains the code for the shiny applications used. In order to run an application, open server.R or ui.R for that application and run the app from the RStudio console. The apps can be served on the web from a Shiny Server. 
* **z_archive**: Miscellaneous old file archive. 

Within each folder in the file directory, contents are broken up by sub-project. The sub-projects are listed and described below:

**Sub-project Guide**
* **dataOutages**: This sub-project investigated data outages across the Oregon portion of the PORTAL freeway data system. More information about this sub-project is available here: http://bigtransportdata.com/portal?section=data-outages
*  **laneNumberCheck**: This sub-project investigated the differences in mean lane speeds to check if lane numbers were set properly within the metadata. More information about this sub-project is available here: http://bigtransportdata.com/portal?section=speed-diff-analysis
*  **outlierExploration**: This sub-project prepared a large sample of raw data for the Oregon PORTAL freeway data system to detect outlier measurements using the created web app. More information about this sub-project is available here: http://bigtransportdata.com/portal?section=outlier-detection
*  **specificIssues**: This sub-project investigated specific documented issues within the PORTAL freeway database system. More information about this sub-project is available here: http://bigtransportdata.com/portal?section=specific-issues
*  **speedValidation**: This sub-project investigated several stations where speeds were suspect using  comparison of PORTAL data with ground truth data collected using a speed gun. More information about this project is available here: http://bigtransportdata.com/portal?section=speed-validation
*  **valueFlagging**: This sub-project used theoretical threshold values to detect likely erroneous measurements in the PORTAL freeway data system. More information about this project is available here: http://bigtransportdata.com/portal?section=error-value-detect
