# Set working directory
sysName = Sys.info()["sysname"]
if (sysName == "Linux"){
  setwd(setwd("~/_ODOT_Portal/investigations"))
}else{
  setwd("/Users/bblanc/OneDrive/_Portal_ODOT/investigations/")
}

# Load packages
require(knitr)
require(markdown)
require(rmarkdown)

render("valueFlagReport.Rmd", "pdf_document")
