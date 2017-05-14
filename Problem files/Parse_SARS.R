require(tidyverse)
library(openxlsx)

setwd("K:/shivani/pdf to excel")

sheets <- getSheetNames("14-F-0402_DOC_33_GPS_OCX_December2013SAR.xlsx")

###############################################################################

descriptions <- data.frame(
  Source.File = rep("14-F-0402_DOC_33_GPS_OCX_December2013SAR.xlsx", length(sheets)),
  Table = character(length(sheets)),
  Description = character(length(sheets)),
  stringsAsFactors = FALSE
)

##################################################################################

datalist = list()


####################################################################################

find_sheet <- function(sheet){
  
  
  for(i in seq_along(sheets)){
  
  current_sheet <- FYDP_open_sheet("14-F-0402_DOC_33_GPS_OCX_December2013SAR.xlsx", sheets[i])
  if(grepl("Cost and Funding", current_sheet[1,1]) &
     grepl("Cost and Summary", current_sheet[2,1])){

    
    # current_sheet <- FYDP_tidy_sheet(current_sheet)
    # current_sheet <- mutate(current_sheet, Table = sheets[i])
    # datalist[[i]] <- current_sheet
    cat("Found the cost table, ", i, "\n")
 
  }
  
  stop()
  cat("Reading", i, "\n")
  
  
}

} 











