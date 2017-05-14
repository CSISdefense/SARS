FYDP_open_sheet <- function(filename, sheetname){
  opened <- read.xlsx(
    xlsxFile = filename,
    sheet = sheetname,
    colNames = FALSE
  )
  return(opened)
}


#########################################################################

FYDP_tidy_sheet <- function(sheet){
  
  source("FYDP_tidy_functions.R")
  
  table <- suppressWarnings(FYDPt_cut_table(sheet))
  table <- suppressWarnings(FYDPt_tidy_table(table))
  table <- FYDPt_extend_table(table, sheet)
  
  return(table)
}