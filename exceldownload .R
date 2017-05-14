
################################################################

# Shivani Pandya

#################################################################


setwd('')

library(RCurl)
library(XML)


download_year_xlsx_files <- function(year) {
  #
  # Description
  # -------------------
  # This is a function to download all xls and xlsx files for a specific year
  #
  
  domain <- 'http://comptroller.defense.gov'
  prelink <- 'http://comptroller.defense.gov/Budget-Materials/Budget'
  # Creates the URL for the specific year
  link <- paste0(prelink, year, '/')
  
  # Get html from the specific URL
  htmltxt <- getURL(link)
  htmltxt <- htmlParse(htmltxt)
  
  # Find all <a> elements that ends with "xlsx"
  l1 <- htmltxt['//a[substring(@href, string-length(@href) - 3) = "xlsx"]/@href']
  # Find all <a> elements that ends with "xls"
  l2 <- htmltxt['//a[substring(@href, string-length(@href) - 2) = "xls"]/@href']
  # Join <a> elements
  hreflist <- c(l1, l2)
  
  if (length(hreflist) == 0) {
    return(paste0('0 files downloaded for year ', year, '.'))
  }
  
  # Get absolute URLs (hreflist has only relative urls, we need to paste the domain).
  hreflist <- lapply(hreflist, function(x) {
    domain <- 'http://comptroller.defense.gov'
    link <- paste0(domain, x[1])
    return(link)}
  )
  
  # Get the name of the file (the string after the last "/")
  names <- lapply(hreflist, function(x) {
    txtlist <- strsplit(x, '/')[[1]]
    n <- length(txtlist)
    return(txtlist[n])
  })
  
  # Download all documents in the list
  for (i in 1:length(names)) {
    name <- names[[i]]
    url <- hreflist[[i]]
    download.file(url, name, method='curl')
  }
  
  return(paste0(length(names), ' files downloaded for year ', year, '.'))
}


#
# Run for all the year
#
years <- 1998
for (year in years) {
  d <- download_year_xlsx_files(year)
  print(d)
}