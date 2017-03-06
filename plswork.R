#suffered through by Shivani

#Before you begin: if you're using Windows, you need to download the the windows xpdf packages found in the pdftool folder (should be the zipped file) 
#Follow these instructions to add the binaries to your Windows path: https://mbnuijten.files.wordpress.com/2013/08/manualinstallationxpdflakens.pdf

require(tm)
library(tm)

#
# Set your own working directory
#

setwd("G:/DIIG/pdftool")
#
# Create a list with the files path
#
files <- list.files(path = './files/')
files <- sapply(files, function(x) paste0('./files/', x))

# -------------------------------------------------------------------------------

#
# EXTRACT FUNCTION
#

#
# `Rpdf` is for reading the PDF docs.
# Save the docs in a Corpus (a database for text)
#
Rpdf <- readPDF(control = list(text = "-layout"))
docs <- Corpus(URISource(files), readerControl = list(reader = Rpdf))


#
# The function `get_mission` will extract the "Mission and Description" from all of the PDFS in the file.
#

get_mission <- function(document) {
  
  content <- document$content
  n <- length(content)
  
  mission_row <- grep('Mission and Description$', content)
  initial_row <- mission_row + 2
  
  blank_rows <- grep ('^$', content)
  
  i <- 1
  final_row <- blank_rows[blank_rows > initial_row][i]
  while(content[final_row + 2] != "" &
      length(blank_rows[blank_rows > initial_row]) <= i){
    i <- i + 1
    final_row <- blank_rows[blank_rows > initial_row][i] - 1
  }
  
  text_vector <- content[initial_row:final_row]
  text <- paste(text_vector, collapse = ' ')
  
  return(text)
  
}

# -------------------------------------------------------------------------------

#
# READ AND OUTPUT
#
missions <- sapply(docs, function(x) get_mission(x))
n <- length(missions)
output_text <- c("")

for (i in 1:n) {
  name <- names(missions)[i]
  txt <- missions[i]
  output_text <- c(output_text, name, "", txt, "")
}

write(output_text, "output.txt")
