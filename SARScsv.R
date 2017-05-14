# Shivani Pandya
#
# Descriptions: outputs csv of relevant SARS fields 
#
# A text-mining package is required
#
library(tm)

# If you don't have the package...
# install.packages('tm')



#
# Set working directory
#

setwd("G:/DIIG/pdftool")
#
# Create a list with the files path
#
files <- list.files(path = './files/.')
files <- sapply(files, function(x) paste0('./files/', x))

# -------------------------------------------------------------------------------

#
#     EXTRACT FUNCTION
#

#
# `Rpdf` is created for reading the PDF docs.
# Save the docs in a Corpus (a database for text)
#
Rpdf <- readPDF(control = list(text = "-layout"))
docs <- Corpus(URISource(files), readerControl = list(reader = Rpdf))


#
# The function `get_mission` extact the "Mission and Description" from the PDF.
#

extract_section <- function(document, heading, initial_step) {
  
  content <- document$content
  n <- length(content)
  
  search_heading <- paste0('^ ', heading,'$|^', heading, '$')
  # mission_row <- grep('^Mission and Description$', content)
  mission_row <- grep(search_heading, content)
  initial_row <- mission_row + initial_step
  
  blank_rows <- grep ('^$', content)
  
  i <- 1
  final_row <- blank_rows[blank_rows > initial_row][i] - 1
  while (content[final_row + 2] != "" &
         length(blank_rows[blank_rows > initial_row]) <= i) {
    i <- i + 1
    final_row <- blank_rows[blank_rows > initial_row][i] - 1
  }
  
  text_vector <- content[initial_row:final_row]
  text <- paste(text_vector, collapse = ' ')
  
  return(text)
  
}

# -------------------------------------------------------------------------------

#
#     READ AND OUTPUT
#

df <- data.frame(file = names(files),
                 mission = '',
                 program_name = '',
                 DoD_component = '')

#
# Extract text from docs
#
missions <- sapply(docs, function(x) extract_section(x, 'Mission and Description', 2))


########################## TESTING ONLY
#
# to see the content it failed on type docs[[num]]$content
# where num is the last number that successfully read + 1
#
for(z in 1:length(docs)){
extract_section(docs[[z]], "Program Name", 1)
 cat(z, "\n")
}


##########################################

program_names <- sapply(docs, function(x) extract_section(x, 'Program Name', 1))
components <- sapply(docs, function(x) extract_section(x, 'DoD Component', 1))

df$mission <- missions
df$program_name <- program_names
df$DoD_component <- components

write.csv(df, 'SARSoutput.csv')