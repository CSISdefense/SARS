
#
# A text-mining package is required
#
require(tm)
# If you don't have the package...
# install.packages('tm')



#
# Set working directort
#

# YOU SHOULD CHANGE THIS TO YOUR PERSONAL WORKING DIRECTORY!!!
setwd('/')
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
# We save the docs in a Corpus (a database for text)
#
Rpdf <- readPDF(control = list(text = "-layout"))
docs <- Corpus(URISource(files), readerControl = list(reader = Rpdf))


#
# The function `get_mission` extact the "Mission and Description" from the PDF.
#

extract_section <- function(document, regex, initial_step, multiple) {
  
  content <- document$content
  n <- length(content)
  
  mission_row <- grep(regex, content)
  initial_row <- mission_row + initial_step
  
  if (length(initial_row) == 0) {
    return(NA)
  }
  
  blank_rows <- grep ('^$', content)
  
  final_row <- initial_row
  
  if (multiple) {
    i <- 1
    final_row <- blank_rows[blank_rows > initial_row][i] - 1
    
    while (content[final_row + 2] != "" & length(blank_rows[blank_rows > initial_row]) <= i) {
      i <- i + 1
      final_row <- blank_rows[blank_rows > initial_row][i] - 1
    } 
  }
  
  text_vector <- content[initial_row:final_row]
  text <- paste(text_vector, collapse = ' ')
  
  return(text)
  
}

# -------------------------------------------------------------------------------

#
#     READ AND OUTPUR
#

df <- data.frame(file = names(files),
                 mission = '',
                 program_name = '',
                 DoD_component = '')

#
# Extract text from docs
#
missions <- sapply(docs, function(x) extract_section(x, '^Mission and Description$', 2, TRUE))
program_names <- sapply(docs, function(x) extract_section(x, 'Program Name|(Popular Name)', 1, FALSE))
components <- sapply(docs, function(x) extract_section(x, 'DoD Component', 1, FALSE))

df$mission <- missions
df$program_name <- program_names
df$DoD_component <- components

write.csv(df, 'output.csv')