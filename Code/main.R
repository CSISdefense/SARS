##
## Extract information, main file
##
## Shivani Pandya 
## April, 2017
##

source("./functions.R")

i <- 0
filenames <- get_spreadsheet_names()

for (filename in filenames) {
    i <- i + 1
    print("--------------------------------------")
    print(paste("File", i, "/", length(filenames)))
    print("--------------------------------------")
    print("Filename:")
    print(filename)
    extract_and_save_data(filename)
    print("--------------------------------------")
}
