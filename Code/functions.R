##
## Functions to extract information from Excel files
##
## TODO: Confidence is not always present?
##

require(gdata)
## require(xlsx)

PROGRAM_NAME_TABLE_NUMBERS     <- c(2, 4, 5)
COST_AND_FUNDING_TABLE_NUMBERS <- c(6, 13, 14, 17, 18, 19, 20, 22)

APPROPIATION_ROW_UNITS    <- 11
APPROPIATION_COLUMN_UNITS <- 8

QUANTITY_ROW_UNITS    <- 3
QUANTITY_COLUMN_UNITS <- 4

APPROPRIATION_COLUMN_NAMES <- c(
    "appropriation",
    "sar_by",
    "apb_objective_by",
    "apb_threshold_by",
    "estimate_by",
    "sar_ty",
    "apb_ty",
    "estimate_ty"
)

QUANTITY_COLUMN_NAMES <- c(
    "quantity",
    "sar",
    "apb",
    "estimate"
)

APPROPRIATION_COLUMN_COMBINATIONS <- list(
    ## Column differentials    ## Spreadsheets
    c(2, 4, 3, 2, 3, 1, 2),  ## 1, 4, 6
    c(2, 3, 2, 2, 2, 1, 1),  ## 2, 5
    c(2, 5, 5, 2, 3, 1, 2),  ## 3
    c(1, 3, 1, 3, 2, 3, 2),  ## 7
    c(1, 2, 1, 2, 2, 2, 2),  ## 8, 12
    c(2, 3, 1, 3, 2, 3, 2),  ## 9, 11, 13
    c(1, 3, 1, 3, 2, 2, 2)   ## 10
)

QUANTITY_COLUMN_COMBINATIONS <- list(
    ## Column differentials   ## Spreadsheets
    c(3, 5, 4),             ## 1, 4, 6
    c(3, 7, 5),             ## 3
    c(2, 4, 5),             ## 7
    c(3, 4, 5),             ## 9, 11, 13
    c(1, 1, 1),             ## 2, 10 (next sheet)
    c(NA)                   ## 5, 8, 12  (ignored)
)

## Ensure the local results directory exists
dir.create("./results/", showWarnings = FALSE)


prepare_directory <- function(filename) {
    ##
    ## Process the `program_name` to substitute spaces by underscores, remove
    ## parenthesis, and make all letters lowercase. This is to make sure that
    ## the directory name is useful in a variety of operating systems.
    ##
    ## Given that we don't know for certain which table number contains the
    ## information for the program name, we iterate overall the options that I
    ## found in the current spreadsheet files, which are contained in
    ## `PROGRAM_NAME_TABLE_NUMBERS`. We'll take the first one found.
    ##
    index <- 0
    program_name <- character(0)
    while (not_found(program_name) &&
           index <= length(PROGRAM_NAME_TABLE_NUMBERS)) {
        index <- index + 1
        table_number <- PROGRAM_NAME_TABLE_NUMBERS[index]
        table <- read_table(filename, table_number)
        program_name <- get_program_name(table)
    }
    if (not_found(program_name)) {
        stop("No `program_name` found for current table.")
    }
    program_name <- tolower(program_name)
    program_name <- gsub("/",    "_", program_name)
    program_name <- gsub("-",    "_", program_name)
    program_name <- gsub("\\s",  "_", program_name)
    program_name <- gsub("\\\\", "",  program_name)
    program_name <- gsub("\\(",  "",  program_name)
    program_name <- gsub("\\)",  "",  program_name)
    directory <- paste("./results/",  program_name, sep = "")
    dir.create(directory, showWarnings = FALSE)
    return(directory)
}

get_program_name <- function(table, table_number) {
    ##
    ## Get the program name given the assumption that it is under the exact
    ## string "Program Name" in the "Program.Information" column.
    ##
    program_name_title_row <- which(
        table$Program.Information == "Program Name"
    )
    program_name <- as.character(
        table$Program.Information[program_name_title_row + 1]
    )
    return(program_name)
}

read_table <- function(filename, table_number) {
    ##
    ## Read table from spreadsheet file using the package that best suits you.
    ##

    ## Requires the `gdata` package.
    table <- read.xls(filename, sheet = table_number, header = TRUE)

    ## Requires the `xlsx` package.
    ## table <- read.xlsx(filename, sheetIndex = table_number, header = TRUE)

    return(table)
}

get_spreadsheet_names <- function() {
    ##
    ## Return the spreadsheet files in the "./files" subdirectory
    ##
    return(paste("./files/", list.files(path = "./files/"), sep = ""))
}

extract_and_save_data <- function(filename) {

    directory <- prepare_directory(filename)
    print("Directory:")
    print(directory)

    index <- 0
    found <- FALSE
    while (!found && index <= length(COST_AND_FUNDING_TABLE_NUMBERS)) {
        index <- index + 1
        table_number <- COST_AND_FUNDING_TABLE_NUMBERS[index]

        tryCatch({
            table <- read_table(filename, table_number)
        }, error = function(e) {
            print(e)
            warning(paste("Could not read table: ", table_number))
        })

        if ("Cost.and.Funding" %in% colnames(table)) {
            found <- TRUE
            print(paste("Cost and funding table: ", table_number))
            if (grepl("Cost Summary", table$Cost.and.Funding[1])) {
                if (multiple_cost_projects(table$Cost.and.Funding[1])) {
                    ##
                    ## CASE: Multiple "Cost Summary" tables
                    ##
                    ## NOTE: I'm assuming that the "Cost Summary Total" table is
                    ## the first one to appear from left to right in the
                    ## spreadsheet.
                    ##
                    ## NOTE: the "Quantity" table is ignored in this case as
                    ## it's only present for "subprojects".
                    ##
                    print("Cost summary type:       multiple")
                    data <- get_appropriation_data(table)
                    save_data(data, directory, "appropriation_total")
                } else {
                    ##
                    ## CASE: Single "Cost Summary" table
                    ##
                    print("Cost summary type:       single")
                    data <- get_appropriation_data(table)
                    save_data(data, directory, "appropriation")
                    data <- get_quantity_data(table, filename, table_number)
                    save_data(data, directory, "quantity")
                }
            } else {
                stop("No 'Cost Summary' found in 'Cost and Funding' table.")
            }
        }
    }
    if (!found) {
        stop("No 'Cost and Funding' table found")
    }
    return(table_number)
}

multiple_cost_projects <- function(string) {
    ##
    ## When we find "-", e.g. "Cost Summary - Total Program" it means that there
    ## are various "Cost Summary" tables that are dissaggregated, and we should
    ## get the data for all of them separately.
    ##
    return(grepl("-", string))
}

not_found <- function(value) {
    return(
        identical(character(0), value) ||
        identical(integer(0),   value)
    )
}

get_appropriation_data <- function(table) {
    ##
    ## Use the combinations of columns found in the spreadsheets to get the
    ## actual data from the appropriation tables. In case that we're asking for
    ## an invalid combination of columns, we need to catch the error and ignore
    ## it.
    ##
    initial_column <- 1
    initial_row <- which(table$Cost.and.Funding == "RDT&E")
    rows <- initial_row:(initial_row + APPROPIATION_ROW_UNITS - 1)

    combination <- 0
    found <- FALSE
    while (!found && combination <= length(APPROPRIATION_COLUMN_COMBINATIONS)) {
        data <- NA
        combination <- combination + 1
        columns <- compute_appropriation_columns(combination)

        tryCatch({
            data <- table[rows, columns]
        }, error = function(e) {
            ## Undefined columns selected due to combination
            dummy <- NULL
        })

        if (!is.na(data) && test_correct_appropriation_data(data)) {
            print("Appropriation column differentials:")
            print(columns)
            found <- TRUE
        }
    }
    if (!found) {
        stop("Did not find a combination of columns for appropriation table.")
    }
    colnames(data) <- APPROPRIATION_COLUMN_NAMES
    print("Extracted appropriation table:")
    print(data)
    return(data)
}

get_quantity_data <- function(table, filename, table_number) {
    ##
    ## Use the combinations of columns found in the spreadsheets to get the
    ## actual data from the appropriation tables. In case that we're asking for
    ## an invalid combination of columns, we need to catch the error and ignore
    ## it. We need the `table_number` in case the "Quantity" table is in the
    ## table that is right next to the current one.
    ##
    combination <- 0
    found <- FALSE
    initial_column <- 1
    while (!found && combination <= length(QUANTITY_COLUMN_COMBINATIONS)) {
        initial_row <- which(table$Cost.and.Funding == "Quantity")
        if (not_found(initial_row)) {
            table <- read_table(filename, table_number + 1)
            if ("Quantity" %in% colnames(table)) {
                initial_row <- 0
            } else if ("Quantity" == table[1, 1]) {
                initial_row <- 1
            } else {
                stop("Could not find 'Quantity' cell.")
            }
        }
        rows <- (initial_row + 1):(initial_row + QUANTITY_ROW_UNITS)
        data <- NA
        combination <- combination + 1
        columns <- compute_quantity_columns(combination)

        tryCatch({
            data <- table[rows, columns]
        }, error = function(e) {
            ## Undefined columns selected due to combination
            dummy <- NULL
        })

        if (!is.na(data) && test_correct_quantity_data(data)) {
            print("Quantity column differentials:")
            print(columns)
            found <- TRUE
        }
    }
    if (!found) {
        stop("Did not find a combination of columns for quantity table.")
    }
    colnames(data) <- QUANTITY_COLUMN_NAMES
    print("Extracted quantity table:")
    print(data)
    return(data)
}

compute_appropriation_columns <- function(combination) {
    ##
    ## The data structure used to represent combinations contains the number of
    ## units that we need to move from the "current" column to the next one (and
    ## thus the next "current" column) to get all the necessary columns for the
    ## combination. This is due to the spreadsheets not having data in the
    ## consecutive columns, but spread through different combinations.
    ##
    combination <- APPROPRIATION_COLUMN_COMBINATIONS[[combination]]
    columns <- rep(1, APPROPIATION_COLUMN_UNITS)
    index <- 1
    for (lateral_movement in combination) {
        index <- index + 1
        columns[index] <- columns[index] + sum(combination[1:(index - 1)])
    }
    return(columns)
}

compute_quantity_columns <- function(combination) {
    ##
    ## In this data structure the first element indicates whether to work on the
    ## current table or on the one to the right (ignored in this function), and
    ## the second element in each list corresponds to the same data structure as
    ## in the case of the "appropriation" table.
    ##
    combination <- QUANTITY_COLUMN_COMBINATIONS[[combination]]
    columns <- rep(1, QUANTITY_COLUMN_UNITS)
    index <- 1
    for (lateral_movement in combination) {
        index <- index + 1
        columns[index] <- columns[index] + sum(combination[1:(index - 1)])
    }
    return(columns)
}

test_correct_appropriation_data <- function(data) {
    ##
    ## Test whether or not we have the correct column combination by checking
    ## the values of the first row ("RDT&E") since it's the one that has the
    ## most stable pattern throughout the spreadsheets.
    ##
    ## Assumption: from what I've seen, if any element is NA or empty, then the
    ## row is not valid.
    ##
    RDTE_row <- data[1, ]
    for (element in RDTE_row) {
        if (as.character(element) == "" ||
            as.character(element) == "NA") {
            return(FALSE)
        }
    }
    return(TRUE)
}

test_correct_quantity_data <- function(data) {
    ##
    ## Test whether the quantity data we have is correct. All values must be
    ## integers greater or equal to zero.
    ##
    for (row in 1:nrow(data)) {
        for (col in 1:ncol(data)) {
            if (col == 1) {
                if (!(data[row, col] %in% c("RDT&E", "Procurement", "Total"))) {
                    return(FALSE)
                }
            } else {
                if (is.na(as.numeric(as.character(data[row, col])))) {
                    return(FALSE)
                } else {
                    if (as.numeric(as.character(data[row, col])) < 0) {
                        return(FALSE)
                    }
                }
            }
        }
    }
    return(TRUE)
}

save_data <- function(data, directory, filename) {
    filename <- paste(directory, "/", filename, ".csv", sep = "")
    write.csv(data, file = filename, row.names = FALSE)
}
