#' Convert excel workbook to multiple csv files
#'
#' Converts an excel workbook into multiple csv files (one per tab).  Names the
#' files in the following format: sheetName_excelName.csv.
#'
#' @param path (character) File location of the excel workbook.
#' @param directory (character) Optional.  Directory to download csv files to.
#' @param ... Optional.  Allows arguments from \link[readxl]{read_excel}
#' Defaults to the base directory that \code{path} is located in.
#'
#' @importFrom readxl excel_sheets read_excel
#'
#' @export
#'
#' @author Dominic Mullen \email{dmullen17@@gmail.com}
#'
#' @return (invisible())
excel_to_csv <- function(path, directory = NULL, ...) {
    # Stop if the user doesn't have the readxl package installed
    if (!requireNamespace("readxl")) {
        stop(call. = FALSE,
             "The readxl package is required to convert excel workbooks to csv. Please install it and try again.")
    }

    stopifnot(file.exists(path))
    if (!is.null(directory)) {
        stopifnot(file.exists(directory))
    } else {
        directory = dirname(path)
    }

    excel_name <- basename(path)
    excel_name <- gsub("\\.xls[x]?$", "", excel_name, ignore.case = TRUE)

    # Try to read excel file and split into csvs
    tryCatch({
        sheets <- readxl::excel_sheets(path)

        lapply(seq_along(sheets), function(i) {
            csv = readxl::read_excel(path, sheet = sheets[i], ...)

            if (length(sheets) == 1) {
                file_name <- paste0(excel_name, ".csv")
            } else {
                file_name <- paste0(excel_name, "_", sheets[i], ".csv")}

            file_path <- file.path(directory, file_name)
            utils::write.csv(csv, file_path , row.names = FALSE)})

    },
    error = function(e) {message("Error converting: ", path, " to csv\n")}
    )

    return(invisible())
}