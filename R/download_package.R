#' Remove and substitute special characters in a string.
#'
#' This is a helper function for the 'download_package' function.  This was
#' created as a helper so that users can edit the helper, rather than 'download_package'
#' if they want differing special character substitions.  Substitues special
#' characters from a package identifier. Can be generalized for use with any pid.
#'
#' @author Dominic Mullen, \email{dmullen17@@gmail.com}
#'
#' @param pid (character) The identifier a dataOne object.
#'
#' @return (character) The formatted identifer as a string
remove_special_characters <- function(pid) {
    pid <- pid %>%
        gsub(":", "", .) %>%
        gsub("\\/", "", .) %>%
        gsub("\\.", "", .)

    return(pid)
}

#' Convert excel workbook to multiple csv files
#'
#' This is a helper function for download_package.
#'
#' @param path (character) File location of the excel workbook.
#' @param prefix (character) Optional prefix to prepend to the file name.
#'
#' @author Dominic Mullen \email{dmullen17@@gmail.com}
#'
#' @return (invisible())
excel_to_csv_prefix <- function(path, prefix = NULL) {
    stopifnot(file.exists(path))

    # Try to read excel file and split into csvs
    tryCatch({
        sheets <- excel_sheets(path)

        excel_name <- basename(path)
        excel_name <- gsub("\\.xls[x]?$", "", excel_name, ignore.case = TRUE)

        lapply(seq_along(sheets), function(i) {
            csv = read_excel(path, sheet = sheets[i])

            if (!is.null(prefix)) {
                excel_name <- gsub(prefix, "", excel_name)

                if (length(sheets) == 1) {
                    file_name <- paste0(prefix, "_", excel_name, ".csv")
                } else {
                    file_name <- paste0(prefix, "_", excel_name, "_", sheets[i], ".csv")
                }

            } else {
                if (length(sheets) == 1) {
                    file_name <- paste0(excel_name, ".csv")
                } else {
                    file_name <- paste0(excel_name, "_", sheets[i], ".csv")
                }
            }

            file_path <- file.path(dirname(path), file_name)

            utils::write.csv(csv, file_path , row.names = FALSE)})

    },
    error = function(e) {message("Error converting: ", path, " to csv\n")}
    )

    return(invisible())
}

#' Append one list to another.
#'
#' This function appends one list to another list. It can also be used to
#' prepend, just reverse the order of the lists.
#'
#' @param list1 (list) The list to append to.
#' @param list2 (list) The list being appended.
#'
#' @author Dominic Mullen, \email{dmullen17@@gmail.com}
#'
#' @examples
#' \dontrun{
#' appended_lists <- append_lists(list(1:3), list("a", "b", mean))
#' }
#'
append_lists <- function(list1, list2) {
    # TODO Make this function handle infinite lists

    stopifnot(is.list(list1))
    stopifnot(is.list(list2))
    stopifnot(length(list1) > 0)
    stopifnot(length(list2) > 0)

    n1 <- length(list1)
    n2 <- length(list2)

    for (i in 1:n2) {
        list1[[n1+i]] <- list2[[i]]
    }

    return(list1)
}

#' Calculate the total size (in bytes) of the Objects in a Data Package
#'
#' @param mn (MNode/CNode) The Node to query for Object sizes
#' @param resource_map_pid (character) The identifier of the Data Package's Resource Map
#' @param formatType (character) Optional. Filter to just Objects of the given formatType. One of METADATA, RESOURCE, or DATA or * for all types
#'
#' @author Bryce Mecum
#'
#' @return (numeric) The sum of all Object sizes in the Data Package
get_package_size <- function(mn, resource_map_pid, formatType = "*") {
    size_query <- dataone::query(mn,
                                 paste0("q=resourceMap:\"",
                                        resource_map_pid,
                                        "\"+AND+formatType:",
                                        formatType, "&fl=size"),
                                 as = "data.frame")

    if (nrow(size_query) == 0) {
        return(0)
    }

    sum(as.integer(size_query$size))
}

#' Format bytes to human readable format
#'
#' This is a helper function for 'download_package'
#'
#' @param download_size (numeric) Total size in bytes
convert_bytes <- function(download_size) {
    #' TODO - make this function more robust using gdata::humanReadable as a template
    stopifnot(is.numeric(download_size))

    if (download_size >= 1e+12) {
        download_size <- round(download_size/(1e+12), digits = 2)
        unit = " terabytes"
    } else if (1e+12 > download_size & download_size >= 1e+9) {
        download_size <- round(download_size/(1e+9), digits = 2)
        unit = " gigabytes"
    } else if (1e+9 > download_size & download_size >= 1e+6) {
        download_size <- round(download_size/(1e+6), digits = 2)
        unit = " megabytes"
    } else if (1e+6 > download_size) {
        download_size = round(download_size/1000, digits = 2)
        unit = " kilobytes"
    }

    return(paste0(download_size, " ", unit))
}

#' Download multiple data objects using their pids.
#'
#' @description Download mutiple dataone objects.  This is a helper function
#' for 'datamgmt::download_package'
#' @param mn (MNode) The Dataone Member Node to download the data objects from.
#' @param data_pids (character) A vector of Data object pids.
#' @param out_paths (character) A vector of file paths to download to.
#' @param n_max (numeric) Optional.  Number of attempts at downloading a Data object.
download_data_objects <- function(mn, data_pids, out_paths, n_max = 3) {
    stopifnot(methods::is(mn, "MNode"))
    stopifnot(is.character(data_pids))

    lapply(seq_len(data_pids), function(i) {

        if (file.exists(out_paths[i])) {
            warning(call. = FALSE,
                    paste0("The file ", out_paths[i], " already exists. Skipping download."))
        } else {
            n_tries <- 0
            dataObj <- "error"

            while (n_tries < n_max & dataObj[1] == "download_error") {
                dataObj <- tryCatch({
                    dataone::getObject(mn, data_pids[i])
                    writeBin(dataObj, out_paths[i])
                },
                error = function(e) {return("error")})
                ntries <- ntries + 1
            }
        }
    })

    return(invisible())
}

#' Download one Package without its child Packages.
#'
#' This function downloads all of the Data Objects in a Data Package to the local filesystem.
#' It is particularly useful when a Data Package is too large to download using the web interface.
#'
#' Setting \code{check_download_size} to \code{TRUE} is recommended if you are uncertain of the total download size and want to avoid downloading very large Data Packages.
#'
#' This function will also download any data objects it finds in any child Data Packages of the input data package.
#' If you would only like to download data from one Data Package, set \code{download_child_packages} to \code{FALSE}.
#'
#' @param mn (MNode) The Member Node to download from.
#' @param resource_map_pid (chraracter) The identifier of the Resource Map for the package to download.
#' @param download_directory (character) The path of the directory to download the package to.
#' @param prefix_file_names (logical) Optional.  Whether to prefix file names with the package metadata identifier.  This is useful when downloading files from multiple packages to one directory.
#' @param convert_excel_to_csv (logical) Optional. Whether to convert excel files to csv(s).  The csv files are downloaded as sheetName_excelWorkbookName.csv
#' @param download_column_metadata (logical) Optional.  Whether to download attribute (column) metadata as csvs.  If using this its recommened to also set \code{prefix_file_names = TRUE}
#' @param convert_excel_to_csv (logical) Optional. Whether to convert excel files to csv(s).  This is not recommended if the separate csv files already exist in the package. The csv files are downloaded as sheetName_excelWorkbookName.csv
#'
#' @importFrom utils setTxtProgressBar txtProgressBar write.csv
#'
#' @author Dominic Mullen, \email{dmullen17@@gmail.com}
#'
download_one_package <- function(mn,
                                 resource_map_pid,
                                 download_directory,
                                 prefix_file_names = TRUE,
                                 download_column_metadata = FALSE,
                                 convert_excel_to_csv = FALSE) {
    # TODO Add option for downloading in folder structure that mirrors nesting
    # rather than only all files in one folder
    # TODO Convert check download size to helper function?

    # Check that input arguments are in the correct format
    stopifnot(methods::is(mn, "MNode"))
    stopifnot(is.character(resource_map_pid))
    stopifnot(file.exists(download_directory))
    stopifnot(is.logical(convert_excel_to_csv))
    if (convert_excel_to_csv == TRUE) {
        # Stop if the user doesn't have the readxl package installed
        if (!requireNamespace("readxl")) {
            stop(call. = FALSE,
                 "The readxl package is required to show progress. Please install it and try again.")
        }
    }

    # Get package pids
    package <- arcticdatautils::get_package(mn, resource_map_pid, file_names = TRUE)

    # Initialize data_pids, return if no data present
    if (length(package$data) == 0) {
        return(invisible())
    } else {
        data_pids <- package$data
    }

    # Create file names
    # file_names <- sapply(names(data_pids), function(i) {
    #     ifelse(is.na(file_names[i]),
    #            gsub('[^[:alnum:]]', '_', package$data[i]),
    #            file_names[i])})

    file_names <- names(data_pids)
    prefix <- character(0)
    if (prefix_file_names == TRUE) {
        prefix <- remove_special_characters(package$metadata)
        file_names <- paste0(prefix, "_", file_names)
    }
    out_paths <- file.path(download_directory, file_names)

    download_data_objects(mn, data_pids, out_paths)

    if (download_column_metadata == TRUE) {
        eml <- EML::read_eml(rawToChar(dataone::getObject(mn, package$metadata)))
        download_eml_attributes(eml, download_directory, prefix_file_names)
    }

    if (convert_excel_to_csv == TRUE) {

    }

    return(invisible())
}

#' Download a Data Package
#'
#' This function downloads all of the Data Objects in a Data Package to the local filesystem.
#' It is particularly useful when a Data Package is too large to download using the web interface.
#'
#' Setting \code{check_download_size} to \code{TRUE} is recommended if you are uncertain of the total download size and want to avoid downloading very large Data Packages.
#'
#' This function will also download any data objects it finds in any child Data Packages of the input data package.
#' If you would only like to download data from one Data Package, set \code{download_child_packages} to \code{FALSE}.
#'
#' @param mn (MNode) The Member Node to download from.
#' @param resource_map_pid (chraracter) The identifier of the Resource Map for the package to download.
#' @param download_directory (character) The path of the directory to download the package to.
#' @param check_download_size (logical) Optional.  Whether to check the total download size before continuing.  Setting this to FALSE speeds up the function, especially when the package has many elements.
#' @param download_child_packages (logical) Optional.  Whether to download data from child packages of the selected package.
#' @param prefix_file_names (logical) Optional.  Whether to prefix file names with the package metadata identifier.  This is useful when downloading files from multiple packages to one directory.
#' @param convert_excel_to_csv (logical) Optional. Whether to convert excel files to csv(s).  The csv files are downloaded as sheetName_excelWorkbookName.csv
#' @param download_column_metadata (logical) Optional.  Whether to download attribute (column) metadata as csvs.  If using this its recommened to also set \code{prefix_file_names = TRUE}
#' @param check_first (logical) Optional. Whether to check the PIDs passed in as aruments exist on the MN before continuing. Checks that objects exist and are of the right format type. Setting this to FALSE speeds up the function, especially when the package has many elements.
#'
#' @importFrom utils setTxtProgressBar txtProgressBar write.csv
#'
#' @author Dominic Mullen, \email{dmullen17@@gmail.com}
#'
#' @examples
#' \dontrun{
#' cn <- CNode("PROD")
#' mn <- getMNode(cn, "urn:node:ARCTIC")
#' download_package(mn, "resource_map_urn:uuid:2b4e4174-4e4b-4a46-8ab0-cc032eda8269")
#' }
#'
child_packages <- list()

# Get child package pids
if (download_child_packages == TRUE) {
    # Check that child packages exist
    if (length(package$child_packages) != 0) {
        n_child_packages <- length(package$child_packages)
        progressBar <- utils::txtProgressBar(min = 0, max = n_child_packages, style = 3)

        message("\nDownloading identifiers from child packages...")

        # Loop through child packages and extract pids using get_package()
        child_packages <- lapply(seq_len(n_child_packages), function(i) {
            utils::setTxtProgressBar(progressBar, i)
            arcticdatautils::get_package(mn, package$child_packages[i], file_names = TRUE)
        })

        close(progressBar)
    }
}

#' Download one or multiple Data Packages
#'
#' This function is wrapper for download_package It downloads all of the Data Objects in a Data Package
#' to the local filesystem.  It is particularly useful when a Data Package is too large to download using
#' the web interface.
#'
#' Setting \code{check_download_size} to \code{TRUE} is recommended if you are uncertain of the total download size and want to avoid downloading very large Data Packages.
#'
#' This function will also download any data objects it finds in any child Data Packages of the input data package.
#' If you would only like to download data from one Data Package, set \code{download_child_packages} to \code{FALSE}.
#'
#' @param mn (MNode) The Member Node to download from.
#' @param resource_map_pids (chraracter) The identifiers of the Resource Maps for the packages to download.
#' @param ... Allows arguments from \code{\link{download_package}}
#'
#' @author Dominic Mullen, \email{dmullen17@@gmail.com}
#'
#' \code{\link{download_package}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cn <- CNode("PROD")
#' mn <- getMNode(cn, "urn:node:ARCTIC")
#' download_packages(mn, c("resource_map_doi:10.18739/A21G1P" "resource_map_doi:10.18739/A2RZ6X"),
#' check_download_size = FALSE, prefix_file_names = TRUE, convert_excel_to_csv == TRUE)
#' }
download_packages <- function(mn, resource_map_pids, ...) {

    stopifnot(all(is.character(resource_map_pids)))
    stopifnot(length(resource_map_pids) > 0)

    n_packages <- length(resource_map_pids)

    lapply(seq_len(n_packages), function(i)
    {download_package(mn, resource_map_pid = resource_map_pids[i], ...)})

    return(invisible())
}


# # Check total download size
# if (check_download_size) {
#     child_package_resource_map_pids <- lapply(child_packages, function(package) {
#         package$resource_map
#     })
#
#     download_size <- sum(
#         vapply(c(resource_map_pid, child_package_resource_map_pids), function(pid) {
#             get_package_size(mn, pid)
#         }, 0)
#     )
#
#     download_size_message <- convert_bytes(download_size)
#
#     # Prompt user if they wish to continue based on total download size
#     message(paste0("\nYour download is approximately ", download_size_message, "\n"))
#     continue <- readline(prompt = paste0("Proceed with the download (", download_size_message, ")? Input yes/no: "))
#
#     while (!(continue %in% c("yes", "no"))) {
#         message("Type yes or no without quotation marks or capitals\n")
#         continue <- readline(prompt = "Proceed with the download? Input yes/no: ")
#     }
#
#     # Cancel download if "no" was entered
#     if (continue == "no") {
#         stop("Download cancelled by user")
#     }
# }