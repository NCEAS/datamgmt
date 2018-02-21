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
    pid <- gsub(":", "", pid)
    pid <- gsub("\\/", "", pid)
    pid <- gsub("\\.", "", pid)

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

    # Stop if the user doesn't have the readxl package installed
    if (!requireNamespace("readxl")) {
        stop(call. = FALSE,
             "The readxl package is required to convert excel workbooks to csv. Please install it and try again.")
    }

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
#'
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
#' @param download_directory (character) The path of the directory to download the package to. Defaults to the current working directory.
#' @param check_download_size (logical) Optional.  Whether to check the total download size before continuing.  Setting this to FALSE speeds up the function, especially when the package has many elements.
#' @param download_child_packages (logical) Optional.  Whether to download data from child packages of the selected package.
#' @param prefix_file_names (logical) Optional.  Whether to prefix file names with the package metadata identifier.  This is useful when downloading files from multiple packages to one directory.
#' @param convert_excel_to_csv (logical) Optional. Whether to convert excel files to csv(s).  The csv files are downloaded as sheetName_excelWorkbookName.csv
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
download_package <- function(mn,
                             resource_map_pid,
                             download_directory = getwd(),
                             check_download_size = TRUE,
                             download_child_packages = TRUE,
                             prefix_file_names = FALSE,
                             convert_excel_to_csv = FALSE,
                             check_first = TRUE) {
    # TODO How many child levels should it support, currently one. 3 or 4
    # max - could probably do this with a while loop.
    # TODO resource_map_pid argument accepts metadata pids - could change to
    # not accept metadata pids
    # TODO Add option for downloading in folder structure that mirrors nesting
    # rather than only all files in one folder
    # TODO Convert check download size to helper function?

    # Stop if the user doesn't have the readxl package installed
    if (!requireNamespace("readxl")) {
        stop(call. = FALSE,
             "The readxl package is required to show progress. Please install it and try again.")
    }

    # Check that input arguments are in the correct format
    stopifnot(is.character(resource_map_pid))
    stopifnot(is.character(download_directory))
    stopifnot(file.exists(download_directory))
    stopifnot(is.logical(check_download_size))
    stopifnot(is.logical(download_child_packages))
    stopifnot(is.logical(check_first))

    # Get package pids
    package <- arcticdatautils::get_package(mn, resource_map_pid, file_names = TRUE)
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

    # Initialize data pids and filename prefixes vectors
    data_pids <- vector("character")
    filename_prefixes <- vector("character")

    # Select data pids from initial package, if they exist
    if (length(package$data) != 0) {
        data_pids <- package$data
    }

    # Select data pids from child packages and add to data_pids
    child_data_pids <- unlist(lapply(child_packages, function(package) {
        return(package$data)
    }))

    data_pids <- c(data_pids, child_data_pids)

    # Check that data exists
    if (length(data_pids) == 0) {
        message(warning("No data selected.  Double check the package you entered contains data files"))

    } else {

        # Check total download size
        if (check_download_size) {
            child_package_resource_map_pids <- lapply(child_packages, function(package) {
                package$resource_map
            })

            download_size <- sum(
                vapply(c(resource_map_pid, child_package_resource_map_pids), function(pid) {
                    get_package_size(mn, pid)
                }, 0)
            )

            download_size_message <- convert_bytes(download_size)

            # Prompt user if they wish to continue based on total download size
            message(paste0("\nYour download is approximately ", download_size_message, "\n"))
            continue <- readline(prompt = paste0("Proceed with the download (", download_size_message, ")? Input yes/no: "))

            while (!(continue %in% c("yes", "no"))) {
                message("Type yes or no without quotation marks or capitals\n")
                continue <- readline(prompt = "Proceed with the download? Input yes/no: ")
            }

            # Cancel download if "no" was entered
            if (continue == "no") {
                stop("Download cancelled by user")
            }
        }

        # Create filename prefixes
        if (prefix_file_names == TRUE) {
            # List all packages together
            if (length(child_packages) > 0) {
                all_packages <- append_lists(list(package), child_packages)
            } else {
                all_packages <- list(package)
            }

            # Remove special characters from each package metadata identifier and rep for # of data objects in each package
            filename_prefixes <- unlist(lapply(all_packages, function(all_packages) {
                prefix <- remove_special_characters(all_packages$metadata)
                return(rep(prefix, length(all_packages$data)))
            }))
        }

        # Download data pids to selected directory
        n_data_objects <- length(data_pids)
        file_names <- names(data_pids)
        progressBar <- utils::txtProgressBar(min = 0, max = n_data_objects, style = 3)
        message(paste0("\nDownloading data objects to ", download_directory))

        lapply(seq_len(n_data_objects), function(i) {
            # If file_names[i] is NA, name it using the pid
            file_name <- ifelse(is.na(file_names[i]), gsub('[^[:alnum:]]', '_', data_pids[i]), file_names[i])

            if (prefix_file_names == TRUE) {
                file_name <- paste0(filename_prefixes[i], "__", file_names[i])
            }
            out_path <- file.path(download_directory, file_name)

            if (file.exists(out_path)) {
                warning(call. = FALSE,
                        paste0("The file ", out_path, " already exists. Skipping download."))
            } else {
                # Attempt to download object up to 3 times
                n_tries <- 0
                dataObj <- "download_error"

                while (dataObj[1] == "download_error" & n_tries < 3) {
                    dataObj <- tryCatch(dataone::getObject(mn,
                                                           data_pids[i],
                                                           check = check_first),
                                        error = function(e) {return("download_error")})
                    n_tries <- n_tries + 1
                }

                tryCatch(writeBin(dataObj, out_path), error = function(e) {
                    message(paste0("\n Unable to download ", file_name, ". Please download manually or
                               contact the Arctic Data Center for assistance."))
                })

                # Convert excel workbooks to csv
                if (convert_excel_to_csv == TRUE) {
                    if (grepl(".xls", out_path, ignore.case = TRUE)) {
                        excel_to_csv_prefix(out_path, filename_prefixes[i])
                    }
                }
            }

            utils::setTxtProgressBar(progressBar, i)
        })

        close(progressBar)
    }

    return(invisible())
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