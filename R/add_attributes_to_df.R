##
eml <- read_eml(rawToChar(getObject(mnReal, "doi:10.18739/A2F299")))
data <- read.csv("https://arcticdata.io/metacat/d1/mn/v2/object/urn%3Auuid%3A11986ca1-5560-48ac-b8f9-0469ce561946", header = TRUE)
eml@dataset@dataTable[[3]]
attributes <- get_attributes(eml@dataset@dataTable[[3]]@attributeList)[[1]]

## Add attributes to a data frame
add_attributes <- function(data, attributes) {
    # TODO add $factors case from 'get_attributes'
    stopifnot(is.data.frame(data))
    stopifnot(is.data.frame(attributes))
    stopifnot(ncol(data) == nrow(attributes))

    n_attributes <- nrow(attributes)
    metadata_names <- colnames(attributes)

    for (i in seq_len(n_attributes)) {
        metadata <- c(metadata_names[i] = attributes)
    }

    meta <- list()
    for (i in 1:13) {
        # define variable
        var <- assign(metadata_names[i], attributes[1, i])
        meta <- c(meta, list(meta1))
    }
    names(meta) = metadata_names

    list1 <- list(meta, meta)
    names(list1) <- c("name1", "name2")

}

# data <- data.frame("depth" = c(1,2), "temperature" = c(30, 31))
# attributes(data) <- list("depth" = list("attributeName" = "depth", "unit" = "meter"),
#                          "temperature" = list("attributeName" = "temperature", "unit" = "celsius"))
# attributes(data)$depth
# attributes(data) <- list1
