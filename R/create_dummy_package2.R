#' Create dummy package
#'
#' Creates a fuller package than \code{\link[arcticdatautils]{create_dummy_package}}
#' but is otherwise based on the same concept. This dummy
#' package includes multiple data objects (2 csv's, and 2 jpg's),
#' responsible parties, geographic locations, method steps, etc.
#'
#' @param mn (MNode) The member node to make the dummy package on
#' @param title (character) Optional. Title of package. Defaults to
#' "A Dummy Package".
#'
#' @import arcticdatautils
#' @import EML
#' @import dataone
#'
#' @export
#'
#' @author Irene Steves

create_dummy_package2 <- function(mn,
                                  title = "A Dummy Package") {

    stopifnot(is(mn, "MNode"))
    stopifnot(is.character(title))

    #upload objects
    file.create(c("dummy1.csv", "dummy2.csv", "dummy1.jpg", "dummy2.jpg"))
    #TODO: add actual data to dummy files

    pid_csv1 <- arcticdatautils::publish_object(mn,
                                                path = "dummy1.csv",
                                                format_id = "text/csv")

    pid_csv2 <- arcticdatautils::publish_object(mn,
                                                path = "dummy2.csv",
                                                format_id = "text/csv")

    pid_jpg1 <- arcticdatautils::publish_object(mn,
                                                path = "dummy1.jpg",
                                                format_id = "image/jpeg")

    pid_jpg2 <- arcticdatautils::publish_object(mn,
                                                path = "dummy2.jpg",
                                                format_id = "image/jpeg")

    data_pids <- c(pid_csv1, pid_csv2, pid_jpg1, pid_jpg2)

    #import EML
    eml_path_original <- file.path(system.file(package = "datamgmt"), "dummy_meta_full.xml")

    eml <- EML::read_eml(eml_path_original)
    eml_path <- tempfile(fileext = ".xml") #for saving later

    #add data tables + other entities to eml
    eml@dataset@title[[1]]@.Data <- title

    attr <-  data.frame(

        attributeName = c('Date', 'Location', 'Salinity', 'Temperature'),
        attributeDefinition = c('Date sample was taken on', 'Location code representing location where sample was taken', 'Salinity of sample in PSU', 'Temperature of sample'),
        measurementScale = c('dateTime', 'nominal','ratio', 'interval'),
        domain = c('dateTimeDomain', 'enumeratedDomain','numericDomain', 'numericDomain'),
        formatString = c('MM-DD-YYYY', NA,NA,NA),
        definition = c(NA,NA, NA, NA),
        unit = c(NA, NA, 'dimensionless', 'celsius'),
        numberType = c(NA, NA, 'real', 'real'),
        missingValueCode = c(NA, NA, NA, NA),
        missingValueCodeExplanation = c(NA, NA, NA, NA),

        stringsAsFactors = FALSE)

    Location <- c(CASC = 'Cascade Lake', CHIK = 'Chikumunik Lake', HEAR = 'Heart Lake', NISH = 'Nishlik Lake' )

    fact <- data.frame(attributeName = 'Location', code = names(Location), definition = unname(Location))

    dt1 <- arcticdatautils::pid_to_eml_datatable(mn,
                                                 attributes = attr,
                                                 factors = fact,
                                                 pid = pid_csv1)
    dt2 <- arcticdatautils::pid_to_eml_datatable(mn,
                                                 attributes = attr,
                                                 factors = fact,
                                                 pid = pid_csv2)
    eml@dataset@dataTable <- c(dt1, dt2)

    oe <- arcticdatautils::pid_to_eml_other_entity(mn,
                                                   pids = c(pid_jpg1,
                                                            pid_jpg2))
    eml@dataset@otherEntity@.Data <- oe

    EML::write_eml(eml, eml_path)

    pid_eml <- arcticdatautils::publish_object(mn,
                                               path = eml_path,
                                               format_id = "eml://ecoinformatics.org/eml-2.1.1")

    #resource map together
    resource_map_pid <- arcticdatautils::create_resource_map(
        mn,
        metadata_pid = pid_eml,
        data_pids = data_pids)

    file.remove(c("dummy1.csv", "dummy2.csv", "dummy1.jpg", "dummy2.jpg",
                  eml_path))

    return(list(resource_map = resource_map_pid,
                metadata = pid_eml,
                data = data_pids))
}
