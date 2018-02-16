context("Clone a package")

cn <- dataone::CNode('STAGING')
mn <- dataone::getMNode(cn,'urn:node:mnTestARCTIC')

test_that("Data pids in an EML are extracted correctly", {
    if (!arcticdatautils::is_token_set(mn)) {
        skip("No token set. Skipping test.")
    }

    package <- arcticdatautils::create_dummy_package(mn)
    eml <- EML::read_eml(rawToChar(dataone::getObject(mn, package$metadata)))

    # create_dummy_package uses a dummy pid in 'example-eml.xml' - update pid
    otherEnt <- arcticdatautils::pid_to_eml_other_entity(mn, package$data)
    eml@dataset@otherEntity <- new("ListOfotherEntity", .Data = otherEnt)

    file_path <- file.path(tempdir(), "science_metadata.xml")
    EML::write_eml(eml, file_path)
    package <- arcticdatautils::publish_update(mn,
                                               package$metadata,
                                               package$resource_map,
                                               package$data,
                                               metadata_path = file_path)

    eml <- EML::read_eml(rawToChar(dataone::getObject(mn, package$metadata)))
    data_pid <- get_eml_pids(eml)

    expect_equivalent(data_pid, package$data)
})

