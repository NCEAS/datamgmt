context("Categorize dataset")

# run this to get token first for tests to run:
#googlesheets4::gs4_auth(use_oob = T)

#for travis ci
testthat::skip_if_not(googlesheets4::gs4_has_token(), "No Google Sheet token")


test_that("spelling is correct", {

    expect_error(categorize_dataset("doi:10.18739/A2GH9B946", c("biolog", "oceanogrophy"), "test spelling", T))

    expect_error(categorize_dataset("not a pid", c("biology", "oceanography"), "test pid", T))

    expect_error(categorize_dataset("not a pid", c("biology", "oceanography", "soil science", "ecology", "biology", "oceanography"), "test pid", T))
})

test_that("works properly", {
    expect_message(categorize_dataset("doi:10.18739/A2C24QN9B", c("archaeology", "geology/geophysics"), "working", T), "categorizing dataset")
de})

test_that("doi or versions are already in sheet", {

    #same doi already in the sheet
    expect_error(categorize_dataset("doi:10.18739/A2CK5B", c("biology", "oceanography"), "test pid", T),
                 "Dataset with identifier doi:10.18739/A2CK5B is already categorized - identifier not added. Set overwrite to TRUE to update.")
    #same doi but different themes
    expect_error(categorize_dataset("doi:10.18739/A2CK5B", c("biology", "soil science"), "test pid", T),
                 "Dataset with identifier doi:10.18739/A2CK5B is already categorized - identifier not added. Set overwrite to TRUE to update.")
    #previous version in sheet
    expect_warning(categorize_dataset("doi:10.18739/A2GH9B946" , c("biology", "soil science"), "test pid", T),
                   "identifiers or previous versions already in sheet, updating identifier") #doi:10.18739/A2QJ77Z09

})

test_that("overwrite", {
    #previous version in sheet
    expect_warning(categorize_dataset("doi:10.18739/A2QJ77Z09" , c("biology"), "test pid", T, T),
                   "overwriting themes - identifiers or previous versions already in sheet, updating identifier")
    #update themes
    expect_warning(categorize_dataset("doi:10.18739/A2125Q94Q", c("biology", "soil science"), "test pid", T, T),
                   "overwriting themes")
    #verifying that if you set overwrite to T works based on previous error
    expect_warning(categorize_dataset("doi:10.18739/A2CK5B", c("biology", "soil science"), "test pid", T, T),
                 "overwriting themes")

})

#reset sheet after finished
example <- data.frame(
    stringsAsFactors = FALSE,
    url = c("http://arcticdata.io/catalog/view/doi:10.18739/A2542J85G",
            "http://arcticdata.io/catalog/view/doi:10.18739/A2W66976B",
            "http://arcticdata.io/catalog/view/doi:10.18739/A2PG1HP17",
            "http://arcticdata.io/catalog/view/doi:10.18739/A2125Q94Q",
            "http://arcticdata.io/catalog/view/doi:10.18739/A2VK5P",
            "http://arcticdata.io/catalog/view/doi:10.18739/A2GH9B946"),
    id = c("doi:10.18739/A2542J85G",
           "doi:10.18739/A2W66976B","doi:10.18739/A2PG1HP17",
           "doi:10.18739/A2125Q94Q","doi:10.18739/A2VK5P",
           "doi:10.18739/A2QJ77Z09"),
    dateUploaded = c("2020-05-15 19:51:23",
                     "2018-05-15 21:22:24","2020-05-19 20:31:44","OVERWRITE THEMES",
                     "THE ALREADY CATEGORIZED","OLD PID"),
    abstract = c("The files contain mitochondrial gene sequence data for individual zooplankton (Calanus spp. And Thysanoessa spp.) collected near Barrow, AK. The files are Fasta files that can be opened in most bioinformatics software programs (e.g. Geneious, MegAlign, MEGA) or text/word processing programs. For each sequence, the file contains the species identification, isolate number, station number, collection location (Lat, Lon), collection date, the sequenced gene followed by the sequence.",
                 "The files contain mitochondrial gene sequence data for individual zooplankton (Calanus spp. And Thysanoessa spp.) collected near Barrow, AK. The files are Fasta files that can be opened in most bioinformatics software programs (e.g. Geneious, MegAlign, MEGA) or text/word processing programs. For each sequence, the file contains the species identification, isolate number, station number, collection location (Lat, Lon), collection date, the sequenced gene followed by the sequence.",
                 "The files contain mitochondrial gene sequence data for individual zooplankton (Calanus spp. And Thysanoessa spp.) collected near Barrow, AK. The files are Fasta files that can be opened in most bioinformatics software programs (e.g. Geneious, MegAlign, MEGA) or text/word processing programs. For each sequence, the file contains the species identification, isolate number, station number, collection location (Lat, Lon), collection date, the sequenced gene followed by the sequence.",
                 "Data represents analyzed zooplankton collections from the Chukchi Sea during July and August of 1976. Samples were collected with a Juday Net with a pore opening of 168 um. Species distribution, species abundances  and species biomass (mg/m3) are tabulated in the worksheet. Data are recovered from E.A. Pavstiks. 1984. Zooplankton of the Chukchi Sea as indices of water origins Trudy Arkticheskogo i Antarkticheskogo Nauchno-Issledovatel&amp;amp;",
                 "Zooplankton collected by different expeditions in the East Siberian Sea - August to September 1948 and 1973 are analyzed for species composition, abundance and biomass. Samples were collected with a Juday net using a No 38 silk mesh (168 um) and units are recorded in abundance and biomass (mg/m3). This data was recovered from Pavshtiks, 1994 Composition and Quantitative Distribution of the Zooplankton in the East Siberian Sea. Russian paper with English abstract",
                 "Seasonal water table depth measurements were collected during the 2018 growing seasons at each Mobile Instrumented System Platform (MISP) located at Utqiaġvik (formerly Barrow), Alaska as well as in Atqasuk, Alaska.  If a uniform representation of the water table within each plot was visible, then a WTD (water table depth) tube was placed there to measure the change in water table throughout the season. One water table depth measurement was acquired, the date, start and end times, and the people present were also recorded, at alternating grid meters. Water table depth was taken on the north side of the grid at each WTD tube in order to reduce disturbance of vegetation.  These measurements are part of the Arctic Observing Network (AON) - International Tundra Experiment (ITEX) initiative and help to document seasonal changes in surface moisture and structure at high spatial scales."),
    keywords = c("EARTH SCIENCE > BIOSPHERE > AQUATIC ECOSYSTEMS > PLANKTON > ZOOPLANKTON SHIP STATION 10 KILOMETERS TO 50 KILOMETERS DAILY TO WEEKLY oceans",
                 "EARTH SCIENCE > BIOSPHERE > AQUATIC ECOSYSTEMS > PLANKTON > ZOOPLANKTON SHIP STATION 10 KILOMETERS TO 50 KILOMETERS DAILY TO WEEKLY oceans",NA,
                 "EARTH SCIENCE > OCEANS EARTH SCIENCE > BIOLOGICAL CLASSIFICATION > ANIMALS/INVERTEBRATES EARTH SCIENCE > BIOSPHERE > AQUATIC ECOSYSTEMS > PLANKTON IN SITU/LABORATORY INSTRUMENTS > SAMPLERS > TRAWLS/NETS > BONGO NETS SHIP STATION UNKNOWN biota oceans",
                 "EARTH SCIENCE > OCEANS EARTH SCIENCE > BIOSPHERE > AQUATIC ECOSYSTEMS > PLANKTON > ZOOPLANKTON IN SITU/LABORATORY INSTRUMENTS > SAMPLERS > TRAWLS/NETS > BONGO NETS SHIP TRANSECT UNKNOWN biota oceans",
                 "EARTH SCIENCE > LAND SURFACE > SOILS > SOIL MOISTURE/WATER CONTENT TRANSECT 30 METERS TO 100 METERS IN SITU/LABORATORY INSTRUMENTS > PROBES > PROBES FIELD SURVEY MONTHLY TO ANNUAL environment"),
    title = c("Zooplankton Genetics 2013",
              "Zooplankton Genetics 2014","Zooplankton Genetics 2015",
              "Zooplankton measurements, Chukchi Sea, 1976",
              "Zooplankton_East_Siberian_Sea_1948_and_1973_Pavshtiks",
              "Water Table Depth (WTD) MISP (Mobile Instrumented System Platform) grid in Barrow and Atqasuk, Alaska, 2018"),
    theme1 = c("biology","biology","biology",
               "biology","biology","biology"),
    theme2 = c("oceanography","oceanography",
               "oceanography","oceanography","soil science",
               "soil science"),
    theme3 = c(NA, NA, NA, NA, NA, NA),
    theme4 = c(NA, NA, NA, NA, NA, NA),
    theme5 = c(NA, NA, NA, NA, NA, NA),
    coder = c("Erin", "Erin", "Erin", "Erin", "test pid", "test pid")
)

#write to googlesheet
ss <- "https://docs.google.com/spreadsheets/d/1GEj9THJdh22KCe1RywewbruUiiVkfZkFH8FO1ib9ysM/edit#gid=1479370118"
googlesheets4::range_write(data = example,
                           ss = ss)
googlesheets4::range_delete(ss, sheet = "dataset_categorization", range = "8", shift = "up")
