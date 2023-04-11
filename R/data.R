#' Shapefile for linking geographic points to country - county Darwin Core terms
#'
#' Within Australia, the next smaller administrative unit after country is
#' State / Territory, after which comes a 'Local Government Area'. These are
#' equivalent to the Darwin Core terms stateProvince and county.
#'
#' The Australian Bureau of Statistics (ABS) provides shapefile data for local
#' government areas in Australia which also includes the associated higher
#' level geographies (State / Territory and Country). More information is here:
#' \url{https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files}.
#'
#' The download link for the raw data is (Local Government Areas - 2022 -
#' Shapefile in GDA2020):
#' https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files/LGA_2022_AUST_GDA2020_SHP.zip
"county_stateProvince_aus"


#' The data downloaded from ABS has been slightly modified with empty geometries
#' removed, terms renamed to Darwin Core terms, and not needed fields removed.
#' @format An \code{sf} POLYGON object with 547 features and 4 fields:
#' \describe{
#'   \item{datasetID}{\url{http://rs.tdwg.org/dwc/terms/datasetID}}
#'   \item{locationID}{\url{http://rs.tdwg.org/dwc/terms/locationID} The
#'   location identifier given in the data set.}
#'   \item{basisOfRecord}{\url{http://rs.tdwg.org/dwc/terms/basisOfRecord} We
#'   have included two values. "PreservedSpecimen" has been used where the
#'   record came from a trap collection. "HumanObservation" has been used where
#'   the record came from the supplementary survey.}
#'   \item{recordedBy}{\url{http://rs.tdwg.org/dwc/iri/recordedBy} All records
#'   were recorded by the Department of Primary Industries and Regional
#'   Development ("DPIRD")}
#'   \item{occurrenceStatus}{\url{http://rs.tdwg.org/dwc/iri/occurrenceStatus}
#'   Either "Present" or "Absent".}
#'   \item{samplingProtocol}{\url{http://rs.tdwg.org/dwc/terms/samplingProtocol}
#'   "CSIRO pitfall trap baited with 1kg dung" describes records from a trap
#'   collection. "Dung pads in paddock searched for dung beetles" describes
#'   records from the supplementary survey.}
#'   \item{scientificName}{\url{http://rs.tdwg.org/dwc/terms/scientificName}
#'   Sixteen levels, including 13 species level identifications (Bubas bison,
#'   Copris hispanus, Coelostoma australe, Euoniticellus fulvus, E. intermedius,
#'   E. pallipes, Heteronychus arator, Onitis alexis, Onitis aygulus,
#'   Onthophagus binodis, O. ferox, O. taurus, O. vermiculatus), two genus level
#'   identifications (Aphodius and Hister) and one subfamily level
#'   identification (Aphodiinae)}
#'   \item{identifiedBy}{\url{http://rs.tdwg.org/dwc/terms/identifiedBy} All
#'   specimens were identified by DPIRD staff or DPIRD contracted consultants}
#'   \item{verbatimEventDate}{
#'   \url{http://rs.tdwg.org/dwc/terms/verbatimEventDate} Date or date-time
#'   recorded by DPIRD. This is the date/date-time that the traps were collected
#'   or the human observation conducted. Note that not all records include the
#'   time. Times shown are in the local time zone (Australia/Perth)}
#'   \item{verbatimLatitude}{\url{http://rs.tdwg.org/dwc/terms/verbatimLatitude}
#'   The Latitude recorded by DPIRD. Note that at all trapping sites two traps
#'   were placed 10-20 m apart. For some locations/records the coordinates of
#'   the individual trap have been recorded, otherwise just one value for both
#'   traps is given.}
#'   \item{verbatimLongitude}{
#'   \url{http://rs.tdwg.org/dwc/terms/verbatimLongitude}
#'   The Longitude recorded by DPIRD.}
#'   \item{individualCount}{\url{http://rs.tdwg.org/dwc/terms/individualCount}
#'   Note that records from the supplementary survey have been assigned a
#'   count value of NA.}
#'    \item{occurrenceRemarks}{
#'    \url{http://rs.tdwg.org/dwc/terms/occurrenceRemarks} All NAs. Included
#'    here to match other data sets.}
#'
#' }
"dpird_2012_2014"
