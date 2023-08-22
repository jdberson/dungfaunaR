#' Shapefile for linking geographic points to country - county Darwin Core terms
#'
#' Within Australia, the next smaller administrative unit after country is
#' State / Territory, after which comes a 'Local Government Area'. These are
#' equivalent to the Darwin Core terms stateProvince and county respectively.
#' We have included these data within the package to aid with assigning the
#' county, stateProvince, country and countryCode fields to the dung fauna data.
#'
#' The Australian Bureau of Statistics (ABS) provides shapefile data for local
#' government areas in Australia which also includes the associated higher
#' level geographies (State / Territory and Country). More information is here:
#' \url{https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files}.
#'
#' The data downloaded from ABS has been slightly modified with empty geometries
#' removed, terms renamed to Darwin Core terms, and not needed fields removed.
#'
#' @format An \code{sf} MULTIPOLYGON object with 547 features and 4 fields:
#' \describe{
#'   \item{county}{\url{http://rs.tdwg.org/dwc/terms/county} Local government
#'   areas in Australia, nested within stateProvince. The geometry provides the
#'   boundaries of these areas.}
#'   \item{stateProvince}{\url{http://rs.tdwg.org/dwc/terms/stateProvince} The
#'   State or Territory within which the county / local government area is
#'   located.}
#'   \item{country}{\url{http://rs.tdwg.org/dwc/terms/country} All features are
#'   within Australia}
#'   \item{countryCode}{\url{http://rs.tdwg.org/dwc/terms/countryCode} The code
#'   used for Australia}
#'   \item{geometry}{\url{http://rs.tdwg.org/dwc/terms/geometry} Multipolygons
#'   providing the boundaries for county / local government area.}
#' }
#' @source <https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files/LGA_2022_AUST_GDA2020_SHP.zip>
"county_stateProvince_aus"


#' A dataset of deliberately introduced dung beetle records (long format)
#'
#' These data provide counts of deliberately introduced dung beetles in
#' Australia for 23 species and 10,272 sampling events with each row
#' representing the data for one species from one sampling event. Column
#' headings use the Darwin Core terms, except for column headings that include
#' an underscore (_). In the latter case, the underscore has been included to
#' indicate that a non Darwin Core term has been used.
#'
#' The data are fully documented in Berson et al (submitted).
#'
#' @format A data frame with 58 variables and 232,599 rows
"dungfauna_occurrence"



#' A dataset of deliberately introduced dung beetle records (wide format)
#'
#' These data provide counts of deliberately introduced dung beetles in
#' Australia for 10,272 sampling events with each row representing one
#' sampling event. Column headings use the Darwin Core terms, except for columns
#' showing species counts (columns from "Bubas bison" to "Sisyphus spinipes")
#' and column headings that include an underscore (_). In the latter case, the
#' underscore has been included to indicate that a non Darwin Core term has been
#' used. The variables are as documented in Berson et al (submitted) except for
#' the variables "Bubas bison" to "Sisyphus spinipes". See below under
#' **Format**.
#'
#' The values in the columns "Bubas bison" to "Sisyphus spinipes"
#' represent the number of individuals of the species collected by the method
#' described in the column samplingProtocol, except where the value in
#' eventRemarks is "Species counts are visual activity ratings". In the latter
#' case the values range from 0 to 5, with 0 indicating the species was not
#' found, and 5 indicating that the species was found in almost every dung pad
#' searched.
#'
#' @format A data frame with 64 variables and 10,113 rows:
#' \describe{
#'   \item{`Bubas bison`, `Copris elphenor`, `Copris hispanus`,
#'   `Digitonthophagus gazella`, `Euoniticellus africanus`,
#'   `Euoniticellus fulvus`, `Euoniticellus intermedius`,
#'   `Euoniticellus pallipes`, `Geotrupes spiniger`, `Liatongus militaris`,
#'   `Onitis alexis`, `Onitis aygulus`, `Onitis caffer`, `Onitis pecuarius`,
#'   `Onitis vanderkelleni`, `Onitis viridulus`, `Onthophagus binodis`,
#'   `Onthophagus nigriventris`, `Onthophagus obliquus`,
#'   `Onthophagus sagittarius`, `Onthophagus taurus`, `Sisyphus rubrus`,
#'   `Sisyphus spinipes`}{Either the number of individual specimens collected,
#'   or the activity level observed, for the given species during the sampling
#'   event. See **Details**}
#' }
"dungfauna_event"




