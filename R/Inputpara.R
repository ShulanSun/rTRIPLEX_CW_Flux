#' Inputpara
#'
#' A dataset containing the list of input parameters and their description.
#'
#' @format
#' A data frame with 1 rows and 31 variables:
#' \describe{
#'   \item{HV1}{the height at wind measurement, m}
#'   \item{hc}{the average canopy height, m}
#'   \item{N}{leaf nitrogen content, percent}
#'   \item{Nm}{maximum nitrogen content, percent}
#'   \item{m}{coefficient}
#'   \item{g0}{initial stomatal conductance, m mol m-2 s-1}
#'   \item{Vm25}{maximum carboxylation rate at 25 degrees Celsius, umol m-2 s-1}
#'   \item{Rgas}{molar gas constant, m3 Pa mol-1 K-1}
#'   \item{O2}{oxygen concentration in the atmosphere, Pa}
#'   \item{Ls}{standard longitude of time zone}
#'   \item{Le}{local longitude, degree}
#'   \item{latitude}{local latitude, degree}
#'   \item{LAI}{leaf area index of canopy, m2 m-2}
#'   \item{SWCs}{saturated soil volumetric moisture content at depth of 30 cm, percent}
#'   \item{SWCw}{wilting soil volumetric moisture content at depth of 30 cm, percent}
#'   \item{VPD_close}{the VPD at stomatal closure, kPa}
#'   \item{VPD_open}{the VPD at stomatal opening, kPa}
#'   \item{Mf}{biomass density of for leaf, kg C m-2 day-1}
#'   \item{Ms}{biomass density of for sapwood, kg C m-2 day-1}
#'   \item{Mr}{biomass density of for root, kg C m-2 day-1}
#'   \item{rmf}{maintenance respiration coefficient for leaf}
#'   \item{rms}{maintenance respiration coefficient for stem}
#'   \item{rmr}{maintenance respiration coefficient for root}
#'   \item{rgf}{growth respiration coefficient for leaf}
#'   \item{rgs}{growth respiration coefficient for sapwood}
#'   \item{rgr}{growth respiration coefficient for root}
#'   \item{raf}{carbon allocation fraction for leaf}
#'   \item{ras}{carbon allocation fraction for sapwood}
#'   \item{rar}{carbon allocation fraction for root}
#'   \item{Q10}{temperature sensitivity factor}
#'   \item{Tref}{base temperature for Q10, degrees Celsius}
#' }
#' @examples
#' head(Input_parameter)
