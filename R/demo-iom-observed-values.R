#' Demo observed values for physical model for Isle of Man
#'
#' A dataset containing demo observed values. Stored as .csv
#'     demo_observed_values <- utils::read.csv(
#'      system.file("extdat", "input-file-data-to-use-multi-year-iom.csv",
#'              package = "rict"), check.names = F)
#'
#'
#' @format A data frame with 24 rows and 33 variables:
#' \describe{
#'   \item{SITE}{Site identifier}
#'   \item{Waterbody}{Water body identifier}
#'   \item{Year}{Year of sampling}
#'   \item{NGR}{National grid letters}
#'   \item{Easting}{Easting co-ordinate}
#'   \item{Northing}{Northing co-ordinate}
#'   \item{Altitude}{Altitude in metres}
#'   \item{Slope}{Slope in metres per km}
#'   \item{Dist_from_Source}{Distance from Source in km}
#'   \item{Alkalinity}{Hardness, Calcium concentration or Electrical
#'   conductivity can be provided if unable to provide Alkalinity – One of the
#'   values must be present (spreadsheet validation)}
#'   \item{Hardness}{Can be provided instead of Alkalinity and follows the same
#'   validation rules (spreadsheet validation)} \item{Calcium}{Can be provided
#'   instead of Alkalinity and follows the same validation rules (spreadsheet
#'   validation)} \item{Conductivity}{Can be provided instead of Alkalinity and
#'   follows the same validation rules (spreadsheet validation)}
#'   \item{Spr_Season_ID}{season ID}
#'   \item{\code{Spr_TL2_WHPT_ASPT (AbW,DistFam)}}{ASPT}
#'   \item{\code{Spr_TL2_WHPT_NTaxa (AbW,DistFam)}}{NTAXA}
#'   \item{Spr_Ntaxa_Bias}{bias}
#'   \item{Sum_Season_ID}{season ID}
#'   \item{\code{Sum_TL2_WHPT_ASPT (AbW,DistFam)}}{ASPT}
#'   \item{\code{Sum_TL2_WHPT_NTaxa (AbW,DistFam)}}{NTAXA}
#'   \item{Sum_Ntaxa_Bias}{bias}
#'   \item{Aut_Season_ID}{season ID}
#'   \item{\code{Aut_TL2_WHPT_ASPT (AbW,DistFam)}}{ASPT}
#'   \item{\code{Aut_TL2_WHPT_NTaxa (AbW,DistFam)}}{NTAXA}
#'   \item{Aut_Ntaxa_Bias}{bias}
#' }
"demo_iom_observed_values"
