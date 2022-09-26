#' Demo observed values for gis model
#'
#' A dataset containing demo observed log transformed gis values for model 44.
#' Stored as .csv:
demo_gis_values_log <- utils::read.csv(system.file("extdat",
  "environmental-test-data-model-44-log.csv",
  package = "rict"
),
check.names =
  FALSE
)
#'
#' @format A data frame with 12 rows and 18 variables:
#' \describe{
#'   \item{SITE}{Site identifier}
#'   \item{Year}{Year of sampling}
#'   \item{WATERBODY}{Water body identifier}
#'   \item{Alkalinity}{Alkalinity}
#'   \item{Conductivity}{Conductivity}
#'   \item{hardness}{hardness}
#'   \item{calcium}{calcium}
#'   \item{OBJECTID}{Optional object ID reference from map-based locator}
#'   \item{SX}{Sampling site X coordinate (British National Grid)}
#'   \item{SY}{Sampling site Y coordinate (British National Grid)}
#'   \item{EX}{Optionally for reference, the nearest X grid coordinate used for predictive variables}
#'   \item{EY}{Optionally for reference, the nearest Y grid coordinate used for predictive variables}
#'   \item{shape_Length}{shape_Length}
#'   \item{Altitude}{Altitude}
#'   \item{d_f_source}{d_f_source}
#'   \item{logaltbar}{logaltbar}
#'   \item{log_area}{log_area}
#'   \item{chalk}{chalk}
#'   \item{clay}{clay}
#'   \item{hardrock}{hardrock}
#'   \item{limestone}{limestone}
#'   \item{peat}{peat}
#'   \item{disch_cat}{Discharge category}
#'   \item{slope}{slope}
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
"demo_gis_values_log"
