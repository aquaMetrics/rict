#' Demo observed values for gis model
#'
#' A dataset containing demo observed gis values. Stored as .csv
#'     demo_gis_values <- utils::read.csv(
#'      system.file("extdat", "environmental-test-data-model-44.csv",
#'              package = "rict"), check.names = F)
#'
#' @format A data frame with 12 rows and 18 variables:
#' \describe{
#'   \item{...}{DOCUMENT modelled varibales here!!!}
#'   \item{SITE}{Site identifier}
#'   \item{WATERBODY}{Water body identifier}
#'   \item{Year}{Year of sampling}
#'   \item{NGR}{National grid letters}
#'   \item{Easting}{Easting co-ordinate}
#'   \item{Northing}{Northing co-ordinate}
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
"demo_gis_values"
