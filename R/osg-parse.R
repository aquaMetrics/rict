#' Converts OS Grid Reference to BNG/WGS coordinates.
#'
#' This function is taken from the rnrfa package and licenced under GPL-3.
#' Using this avoids direct dependency on rgdal package.
#'
#'
#' @author Claudia Vitolo
#' @description This function converts an Ordnance Survey (OS) grid reference to
#' easting/northing or latitude/longitude coordinates.
#'
#' @param grid_refs This is a string (or a character vector) that contains the
#' OS grid Reference.
#' @param coord_system By default, this is "BNG" which stands for British
#' National Grids. The other option is to set coord_system = "WGS84", which
#' returns latitude/longitude coordinates (more info can be found here
#' https://www.epsg-registry.org/).
#'
#' @return vector made of two elements: the easting and northing (by default) or
#' latitude and longitude coordinates.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # single entry
#' osg_parse(grid_refs = "TQ722213")
#'
#' # multiple entries
#' osg_parse(grid_refs = c("SN831869", "SN829838"))
#' }
#'
#' # Begin Exclude Linting
osg_parse <- function(grid_refs, coord_system = c("BNG", "WGS84")) {
  gridRefs <- toupper(as.character(grid_refs))
  CoordSystem <- match.arg(coord_system)
  epsg.out <- unname(c(BNG = 27700, WGS84 = 4326)[CoordSystem])
  names.out <- list(
    BNG = c("easting", "northing"),
    WGS84 = c("lon", "lat")
  )[[CoordSystem]]
  letter <- strsplit(substr(gridRefs, 1L, 2L),
    split = "",
    fixed = TRUE
  )
  letter <- do.call(rbind, letter)
  epsg.source <- ifelse(letter[, 1] == "I", 29902, 27700)
  offset1 <- list(S = c(x = 0, y = 0), T = c(5, 0), N = c(
    0,
    5
  ), H = c(0, 10), O = c(5, 5), I = c(0, 0))
  offset1 <- do.call(rbind, offset1)
  offset2 <- list(
    A = c(y = 4, x = 0), B = c(4, 1), C = c(
      4,
      2
    ), D = c(4, 3), E = c(4, 4), F = c(3, 0), G = c(3, 1),
    H = c(3, 2), J = c(3, 3), K = c(3, 4), L = c(2, 0), M = c(
      2,
      1
    ), N = c(2, 2), O = c(2, 3), P = c(2, 4), Q = c(
      1,
      0
    ), R = c(1, 1), S = c(1, 2), T = c(1, 3), U = c(
      1,
      4
    ), V = c(0, 0), W = c(0, 1), X = c(0, 2), Y = c(
      0,
      3
    ), Z = c(0, 4)
  )
  offset2 <- do.call(rbind, offset2)[, c("x", "y")]
  offset <- offset1[letter[, 1], , drop = FALSE] + offset2[letter[
    ,
    2
  ], , drop = FALSE]
  padz <- function(x, n = max(nchar(x))) {
    gsub(
      " ", "0",
      formatC(x, width = -n)
    )
  }
  n <- nchar(gridRefs) - 2
  x <- paste0(offset[, "x"], padz(substr(
    gridRefs, 3,
    (n / 2) + 2
  ), n = 5))
  y <- paste0(offset[, "y"], padz(substr(gridRefs, (n / 2) +
    3, n + 2), n = 5))

  .transform_crs <- function(x, y, from, to) {
    df <- data.frame(
      x = as.numeric(x), y = as.numeric(y), from,
      to
    )
    .transform <- function(x) {
      if (length(unique(x$from)) > 1) {
        stop("Cannot handle multiple source CRS.")
      }
      if (length(unique(x$to)) > 1) {
        stop("Cannot handle multiple target CRS.")
      }
      xy <- x[, c("x", "y")]
      from <- x$from[1]
      to <- x$to[1]
      if (from == to) {
        return(xy)
      }
      sp::coordinates(xy) <- ~ x + y
      sp::proj4string(xy) <- sp::CRS(paste0(
        "+init=epsg:",
        from
      ))
      xy.new <- sp::spTransform(xy, sp::CRS(paste0(
        "+init=epsg:",
        to
      )))
      as.data.frame(sp::coordinates(xy.new))
    }
    grouped <- split(df, f = df[, c("from", "to")])
    unsplit(lapply(grouped, .transform), f = df[, c(
      "from",
      "to"
    )])
  }


  xy <- .transform_crs(
    x = as.numeric(x), y = as.numeric(y),
    from = epsg.source, to = epsg.out
  )
  colnames(xy) <- names.out
  return(as.list(xy))
}
# End Exclude Linting
