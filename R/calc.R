#' Normalize the area of a compound by the internal standard 
#' @param c1_area numeric
#' @param IS_area numeric
#' @details We here assume the IS concentration is constant across all samples
#' @noRd
normalizeIS <- function(c1_area, IS_area){
  checkmate::assertNumeric(c1_area, finite = TRUE)
  checkmate::assertNumeric(IS_area, finite = TRUE)

  c1_area/IS_area
}

response_factor <- function(peak_area, conc){
  checkmate::assertNumeric(peak_area, finite = TRUE)
  checkmate::assertNumeric(conc, finite = TRUE)
  peak_area/conc
}

RRF <- function(RF1, RF2){
  checkmate::assertNumeric(RF1, finite = TRUE)
  checkmate::assertNumeric(RF2, finite = TRUE)
  RF1/RF2
}



#' Calculate precision (Coefficient of variation)
#' @param x vector
#' @param percent To return the value as percentage
#'
#' @details A simple calculation of the coefficient of variation (CV) is done
#' as the standard deviation divided by the mean. By default, the result is in percentage. 
#' @return numeric
#' @export
#'
precision <- function(x, percent = TRUE){
  p <- sd(x)/mean(x)
  if(percent){
    p <- p * 100
  }
  p
}
