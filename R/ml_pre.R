
#' Prepare data for machine learning training 
#' 
#' @param chrom_res ChromRes object
#' @details The function takes ChromRes object with all peaks must be integrated. 
#' It returns a 2 matrices. One with all the features and the other with the target.
#' The dimensions of the matrices are qxt where q is the number of transitions and t are time points.
#' Each of the rows will have unique label value.
#' @return A list with two matrices. One with features and the other with target.
#' @export
chrom_to_tensor <- function(chrom_res){

    stopifnot(getValidity(chrom_res))

    
}


