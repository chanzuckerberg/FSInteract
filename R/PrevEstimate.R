PrevEstimate <- function(interactions, z,
                         L=100L, output_list=FALSE) {
    ## check interactions, L,  output_list
  interactions <- as.list(interactions)
  L <- as.integer(L)
  output_list <- as.logical(output_list)

  if (length(interactions)<1L) 
      stop("You must have at least one interaction")
  if (L < 1L)
    stop("L must be >= 1")
  
  is_sparse <- is(z,"Matrix")
  
  
  if (is_sparse) {
    z <- t(z)
    z <- list(z@i, z@p)
  }
  
  if (!is_sparse) {
    if (!is.matrix(z)) stop("z must be a matrix")
    if (nrow(z) == 0) stop("z must have more than 0 rows")
  }

  output <- PrevEstimate_internal(interactions, z, L, is_sparse)
    
  if (!output_list) {
    output<-convert.to.data.frame(output)
  }
  return(output)
}

