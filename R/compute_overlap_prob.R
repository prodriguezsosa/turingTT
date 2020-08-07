#' Compute the probability that the same word is drawn by chance given two lists of nearest neighbors
#'
#' @param nns1 character vector of nearest neighbors 1
#' @param nns2 character vector of nearest neighbors 2
#' @return the probability (numeric) that the same word is drawn by chance
#'
#' @export
compute_overlap_prob <- function(nns1, nns2){
  prob <- length(intersect(nns1, nns2))/length(nns1) # = p = probability of selecting a token that is in both lists
  overlap_prob <- prob^2 # probability of selecting the same token from each list = p*p
  return(overlap_prob)
}
