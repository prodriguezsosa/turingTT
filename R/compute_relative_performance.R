#' Compute the performance statistic as explained in Rodriguez & Spirling 2020
#'
#' @param tt_data processed triad task app output
#' @param nns2 character vector specifying pair of models to compare (must match labels used in tt_data)
#' @param overlap_prob numeric between 0-1, output of `compute_overlap_prob` for the corresponding models
#' @return 
#'
#' @export
compute_relative_performance <- function(tt_data, models, overlap_prob = NULL){
  group_tallies <- table(tt_data$winner) # count the number of times each model was selected
  if(!is.null(overlap_prob)){
    non_overlap_stat <- (group_tallies[models[1]]/sum(group_tallies)) # models[2] is baseline
    ajusted_stat <- overlap_prob*0.5 + (1 - overlap_prob)*non_overlap_stat
    out <- unname(ajusted_stat/0.5) # divide by 0.5 such that 1 = indifference
  }else{
    out <- unname((group_tallies[models[1]]/sum(group_tallies))/0.5)
  }
  return(out)
}