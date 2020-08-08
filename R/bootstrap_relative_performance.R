#' Bootstrap the performance statistic as explained in Rodriguez & Spirling 2020
#'
#' @param tt_data processed triad task app output
#' @param x the cue for which performance statistic is to be bootstrapped
#' @param overlap_prob numeric between 0-1, output of `compute_overlap_prob` for the corresponding models
#' @param num_iters number of bootstraps
#' @param seed seed for replication purposes
#' @return 
#'
#' @export
bootstrap_relative_performance <- function(tt_tibble, x = NULL, models = NULL, overlap_prob = NULL, num_iters = 100, seed = 1984L){
  set.seed(seed)
  tt_data <- tt_tibble %>% filter(left.source %in% models & right.source %in% models) %>% filter(cue == x)
  tt_data <- replicate(num_iters, sample_n(tt_data, size = nrow(tt_data), replace = TRUE), simplify = FALSE) 
  relative_performance <- tt_data %>% map(compute_relative_performance, models = models, overlap_prob = overlap_prob) %>% unlist()
  out <- tibble(cue = x, mu = mean(relative_performance), std.error = sd(relative_performance))
  return(out)
}