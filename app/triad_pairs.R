#' Generate word pairs for triad task
#'
#' @param nn1 character vector of nearest neighbors
#' @param nn2 character vector of nearest neighbors
#' @param num_pairs numeric indicating number of pairs to generate
#' @param rm_same logical value indicating whether to remove word pairs with the same word
#' @param rand_order logical value indicating whether to randomize left right order
#' @return a data.frame with word pairs and respective ranks in each model
#' @export
triad_pairs <- function(nn1, nn2, model_names = c("model1", "model2"), num_pairs = 1, rm_same = TRUE, rand_order = TRUE){
  # find all potential pairs
  all_pairs <- expand.grid(nn1, nn2, stringsAsFactors = FALSE) %>% setNames(c("word1", "word2"))
  # remove pairs where both words are the same 
  if(rm_same) all_pairs <- all_pairs %>% filter(!(word1 == word2))
  # randomly sample num_pairs
  word_pairs <- all_pairs %>% sample_n(num_pairs)
  # register rank of each word
  word_pairs <- word_pairs %>% rowwise() %>% mutate(rank1 = which(nn1 == word1), rank2 = which(nn2 == word2), model1 = model_names[1], model2 = model_names[2]) %>% ungroup()
  # place each model in a list
  model_list <- list(word_pairs[,c(1,3,5)], word_pairs[,c(2,4,6)])
  # generate table of order
  if(rand_order){order_tibble <- lapply(1:num_pairs, function(x) sample(c(1,2))) %>% do.call(rbind,.)}else{
    order_tibble <- lapply(1:num_pairs, function(x) c(1,2)) %>% do.call(rbind,.)
  }
  # order pairs
  tt_tibble <- lapply(1:num_pairs, function(i){
    cbind(model_list[[order_tibble[i,1]]][i,], model_list[[order_tibble[i,2]]][i,]) %>% setNames(c("left.word", "left.rank", "left.source", "right.word", "right.rank", "right.source"))
  }) %>% do.call(rbind,.)
  return(tt_tibble)
}