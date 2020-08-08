#' Load and clean triad task app tt files
#'
#' @param file path to file
#' @return 
#'
#' @export
loadTT <- function(file){
  hit <- read.table(file, sep = ",", header = TRUE, stringsAsFactors = FALSE) # load hit
  hit <- hit[,2:ncol(hit)]  # remove row numbers
  hit$winner <- ifelse(hit$left.choice == "TRUE", hit$left.source, hit$right.source) # identify winner (i.e. model that was selected)
  return(hit)
}