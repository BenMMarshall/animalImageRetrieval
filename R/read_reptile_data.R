#' Read in reptile database data
#'
#' @name read_reptile_data
#' @description Simple filter wrapper for reading in the provided reptiledatabase data
#' @param orderFilter character string to filter by order, must be present in Order column
#' @param familyFilter character string to filter by family, must be present in Family column
#' @param speciesFilter character string to filter by species, must be present in Species column
#' @return a
#'
#' @export
read_reptile_data <- function(orderFilter = NULL, familyFilter = NULL, speciesFilter = NULL){
  repdb <- read.csv(here::here("data", "reptile_checklist_2023_09.csv"))
  if(!is.null(orderFilter)){
    repdb <- repdb[repdb$Order == orderFilter,]
  }
  if(!is.null(familyFilter)){
    repdb <- repdb[repdb$Family == familyFilter,]
  }
  if(!is.null(speciesFilter)){
    repdb <- repdb[repdb$Species == speciesFilter,]
  }
  return(repdb)
}
