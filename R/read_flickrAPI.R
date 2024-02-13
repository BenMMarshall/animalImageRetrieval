#' Read in api key for flickr
#'
#' @name read_flickrAPI
#' @description A
#' @param abc abc
#' @return a
#'
#' @export
read_flickrAPI <- function(){
  apiKey <- scan(here::here("notebook", "flickrAPIkey.txt"), character())
  return(apiKey)
}
