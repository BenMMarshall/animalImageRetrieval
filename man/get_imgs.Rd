% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_imgs.R
\name{get_imgs}
\alias{get_imgs}
\title{Download the images and save in source folder}
\usage{
get_imgs(urlData, source, flickrAPI)
}
\arguments{
\item{urlData}{The urlData that is one of the dataframes from teh get_url functions}

\item{source}{The source, each requires a slightly different approach and need to be saved in another location.}
}
\value{
Nothing in R, saves images to directory.
}
\description{
A wrapper to get the images from the other functions.
}
