#' Read hierachy from csv files
#' 
#' This function reads a list of csv files and builds a hierachy from them to be used in the ChameleonK package.
#' The function reads the csv files from a directory where for each quasi-identifier a csv file is stored, that contains the hierachy of the quasi-identifier.
#' The structure of the csv file is expected to have in the first column the original values of the quasi-identifier attribute.
#' The second column contains the generalization of the original value, the third column the generalization of the second column and so on.
#' Skips are allowed and represented by an empty cell in the csv file. This means that two cells can be merged also on the next next level of generalization.
#' 
#' @param path A character string representing the path to a directory containing csv files.
#' @param seperator A character string representing the seperator used in the csv files. 
#' @export 


read_hierachy_from_csv <- function(path, seperator=";"){
  if(is.character(path)){
    filenames <- list.files(path, pattern="*.csv", full.names=TRUE)
    ldf <- lapply(filenames, read.csv, sep=seperator, colClasses="character")
    hierachy <- lapply(ldf, built_hierachy_from_dataframe)
    names(hierachy) <- lapply(filenames, function(x) stringr::str_extract(x,stringr::regex("[:alpha:]*(?=.csv)")))
    return(hierachy)
  }else{
    stop("invalid path")
  }
}
