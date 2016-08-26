#' Subset reverse scored data frame 
#'
#' This function allows you to reverse score and subset data from a data frame.  
#' It returns a data frame of items with a particular preface (e.g. BFI).
#'
#' The \code{\link{subsetReverse}} function requires the target dataset to 
#' to be formatted in a particular way.  
#'
#' \itemize{
#'   \item All items must be ordered from lowest to highest
#'   \item Any column with the test preface must refer to a scored test item.  
#' }
#' @param data What is the name of data set? Defaults to data.
#' @param varPreface What is the variable preface of the scale? 
#' @param reverseNum Which items are reverse scored?
#' @param maxScore What is the maximum score possible?
#' @param minScore What is the minimum score possible?
#' @param items The number of items in the test
#' @keywords reverse score subset
#' @export
#' @examples
#' subsetReverse(myData, "BFI", reverseNum=c(1,3,5,7), maxScore=5, minScore=1)
#' @section Author(s): 
#' Joseph Hoyda


subsetReverse <- function(data = data, varPreface, reverseNum = c(), maxScore, minScore, items) {

     newDFname <- paste("Reverse", varPreface, sep="")
     colnumbers <- c()
     numcol <- ncol(data)
     # Subset New Data Frame using only prefix scores.  Add "REVERSE" prefix to every col name.
     for (i in c(1:ncol(data))) {
          for (j in c(1:items)) {
               if (colnames(data)[i] == paste(varPreface, j, sep="")) {
                    colnumbers <- append(colnumbers, i)
               }
          }
     }
     NewDF <- data[, colnumbers]
     colnames(NewDF) <- paste("REVERSE", colnames(NewDF), sep="_")
     
     # Reverse score items  
     for (i in 1:ncol(NewDF)) {
          if (i %in% reverseNum)
               NewDF[,i] <- maxScore+minScore - NewDF[,i]
     }
     return(NewDF)
}