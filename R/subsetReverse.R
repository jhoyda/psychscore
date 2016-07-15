#' Subset Reverse Scored Data Frame 
#'
#' This function allows you to reverse score and subset data from a data frame.  It returns a data frame of items with a particular preface (e.g. BFI)
#' @param data What is the name of data set? Defaults to data.
#' @param varPreface What is the variable preface of the scale? 
#' @param reverseNum Which items are reverse scored?
#' @param maxScore What is the maximum score possible?
#' @param minScore What is the minimum score possible?
#' @keywords 
#' @export
#' @examples
#' subsetReverse(myData, "BFI", reverseNum=c(1,3,5,7), maxScore=5, minScore=1)



subsetReverse <- function(data = data, varPreface, reverseNum = c(), maxScore, minScore) {
     newDFname <- paste("Reverse", varPreface, sep="")
     # Subset New Data Frame using only prefix scores.  Add "REVERSE" prefix to every col name.
     NewDF <- data[, grepl(varPreface, names(data))]
     colnames(NewDF) <- paste("REVERSE", colnames(NewDF), sep="_")
     
     # Reverse score items  
     for (i in 1:ncol(NewDF)) {
          if (i %in% reverseNum)
               NewDF[,i] <- maxScore+minScore - NewDF[,i]
     }
     return(NewDF)
}