#' Interactive Reverse Scoring Function
#'
#' An interactive function for quickly performing statistical analyses on a variety of subscales.  It is designed for researchers who need to document their analysis in a script file that can be easily shared. 
#' @keywords score subscales 
#' @export
#' @examples
#' revScore()
#' @section Author(s): 
#' Joseph Hoyda

revScore <- function()
{
     # Ask some questions
     dataSetName <- readline(prompt="Welcome to the reverse scoring function. What is the name of your dataset? ")
     preface <- readline(prompt="What is the variable preface for this scale? (e.g., BFI, BFAS): ")
     num <- readline(prompt="How many items are there?  (e.g., 44): ")
     reverse <- readline(prompt="Which items are reverse-coded?  Please separate each item with a comma. (e.g., 2, 7, 3, etc.): ")
     minScore <- readline(prompt="What is the minimum score in this test? ")
     maxScore <- readline(prompt="What is the maximum score in this test? ")
     totalScaleScores <- readline(prompt="Do you want total scale scores?  If so, please type T, otherwise type F: ")

     # CODE: Subset new data frame with reverse scores
     subsetReverseName <- paste("Reverse", preface, sep="")
     subsetRevString <- paste(subsetReverseName, " <- ", 
                              "subsetReverse(", "data=", dataSetName, ", varPreface=", 
                              "\"", preface, "\", ", 
                              "reverseNum= c(", toString(reverse), "),", 
                              "maxScore=", toString(maxScore), ", ",
                              "minScore=", toString(minScore), ")",
                              sep="")

     # FUNCTION: Create Keys for subscales
     subscaleNum <- readline(prompt="How many subscales are there?  ")
     if (is.na(subscaleNum)) {
          subscaleNum = 0
     }
     subKeyList <- list()
     keyListName <- paste(preface, "KeyList", sep="")
     keyString <- paste(keyListName, " <- list(", sep="")
     for (i in 1:as.numeric(subscaleNum)) {
          if (toString(subscaleNum) == 0) {
               break
          } 
          subKeyName <- readline(prompt=paste("What is the name of subscale ", toString(i), "? ", sep=""))
          subKeyItem <- readline(prompt=paste("Which items are associated with ", subKeyName, "? ", sep=""))
          tempKey <- as.numeric(unlist(strsplit(subKeyItem, ",")))
          subKeyList[[i]] <- tempKey
          names(subKeyList)[i] <- subKeyName
          if (i < as.numeric(subscaleNum)) {
               keyString <- paste(keyString, subKeyName, "=", 
                                  "c(", toString(subKeyItem), "), ",
                                  sep="")
          } else if (i == as.numeric(subscaleNum)) {
               keyString <- paste(keyString, subKeyName, "=", 
                                  "c(", toString(subKeyItem), ")",
                                  sep="")               
          }
     }
     if (totalScaleScores == "T" | totalScaleScores == "t") {
          keyString <- paste(keyString, ", all=c(1:", num, ")", sep="")
     }
     
     keyString <- paste(keyString, ")", sep="")
     keyName <- paste(preface, ".key", sep="")

     ## Subset new data frame
     cat("# Subset new data frame with reverse scores \n")
     cat(subsetRevString, "\n")     
          
     # CODE: Create key list
     cat("# Create key list \n")
     cat(keyString, "\n")
     cat("# Create Keys", "\n")
     cat(keyName, " <- ", "make.keys(nvars = ", toString(num), ", ",
         keyListName, ")", "\n", sep="")
     
     # CODE: Score the items
     cat("## Score the items", "\n")
     cat("my.scales.", preface, " <- scoreItems(", keyName, ", ", 
         subsetReverseName, "[,1:", toString(num), "])", "\n \n", sep="")

     # CODE: Analyze that data!
     if (totalScaleScores=="T" || totalScaleScores=="t") {
          cat("## Descriptives on everything: \n")
          cat("describe(my.scales.", preface, "$scores) # describe everything", "\n", sep="")
          cat("histml(my.scales.", preface, "$scores)", "\n \n", sep="")
          
          cat("## Cronbach's Alpha for all scales: \n")
          cat("my.scales.", preface, "$alpha \n", sep="")
          cat("alpha(", subsetReverseName, ") \n \n", sep="")
     }

     for (i in 1:subscaleNum) {
          if (toString(subscaleNum) == 0) {
               break
          } 
          cat("## Describe: ", names(subKeyList)[i], "\n", sep="")
          cat("describe(my.scales.", preface, "$scores[,", toString(i), "]) \n", sep="")      
          cat("histml(my.scales.", preface, "$scores[,", toString(i), "]) \n", sep="")
          cat("## Check Cronbach's Alpha ", names(subKeyList[i]), "\n", sep="")
          cat("my.scales.", preface, "$alpha[,", toString(i), "] \n", sep="")
          cat("alpha(", subsetReverseName, "[,as.numeric(unlist(", keyListName, "[", i, "]))]) \n", sep="")
          cat("## Add scores to data set \n")
          cat(subsetReverseName, "$", preface, "_", names(subKeyList[i]), " <- rowMeans(",
              subsetReverseName, "[,c(", toString(subKeyList[[i]]), ")]) \n \n", sep="")
     }

     if (totalScaleScores=="T" || totalScaleScores=="t") {
          cat("## Additional descriptive information \n")
          cat("my.scales.", preface, "\n \n", sep="")
     }

     cat("# Bind new data frame to master dataset \n")
     cat(dataSetName, " <- cbind(", dataSetName, ", ", subsetReverseName, ")", "\n \n", sep="")
}    

