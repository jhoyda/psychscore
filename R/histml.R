#' Detailed Histogram 
#'
#' Detailed histogram with mean, median and normal curve.
#' @param x A vector of values for which a histogram is desired
#' @param y 
#' @keywords histogram
#' @export
#' @examples
#' histml(myData)
#' @section Author(s): 
#' Raymond Mar

histml <- function (x, y = "Sturges") {
     mnx <- mean (x)
     mdx <- median (x)
     sdx <- sd (x)
     h<-hist(x, col = "aliceblue", breaks = y, main = " ", xlab = " ")
     title(expression("Mean" * phantom(" and Median")),col.main="darkblue") # 2 colour main title
     title(expression(phantom("Mean and ") * "Median"),col.main="darkred")
     title(expression(phantom("Mean ") * "and " * phantom("Median"),col.main="black"))
     xfit<-seq(min(x),max(x),length=40) # Fit x-axis for normal curve
     yfit<-dnorm(xfit,mean=mnx,sd=sdx)  # Fit y-axis for normal curve
     yfit <- yfit*diff(h$mids[1:2])*length(x) # Fit y-axis for normal curve
     lines(xfit, yfit, col="dodgerblue3", lwd=2) # Draw normal curve
     abline (v = mnx, col = "darkblue", lwd=1) # Draw vertical line for Mean
     abline (v = mdx, col = "darkred", lwd=1) # Draw vertical line for Median
     rug(jitter(x))
}
