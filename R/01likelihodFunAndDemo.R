
# to optimise
# ymax - max count calue
# counts - observed counts including NAs
pLikCens <- function(x, ymax, counts){
  
  l <- x[1]
  numNa <- sum(is.na(counts))
  noNa <- counts[!is.na(counts)]
  -sum( # the negative log likelihood
    dpois(noNa, l, log = T)) -
    ppois(q=ymax, lambda=l, lower.tail = F, log.p = T)*numNa
}
ymax <- 7
yVals <- rpois(100, 8)
yValsNa <- yVals
yValsNa[yValsNa > ymax] <- NA
pLikCens(7, 7, yValsNa)
plot(1:25, sapply(1:25, function(x) pLikCens(x, 7, yValsNa)),
     log="y")
grid()
