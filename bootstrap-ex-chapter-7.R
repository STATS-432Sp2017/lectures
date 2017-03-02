my.statistic = function(DF) heteroWLS(DF)$betas
my.simulator = function() resample.data.frame(jobInt)

## borrowed functions
resample <- function(x) { sample(x,size=length(x),replace=TRUE) }
resample.data.frame <- function(data) {
  sample.rows <- resample(1:nrow(data))
  return(data[sample.rows,])
}
rboot <- function(statistic, simulator, B) {
  tboots <- replicate(B, statistic(simulator()))
  if(is.null(dim(tboots))) {
    tboots <- array(tboots, dim=c(1, B))
  }
  return(tboots)
}
bootstrap <- function(tboots, summarizer, ...) {
  summaries <- apply(tboots, 1, summarizer, ...)
  # using apply() like this has interchanged rows and columns
  # because each chunk processed by apply() results in a new column, but
  # here those chunks are the rows of tboots
  # therefore use transpose to restore original orientation
  return(t(summaries))
}
bootstrap.ci <- function(statistic=NULL, simulator=NULL, tboots=NULL,
                         B=if(!is.null(tboots)) { ncol(tboots) },
                         t.hat, level) {
  # draw the bootstrap values, if not already provided
  if (is.null(tboots)) {
    # panic if we're not given an array of simulated values _and_ also lack
    # the means to calculate it for ourselves
    stopifnot(!is.null(statistic))
    stopifnot(!is.null(simulator))
    stopifnot(!is.null(B))
    tboots <- rboot(statistic, simulator, B)
  }
  # easier to work with error probability than confidence level
  alpha <- 1-level
  # Calculate probability intervals for each coordinate
  intervals <- bootstrap(tboots, summarizer=equitails, alpha=alpha)
  upper <- t.hat + (t.hat - intervals[,1])
  lower <- t.hat + (t.hat - intervals[,2])
  # calculate CIs, centered on observed value plus bootstrap fluctuations
  # around it
  CIs <- cbind(lower = lower, upper = upper)
  return(CIs)
}

equitails <- function(x, alpha) {
  lower <- quantile(x, alpha/2)
  upper <- quantile(x, 1-alpha/2)
  return(c(lower, upper))
}

## Get the bootstrap CI
alpha=0.05
level = 1-alpha
B = 50
CI = bootstrap.ci(my.statistic, my.simulator, B=B, level=level)
CI

