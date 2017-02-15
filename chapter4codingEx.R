### Starter code for Homework 3, problem 3 ###

#Our true mean function: will be sin(x/2) on [0,4*pi] and sin(6*x) on [4*pi,8*pi]
mu <- function(x){
  #Initialize a vector of zeros
  y = numeric(length(x))
  #Figure out which points are to the left or right of 4*pi
  leftPoints = (x<=4*pi)
  rightPoints = (x>4*pi)
  #Assign the appropriate sine values
  y[leftPoints] = sin(x[leftPoints]/2)
  y[rightPoints] = sin(6*x[rightPoints])
  #Return y
  y
}

#A function to draw a sample from this curve
generateSample <- function(n){
  #Sample the x coordinates uniformly on [0,8*pi].
  #We sort the x here to make plotting easier later.
  x = sort(runif(n,0,6*pi))
  #Sample the y coordinates as gaussians around mu(x)
  y = mu(x) + rnorm(n,0,.2)
  #Bind this all together into a data frame
  data.frame(x=x,y=y)  
}

#We set the seed so that your homeworks will match
set.seed(7)
#Sample 300 points.  This is your data set!
prob3data = generateSample(300)



plot(prob3data, bty='n',pch=19,col=4)
abline(v=4*pi,col='grey')
curve(mu(x), from=0, to=6*pi,col='red',lwd=2,add=TRUE)


##---```{r,fig.width=6,fig.align='center',fig.cap='\\label{fig:prob3mu} Our generated data with the true mean function in red.'}
source('hw3prob3.R')
plot(prob3data, bty='n',pch=19,col=4)
abline(v=4*pi,col='grey')
curve(mu(x), from=0, to=6*pi,col='red',lwd=2,add=TRUE)




## --- ```{r, results='hide'}
smallX = prob3data$x<4*pi
bigX = !smallX
npXsmall = npreg(y~x, data=prob3data[smallX,], tol=1e-3, ftol=1e-4)
npXbig = npreg(y~x, data=prob3data[bigX,], tol=1e-3, ftol=1e-4)
npXall = npreg(y~x, data=prob3data, tol=1e-3, ftol=1e-4)

## --- The bandwidths are given in \autoref{tab:prob3bws}. Note that for $x<4\pi$, the bandwidth is much larger than for $x>4\pi$. The regression on all the $x$ values is somewhere in between (though much closer to the bandwidth for $x>4\pi$).

## --- ```{r}
bws = double(3)
bws[1] = npXsmall$bw
bws[2] = npXbig$bw
bws[3] = npXall$bw
names(bws) = c('x < 4pi', 'x > 4pi','all x')


## --- ```{r,fig.width=5,fig.height=4,fig.align='center',fig.cap='\\label{fig:prob3regsA} Small values of x.'}
plotAllNp <- function(dat, mod, from, to){
  plot(dat, col='grey60', pch=19, cex=.5, las=1, bty='n')
  lines(dat[,1],fitted(mod), col=4, lwd=2)
  curve(mu(x), from=from, to=to, col=2, add=TRUE)
}
plotAllNp(prob3data[smallX,], npXsmall, 0, 4*pi)


## --- ```{r,fig.width=5,fig.height=4,fig.align='center',fig.cap='\\label{fig:prob3regsB} Large values of x.'}
plotAllNp(prob3data[bigX,], npXbig, 4*pi, 6*pi)


## --- {r,fig.width=5,fig.height=4,fig.align='center',fig.cap='\\label{fig:prob3regsC} All values of x.'}
plotAllNp(prob3data, npXall, 0, 6*pi)
abline(v=4*pi, col='grey', lwd=.5)


## Again, we can use the output of `npreg` to get CV estimates of 
## prediction risk for each of these regressions to try to support our 
## visual conclusions. In particular, these estimates are 
round(npXsmall$bws$fval,3)
round(npXbig$bws$fval,3)
round(npXall$bws$fval,3)
## In this case, the model fit to the small values of $x$ does the best, 
## while that fit to the larger values of $x$ only seems to do worse than the model 
## fit to all the values. This is a little bit counterintuitive, as I would have expected 
## the model fit to all the data to be the worst. There are a few possible explanations.

## The first is that, for large values of $x$, `mu(x)` is oscillating much more quickly. 
## So, the right half of the function is much more difficult to estimate, and we don't 
## actually have that much data (only `r sum(bigX)` points). So, relative to the overall 
## variance, we might actually be doing better. A bonehead way to check would be to 
## examine each of those estimates relative to the variance in $y$, a pseudo $R^2$ (note 
## that here, we are using an out-of-sample measure in the numerator, so this statistic 
## does _not_ have the usual downsides): 
round(npXsmall$bws$fval/var(prob3data[smallX,2]),3)
round(npXbig$bws$fval/var(prob3data[bigX,2]),3)
round(npXall$bws$fval/var(prob3data[,2]),3) 
## These numbers seem to support this first explanation. 

A second (and related) explanation is that the left half is so easy, that it's contribution to the overall estimate of the MSE outweighs the contribution of the right half. Remember, we have about two thirds of the data in the left half.

A third possibility is that this is simply a stochastic problem: this is only an estimate of the MSE based on 5-fold cross validation. If we ran `npreg` again (on the same data), we would get a different estimate. While possible, my bet is that the Monte Carlo error (from CV) is not large enough for this explanation to explain much. All told, I bet the the first two explanations account for the majority of the issue.
