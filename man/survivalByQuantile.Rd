\name{survivalByQuantile}
\alias{survivalByQuantile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{survivalByQuantile
%%  ~~function to do ... ~~
}
\description{Apply survival (specify type later) analysis on quantiles of
threshold ranging from 0.01 to 0.99. Then use generated p-value and its
corresponding threshold quantile to create a countinuous plot for choosing a
proper threshold.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
survivalByQuantile(input.var, input.mat, surv.dat, percentiles = seq(.01,.99,0.01))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input.var}{a character object specifying which subject survival analysis
  is applying to.
%%     ~~Describe \code{x} here~~
}
  \item{input.mat}{a data frame containing information about an object to be tested.
  The sample order must agree with surv.dat's sample order
%%     ~~Describe \code{x} here~~
}
  \item{surv.dat}{a data frame containing all survival information. The sample
  order must agree with input.dat's sample order
%%     ~~Describe \code{x} here~~
}
  \item{percentiles}{a list of numeric values specifying all of the percentiles
  to be tested on
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{The function will generate a data frame containing hazard ratio,
lower bound, upper bound, percentile, cutoff threshold, and p-value. This data
frame can then be manipulated for various plots
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
