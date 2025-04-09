\name{edpart}
\alias{edpart}
\title{
Ed's Recursive Partitioning Algorithm
}
\description{
This is an incomplete function that is intended to mimic rpart() from
the rpart package.
}
\usage{
edpart(dvs, ivs, crit = "none", minsplit = 10, minbucket = 3, mincp = 0.01, folds = 5)
}
\arguments{
  \item{dvs}{
Data columns containing response variables.
}
  \item{ivs}{
Data columns containing explanatory variables.
}
  \item{crit}{
Criterion used for splitting (currently unused).
}
  \item{minsplit}{
Minimum number of observations in a node, to attempt a new split.
}
  \item{minbucket}{
Minimum number of observations in a terminal node.
}
  \item{mincp}{
Complexity parameter.
}
  \item{folds}{
Number of cross-validation folds.
}
}
\details{
This function is incomplete. It successfully reproduces some rpart
results. A minor discrepancy between edpart and rpart is that rpart stratifies cross-
validation samples, whereas edpart chooses randomly.  This seems to be most 
noticeable when there aren't many observations in one class, which means there is
no guarantee that all classes appear in each validation sample.
}
\value{
A cptable containing splits and related results, similar to the rpart cptable.
}
\references{
Merkle, E. C., & Shaffer, V. A. (2011). Binary recursive partitioning: Background, methods, and application to psychology. British Journal of Mathematical and Statistical Psychology, 64, 161-181.

Breiman, L., Friedman, J., Olshen, R. A., & Stone, C. J. (1984). Classification and regression trees. Wadsworth.
}
\author{
Edgar C. Merkle
}
\seealso{
\code{rpart()} from the rpart package.
}
\examples{
\dontrun{
library("rpart")
data(Boston, package = "MASS")

rfit <- rpart(medv ~ ., data = Boston)
rfit$cptable

efit <- edpart(Boston$medv, Boston[,1:13])
efit
}
}
