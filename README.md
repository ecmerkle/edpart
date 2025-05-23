# edpart
Ed's recursive partitioning and regression trees package to reproduce some results of the [rpart package.](https://doi.org/10.32614/CRAN.package.rpart) The edpart package is incomplete and will probably never be completed. It is written purely in R with no dependencies, so it might be useful for self-study. Here is an example of its use:

```r
library("edpart")
data(Boston, package = "MASS")

## tree predicting medv based on the first 13 variables
efit <- edpart(Boston$medv, Boston[,1:13])

## returns cptable akin to rpart's cptable
efit

## comparison to rpart
library("rpart")
rfit <- rpart(medv ~ ., data = Boston)
rfit$cptable
```

If you are interested in citing this package, I prefer that you cite the paper below instead (the package was used to write the paper):

Merkle, E. C. & Shaffer, V. A. (2011). [Binary recursive partitioning: Background, methods, and application to
psychology](https://ecmerkle.github.io/pub/MerkleShaffer2011.pdf). *British Journal of Mathematical and Statistical Psychology*, *64*, 161--181. (DOI: [10.1348/000711010X503129](http://dx.doi.org/10.1348/000711010X503129))

