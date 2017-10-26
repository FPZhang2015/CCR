\name{data_microarray}
\alias{data_microarray}
\docType{data}
\title{
microarray dataset
}
\description{
A dataset containing the scores of 2 platforms and 2 labs.
}
\usage{data("data_microarray")}
\format{
  The format is: A data frame with 800 rows and 4 variables
  \describe{
       \item{y1}{the scores of replicate 1}
       \item{y2}{the scores of replicate 2}
       \item{x1}{the factor variabel of platforms}
       \item{x2}{the factor variabel of labs}
   }
}

\source{
Irizarry et al. (2005).
}
\references{
Irizarry, R. A., Warren, D., Spencer, F., Kim, I. F., Biswal, S., Frank, B. C., Gabrielson, E., Garcia, J. G., Geoghegan, J., Germino, G., et al. (2005). Multiple-laboratory comparison of microarray platforms. Nature Methods 2, 345-350.
}
\examples{
data(data_microarray)

 \dontrun{
data(data_microarray)
## with slope without ht
par.ini = c(-1, 1, 0.1, 0.1)    # initial value
fit <- ccrFit(cbind(y1,y2)~x1+x2, 
              data = data_microarray,
              link = 'logitlogit', 
              is.slope = TRUE, is.ht = FALSE, 
              par.ini = par.ini)

## with slope with ht, including iteractions
par.ini = c(-1, 0.1, 0.1, 0, 1,  0.1, 0.1, 0)
fit <- ccrFit(cbind(y1,y2)~x1*x2, 
              data = data_microarray,
              link = 'logitlogit', 
              is.slope = TRUE, is.ht = TRUE, 
              par.ini = par.ini)
     }
}
\keyword{datasets}
