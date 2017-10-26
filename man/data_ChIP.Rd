\name{data_ChIP}
\alias{data_ChIP}
\docType{data}
\title{
 ChIP-seq dataset
}
\description{
A dataset containing the scores of six algorithms, MACS, SPP, Peakseq, Cisgenome, Quest and SISSRS.
}
\usage{data("data_ChIP")}
\format{
  A data frame with 36000 observations on the following 3 variables.
  \describe{
    \item{\code{y1}}{a numeric vector}
    \item{\code{y2}}{a numeric vector}
    \item{\code{x}}{a factor with levels \code{cisgenome} \code{macs} \code{peakseq} \code{quest} \code{sissrs} \code{spp}}
  }
}

\source{
Li, Q., et al.(2011).
}
\references{
 Li, Q., Brown, J. B., Huang, H., Bickel, P. J., et al. (2011).
 Measuring reproducibility of high-throughput experiments. The Annals of Applied Statistics 5, 1752-1779.
}
\examples{
data(data_ChIP)

\dontrun{
data(data_ChIP)
## estimate
par.ini = c(1, 0, -0.1, 0, 0, 0.5)  # initial value
fit <- ccrFit(cbind(y1, y2)~x, data = data_ChIP,
             link = 'loglog', par.ini = par.ini)
    }
}
\keyword{datasets}
