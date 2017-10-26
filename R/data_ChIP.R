
    #' \title{ChIP-seq dataset}
    #' \description{A dataset containing the scores of six algorithms,
    #' MACS, SPP, Peakseq, Cisgenome, Quest and SISSRS.}
    #' 
    #' The variables are as follows:
    #'
    #' \format{A data frame with 36,000 rows and 3 variables}
    #' 
    #'\describe{
    #'   \item{y1}{the scores of replicate 1}
    #'   \item{y2}{the scores of replicate 2}
    #'   \item{x}{the factor variabel of six algorithms}
    #' }
    
    #' \source {Li, Q.(2011).}
    #' \docType{data}
    #' \keywords{ data_ChIP}
    #' \usage{data(data_ChIP)}
    #'
    #' \references{
    #' \itemize{
    #' \item Li, Q., Brown, J. B., Huang, H., Bickel, P. J., et al. (2011).
    #' Measuring reproducibility of high-throughput experiments.
    #' The Annals of Applied Statistics 5, 1752-1779.
    #'   }
    #'   }
    #'   
    #'
    #' \examples{
    #' \dontrun{
    #' data(data_ChIP)
    #' summary(data_ChIP)
    #' ## estimate
    #' par.ini = c(1, 0, -0.1, 0, 0, 0.5)  # initial value
    #' fit <- ccrFit(cbind(y1, y2)~x, data = data_ChIP,
    #'              par.ini = par.ini, method = 'logitlogit')
    #' }
    #' }
    #' 'data_ChIP'
    