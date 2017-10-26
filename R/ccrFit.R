
  #' @title Fit the correspondence curve regression
  #'
  #' @rdname ccrFit
  #' 
  #' @param formula
  #' object of class 'formula'
  #' (or one that can be coerced to that class):
  #' a symbolic description of the model to be fitted.
  #' The details of model specification are given under 'Details'.
  #' 
  #' @param data
  #' an optional data frame, list or environment
  #' (or object coercible by as.data.frame to a data frame)
  #' containing the variables in the model. If not found in data,
  #' the variables are taken from environment(formula),
  #' typically the environment from which ccrFit is called.
  #' @param model
  #' logical. If TRUE the corresponding components of the fit  are returned.
  #' @param link
  #' The link function to be used for fitting, currently only 
  #' link = 'loglog' and 'logitlogit'.
  #' @param is.slope
  #' logical. Only for link 'logitlogit'. Default is FALSE.
  #' @param is.ht
  #' logical. Only for link 'logitlogit'. Default is FALSE.
  #' @param par.ini
  #' the initial values for the estimate parameters.
  #' If is.null(par.ini) == TRUE, par.ini is set in the the details.
  #' @param tm
  #' The vector of tm.
  #' @param method
  #' The method to be used for fitting, currently only method = 'ccr.fit'.
  #' @param sig.level
  #' The significant level. Default is 0.05.
  #'
  #' @description Fit the correspondence curve regression
  #' 
  #' @details Please refer to \strong{Li, Q. and Zhang, F. (2017)}.
  #' 
  #' @references  Li, Q. and Zhang, F. (2017). 
  #' A regression framework for assessing covariate effects 
  #' on the reproducibility of high-throughput experiments. 
  #' Biometrics, \url{http://dx.doi.org/10.1111/biom.12832}.
  #' 
  #' @return A list with the elements:
  #' 
  #' \describe{
  #'   \item{coefficients}{a named vector of coefficients.}
  #'   \item{model}{if requested (the default), the model frame used.}
  #'   \item{call}{the matched call.}
  #'   \item{std.error}{the estimated standard errors.}
  #'   \item{CI}{the confidence intervals.}
  #'   }
  #'   
  #' @author Feipeng Zhang and Qunhua Li
  #' 
  #' @keywords ccrFit
  
  #' @import stats
  #' @export
  #'
  #' @examples
  #' ## The example of ChIP data
  #' \dontrun{
  #' data(data_ChIP)
  #' ## estimate
  #' par.ini = c(1, 0, -0.1, 0, 0, 0.5)  # initial value
  #' fit <- ccrFit(cbind(y1, y2)~x, data = data_ChIP,
  #'              link = 'loglog', par.ini = par.ini)
  #'              
  #'              
  #' ## The example of microarray data
  #' data(data_microarray)
  #' ## with slope without ht
  #' par.ini = c(-1, 1, 0.1, 0.1)    # initial value
  #' fit <- ccrFit(cbind(y1,y2)~x1+x2, 
  #'               data = data_microarray,
  #'               link = 'logitlogit', 
  #'               is.slope = TRUE, is.ht = FALSE, 
  #'               par.ini = par.ini)
  #'
  #'## with slope with ht, including iteractions
  #' par.ini = c(-1, 0.1, 0.1, 0, 1,  0.1, 0.1, 0)
  #' fit <- ccrFit(cbind(y1,y2)~x1*x2, 
  #'               data = data_microarray,
  #'               link = 'logitlogit', 
  #'               is.slope = TRUE, is.ht = TRUE, 
  #'               par.ini = par.ini)
  #' }
  
  ccrFit <- function (formula, data, model = TRUE, 
                    link = c('loglog', 'logitlogit'), 
                    is.slope = FALSE, is.ht = FALSE, 
                    par.ini, 
                    tm=seq(0.01, 0.9999, length.out = 50), 
                    method = 'ccr.fit', sig.level = 0.05) 
  {
  
  #method <- match.arg(method)
  
  call <- match.call()
  
  
  if(missing(data)) data <- environment(formula)
  mf <- match.call(expand.dots = FALSE)
  m <- match(c('formula', 'data'),
  names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  ## need stats:: for non-standard evaluation
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  if(identical(method, 'model.frame')) return(mf)
  
  # if (!is.character(method) && !is.function(method))
  # stop('invalid 'method' argument')
  
  mt <- attr(mf, 'terms') # allow model.frame to have updated it
  
  y <- model.response(mf, 'numeric')  # response
  
  if (is.empty.model(mt)) {
  x <- NULL
  z <- list(coefficients =  numeric() )
  }
  else {
  ## We want to set the name on this call and the one below for the
  ## sake of messages from the fitter function
  x <- model.matrix(mt, mf, contrasts)
  # z <- ccr.fit(y, x, family, is.slope, is.ht, par.ini,
  #              tm, sig.level)
  z <- eval(call(if(is.function(method)) 'method' else method,
  y, x, link, is.slope, is.ht, par.ini, 
  tm, sig.level))
  }
  class(z) <- c('lm')
  z$contrasts <- attr(x, 'contrasts')
  #z$xlevels <- .getXlevels(mt, mf)
  z$call <- call
  z$terms <- mt
  if (model)   z$model <- mf
  
  z$x <- x
  z$y <- y
  
  z
  }