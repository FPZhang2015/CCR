
ccr.fit <- function(y, x, link,  
                    is.slope = FALSE, is.ht = FALSE, 
                    par.ini, tm, sig.level
)
{
  if (is.null(n <- nrow(x))) stop("'x' must be a matrix")
  if(n == 0L) stop("0 (non-NA) cases")
  p <- ncol(x)
  if (p == 0L) {
    ## oops, null model
    return(list(coefficients = numeric() ))
  }
  
  
  ny <- NCOL(y)
  ## treat one-col matrix as vector
  if(is.matrix(y) && ny != 2)
    stop("y need have 2 columns")
  if (NROW(y) != n)
    stop("incompatible dimensions")
  # chkDots(...)
  
  y1 <- y[,1]
  y2 <- y[,2]
  
  ## some preliminary functions
  gfun <-  switch(link, 
                  loglog = function(x){log(x)},
                  logitlogit = function(x){log(x/(1-x))} )
  
  gfun.inv <- switch(link, 
                     loglog = function(x){exp(x)},
                     logitlogit = function(x){exp(x)/(1+exp(x))} )
  
  hfun <-  switch(link, 
                  loglog = function(x){log(x)}, 
                  logitlogit = function(x){log(x/(1-x))} )
  
  ## tm and psi.tm are declared before
  
  prepare.Uim <- function(a, tm){ ### Uim
    
    n <- nrow(a)
    m <- length(tm)
    
    # a: (y_i1, y_i2) scores
    a.cdf1 <- ecdf(a[,1])
    a.cdf2 <- ecdf(a[,2])
    cdf1.value <- a.cdf1(a[,1])*n/(n+1)
    cdf2.value <- a.cdf2(a[,2])*n/(n+1)
    
    
    # get counts in each category
    wt.temp <- 1*(1*outer(cdf1.value, tm, "<=") + 
                    1*outer(cdf2.value, tm, "<=")==2) 
    wt <- cbind(wt.temp[, 1], wt.temp[, -1] - wt.temp[, -m])     # n*m
    
    invisible(wt)
  }
  
  likFun <-  switch(link, 
                    loglog = function(thets){
                      
                      #modmat <- model.matrix(formula)
                      modmat <- x
                      
                      lables <- apply(modmat, 1, paste, collapse ="|")
                      lables_uqi <- unique(lables)
                      
                      lik <- 0
                      for (i in 1:length(lables_uqi)){
                        is.in <- which(lables==lables_uqi[i])
                        y1.x <- y1[is.in]
                        y2.x <- y2[is.in]
                        
                        x.i <- unlist(strsplit(lables_uqi[i],split = "|"))
                        x.i <- as.numeric(x.i[which(!(x.i=="|"))])
                        
                        wt <- prepare.Uim(cbind(y1.x, y2.x), tm)
                        
                        
                        coef <- drop(thets %*% x.i) 
                        
                        ht <- hfun(tm)
                        ght <- gfun.inv(coef * ht)
                        dght <- c(ght[1], diff(ght))
                        
                        lpr <- ifelse(dght > 0, log(dght), Inf) 
                        lik <- lik + sum(wt %*% lpr)
                      }
                      
                      return(lik)
                    },
                    logitlogit =  function(thets){
                      
                      #modmat <- model.matrix(formula)
                      modmat <- x
                      
                      lables <- apply(modmat, 1, paste, collapse ="|")
                      lables_uqi <- unique(lables)
                      
                      lik <- 0
                      for (i in 1:length(lables_uqi)){
                        is.in <- which(lables==lables_uqi[i])
                        y1.x <- y1[is.in]
                        y2.x <- y2[is.in]
                        
                        wt <- prepare.Uim(cbind(y1.x, y2.x), tm)
                        ht <- hfun(tm)
                        
                        x.i <- unlist(strsplit(lables_uqi[i],split = "|"))
                        x.i <- as.numeric(x.i[which(!(x.i=="|"))])
                        
                        nx.i <- length(x.i)
                        
                        if(is.ht == FALSE){
                          if(is.slope == FALSE){
                            coef1 <- drop(thets %*% x.i) 
                            coef2 <- 1
                          }else{
                            coef1 <- drop(thets[-(nx.i + 1)] %*% x.i)
                            coef2 <- thets[nx.i + 1]
                          }
                        }else if(is.ht == TRUE){
                          if(is.slope == FALSE){
                            coef1 <- drop(thets[1:nx.i] %*% x.i)
                            coef2 <- 1+drop(thets[-(1:nx.i)] %*% x.i[-1])
                          }else{
                            coef1 <- drop(thets[1:nx.i] %*% x.i)
                            coef2 <- drop(thets[-(1:nx.i)] %*% x.i) 
                          }
                        }
                        
                        
                        
                        ght <- gfun.inv(coef1 + coef2 * ht)
                        dght <- c(ght[1], diff(ght))
                        
                        lpr <- ifelse(dght > 0, log(dght), Inf) 
                        lik <- lik + sum(wt %*% lpr)
                      }
                      
                      return(lik)
                    }
  )
  
  #px <- ncol(model.matrix(formula))
  px <- ncol(x)
  
  # initial values
  if(is.null(par.ini)){
    par0 = c(1, rep(0.1, px-1))
    if(is.ht == FALSE){
      par1 <-  par0
    }else{
      par1 <- c(par0, par0[-1])
    }
    if(is.slope == FALSE){
      par2 <- par1
    }else{
      par2 <- c(0, par1)
      par2[px+1] <- 1
      par2[-(px+1)] <- par1
    }
    par.ini = switch(
      link,
      loglog = par0,
      logitlogit = par2
    )
  }
  
  
  
  
  fit <- optim(par = par.ini,   
               fn = function(thets){-likFun(thets)}, 
               method ="BFGS", hessian= TRUE) 
  
  est <- fit$par        # estimate
  
  
  ## names 
  #dn0 <-  colnames(model.matrix(formula))
  dn0 <- colnames(x)
  if(is.ht == FALSE){
    dn1 <-  dn0
  }else{
    dn1 <- c(dn0, paste0(dn0[-1], ":ht", seq = ""))
  }
  
  if(is.slope == FALSE){
    dn <- dn1
    if(is.null(dn)) dn <- paste0("x", 1L:px)
  }else{
    dn <- c("", dn1)
    if(is.null(dn1)) dn1 <- paste0("x", 1L:px)
    dn[px+1] <- "slope"
    dn[-(px+1)] <- dn1
  }
  
  names(est) <- switch(link, 
                       loglog = dn0,
                       logitlogit = dn)
  
  ese <- sqrt(diag(solve(fit$hessian)))       # standard error
  CI.low <- est - qnorm(1-sig.level/2, 0, 1) * ese
  CI.up <- est + qnorm(1-sig.level/2, 0, 1) * ese
  
  z = list()
  z$coefficients <- est
  z$std.error <- ese
  z$CI <- cbind(CI.low, CI.up)
  
  return(c(z[c("coefficients", "std.error", "CI")]))
  
}