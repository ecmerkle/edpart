"calc.xval" <- function(cpvals,dv.preds,valid.dv,crit)
  {
    # Function to calculate cross-validated error and SEs
    # according to equations provided by Breiman et al.
    n.cps <- length(cpvals)
    n.obs <- dim(valid.dv)[1]
    xval.res <- matrix(0,n.cps,2)
    
    if (crit=="gini")
      {
        tab.dv <- table(valid.dv)
        rt.pred <- names(tab.dv)[tab.dv==max(tab.dv)]
        if (length(rt.pred) > 1){
          rt.pred <- rt.pred[which.max(runif(length(rt.pred)))]
        }
        corr.pred <- valid.dv==rt.pred
        rt.err <- sum((1-corr.pred)^2)/n.obs
        for (i in 1:n.cps){
          corr.pred <- valid.dv==dv.preds[,i]

          # Equations from pp. 307-308 Breiman et al:
          tmp.err <- sum((1-corr.pred)^2)/n.obs
          tmp.err.std <- tmp.err/rt.err
          
          tmp.se <- (1/n.obs)*sum(((1-corr.pred)^2/rt.err - tmp.err.std)^2)
          tmp.se <- sqrt(tmp.se/n.obs)
          
          xval.res[i,1:2] <- c(tmp.err.std,tmp.se)
        }
      }
    if (crit=="ss")
      {
        rt.err <- (1/n.obs)*sum((valid.dv-mean(valid.dv))^2)
        for (i in 1:n.cps){
          # Equations from p. 307, 308 Breiman:
          # (appears to be what rpart uses)
          tmp.err <- (1/n.obs)*sum((valid.dv-dv.preds[,i])^2)
          tmp.err.std <- tmp.err/rt.err

          tmp.se <- (1/n.obs)*sum(((valid.dv-dv.preds[,i])^2/rt.err -
                                   tmp.err.std)^2)
          tmp.se <- sqrt(tmp.se/n.obs)  #(rt.err*n.obs))
          
          # Calculating SE as defined on p. 309:
          # (parentheses in Breiman are confusing)
          # Gives smaller estimates than rpart...
          # Problem with equation, or rpart uses equation on top
          # p 308?
          #s2.1 <- (1/n.obs)*(sum((valid.dv-dv.preds[,i])^4)) - (tmp.err)^2
          #s2.2 <- (1/n.obs)*(sum((valid.dv-mean(valid.dv))^4)) - (rt.err)^2
          #s.12 <- (1/n.obs)*(sum((valid.dv-dv.preds[,i])^(2)*
          #                       (valid.dv-mean(valid.dv))^2)) -
          #                   tmp.err*rt.err
          #tmp.se <- tmp.err.std*sqrt((1/n.obs)*(s2.1/(tmp.err^2) -
          #                                  (2*s.12/(tmp.err*rt.err)) +
          #                                  (s2.2/(rt.err^2))))

          xval.res[i,1:2] <- c(tmp.err.std,tmp.se)
        }
      }

    xval.res
  }
