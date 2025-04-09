"edpart" <- function(dvs,ivs,crit="none",minsplit=10,minbucket=3,mincp=.01,
                     folds=5)
{
  # Main file for my BRP implementation

  # Prelim variables:
  n.ivs <- dim(ivs)[2]
  sampsize <- dim(ivs)[1]
  maxdepth <- 30
  summ.tab <- NULL # cols are nodenum,bestsplt,deltai,yval,lnode,imp.l,
                   # rnode,imp.r
  #cptable <- NULL # cols are cp,nsplit,relerr,rawerr,xerr,xstd

  # If they don't specify a criterion, choose one:
  if (crit=="none"){
    ifelse(is.factor(dvs) == T,crit <- "gini",crit <- "ss")
  }
  
  # Calculate error of/prediction for root node:
  rt.err <- calc.err(dvs,0,crit,0,sampsize)
  if (crit=="gini"){
    tmp <- table(dvs)
    rt.pred <- as.numeric(names(tmp)[tmp==max(tmp)])
  }
  else{
    rt.pred <- mean(dvs)
  }
  
  # Recursively partition data:
  s.t <- do.parts(dvs,ivs,n.ivs,sampsize,rt.err,rt.pred,1,1,crit,summ.tab,
                       minsplit,minbucket,maxdepth,rt.err)
  if (crit=="gini"){rt.err <- calc.risk(dvs,rt.pred,sampsize)} #)/sampsize}
  cptab <- prune(s.t$summ.tab,mincp,rt.err,crit)

  # Fill in cross-validated results for cp table:
  xval.res <- xvalid8(dvs,ivs,cptab,folds,crit,n.ivs,minsplit,minbucket,
                      mincp,maxdepth)

  cptab <- cbind(cptab,xval.res)
  names(cptab)[4:5] <- c("xerror","xstd")
  
  cptab
  
  #list(summ=summ.tab,cptable=cptable)
  #s.t$summ.tab
}
