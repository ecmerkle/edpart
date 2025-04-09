"xvalid8" <- function(dvs,ivs,cptab,folds,crit,n.ivs,minsplit,
                      minbucket,mincp,maxdepth)
  {
    # Function to do k-fold cross-validation on classification/
    # regression trees.

    # FOR MULTIVARIATE TREE, MUST SAVE PREDICTIONS IN DIFFERENT WAY.
    # Dimensions: cpvals, number of observations, dvs
    
    # Prelims:
    n.cps <- dim(cptab)[1]
    
    # Get geometric means between cp values, these will be
    # targeted by prune.cp later
    #cpvals <- sqrt(cptab[1:(n.cps-1),1]*cptab[2:n.cps,1])
    cpvals <- sqrt(cptab[,1]*c(1,cptab[2:n.cps,1]))
    
    # Initialize matrix of predictions for each entry in cpvals:
    # CHANGE FOR MULTIVARIATE
    dv.preds <- matrix(0,dim(ivs)[1],length(cpvals))
    
    # Split data into k folds:
    split.dat <- kfold(matrix(dvs),ivs,folds)
    learn.dat <- split.dat$learn
    valid.dat <- split.dat$valid
    valid.dv <- split.dat$vld.dv
    
    # For each entry in cpvals, build trees with each learning sample.
    # Make predictions on each validation sample.  Save all predictions
    # in specific column of dv.preds.
    row.start <- 1
    for (j in 1:folds){
      liv <- eval(parse(text=paste("learn.dat$learniv",j,sep="")))
      ldv <- eval(parse(text=paste("learn.dat$learndv",j,sep="")))
      viv <- eval(parse(text=paste("valid.dat$validiv",j,sep="")))
      lsize <- dim(liv)[1]
      vsize <- dim(viv)[1]
      row.end <- row.start+vsize-1
      if (is.character(ldv)){ldv <- factor(as.numeric(ldv))}

      rt.err <- calc.err(ldv,0,crit,0,lsize)
      if (crit=="gini"){
        tmp <- table(dvs)
        rt.pred <- as.numeric(names(tmp)[tmp==max(tmp)])
        if (length(rt.pred)>1){
          rt.pred <- rt.pred[which.max(runif(length(rt.pred)))]
        }
      }
      else{
        rt.pred <- mean(dvs)
      }

      s.t <- do.parts(ldv,liv,n.ivs,lsize,rt.err,rt.pred,1,1,crit,NULL,
                      minsplit,minbucket,maxdepth,rt.err)

      # If go backwards (from length(cpvals) to 1),
      # can use s.t.prn and prune smaller trees.
      # This could save time.
      s.t.prn <- s.t$summ.tab

      for (i in length(cpvals):2){  #1:length(cpvals)){
        s.t.prn <- as.matrix(prune.cp(s.t.prn,cpvals[i],rt.err))
        new.preds <- pred(viv,s.t.prn)
        dv.preds[row.start:row.end,i] <- new.preds
        #cur.row <- cur.row + vsize
      }
      if (crit=="ss"){
        dv.preds[row.start:row.end,1] <- mean(ldv)
      }
      if (crit=="gini"){
        dv.tab <- table(ldv)
        tmp.pred <- names(dv.tab)[dv.tab==max(dv.tab)]
        if (tmp.pred > 1){
          tmp.pred <- tmp.pred[which.max(runif(length(tmp.pred)))]
        }
        dv.preds[row.start:row.end,1] <- names(dv.tab)[dv.tab==max(dv.tab)]
      }
      row.start <- row.end+1
    }

    # valid.dv has ordered dv data; use to compare to predictions
    # across trees.
    xval.res <- calc.xval(cpvals,dv.preds,valid.dv,crit)

    xval.res
  }
