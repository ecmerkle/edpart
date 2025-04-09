"srch.splt" <- function(dvs,ivs,n.ivs,nodenum,minbucket,crit,i.t,sampsize,
                        rt.err)
{
  # For a given set of dvs and ivs, searches all possible splits
  # among ivs and returns the best split

  # dvs is data.frame of response variables/data
  # ivs is data.frame of predictor variables/data
  # n.ivs is number of predictor variables
  # nodenum is max node number that has been used to this point
  # minbucket is minimum permissible number of observations in a child node 
  # crit is criterion used
  # i.t is error of parent node
  deltas <- iv.nums <- splts <- NULL


  # If there is only one iv, this screws up.  Solution: if there
  # is only 1 iv, cbind columns of zeros to end of it.  Then make
  # sure it is of class "data.frame".  Do this in parent function.
  for (i in 1:n.ivs){
    # Order potential iv splits based on whether iv[,i] is
    # categorical or continuous:
    pot.splts <- order.ivs(ivs,n.ivs,dvs,i)
    split.opts <- pot.splts$split.opts
    ncats <- pot.splts$ncats
    
    # Attempt all splits on iv[,i]
    for (j in 1:(ncats-1)){
      splt.try <- split(split.opts,c(rep(1,j),rep(2,(ncats-j))))

      node.l <- dvs[ivs[,i] %in% splt.try$`1`]
      node.r <- dvs[ivs[,i] %in% splt.try$`2`]

      # Check minbucket restriction
      if (length(node.l)<minbucket | length(node.r)<minbucket){
        delta.i <- 0
        next
      }
      
      # Calculate delta i(t) for the split:
      delta.i <- calc.err(node.l,node.r,crit,i.t,sampsize)

      deltas <- c(deltas,delta.i)
      iv.nums <- c(iv.nums,i)
      splts <- c(splts,j)
    }
  }

  best.loc <- which.max(deltas)
  max.delta.i <- max(deltas)

  iv.win <- iv.nums[best.loc]
  best.iv <- names(ivs)[iv.win]
  best.splt <- splts[best.loc]
  pot.splts <- order.ivs(ivs,n.ivs,dvs,iv.win)
  split.opts <- pot.splts$split.opts
  ncats <- pot.splts$ncats
  #cp <- max.delta.i/i.t
  
  # Split data between the two nodes:
  splt.try <- split(split.opts,c(rep(1,best.splt),rep(2,(ncats-best.splt))))  
  dv.l <- dvs[ivs[,iv.win] %in% splt.try$`1`]
  imp.l <- calc.err(dv.l,0,crit,0,sampsize)
  dv.r <- dvs[ivs[,iv.win] %in% splt.try$`2`]
  imp.r <- calc.err(dv.r,0,crit,0,sampsize)

  resub.l <- resub.r <- N.l <- N.r <- NULL
  if (crit=="gini"){
    N.l <- length(dv.l)
    N.r <- length(dv.r)
    tab.dv <- table(dv.l)
    pred.l <- names(tab.dv)[tab.dv==max(tab.dv)]
    # If there there is a tie between classes for max number
    # of observations in a node, randomly choose one for prediction
    # (Could also base this on relative proportion of classes in full
    #  data, but this could also result in a tie.):
    if (N.l>1){
      pred.l <- pred.l[which.max(runif(length(pred.l)))]
    }
    resub.l <- calc.risk(dv.l,pred.l,sampsize)
    tab.dv <- table(dv.r)
    pred.r <- names(tab.dv)[tab.dv==max(tab.dv)]
    if (N.r>1){
     pred.r <- pred.r[which.max(runif(length(pred.r)))]
    }
    resub.r <- calc.risk(dv.r,pred.r,sampsize)
  }
  if (crit=="ss"){
    pred.l <- mean(dv.l)
    pred.r <- mean(dv.r)
  }
  iv.l <- ivs[ivs[,iv.win] %in% splt.try$`1`,]
  iv.r <- ivs[ivs[,iv.win] %in% splt.try$`2`,]
  nodenum.l <- nodenum+1
  nodenum.r <- nodenum+2
  
  # Write best split differently depending on whether iv is
  # continuous/categorical:
  if (is.factor(ivs[,iv.win])){
    node.l <- NULL
    for (nm.iter in 1:best.splt){
      node.l <- paste(node.l,split.opts[nm.iter],sep="")
    }
    best.splt <- paste(best.iv," == ",node.l,sep="")
  }else{
  best.splt <- paste(best.iv," <= ",mean(split.opts[best.splt:
                                                    (best.splt+1)]),sep="")
  }

  # Check if overall cp decrease is greater than min.cp:
  #if (max.delta.i/rt.err < .01){
  #  dv.l <- dv.r <- NULL #imp.l <- imp.r <- 0
  #}
  
  # Could add output for surrogates:
  list(best.splt=best.splt,max.delta.i=max.delta.i,pred.l=pred.l,
       pred.r=pred.r,dv.l=dv.l,dv.r=dv.r,iv.l=iv.l,iv.r=iv.r,imp.l=imp.l,
       imp.r=imp.r,nodenum.l=nodenum.l,nodenum.r=nodenum.r,resub.l=resub.l,
       resub.r=resub.r,N.l=N.l,N.r=N.r)
}
