"do.parts" <- function(dvs,ivs,n.ivs,sampsize,cur.err,pred,parnum,nodenum,
                       crit,summ.tab,minsplit,minbucket,maxdepth,rt.err)
{
  ifelse(is.null(summ.tab),dpth <- 0,dpth <- get.depth(parnum,summ.tab))
  if (length(dvs) < minsplit | dpth == maxdepth | cur.err == 0){
    summ.tab <- rbind(summ.tab,c(parnum,NA,NA,pred,rep(NA,4)))
  }
  else{
    new.splt <- srch.splt(dvs,ivs,n.ivs,nodenum,minbucket,crit,cur.err,
                          sampsize,rt.err)

    if (crit=="gini"){
      summ.tab <- rbind(summ.tab,c(parnum,new.splt$best.splt,
                                   new.splt$max.delta.i,pred,
                                   new.splt$nodenum.l,new.splt$resub.l,
                                   new.splt$nodenum.r,new.splt$resub.r))
    }else{
      summ.tab <- rbind(summ.tab,c(parnum,new.splt$best.splt,
                                   new.splt$max.delta.i,pred,
                                   new.splt$nodenum.l,new.splt$imp.l,
                                   new.splt$nodenum.r,new.splt$imp.r))
    }
    
    parnum <- new.splt$nodenum.l
    nodenum <- new.splt$nodenum.r

    s.t <- do.parts(new.splt$dv.l,new.splt$iv.l,n.ivs,sampsize,new.splt$imp.l,
                         new.splt$pred.l,parnum,nodenum,crit,
                         summ.tab,minsplit,minbucket,maxdepth,rt.err)

    summ.tab <- s.t$summ.tab
    parnum <- nodenum
    nodenum <- s.t$nodenum
    s.t <- do.parts(new.splt$dv.r,new.splt$iv.r,n.ivs,sampsize,new.splt$imp.r,
                    new.splt$pred.r,parnum,nodenum,crit,
                    summ.tab,minsplit,minbucket,maxdepth,rt.err)
    summ.tab <- s.t$summ.tab
    nodenum <- s.t$nodenum
  }
  list(summ.tab=summ.tab,nodenum=nodenum)
}
