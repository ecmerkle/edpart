"prune.cp" <- function(summ.tab,targcp,rt.err)
  {
    # Function to prune tree to a specific cp (targcp).
    # Used for cross-validation routines to calculate
    # cross-validated error and SEs for given values of
    # cp.
    cptab <- NULL

    # Prelim calls:
    tmpres <- matrix(0,dim(summ.tab)[1],4)
    tmperr <- termerr(1,summ.tab,0,0)
    cps <- 0
    
    while (min(cps) < targcp & dim(summ.tab)[1]>2){
#      print(summ.tab)
      tmpres <- matrix(0,dim(summ.tab)[1],4)
      tmpres[1,] <- c(1,rt.err,tmperr$imp,tmperr$nterms)
      for (i in 2:(dim(summ.tab)[1])){
        nodenum <- as.numeric(summ.tab[i,1])
        tmperr <- termerr(nodenum,summ.tab,0,0)
        ifelse(nodenum%%2==1,colnum<-8,colnum<-6)
        node.err <- as.numeric(summ.tab[which(summ.tab[,(colnum-1)]==nodenum),
                                        colnum])
        tmpres[i,] <- c(nodenum,node.err,tmperr$imp,tmperr$nterms)
      }
      tmpres <- tmpres[tmpres[,4]>1,]
      
      # If tmpres becomes 1x4 matrix, R converts it to numeric.
      # Must convert it back to matrix.
      # (would be nice to have a more elegant solution):
      if ("numeric" %in% class(tmpres)){tmpres <- matrix(tmpres,1,4)}
      cps <- (tmpres[,2]-tmpres[,3])/((tmpres[,4]-1)*rt.err)
      if (min(cps) < targcp){
        snipnode <- tmpres[which.min(cps),1]
        # Prune off branches
        summ.tab <- snip(summ.tab,snipnode)
        if ("character" %in% class(summ.tab)){summ.tab <- matrix(summ.tab,1,8)}
        tmperr <- termerr(1,summ.tab,0,0)
        if (tmperr$nterms==1){tmperr$imp<-rt.err}
        cptab <- rbind(cptab,c(min(cps),tmperr$nterms,tmperr$imp/rt.err))
      }
    }

    summ.tab
    # Reorder cp table, break off splits < mincp:
    #cptab <- cptab[order(cptab[,1],decreasing=T),]
    #cutoff <- sum(cptab[,1]>mincp)+1
    #cptab <- cptab[1:cutoff,]
    #cptab[cutoff,1] <- mincp
    #cptab <- as.data.frame(cptab)
    #names(cptab) <- c("CP","nterm","relerr")
    
    #cptab
  }
