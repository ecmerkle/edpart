"prune" <- function(summ.tab,mincp,rt.err,crit)
  {
    # Function to do cost-complexity pruning; described
    # in Breiman et al. p. 69.
    cptab <- NULL

    # Prelim calls:
    tmperr <- termerr(1,summ.tab,0,0)
    
    while (dim(summ.tab)[1]>2){
      tmperr <- termerr(1,summ.tab,0,0)
      tmpres <- matrix(0,dim(summ.tab)[1],4)
      tmpres[1,] <- c(1,rt.err,tmperr$imp,tmperr$nterms)

      for (i in 2:(dim(summ.tab)[1])){
        nodenum <- as.numeric(summ.tab[i,1])
        tmperr <- termerr(nodenum,summ.tab,0,0)
        # Odd nodenums always split right.  But not sure how general
        # this is, so many have to change ifelse() at some point
        ifelse(nodenum%%2==1,colnum<-8,colnum<-6)
        node.err <- as.numeric(summ.tab[which(summ.tab[,(colnum-1)]==nodenum),
                                        colnum])
        tmpres[i,] <- c(nodenum,node.err,tmperr$imp,tmperr$nterms)
      }
      tmpres <- tmpres[tmpres[,4]>1,,drop=F]

      cps <- (tmpres[,2]-tmpres[,3])/((tmpres[,4]-1)*rt.err)
      snipnode <- tmpres[which.min(cps),1]

      # Prune off branches
      summ.tab <- snip(summ.tab,snipnode)
      tmperr <- termerr(1,summ.tab,0,0)
      if (tmperr$nterms==1){tmperr$imp<-rt.err}
      cptab <- rbind(cptab,c(min(cps),tmperr$nterms,tmperr$imp/rt.err))
    }

    # Reorder cp table, break off splits < mincp:
    cptab <- cptab[order(cptab[,1],decreasing=T),]
    cutoff <- sum(cptab[,1]>mincp)+1
    cptab <- cptab[1:cutoff,]
    cptab[cutoff,1] <- mincp
    cptab <- as.data.frame(cptab)
    names(cptab) <- c("CP","nterm","relerr")
    
    cptab
  }
