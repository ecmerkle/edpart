"get.depth" <- function(nodenum,summ.tab)
  {
    # Calculates depth of a node based on summary table
    tmp.summ <- cbind(summ.tab[,1],summ.tab[,5],summ.tab[,7])

    dpth <- 0
    while (nodenum != 1){
      nodenum.loc <- which(tmp.summ[,2:3]==nodenum,arr.ind=T)[1]
      nodenum <- tmp.summ[nodenum.loc,1]
      dpth <- dpth+1
    }

    dpth
  }
