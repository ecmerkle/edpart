"termerr" <- function(nodenum,summ.tab,nterms,imp)
  {
    # Function to calculate number of terminal nodes
    # attached to nodenum, as well as error associated with
    # those terminal nodes.
    rownum <- which(summ.tab[,1]==nodenum)
    if (is.na(summ.tab[rownum,2])){
      nterms <- nterms + 1
      if(sum(summ.tab[,5]==nodenum)==1){
        rowpar <- which(summ.tab[,5]==nodenum)
        colpar <- 6}
      else{
        rowpar <- which(summ.tab[,7]==nodenum)
        colpar <- 8}
      imp <- imp + summ.tab[rowpar,colpar]
    }
    else{
      node.l <- summ.tab[rownum,5]
      tmp.l <- nterm(node.l,summ.tab,nterms,imp)
      nterms <- tmp.l$nterms
      imp <- tmp.l$imp
      
      node.r <- summ.tab[rownum,7]
      tmp.r <- nterm(node.r,summ.tab,nterms,imp)
      nterms <- tmp.r$nterms
      imp <- tmp.r$imp
    }

    list(nterms=nterms,imp=imp)
  }
