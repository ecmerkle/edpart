"snip" <- function(summ.tab,nodenum)
  {
    # Snips off subtree starting at nodenum
#print(summ.tab)
    if (is.na(nodenum)){
      summ.tab <- summ.tab}
    else{
      if (nodenum=="1"){
        summ.tab[1,c(2:3,5:8)] <- NA
        summ.tab <- summ.tab[1,,drop=F]
      }else{
        #print(dim(summ.tab))
        tmprow <- which(summ.tab[,1]==nodenum)
#print(summ.tab[tmprow,])
        node.l <- summ.tab[tmprow,5]
        node.r <- summ.tab[tmprow,7]
        #summ.tab <- summ.tab[-tmprow,]
        summ.tab[tmprow,c(2:3,5:8)] <- NA
      
        summ.tab <- snip(summ.tab,node.l)
        if (!is.na(node.l)){ # & node.l>1){
          tmprow <- which(summ.tab[,1]==node.l)
          summ.tab <- summ.tab[-tmprow,,drop=F]
          #summ.tab[tmprow,c(2:3,5:8)] <- NA
        }
        summ.tab <- snip(summ.tab,node.r)
        if (!is.na(node.r)){
          tmprow <- which(summ.tab[,1]==node.r)
          summ.tab <- summ.tab[-tmprow,,drop=F]
          #summ.tab[tmprow,c(2:3,5:8)] <- NA
        }
      }
    }

    summ.tab
  }
