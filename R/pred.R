"pred" <- function(x,summ.tab)
  {
    # Given a tree structure and new data (x's), makes
    # predictions.
    x.len <- dim(x)[1]
    y.preds <- rep(0,x.len)
    
    for (i in 1:x.len){
      curr.row <- 1
      curr.splt <- summ.tab[1,2]  # could be anything but NA
      tmpdat <- x[i,]
      
      while (!is.na(curr.splt)){  #node)){
        #print(curr.node)
        #newcond <- summ.tab[curr.row,2]
        fullcond <- paste("tmpdat$",curr.splt,sep="")
        #print(eval(parse(text=fullcond)))
        ifelse(eval(parse(text=fullcond)),
               curr.node<-summ.tab[curr.row,5],
               curr.node<-summ.tab[curr.row,7])
        curr.row <- which(summ.tab[,1]==curr.node)
        curr.splt <- summ.tab[curr.row,2]
      }
      y.preds[i] <- as.numeric(summ.tab[curr.row,4])
    }

    y.preds
  }
    
