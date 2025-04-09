"kfold" <- function(dvs,ivs,folds)
{
  # function to split data into k sets for cv
  # returns list of k learning sets and k validation sets
  # if list is "tmp", can call any specific set by, i.e.: 
  # tmp$learn$learn1 or tmp$valid$valid2

  # this breaks if dim(dat)[1] == folds

  srtvec <- runif(dim(ivs)[1])
  srtiv <- ivs[order(srtvec),]
  srtdv <- matrix(dvs[order(srtvec),])
  
  # number of observations that go in each validation set:
  k <- ceiling(dim(ivs)[1]/folds)
  grpnum <- rep(1:folds,k)[1:(dim(ivs)[1])]
  
  for (i in 1:folds){
    eval(parse(text=paste("validiv",i," <- srtiv[grpnum==",i,",]",sep="")))
    eval(parse(text=paste("validdv",i," <- matrix(srtdv[grpnum==",i,",])",
                 sep="")))

    eval(parse(text=paste("learniv",i," <- srtiv[!grpnum==",i,",]",sep="")))
    eval(parse(text=paste("learndv",i," <- matrix(srtdv[!grpnum==",i,",])",
                 sep="")))
  }

  lrn.com <- "learn <- list(learniv1=learniv1,learndv1=learndv1"
  vld.com <- "valid <- list(validiv1=validiv1,validdv1=validdv1"
  vld.dv.com <- "vld.dv <- rbind(validdv1"
  for (i in 2:folds){
    tmp.lrn <- paste(",learniv",i,"=learniv",i,",learndv",i,"=learndv",i,
                     sep="")
    lrn.com <- paste(lrn.com,tmp.lrn,sep="")

    tmp.vld <- paste(",validiv",i,"=validiv",i,",validdv",i,"=validdv",i,
                     sep="")
    vld.com <- paste(vld.com,tmp.vld)

    tmp.vld.dv <- paste(",validdv",i,sep="")
    vld.dv.com <- paste(vld.dv.com,tmp.vld.dv)
  }
  lrn.com <- paste(lrn.com,")",sep="")
  vld.com <- paste(vld.com,")",sep="")
  vld.dv.com <- paste(vld.dv.com,")",sep="")
  
  eval(parse(text=lrn.com))
  eval(parse(text=vld.com))
  eval(parse(text=vld.dv.com))
  
  out <- list(learn=learn,valid=valid,vld.dv=vld.dv)
  out
}
