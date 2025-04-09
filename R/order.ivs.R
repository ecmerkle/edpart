"order.ivs" <- function(ivs,n.ivs,dvs,iter)
{
  # Orders potential IV splits based on whether iv[,i] is
  # categorical or continuous:
  if (class(ivs[,iter]) != "factor"){
    split.opts <- unique(ivs[,iter])[order(unique(ivs[,iter]))]
  }
  if (class(ivs[,iter]) == "factor"){
    ncats <- length(levels(ivs[,iter]))
    
    if(class(dvs)!="factor"){
      cat.mns <- tapply(dvs,ivs[,iter],mean)
      split.opts <- levels(ivs[,iter])[order(cat.mns)]
    }
    # How to order categorical ivs based on categorical dvs?
  }
  ncats <- length(split.opts)
  list(ncats=ncats, split.opts=split.opts)
}
