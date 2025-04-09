"calc.err" <- function(node.l,node.r,crit,i.t,sampsize)
{
  # Calculates delta i(t) for a specific split
  # To get i(t) for a single node, pass all observations
  # in the node as node.l, node.r=0, and i.t=0.

  if (crit=="ss"){
    ss.l <- sum((node.l - mean(node.l))^2)
    ss.r <- sum((node.r - mean(node.r))^2)

    delta.i <- i.t - (ss.l + ss.r)/sampsize
  }

  if (crit=="gini"){
    # if node.l is a factor, its levels will be the same as
    # the original data.  So can use one nclass for node.l and node.r
    nclass <- length(levels(node.l))

    i.l <- i.r <- 0
    # Calculate class proportions; differs depending on whether
    # there are any observations in node.r.
    cls.props.l <- summary(node.l)/length(node.l)
    if (i.t==0){
      node.r <- NULL
      p.r <- i.r <- 0
      p.l <- length(node.l)/(length(node.l)+length(node.r))
      for (i in 1:nclass){
        i.l <- i.l + cls.props.l[i]^2
      }
      #i.l <- (1-i.l)*(length(node.l)/sampsize)
      i.l <- (1-i.l)*length(node.l)
    }else{
      cls.props.r <- summary(node.r)/length(node.r)
      p.l <- length(node.l)/(length(node.l)+length(node.r))
      p.r <- 1-p.l
      for (i in 1:nclass){
        i.l <- i.l + cls.props.l[i]^2
        i.r <- i.r + cls.props.r[i]^2
      }
      p.t <- (length(node.l) + length(node.r))/sampsize
      i.l <- (1-i.l)*(length(node.l)) #/sampsize)  #*p.l*p.t
      i.r <- (1-i.r)*(length(node.r)) #/sampsize) #*p.r*p.t
    }

    delta.i <- (i.t - i.l - i.r)  #p.l*i.l - p.r*i.r
  }

  # If i.t=0, then they want impurity of single node.  To get this,
  # take the negative value.
  if (i.t==0){delta.i <- -delta.i}
  #ifelse(i.t==0,delta.i <- -delta.i, delta.i <- sampsize*(delta.i))
  
  delta.i
}
