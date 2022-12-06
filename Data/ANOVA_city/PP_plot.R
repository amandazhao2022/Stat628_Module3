########################################################################
## (Function) pp.plot: draw a normal pp-plot for vectors
## (Input)
# x: numeric matrix (n*p);
# pch: type of points;
# lty: type of lines;
# xlab: label of x-axis;
# main: title of plot;
# line.add: a logistic value, whether lines are added;
# line.method: how to get lines; 
#              "MSD" => Each line is determined by estimates of mean and standard deviations;
#              "Q"   => Each line is determined by quantiles of given probabilities;
# line.probs: two probabilities are desired when line.method = "Q";
# legend.add : a logistic value, whether legend is added;
# legend.location: the location of legend;
## (Output)
pp.plot=function(x,
                 pch = 1,lty = 1, xlab,main = "Normal P-P Plot",
                 line.add = TRUE, line.method = c("MSD","Q"),line.probs = c(0.25,0.75),
                 legend.add=TRUE,legend.location="bottomright"){
  x.mat = as.matrix(x)
  x.range = c(min(x),max(x))
  n <- nrow(x.mat)
  p <- ncol(x.mat)
  try(if (length(pch)<p) stop("There are not enough values for 'pch'."))
  if (!is.null(colnames(x.mat))) {
    x.names = colnames(x.mat)
  }else x.names = paste("Var",1:p)
  if (line.add){
    try(if (is.null(line.method)) stop("The choices of 'line.method' are 'MSD' or 'Q'."))
    try(if (length(lty)<p) stop("There are not enough values for 'lty'."))
  }
  
  x.at = seq(from = floor.p(min(x.range),digits = -1*(floor(log(max(x.range)-min(x.range),base=10))-1)),
             to = ceiling.p(max(x.range),digits = -1*(floor(log(max(x.range)-min(x.range),base=10))-1)), 
             by = floor.p((max(x.range)-min(x.range))/10,digits = -1*(floor(log(max(x.range)-min(x.range),base=10))-1)))
  plot(seq(min(x.range),max(x.range),length.out=n*p),qnorm(seq(0.01,0.99,length.out=n*p)),type="n",
       xaxt="n",yaxt="n",xlim = c(min(x),max(x)),ylim=qnorm(c(0.01,0.99)),
       xlab=xlab,ylab="Normal % probability",main=main)
  axis(1,at=x.at)
  axis(2,labels=c(1,5,10,20,30,50,70,80,90,95,99),
       at=qnorm(c(1,5,10,20,30,50,70,80,90,95,99)/100))
  
  for (j in 1:p){
    z = x.mat[,j] 
    z.sorted = z[order(z,decreasing = F)]
    z.p = (c(1:n)-0.5)/n 
    z.q = qnorm(p=z.p,mean=0,sd=1)
    points(z.sorted,z.q,pch=pch[j])
    if (line.add){
      if (line.method == "Q"){
        zz = quantile(z,line.probs,na.rm = TRUE)
        zz.q = qnorm(line.probs)
        slope = diff(zz.q)/diff(zz)
        int = zz.q[1] - slope*zz[1]
        abline(a = int,b =slope,lty=lty[j])
      }
      if (line.method == "MSD"){
        zz.m = median(z)
        zz.m.q = qnorm(0.5,mean=0,sd=1)
        zz.sd = abs(quantile(z.sorted,pnorm(-1))-quantile(z.sorted,pnorm(1)))/2
        zz = seq(min(x.at)*0.5,max(x.at)*1.5,length.out=100)
        zz.p=pnorm(zz,mean=zz.m,sd=zz.sd)
        zz.q=qnorm(zz.p,0,1)
        lines(zz,zz.q,lty=lty[j])
      }
    }
  }
  if (legend.add){
    if (line.add){
      legend(legend.location,legend=x.names,title="Variable",pch=pch,lty=lty,bty="n")
    }else{
      legend(legend.location,legend=x.names,title="Variable",pch=pch,bty="n")
    }
  }
}
########################################################################
## (Function) floor.p: round the values in its first argument to the specified number of decimal places (smaller)
## (Input)
# x: a numeric vector;
# digits: integer indicating the numbber of decimal places;
## (Output)
# y: a numeric vector;
floor.p = function(x,digits){
  num = 10^(-digits)
  y = floor(x/num)*num
  return(y)
}
## (Function) floor.p: round the values in its first argument to the specified number of decimal places (larger)
## (Input)
# x: a numeric vector;
# digits: integer indicating the numbber of decimal places;
## (Output)
# y: a numeric vector;
ceiling.p = function(x,digits){
  num = 10^(-digits)
  y = ceiling(x/num)*num
  return(y)
}
