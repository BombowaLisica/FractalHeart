# setwd()

heart <- function(t) {
  x <- 16*sin(t)^3
  y <- 13*cos(t) - 5*cos(2*t) - 2*cos(3*t)-cos(4*t)
  return(list(x=x, y=y))
}



plotHeart <- function(min=-100        # min of t
                      , max=100       # max of t
                      , count=600     # number of points
                      , matsize=400   # picture size
                      , col="maroon"
                      , bg="bisque"
                      , pch=20
                      , cex=0.8
                      , type="l"
                      , title=NULL){
  v <- seq(min, max, length.out = count)
  x <- numeric()
  y <- numeric()
  n <- 1
  for (i in v){
    h <- heart(i)
    x[n] <- h$x
    y[n] <- h$y
    n <- n + 1
  }
  
  par(bg=bg)
  plot(x, y, axes=FALSE, xlab="", ylab="", col=col, pch=pch, cex=cex, type=type, main=title)
  
  # resizing to fit picture size
  x_d <- max(x) - min(x)
  x_s <- min(x) + x_d/2
  x_resise <- x*(matsize/x_d) + (matsize/2 - x_s)
  y_d <- max(y) - min(y)
  y_s <- min(y) + y_d/2
  y_resise <- y*(matsize/y_d) + (matsize/2 - y_s)
  
  M <- matrix(0, nrow=matsize, ncol=matsize)
  for(j in 1:matsize){
    M[floor(x_resise[j]), floor(y_resise[j])] <- 1
  }
  
  # deleting some points in the center
  for(k in 1:matsize){
    if(rowSums(M)[k] > 50){ # 0.1*count could be better
      M[k,] <- 0
    }
    if(colSums(M)[k] > 50){ # 0.1*count could be better
      M[,k] <- 0
    }
  }
  
  return(list(x=x, y=y, x_resise=x_resise, y_resise=y_resise, M=M))
}





###################################################################################

# based on: https://www.codeproject.com/Articles/1195034/A-Few-Approaches-to-Generating-Fractal-Images-in-R

plotmat <- function(M                # matrix returned from plotHeart
                    , matsize=400    # picture size
                    , col="maroon"   #
                    , export=0       # if 1, export to png
                    , exName="heart" # name of exported file
                    , bg="bisque"
                    , pch=20
                    , cex=1.0
                    , title=NULL) {
  m = nrow(M);
  d = 0;
  X = NULL;
  Y = NULL;
  
  # Building X and Y arrays for plotting from not equal to zero values in M
  for (i in 1:m) {
    for (j in 1:m) {
      if(M[i,j] == 0){
        next
      } else {
        d=d+1;
        X[d]=i;
        Y[d]=j
      } 
    }
  };
  cat(" *** Matrix(", m,"x",m,")", d, "DOTS\n");
  # Plotting
  par(bg=bg)
  plot(X, Y, axes=FALSE, xlab="", ylab="", col=col, pch=pch, cex=cex, main=title)
  # export to file
  if(export == 1){
    fn = paste0(exName, ".png")
    # Writing png-file
    dev.copy(png, filename=fn, width=matsize, height=matsize);
    # Cleaning
    dev.off()
    # graphics.off();
  }
}


gpBrownianTree1 <- function(M                 # matrix returned from plotHeart
                            , n               # number of dots 
                            , col="maroon"    
                            , export=0        # if 1, export to png
                            , exName="heart"  # name of exported file
                            , pch=20
                            , cex=0.8
                            , title=NULL) {
  m <- nrow(M)
  cat(" *** START:", date(),"m=",m,"n=",n,"col=",col,"\n");
  # Main loops: Generating matrix M
  for (i in 1:n) {
    if(i >= 1) {
      x <- sample(1:m, 1, replace=F)
      y <- sample(1:m, 1, replace=F)
    }
    while(1) {
      ox <- x;
      oy <- y;
      x <- x + sample(-1:1, 1, replace=F);
      y <- y + sample(-1:1, 1, replace=F);
      if(x <= m && y <= m && x > 0 && y > 0 && M[x,y]){
        if(ox <= m && oy <= m && ox > 0 && oy > 0){
          M[ox,oy] <- 1;
          break
        }
      }
      if(!(x <= m && y <= m && x > 0 && y > 0)) {break}
    }
  }
  plotmat(M, col=col, export=export, exName=exName, matsize=m, pch=pch, cex=cex, title=title);
  return(M)
  cat(" *** END:",date(),"\n");
}



####################################################################################


for(i in seq(100,400,50)){
  par(mfrow=c(1,2))
  p <- plotHeart(min=-100, max=100, count=i, matsize=400, col="maroon", bg="bisque", pch=20, cex=-0.1, type="l", title=paste("count = ", i))
  plotmat(p$M, export=0, exName = "heart01")
}


p <- plotHeart(min=-100, max=100, count=300, matsize=400, col="maroon", bg="bisque", pch=20, cex=-0.1, type="l")
plotmat(p$M)
par(mfrow=c(1,1))
gpBrownianTree1(p$M, 40000, "maroon", 0, "", 20, 0.8, NULL)



#################################################################

t = seq(-pi, pi, 0.00001)
d <- data.frame(t = t, x = heart(t)$x, y = heart(t)$y)

with(d, plot(x, y, type = "l", axes = FALSE, xlab = "", ylab = ""
             , xlim = c(-17, 17), ylim = c(-19, 13)
             , col = "maroon", lwd = 2
             , yaxs = "i"))
par(xaxp = c(-18, 18, 36)
    , yaxp = c(-19, 13, 32)
    , tck = 0.005)
axis(1, pos=0, label = FALSE)
axis(2, pos=0, label = FALSE, yaxs = "i")

xtext = expression(paste("x = 16sin" ^ "3", "(t)"))
ytext = expression(paste("y = 13cos(t) - 5cos(2t) - 2cos(3t) - cos(4t)"))

fonts <- list(mono = "Consolas")
par(family = "mono")
mtext(xtext, side=1, line=0.5, at=-15)
mtext(ytext, side=1, line=1.5, at=-8)


