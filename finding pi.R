find_pi = function(B=5000, seed=10, make_plot=TRUE){
  set.seed(seed)
  cols = c('#F8766D33', '#00BFC433')
  if (make_plot==TRUE){
    plot(NA, xlim = c(-1.1,1.1), ylim = c(-1.1,1.1), xlab = "x", ylab = "y", asp=1)
    make_square()
    grid()
    make_circle()
  }
  point = matrix(runif(2*B, -1, 1), B, 2)
  bools = matrix(rep(NA, B), B, 1)
  for (i in 1:B){
    x = point[i,1]
    y = point[i,2]
    if ((x^2 + y^2) <= 1){
      bools[i,1] = TRUE
    }else{
      bools[i,1] = FALSE
    }
    if(make_plot==TRUE){
      points(x,y,pch=16, col=cols[(bools[i,1]) + 1])
    }
  }
  hat_pi = 4*sum(bools)/B
  return(hat_pi)
}
