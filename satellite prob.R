library(stat297)

get_coords=function(dists, theta=c(0,0,0)){
  coords = matrix(c(-300, 300, 300, 300, 0, -300), ncol=2, byrow=TRUE)
  ((coords[1,1]-theta[1])^2+(coords[1,2]-theta[2])^2-(dists[1]-theta[3])^2)^2 + 
  ((coords[2,1]-theta[1])^2+(coords[2,2]-theta[2])^2-(dists[2]-theta[3])^2)^2 + 
  ((coords[3,1]-theta[1])^2+(coords[3,2]-theta[2])^2-(dists[3]-theta[3])^2)^2
}

get_position = function(dists){
  if (is.matrix(dists)){
    coords = matrix(rep(NA, 2*dim(dists)[1]), ncol=2)
    for (i in 1:dim(dists)[1]){
      pair = optim(c(0,0,0), get_coords, dists=c(dists[i,1], dists[i,2], dists[i,3]))$par[1:2]
      coords[i,1] = pair[1]
      coords[i,2] = pair[2]
    }
  }else{
     coords = matrix(c(NA,NA), ncol=2)
     pair = optim(c(0,0,0), get_coords, dists=dists)$par[1:2]
     coords[1,1] = pair[1]
     coords[1,2] = pair[2]
  }
  class(coords) = 'calc'
  return(coords)
}

summary.calc = function(mat){
  for (i in 1:dim(mat)[1]){
    print('## The estimated position is: ')
    print(paste('## X = ', mat[1]))
    print(paste('## Y = ', mat[2])) 
  }
}

plot.calc = function(mat){
  plot(NA, xlim = c(-315,315), ylim = c(-315,315), xlab = "x", ylab = "y", asp=1)
  grid()
  make_circle(radius=200, col='black', fill='#D3D3D3')
  make_circle(center=c(-300,300), radius=15, col='black', fill='#F8756C')
  make_circle(center=c(300,300), radius=15, col='black', fill='#00BA38')
  make_circle(center=c(0,-300), radius=15, col='black', fill='#619CFF')
  for (i in 1:dim(mat)[1]){
    points(mat[i,1], mat[i,2], col='#ED9A09', pch=4, lwd=2) 
  }
}
