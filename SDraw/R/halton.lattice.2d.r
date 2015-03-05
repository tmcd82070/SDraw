halton.lattice.2d <- function(bbox=matrix(c(0,0,1,1),2), J=NULL, eta=c(1,1), bases=c(2,3)){
  # 
  # Return coordinates in a 2d Halton lattice, as a set of (x,y) vectors 
  #
  # Input: 
  #   bbox = bounding box for the Halton lattice. bbox[1,] = c(min, max) of dimension 1, bbox[2,] = c(min, max)
  #     of dimension 2, etc. Default is the unit box [0,1]X[0,1]
  #   J = 2X1 vector of base powers.  J[1] is for dimention 1, J[2] for dimension 2, etc.
  #     J determines the size and shape of the lowest level of Halton boxes. If J=NULL (the default), 
  #     J is choosen so that Halton boxes are as square as possible. 
  #   eta = 2X1 vector of number of points to add inside each Halton box.  e.g., if 
  #     eta = c(3,2), a small grid of 3 by 2 points is added inside each Halton box. eta[1] is for 
  #     dimension 1, eta[2] is for dimension 2, etc. 
  #   bases = 2X1 vector of Halton bases.  These must be co-prime. 

  

}