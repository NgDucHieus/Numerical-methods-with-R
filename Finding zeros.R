#finding zeros of polymial
polyroot(c(1, -3, 2)) # 1-3x+2x^2=0

sol = polyroot(c(1,2i,3 - 7i))

p = function(x) 1 +2i*x + (3 -7i)*x^2
p(sol[1])
p(sol[2])
#bisection method
bisectionroot = function(f,xmin,xmax,tol = 1e-5)
{
  a = xmin
  b = xmax
  if (a>=b)
  {
    cat("Error:xmin > xmax")
    return(NULL)
  }
  if(f(a) == 0) 
  {
    return(a)
  }
  else if (f(b) == 0)
  {
    return(b)
  }
  else if (f(a)*f(b) > 0)
  {
    cat("Error: f(xmin) and f(xmax) of same sign \n")
    return(NULL)
  }
  #if inputs OK, converge to root 
  iter = 0
  while((b-a)>tol)
  {
    c = (a+b)/2
    if(f(c) ==0)
    {
      return(c)
    }
    else if (f(a)*f(c) <0)
    {
      b=c
    }
    else{
      a = c
    }
    iter = iter + 1
  }
  return (c((a+b)/2,iter,(b-a))) #root, iterations, precision
  
}
f = function(x) x^3 -sin(X)^2
bisecroot = bisectionroot(f,0.5,1)






