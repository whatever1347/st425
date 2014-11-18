inequality.Sim1 <- function(n){
  a <- runif(n,0,1); b <- runif(n,0,1); #generating random numbers from U(0,1)
  diffOrder <- runif(n); diff <- runif(n); sDiff <- runif(n); sDiffOrder <- runif(n)  
  #initializing dimension
  aOrder <- sort(a); bOrder <- sort(b);
  for(i in 1:n){
    diffOrder[i]=abs(aOrder[i]-bOrder[i]); #part 1
    diff[i] = abs(a[i]-b[i]);
    sDiff = (a[i]-b[i])^2; #part 2
    sDiffOrder = (aOrder[i]-bOrder[i])^2
  };
  C1 = max(diffOrder)/max(diff);
  C2 = sum(sDiffOrder)/sum(sDiff);
  return(c(C1,C2))
}
