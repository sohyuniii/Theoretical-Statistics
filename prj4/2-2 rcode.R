rm(list=ls())
library(dplyr)
setwd("C:/Users/user/Desktop/ewha2/mathmatical2/project4")
kospi = read.csv("kospi.csv")
colnames(kospi) = c("date", "St")  
kospi$St = as.numeric(gsub(",", "", kospi$St))
kospi$date = as.Date(kospi$date)
kospi_2016 = kospi[c(1:246),]
kospi_2017 = kospi[c(247:489),]

r = log(1.05)
mu  =  0.06168493 ; sig = 0.1189424
So = 2026.46 ; K = 2026.16

### 1)
St = kospi_2016[,2]
set.seed(1234567)
M = 100000
z = rnorm(M)

# method1 : Monte-Carlo Simulation
ftn.monte = function(t){ 
  s = So*exp((r-sig^2/2)*t+sig*sqrt(t)*z)
  ct = exp(-r*t)*sum(ifelse(s>K, s-K, 0))/M
  pt = exp(-r*t)*sum(ifelse(s<K, K-s, 0))/M
  return(c(ct,pt))
}
# method2 :Black-Scholes-Merton
ftn.black = function(t){
  d1 = (log(So/K)+(r+sig^2/2)*t)/(sig*sqrt(t))
  d2 = d1-sig*sqrt(t)
  ct = exp(-r*t) *(So*exp(r*t)*pnorm(d1) - K*pnorm(d2))
  pt = exp(-r*t) *(-So*exp(r*t)*pnorm(-d1) + K*pnorm(-d2)) 
  return(c(ct,pt))
}

t = c(1/12,1/4,1/2,3/4,1)

call.price= data.frame(t=c(1,3,6,9,12),monte = numeric(5), black = numeric(5))
for (i in 1:5){
  call.price[i,2] = ftn.monte(t[i])[1]
  call.price[i,3] = ftn.black(t[i])[1]
  
}
put.price= data.frame(t=c(1,3,6,9,12),monte = numeric(5), black = numeric(5))
for (i in 1:5){
  put.price[i,2] = ftn.monte(t[i])[2]
  put.price[i,3] = ftn.black(t[i])[2]
  
}

call.price
put.price

### 2)
St = c(2067.57,2160.23,2391.79,2394.47,2467.49)
ct = call.price[,2]
pt = put.price[,2]
ftn.portpolio = function(c,p){
  total = c*ct+p*pt
  net.price = (c*ifelse(St>K, St-K, 0)+p*ifelse(St<K, K-St, 0))-total
  profit = (net.price/total)*100
  return(profit)
}

data.frame(t=c(1,3,6,9,12),profit=100*(St-So)/So,portfolio.a=ftn.portpolio(150,50),
           portfolio.b=ftn.portpolio(100,100), portfolio.c=ftn.portpolio(50,150))
