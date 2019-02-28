rm(list=ls())
library(dplyr) ; library(ggplot2)
setwd("C:/Users/user/Desktop/ewha2/mathmatical2/project1")

### Functions
ols_bass = function(data,n){
  ols = lm(St~Yt_1+I(Yt_1^2),data=data[1:n,]) 
  a = ols$coef[1] ; b = ols$coef[2] ; c = ols$coef[3]
  m = (-sqrt(b^2-4*a*c)-b)/(2*c) ; q=-m*c ; p=q-b
  total = data$Yt[length(data$Yt)]
  se = 100*(m-total)/total
  return(c(m,se))
}

ols_logis = function(data,n){
  ols = lm(St~-1+Yt_1+I(Yt_1^2),data=data[1:n,])
  a = ols$coef[1] ; b = ols$coef[2]
  q = a ; m = -q/b
  total = data$Yt[length(data$Yt)]
  se = 100*(m-total)/total
  return(c(m,se))
}

ols_gum = function(data,n){
  ols = lm(St~-1+Yt_1+I(Yt_1*log(Yt_1)),data=data[1:n,]) 
  a = ols$coef[1] ; b = ols$coef[2] 
  q = -b ; m = exp(-a/b)
  total = data$Yt[length(data$Yt)]
  se = 100*(m-total)/total
  return(c(m,se))
}

ols_exp = function(data,n){
  ols = lm(St~Yt_1,data=data[1:n,]) 
  a = ols$coef[1] ; b = ols$coef[2] 
  p = -b  ; m = a/p 
  total = data$Yt[length(data$Yt)]
  se = 100*(m-total)/total
  return(c(m,se))
}

# 신과 함께 - 죄와 벌
data1 = read.csv("AlongWithTheGods.csv",stringsAsFactors = F) 
data1$date=as.Date(data1$date)
data1 = data1[-c(1:6),]

### Time Series plot
ggplot(data1,aes(date,St))+geom_line()+ggtitle("Time Series of S(t) (일별 관객수)")+
  theme(plot.title = element_text(color="darkblue", size=14, face="bold",hjust=0.5))
ggplot(data1,aes(date,Yt))+geom_line()+ggtitle("Time Series of Y(t) (누적 관객수)")
ggplot(table1,aes(date,St))+geom_line()+ggtitle("Time Series of S(t) (일별 관객수)")

### Find A Best Model
holidays <- as.Date(c("2017-12-25", "2018-01-01", "2018-02-15", "2018-02-16", "2018-02-17", "2018-02-18",
                      "2018-03-01", "2018-05-05", "2018-05-07", "2018-05-22", "2018-06-06", "2018-06-13", "2018-08-15"))
table1 <- data.frame()
for (i in 1:length(data1$St)) {
  if (weekdays(data1$date[i]) %in% c("토요일", "일요일") | data1$date[i] %in% holidays) { 
    table1 <- rbind(table1, data.frame(date = rep(data1[i,1], 2), St = rep(data1[i,2]/2, 2))) }
  else { table1 <- rbind(table1, data1[i,1:2]) }
}

for (i in 2:length(table1$St)){
  table1$Yt[1] = table1$St[1]
  table1$Yt[i] = table1$St[i]+table1$Yt[i-1]
  table1$Yt_1[1] = NA
  table1$Yt_1[i] = table1$Yt[i-1]
  table1$t = seq(1,length(table1$St))
}
data.frame(n = c(7,14,28),
           Bass = c(ols_bass(table1,7)[2],ols_bass(table1,14)[2] ,ols_bass(table1,28)[2]),
           Logistic = c(ols_logis(table1,7)[2],ols_logis(table1,14)[2],ols_logis(table1,28)[2]),
           Gumbel = c(ols_gum(table1,7)[2],ols_gum(table1,14)[2],ols_gum(table1,28)[2]),
           Exponential = c(ols_exp(table1,7)[2],ols_exp(table1,14)[2],ols_exp(table1,28)[2]))
           
### Q-Q plot
total1=table1$Yt[length(table1$Yt)]
bass = function(data,n){
  ols = lm(St~Yt_1+I(Yt_1^2),data=data[1:n,]) 
  a = ols$coef[1] ; b = ols$coef[2] ; c = ols$coef[3]
  m = 11211221 ; q=-m*c ; p=q-b
  return(c(p,q))
}
p1=bass(table1,155)[1] ; q1=bass(table1,155)[2] ; k1=p1+q1 ; c1=q1/p1 

qqtable1 = table1 %>% select(t,Yt) %>% filter(t<=94) %>%
  mutate(Ur=Yt/(total1+1),quan=(1/k1)*log((1+c1*Ur)/(1-Ur)))
ggplot(qqtable1,aes(quan,t))+geom_point()+geom_smooth(method="lm")+
  ggtitle("신과함께 :  Bass Q-Q plot")




# 어벤져스 : 인피니티 워
data2 = read.csv("Avengers.csv")
data2$date=as.Date(data2$date)
data2 = data2[-1,]

### Time Series plot
plot(data2$date,data2$St,type="l",lwd=2,col="dark green",xlab="t (시간)",ylab="",main="S(t) (일별 관객수)")
plot(data2$date,data2$Yt,type="l",lwd=2,col="Salmon",xlab="t (시간)",ylab="",main="Y(t) (누적 관객수)")

### Find A Best Model
table2 <- data.frame()
for (i in 1:length(data2$St)) {
  if (weekdays(data2$date[i]) %in% c("토요일", "일요일") | data2$date[i] %in% holidays) { 
    table2 <- rbind(table2, data.frame(date = rep(data2[i,1], 2), St = rep(data2[i,2]/2, 2))) }
  else { table2 <- rbind(table2, data2[i,1:2]) }
}

for (i in 2:length(table2$St)){
  table2$Yt[1] = table2$St[1]
  table2$Yt[i] = table2$St[i]+table2$Yt[i-1]
  table2$Yt_1[1] = NA
  table2$Yt_1[i] = table2$Yt[i-1]
  table2$t = seq(1,length(table2$St))
}
data.frame(n = c(7,14,28),
           Bass = c(ols_bass(table2,7)[2],ols_bass(table2,14)[2] ,ols_bass(table2,28)[2]),
           Logistic = c(ols_logis(table2,7)[2],ols_logis(table2,14)[2],ols_logis(table2,28)[2]),
           Gumbel = c(ols_gum(table2,7)[2],ols_gum(table2,14)[2],ols_gum(table2,28)[2]),
           Exponential = c(ols_exp(table2,7)[2],ols_exp(table2,14)[2],ols_exp(table2,28)[2]))

### Q-Q plot
total2=table2$Yt[length(table2$Yt)]
bass = function(data,n){
  ols = lm(St~Yt_1+I(Yt_1^2),data=data[1:n,]) 
  a = ols$coef[1] ; b = ols$coef[2] ; c = ols$coef[3]
  m = 11211221 ; q=-m*c ; p=q-b
  return(c(p,q))
}
p2=bass(table2,155)[1] ; q2=bass(table2,155)[2] ; k2=p2+q2 ; c2=q2/p2 
qqtable2 = table2 %>% select(t,Yt) %>% filter(t<=94) %>%
  mutate(Ur=Yt/(total2+1),quan=(1/k2)*log((1+c2*Ur)/(1-Ur))) 
ggplot(qqtable2,aes(quan,t))+geom_point()+geom_smooth(method="lm")+
  ggtitle("Avengers :  Bass Q-Q plot")

# 너의 결혼식
data3 = read.csv("wedding.csv",stringsAsFactors = F)
data3$date=as.Date(data3$date)
data3 = data3[-c(1:14),]

ggplot(data3,aes(date,St))+geom_line()+ggtitle("Time Series of S(t) (일별 관객수)")
ggplot(data3,aes(date,Yt))+geom_line()+ggtitle("Time Series of Y(t) (누적 관객수)")

### Find A Best Model
table3 <- data.frame()
for (i in 1:length(data3$St)) {
  if (weekdays(data3$date[i]) %in% c("토요일", "일요일")) { 
    table3 <- rbind(table3, data.frame(date = rep(data3[i,1], 2), St = rep(data3[i,2]/2, 2))) }
  else { table3 <- rbind(table3, data3[i,1:2]) }
}

for (i in 2:length(table3$St)){
  table3$Yt[1] = table3$St[1]
  table3$Yt[i] = table3$St[i]+table3$Yt[i-1]
  table3$Yt_1[1] = NA
  table3$Yt_1[i] = table3$Yt[i-1]
  table3$t = seq(1,length(table3$St))
}
data.frame(n = c(7,14,28),
           Bass = c(ols_bass(table3,7)[2],ols_bass(table3,14)[2] ,ols_bass(table3,28)[2]),
           Logistic = c(ols_logis(table3,7)[2],ols_logis(table3,14)[2],ols_logis(table3,28)[2]),
           Gumbel = c(ols_gum(table3,7)[2],ols_gum(table3,14)[2],ols_gum(table3,28)[2]),
           Exponential = c(ols_exp(table3,7)[2],ols_exp(table3,14)[2],ols_exp(table3,28)[2]))
total3=ols_logis(table3,length(table3$date))[1]
qqtable3 = table3 %>% select(t,Yt) %>% 
  mutate(Ur=Yt/(total3+1),quan=log(Ur/(1-Ur))) 
ggplot(qqtable3,aes(quan,t))+geom_point()+geom_smooth(method="lm")+
  ggtitle("너의 결혼식 : Logistic Q-Q plot") 

