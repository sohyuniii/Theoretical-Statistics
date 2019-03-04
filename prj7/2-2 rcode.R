rm(list=ls())
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(survival)
library(gam)

setwd("C:/Users/user/Desktop/ewha2/mathmatical2/project7")
cancel <- read_excel("cancel.xlsx", sheet = 1)

# delta 종속변수(해지:1/유지:0)
# x1 가입연령
# x2 납입방법(1:월납/2:3개월납/3:6개월납/4:연납)
# x3 납입기간/년
# x4 수금방법(1:방문,2:자동이체,3:지로,4:직납,5:카드납)
# x5 보험료(1회 납입시의 보험료)
# x6 부활유무(유:1/무:0)
# x7 계약일자
# x8 지금만기일자
# x9 최종납입횟수
# x10 상품중분류
# x11 상품소분류
# x12 보험기간
# x13 월차
# x14 연체여부
# x15 연체율

cancel$x2 <- as.factor(cancel$x2)
cancel$x4 <- as.factor(cancel$x4)
cancel$x6 <- as.factor(cancel$x6)
cancel$x10 <- as.factor(cancel$x10)
cancel$x11 <- as.factor(cancel$x11)
cancel$x14 <- as.factor(cancel$x14)

for (i in 1:nrow(cancel)) {
  if (substr(cancel[i,"x7"], 5, 8) == "0229") { cancel[i,"x7"] <- paste(substr(cancel[i,"x7"], 1, 7), "8", sep = "") }
  if (substr(cancel[i,"x8"], 5, 8) == "0229") { cancel[i,"x8"] <- paste(substr(cancel[i,"x8"], 1, 7), "8", sep = "") }
}

cancel <- cancel %>% mutate(x7 = as.Date(paste(substr(x7, 1, 4), substr(x7, 5, 6), substr(x7, 7, 8), sep = "/")),
                             x8 = as.Date(paste(substr(x8, 1, 4), substr(x8, 5, 6), substr(x8, 7, 8), sep = "/")))

train <- cancel %>% filter(train == 1) 
test <- cancel %>% filter(train == 2) 


############## COX ##############

cox1 <- coxph(Surv(x9, delta) ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11, train)
cox1.step <- coxph(Surv(x9, delta) ~ x1 + x6 + x7 + x9 + x10, data = train) #stepAIC(cox1, alpha=0.15) = 1654.04
cox3 <- coxph(Surv(x9, delta) ~ log(x1) + x2 + log(x5) + x7 + log(x13)*x14 + x6*x15, train)  # AIC : 1454.91
cox4 <- coxph(Surv(x9, delta) ~ x2 + log(x5) + x7 + log(x13) + 
                x14 + x6 + x15 + x1 + x8 + x12 + log(x13):x14 + x6:x3 + x3:x5 + 
                x7:x6 + x7:x15 + x15:x8 + x14:x15, data = train)  # AIC : 1375.047
cox4 <- coxph(Surv(x9, delta) ~ x2 + log(x5) + x14 + x1 + log(x13)*x14 + x6*x3 + x7*x6 + x7*x15 + x15*x8 + x14*x15, data = train)


cox.fit <- cox3

cox.prob <- 1-exp(-predict(cox.fit,type="expected"))
cox.top500 <- which(cox.prob >= sort(cox.prob, decreasing = T)[500])
cox.table <- cbind(train, cox.prob) %>% arrange(desc(cox.prob))
sum(cox.table[1:500, "delta"])/500 / (sum(cox.table[, "delta"])/5000)


############## GLM ##############

glm.fit <- glm(delta ~ x1 + x13 + x6 + x7 + x8 + x12 + x14 + x15 + log(x5) + x9 + 
                 x1:x13 + x1:x6 + x13:x6 + x13:x8 + x13:x12 + x13:log(x5) + x13:x9 + 
                 x6:x7 + x6:x12 + x7:x8 + x7:x14 + x7:x15 + x7:x9 + x8:x15 + 
                 x8:log(x5) + x8:x9 + x12:log(x5) + x14:x15 + x14:log(x5) + x14:x9 + x15:x9 + 
                 log(x5):x9, family=binomial(link='logit'), data=train)

glm.prob <- predict.glm(glm.fit, type='response', na.action = na.pass, newdata=train)

lm <- lm(glm.prob~-1+cox.prob)
summary(lm)
anova(lm)
gg=glm.prob
cc =cox.prob
m = data.frame(gg,cc)
m = m %>% filter(cc!=0) %>% mutate(gl = log(gg),cl=log(cc)+log(3))
lm = lm(cl~-1+gl,data=m)    
