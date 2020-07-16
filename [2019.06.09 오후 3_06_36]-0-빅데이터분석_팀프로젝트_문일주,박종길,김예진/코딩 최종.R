library(HH)
library(car)
setwd("c:/kpu_data")
df <- read.csv("C:/kpu_data/인플루엔자.csv")
newdf <- na.omit(df)
options(scipen=999)
hist(newdf$인플루엔자)
plot(newdf)
summary(newdf)

#극한중심정리(정규성띄는지 확인)
Samples <- 0
for(i in 1:160){
  s <- sample(newdf$인플루엔자, 160, replace = T)
  Samples[i] <- mean(s)
}
hist(Samples)
qqnorm(Samples)
qqline(Samples)
shapiro.test(Samples)

#로그를 씌우면 정규성을 띄는가?
hist(log(newdf$인플루엔자))
qqnorm(log(newdf$인플루엔자))
qqline(log(newdf$인플루엔자))


#로그로 바꿧을때
s <- log(newdf$인플루엔자)
s

#지수화 했을때
d <- exp(s)
d


#기술통계
str(newdf)
attach(newdf)
range(log(인플루엔자)) #범위
range(입국)
range(교통량)
range(도시가스)
range(구글검색량)
range(감기가능지수)
range(PM10)
summary(log(인플루엔자))
summary(입국)
summary(교통량)
summary(도시가스)
summary(구글검색량)
summary(감기가능지수)
summary(PM10)
var(log(인플루엔자)) #분산
var(입국)
var(교통량)
var(도시가스)
var(구글검색량)
var(감기가능지수)
var(기침)
var(PM10)
sd(log(인플루엔자)) #표준편차
sd(입국)
sd(교통량)
sd(도시가스)
sd(구글검색량)
sd(감기가능지수)
sd(기침)
sd(PM10)

#변수 상관관계
cor(log(newdf$인플루엔자), newdf$PM10)
cor(log(newdf$인플루엔자), newdf$입국)
cor(log(newdf$인플루엔자), newdf$교통량)
cor(log(newdf$인플루엔자), newdf$도시가스)
cor(log(newdf$인플루엔자), newdf$구글검색량)
cor(log(newdf$인플루엔자), newdf$감기가능지수)



#변수 선택법
full <- (lm(log(인플루엔자) ~ 입국+교통량+도시가스+구글검색량+감기가능지수+PM10, data=newdf))
null <-(lm(log(인플루엔자) ~1, data=newdf))

#전진 선택법
forward <- step(null, direction = "forward", scope = list(lower=null, upper=full))
summary(forward)
AIC(forward)
BIC(forward)

#후진 제거법
backward <- step(full, direction = "backward", scope = list(lower=null, upper=full))
summary(backward)
AIC(backward)
BIC(backward)


#단계적 방법
both <- step(null, direction = "both", scope = list(lower=null, upper=full))
summary(both)
AIC(both)
BIC(both)
anova(both)
vif(both)


#예측
pred <- predict(both, newdata = newdf)
pred <- as.data.frame(pred)
tmp <- cbind(log(newdf$인플루엔자), pred)
colnames(tmp) <- c("real", "pred")
residual <- tmp$real - tmp$pred 
residual
plot(log(newdf$인플루엔자), type="l", col="red")
lines(tmp$pred, col="blue", type="l")

#회귀분석 가설 검정
par(mfrow = c(2,2))
plot(both)

#가설 검정 심화
##QQPlot
qqPlot(both, labels=row, names(newdf), id.method="identify", simulate = TRUE, main = "Q-Q Plot")


#선형성 검정
crPlots(both)

#잔차의 등분산성 검정
ncvTest(both)

#잔차의 정규성 검정
shapiro.test(residual)

#잔차의 독립성 검정
durbinWatsonTest(both)

#등분산성 plot으로 보여줌
spreadLevelPlot(both)

#log 씌운 회귀식
ylog <- 3.1884403 + (-0.2375559) * (-8.755) + 0.0299718 * 9.809 + 0.0144590 * 4.503 + 0.0042935 * 4.783 + 0.0003219 * 3.869
ylog

#회귀식에 t-value 곱한값(지수변환)
y <- exp(3.1884403) + (-0.2375559) * (-8.755) + 0.0299718 * 9.809 + 0.0144590 * 4.503 + 0.0042935 * 4.783 + 0.0003219 * 3.869
y




