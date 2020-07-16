#week05-1 sample_dust.csv 파일을 열고 PM25의 평균, 중앙값, IQR, 사분위수를 구하시오 
#단, 결측 값의 경우 제거하고 진행한다

setwd("c:/kpu_data") #작업경로 설정
DF <- read.csv("dust_sample.csv") #csv 포멧을 읽어들여 DF에 할당
DF<-na.omit(DF) #단순결측제거 : 하나라도 행에 결측치 존재시 행을 날림

mean(DF$PM25) #평균 = mean(DF$PM25,na.rm=T)
median(DF$PM25) #중앙값 =median(DF$PM25,na.rm=T)
IQR(DF$PM25) #IQR 구하기(Q1과 Q3의 범위) = IQR(DF$PM25, na.rm=T)
quantile(DF$PM25) #사분위 수 = quantile(DF$PM25, na.rm=T)

#평균, 중앙값, Q1, Q3 한번에 보기
summary(DF$PM25)

#week05-2 PM25의 히스토그램, Box plot 도형을 그리시오
hist(DF$PM25) #히스토그램
boxplot(DF$PM25) #Box plot

#week05-3 질소산화물(NO2)와 초미세먼지(PM25)간의 그래프, 공분산, 상관계수를 구하시오
plot(DF$NO2,DF$PM25) #두 변수를 그래프로 보여줌
cov(DF$NO2,DF$PM25) #공분산
cor(DF$NO2,DF$PM25) #상관계수
cor.test(DF$NO2,DF$PM25) #p-value 함께 알려줌

