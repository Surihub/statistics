#####################################
############# 패키지 ################
#####################################

# Tidyverse 패키지 설치
install.packages("palmerpenguins")
install.packages('GGally')
install.packages("corrplot")
install.packages("MatchIt")
install.packages("Ismeans")
install.packages('kml')

# 필요한 라이브러리 로드
library(dplyr)
library(ggplot2)
library(GGally)
library(corrplot)
library(palmerpenguins)
library(kml)

#####################################
############# EDA ###################
#####################################

# 데이터 불러오기(펭귄 데이터)
data(package = 'palmerpenguins')

# 데이터 이름 바꾸기
colnames(penguins) = c('종', '서식지', '부리길이', '부리폭', '날개길이', '무게', '성별', '연도')

# 데이터 확인하기
View(penguins)

# 전체적인 EDA, 시각화
ggpairs(penguins)

# EDA(일변량, 수치형)
hist(penguins$부리폭)
mean(penguins$부리폭, na.rm=TRUE) # 결측치가 존재하면 평균이 NA로 나옴. na.rm=TRUE로 결측치 제거
sd(penguins$부리폭, na.rm=TRUE)

# EDA(이변량, 명-수치형)
aggregate(penguins$무게, by = list(penguins$종), FUN = mean, na.rm=TRUE)
aggregate(penguins$무게, by = list(penguins$종), FUN = median, na.rm=TRUE)
aggregate(penguins$무게, by = list(penguins$종), FUN = sd, na.rm=TRUE)
boxplot(무게~종, data = penguins, xlab = '종', ylab = '무게')

# EDA(일변량, 명목형)
table(penguins$종)
prop.table(table(penguins$종))
barplot(table(penguins$종), main = "species ratio", xlab = "종", ylab = "ration")

#####################################
########### 통계적 처리 #############
#####################################

# 정규성 검정, 0.05보다 크거나 같으면 정규분포따른다. 0.05보다 작으면 변환 
shapiro.test(log(penguins$무게))
shapiro.test(penguins$부리폭)

# 분산 다른 경우(성별에 따른 무게)
ggplot(data = penguins, aes(x=성별, y=무게, group = 성별))+geom_boxplot()
var.test(penguins$무게~penguins$성별)

# 분산 같은 경우(종에 따른 무게)
df = penguins[penguins$종 %in% c("Adelie", "Chinstrap"), ]
ggplot(data = df, aes(x=종, y=무게, group = 종))+geom_boxplot()
var.test(df$무게~df$종)

# t-test:분산 같은 경우
t.test(무게~종, var.equal=TRUE, data = df)

# t-test:분산 다른 경우
t.test(무게~성별, var.equal=FALSE, data = penguins)
t.test(무게~성별, data = penguins) # false일 경우 var.equal 생략가능
t.test(무게~성별, var.equal=TRUE, data = penguins)

# 비율 비교
fisher.test(penguins$종, penguins$성별) # 대상수가 10개 미만일 때. 10개 이상이면 chisq.test()
fisher.test(penguins$성별, penguins$종) # 대상수가 10개 미만일 때. 10개 이상이면 chisq.test()
fisher.test(penguins$종, penguins$서식지) # 대상수가 10개 미만일 때. 10개 이상이면 chisq.test()

# 상관분석 & 산점도 그리기
corrplot(cor(na.omit(penguins[c('부리길이', '부리폭', '날개길이', '무게')])), method = 'circle')
plot(penguins$날개길이, penguins$무게)

ggplot(penguins, aes(x = 날개길이, y = 무게)) + geom_point() + 
  labs(title = "Scatter Plot of Bill Length vs. Bill Depth", x = "부리길이", y = "부리폭")
cor.test(penguins$날개길이, penguins$무게)

# 회귀분석
model = lm(무게 ~ 날개길이 , data = penguins)
summary(model)
mean(penguins$무게, na.rm=TRUE)
mean(penguins$날개길이, na.rm=TRUE)

# 신뢰구간
exp(confint(model))

# 다중 선형 회귀 모델 적합
model <- lm(무게 ~ 부리길이+부리폭 , data = penguins)
summary(model)

# 모델 요약 출력
summary(model)

# 성향점수 매칭법
penguins_c = na.omit(penguins)
attach(penguins_c)


# ......나중에

# 군집분석 k-means
# kml:k means for longitudinal data


# ......나중에

# 모평균 신뢰구간 추정
data_m = na.omit(penguins$무게)

# 신뢰수준 설정
confidence_level = 0.95

# 모평균 추정
t_critical = qt((1 + confidence_level) / 2, df = length(data_m) - 1)
margin_of_error = t_critical * (sd(data_m) / sqrt(length(data_m)))
confidence_interval = c(mean(data_m) - margin_of_error, mean(data_m) + margin_of_error)
confidence_interval

# 모비율 신뢰구간 추정
data_p = na.omit(penguins$성별)
data_count = table(data_p)

# 신뢰수준 설정
confidence_level <- 0.95

# 모비율 추정
num_success <- data_count["female"]
sample_size <- sum(data_count)
z_critical <- qnorm((1 + confidence_level) / 2)
p_hat <- num_success / sample_size
margin_of_error <- z_critical * sqrt((p_hat * (1 - p_hat)) / sample_size)
confidence_interval <- c(p_hat - margin_of_error, p_hat + margin_of_error)
confidence_interval



t_critical = qt((1 + confidence_level) / 2, df = length(data_p) - 1)
margin_of_error = t_critical * (sd(data_p) / sqrt(length(data_p)))
confidence_interval = c(mean(data_p) - margin_of_error, mean(data_p) + margin_of_error)
confidence_interval
