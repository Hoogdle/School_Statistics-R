# 2024-05-30 목요일

# 구간 추정
# sigma를 아는 경우

data = c(89.6,88.8,86.3,82.6,84.7,85.8,80.5,84.3,91.3,86.7,87.6,85.2,78.5,85.3,81.5,83.9,86.9,82.0,83.0,85.5)

n = length(data) # 자료수
xbar = mean(data)  # 표본평균
sigma = sqrt(3.1) # 표준오차
se = sigma/sqrt(n)

error = qnorm(0.975)*se

LCL = xbar - error
UCL = xbar + error

LCL #[1] 84.22836
UCL #[1] 85.77164


##### 9-4 sigma를 아는 경우 ######

mu0 = 87 # H0 : mu=87, H1 : mu<87
z0 = (xbar-mu0)/se # 검정통계량 Z = (xbar-mu0)/(sigma/sqrt(n))
pval = pnorm(z0) # z0가 가지는 확률 (P(Z<z0))
z0 #[1] -5.080005
pval  # [1] 1.887124e-07 # 유의 수준 > pvalue 이면 귀무가설(H0) 기각

# 문제에서 주어진 a = 0.05 이고 a > p-value 이므로 귀무 가설을 기각한다.
# 즉 대립가설을 받아들일 만한 충분한 근거를 가지고 있다.





####### sigma를 모르는 경우 => T #######

s = sd(data)
se = s/sqrt(n)
error = qt(0.975,n-1)*se

LCL = xbar - error
UCL = xbar + error

LCL #[1] 83.53347
UCL #[1] 86.46653 # N에 비해 범위가 더 넓어졌다!

mu0 = 87 # H0 : mu=87, H1 : mu<87
t0 = (xbar-mu0)/se 
tval = qt(0.05,n-1) 
t0 #[1] -2.854385 
tval #[1] -1.729133 # 임계치 값

pval = pt(t0,n-1)
pval #[1] 0.005072222 # 유의확률

# cf) if 양측검정 => pvalue를 2배로!



### 위 과정을 간단하게 해주는 함수
# T검정에서 t.test

t.test(data,mu=87,conf.level=0.95)
# data
# mu = 87 # 귀무가설 (H0)
# conf.level = 0.95 # 신뢰수준을 95프로 하겠다 (양측검증을 default로 함)

# One Sample t-test

# data:  data
# t = -2.8544, df = 19, p-value = 0.01014       # df는 자유도, p-value를 자동으로 계산해줌
# alternative hypothesis: true mean is not equal to 87 # alternative h~~ 는 대립가설, true mean(==mu 귀무가설)
# 95 percent confidence interval: # 신뢰구간을 자동으로 계산해줌
#  83.53347 86.46653
# sample estimates:
# mean of x
#        85

# 나는 작은 쪽으로 단측 검증을 하고 싶음
t.test(data,mu=87,conf.level=0.95,alternative="less") # 대립가설을 작은쪽으로 하겠다는 의미, 왼편

# One Sample t-test

# data:  data
# t = -2.8544, df = 19, p-value = 0.005072
# alternative hypothesis: true mean is less than 87
# 95 percent confidence interval:
#      -Inf 86.21156
# sample estimates:
# mean of x
#        85

# 큰 쪽으로 단측 검증을 하고자 한다면, alternative="greater"


#### 모비율에 대한 검정 ####

# p. 174

n = 30          
x = 18
phat = x/n
se = sqrt(phat*(1-phat)/n) # 표준오차
error = qnorm(0.975)*se #95% 오차한계
LCL = phat - error # 하한
UCL = phat + error # 상한

LCL #[1] 0.4246955
UCL #[1] 0.7753045

p0 = 0.5 # 모비율
se = sqrt(p0*(1-p0)/n) # 표준오차
z0 = (phat-p0)/se # 검정통계량
z0  #[1] 1.095445
pval = 2*(1-pnorm(z0)) # 양측 p-value, 양측으로 할 때는 2배를 해줘야함!
pval #[1] 0.2733217
# 유의 수준 5%보다 (0.05) 훨씬 큰 값 => 귀무가설 유지!(추가 정리!)
round(c(Z0=z0,p_value = pval),3)
#      Z0 p_value
#   1.095   0.273

### 모비율을 간단하게 처리해주는 함수 => prop.test
# prop.test는 카이제곱으로 비율에 대한 검정을 해줌
prop.test(x=18,n=30,p=0.5,correct=F) # correct는 연속성 수정 관련, correct에 따라 p-value 가 달라진다. 카이제곱으로 하므로 연속성 수정 필요 없음 -> correct = F


# 1-sample proportions test without continuity correction

# data:  18 out of 30, null probability 0.5
# X-squared = 1.2, df = 1, p-value = 0.2733 # p-value로 귀무가설 채택여부 확인하기!d, p-value 데이터만 가져오면 됨!
# alternative hypothesis: true p is not equal to 0.5
# 95 percent confidence interval:
#  0.4232036 0.7540937
# sample estimates:
#   p
# 0.6






