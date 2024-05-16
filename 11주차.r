# *신뢰구간은 <-n-> , 중간에서 퍼져나가는...
# ex) 신뢰구간 99% -> qnorm(0.995,~~)


# p150

# 연습문제 4번

# 연습문제 2의 자료에서 모표준편차 sigma를 추정하여라
score = c(35,19,40,35,51,41,27,23,39,21,41,31,46,51,34,37,36,55,52,32)
s = sd(score)
s

# 연습문제 5번
# 어느 대학에서 남학생의 평균신장 mu를 추정하기 위하여 50명을 임의로 추출한 결과
# xbar = 173, s = 3 이었따.
# 남학생의 평균신장 mu에 대한 95% 신뢰구간을 구하여라

xbar = 173
s = 3
n = 50

# *지금은 sigma를 모르는 경우(but 대표본인 경우에는 추정 가능)

# 1) sigma를 아는 경우 (z 분포)
# n이 충분히 크다 => 중심극한정리 => norm에 근사
SE = s/sqrt(n) # == 표준오차
error = qnorm(0.975)*SE # == 오차한계 (== 100(1-a)%)

LCL = xbar - error # 하한
UCL = xbar + error # 상한

LCL #[1] 172.1685
UCL #[1] 173.8315

# 2) sigma를 모르는 경우(t 분포)
SE = s/sqrt(n) # == 표준오차
error = qt(0.975,n-1)*SE # == 오차한계 (== 100(1-a)%)

LCL = xbar - error # 하한
UCL = xbar + error # 상한

LCL #[1] 172.1474
UCL #[1] 173.8526


# p151
# 연습문제 6번

# 10개의 공을 임의로 택함, 지름의 측정 평균이 6.25cm, 표준편차 0.15cm.
# 공의 평균지름에 대한 99% 신뢰구간을 구하여라
# 단, 공의 평균지름은 정규모집단에 따른다.(=공의 지름은 정규분포를 따른다)(이 조건이 없으면 t에 적용이 안 됨)

# cf) 중심극한정리 norm를 따르지 않아도 표본을 크게 뽑으면 norm에 근사가능!

# 정규모집단에서 뽑은 공들을 t분포에 적용!

# sol)
xbar = 6.25
s = 0.15
n = 10

SE = s/sqrt(n) #== 표준오차
error = qt(0.995,n-1)*SE

LCL = xbar - error
UCL = xbar + error

LCL #[1] 6.095847
UCL #[1] 6.404153


### 연습문제 7번(good)
# 임의로 선택된 10명, 책임감 득점 표, 책임감 득점은 정규분포를 따른다.
# 평균득점에 대한 90% 신뢰구간?

score = c(28.5,25.2,28.7,41.0,29.1,32.3,37.7,39.9,26.8,28.8)
xbar = mean(score)
s = sd(score)
n = 10

SE = s/sqrt(n)
error = qt(0.95,n-1)*SE

LCL = xbar - error
UCL = xbar + error

LCL
UCL


### 연습문제 8번(good)
# 30명 학생의 최대 호흡량 데이터
# 평균최대호흡량에 대한 95% 신뢰구간?

data = c(132,103,91,108,167,119,154,200,109,133,196,93,187,121,163,166,84,110,157,138,95,120,163,88,102,127,136,175,99,113)

xbar = mean(data)
s = sd(data)
n = 30

SE = s/sqrt(n)
error = qt(0.975,n-1)*SE

LCL = xbar - error
UCL = xbar + error

LCL #[1] 119.0171
UCL #[1] 144.2495


### 연습문제 9번(what)
# 주부 100명 뽑음
# 20%만 회사제품 선호
# 회사 제품을 선호하는 모비율에 대한 95% 신뢰구간?

# E(X) = np
# V(X) = np(1-p)

n = 100
xbar = 100*0.2
s = sqrt(100*0.2(1-0.2))

SE = s/sqrt(n)
error = qt(0.975,n-1)*SE

LCL = xbar - error
UCL = xbar + error

LCL #[1] 13.29596
UCL #[1] 26.70404


### 9번 (교수님 풀이) #모분산 추정!(good)
phat = 0.2
n = 100

SE = sqrt(phat*(1-phat)/n) # 표준오차 추정량
error = qnorm(0.975)*SE # 100(1-a)% 오차한계

LCL = phat - error
UCL = phat + error

LCL #[1] 0.1216014
UCL #[1] 0.2783986



### 연습문제 10번(what)

# 200명 추출
# 112명 지지
# 지지율에 대한 90% 신뢰구간?

p = 112/200

# E(X) = np
# V(X) = np(1-p)

n = 200
xbar = n*p
s = sqrt(n*p(1-p))

SE = s/sqrt(n)
error = qt(0.95,n-1)*SE

LCL = xbar - error
UCL = xbar + error

LCL #[1] 108.0519
UCL #[1] 115.9481


### 10번 다시 풀이 (good)

n = 200
phat = 112/200

SE = sqrt(phat*(1-phat)/n) # 표준오차 추정량
error = qnorm(0.95)*SE # 100(1-a)% 오차한계

LCL = phat - error
UCL = phat + error

LCL #[1] 0.5022659
UCL #[1] 0.6177341
