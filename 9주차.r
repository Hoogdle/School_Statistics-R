# x~N(20,8^2)   n=16    xbar~N(20,(8/4)^2)

# p(xbar>=23) = ?
1 - pnorm(23,20,2) #[1] 0.0668072

# p(18<=xbar<=21)
pnorm(21,20,2)-pnorm(18,20,2) #[1] 0.5328072

# p120 연습문제 5
# x~N(40,18^2)  n=36    xbar~N(40,(18/6)^2)
pnorm(43,40,3) - pnorm(34,40,3) #[1] 0.8185946

# 연습문제 6
# x~N(900,40^2) 
#1 p(x<=940)
pnorm(940,900,40) * 1000 #[1] 841.3447
# 1000개가 샘플이 아님!, x에 대한 샘플

#2 p(xbar>905)
1-pnorm(905,900,4) #[1] 0.1056498

# 연습문제 13
# 평균 300만원, 표준편차 100만원
# 임의로 추출한 100명에 대한 표본 평균이 290만보다 클 확률?

# x~N(300,100^2)
# n=100, xbar~N(300,100^2/100) -> xbar~N(300,10^2)
1-pnorm(290,300,10) #[1] 0.8413447

# 연습문제 14
# x~N(100,10^2) 
# n=25
# xbar~N(100,10^2/25) == xbar~N(100,2^2)
pnorm(102,100,2) - pnorm(97,100,2) [1] 0.7745375

# 연습문제 15
qnorm(0.95,120,4) #[1] 126.5794


# {분포 그래프}

# <카이제곱분포>
# x가 취하는 범위 0~15
# dchisq(x,A) A는 자유도!
# 자유도 A가 커지면 커질수록 정규분포처럼 그려진다.
curve(dchisq(x,30),0,60) 


# <표준 정규분포>
curve(dnorm(x),-5,5)

# <t분포> *자유도가 있어야!
curve(dt(x,2),-5,5,add=T) # add=T(True)로 그래프 겹쳐서 그리기


### 연습문제 121p

# 연습문제 7번
# (1) 자유도 4일때 상위 5프로의 백분위수
qchisq(0.95,4) #[1] 9.487729
# (2) 자유도 16일 때 하위 2.5프로의 백분위수
qchisq(0.025,16) #[1] 6.907664

# 연습문제 8번
# (2) 자유도 20, 카이제곱분포 따름, 다음 확률?
# P(V>=31.4104)??
1-pchisq(31.4104,20) #[1] 0.0500004

#(3)
pchisq(28.4120,20) - pchisq(9.5908,20) #[1] 0.8750001

# 연습문제 9번
# (1) 자유도 6, 상위 5프로 백분위수? 
# 백분위수는 '값'을 찾으라는 것
qt(0.95,6) #[1] 1.94318
qt(0.01,20) #[1] -2.527977


# 연습문제 10번
# 확률변수 T가 자유도 25인 t분포 따름
# (1) P(1.708<=T<=2.787)
pt(2.787,25) - pt(1.708,25) #[1] 0.04500813
