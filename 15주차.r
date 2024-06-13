# 2024-06-13

# 두 집단 평균 비교

# p201, ex)10-1
# 1. 두 집단이 독립인지 비교
# 2. 두 집단이 등분산인지 이분산인지 확인
# -> 등분산이라면 t분포에 적용!

# p204 분산비교를 먼저 한 후 (등분산인지 확인)
# p202 두 집단 평균비교

X = c(105,108,86,103,104,107,124,105)
Y = c(89,92,84,97,103,107,111,97,101,106)

# 등분산인지 확인! 
# H0 = 시그마1 제곱 / 시그마2 제곱 =1
# H1 = 시그마1 제곱 > 시그마2 제곱
# 되도록이면 '큰 검정으로'

m  = length(X)
m # for 자유도

n = length(Y)
n # for 자유도

F0 = var(X)/var(Y)
F0

falpha = qf(0.95,m-1,n-1)
falpha

pval = 1-pf(F0,m-1,n-1)
pval

# F0 = [1] 3.292746
# pval = [1] 0.2970492

# 귀무가설을 기각하게 되면 3.2(30프로 정도의 오류를 범하는 것)
# 오류를 범하는 범위를 3프로만 허용!
# --> 귀무가설을 기각하지 못한다.

### var.test 명령 하나면 양측 검증을 해준다.
### var.test는 등분산을 검정하는 명령어

var.test(X,Y) # 양측검정
# F test to compare two variances

# data:  X and Y
# F = 1.4447, num df = 7, denom df = 9, p-value = 0.5941
# alternative hypothesis: true ratio of variances is not equal to 1 # ==양측검증으로 했어유
# 95 percent confidence interval:
#  0.3442286 6.9683117 # 분산비에 대한 구간 추정값, 위 예제는 구간에 포함되므로 등분산이 아니라고 할 순 없다.
                                            # 구간 추정은 양측의 값을 띄어내는 것을 원리로 적용
# sample estimates:
# ratio of variances # 분산비
#           1.444744

var.test(X,Y,alt="greater",conf.level=0.9)  # 단측 검증 (alt=" ") # conf.level는 95프로 default
# F test to compare two variances

# data:  X and Y
# F = 1.4447, num df = 7, denom df = 9, p-value = 0.297 # 전의 F값이 그대로 나옴을 확인 num df == 자유도(분자)
                                                            # denom df == 자유도(분모) # 단측으로 했기에 pvalue가 절반이 된다.
# alternative hypothesis: true ratio of variances is greater than 1 # greater 옵션
# 90 percent confidence interval:
#  0.5766718       Inf 
# sample estimates:
# ratio of variances # 분산비(F분포)
#           1.444744

# var.test만 잘 활용하면 (t.test도 마찬가지) 검정 과정속에 식들을 일일이 계산할 필요가 없다.


### 위 과정으로 등분산이 아님이 아님을 즉, 등분산임을 확인하였고 이제는 t.test로 평균 비교를 할거!

# 두집단 평균비교(등분산 일때)
t.test(X,Y,var.equal=T) # var.equal = T 등분산임

# Two Sample t-test

# data:  X and Y
# t = 1.4775, df = 16, p-value = 0.159
# alternative hypothesis: true difference in means is not equal to 0 #양측 검증을 햇음
# 95 percent confidence interval:
#  -2.848081 15.948081 
# sample estimates:
# mean of x mean of y
#    105.25     98.70

# 등분산이 아닌 경우(이분산일 때, var.equal=T를 추가하지 않을 때, default가 var.equal=F(이분산)이다.)

t.test(X,Y)
#  Welch Two Sample t-test

# data:  X and Y
# t = 1.446, df = 13.645, p-value = 0.1707
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -3.188892 16.288892
# sample estimates:
# mean of x mean of y
#    105.25     98.70

# t 검정을 위한 모든 과정을 t.test 하나만으로 간편하게 해결!


### {Paired T test}
# 202p 예제 10-5

Before = c(128,131,131,127,132,125,141,137,118,132,129,135)
After = c(120,124,130,118,140,128,140,135,126,130,126,127)

d = Before - After # 교수님께서 양으로 계산하기 위해 before - after로 진행하셧음(책에서는 after - before)
d

d0 = 5

n = length(Before)
n

se = sqrt(var(d)/n)
se 

cv = qt(0.975,n-1) # 양측
cv #[1] 0.2791266

t0 = (mean(d)-d0)/se
t0 #[1] -1.882119

pval = 2*(pt(t0,n-1)) # 값이 음수이므로 
pval #[1] 0.08652668


### 위 과정을 한 줄의 명령어 처리!
t.test(Before,After,mu=5,paired=T) #mu(대응비교의 차잇값)의 default는 0, (두 값이 같음을 의미) #paired=T를 주면 '대응비교'

# Paired t-test

# data:  Before and After
# t = -1.8821, df = 11, p-value = 0.08653 # greate(단측)r를 주면 p-value가 절반이였을것!
# alternative hypothesis: true mean difference is not equal to 5 # 양측 검증으로 mu(5)가 있는지 확인!
# 95 percent confidence interval:
#  -1.869825  5.536492 # mu 5가 신뢰구간 범위 내에 들어가 있다! => 귀무가설 기각 X (추가정리 필요!)
# sample estimates:
# mean difference
#        1.833333

### {모비율에 대한 비교}

n1 = 100
n2 = 100
x1 = 21
x2 = 38
phat1 = x1/n1
phat2 = x2/n2
alpha = 0.01
phat = (x1+x2)/(n1+n2)      # 공통 모비율
se = sqrt(phat*(1-phat)*(1/n1+1/n2)) #표준오차
z0 = (phat1-phat2)/se # 검정통계량
z0 #[1] -2.635897
cv = qnorm(alpha) # 0.01 임계치
cv #[1] -2.326348
zalpha = qnorm(0.99) #Z_a (Z alpha)
zalpha #[1] 2.326348
pval = pnorm(z0) # p-value
pval #[1] 0.004195766

L = (phat1-phat2)-zalpha*sqrt((phat1*(1-phat1)/n1)+(phat2*(1-phat2)/n2))
U = (phat1-phat2)+zalpha*sqrt((phat1*(1-phat1)/n1)+(phat2*(1-phat2)/n2))
L #[1] -0.3174068
U #[1] -0.02259323


### prop.test로 위 과정을 심플화 가능
### 대신 z는 구하지 못하지만 p-value를 구할 순 있다!

### prop.test

prop.test(x=c(21,38), n=c(100,100), correct=F, conf.level=0.99) # conf.level == 99프로 신뢰구간 (양측검증)

# z 값은 나오지 않지만 pvalu 값을 동일하게 나온다.

# 2-sample test for equality of proportions without continuity correction

# data:  c(21, 38) out of c(100, 100)
# X-squared = 6.948, df = 1, p-value = 0.008392
# alternative hypothesis: two.sided
# 99 percent confidence interval:
#  -0.333214919 -0.006785081
# sample estimates:
# prop 1 prop 2
#   0.21   0.38


prop.test(x=c(21,38), n=c(100,100), correct=F, alt="less",conf.level=0.99) # conf.level == 99프로 신뢰구간 (단측검증)

# 2-sample test for equality of proportions without continuity correction

# data:  c(21, 38) out of c(100, 100)
# X-squared = 6.948, df = 1, p-value = 0.004196
# alternative hypothesis: less
# 99 percent confidence interval:
#  -1.00000000 -0.02259323
# sample estimates:
# prop 1 prop 2
#   0.21   0.38



