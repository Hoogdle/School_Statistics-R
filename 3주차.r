# cf
# 막대 그래표 => 질적 자료
# 히스토그램 => 양적 자료

data = c(150,153,155,152,146,160,155,155,146,155,163,151,159) # combine 함수 # 경계값의 값이 포함된다!
# 즉 140이상 150이하로 세팅 되어 있다!
# 147 -> 150 햇는데 140~150 구간의 갯수가 유지된다.
data

hist(data) # 히스토그램 그래프를 그리는 함수
hist(data,right=FALSE) #140이상 150미만으로 설정! #140~150은 2명으로 변경됨.
# right=FALSE 많이 사용된다!
hist(data,br=seq(140,170,10),right=FALSE) # 10단위로 자르기! 
# seq(140,170,10) 140부터 170까지 연속으로 하는데 10씩 증가하도록
# seq는 연속으로 값을 만드는 함수
hist(data,br=seq(140,170,5),right=FALSE) #140~170까지 연속으로 하는데 증가치를 5씩!

stem(data) #줄기 잎 그림은 인터프리터에 나오게 된다.
# cf) 줄기 잎에서는 줄기가 14이면 140~149까지 다룬다는 뜻.
boxplot(data) #Boxplot 함수 

# 2주차 복습

############################################
xbar = mean(data)

med = median(data) # 중위수
med std 

s2 = var(data) #분산
s2

std = sd(data) #표준편차
std

n = length(data)
n

cv = std/xbar
cv
############################################



# p.37 예제 3-5
height = c(162,163,166,168,169,171,173,174,175,179)
weight = c(54,56,56,64,62,64,82,67,71,74)

height
weight

students = data.frame(height,weight) #여러 개의 변수를 하나의 데이터 set으로 join
# height와 weight를 묶어서 student라는 변수 만듦
students
# 1     162     54
# 2     163     56
# 3     166     56
# 4     168     64
# 5     169     62
# 6     171     64
# 7     173     82
# 8     174     67
# 9     175     71
# 10    179     74

str(students)
# 'data.frame':   10 obs. of  2 variables:
#  $ height: num  162 163 166 168 169 171 173 174 175 179
#  $ weight: num  54 56 56 64 62 64 82 67 71 74
### 2개의 변수로 20개의 데이터를 읽은 것을 확인할 수 있음

# 자료가 height,weight,student 이렇게 3가지가 존재.
# 어떤 데이터 set에 있는 변수인지 확인해야한다.
# => attach(students)로 이후의 변수들은 students안에 있는 변수들이다. 라고 선언하는 것
attach(students)
# The following objects are masked _by_ .GlobalEnv:

#     height, weight
plot(height,weight,main="Scatter plot") #main은 그래프에 대한 Title(제목 짓기!)
# weight와 height가 가리키는 산점도를 그려주는 plot
# 1 parameter => x축, 2 parmeter => y축

# cor() : 상관계수 계산, 복잡한 계산 root를 한 번에 계산해줌!
cor(weight,height)
#[1] 0.834219

