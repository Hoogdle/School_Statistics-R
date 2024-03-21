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

