name = c("LEE","KIM","KANG")
kor = c(80,90,70)
eng = c(90,80,70)

sungjuk = data.frame(name,kor,eng) # 3개의 변수가 하나의 구조를 가지게 함

sungjuk
#   name kor eng
# 1  LEE  80  90
# 2  KIM  90  80
# 3 KANG  70  70

###########################################################

height = c(162,163,166,168,169,171,173,174,175,179)
weight = c(54,56,56,64,62,64,82,67,71,74)

students = data.frame(height,weight)

students
   height weight 
1     162     54
2     163     56
3     166     56
4     168     64
5     169     62
6     171     64
7     173     82
8     174     67
9     175     71
10    179     74
> #    height weight
> # 1     168     45
> # 2     166     45
> # 3     170     46
> # 4     167     45
> # 5     160     44
> # 6     162     46
> # 7     158     45
> # 8     163     45
> # 9     158     45
> # 10    167     46
> # 11    167     43
> # 12    167     47
> # 13    163     46
> # 14    162     44
> # 15    155     45

str(students) # structer 확인

# 'data.frame':   10 obs. of  2 variables:
#  $ height: num  162 163 166 168 169 171 173 174 175 179
#  $ weight: num  54 56 56 64 62 64 82 67 71 74


##################################################################################
#if not attach
# students$hegiht, students$weight와 같이 $로 해당 데이터의 집합을 명시해줘야 함.

attach(students)
# students 생략 가능, 자동으로 students의 집합인 것을 인식함.
##################################################################################

plot(height,weight, main = "Scatter plot")

cor(weight,height)
# [1] 0.2132904

#################################질적 자료#################################################

# 데이터 읽어들이기
blood2 = read.table("c:\\Users\\rlaxo\\OneDrive\\바탕 화면\\R_4주차_실습.txt",header=T) #header=FALSE, 제목이 없음
blood2
# 이름을 주지 않았기 때문에 V1(Vairable1)으로 표시됨.
#    V1 
# 1   A
# 2   B
# 3   B
# 4   B
# 5   O
# 6   B
# 7   A
# 8   A
# 9   B
# 10  B
# 11 AB
# 12  B
# 13  A
# 14  O
# 15  A
# 16  O
# 17  A
# 18  A
# 19  B
# 20  A
# 21  O
# 22  O
# 23  B
# 24  B

str(blood2)
table(blood2$blood)

attach(blood2)
barplot(table(blood)) #table의 데이터로 막대 그래프 생성

slices = c(8,1,10,5) #table의 slices에 저장 
pie(slices)# 원형그래프 생성



#################################양적 자료#################################################

# p.38 연습문제 2번
data2 = c(9,12,18,14,12,14,12,10,16,11,9,11,13,11,13,15,13,14)
mean(data2) #[1] 12.61111
sd(data2) #[1] 2.354942
median(data2) #[1] 12.5
IQR(data2) #[1] 3
IQR(data2)/2 #[1] 1.5

