### hw_1_question


########################################################### Task 1

# 查看內建資料集: 鳶尾花(iris)資料集
iris

# 使用dim(), 回傳iris的列數與欄數
dim(iris)

# 使用head() 回傳iris的前六列
head(iris)

# 使用tail() 回傳iris的後六列
tail(iris)

# 使用str() 
str(iris)

# 使用summary() 查看iris敘述性統計、類別型資料概述。
summary(iris)

########################################################### Task 2

# 使用for loop 印出九九乘法表
# Ex: (1x1=1 1x2=2...1x9=9 ~ 9x1=9 9x2=18... 9x9=81)
numbers<-c(1:9)
for(i in numbers){
  print(paste0(as.character(i),'X',as.character(1:9),'=',i*1:9))
}

########################################################### Task 3

# 使用sample(), 產出10個介於10~100的整數，並存在變數 nums
tentohundred<-c(10:100)
nums<-sample(tentohundred,size=10)
# 查看nums
print(nums)

# 1.使用for loop 以及 if-else，印出大於50的偶數，並提示("偶數且大於50": 數字value)
# 2.特別規則：若數字為66，則提示("太66666666666了")並中止迴圈。
for(i in nums){
  if (i==66){
    print('太66666666666了')
    break
  }else if(i>50 & i%%2==0){
    print(paste0('偶數且大於50',':數字',i))
  }
}
  
  
  
  



########################################################### Task 4
# 請寫一段程式碼，能判斷輸入之西元年分 year 是否為閏年
if (year%%4!=0){
  print('no')
}else if(year%%100==0&year%%400!=0){
  print('no')
}else{
  print('yes')
}





########################################################### Task 5

# 猜數字遊戲
# 1. 請寫一個由電腦隨機產生不同數字的四位數(1A2B遊戲)
# 2. 玩家可重覆猜電腦所產生的數字，並提示猜測的結果(EX:1A2B)
# 3. 一旦猜對，系統可自動計算玩家猜測的次數
Ans<-sample(1000:9999,size = 1)






