library(SportsAnalytics)
library(data.table)
library(dplyr)
nba <- fetch_NBAPlayerStatistics(season = '17-18')
nba <- data.table(nba)
class(nba)
View(nba)

#data.table索引：DT[i,j,by]
#1. i:篩選Row，使用booling value或index

#(1) 擷取姓名含Alex的球員
nba[grepl('Alex',Name)]

#(2) 篩選所有姓名包含A的SG
nba[grepl('A',Name)&Position=='SG']

#(3)篩選出場數超過50場的球員
nba[GamesPlayed>50]

#2. j:決定輸出的colume，可以是原始欄位，也可以是計算後的欄位

#(1) 計算所有球員平均出場數
nba[,mean(GamesPlayed)]

#(2) 一次計算多個值，此時j欄位要用.()包覆
nba[,.(mean(GamesPlayed),sd(GamesPlayed))]
nba[,.(GamesPlayedMean=mean(GamesPlayed),GamesPlayedSD=sd(GamesPlayed))]

#(3) 配合i參數使用
nba[GamesPlayed>50,.(TotalMinutesPlayedMean=mean(TotalMinutesPlayed),TotalMinutesPlayedSD=sd(TotalMinutesPlayed))]

#3. by:分組計算的依據
#(1) 計算各隊球員數和平均抄截數(.N計算個數)
nba[,.(.N,StealsMean=mean(Steals)),by=Team]

#(2) 綜合使用三個參數，計算各隊中鋒數和其平均block次數
nba[Position=='C',.(.N,BlocksMean=mean(Blocks)),by=Team]


#dplyr
nba <- fetch_NBAPlayerStatistics('17-18')
#1. select：選取column，用?select_helpers看更多
select1 <- select(nba,Name,starts_with('Threes'),starts_with(('FieldGoals')))
head(select1)
select2 <- select(nba,Name:FreeThrowsAttempted)
head(select2)

#2. filter：選取row
#(1) 看出場時間超過2000分鐘的球員
filter1 <- filter(nba,TotalMinutesPlayed>2000)
filter1
#(2) 找隊伍名稱是LAC或ORL
filter2 <- filter(nba,Team %in% c('LAC','ORL'))
head(filter2)
#(3) 計算後再篩選，也可用&串聯
filter3 <- filter(nba,FieldGoalsMade/FieldGoalsAttempted>0.7&GamesPlayed>5)
filter3

#3. mutate：增加新欄位
#(1) 增加新欄位AveragePlayed
mutate1 <- mutate(nba,AveragePlayed=TotalMinutesPlayed/GamesPlayed)
mutate1$AveragePlayed[1:10]

#4. summarise：計算統計值
#(1) 計算各種統計值
sum1 <- summarise(nba,nplayer=n(),nTeam=n_distinct(Team))
sum1
#(2) 和filter混用
sum2 <- summarise(filter1,nPlayer=n())
sum2

#5. group_by：設定分組依據
#(1) 和summarise混用
group1 <- group_by(nba,Team) %>% 
  summarise(nPlayer=n())
group1
#(2) 多個分組條件
group2 <- group_by(nba,Team,Position) %>% 
  summarise(nPlayer=n())
head(group2)

#6. arrange：觀察值排序
#(1) 遞增排序
arrange1 <- arrange(nba,GamesPlayed)
head(arrange1)
#(2) 遞減排序
arrange2 <- arrange(nba,desc(GamesPlayed),desc(TotalMinutesPlayed))
head(arrange2)

#7. rename：重新命名欄位
#(1) 重新命名
rename1 <- rename(nba,GP=GamesPlayed)
rename1[1:5,1:5]
