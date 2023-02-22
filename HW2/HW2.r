#PART 4
print("PART 4")
tsla <- read.csv("https://people.bu.edu/kalathur/datasets/TSLA.csv")

#a
print("a")
close <- summary(tsla$Close)
names(close) <- c("Min","Q1","Q2","Mean","Q3","Max")
print(close)
print("=========================================================================================")
#b
print("b")
minP <- which.min(tsla$Close)
tc <- tsla$Close[minP]
td <- tsla$Date[minP]
print(sprintf("The minimum Tesla value of %d is at row %d on %s", tc, minP, td))
print("=========================================================================================")
#c
print("c")
maxP <- which.max(tsla$Close)
tc <- tsla$Close[maxP]
td <- tsla$Date[maxP]
print(sprintf("The maximum Tesla value of %d is at row %d on %s", tc, maxP, td))
print("=========================================================================================")
#d
print("d")
Profit <- sum((tsla$Close - tsla$Open) > 0, na.rm = TRUE)
Prob <- Profit / totalRow
Percentage <- Prob*100
print(sprintf("The probability is %f percent ",Percentage))
print("=========================================================================================")
#e
print("e")
One_Million <- sum(tsla$Volume > 100000000, na.rm = TRUE)
Prob <- One_Million / totalRow
Percentage <- Prob*100
print(sprintf("The probability is %f percent ",Percentage))
print("=========================================================================================")
#f
print("f")
Profit<- sum((tsla$Close - tsla$Open) > 0 & tsla$Volume > 100000000, na.rm = TRUE)
Prob <- Profit / totalRow
Percentage <- Prob*100
print(sprintf("The probability is %f percent ",Percentage))
print("=========================================================================================")
#g
print("g")
Sell_out<- tail(tsla$Close,1)
Buy_in <- which.min(tsla$Close)
profit <- Sell_out - buyinPrice
print(sprintf("Buy in with the lowest price $%d, and sold out :$%d on the last day. The net profit is :$%d", Buy_in, Sell_out, profit))
