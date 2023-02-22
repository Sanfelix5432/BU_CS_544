library(prob)
#part2
print("PART2")
output <- rolldie(3)
output$probs <- 1/nrow(output)
#a 
print("a")
sumRow <- rowSums(output[c('X1', 'X2', 'X3')]) 
Three_Eight <- output[sumRow >3 & sumRow <8,]
print(Three_Eight)
print(sum(Three_Eight$probs))
print("=========================================================================================")
#b
print("b")
ThreeRow <- (output['X1'] == output['X2']) &(output['X2'] == output['X3'])
all_three <- output[ThreeRow,]
print(all_three)
print(sum(all_three$probs))
print("=========================================================================================")
#c
print("c")
twoRow <- (output['X1'] == output['X2']) | (output['X1'] == output['X3']) |(output['X2'] == output['X3'])
TwoRow <- twoRow &(!ThreeRow)
Two <- output[TwoRow,]
print(Two)
print(sum(Two$probs))
print("=========================================================================================")
#d
print("d")
None <- (output['X1'] != output['X2']) & (output['X1'] != output['X3']) & (output['X2'] != output['X3'])
NoneIdentical <- output[None,]
print(NoneIdentical)
print(sum(NoneIdentical$probs))
print("=========================================================================================")
#e
print("e")
Two_identical <- twoRow &(!ThreeRow) & output[sumRow >3 & sumRow <8,]
two_indentical <- output[Two_identical,]
print(two_indentical)
print(sum(two_indentical$probs))
print("=========================================================================================")
#PART 2
print("PART 3")
sum_of_first_N_even_squares <- function(n){
    n = n
    sum = 0
    for (i in 1:n){
        n <- 2*i
        sum <- sum + n^2
    }
    print(sum)
}
print("Sum of first 2 even function")
sum_of_first_N_even_squares(2)
print("Sum of first 5 even function")
sum_of_first_N_even_squares(5)
print("Sum of first 10 even function")
sum_of_first_N_even_squares(10)
print("=========================================================================================")
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
