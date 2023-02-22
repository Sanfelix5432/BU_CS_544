library(ggplot2)
#Part1
print("PArt 1")
forbes <- read.csv("https://people.bu.edu/kalathur/datasets/forbes.csv")
print(forbes)
print("PArt 1")
#a
print("a")
ggplot(forbes,aes(x = country))+geom_bar(fill = "blue") + xlab("Country") + ylab("Number of Rich People") + ggtitle("Frequencies of the Number of Rich People by Country")
#b
print("b")
ggplot(forbes, aes(x = gender, fill = gender)) + geom_bar() + xlab("Gender") + ylab("Number of People") + ggtitle("Distribution of Females and Males in the dataset.")
#c
print("c")
top5 <- as.data.frame(table(forbes$category))[1:5, ]
ggplot(forbes, aes(x=category, fill=gender)) + geom_bar(position="dodge") + labs(title="Distribution of Females and Males across Top 5 Categories in the dataset.",x="Category", y="Count") + scale_x_discrete(limits=top5$Var1)

#Part2
print("Part 2")
us_quarters <- read.csv("https://people.bu.edu/kalathur/datasets/us_quarters.csv")
head(us_quarters)
#a
print("a")
us_quarters$State[which.max(us_quarters$DenverMint)]
us_quarters$State[which.max(us_quarters$PhillyMint)]
us_quarters$State[which.min(us_quarters$DenverMint)]
us_quarters$State[which.min(us_quarters$PhillyMint)]
#b
print("b")
par(mfrow=c(1,2),mar = c(1, 1, 1, 1))
barplot(cbind(DenverMint, PhillyMint) ~ State, col = c('blue','grey'),data = us_quarters, beside = T, legend = T)
#c
print("c")
par(mfrow=c(1,2),mar = c(1, 1, 1, 1))
boxplot(us_quarters$DenverMint, main="Denver Mint", ylab="Quarters (in thousands)")
boxplot(us_quarters$PhillyMint, main="Philly Mint", ylab="Quarters (in thousands)")
#d
fd = fivenum(us_quarters$DenverMint)
us_quarters$State[c(which(us_quarters$DenverMint > (fd[4]+1.5*(fd[4]-fd[2]))),which(us_quarters$DenverMint < (fd[2]-1.5*(fd[4]-fd[2]))))]
fp = fivenum(us_quarters$PhillyMint)
us_quarters$State[c(which(us_quarters$PhillyMint > (fp[4]+1.5*(fp[4]-fp[2]))),which(us_quarters$PhillyMint < (fp[2]-1.5*(fp[4]-fp[2]))))]

#part3
stocks <- read.csv("https://people.bu.edu/kalathur/datasets/stocks.csv")
#a
pairs(~ MSFT + AAPL + GOOG + FB + AMZN + TSLA, data = stocks)
#b
stocks1 <- subset(stocks, select = -c(Date))
matrix <- cor(stocks1)
round(matrix, 2)
#c
#d
n <- ncol(stocks)
for (i in 1:n) {
  stock <- colnames(stocks)[i+1]
  corr <- cm[i, ]
  top3 <- names(sort(corr, decreasing = TRUE))[2:(2 + 3)]
  cat(sprintf("Top 3 for Stock %s\n%s\t%s\t%s\n%0.2f\t%0.2f\t%0.2f\n\n",stock, top3[1], top3[2], top3[3], corr[top3[1]], corr[top3[2]], corr[top3[3]]))}

#part4
scores <- read.csv("https://people.bu.edu/kalathur/datasets/scores.csv")
#a
graph <- hist(scores$Score,breaks=8)
text(graph$breaks+2.5,graph$counts,labels=graph$counts)
grade <- hist(scores$Score,breaks=c(35,40,45,50,55,60,65,70,75,80,85))
n <- unlist(grade[2])
r <- unlist(grade[1])
numIter = 10
for (i in 1:numIter) {
  ressult <- sprintf("%d students in range (%d,%d]",n[i],r[i],r[i+1])
  print(ressult)
}

#b
grade <- hist(scores$Score,breaks=c(30,50,70,90))
n <- unlist(grade[2])
r <- unlist(grade[1])
class <- c("C","B","A")
numIter = length(c)
for (i in 1:numIter) {
  result <- sprintf("%d students in %s grade range (%d,%d]",n[i],class[i],r[i],r[i+1])
  print(result)
}
