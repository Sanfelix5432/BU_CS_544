#part3
stocks <- read.csv("https://people.bu.edu/kalathur/datasets/stocks.csv")
#a
pairs(~ MSFT + AAPL + GOOG + FB + AMZN + TSLA, data = stocks)
#b
stocks1 <- subset(stocks, select = -c(Date))
cm <- cor(stocks1)
round(res, 2)
#c
summary(stocks)
#d
n <- ncol(stocks)
for (i in 1:n) {
  stock <- colnames(stocks)[i+1]
  corr <- cm[i, ]
  top3 <- names(sort(corr, decreasing = TRUE))[2:(2 + 3)]
  cat(sprintf("Top 3 for Stock %s\n%s\t%s\t%s\n%0.2f\t%0.2f\t%0.2f\n\n",stock, top3[1], top3[2], top3[3], corr[top3[1]], corr[top3[2]], corr[top3[3]]))}