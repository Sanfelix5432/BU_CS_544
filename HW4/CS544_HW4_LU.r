#part 1
p_1 <- 0.4
n_1 <- 5
#a
successes <- 0:n_1
CDF1 <- pbinom(successes, size=n_1, prob=p_1)
print(CDF1)
PMF1 <- dbinom(successes, size=n_1, prob=p_1)
print(PMF1)
par(mfrow=c(1,1), mar = c(1,1,1,1))
plot(successes, CDF1 ,type='h',xlab="sucesses",ylab="probabilities",col="blue",lwd=3,main="CDF")
plot(successes, PMF1,type='h',xlab="sucesses",ylab="probabilities",col="blue",lwd=3,main="PMF")
#b
p_1_b <- dbinom(2,n_1,p_1)
print(p_1_b)
#c
p_1_c <- 1-pbinom(1,n_1,p_1)
print(p_1_c)
#d
set.seed(100)
perfect_scores_num <- rbinom(1000, size = n_1, prob = p_1)
barplot(table(perfect_scores_num), main = "frequencies of successes", xlab = "number of perfect scores", ylab = "frequency")
#########################################################################################################################################
#part 2
p_2 <- 0.6 
r <- 3 
n_2 <- 10
#a
x <- 0:(n_2 - r)
PMF2 <- dnbinom(x, size = r, prob = p_2)
plot(x, PMF2, type = "h", xlab = "number of failures", ylab = "probability", main = "PMF")
print(PMF2)
CDF2 <- pnbinom(x, size = r, prob = p_2)
print(CDF2)
plot(x, CDF2, type = "s", xlab = "number of failures", ylab = "probability", main = "CDF")
#b
prob__of_4 <- dnbinom(4, size = r, prob = p_2)
prob__of_4
#c
atmost_4 <- pnbinom(4, size = r, prob = p_2)
atmost_4
#d
set.seed(100)
failures <- rnbinom(1000, size = r, prob = p_2)
barplot(table(failures), main = "frequencies of the failures", xlab = "Number of failures", ylab = "Frequency")
#########################################################################################################################################
#part 3
n_3 <- 20
p_3 <- 0.6
#a
question_num <- 0:20
probability <- dhyper(question_num,m=p_3*100,n=40,k=n_3)
print(probability)
plot(question_num, probability, type = "h", xlab = "Number of Multiple Choice Questions", ylab = "Probability")
#b
exact10 <- dhyper(10,m=p_3*100,n=40,k=n_3)
exact10
#c
least10 <- 1 - phyper(9,m=p_3*100,n=40,k=n_3)
least10
#d
set.seed(100)
data_in_sim <- rhyper(1000,m=p_3*100,n=40,k=n_3)
print(data_in_sim)
barplot(table(data_in_sim), xlab = "Number of Multiple Choice Questions", ylab = "Frequency")
#########################################################################################################################################
#part 4
Question_num <- 10
#a
dpois(8, Question_num)
#b
ppois(8, Question_num)
#c
ppois(12, Question_num) - ppois(5, Question_num)
#d
first_20_question <- 0:20
prob_20 <- dpois(first_20_question, Question_num)
plot(first_20_question, prob_20, type = "h", xlab = "Number of Questions", ylab = "Probability", main = "PMF")
#e
set.seed(100)
data_in_sim <- rpois(50, Question_num)
barplot(table(data_in_sim), xlab = "Number of Questions", ylab = "Frequency")
boxplot(data_in_sim, xlab = "Number of Questions")
#########################################################################################################################################
#part 5
mean <- 100
stand_deviation <- 10
#a
x <- seq(mean - 3*stand_deviation, mean + 3*stand_deviation, length.out = 100)
y <- dnorm(x, mean = mean, sd = stand_deviation)
plot(x, y, type = "l", main = "PDF", xlab = "Money Spent ($)", ylab = "Density")
#b
1 - pnorm(120, mean = mean, sd = stand_deviation)
#c
pnorm(90, mean = mean, sd = stand_deviation) - pnorm(80, mean = mean, sd = stand_deviation)
#d
1 - pnorm(mean + stand_deviation, mean = mean, sd = stand_deviation) + pnorm(mean - stand_deviation, mean = mean, sd = stand_deviation)
1 - pnorm(mean + 2*stand_deviation, mean = mean, sd = stand_deviation) + pnorm(mean - 2*stand_deviation, mean = mean, sd = stand_deviation)
1 - pnorm(mean + 3*stand_deviation, mean = mean, sd = stand_deviation) + pnorm(mean - 3*stand_deviation, mean = mean, sd = stand_deviation)
#e
qnorm(0.1, mean = mean, sd = stand_deviation)
qnorm(0.9, mean = mean, sd = stand_deviation)
#f
qnorm(0.98, mean = mean, sd = stand_deviation)
#g
set.seed(100)
visitors <- rnorm(10000, mean = mean, sd = stand_deviation)
hist(visitors, main = "plot for 10,000 visitors using the above distribution",xlab = "Money Spent ($)", col = "blue", breaks = 20)
