#Part1
##a
boston <- read.csv("https://people.bu.edu/kalathur/datasets/bostonCityEarnings.csv", colClasses = c("character", "character", "character", "integer", "character"))
hist(boston$Earnings, breaks=seq(0, 400000, by=50000), xlab="Earnings", ylab="Frequency", main="Histogram of Employee Earnings")
axis(side=1, at=seq(0, 400000, by=50000), labels=seq(0, 400000, by=50000))
mean_earnings <- mean(boston$Earnings)
sd_earnings <- sd(boston$Earnings)
cat("Mean of Employee Earnings: ", mean_earnings, "\n")
cat("Standard Deviation of Employee Earnings: ", sd_earnings, "\n")

#b
print('B')
set.seed(7308)
n_samples <- 1000
n <- 10
sample_means <- replicate(n_samples, mean(sample(boston$Earnings, size=n, replace=FALSE)))
hist(sample_means, main="Histogram of Sample Means (n=10)", xlab="Sample Means", ylab="Frequency")
mean_sample_means <- mean(sample_means)
sd_sample_means <- sd(sample_means)
cat("Mean of Sample Means (n=10): ", mean_sample_means, "\n")
cat("Standard Deviation of Sample Means (n=10): ", sd_sample_means, "\n")
#c
print('C')
set.seed(7308)
n_samples <- 1000
n <- 40
sample_means <- replicate(n_samples, mean(sample(boston$Earnings, size=n, replace=FALSE)))
hist(sample_means, main="Histogram of Sample Means (n=40)", xlab="Sample Means", ylab="Frequency")
mean_sample_means <- mean(sample_means)
sd_sample_means <- sd(sample_means)
cat("Mean of Sample Means (n=40): ", mean_sample_means, "\n")
cat("Standard Deviation of Sample Means (n=40): ", sd_sample_means, "\n")
#d
print('D')

#part 2
print('Part 2')
#a
print('A')
set.seed(7308)
n <- 1000
negbin <- rnbinom(n, size=3, prob=0.5)
proportions <- table(negbin) / n
barplot(proportions, xlab="Distinct Values", ylab="Proportions", main="Barplot of Negative Binomial Distribution")
#b
print('B')
set.seed(7308)
n_samples <- 5000
sample_sizes <- c(10, 20, 30, 40)
par(mfrow=c(2, 2))
for (i in 1:length(sample_sizes)) {
  n <- sample_sizes[i]
  sample_means <- replicate(n_samples, mean(sample(negbin, size=n, replace=FALSE)))
  hist(sample_means, main=paste("Histogram of Sample Means (n=", n, ")", sep=""), xlab="Sample Means", ylab="Frequency")
}
#c
print('C')
#c
set.seed(7308)
data <- rnbinom(1000, size = 3, prob = 0.5)
results <- matrix(0, nrow = 2, ncol = 5)
colnames(results) <- c("Data", "Sample_Size_10", "Sample_Size_20", "Sample_Size_30", "Sample_Size_40")
results[1, 1] <- mean(data)
results[2, 1] <- sd(data)
for (i in 1:4) {
  sample_size <- 10*i
  samples <- replicate(5000, sample(data, size = sample_size, replace = FALSE))
  means <- apply(samples, 2, mean)
  results[1, i+1] <- mean(means) 
  results[2, i+1] <- sd(means) 
}

results

#part 3
print('Part 3')
boston <- read.csv(
  "https://people.bu.edu/kalathur/datasets/bostonCityEarnings.csv",
  colClasses = c("character", "character", "character", "integer", "character")
)
top_departments <- names(sort(table(boston$Department), decreasing = TRUE)[1:5])
subset_data <- boston[boston$Department %in% top_departments,]
set.seed(7308)
#a
print('A')
sample_data <- subset_data[sample(nrow(subset_data), 50, replace = TRUE),]
table(sample_data$Department)
prop.table(table(sample_data$Department)) * 100
#b
print('B')
set.seed(7308)
earnings_range <- range(subset_data$Earnings)
step <- diff(earnings_range) / length(subset_data$Earnings)
inclusion_probs <- (subset_data$Earnings - earnings_range[1]) / step
sample_indices <- seq(1, nrow(subset_data), length.out = 50, along.with = inclusion_probs)
sample_data <- subset_data[sample_indices, ]
table(sample_data$Department)
prop.table(table(sample_data$Department)) * 100
#c
print('C')
set.seed(7308)
strata_sizes <- table(subset_data$Department)
stratum_sample_sizes <- floor(strata_sizes / sum(strata_sizes) * 50)
stratum_sample <- lapply(split(subset_data, subset_data$Department), function(x) x[sample(nrow(x), stratum_sample_sizes[unique(x$Department)]),])
sample_data <- do.call(rbind, stratum_sample)
table(sample_data$Department)
prop.table(table(sample_data$Department)) * 100
#d
print('D')
mean(subset_data$Earnings)
mean(sample_data$Earnings)
set.seed(7308)
simple_random_sample <- subset_data[sample(nrow(subset_data), 50, replace = TRUE),]
mean(simple_random_sample$Earnings)
earnings_range <- range(subset_data$Earnings)
step <- diff(earnings_range) / length(subset_data$Earnings)
inclusion_probs <- (subset_data$Earnings - earnings_range[1]) / step
sample_indices <- seq(1, nrow(subset_data), length.out = 50, along.with = inclusion_probs)
systematic_sample <- subset_data[sample_indices, ]
mean(systematic_sample$Earnings)
strata_sizes <- table(subset_data$Department)
stratum_sample_sizes <- floor(strata_sizes / sum(strata_sizes) * 50)
stratum_sample <- lapply(split(subset_data, subset_data$Department), function(x) x[sample(nrow(x), stratum_sample_sizes[unique(x$Department)]),])
stratified_sample <- do.call(rbind, stratum_sample)
mean(stratified_sample$Earnings)



