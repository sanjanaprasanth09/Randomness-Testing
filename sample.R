pi <- readLines("x") #string of digits of pi
pi_vector <- as.integer(strsplit(pi, "") [[1]])
digits <- pi_vector[1:15000000]

#seperating digits
two_dig <- digits[seq(1, length(digits), by = 2)] * 10 +
  digits[seq(2, length(digits), by = 2)]

three_dig <- digits[seq(1, length(digits), by = 3)] * 100 +
  digits[seq(2, length(digits), by = 3)] * 10 +
  digits[seq(3, length(digits), by = 3)]

four_dig <- digits[seq(1, length(digits), by = 4)] * 1000 +
  digits[seq(2, length(digits), by = 4)] * 100 +
  digits[seq(3, length(digits), by = 4)] * 10 +
  digits[seq(4, length(digits), by = 4)]

five_dig <- digits[seq(1, length(digits), by = 5)] * 10000 +
  digits[seq(2, length(digits), by = 5)] * 1000 +
  digits[seq(3, length(digits), by = 5)] * 100 +
  digits[seq(4, length(digits), by = 5)] * 10 +
  digits[seq(5, length(digits), by = 5)]

#chi-squared tests

##one digit
freqs <- table(factor(digits, levels = 0:9)) 
chisq.test(freqs, p = rep(1/length(freqs), length(freqs)))

##two digits
freqs <- table(factor(two_dig, levels = 0:99)) 
chisq.test(freqs, p = rep(1/length(freqs), length(freqs)))

##three digits
freqs <- table(factor(three_dig, levels = 0:999)) 
chisq.test(freqs, p = rep(1/length(freqs), length(freqs)))

##four digits
freqs <- table(factor(four_dig, levels = 0:9999)) 
chisq.test(freqs, p = rep(1/length(freqs), length(freqs)))

##five digits
freqs <- table(factor(five_dig, levels = 0:99999)) 
chisq.test(freqs, p = rep(1/length(freqs), length(freqs)))

##six digits
freqs <- table(factor(six_dig, levels = 0:999999)) 
chisq.test(freqs, p = rep(1/length(freqs), length(freqs)))

#gap test
library(randtoolbox)

##one digit:
test_dig <- 1
lb <- 0.05
ub <- 0.15
while(test_dig <= 9) {
  gaptest <- gap.test(digits_uni, lower = lb, upper = ub, echo = FALSE)
  print(test_dig)
  print (gaptest [3])
  lb <- lb + 0.1
  ub <- ub + 0.1
  test_dig <- test_dig + 1
}

##two digits:
test_dig <- 1
lb <- 0.005
ub <- 0.015
while(test_dig <= 99) {
  gaptest <- gap.test(two_dig_uni, lower = lb, upper = ub, echo = FALSE)
  print(test_dig)
  print (gaptest [3])
  lb <- lb + 0.01
  ub <- ub + 0.01
  test_dig <- test_dig + 1
}

##three digits:
test_dig <- 1
lb <- 0.0005
ub <- 0.0015
while(test_dig <= 999) {
  gaptest <- gap.test(three_dig_uni, lower = lb, upper = ub, echo = FALSE)
  print(test_dig)
  print (gaptest [3])
  lb <- lb + 0.001
  ub <- ub + 0.001
  test_dig <- test_dig + 1
}

#permutation test
set.seed(1)

##one digit
obs_counts <- table(factor(digits, levels = 0:9)) 
expected_counts <- rep(length(digits) / 10, 10)
obs_stat <- sum((obs_counts - expected_counts)^2 / expected_counts)
num_permutations <- 5000
perm_stats <- numeric(num_permutations)
for (i in 1:num_permutations) {
  perm_sample <- sample(0:9, length(digits), replace = TRUE)
  perm_counts <- table(perm_sample)
  perm_stats[i] <- sum((perm_counts - expected_counts)^2 / expected_counts)
}
p_value <- mean(perm_stats >= obs_stat)
print(p_value)

##two digits
obs_counts <- table(factor(two_dig, levels = 0:99)) 
expected_counts <- rep(length(two_dig)/ 100, 100)
obs_stat <- sum((obs_counts - expected_counts)^2 / expected_counts)
num_permutations <- 5000
perm_stats <- numeric(num_permutations)
for (i in 1:num_permutations) {
  perm_sample <- sample(0:99, length(two_dig), replace = TRUE)
  perm_counts <- table(perm_sample)
  perm_stats[i] <- sum((perm_counts - expected_counts)^2 / expected_counts)
}
p_value <- mean(perm_stats >= obs_stat)
print(p_value)

##three digits
obs_counts <- table(factor(three_dig, levels = 0:999))
expected_counts <- rep(length(three_dig)/ 1000, 1000)
obs_stat <- sum((obs_counts - expected_counts)^2 / expected_counts)
num_permutations <- 5000
perm_stats <- numeric(num_permutations)
for (i in 1:num_permutations) {
  perm_sample <- sample(0:999, length(three_dig), replace = TRUE)
  perm_counts <- table(factor(perm_sample, levels = 0:999))
  perm_stats[i] <- sum((perm_counts - expected_counts)^2 / expected_counts)
}
p_value <- mean(perm_stats >= obs_stat)
print(p_value)

##four digits
obs_counts <- table(factor(four_dig, levels = 0:9999))
expected_counts <- rep(length(four_dig)/ 10000, 10000)
obs_stat <- sum((obs_counts - expected_counts)^2 / expected_counts)
num_permutations <- 5000
perm_stats <- numeric(num_permutations)
for (i in 1:num_permutations) {
  perm_sample <- sample(0:9999, length(four_dig), replace = TRUE)
  perm_counts <- table(factor(perm_sample, levels = 0:9999))
  perm_stats[i] <- sum((perm_counts - expected_counts)^2 / expected_counts)
}
p_value <- mean(perm_stats >= obs_stat)
print(p_value)

##five digits
obs_counts <- table(factor(five_dig, levels = 0:99999))
expected_counts <- rep(length(five_dig)/ 100000, 100000)
obs_stat <- sum((obs_counts - expected_counts)^2 / expected_counts)
num_permutations <- 5000
perm_stats <- numeric(num_permutations)
for (i in 1:num_permutations) {
  perm_sample <- sample(0:99999, length(five_dig), replace = TRUE)
  perm_counts <- table(factor(perm_sample, levels = 0:99999))
  perm_stats[i] <- sum((perm_counts - expected_counts)^2 / expected_counts)
}
p_value <- mean(perm_stats >= obs_stat)
print(p_value)



