# Download Digits of Pi

```{r}
pi <- readLines("x") #string of digits of pi
pi_vector <- as.integer(strsplit(pi, "") [[1]])
digits <- pi_vector[1:15000000]
```

# Separating Digits

```{r}
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
```

# Chi - Squared Test

## one digit

```{r}
freqs <- table(factor(digits, levels = 0:9)) 
chisq.test(freqs, p = rep(1/length(freqs), length(freqs)))
```

## two digits

```{r}
freqs <- table(factor(two_dig, levels = 0:99)) 
chisq.test(freqs, p = rep(1/length(freqs), length(freqs)))
```

## three digits

```{r}
freqs <- table(factor(three_dig, levels = 0:999)) 
chisq.test(freqs, p = rep(1/length(freqs), length(freqs)))
```

## four digits

```{r}
freqs <- table(factor(four_dig, levels = 0:9999)) 
chisq.test(freqs, p = rep(1/length(freqs), length(freqs)))
```

## five digits

```{r}
freqs <- table(factor(five_dig, levels = 0:99999)) 
chisq.test(freqs, p = rep(1/length(freqs), length(freqs)))
```

# Gap Test:

```{r}
library(randtoolbox)
```

## one digit:

```{r}
digits_uni <- digits/10
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
```

## two digits:

```{r}
two_dig_uni <- two_dig/100
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
```

## three digits:

```{r}
three_dig_uni <- three_dig/1000
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
```
