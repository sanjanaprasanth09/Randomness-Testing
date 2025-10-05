freqs<- table(digits)
chisq.test(freqs, p = rep(1/length(freqs),length(freqs)))
