library(ggplot2)

binomial = rbinom(1000000, 10, 0.5)
normal = rnorm(1000000, 10, 0.5)

bino_df <- data.frame(weight = binomial)
norm_df <- data.frame(weight = normal)

ggplot(bino_df, aes(x=weight)) + geom_histogram()
ggplot(norm_df, aes(x=weight)) + geom_histogram()
