library(ggplot2)

binomial = rbinom(1000, 10, 0.5)
normal = rnorm(1000, 0, 0.25)
uniform = runif(1000, -1, 1)

bino_df <- data.frame(weight = binomial)
norm_df <- data.frame(weight = normal)
unif_df <- data.frame(weight = uniform)

ggplot(bino_df, aes(x=weight)) + geom_histogram()
ggplot(norm_df, aes(x=weight)) + geom_histogram()
ggplot(unif_df, aes(x=weight)) + geom_histogram()