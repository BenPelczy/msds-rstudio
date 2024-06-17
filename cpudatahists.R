library(tidyverse)
library(ggplot2)

msds_data <- read.csv("MSDS-Orientation-Computer-Survey(in).csv")

ggplot(as.msds_data, aes(x="CPU Number of Cores (int)")) + geom_histogram(bins = 20, color = "#D42121") + ggtitle("CPU Number of Cores")

ggplot(as.msds_data, aes(x="CPU Cycle Rate (in GHz)")) + geom_histogram(bins = 5, color = "#D42121") + ggtitle("CPU Cycle Rate")

ggplot(as.msds_data, aes(x="RAM (in GB)")) + geom_histogram(bins = 50, color = "#D42121") + ggtitle("RAM")

ggplot(as.msds_data, aes(x="Hard Drive Size (in GB)")) + geom_histogram(bins = 50, color = "#D42121") + ggtitle("Hard Drive Size")
