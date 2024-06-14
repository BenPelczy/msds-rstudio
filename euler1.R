sum <- 0
x <- 1
while (x < 1000) {
  if (x %% 3 == 0 | x %% 5 == 0) {
    sum = sum + x
  }
  x = x + 1
}
print(sum)