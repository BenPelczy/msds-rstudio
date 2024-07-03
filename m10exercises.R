fib <- numeric(10)
fib[1] <- fib[2] <- 1
for (i in 3:10) {
  fib[i] <- fib[i - 2] + fib[i - 1]
}

print(fib)

get_fib <- function(i=10) {
  fib <- numeric(i)
  fib[1] <- fib[2] <- 1
  for (j in 3:i) {
    fib[j] <- fib[j - 2] + fib[j - 1]
  }
  return(fib)
}

fib20 <- get_fib(20)
print(fib20)

is_percent <- function(x) {
  if (x >= 0 & x <= 1) {
    return(1)
  }
  return(0)
}

isp1 <- is_percent(.5)
isp2 <- is_percent(5)
isp3 <- is_percent(2.5)
isp4 <- is_percent(.00001)
isp5 <- is_percent(1)
print(isp1)
print(isp2)
print(isp3)
print(isp4)
print(isp5)

get_tenVWX <- function(strex) {
  tenVWX <- c(head(letters, 10), LETTERS[22:24])
  res <- c()
  for(i in unlist(strsplit(strex, ""))) {
    if(i %in% tenVWX) {
      res <- c(res, i)
    }
  }
  return(paste(res, collapse = ""))
}
print(get_tenVWX("The FoX Very Quickly Ran"))

print100 <- function() {
  for (i in 1:100) {
    print(paste(i))
    if (i %% 3 == 0) {
      print("Fizz")
    }
    if (i %% 5 == 0) {
      print("Buzz")
    }
  }
}

print100()

ex_unique <- function(strex) {
  return(unique(unlist(strsplit(tolower(strex), ""))))
}

print(ex_unique("strings on this"))
print(ex_unique(c(2, 3, 5, 2)))
