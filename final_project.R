
# Load packages
library(tidyverse)
library(data.table)
library(MASS)


# Compile C files we will want to make use of later
dyn.load("kde.so")
dyn.load("kde2.so")


# Read in data set
df <- read.csv("./Data/stats.csv")


# Choose only the columns we want to use
df1 <- df[, c(1, 2, 3, 5, 6, 7, 8, 9, 12, 13)]    # Baseball statistics
df2 <- df[, c(1, 2, 11, 20, 21)]    # Raw measurements
df <- df[, -c(3, 4, 6, 18, 19, 24)]    # All variables

# Rename columns for convenience
colnames(df) <- c("last_name", "first_name", "ab", 
                  "hits", "singles", "doubles", "triples", "home_runs",
                  "strikeouts", "walks", "ba", "slg", "obp", "rbi",
                  "ev", "la", "sweet_spot", "barrel_rate")

colnames(df1) <- c("last_name", "first_name", "ab", "hits", "singles", "doubles", "triples", "home_runs",
                   "ba", "slg")

colnames(df2) <- c("last_name", "first_name", "home_runs", "ev", "la")
  
head(df, 10)


plot(df$ba, df$home_runs, xlab = "Batting Average", ylab = "Home Runs", col = "red")
plot(df$hits, df$home_runs, xlab = "Hits", ylab = "Home Runs", col = "red")


plot(df$ev, df$home_runs, xlab = "Exit Velocity", ylab = "Home Runs", col = "blue")
plot(df$la, df$home_runs, xlab = "Launch Angle", ylab = "Home Runs", col = "blue")
plot(df$ev, df$la, xlab = "Exit Velocity", ylab = "Launch Angle", col = "black")


g <- lm(home_runs ~ singles + doubles + triples, data = df)
summary(g)

gdumb <- lm(home_runs ~ ba, data = df)
summary(gdumb)



######################################################################

# Kernel density estimates 
m <- 100
n <- nrow(df)
x <- df$ev
g <- seq(from = min(df$ev), to = max(df$ev), length = m)
bw <- bw.nrd(x)

y <- .C('kde', as.integer(m), as.integer(n), as.double(g), as.double(bw), as.double(x), y = double(m))$y
plot(g, y, type = 'l', col = 'blue', main = "Kernel Density Estimate of EV", xlab = "Exit Velocity", ylab = "Density")


x <- df$la
g <- seq(from = min(df$la), to = max(df$la), length = m)
bw <- bw.nrd(x)

y <- .C('kde', as.integer(m), as.integer(n), as.double(g), as.double(bw), as.double(x), y = double(m))$y
plot(g, y, type = 'l', col = 'blue', main = "Kernel Density Estimate of LA", xlab = "Launch Angle", ylab = "Density")



x <- df$home_runs
g <- seq(from = min(df$home_runs), to = max(df$home_runs), length = m)
bw <- bw.nrd(x)

y <- .C('kde', as.integer(m), as.integer(n), as.double(g), as.double(bw), as.double(x), y = double(m))$y
plot(g, y, type = 'l', col = 'blue', main = "Kernel Density Estimate of HRs", xlab = "Home Runs", ylab = "Density")


x <- df$barrel_rate
g <- seq(from = min(df$home_runs), to = max(df$home_runs), length = m)
bw <- bw.nrd(x)

y <- .C('kde', as.integer(m), as.integer(n), as.double(g), as.double(bw), as.double(x), y = double(m))$y
plot(g, y, type = 'l', col = 'blue', main = "Kernel Density Estimate of Barrels", xlab = "Barrels", ylab = "Density")



######################################################################

# Kernel Regression (also Generalized Additive Model (GAM)?)
n <- length(x)
x <- df$ev
y <- df$home_runs
bw <- bw.nrd(x)
g2 <- seq(min(x), max(x), length = m)

densities <- .C('kde2', as.integer(m), as.integer(n), as.double(g2), as.double(bw), as.double(x), as.double(y), res2 = double(m))$res2

plot(g2, densities, type = 'l', col = 'blue', main = "HR Kernel Regression Using EV", xlab = "Exit Velocity", ylab = "Density")

# Kernel regression plot with confidence interval bands
samp_i <- sample(1:length(x), size = 100, replace = TRUE)
samp_x <- x[samp_i]
samp_y <- y[samp_i]

n <- length(samp_x)    # 100
g2 <- seq(min(x), max(x), length.out = m)

estimates_200 <- list()
for (j in 1:200){
  samp_i <- sample(1:length(x), size = 100, replace = TRUE)
  samp_x <- x[samp_i]
  samp_y <- y[samp_i]
  d <- .C('kde2', as.integer(m), as.integer(n), as.double(g2), as.double(bw), as.double(samp_x), as.double(samp_y), res2 = double(m))$res2
  estimates_200 <- c(estimates_200, list(d))
}

percentiles <- list()
for (i in 1:100){
  m2.5 <- sort(sapply(estimates_200, '[[', i))[5] # 2.5th percentile of each m 
  m97.5 <- sort(sapply(estimates_200, '[[', i))[195] # 97.5th percentile of each m 
  percentiles <- c(percentiles, list(c(m2.5, m97.5)))
}

lower <- unlist(lapply(percentiles, `[[`, 1))
upper <- unlist(lapply(percentiles, `[[`, 2))
ci <- data.frame(x = g2, y = densities, lower = lower, upper = upper)

p <- ggplot(data=ci, aes(x=x, y=y)) + geom_line()
p <- p+geom_ribbon(aes(ymin=lower, ymax=upper), linetype=2, col = 'blue', fill = 'blue', alpha = 0.2) + ggtitle("Kernel Regression of Home Runs ~ Exit Velocity") + theme(plot.title = element_text(hjust = 0.5)) + labs(x = 'EV', y = 'HR')
p



n <- length(x)
x <- df$la
y <- df$home_runs
bw <- bw.nrd(x)
g2 <- seq(min(x), max(x), length = m)

densities <- .C('kde2', as.integer(m), as.integer(n), as.double(g2), as.double(bw), as.double(x), as.double(y), res2 = double(m))$res2

plot(g2, densities, type = 'l', col = 'blue', main = "HR Kernel Regression Using LA", xlab = "Launch Angle", ylab = "Density", ylim = range(0:40))

# Kernel regression plot with confidence interval bands
samp_i <- sample(1:length(x), size = 100, replace = TRUE)
samp_x <- x[samp_i]
samp_y <- y[samp_i]

n <- length(samp_x)    # 100
g2 <- seq(min(x), max(x), length.out = m)

estimates_200 <- list()
for (j in 1:200){
  samp_i <- sample(1:length(x), size = 100, replace = TRUE)
  samp_x <- x[samp_i]
  samp_y <- y[samp_i]
  d <- .C('kde2', as.integer(m), as.integer(n), as.double(g2), as.double(bw), as.double(samp_x), as.double(samp_y), res2 = double(m))$res2
  estimates_200 <- c(estimates_200, list(d))
}

percentiles <- list()
for (i in 1:100){
  m2.5 <- sort(sapply(estimates_200, '[[', i))[5] # 2.5th percentile of each m 
  m97.5 <- sort(sapply(estimates_200, '[[', i))[195] # 97.5th percentile of each m 
  percentiles <- c(percentiles, list(c(m2.5, m97.5)))
}

lower <- unlist(lapply(percentiles, `[[`, 1))
upper <- unlist(lapply(percentiles, `[[`, 2))
ci <- data.frame(x = g2, y = densities, lower = lower, upper = upper)

p <- ggplot(data=ci, aes(x=x, y=y)) + geom_line()
p <- p+geom_ribbon(aes(ymin=lower, ymax=upper), linetype=2, col = 'blue', fill = 'blue', alpha = 0.2) + ggtitle("Kernel Regression of Home Runs ~ Launch Angle") + theme(plot.title = element_text(hjust = 0.5)) + labs(x = 'LA', y = 'HR')
p



n <- length(x)
x <- df$barrel_rate
y <- df$home_runs
bw <- bw.nrd(x)
g2 <- seq(min(x), max(x), length = m)

densities <- .C('kde2', as.integer(m), as.integer(n), as.double(g2), as.double(bw), as.double(x), as.double(y), res2 = double(m))$res2

plot(g2, densities, type = 'l', col = 'blue', main = "HR Kernel Regression Using LA", xlab = "Launch Angle", ylab = "Density", ylim = range(0:40))

# Kernel regression plot with confidence interval bands
samp_i <- sample(1:length(x), size = 100, replace = TRUE)
samp_x <- x[samp_i]
samp_y <- y[samp_i]

n <- length(samp_x)    # 100
g2 <- seq(min(x), max(x), length.out = m)

estimates_200 <- list()
for (j in 1:200){
  samp_i <- sample(1:length(x), size = 100, replace = TRUE)
  samp_x <- x[samp_i]
  samp_y <- y[samp_i]
  d <- .C('kde2', as.integer(m), as.integer(n), as.double(g2), as.double(bw), as.double(samp_x), as.double(samp_y), res2 = double(m))$res2
  estimates_200 <- c(estimates_200, list(d))
}

percentiles <- list()
for (i in 1:100){
  m2.5 <- sort(sapply(estimates_200, '[[', i))[5] # 2.5th percentile of each m 
  m97.5 <- sort(sapply(estimates_200, '[[', i))[195] # 97.5th percentile of each m 
  percentiles <- c(percentiles, list(c(m2.5, m97.5)))
}

lower <- unlist(lapply(percentiles, `[[`, 1))
upper <- unlist(lapply(percentiles, `[[`, 2))
ci <- data.frame(x = g2, y = densities, lower = lower, upper = upper)

p <- ggplot(data=ci, aes(x=x, y=y)) + geom_line()
p <- p+geom_ribbon(aes(ymin=lower, ymax=upper), linetype=2, col = 'blue', fill = 'blue', alpha = 0.2) + ggtitle("Kernel Regression of Home Runs ~ Barrels") + theme(plot.title = element_text(hjust = 0.5)) + labs(x = 'Barrels', y = 'HR')
p



######################################################################

# Linear Regression
g <- lm(home_runs ~ ev + la, data = df)
summary(g)
plot(g)

g2 <- lm(home_runs ~ barrel_rate + doubles, data = df)
summary(g2)
plot(g2)

# We don't even care about power anymore and our model is still better
# Adding exit velo back in doesn't do anything

g3 <- lm(home_runs ~ barrel_rate, data = df)
summary(g3)

g4 <- lm(home_runs ~ ev + la + doubles, data = df)
summary(g4)



