library(tidyverse)
library(lmtest)
library(sandwich)
library(Metrics)
library(car)
library(AER)
data <- read_csv("grain.csv")



# ----- Task 5 ----- 

lm.2 <- lm(log(quantity) ~ log(price) + 
           seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12, data = data)
coeftest(lm.5, vcov. = vcovHC(lm.5, 'HC1'))
# the estimated demand elasticity is -0.66 with SE = 0.075



# ----- Task 7 ----- 

lm.1 <- lm(log(price) ~ cartel + 
           seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12, data = data)

lm.3 <- ivreg(log(quantity) ~ log(price) + 
              seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12 | 
              seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12 + cartel, data = data)
lm.3.2 <- 

lm.4 <- ivreg(log(quantity) ~ log(price) + 
              seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12 | 
              seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12 + ice, data = data)

lm.5 <- ivreg(log(quantity) ~ log(price) + 
              seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12 | 
              seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12 + cartel + ice, data = data)

coeftest(lm.1, vcov. = vcovHC(lm.1, "HC1"))

rows <- c('Estimated coefficient on smkban', '(standard errors in parentheses)', 'Predicted probabilities of smoking for Mr. A:', '(i) with smoking ban', '(ii) without smoking ban', 'Difference, (i)- (ii)',
          'Predicted probabilities of smoking for Mr. B:', '(iii) with smoking ban', '(iv) without smoking ban', 'Difference, (iii)- (iv)')

myH0 <- c("seas1", "seas2", "seas3", "seas4", "seas5", "seas6", "seas7", "seas8", "seas9", "seas10", "seas11", "seas12")
a <- linearHypothesis(lm.1, myH0, vcov. = vcovHC(lm.1, "HC1"))

one <- c(lm.1$coefficients[2],
         coeftest(lm.1, vcov.=vcovHC(lm.1, "HC1"))[2,2],
         NA,
         linearHypothesis(lm.1, myH0, vcov. = vcovHC(lm.1, "HC1"))[2,3],
         linearHypothesis(lm.1, myH0, vcov. = vcovHC(lm.1, "HC1"))[2,4],
         NA,
         NA,
         NA,
         NA)

two <- c(lm.2$coefficients[2],
         NA,
         coeftest(lm.2, vcov.=vcovHC(lm.2, "HC1"))[2,2],
         linearHypothesis(lm.2, myH0, vcov. = vcovHC(lm.2, "HC1"))[2,3],
         linearHypothesis(lm.2, myH0, vcov. = vcovHC(lm.2, "HC1"))[2,4],
         NA,
         NA,
         NA,
         NA)

three <- c(lm.3$coefficients[2],
           NA,
           coeftest(lm.3, vcov.=vcovHC(lm.3, "HC1"))[2,2],
           linearHypothesis(lm.3, myH0, vcov. = vcovHC(lm.3, "HC1"))[2,3],
           linearHypothesis(lm.3, myH0, vcov. = vcovHC(lm.3, "HC1"))[2,4],
           NA,
           NA,
           NA,
           NA)