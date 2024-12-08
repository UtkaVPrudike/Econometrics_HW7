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
coeftest(lm.2, vcov. = vcovHC(lm.2, 'HC1'))
# the estimated demand elasticity is -0.66 with SE = 0.075

# This estimate is probably biased, because interaction of supply and demand introduces simultaneous causality bias.
# When a non-monetary factor shifts the demand curve to the right, the demand quantity increases and thus drives the price up, ceteris paribus
# The opposite situation occurs when a non-monetary factor shifts the demand curve left.



# ----- Task 7 ----- 

lm.1 <- lm(log(price) ~ cartel + 
           seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12, data = data)

lm.3 <- ivreg(log(quantity) ~ log(price) + 
              seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12 | 
              seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12 + cartel, data = data)

lm.3.2 <- lm(log(price) ~ cartel +
             seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12, data = data)

lm.4 <- ivreg(log(quantity) ~ log(price) + 
              seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12 | 
              seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12 + ice, data = data)

lm.4.2 <- lm(log(price) ~ ice +
             seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12, data = data)

lm.5 <- ivreg(log(quantity) ~ log(price) + 
              seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12 | 
              seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12 + cartel + ice, data = data)

lm.5.2 <- lm(log(price) ~ cartel + ice +
             seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12, data = data)

data <- data %>% mutate(res.5 = lm.5$residuals)

lm.5.3 <- lm(res.5 ~ cartel + ice +
             seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12, data = data)

jtest.5 <- linearHypothesis(lm.5.3, c("ice", "cartel"))

lm.6 <- ivreg(log(quantity) ~ log(price) + 
                seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12 + ice | 
                seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12 + cartel + ice, data = data)

lm.6.2 <- lm(log(price) ~ cartel + ice +
               seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12, data = data)

myH0 <- c("seas1", "seas2", "seas3", "seas4", "seas5", "seas6", "seas7", "seas8", "seas9", "seas10", "seas11", "seas12")


rows <- c('cartel', '(SE)', 'ln(P)', '(SE)', 'F statistic testing coefficients on monthly indicators', '(p-value)', 'First-stage F statistic', '(p-value)',
          'J-test of overidentifying restrictions', '(p-value)')

one <- c(lm.1$coefficients[2],
         coeftest(lm.1, vcov.=vcovHC(lm.1, "HC1"))[2,2],
         NA,
         NA,
         linearHypothesis(lm.1, myH0, vcov. = vcovHC(lm.1, "HC1"))[2,3],
         linearHypothesis(lm.1, myH0, vcov. = vcovHC(lm.1, "HC1"))[2,4],
         NA,
         NA,
         NA,
         NA)

two <- c(NA,
         NA,
         lm.2$coefficients[2],
         coeftest(lm.2, vcov.=vcovHC(lm.2, "HC1"))[2,2],
         linearHypothesis(lm.2, myH0, vcov. = vcovHC(lm.2, "HC1"))[2,3],
         linearHypothesis(lm.2, myH0, vcov. = vcovHC(lm.2, "HC1"))[2,4],
         NA,
         NA,
         NA,
         NA)

three <- c(NA,
           NA,
           lm.3$coefficients[2],
           coeftest(lm.3, vcov.=vcovHC(lm.3, "HC1"))[2,2],
           linearHypothesis(lm.3, myH0, vcov. = vcovHC(lm.3, "HC1"))[2,3] / 12,
           linearHypothesis(lm.3, myH0, vcov. = vcovHC(lm.3, "HC1"))[2,4],
           linearHypothesis(lm.3.2, c("cartel"), vcov. = vcovHC(lm.3.2, "HC1"))[2,3],
           linearHypothesis(lm.3.2, c("cartel"), vcov. = vcovHC(lm.3.2, "HC1"))[2,4],
           NA,
           NA)

four <- c(NA,
          NA,
          lm.4$coefficients[2],
          coeftest(lm.4, vcov.=vcovHC(lm.4, "HC1"))[2,2],
          linearHypothesis(lm.4, myH0, vcov. = vcovHC(lm.4, "HC1"))[2,3] / 12,
          linearHypothesis(lm.4, myH0, vcov. = vcovHC(lm.4, "HC1"))[2,4],
          linearHypothesis(lm.4.2, c("ice"), vcov. = vcovHC(lm.4.2, "HC1"))[2,3],
          linearHypothesis(lm.4.2, c("ice"), vcov. = vcovHC(lm.4.2, "HC1"))[2,4],
          NA,
          NA)

five <- c(NA,
          NA,
          lm.5$coefficients[2],
          coeftest(lm.5, vcov.=vcovHC(lm.5, "HC1"))[2,2],
          linearHypothesis(lm.5, myH0, vcov. = vcovHC(lm.5, "HC1"))[2,3] / 12,
          linearHypothesis(lm.5, myH0, vcov. = vcovHC(lm.5, "HC1"))[2,4],
          linearHypothesis(lm.5.2, c("cartel", "ice"), vcov. = vcovHC(lm.5.2, "HC1"))[2,3],
          linearHypothesis(lm.5.2, c("cartel", "ice"), vcov. = vcovHC(lm.5.2, "HC1"))[2,4],
          jtest.5[2,5] * 2,
          1 - pchisq(jtest.5[2,5] * 2, 1))

six <- c(NA,
         NA,
         lm.6$coefficients[2],
         coeftest(lm.6, vcov.=vcovHC(lm.6, "HC1"))[2,2],
         linearHypothesis(lm.6, myH0, vcov. = vcovHC(lm.6, "HC1"))[2,3] / 12,
         linearHypothesis(lm.6, myH0, vcov. = vcovHC(lm.6, "HC1"))[2,4],
         linearHypothesis(lm.6.2, c("cartel", "ice"), vcov. = vcovHC(lm.6.2, "HC1"))[2,3],
         linearHypothesis(lm.6.2, c("cartel", "ice"), vcov. = vcovHC(lm.6.2, "HC1"))[2,4],
         NA,
         NA)


my_table <- tibble(rows = rows, one, two, three, four, five, six) %>%
  mutate(across(2:7, ~ round(.x, 3))) %>%
  mutate(across(2:7, as.character)) %>%
  mutate(across(2:7, ~ replace_na(.x, '-'))) %>%
  mutate(across(2:7, ~ case_when(rows == '(SE)' & .x != '-' ~  paste('(', .x, ')', sep=''),
                                 rows == '(p-value)' & .x != '-' ~  paste('(', .x, ')', sep=''),
                                 .default = .x) )) %>%
  add_row(rows = 'Instrumental variables', one = '-', two = '-', three = 'cartel', four = 'ice',  five = 'cartel, ice', six = 'cartel', .before = 7) %>%
  add_row(rows = 'Estimation method', one = 'OLS', two = 'OLS', three = 'TSLS', four = 'TSLS',  five = 'TSLS', six = 'TSLS', .before = 7) %>%
  add_row(rows = 'Dependent variable:', one = 'ln(P)', two = 'ln(Q)', three = 'ln(Q)', four = 'ln(Q)',  five = 'ln(Q)', six = 'ln(Q)', .before = 1)

my_table
# Regression six includes ice as a control variable and uses cartel as the instrument
