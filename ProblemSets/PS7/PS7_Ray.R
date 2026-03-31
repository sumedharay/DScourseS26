# PS7_Ray.R
# Econ 5253 - Spring 2026

library(tidyverse)
library(mice)
library(stargazer)

wages <- read.csv("wages.csv")
wages <- wages %>% filter(!is.na(hgc), !is.na(tenure))

missing_rate <- mean(is.na(wages$logwage))
cat("Missing rate for logwage:", round(missing_rate * 100, 2), "%\n")

stargazer(as.data.frame(wages),
          type="latex", out="summary_table.tex",
          title="Summary Statistics", summary=TRUE, digits=2)

wages_cc <- wages %>% filter(!is.na(logwage))
model_cc <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data=wages_cc)

wages_mean <- wages %>%
  mutate(logwage = ifelse(is.na(logwage), mean(logwage, na.rm=TRUE), logwage))
model_mean <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data=wages_mean)

wages_pred <- wages %>%
  mutate(logwage = ifelse(is.na(logwage),
                          predict(model_cc, newdata=wages[is.na(wages$logwage),]),
                          logwage))
model_pred <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data=wages_pred)

stargazer(model_cc, model_mean, model_pred,
          type="latex", out="regression_table.tex",
          title="Returns to Schooling Under Different Imputation Methods",
          column.labels=c("Complete Cases", "Mean Imputation", "Predicted Values"),
          covariate.labels=c("Years of Schooling (hgc)", "College",
                             "Tenure", "Tenure$^2$", "Age", "Married"),
          dep.var.labels="Log Wage",
          digits=4, no.space=TRUE,
          notes="True value of $\\hat{\\beta}_1 = 0.093$.",
          notes.append=FALSE)

cat("regression_table.tex written successfully\n")

wages_mice <- mice(wages, m=5, method="pmm", seed=12345, printFlag=FALSE)
model_mice_fit <- with(wages_mice,
                       lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married))
model_mice_pooled <- pool(model_mice_fit)

cat("\n--- MICE Pooled Results ---\n")
print(summary(model_mice_pooled))
