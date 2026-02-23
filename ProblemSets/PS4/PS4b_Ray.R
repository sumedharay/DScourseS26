# PS4b - sparklyr Exercise

library(sparklyr)
library(tidyverse)

# Connect to Spark
sc <- spark_connect(master = "local")

# Load iris as tibble
df1 <- as_tibble(iris)

# Copy to Spark
df <- copy_to(sc, df1)

# (7) Check classes
print(class(df1))
print(class(df))

# (8) Check column names
print(colnames(df1))
print(colnames(df))

# (9) Select operation
df %>% select(Sepal_Length, Species) %>% head %>% print

# (10) Filter operation
df %>% filter(Sepal_Length > 5.5) %>% head %>% print

# (11) Combined select + filter
df %>% filter(Sepal_Length > 5.5) %>% select(Sepal_Length, Species) %>% head %>% print

# (12) Group by + summarize
df2 <- df %>% group_by(Species) %>%
  summarize(mean = mean(Sepal_Length), count = n())
df2 %>% head %>% print

# (13) Arrange
df2 %>% arrange(Species) %>% head %>% print
