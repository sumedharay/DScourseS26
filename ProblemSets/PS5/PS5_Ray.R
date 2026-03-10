## ============================================================
## Econ 5253 - Problem Set 5
## Sumedha Ray
## ============================================================

## ---- Install and load packages ----
# Uncomment to install if not already installed:
# install.packages(c("rvest", "dplyr", "stringr", "wbstats", "ggplot2", "knitr"))

library(rvest)
library(dplyr)
library(stringr)
library(wbstats)
library(ggplot2)
library(knitr)


## ============================================================
## Question 3: Wikipedia Data
## ============================================================

url <- "https://en.wikipedia.org/wiki/List_of_Indian_states_and_union_territories_by_literacy_rate"
page <- read_html(url)
tables <- page %>% html_nodes("table") %>% html_table(fill = TRUE)

# Table 3 has state-level literacy rates by census year (1951-2011)
literacy_df <- tables[[3]]

# Fix column names
colnames(literacy_df) <- c("state", "1951", "1961", "1971", "1981", "1991", "2001", "2011")

# Remove the first row if it's a duplicate header
literacy_df <- literacy_df %>% filter(state != "State/UT", state != "")

print(paste("States scraped:", nrow(literacy_df)))
print(head(literacy_df, 10))

write.csv(literacy_df, "india_literacy.csv", row.names = FALSE)
cat("Saved: india_literacy.csv\n")


## ============================================================
## Question 4: API — World Bank Data 
## ============================================================

indicators <- c(
  "bank_account"   = "FX.OWN.TOTL.FE.ZS",
  "homicide_rate"  = "VC.IHR.PSRC.P5",
  "fertility_rate" = "SP.DYN.TFRT.IN",
  "maternal_mort"  = "SH.STA.MMRT"
)


httr::set_config(httr::timeout(60))
wb_data <- wb_data(
  indicator = indicators,
  country   = "IN",
  start_date = 1990,
  end_date   = 2023
)

# Rename for clarity and reorder
wb_clean <- wb_data %>%
  select(country, date, bank_account, homicide_rate, fertility_rate, maternal_mort) %>%
  arrange(date)

print(paste("World Bank observations:", nrow(wb_clean)))
print(head(wb_clean, 10))

# Save to CSV
write.csv(wb_clean, "india_wb_indicators.csv", row.names = FALSE)
cat("Saved: india_wb_indicators.csv\n")


p1 <- ggplot(wb_clean %>% filter(!is.na(bank_account)),
             aes(x = date, y = bank_account)) +
  geom_line(color = "#2c7bb6", linewidth = 1) +
  geom_point(color = "#2c7bb6", size = 1.5) +
  labs(
    title   = "Women Owning a Bank Account — India",
    subtitle = "World Bank Findex Data",
    x = "Year", y = "% of female population",
    caption = paste0("Number of observations: ", sum(!is.na(wb_clean$bank_account)))
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p1)
ggsave("india_bank_account.pdf", p1, width = 7, height = 4)

p2 <- ggplot(wb_clean %>% filter(!is.na(homicide_rate)),
             aes(x = date, y = homicide_rate)) +
  geom_line(color = "#d7191c", linewidth = 1) +
  geom_point(color = "#d7191c", size = 1.5) +
  labs(
    title    = "Intentional Homicide Rate — India",
    subtitle = "World Bank Data, 1990–2023",
    x = "Year", y = "Per 100,000 people",
    caption = paste0("Number of observations: ", sum(!is.na(wb_clean$homicide_rate)))
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p2)
ggsave("india_homicide.pdf", p2, width = 7, height = 4)


getwd()
