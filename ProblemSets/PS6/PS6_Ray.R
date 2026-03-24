# ============================================================
# PS6_Ray.R
# Econ 5253 - Data Science for Economists, Spring 2026
# Author: Sumedha Ray
# Description: Parse, clean, and visualize ProQuest dissertation data
# ============================================================

# ---- 0. Load libraries ----
options(bitmapType = "cairo")
library(tidyverse)
library(scales)

# ---- 1. Parse the RIS file ----
# RIS files are plain text with tagged fields (e.g., "T1  - Title").
# Each record ends with "ER  -". We read the file line by line and
# extract the fields we need.

ris_path <- "ProQuestDocuments-2026-03-10.ris"
lines    <- readLines(ris_path, encoding = "UTF-8", warn = FALSE)

# Helper: extract the value after the two-letter tag
get_field <- function(lines, tag) {
  pattern <- paste0("^", tag, "  - (.+)$")
  matches <- regmatches(lines, regexpr(pattern, lines, perl = TRUE))
  if (length(matches) == 0) return(NA_character_)
  sub(pattern, "\\1", matches[1])
}

# Split into individual records on the "ER  -" end-of-record marker
record_breaks <- which(grepl("^ER  -", lines))
record_starts <- c(1, head(record_breaks + 1, -1))

records <- mapply(function(s, e) lines[s:e],
                  record_starts,
                  record_breaks,
                  SIMPLIFY = FALSE)

# Parse each record into a one-row data frame
parse_record <- function(rec) {
  # Keywords: collect all KW lines
  kw_lines <- rec[grepl("^KW  - ", rec)]
  keywords  <- sub("^KW  - ", "", kw_lines)

  # Primary subfield: coded as "NNNN:SubfieldName" in KW lines
  coded_kws  <- keywords[grepl("^[0-9]{4}:", keywords)]
  subfield   <- if (length(coded_kws) > 0)
                  sub("^[0-9]{4}:", "", coded_kws[1])
                else NA_character_

  # Sensitive topic flag: dissertations mentioning race, gender,
  # religion, or sexuality in their keywords
  sensitive_terms <- c("race", "gender", "religion", "sexuality",
                        "discrimination", "ethnicity", "immigrant",
                        "poverty", "inequality", "drug", "alcohol",
                        "crime", "prison", "HIV", "abortion",
                        "minority", "refugee", "violence")
  is_sensitive <- any(str_detect(tolower(paste(keywords, collapse = " ")),
                                 paste(sensitive_terms, collapse = "|")))

  # Multiple advisors: collect all A3 lines
  advisors <- sub("^A3  - ", "", rec[grepl("^A3  - ", rec)])
  advisor  <- if (length(advisors) > 0) paste(advisors, collapse = "; ") else NA_character_

  tibble(
    title      = get_field(rec, "T1"),
    author     = get_field(rec, "AU"),
    advisor    = advisor,
    year       = as.integer(get_field(rec, "Y1")),
    university = get_field(rec, "PB"),
    state      = sub("United States -- ", "", get_field(rec, "CY")),
    pages      = as.integer(get_field(rec, "SP")),
    subfield   = subfield,
    is_sensitive = is_sensitive,
    keywords_raw = paste(keywords, collapse = "; ")
  )
}

df_raw <- bind_rows(lapply(records, parse_record))

# ---- 2. Clean the data ----

df <- df_raw %>%
  # Remove records with missing title or university
  filter(!is.na(title), !is.na(university)) %>%
  # Clean up subfield labels: trim whitespace
  mutate(subfield = str_trim(subfield)) %>%
  # Recode a handful of near-duplicate subfield labels
  mutate(subfield = case_when(
    subfield == "Agricultural economics" ~ "Agricultural Economics",
    subfield == "Environmental economics" ~ "Environmental Economics",
    subfield == "Labor economics"         ~ "Labor Economics",
    subfield == "Economic theory"         ~ "Economic Theory",
    subfield == "Economics"               ~ "Economics",
    TRUE                                  ~ subfield
  )) %>%
  # Pages: drop implausible values (< 10 or > 600)
  mutate(pages = if_else(pages < 10 | pages > 600, NA_integer_, pages))

# Save cleaned data as CSV for reproducibility
write_csv(df, "dissertations_clean.csv")

cat("Records parsed:", nrow(df), "\n")
cat("Fields: title, author, advisor, year, university, state, pages,",
    "subfield, is_sensitive\n")

# ============================================================
# VISUALIZATION 1: Top 15 Universities by Dissertation Count
# ============================================================

top_unis <- df %>%
  count(university, name = "n") %>%
  top_n(15, n) %>%
  mutate(university = fct_reorder(university, n))

p1 <- ggplot(top_unis, aes(x = n, y = university)) +
  geom_col(fill = "#2C5F8A", width = 0.7) +
  geom_text(aes(label = n), hjust = -0.2, size = 3.2, color = "gray20") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Top 15 Universities by Economics Dissertation Output (2015)",
    subtitle = "Based on 100 dissertations from ProQuest Dissertations & Theses Global",
    x        = "Number of Dissertations",
    y        = NULL,
    caption  = "Source: ProQuest Dissertations & Theses Global"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "gray40", size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y   = element_text(size = 9)
  )

ggsave("PS6a_Ray.png", plot = p1, width = 8, height = 6, dpi = 300)

# ============================================================
# VISUALIZATION 2: Dissertation Count by Subfield
# ============================================================

top_subfields <- df %>%
  filter(!is.na(subfield)) %>%
  count(subfield, name = "n") %>%
  top_n(12, n) %>%
  mutate(subfield = fct_reorder(subfield, n))

p2 <- ggplot(top_subfields, aes(x = n, y = subfield)) +
  geom_col(fill = "#5B8C5A", width = 0.7) +
  geom_text(aes(label = n), hjust = -0.2, size = 3.2, color = "gray20") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Economics Dissertations by Subfield (2015)",
    subtitle = "Primary subfield classification from ProQuest subject codes",
    x        = "Number of Dissertations",
    y        = NULL,
    caption  = "Source: ProQuest Dissertations & Theses Global"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "gray40", size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y   = element_text(size = 9)
  )

ggsave("PS6b_Ray.png", plot = p2, width = 8, height = 6, dpi = 300)

# ============================================================
# VISUALIZATION 3: Dissertation Length by Subfield (Box Plot)
# ============================================================

# Keep subfields with at least 5 observations for meaningful boxes
subfields_5plus <- df %>%
  filter(!is.na(subfield), !is.na(pages)) %>%
  count(subfield) %>%
  filter(n >= 5) %>%
  pull(subfield)

df_pages <- df %>%
  filter(subfield %in% subfields_5plus, !is.na(pages)) %>%
  mutate(subfield = fct_reorder(subfield, pages, .fun = median))

p3 <- ggplot(df_pages, aes(x = pages, y = subfield)) +
  geom_boxplot(fill = "#8C5A8C", alpha = 0.7, outlier.color = "gray50",
               outlier.size = 1.5, width = 0.6) +
  geom_jitter(height = 0.15, alpha = 0.4, size = 1.5, color = "#8C5A8C") +
  labs(
    title    = "Dissertation Length by Subfield (2015)",
    subtitle = "Subfields with at least 5 dissertations; ordered by median page count",
    x        = "Number of Pages",
    y        = NULL,
    caption  = "Source: ProQuest Dissertations & Theses Global"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "gray40", size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y   = element_text(size = 9)
  )

ggsave("PS6c_Ray.png", plot = p3, width = 8, height = 6, dpi = 300)

cat("Done! Three plots saved: PS6a_Ray.png, PS6b_Ray.png, PS6c_Ray.png\n")
