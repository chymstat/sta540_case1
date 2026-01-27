library(tidyverse)
library(readxl)


### read and clean the data
df <- read.csv('CTN_FINAL.csv')
dict <- read_excel("CTN0083-Data-Dictionary.xlsx")
colnames(dict) <- dict[2, ]
dict <- dict[-c(1, 2), ]

### find appropriate observations
filtered_data <- df %>%
  filter(PO_FLAG == "Include" & WAVE != 3)

### filter data for table1
t1_dt <- filtered_data %>% 
  select(Q3_1,
         Q5_1,
         Q5_3,
         Q6_2,
         Q11_2,
         Q11_3,
         Q11_4,
         Q11_5,
         LAST_HIV_TEST_MONTHS,
         Q11_7)

t1_fields <- c('Q3_1',
              'Q5_1',
              'Q5_3',
              'Q6_2',
              'Q11_2',
              'Q11_3',
              'Q11_4',
              'Q11_5',
              'Last_hiv_test_months',
              'Q11_7')

t1_dict <- dict %>% 
  filter(FIELD_ID %in% t1_fields)

library(stringr)
### q31 age summary of participants
age_summary <- t1_dt %>%
  summarise(
    age_label  = "Age (years)",
    median = median(Q3_1, na.rm = TRUE),
    q1     = quantile(Q3_1, 0.25, na.rm = TRUE),
    q3     = quantile(Q3_1, 0.75, na.rm = TRUE)
  )

### q51 ethnicity
latino_summary <- t1_dt %>%
  summarise(
    ethnicity_label = "Hispanic/Latinx",
    n = sum(Q5_1 == 1, na.rm = TRUE),
    prop = n / sum(!is.na(Q5_1))
  )

### q53 race
race_labels <- c(
  '24'='Black or African American',
  '23'='White',
  '28'='Other',
  '25'='American Indian or Alaskan Native'
)
# clean the data and handle special cases
race_count <- t1_dt %>%
  mutate(
    race_class = case_when(
      is.na(Q5_3) | Q5_3 == "" ~ "NA",
      str_detect(as.character(Q5_3), ",") ~ "Multiracial",
      TRUE ~ as.character(Q5_3)
    ),
    race_label = recode(race_class, !!!race_labels, .default = race_class)
  ) %>%
  count(race_label, name = "n") %>%
  mutate(
    prop = if_else(
      race_label == "NA",
      0,
      n / sum(n[race_label != "NA"])
    )
  ) %>%
  arrange(race_label) 
  
### q62 prep history
prep_summary <- t1_dt %>%
  mutate(
    prep_status = case_when(
      Q6_2 == 1 ~ "In the past 6 months took PrEP",
      Q6_2 == 3 ~ "Never took PrEP",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(prep_status)) %>%
  count(prep_status, name = "n") %>%
  mutate(
    prop = n / sum(n)
  )

### q112 num of partners
sex_partner_summary <- t1_dt %>%
  summarise(
    partner_num_label  = "Number of male sex partners in the past 90 days",
    median = median(Q11_2, na.rm = TRUE),
    q1     = quantile(Q11_2, 0.25, na.rm = TRUE),
    q3     = quantile(Q11_2, 0.75, na.rm = TRUE)
  )

### q113 condom use
condom_summary <- t1_dt %>%
  mutate(
    condom_status = case_when(
      Q11_3 == 1 ~ "Never",
      Q11_3 == 2 ~ "Sometimes",
      Q11_3 == 3 ~ "About half the time",
      Q11_3 == 4 ~ "Most of the time",
      Q11_3 == 5 ~ "Always",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(condom_status)) %>%
  count(condom_status, name = "n") %>%
  mutate(
    prop = n / sum(n)
  )

### q114 receptive history
receptive_summary <- t1_dt %>%
  summarise(
    receptive_label = "Condomless receptive anal sex in the past 90 days",
    n = sum(Q11_4 == 1, na.rm = TRUE),
    prop = n / sum(!is.na(Q11_4))
  )

### q115 hiv test history
hivtest_summary <- t1_dt %>%
  summarise(
    `Ever tested for HIV during lifetime` = sum(Q11_5 == 1, na.rm = TRUE),
    `Not tested for HIV` = sum(Q11_5 == 2, na.rm = TRUE),
    total_n = sum(!is.na(Q11_5))
  ) %>%
  pivot_longer(
    cols = -total_n,
    names_to = "label",
    values_to = "n"
  ) %>%
  mutate(
    prop = n / total_n
  ) %>%
  select(label, n, prop)


### q116 time to last hiv test
hiv_test_time <- t1_dt %>%
  filter(LAST_HIV_TEST_MONTHS < 9999) %>%
  summarise(
    time_label  = "Months since last HIV test",
    median = median(LAST_HIV_TEST_MONTHS, na.rm = TRUE),
    q1     = quantile(LAST_HIV_TEST_MONTHS, 0.25, na.rm = TRUE),
    q3     = quantile(LAST_HIV_TEST_MONTHS, 0.75, na.rm = TRUE)
  )

### q117 reasons for no hiv test
nohiv_test_reason <- t1_dt %>%
  filter(LAST_HIV_TEST_MONTHS == 9999) %>%
  mutate(
    reasons = case_when(
      Q11_7 == 1 ~ "Unlikely to be exposed to HIV",
      Q11_7 == 2 ~ "Afraid of testing HIV-positive",
      Q11_7 == 3 ~ "Did not want to think about HIV/HIV-positive",
      Q11_7 == 4 ~ "Worried about names being reported if positive",
      Q11_7 == 5 ~ "Dislike for needles",
      Q11_7 == 6 ~ "Unable to trust that the results will be confidential",
      Q11_7 == 7 ~ "Unaware of where to get tested",
      Q11_7 == 8 ~ "Other reasons",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(reasons)) %>%
  count(reasons, name = "n") %>%
  mutate(
    prop = n / sum(n)
  )

### form blocks for output
age_row <- age_summary %>%
  transmute(
    Characteristic = "Age in years, median (IQR)",
    Value = sprintf("%d (%d–%d)", median, q1, q3)
  )

ethnicity_block <- tibble(
  Characteristic = "Ethnicity, n (%)",
  Value = ""
)
ethnicity_rows <- latino_summary %>%
  transmute(
    Characteristic = paste0("&nbsp;&nbsp;", ethnicity_label), 
    Value = sprintf("%d (%.0f)", n, prop * 100)
  )

race_block <- tibble(
  Characteristic = "Race, n (%)",
  Value = ""
)
race_rows <- race_count %>%
  filter(race_label != 'NA') %>%
  transmute(
    Characteristic = paste0("&nbsp;&nbsp;", race_label),
    Value = sprintf("%d (%.1f)", n, prop * 100)
  )

prep_block <- tibble(
  Characteristic = "History of PrEP uptake, n (%)",
  Value = ""
)
prep_rows <- prep_summary %>%
  transmute(
    Characteristic = paste0("&nbsp;&nbsp;", prep_status),
    Value = sprintf("%d (%.1f)", n, prop * 100)
  )

num_partner_row <- sex_partner_summary %>%
  transmute(
    Characteristic = "Number of male sex partners in the past 90 days, median (IQR)",
    Value = sprintf("%d (%d–%d)", median, q1, q3)
  )

condom_use_block <- tibble(
  Characteristic = "Condom use, n (%)",
  Value = ""
)
condom_use_rows <- condom_summary %>%
  transmute(
    Characteristic = paste0("&nbsp;&nbsp;", condom_status),
    Value = sprintf("%d (%.1f)", n, prop * 100)
  )

receptive_rows <- receptive_summary %>%
  transmute(
    Characteristic = receptive_label,
    Value = sprintf("%d (%.1f)", n, prop * 100)
  )

hivtest_row <- hivtest_summary %>%
  filter(label == "Ever tested for HIV during lifetime") %>%
  transmute(
    Characteristic = "Ever tested for HIV during lifetime, n (%)",
    Value = sprintf("%d (%.1f)", n, prop * 100)
  )
not_tested_row <- hivtest_summary %>%
  filter(label == "Not tested for HIV") %>%
  transmute(
    Characteristic = "If not tested for HIV, n (%)",
    Value = sprintf("%d (%.1f)", n, prop * 100)
  )

hiv_time_block <- tibble(
  Characteristic = "If tested for HIV, median (IQR)",
  Value = ""
)
hiv_time_row <- hiv_test_time %>%
  transmute(
    Characteristic = paste0("&nbsp;&nbsp;", "Months since last HIV test"),
    Value = sprintf("%d (%d–%d)", round(median,0), round(q1,0), round(q3,0))
  )

reasons_block <- tibble(
  Characteristic = "Main reasons cited by the 63 participants for not getting tested, n (%)",
  Value = ""
)
reasons_rows <- nohiv_test_reason %>%
  transmute(
    Characteristic = paste0("&nbsp;&nbsp;", reasons),
    Value = sprintf("%d (%.1f)", n, prop * 100)
  )

### merge blocks to meet the format in manuscript
table1_final <- bind_rows(
  age_row,
  
  ethnicity_block,
  ethnicity_rows,
  
  race_block,
  race_rows,
  
  prep_block,
  prep_rows,
  
  num_partner_row,        
  
  condom_use_block,
  condom_use_rows,
  
  receptive_rows,
  
  hivtest_row,
  not_tested_row,
  
  hiv_time_block,
  hiv_time_row,
  
  reasons_block,
  reasons_rows
)

### output final table

library(knitr)
library(kableExtra)

bold_rows <- which(table1_final$Value == "")

kbl <- kable(
  table1_final,
  format = "html",
  escape = FALSE,
  col.names = c("Characteristic", "Value")
) %>%
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover")
  ) %>%
  row_spec(bold_rows, bold = TRUE)

save_kable(kbl, "Table1.html")











