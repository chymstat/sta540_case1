library(tidyverse)
library(gt)
library(ggplot2)

### construct data
so_dt <- filtered_data %>%    #secondary_outcome_data
  mutate(
    ORA_REDEEMED = if_else(ORA_REDEEMED == "Yes", "Yes", "No")
  )

### table a-taps
### firstly construct long form for each substance
### then use a function to transfer them to format in appendix c
### finally combine the blocks together
alcohol_dt <- so_dt %>%
  mutate(
    Use = case_when(
      Q12_2 == 7 | Q13_1 == 2   ~ "None", 
      (Q13_2 != 2 | Q13_3 == 1) ~ "High_Risk_Substance_Use",
      TRUE ~ "Problem_Use"
    )
  ) %>%
  count(ORA_REDEEMED, Use) 

cannabis_dt <- so_dt %>%
  mutate(
    Use = case_when(
      Q12_3 == 7 | (Q12_3 != 7 & Q13_5 == 2)   ~ "None", 
      Q13_6 ==1 ~ "High_Risk_Substance_Use",
      TRUE ~ "Problem_Use"
    )
  ) %>%
  count(ORA_REDEEMED, Use)

stimulants_dt <- so_dt %>%
  mutate(
    Use = case_when(
      (Q13_8 == 1) ~ "Problem_Use/High_Risk_Substance_Use",
      TRUE ~ "None"
    )
  ) %>%
  count(ORA_REDEEMED, Use)

opioid_dt <- so_dt %>%
  mutate(
    Use = case_when(
      (Q13_14 == 1) ~ "Problem_Use/High_Risk_Substance_Use",
      TRUE ~ "None"
    )
  ) %>%
  count(ORA_REDEEMED, Use)

sedative_dt <- so_dt %>%
  mutate(
    Use = case_when(
      (Q12_4 != 7 & Q13_17 == 9999) ~ 'Missing',
      (Q13_17 == 1) ~ "Problem_Use/High_Risk_Substance_Use",
      TRUE ~ "None"
    )
  ) %>%
  count(ORA_REDEEMED, Use)

prescribe_dt <- so_dt %>%
  mutate(
    Use = case_when(
      (Q12_4 != 7 & Q13_20 == 9999) ~ 'Missing',
      (Q13_20 == 1) ~ "Problem_Use/High_Risk_Substance_Use",
      TRUE ~ "None"
    )
  ) %>%
  count(ORA_REDEEMED, Use)

# function used to transfer dataframe to tables in appendix c
make_table1_fisher <- function(df,
                               group_var = "ORA_REDEEMED",
                               category_var,
                               group_labels = c(
                                 Yes = "Ordered test kit",
                                 No  = "Did not order test kit"
                               ),
                               digits = 1,
                               p_digits = 3) {
  
  # denom N
  denom <- df %>%
    group_by(.data[[group_var]]) %>%
    summarise(N = sum(n), .groups = "drop")
  
  # n/N , percent
  tab_long <- df %>%
    left_join(denom, by = group_var) %>%
    mutate(
      percent = round(n / N * 100, digits),
      n_N = paste0(n, "/", N)
    ) %>%
    select(
      !!sym(group_var),
      !!sym(category_var),
      n_N,
      percent
    )
  
  # wide
  tab_wide <- tab_long %>%
    pivot_wider(
      names_from = !!sym(group_var),
      values_from = c(n_N, percent)
    ) %>%
    rename(
      `Ordered test kit n/N` = n_N_Yes,
      `Ordered test kit Percent` = percent_Yes,
      `Did not order test kit n/N` = n_N_No,
      `Did not order test kit Percent` = percent_No
    )
  
  # Fisher exact test
  fisher_mat <- df %>%
    pivot_wider(
      names_from = !!sym(category_var),
      values_from = n
    ) %>%
    column_to_rownames(group_var) %>%
    as.matrix()
  
  p_value <- round(fisher.test(fisher_mat)$p.value, p_digits)
  
  #  P-value
  tab_final <- tab_wide %>%
    mutate(
      `P-value` = c(p_value, rep("", n() - 1))
    )
  
  return(tab_final)
}
alcohol_table <- make_table1_fisher(
  df = alcohol_dt,
  category_var = "Use"
)

cannabis_table <- make_table1_fisher(
  df = cannabis_dt,
  category_var = "Use"
)

stimulants_table <- make_table1_fisher(
  df = stimulants_dt,
  category_var = "Use"
)

opioid_table <- make_table1_fisher(
  df = opioid_dt,
  category_var = "Use"
)

sedative_table <- make_table1_fisher(
  df = sedative_dt,
  category_var = "Use"
)

prescribe_table <- make_table1_fisher(
  df = prescribe_dt,
  category_var = "Use"
)


# transfer r dataframe to blocks for display
add_block_header <- function(tbl, block_name) {
  header_row <- tibble(
    Use = block_name,
    `Did not order test kit n/N` = "",
    `Ordered test kit n/N` = "",
    `Did not order test kit Percent` = NA_real_,
    `Ordered test kit Percent` = NA_real_,
    `P-value` = ""
  )
  bind_rows(header_row, tbl)
}

alcohol_tbl <- add_block_header(alcohol_table, "Alcohol")
cannabis_tbl <- add_block_header(cannabis_table, "Cannabis")
stimulants_tbl <- add_block_header(stimulants_table, "Stimulants")
opioid_tbl <- add_block_header(opioid_table, "Opioid")
sedative_tbl <- add_block_header(sedative_table, "Sedative")
prescribe_tbl <- add_block_header(prescribe_table, "Prescribed stimulant")

table_a <- bind_rows(
  alcohol_tbl,
  cannabis_tbl,
  stimulants_tbl,
  opioid_tbl,
  sedative_tbl,
  prescribe_tbl
)

# render final result to a html file similar to that in appendix c
gt(table_a) %>%
  tab_header(
    title = md("**a. Tobacco, alcohol, prescription medications, and other substance use**")
  ) %>%
  sub_missing(
    columns = c(
      `Did not order test kit Percent`,
      `Ordered test kit Percent`,
      `P-value`
    ),
    missing_text = ""
  ) %>%
  cols_align(
    align = "center",
    -Use
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = is.na(`Did not order test kit Percent`)
    )
  ) %>%
  gtsave("a. substance_use.html")






### table b-stage change
q151_summary <- so_dt %>%
  mutate(
    Stage = case_when(
      Q15_1 == 1 ~ "Precontemplation",
      Q15_1 == 2 ~ "Contemplation",
      Q15_1 == 3 ~ "Determination",
      Q15_1 == 4 ~ "Action",
      Q15_1 == 5 ~ "Maintenance",
      TRUE ~ "Missing"
    )
  ) %>%
  count(ORA_REDEEMED, Stage)

q151_table <- make_table1_fisher(
  df = q151_summary,
  category_var = "Stage"
)


stage_labels <- tibble::tibble(
  Stage = c(
    "Precontemplation",
    "Contemplation",
    "Determination",
    "Action",
    "Maintenance"
  ),
  Description = c(
    "I do not see any need to regularly test for HIV",
    "I think I should get tested for HIV regularly, but I am not sure",
    "I am ready to start getting regularly tested for HIV",
    "I am trying to get tested regularly for HIV",
    "I have been getting tested for HIV regularly over the past few years"
  )
)

q151_table <- q151_table %>%
  left_join(stage_labels, by = "Stage") %>%
  relocate(Description, .after = Stage)

stage_gt <- q151_table %>%
  gt() %>%
  tab_header(
    title = md("**b. Stage of health behavior change**")
  ) %>%
  cols_label(
    Stage = "Stage",
    Description = "Description",
    `Did not order test kit n/N` = md("Did not order test kit<br>n/N"),
    `Ordered test kit n/N` = md("Ordered test kit<br>n/N"),
    `Did not order test kit Percent` = md("Did not order test kit<br>Percent"),
    `Ordered test kit Percent` = md("Ordered test kit<br>Percent"),
    `P-value` = "P-value"
  ) %>%
  fmt_number(
    columns = ends_with("Percent"),
    decimals = 1
  ) %>%
    fmt_number(
      columns = `P-value`,
      decimals = 3
    ) %>%
  cols_align(
    align = "left",
    columns = c(Stage, Description)
  ) %>%
    cols_align(
      align = "center",
      columns = -c(Stage, Description)
    ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = Stage)
  )
gtsave(
  stage_gt,
  filename = "b. stage_of_health_behavior_change.html"
)




### table c-HIV test attitudes
q15_statements <- tibble(
  question = c(
    "Q15_3", "Q15_4", "Q15_5", "Q15_6", "Q15_7"
  ),
  statement = c(
    "Getting tested for HIV helps people feel better",
    "Getting tested for HIV helps people from getting HIV",
    "People in my life would leave if I had HIV",
    "People who tested positive for HIV should hide it from others",
    "I would rather not know if I have HIV"
  )
)
q15_summary <- so_dt %>%
  select(ORA_REDEEMED, all_of(q15_statements$question)) %>%
  pivot_longer(
    cols = all_of(q15_statements$question),
    names_to = "question",
    values_to = "agree"
  ) %>%
  mutate(
    order = if_else(ORA_REDEEMED == 'Yes', "Ordered", "Did not order"),
    agree = if_else(agree == 1, "Agree", "Disagree")
  ) %>%
  count(question, order, agree) %>%
  left_join(q15_statements, by = "question") %>%
  relocate(statement, .after = question) %>% 
  drop_na()

denom15 <- q15_summary %>%
  group_by(question, order) %>%
  summarise(N = sum(n), .groups = "drop")

q15_tab_long <- q15_summary %>%
  left_join(denom15, by = c("question", "order")) %>%
  mutate(
    percent = round(n / N * 100, 1),
    `n/N` = paste0(n, "/", N)
  )

fisher_p15 <- q15_tab_long %>%
  select(question, order, agree, n) %>%
  pivot_wider(
    names_from = c(order, agree),
    values_from = n,
    values_fill = 0
  ) %>%
  rowwise() %>%
  mutate(
    p_value = fisher.test(
      matrix(
        c(
          Ordered_Agree, Ordered_Disagree,
          `Did not order_Agree`, `Did not order_Disagree`
        ),
        nrow = 2,
        byrow = TRUE
      )
    )$p.value
  ) %>%
  ungroup() %>%
  select(question, p_value)

final_tableq15 <- q15_tab_long %>%
  left_join(fisher_p15, by = "question") %>%
  mutate(
    p_value = if_else(agree == "Agree",
                      sprintf("%.3f", p_value),
                      "")
  ) %>%
  select(
    statement,
    agree,
    order,
    `n/N`,
    percent,
    p_value
  ) %>%
  pivot_wider(
    names_from = order,
    values_from = c(`n/N`, percent)
  ) %>%
  arrange(statement) %>%
  relocate(p_value, .after = last_col())

colnames(final_tableq15) <- c(
  "Statements",
  "Response",
  "n/N_Did not order test kit",
  "n/N_Ordered test kit",
  "percent_Did not order test kit",
  "percent_Ordered test kit",
  "P-value"
)

# make the display clearer
keep_rows15 <- c(1, 3, 5, 7, 9)
q15_final_table_display <- final_tableq15 %>%
  mutate(
    Statements = if_else(
      row_number() %in% keep_rows15,
      Statements,
      ""
    )
  )

gt(q15_final_table_display) %>%
  tab_header(
    title = md("**c. Attitudes toward HIV testing**")
  ) %>%
  fmt_markdown(everything()) %>%
  cols_align(
    align = "center",
    -Statements
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = Statements,
      rows = Statements != ""
    )
  ) %>%
  gtsave("c. hiv_test.html")






### table d-HIV treatment attitudes
q94_statements <- tibble(
  question = c(
    "Q94_1", "Q94_5", "Q94_6", "Q94_7", "Q94_8",
    "Q94_9", "Q94_10", "Q94_11", "Q94_12", "Q94_13"
  ),
  statement = c(
    "I am less threatened by the idea of being HIV positive than I used to be",
    "I am less worried about HIV infection than I used to be",
    "I think HIV/AIDS is less of a problem than it used to be",
    "I think HIV/AIDS is a less serious threat than it used to be because of new HIV/AIDS treatments",
    "I am much less concerned about becoming HIV positive myself because of new HIV/AIDS treatments",
    "I think that condom use during sex is less necessary now that new HIV/AIDS treatments are available",
    "I think that someone who is HIV positive now needs to care less about condom use",
    "I think that the need for condom use is less than it used to be, because you can always start new treatments",
    "I think that someone who is HIV positive and uses new HIV/AIDS treatments can be cured",
    "I think that new HIV/AIDS treatments can eradicate the virus from your body"
  )
)
q94_items <- q94_statements$question
q94_summary<- so_dt %>%
  group_by(ORA_REDEEMED) %>%
  summarise(
    across(
      all_of(q94_items),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE)
      )
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    -ORA_REDEEMED,
    names_to = c("question", ".value"),
    names_sep = "_(?=[^_]+$)"
  ) %>%
  left_join(q94_statements, by = "question") %>%
  select(ORA_REDEEMED, question, statement, mean, sd)


q94_pvalues <- so_dt %>%
  select(ORA_REDEEMED, all_of(q94_items)) %>%
  pivot_longer(
    -ORA_REDEEMED,
    names_to = "question",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  group_by(question) %>%
  summarise(
    p_value = wilcox.test(
      value ~ ORA_REDEEMED,
      exact = FALSE
    )$p.value,
    .groups = "drop"
  )

q94_table_long <- q94_summary %>%
  mutate(
    mean_sd = sprintf("%.1f (%.1f)", mean, sd)
  )

q94_table <- q94_table_long %>%
  select(ORA_REDEEMED, question, statement, mean_sd) %>%
  pivot_wider(
    names_from = ORA_REDEEMED,
    values_from = mean_sd
  ) %>%
  left_join(q94_pvalues, by = "question") %>%
  mutate(
    p_value = sprintf("%.3f", p_value)
  ) %>%
  select(
    statement,
    `Yes`, # Ordered test kit
    `No`, # Did not order
    p_value
  )
colnames(q94_table) <- c(
  "Statements",
  "Ordered test kit Mean (SD)",
  "Did not order test kit Mean (SD)",
  "P-value (Wilcoxon rank test)"
)

gt(q94_table) %>%
  tab_header(
    title = md("**d. Attitudes toward HIV treatment (continuous scale from 1 [strongly disagree] to 7 [strongly agree])**")
  ) %>%
  fmt_markdown(everything()) %>%
  cols_align(
    align = "center",
    -Statements
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = Statements,
      rows = Statements != ""
    )
  ) %>%
  gtsave("d. hiv_attitudes.html")




### table e-HIV stigma
q14_statements <- tibble(
  question = c("Q14_2", "Q14_3", "Q14_4", "Q14_5"),
  statement = c(
    "I feel afraid of people living with HIV/AIDS",
    "I could not be friends with someone who has HIV/AIDS",
    "People who get HIV/AIDS through sex or drug use got what they deserve",
    "I feel anger toward people with HIV/AIDS"
  )
)


q14_count_by_order <- so_dt %>%
  select(ORA_REDEEMED, all_of(q14_statements$question)) %>%
  pivot_longer(
    cols = all_of(q14_statements$question),
    names_to = "question",
    values_to = "response"
  ) %>%
  count(ORA_REDEEMED, question, response) %>%
  complete(
    ORA_REDEEMED = c('Yes', 'No'),
    question = q14_statements$question,
    response = 1:7,
    fill = list(n = 0)
  ) %>%
  left_join(q14_statements, by = "question") %>%
  mutate(
    response_label = factor(
      response,
      levels = 1:7,
      labels = c(
        "Strongly agree",
        "Agree",
        "Somewhat agree",
        "Neither agree nor disagree",
        "Somewhat disagree",
        "Disagree",
        "Strongly disagree"
      )
    )
  ) %>%
  select(ORA_REDEEMED, question, statement, response, response_label, n)


q14_denominator <- q14_count_by_order %>%
  group_by(ORA_REDEEMED, question) %>%
  summarise(N = sum(n), .groups = "drop")


q14_table_long <- q14_count_by_order %>%
  left_join(q14_denominator, by = c("ORA_REDEEMED", "question")) %>%
  mutate(
    percent = round(n / N * 100, 1),
    `n/N` = paste0(n, "/", N)
  )

q14_pvalues <- so_dt %>%
  select(ORA_REDEEMED, all_of(q14_statements$question)) %>%
  pivot_longer(
    cols = all_of(q14_statements$question),
    names_to = "question",
    values_to = "response"
  ) %>%
  filter(!is.na(response)) %>%
  group_by(question) %>%
  summarise(
    p_value = wilcox.test(
      response ~ ORA_REDEEMED,
      exact = FALSE
    )$p.value,
    .groups = "drop"
  )

q14_final_table <- q14_table_long %>%
  left_join(q14_pvalues, by = "question") %>%
  mutate(
    p_value = if_else(
      response == min(response),
      sprintf("%.3f", p_value),
      ""
    )
  ) %>%
  arrange(
    question,
    response # Strongly agree → Strongly disagree
  ) %>%
  select(
    statement,
    response_label,
    ORA_REDEEMED,
    `n/N`,
    percent,
    p_value
  ) %>%
  pivot_wider(
    names_from = ORA_REDEEMED,
    values_from = c(`n/N`, percent)
  ) %>%
  relocate(p_value, .after = last_col())
colnames(q14_final_table) <- c(
  "Statements",
  "Response",
  "n/N_Did not order test kit",
  "n/N_Ordered test kit",
  "percent_Did not order test kit",
  "percent_Ordered test kit",
  "P-value (Wilcoxon rank test)"
)

keep_rows14 <- c(1, 8, 15, 22)
q14_final_table_display <- q14_final_table %>%
  mutate(
    Statements = if_else(
      row_number() %in% keep_rows14,
      Statements,
      ""
    )
  )

gt(q14_final_table_display) %>%
  tab_header(
    title = md("**e. HIV-related stigma among study participants**")
  ) %>%
  fmt_markdown(everything()) %>%
  cols_align(
    align = "center",
    -Statements
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = Statements,
      rows = Statements != ""
    )
  ) %>%
  gtsave("e. hiv_stigma.html")



### table f-Medical mistrust

q16_statements <- tibble(
  question = paste0("Q16_", 1:7),
  statement = c(
    "You'd better be cautious when dealing with healthcare organizations",
    "Patients have sometimes been deceived or misled by health care organizations",
    "When health care organizations make mistakes they usually cover it up",
    "Health care organizations have sometimes done harmful experiments on patients without their knowledge",
    "Health care organizations don’t always keep your information totally private",
    "Sometimes I wonder if health care organizations really know what they are doing",
    "Mistakes are common in health care organizations"
  )
)

label_q16_response <- function(x) {
  case_when(
    x %in% c(1, 28) ~ "Strongly agree",
    x %in% c(2, 30) ~ "Agree",
    x %in% c(6, 33) ~ "Disagree",
    x %in% c(7, 34) ~ "Strongly disagree",
    TRUE ~ "Missing"
  )
}


q16_count_by_order <- so_dt %>%
  select(ORA_REDEEMED, all_of(q16_statements$question)) %>%
  pivot_longer(
    cols = all_of(q16_statements$question),
    names_to = "question",
    values_to = "response"
  ) %>%
  mutate(
    response_label = label_q16_response(response)
  ) %>%
  count(ORA_REDEEMED, question, response_label) %>%
  complete(
    ORA_REDEEMED = c("Yes", "No"),
    question = q16_statements$question,
    response_label = c(
      "Strongly agree",
      "Agree",
      "Disagree",
      "Strongly disagree"
    ),
    fill = list(n = 0)
  ) %>%
  left_join(q16_statements, by = "question") %>%
  select(ORA_REDEEMED, question, statement, response_label, n) %>%
  arrange(question, response_label, ORA_REDEEMED) %>%
  filter(!grepl("Missing", response_label, ignore.case = TRUE))

q16_denominator <- q16_count_by_order %>%
  group_by(ORA_REDEEMED, question) %>%
  summarise(N = sum(n), .groups = "drop")

q16_table_long <- q16_count_by_order %>%
  left_join(q16_denominator, by = c("ORA_REDEEMED", "question")) %>%
  mutate(
    percent = round(n / N * 100, 1),
    `n/N` = paste0(n, "/", N)
  )

q16_pvalues <- so_dt %>%
  select(ORA_REDEEMED, all_of(q16_statements$question)) %>%
  pivot_longer(
    cols = all_of(q16_statements$question),
    names_to = "question",
    values_to = "response"
  ) %>%
  filter(!is.na(response)) %>%
  group_by(question) %>%
  summarise(
    p_value = wilcox.test(
      response ~ ORA_REDEEMED,
      exact = FALSE
    )$p.value,
    .groups = "drop"
  )

q16_final_table <- q16_table_long %>%
  left_join(q16_pvalues, by = "question") %>%
  mutate(
    p_value = if_else(
      response_label == "Strongly agree",
      sprintf("%.3f", p_value),
      ""
    ),
    response_label = factor(
      response_label,
      levels = c(
        "Strongly agree",
        "Agree",
        "Disagree",
        "Strongly disagree"
      )
    )
  ) %>%
  arrange(question, response_label) %>%
  select(
    statement,
    response_label,
    ORA_REDEEMED,
    `n/N`,
    percent,
    p_value
  ) %>%
  pivot_wider(
    names_from = ORA_REDEEMED,
    values_from = c(`n/N`, percent)
  ) %>%
  relocate(p_value, .after = last_col())

colnames(q16_final_table) <- c(
  "Statements",
  "Response",
  "n/N_Did not order test kit",
  "n/N_Ordered test kit",
  "percent_Did not order test kit",
  "percent_Ordered test kit",
  "P-value (Wilcoxon rank test)"
)

keep_rows <- c(1, 5, 9, 13, 17, 21, 25)
q16_final_table_display <- q16_final_table %>%
  mutate(
    Statements = if_else(
      row_number() %in% keep_rows,
      Statements,
      ""
    )
  )

gt(q16_final_table_display) %>%
  tab_header(
    title = md("**f. Medical mistrust**")
  ) %>%
  fmt_markdown(everything()) %>%
  cols_align(
    align = "center",
    -Statements
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = Statements,
      rows = Statements != ""
    )
  ) %>%
  gtsave("f. medical_mistrust.html")




### figure-S1

library(ggplot2)

q7_items <- tibble(
  question = c(
    "Q7_19_FU1","Q7_18_FU1","Q7_17_FU1","Q7_16_FU1","Q7_15_FU1",
    "Q7_14_FU1","Q7_13_FU1","Q7_12_FU1","Q7_11_FU1","Q7_10_FU1",
    "Q7_9_FU1","Q7_8_FU1","Q7_7_FU1","Q7_6_FU1","Q7_5_FU1",
    "Q7_4_FU1","Q7_3_FU1","Q7_2_FU1"
  ),
  statement = c(
    "PrEP is only partially effective",
    "Gay and bisexual men who take PrEP are promiscuous",
    "PrEP gives gay and bisexual men more options to remain safe",
    "Gay and bisexual men who take PrEP are risk-takers",
    "PrEP is an excuse for gay and bisexual men to avoid using condoms",
    "Gay and bisexual men who take PrEP are being responsible",
    "PrEP is too expensive",
    "I would be more comfortable using PrEP if I just knew how it would affect my health",
    "Not knowing if there are long-term side effects of taking a daily HIV pill makes me very uncomfortable",
    "I am ashamed to tell others that I am on PrEP",
    "I worry about what other people would think of me if they knew I was on PrEP",
    "I worry that others will think I have HIV if they find out I am on PrEP",
    "I feel uncomfortable taking HIV medication when I don’t have HIV",
    "Taking PrEP makes me more likely to have sex without using a condom",
    "Having PrEP available makes safer sex less important",
    "Taking PrEP means I can have sex without using condoms",
    "By taking PrEP, I am lowering my chances of getting infected with HIV",
    "Taking a daily HIV pill is a good way to protect myself from getting HIV"
  )
)

q7_long <- so_dt %>%
  select(all_of(q7_items$question)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "question",
    values_to = "response"
  ) %>%
  mutate(
    response_label = case_when(
      response == 1 ~ "Strongly agree",
      response == 2 ~ "Somewhat agree",
      response == 3 ~ "Neither agree or disagree",
      response == 4 ~ "Somewhat disagree",
      response == 5 ~ "Strongly disagree",
      TRUE ~ NA_character_  
    )
  ) %>%
  filter(!is.na(response_label)) %>%
  count(question, response_label, name = "n") %>%
  group_by(question) %>%
  mutate(
    N = sum(n),
    percent = n / N * 100
  ) %>%
  ungroup() %>%
  left_join(q7_items, by = "question")

q7_long$response_label <- factor(
  q7_long$response_label,
  levels = c(
    "Strongly agree",
    "Somewhat agree",
    "Neither agree or disagree",
    "Somewhat disagree",
    "Strongly disagree"
  )
)


ggplot(
  q7_long,
  aes(
    x = percent,
    y = factor(statement, levels = q7_items$statement),
    fill = response_label
  )
) +
  geom_col(width = 0.8) +
  geom_text(
    aes(label = ifelse(percent >= 5,
                       paste0(round(percent, 1), "%"),
                       "")),
    position = position_stack(vjust = 0.5, reverse = FALSE),
    size = 3
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    expand = c(0, 0),
    labels = function(x) paste0(x, "%")
  ) +
  scale_fill_brewer(
    palette = "RdBu",
    direction = -1,   
    name = NULL,
    limits = levels(q7_long$response_label)  
  ) +
  labs(
    title = "Figure S1. Opinions about PrEP among participants",
    x = NULL, 
    y = NULL
    ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )



### figure-S2

q8_items <- tibble(
  question = c(
    "Q8_1_13_FU1",
    "Q8_1_12_FU1",
    "Q8_1_11_FU1",
    "Q8_1_10_FU1",
    "Q8_1_9_FU1",
    "Q8_1_8_FU1",
    "Q8_1_7_FU1",
    "Q8_1_6_FU1",
    "Q8_1_5_FU1",
    "Q8_1_4_FU1",
    "Q8_1_3_FU1",
    "Q8_1_2_FU1",
    "Q8_1_1_FU1"
  ),
  statement = c(
    "I would be ashamed to take PrEP",
    "I do not really think I am at high enough risk for HIV to warrant taking PrEP",
    "If I do become HIV+, certain medicines won’t work because I was taking PrEP",
    "Potentially having long-term health effects because of PrEP",
    "I don’t want to talk to my doctor about my sex life",
    "People will see me taking the medication and think that I have HIV",
    "People will see me taking the medication and will want to know why I’m taking it",
    "Having to take PrEP means I’m putting myself at risk for HIV",
    "Taking PrEP might make my partner(s) expect me to have anal sex without a condom",
    "PrEP might make me more willing to have anal sex without a condom",
    "PrEP not protecting me against HIV infection completely",
    "Potentially experiencing side effects",
    "Having to take a pill every day"
  )
)

q8_long <- so_dt %>%
  select(all_of(q8_items$question)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "question",
    values_to = "response"
  ) %>%
  mutate(
    response_label = case_when(
      response == 1 ~ "Not at all important",
      response == 2 ~ "Slightly important",
      response == 3 ~ "Moderately important",
      response == 4 ~ "Very important",
      response == 5 ~ "Extremely important",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(response_label)) %>%
  count(question, response_label, name = "n") %>%
  group_by(question) %>%
  mutate(
    N = sum(n),
    percent = n / N * 100
  ) %>%
  ungroup() %>%
  left_join(q8_items, by = "question")

q8_long$response_label <- factor(
  q8_long$response_label,
  levels = c(
    "Not at all important",
    "Slightly important",
    "Moderately important",
    "Very important",
    "Extremely important"
  )
)

ggplot(
  q8_long,
  aes(
    x = percent,
    y = factor(statement, levels = q8_items$statement),
    fill = response_label
  )
) +
  geom_col(width = 0.8) +
  geom_text(
    aes(label = ifelse(percent >= 5,
                       paste0(round(percent, 1), "%"),
                       "")),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    expand = c(0, 0),
    labels = function(x) paste0(x, "%")
  ) +
  scale_fill_brewer(
    palette = "RdBu",
    direction = -1,
    name = NULL,
    limits = levels(q8_long$response_label)
  ) +
  labs(
    title = "Figure S2. Reported barriers to PrEP uptake among participants",
    x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )


### figure-S3
q8_2_items <- tibble(
  question = c(
    "Q8_2_8_FU1",
    "Q8_2_7_FU1",
    "Q8_2_6_FU1",
    "Q8_2_5_FU1",
    "Q8_2_4_FU1",
    "Q8_2_3_FU1",
    "Q8_2_2_FU1",
    "Q8_2_1_FU1"
  ),
  statement = c(
    "Hearing from my friends that they had good experiences with PrEP",
    "I am a gay/bisexual guy or a transwoman and I know I am at really high risk for HIV",
    "I am concerned that, even if I use condoms, I could still get HIV",
    "Hearing from my doctor that taking PrEP would be best for my health",
    "Not having to go to my regular doctor to get PrEP",
    "Getting free one-on-one counseling and support while I am on PrEP",
    "Getting free HIV/STD testing while I am on PrEP",
    "Not having to pay for PrEP or because it is affordable"
  )
)


q8_2_long <- so_dt %>%
  select(all_of(q8_2_items$question)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "question",
    values_to = "response"
  ) %>%
  mutate(
    response_label = case_when(
      response == 1 ~ "Extremely important",
      response == 2 ~ "Very important",
      response == 3 ~ "Moderately important",
      response == 4 ~ "Slightly important",
      response == 5 ~ "Not at all important",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(response_label)) %>%
  count(question, response_label, name = "n") %>%
  group_by(question) %>%
  mutate(
    N = sum(n),
    percent = n / N * 100
  ) %>%
  ungroup() %>%
  left_join(q8_2_items, by = "question")


q8_2_long$response_label <- factor(
  q8_2_long$response_label,
  levels = c(
    "Extremely important",
    "Very important",
    "Moderately important",
    "Slightly important",
    "Not at all important"
  )
)

ggplot(
  q8_2_long,
  aes(
    x = percent,
    y = factor(statement, levels = q8_2_items$statement),
    fill = response_label
  )
) +
  geom_col(width = 0.8) +
  geom_text(
    aes(label = ifelse(percent >= 5,
                       paste0(round(percent, 1), "%"),
                       "")),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    expand = c(0, 0),
    labels = function(x) paste0(x, "%")
  ) +
  scale_fill_brewer(
    palette = "RdBu",
    direction = 1,   
    name = NULL,
    limits = levels(q8_2_long$response_label)
  ) +
  labs(
    title = "Figure S3. Reported facilitators to PrEP uptake among participants",
    x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )



