library(emmeans)
library(tidyverse)
### construct data
po_dt <- filtered_data %>% # primary_outcome_data
  select(ORA_REDEEMED,
         SITE_TYPE,
         SITE,
         WAVE)
order_by_site <- po_dt %>%
  filter(ORA_REDEEMED == "Yes") %>%
  count(SITE, name = "num_ordered") %>%
  mutate(
    WAVE = case_when(
      SITE %in% c("Facebook", "Google", "Grindr") ~ 1,
      SITE %in% c("Jack'd", "Instagram") ~ 2,
      TRUE ~ NA_real_
    )
  ) %>%
  bind_rows(
    tibble(
      SITE = "Bing",
      num_ordered = 0,
      WAVE = 2
    )
  ) %>%
  mutate(
    wave_dur = case_when(
      WAVE == 1 ~ 70,
      WAVE == 2 ~ 38,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    WAVE = factor(WAVE),
    SITE = factor(SITE)
  )

order_by_site$WAVE <- factor(order_by_site$WAVE)

# add CAT labels to match the row and col used in appendix a
order_by_site$CAT <- with(order_by_site, factor(
  ifelse(WAVE == 1 & SITE == "Facebook", "A",
         ifelse(WAVE == 2 & SITE == "Instagram", "A",
                ifelse(WAVE == 1 & SITE == "Grindr", "B",
                       ifelse(WAVE == 2 & SITE == "Jack'd", "B",
                              ifelse(WAVE == 1 & SITE == "Google", "C",
                                     ifelse(WAVE == 2 & SITE == "Bing", "C", NA)))))),
  levels = c("A","B","C")
))

### fit the main model
fit <- glm(
  num_ordered ~ WAVE * CAT + offset(log(wave_dur)),
  family = poisson(link = "log"),
  data = order_by_site
)
#summary(fit)


### provide rate results 
emm_rate <- emmeans(
  fit,
  ~ WAVE * CAT,
  type = "response",
  offset = 0
)
emm_rate

### pairwise testing for poisson rates
pairwise_poisson_wave <- function(d) {
  pairs <- combn(seq_len(nrow(d)), 2)
  
  res <- apply(pairs, 2, function(idx) {
    i <- idx[1]; j <- idx[2]
    
    test <- poisson.test(
      x = c(d$num_ordered[i], d$num_ordered[j]),
      T = c(d$wave_dur[i], d$wave_dur[j]),
      r = 1
    )
    
    data.frame(
      contrast = paste(d$SITE[i], "vs", d$SITE[j]),
      rate_i = d$num_ordered[i] / d$wave_dur[i],
      rate_j = d$num_ordered[j] / d$wave_dur[j],
      RR = test$estimate,
      p_raw = test$p.value
    )
  })
  
  do.call(rbind, res)
}

w1 <- subset(order_by_site, WAVE == 1)
res_w1 <- pairwise_poisson_wave(w1)
res_w1$p_adj <- p.adjust(res_w1$p_raw, method = "hochberg")
res_w1


w2 <- subset(order_by_site, WAVE == 2)
res_w2 <- pairwise_poisson_wave(w2)
res_w2$p_adj <- p.adjust(res_w2$p_raw, method = "hochberg")
res_w2


### testing for contrast in appendix a using likelihood ratio

fit_full <- glm(
  num_ordered ~ CAT * WAVE,
  offset = log(wave_dur),
  family = poisson,
  data = order_by_site
)

dat_AB <- order_by_site
dat_AB$CAT_AB <- as.character(dat_AB$CAT)
dat_AB$CAT_AB[dat_AB$CAT_AB %in% c("A","B")] <- "AB"
dat_AB$CAT_AB <- factor(dat_AB$CAT_AB)


fit_AB <- glm(
  num_ordered ~ CAT_AB * WAVE,
  offset = log(wave_dur),
  family = poisson,
  data = dat_AB
)

anova(fit_AB, fit_full, test = "Chisq")


dat_AC <- order_by_site
dat_AC$CAT_AC <- as.character(dat_AC$CAT)
dat_AC$CAT_AC[dat_AC$CAT_AC %in% c("A","C")] <- "AC"
dat_AC$CAT_AC <- factor(dat_AC$CAT_AC)


fit_AC <- glm(
  num_ordered ~ CAT_AC * WAVE,
  offset = log(wave_dur),
  family = poisson,
  data = dat_AC
)


anova(fit_AC, fit_full, test = "Chisq")


dat_BC <- order_by_site
dat_BC$CAT_BC <- as.character(dat_BC$CAT)
dat_BC$CAT_BC[dat_BC$CAT_BC %in% c("B","C")] <- "BC"
dat_BC$CAT_BC <- factor(dat_BC$CAT_BC)


fit_BC <- glm(
  num_ordered ~ CAT_BC * WAVE,
  offset = log(wave_dur),
  family = poisson,
  data = dat_BC
)


anova(fit_BC, fit_full, test = "Chisq")
















