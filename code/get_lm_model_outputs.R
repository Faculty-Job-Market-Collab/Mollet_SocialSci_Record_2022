# Figure 7. Job Application benchmarks and their impact on success - Gender
#covariates: first-gen (undergrad), position, gender, PPER, 
#disability, residence, google scholar citations, and postdoc fellowship

library(jtools)
library(Hmisc)
options(max.print = 10999)

#Table. correlation matrix of the variables and regression output----
fig7_t_data <- fig7_data_two %>% 
  select(as.numeric(9), as.numeric(17:27)) #pull on-site interview # & dummy variables

fig7_table <- rcorr(as.matrix(fig7_t_data))

fig7_cor_matrix <- as_tibble(fig7_table$r, rownames = "attribute") %>% 
  rename("on_site_interview_correlation" = "on_site_interviews",
         "offsite_interview_correlation" = "offsite_bin_dummy",
         "apps_submitted_correlation" = "apps_bin_dummy") %>% 
  rename_with(~ str_replace(.x, "_dummy", "_correlation"))

fig7_pval <- as_tibble(fig7_table$P, rownames = "attribute") %>% 
  rename("on_site_interview_pval" = "on_site_interviews",
         "offsite_interview_pval" = "offsite_bin_dummy",
         "apps_submitted_pval" = "apps_bin_dummy") %>% 
  rename_with(~ str_replace(.x, "_dummy", "_pval"))

fig7_n <- as_tibble(fig7_table$n, rownames = "attribute") %>% 
  rename("on_site_interview_n" = "on_site_interviews",
         "offsite_interview_n" = "offsite_bin_dummy",
         "apps_submitted_n" = "apps_bin_dummy") %>% 
  rename_with(~ str_replace(.x, "_dummy", "_n"))

fig7_clean_matrix <- left_join(fig7_cor_matrix, fig7_pval, 
                               by = "attribute") %>% 
  left_join(., fig7_n, by = "attribute")

write_csv(fig7_clean_matrix, 
          paste0("mollet_socialsci/figures/Correlation_matrix_Fig7-8_", 
          Sys.Date(), ".csv"))

#A. Off-site correlations with Application Numbers (Remote, on-site, offers) -- Differences in correlations by gender & race----
panelA_lm <- lm(off_site_interviews ~ apps_bin_dummy*gender_dummy *
                 disability_dummy * undergrad_dummy * peer_dummy *
               position_dummy * residence_dummy * citation_dummy * 
               fellowship_dummy, data = fig7_data_two)

sink(paste0("mollet_socialsci/figures/offsite_v_apps_summary_", Sys.Date(), ".txt"))
summ(panelA_lm)
sink()

panelA_coef <- summ(panelA_lm)$coeftable

panelA_coef <- get_wilcox_tbl("panelA_coef", "offsite_v_apps") %>% filter(!is.na(p))

write_csv(panelA_coef, 
          paste0("mollet_socialsci/figures/Coefficients_offsite_v_apps_", 
                 Sys.Date(), ".csv"))

#B. Onsite w/ correlations ----

panelB_lm <- lm(on_site_interviews ~ apps_bin_dummy * gender_dummy *
                 disability_dummy * undergrad_dummy * peer_dummy *
                 position_dummy * residence_dummy * citation_dummy * 
                 fellowship_dummy, data = fig7_data_two)

sink(paste0("mollet_socialsci/figures/onsite_v_apps_summary_", Sys.Date(), ".txt"))
summ(panelB_lm)
sink()

panelB_coef <- summ(panelB_lm)$coeftable

panelB_coef <- get_wilcox_tbl("panelB_coef", "onsite_v_apps") %>% filter(!is.na(p))

write_csv(panelB_coef, 
          paste0("mollet_socialsci/figures/Coefficients_onsite_v_apps_", 
                 Sys.Date(), ".csv"))

#C. faculty offers and applications ----
panelC_lm <- lm(faculty_offers ~ apps_bin_dummy * gender_dummy*
                 disability_dummy * undergrad_dummy * peer_dummy *
                 position_dummy * residence_dummy * citation_dummy * 
                 fellowship_dummy, 
               data = fig7_data_two)

sink(paste0("mollet_socialsci/figures/offers_v_apps_summary_", Sys.Date(), ".txt"))
summ(panelC_lm)
sink()

panelC_coef <- summ(panelC_lm)$coeftable

panelC_coef <- get_wilcox_tbl("panelC_coef", "offers_v_apps") %>% filter(!is.na(p))

write_csv(panelC_coef, 
          paste0("mollet_socialsci/figures/Coefficients_offers_v_apps_", 
                 Sys.Date(), ".csv"))

#D. Correlations between interviews and offers (remote, off-site) ----
panelD_lm <- lm(faculty_offers ~ offsite_bin_dummy * gender_dummy*
                 disability_dummy * undergrad_dummy * peer_dummy *
                 position_dummy * residence_dummy * citation_dummy * 
                 fellowship_dummy, 
               data = fig7_data_two)

sink(paste0("mollet_socialsci/figures/offers_v_offsite_summary_", Sys.Date(), ".txt"))
summ(panelD_lm)
sink()

panelD_coef <- summ(panelD_lm)$coeftable

panelD_coef <- get_wilcox_tbl("panelD_coef", "offers_v_offsite") %>% filter(!is.na(p))

write_csv(panelD_coef, 
          paste0("mollet_socialsci/figures/Coefficients_offers_v_offsite_", 
                 Sys.Date(), ".csv"))

#E. Onsite & faculty offers----
panelE_lm <- lm(faculty_offers ~ on_site_interviews * gender_dummy*
                 disability_dummy * undergrad_dummy * peer_dummy *
                 position_dummy * residence_dummy * citation_dummy * 
                 fellowship_dummy, 
               data = fig7_data_two)

sink(paste0("mollet_socialsci/figures/offers_v_onsite_summary_", Sys.Date(), ".txt"))
summ(panelE_lm)
sink()

panelE_coef <- summ(panelE_lm)$coeftable

panelE_coef <- get_wilcox_tbl("panelE_coef", "offers_v_onsite") %>% filter(!is.na(p))

write_csv(panelE_coef, 
          paste0("mollet_socialsci/figures/Coefficients_offers_v_onsite_", 
                 Sys.Date(), ".csv"))