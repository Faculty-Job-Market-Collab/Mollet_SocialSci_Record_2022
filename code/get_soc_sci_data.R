#setup for loading from full data set
#source("code/load_data.R")

# Identify social science/humanities responses ----
#ss_data <- clean_data %>%
#  filter(survey_year == "2019-2020") %>%
#  filter(research_category == "Social, Behavior, & Economic Sciences" |
#           research_category == "Humanities") %>% 
#  select(-grants_awarded, -transition_award) %>% 
#  distinct()
#
#write_csv(ss_data, "mollet_socialsci/data/social-sci_19-20_data.csv")

#ss_ids <- ss_data %>% 
#  select(id) %>% unique()
#
#ss_tidy_data <- left_join(ss_ids, tidy_data, by = 'id') 
#
#write_csv(ss_tidy_data, "mollet_socialsci/data/social-sci_19-20_tidy-data.csv")

#ss_demo_data <- left_join(ss_ids, demo_data, by = 'id') %>% distinct()
#
#ss_network_data <- left_join(ss_ids, network_data, by = 'id') %>% distinct()
#
#ss_network <- left_join(ss_ids, network, by = 'id') %>% distinct()
#
#ss_qualifications <- left_join(ss_ids, qualifications, by = 'id') %>% distinct()

#setup
library(tidyverse) #for data wrangling
library(car) #for anova

source("code/analysis_functions.R") #functions for generating plots
source("code/get_plot_options.R")

#load clean datasets----
ss_data <- read_csv("mollet_socialsci/data/social-sci_19-20_data.csv")

ss_tidy_data <- read_csv("mollet_socialsci/data/social-sci_19-20_tidy-data.csv")

ss_qualif_data <- ss_tidy_data %>% 
  select(id, question, response) %>% 
  filter(question %in% c("teaching_types", "fellowship",
                         "num_awards", "num_teaching_xp",
                         "peer-reviewed_papers", "conference_abstracts",                   
                         "corresponding_author", "first_author", "last_author", 
                         "preprint_status", "preprint_number", "CNS_status",
                         "CNS_number", "CNS_first_author", "teaching_status",
                         "scholar_citations_all", "scholar_citations_5y",
                         "scholar_hindex", "scholar_hindex_5y", "patent_status", 
                         "patent_number", "postdoctoral_fellow",
                         "predoctoral_fellow", "grant_pi", "grant_copi",
                         "first_author_binned", "scholar_hindex_binned",
                         "peer-reviewed_papers_binned", "scholar_citations_all_binned")) %>% 
  distinct()

ss_app_outcomes <- ss_data %>% 
  select(id, faculty_offers, offer_responses, num_rejection_reasons, 
         simple_offers, apps_submitted_binned, RI_apps_submitted_binned,                
         PUI_apps_submitted_binned, off_site_interviews_binned,
         rejections_received_binned, on_site_interviews, application_cycles,
         apps_submitted, RI_apps_submitted, PUI_apps_submitted,
         off_site_interviews, rejections_received) %>% 
  distinct()

#Figure 1----
fig1_id <- ss_data %>% 
  select(id, peer) %>% 
  distinct()

fig1_gender <- ss_data %>% 
  select(id, simple_gender) %>% 
  distinct()

fig1_demo <- full_join(fig1_id, fig1_gender) %>% 
  distinct()

fig1_res <- ss_data %>% 
  select(id, research_category) %>% 
  distinct()

fig1_all_data <- full_join(fig1_gender, fig1_id, by = "id") %>% 
  left_join(., fig1_res, by = "id") %>% 
  distinct() 

fig1_inst_data <- ss_tidy_data %>% 
  filter(str_detect(question, ".+_apps_submitted.+")) %>% 
  select(id, question, response) %>% 
  left_join(., fig1_gender, by = "id") %>% 
  distinct() %>% 
  filter(!is.na(response))

fig1a_data <- ss_data %>% 
  select(id, application_cycles) %>% 
  left_join(., fig1_res, by = "id") %>%
  distinct() %>% 
  mutate(num_application_cycles = if_else(
    str_detect(application_cycles, ">5"), 
    5, as.numeric(application_cycles)))

fig1b_data <- ss_data %>% 
  select(id, number_postdocs) %>% 
  left_join(., fig1_res) %>% 
  distinct()

metric_data <- ss_qualif_data %>%
  left_join(., fig1_res, by = "id") %>% 
  filter(!is.na(question)) %>% 
  filter(question %in% c("first_author",
                         "peer-reviewed_papers", "scholar_hindex",
                         "scholar_citations_all")) %>% 
  distinct()

grants_data <- ss_qualif_data %>%
  left_join(., fig1_res, by = "id") %>% 
  filter(!is.na(question)) %>% 
  filter(question %in% c("grant_copi", "grant_pi",
                         "predoctoral_fellow", "postdoctoral_fellow")) %>% 
  distinct()

teaching_data <- ss_qualif_data %>%
  left_join(., fig1_res, by = "id") %>% 
  filter(!is.na(question)) %>% 
  filter(question %in% c("teaching_status", "teaching_types")) %>% 
  filter(response %in% c("Teaching certificate", "Total adjunct positions", 
                         "Visiting assistant professor", "No",
                         "Yes, experience beyond TA", "Yes, TA")) %>% 
  distinct()
  
source("mollet_socialsci/code/figure_1.R")

#Figure 3----
source("mollet_socialsci/code/figure_3.R")

#Figure 2----
fig2_data <- ss_tidy_data %>% 
  select(id, question, response) %>% 
  filter(question %in% c("simple_gender", "off_site_interviews", "simple_offers",
                         "on_site_interviews", "peer", "faculty_offers",
                         "apps_submitted_binned", "apps_submitted")) %>% 
  distinct() %>% 
  spread(key = question, value = response) %>% 
  filter(!is.na(apps_submitted)) %>% 
  mutate(simple_faculty_offer = case_when(
    as.numeric(faculty_offers) == 0 ~ "0",
    as.numeric(faculty_offers) >= 1 ~ "1+"
  ),
  simple_faculty_offer = factor(simple_faculty_offer, 
                                levels = c("0", "1+")))

ss_id_simple_offer <- select(fig2_data, id, simple_faculty_offer)

fig2_data_two <- fig2_data %>% 
  filter(simple_gender != "No Response")

fig2_data_women <- fig2_data %>% 
  filter(simple_gender == "Woman/Trans/GNC")

fig2_data_men <- fig2_data %>% 
  filter(simple_gender == "Man")

fig2_data_peer <- fig2_data %>% 
  filter(peer == "Yes")

fig2_data_nonpeer <- fig2_data %>% 
  filter(peer == "No")

fig2_tidy_data <- fig2_data %>% 
  gather(2:6, key = question, value = response) %>% 
  filter(question != "apps_submitted_binned") %>% 
  mutate(question = case_when(
    question == "apps_submitted" ~ "Applications submitted",
    question == "faculty_offers" ~ "Faculty offers",
    question == "off_site_interviews" ~ "Off-site interviews",
    question == "on_site_interviews" ~ "On-site interviews"),
    simple_gender = factor(simple_gender, levels = gender_simple_breaks),
    peer = factor(peer, levels = peer_breaks),
    question = factor(question, levels = c(
      "Applications submitted", "Off-site interviews",
      "On-site interviews", "Faculty offers"))
    )

source("mollet_socialsci/code/figure_2.R")

#Figure 4 ----
fig4_data <- ss_tidy_data %>% 
  select(id, question, response) %>% 
  filter(!is.na(question)) %>% 
  filter(question %in% c("first_author", "predoctoral_fellow",
                         "peer-reviewed_papers", "scholar_hindex",
                         "scholar_citations_all", "grant_pi", 
                         "postdoctoral_fellow", "application_cycles", 
                         "apps_submitted", "faculty_offers",
                         "grant_copi", "teaching_status")) %>% 
  mutate(response = if_else(str_detect(response, ">5"), 
                            "5", response)) %>% 
  distinct()

offer_percent_data <- fig4_data %>% 
  filter(question == "apps_submitted" | question == "faculty_offers") %>% 
  spread(key = question, value = response) %>% 
  mutate(perc_offers = get_percent(faculty_offers, apps_submitted)) %>% 
  select(id, perc_offers)

fig4_summary <- fig4_data %>% 
  filter(question %not_in% c("postdoctoral_fellow",
                             "teaching_status",
                             "predoctoral_fellow")) %>% 
  group_by(question) %>% 
  summarise(n = n(), med = median(as.numeric(response), 
                                  na.rm = TRUE))

fig4_data_join <- left_join(fig4_data, offer_percent_data, by = "id") %>% 
  distinct() %>% 
  filter(!is.na(perc_offers))

source("mollet_socialsci/code/figure_4.R")

#Figure 5----
fig5_data <- left_join(ss_qualif_data, fig1_gender, by = "id") %>% 
  filter(!is.na(question)) %>% 
  filter(!is.na(response)) %>% 
  distinct()

fig5A_data <- fig5_data %>% 
  filter(question == "first_author_binned" | 
           question == "first_author") %>% 
  distinct() 

fig5ef_data <- ss_data %>% 
  select(id, postdoctoral_fellow, fellowship, grant_pi, grant_copi) %>% 
  left_join(., fig1_gender, by = "id") %>% distinct() %>% 
  mutate(grant = case_when(
    grant_pi == "Yes" ~ "Yes",
    grant_copi == "Yes" ~ "Yes",
    grant_pi == "NR" | grant_copi == "NR" ~ "NR",
    TRUE ~ "No")) %>% 
  filter(grant != "NR")

source("mollet_socialsci/code/figure_5.R")

#Figure 6----
fig6_data <- left_join(ss_qualif_data, fig1_demo, by = "id") %>% 
  filter(!is.na(question)) %>%
  filter(!is.na(response)) %>% 
  distinct()

fig6a_data <- fig6_data %>% 
  filter(question == "first_author_binned" | 
           question == "first_author") %>% 
  distinct() 

fig6ef_data <- ss_data %>% 
  select(id, postdoctoral_fellow, fellowship,
         grant_pi, grant_copi) %>% 
  left_join(., fig1_id, by = "id") %>% distinct() %>% 
  mutate(grant = case_when(
    grant_pi == "Yes" ~ "Yes",
    grant_copi == "Yes" ~ "Yes",
    grant_pi == "NR" | grant_copi == "NR" ~ "NR",
    TRUE ~ "No")) %>% 
  filter(grant != "NR")

source("mollet_socialsci/code/figure_6.R")

#Figure 7----
fig7_data <- ss_tidy_data %>% 
  select(id, question, response) %>% 
  filter(question %in% c("simple_gender", "off_site_interviews", 
                         "on_site_interviews", "peer", "faculty_offers", 
                         "apps_submitted_binned", "apps_submitted", 
                         "off_site_interviews_binned", "position",
                         "first_gen_undergrad", "disability_status",
                         "residence", "scholar_citations_all_binned",
                         "postdoctoral_fellow")
         ) %>% 
  distinct() %>% 
  spread(key = question, value = response) %>% 
  filter(!is.na(apps_submitted)) %>% 
  filter(faculty_offers != "NR") %>% 
  mutate(simple_faculty_offer = case_when(
    as.numeric(faculty_offers) == 0 ~ "0",
    as.numeric(faculty_offers) >= 1 ~ "1+"
  ),
  simple_faculty_offer = factor(simple_faculty_offer, 
                                levels = c("0", "1+")),
  peer = factor(peer, peer_breaks),
  simple_gender = factor(simple_gender, gender_simple_breaks),
  apps_submitted_binned = factor(apps_submitted_binned, 
                                 levels = bin_levels_small),
  off_site_interviews_binned = factor(off_site_interviews_binned,
                                      levels = bin_levels_small))# %>% 
  #left_join(., fellow_data, by = "id")

ss_id_simple_offer <- select(fig7_data, id, simple_faculty_offer)

app_dummy_var <- fig7_data %>% 
  select(apps_submitted_binned) %>% distinct() %>% 
  arrange(apps_submitted_binned) %>% 
  rowid_to_column(., "apps_bin_dummy")

offsite_dummy_var <- fig7_data %>% 
  filter(!is.na(off_site_interviews_binned)) %>% 
  select(off_site_interviews_binned) %>% distinct() %>% 
  arrange(off_site_interviews_binned) %>% 
  rowid_to_column(., "offsite_bin_dummy")

fig7_data_two <- fig7_data %>% 
  filter(simple_gender != "No Response") %>% 
  left_join(., app_dummy_var, by = "apps_submitted_binned") %>% 
  left_join(., offsite_dummy_var, by = "off_site_interviews_binned") %>% 
  mutate(gender_dummy = case_when(
    simple_gender == "Man" ~ 0,
    simple_gender == "Woman/Trans/GNC" ~ 1),
    on_site_interviews = as.numeric(on_site_interviews),
    offer_dummy = case_when(
      simple_faculty_offer == "0" ~ 0,
      simple_faculty_offer == "1+" ~ 1),
    disability_dummy = case_when(
      disability_status == "No" ~ 0,
      disability_status == "Yes, hidden" ~ 1,
      disability_status == "Yes, visible" ~ 2),
    undergrad_dummy = case_when(
      first_gen_undergrad == "No" ~ 0,
      first_gen_undergrad == "Yes" ~ 1),
    peer_dummy = case_when(
      peer == "Yes" ~ 1, 
      peer == "No" ~ 0),
    position_dummy = case_when(
      position == "PhD Candidate (ABD)" ~ 0,
      position == "Postdoc" ~ 1,
      position == "Non-tenure track faculty" ~ 2),
    residence_dummy = case_when(
      residence == "USA" ~ 0,
      residence == "Canada" ~ 1,
      residence == "Outside the US or CA" ~ 2),
    citation_dummy = case_when(
      scholar_citations_all_binned == "0" ~ 0, 
      scholar_citations_all_binned == "< 10" ~ 1, 
      scholar_citations_all_binned == "10-19" ~ 2, 
      scholar_citations_all_binned == "20-49" ~ 3, 
      scholar_citations_all_binned == "50-99" ~ 4, 
      scholar_citations_all_binned == "100-149" ~ 5, 
      scholar_citations_all_binned == "150-199" ~ 6, 
      scholar_citations_all_binned == "200-299" ~ 7, 
      scholar_citations_all_binned == "300-399" ~ 8, 
      scholar_citations_all_binned == "400-499" ~ 9, 
      scholar_citations_all_binned == "500-999" ~ 10, 
      scholar_citations_all_binned == "1000-1499" ~ 11),
    fellowship_dummy = case_when(
      postdoctoral_fellow == "Yes" ~ 1, 
      postdoctoral_fellow == "No" ~ 0)) %>% 
  distinct()

fig8_data <- fig7_data %>% 
  left_join(., app_dummy_var, by = "apps_submitted_binned") %>% 
  left_join(., offsite_dummy_var, by = "off_site_interviews_binned") %>% 
  mutate(peer_dummy = case_when(
    peer == "Yes" ~ 0,
    peer == "No" ~ 1),
    on_site_interviews = as.numeric(on_site_interviews)) %>% 
  distinct()

apps_bin_list <- c("0", "1", "2", "3", "4", 
                   "5-9", "10-14", "15-19", "20-29", 
                   "30-39", "40-49", "50-99", 
                   "100-199", "200-299", "300+")

offsite_bin_list <- c("0", "1", "2", "3", "4", 
                   "5-9", "10-14", "15-19", "20-29", 
                   "30-39")

fig7_data_women <- fig7_data %>% 
  filter(simple_gender == "Woman/Trans/GNC")

fig7_data_men <- fig7_data %>% 
  filter(simple_gender == "Man")

fig7_data_peer <- fig7_data %>% 
  filter(peer == "Yes")

fig7_data_nonpeer <- fig7_data %>% 
  filter(peer == "No")

fig7_tidy_data <- fig7_data %>% 
  gather(2:6, key = question, value = response) %>% 
  filter(!is.na(simple_faculty_offer))

source("mollet_socialsci/code/get_lm_model_outputs.R")

source("mollet_socialsci/code/figure_7.R")

#Figure 8----

source("mollet_socialsci/code/figure_8.R")

#generate tables ----
#demo table
demo_table <- ss_tidy_data %>% 
  filter(question %in% c("simple_gender", "age", "research_category", 
                         "residence", "peer", "dependents",
                         "position", "legal_status", "disability_status", 
                         "first_gen_undergrad", "first_gen_phd")) %>% 
  filter(!is.na(response)) %>% 
  distinct() %>% 
  mutate(response = fct_collapse(response, 
                                 "Citizen/Resident" = c("Citizen", "Permanent resident"),
                                 "Visa" = c("Student visa", "Work visa"),
                                 "Other" = c("Choose not to disclose", "Outside the US or CA"),
                                 "Yes" = c("Yes, hidden", "Yes, visible"),
                                 "Yes, multiple children/adult(s)" = c("Yes, adult(s)", "Yes, multiple children"),
                                 "< 30 years old" = c("20 - 25 years old", "26 - 30 years old")
                                                      )) %>% 
  count(id, question, response) %>% 
  count(question, response) %>% 
  mutate(percent_total = get_percent(n, 197))

write_csv(demo_table, paste0("mollet_socialsci/figures/demographics_",
                             Sys.Date(), ".csv"))

#table of application metrics
metrics_table <- fig4_data %>% 
  filter(question %not_in% c("postdoctoral_fellow", "grant_pi",
                             "teaching_status", "grant_copi",
                             "predoctoral_fellow")) %>% 
  filter(response != "NR") %>% 
  group_by(question) %>% 
  summarise(n = n(), med = median(as.numeric(response), 
                                  na.rm = TRUE), 
            std_dev = round(sd(as.numeric(response, na.rm = TRUE)), 
                            digits = 2),
            min_val = min(as.numeric(response, na.rm = TRUE)),
            max_val = max(as.numeric(response, na.rm = TRUE))) %>% 
  mutate(range = paste0("(", min_val, ", ", max_val, ")")) %>% 
  select(-min_val, -max_val)

write_csv(metrics_table, paste0("mollet_socialsci/figures/metrics_",
                                Sys.Date(), ".csv"))

metrics_table2 <- fig4_data %>% 
  filter(question %in% c("postdoctoral_fellow", "grant_pi",
                             "grant_copi", "predoctoral_fellow")) %>% 
  #filter(response != "NR") %>% 
  count(id, question, response) %>% 
  count(question, response) %>%  
  mutate(percent = get_percent(n, 197))

write_csv(metrics_table2, paste0("mollet_socialsci/figures/grants_",
                                 Sys.Date(), ".csv"))
