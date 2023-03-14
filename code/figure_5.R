# Figure 5 Scholarly Metrics by Gender

#A. 1st author pubs (Two-tailed Wilcoxon rank sum test)----
fig5A_mwu_data <- fig5A_data %>% 
  filter(question == "first_author") %>% 
  filter(simple_gender != "No Response") %>% distinct()

fig5A_wilcox <- wilcox.test(as.numeric(response) ~ simple_gender,
                            data = fig5A_mwu_data,
                            na.rm=TRUE, paired=FALSE, 
                            exact=FALSE, conf.int=TRUE)

save_wilcox(fig5A_wilcox, 
            file = paste0("mollet_socialsci/figures/fig5a_", Sys.Date(), "_mwu_test.csv"))

fig5a_plot <- fig5A_data %>% 
  filter(question == "first_author_binned") %>% 
  get_plot_summary(., "simple_gender", "response", 
                   binary = FALSE) %>% 
  mutate(response = factor(response, levels = bin_levels_small),
         simple_gender = factor(simple_gender, gender_simple_breaks)) %>% 
  ggplot(aes(x = response, y = percent,
             fill = simple_gender))+
  geom_col(position = "dodge")+
  scale_fill_manual(labels = gender_simple_breaks, 
                    values = gender_color)+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "Number of first-author papers\n",
       y = "Percent of respondents\nby gender",
       subtitle = "Mann Whitney U: p>0.05")+
  my_theme

ggsave(paste0("mollet_socialsci/figures/fig5a_", Sys.Date(), ".jpeg"))

#B. H-index (Two-tailed Wilcoxon rank sum test)----
fig5b_mwu_data <-fig5_data %>% 
  filter(question == "scholar_hindex") %>% 
  filter(simple_gender != "No Response") %>% 
  distinct() 

fig5b_wilcox <- wilcox.test(as.numeric(response) ~ simple_gender,
                            data = fig5b_mwu_data,
                            na.rm=TRUE, paired=FALSE, 
                            exact=FALSE, conf.int=TRUE)

save_wilcox(fig5b_wilcox, 
            file = paste0("mollet_socialsci/figures/fig5b_", Sys.Date(), "_mwu_test.csv"))

fig5b_plot <- fig5_data %>% 
  filter(question == "scholar_hindex_binned") %>% 
  get_plot_summary(., "simple_gender", "response", 
                   binary = FALSE) %>% 
  mutate(response = factor(response, 
                           levels = bin_levels_small),
         simple_gender = factor(simple_gender, gender_simple_breaks)) %>% 
  ggplot(aes(x = response, y = percent,
             fill = simple_gender))+
  geom_col(position = "dodge")+
  scale_fill_manual(labels = gender_simple_breaks, 
                    values = gender_color)+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "Applicant Google Scholar H-index\n",
       y = "Percent of respondents\nby gender",
       subtitle = "Mann Whitney U: p>0.05")+
  my_theme

ggsave(paste0("mollet_socialsci/figures/fig5b_", Sys.Date(), ".jpeg"))

#C. Total Pubs (Two-tailed Wilcoxon rank sum test) ----
fig5c_mwu_data <-fig5_data %>% 
  filter(question == "peer-reviewed_papers") %>% 
  filter(simple_gender != "No Response") %>% 
  distinct() 

fig5c_wilcox <- wilcox.test(as.numeric(response) ~ simple_gender,
                            data = fig5c_mwu_data,
                            na.rm=TRUE, paired=FALSE, 
                            exact=FALSE, conf.int=TRUE)

save_wilcox(fig5c_wilcox, 
            file = paste0("mollet_socialsci/figures/fig5c_", Sys.Date(), "_mwu_test.csv"))

fig5c_plot <- fig5_data %>% 
  filter(question == "peer-reviewed_papers_binned") %>% 
  get_plot_summary(., "simple_gender", "response", 
                   binary = FALSE) %>% 
  mutate(response = factor(response, 
                           levels = bin_levels_small),
         simple_gender = factor(simple_gender, gender_simple_breaks)) %>% 
  ggplot(aes(x = response, y = percent,
             fill = simple_gender))+
  geom_col(position = "dodge")+
  scale_fill_manual(labels = gender_simple_breaks, 
                    values = gender_color)+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "Number of peer-reviewed papers\n",
       y = "Percent of respondents\nby gender",
       subtitle = "Mann Whitney U: p>0.05")+
  my_theme

ggsave(paste0("mollet_socialsci/figures/fig5c_", Sys.Date(), ".jpeg"))

#D. Percent of applicants w/ grants (chi-square)----
fig5d_tab_data <- fig5ef_data %>% 
  filter(simple_gender != "No Response") 

fig5d_table <- table(fig5d_tab_data$grant, 
                     fig5d_tab_data$simple_gender)

#fig5d_table <- fig5d_table[,-3]

fig5d_chi <- chisq.test(fig5d_table, simulate.p.value = TRUE)

fig5d_plot <- fig5ef_data %>% 
  count(simple_gender, grant) %>% 
  spread(key = grant, value = n) %>% 
  mutate(total = Yes + No,
         percent = get_percent(Yes, total),
         gender_count = paste0(simple_gender, "\n(n = ", total, ")"),
         simple_gender = factor(simple_gender, gender_simple_breaks)) %>% 
  ggplot(aes(x=gender_count, y = as.numeric(percent),
             fill = simple_gender))+
  geom_col()+
  coord_flip()+
  scale_fill_manual(labels = gender_simple_breaks, 
                    values = gender_color)+
  scale_y_continuous(expand = c(0,0))+
  labs(y="Percent of responses\nby gender", 
       x="Applicants that recieved a grant",
       subtitle = paste0("Pearson's Chi-squared test with\nsimulated p-value\np>0.05"))+
  my_theme_horiz+
  right_margin

ggsave(paste0("mollet_socialsci/figures/fig5d_", Sys.Date(), ".jpeg"))

#E. All Citations (Two-tailed Wilcoxon rank sum test)----
fig5e_mwu_data <-fig5_data %>% 
  filter(question == "scholar_citations_all") %>% 
  distinct() 

fig5e_wilcox <- wilcox.test(as.numeric(response) ~ simple_gender,
                            data = fig5e_mwu_data,
                            na.rm=TRUE, paired=FALSE, 
                            exact=FALSE, conf.int=TRUE)

save_wilcox(fig5e_wilcox, 
            file = paste0("mollet_socialsci/figures/fig5e_", Sys.Date(), "_mwu_test.csv"))

fig5e_plot <- fig5_data %>% 
  filter(question == "scholar_citations_all_binned") %>% 
  get_plot_summary(., "simple_gender", "response", 
                   binary = FALSE) %>% 
  mutate(response = factor(response, levels = bin_levels_big),
         response = fct_explicit_na(response, "No Response"),
         simple_gender = factor(simple_gender, gender_simple_breaks)) %>% 
  ggplot(aes(x = response, y = percent,
             fill = simple_gender))+
  geom_col(position = "dodge")+
  scale_fill_manual(labels = gender_simple_breaks, 
                    values = gender_color)+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "Number of Google Scholar Citations\n",
       y = "Percent respondents\nby gender",
       subtitle = "Mann Whitney U: p>0.05")+
  my_theme

ggsave(paste0("mollet_socialsci/figures/fig5e_", Sys.Date(), ".jpeg"))

#F. Percent of applicants w/ fellowships (chi-square)----
fig5f_tab_data <- fig5ef_data %>% 
  select(id, simple_gender, fellowship) %>% 
  distinct()

fig5f_table <- table(fig5f_tab_data$fellowship, 
                     fig5f_tab_data$simple_gender)

#fig5f_table <- fig5f_table[,-3]

fig5f_chi <- chisq.test(fig5f_table, simulate.p.value = TRUE)

fig5f_plot <- fig5ef_data %>% 
  count(simple_gender, fellowship) %>% 
  spread(key = fellowship, value = n) %>% 
  mutate(total = Yes + No,
         percent = get_percent(Yes, total),
         gender_count = paste0(simple_gender, 
                                "\n(n = ", total, ")"),
         simple_gender = factor(simple_gender, gender_simple_breaks)) %>% 
  ggplot(aes(x=gender_count, y = as.numeric(percent),
             fill = simple_gender))+
  geom_col()+
  coord_flip()+
  scale_fill_manual(labels = gender_simple_breaks, 
                    values = gender_color)+
  scale_y_continuous(expand = c(0,0))+
  labs(y="Percent of responses\nby gender", 
       x="Gender of applicants that received a\npre- or postdoctoral fellowship",
       subtitle = "Pearson's Chi-squared test with\nsimulated p-value:\np>0.05")+
  my_theme_horiz

ggsave(paste0("mollet_socialsci/figures/fig5f_", Sys.Date(), ".jpeg"))

#generate plot----
fig5ab <- plot_grid(fig5a_plot, fig5b_plot, gender_plot_leg, 
                    labels = c('A', 'B', ''),
                    rel_widths = c(1, 1, .5),
                    label_size = 18, nrow = 1)

fig5cd <- plot_grid(fig5c_plot, fig5d_plot, 
                    labels = c('C', 'D', ''),
                    label_size = 18, nrow = 1)

fig5ef <- plot_grid(fig5e_plot, fig5f_plot,
                    labels = c('E', 'F'),
                    label_size = 18, nrow = 1)

Fig5 <- plot_grid(fig5ab, fig5cd,
                  fig5ef, ncol = 1)

ggsave(filename = paste0("Figure_5_", Sys.Date(), ".png"), 
       device = 'png', units = "in", scale = 1.75,
       path = 'mollet_socialsci/figures/', 
       width = 8, height = 10)