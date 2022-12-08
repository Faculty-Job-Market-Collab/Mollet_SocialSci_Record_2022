#Figure 4 - eLife fig 5B - metrics vs % offers

#A. teaching experience----
teach_data <- fig4_data %>% 
  filter(question == "teaching_status") %>% distinct()

fig4_teach_wilcox <- wilcox.test(perc_offers ~ response,
                            data = teach_data,
                            na.rm=TRUE, paired=FALSE, 
                            exact=FALSE, conf.int=TRUE)

fig4a_plot <- teach_data %>% 
  ggplot(aes(x = response, y = perc_offers, 
             color = response))+
  geom_violin()+
  geom_jitter()+
  coord_flip()+
  scale_color_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="Teaching Experience\n(n=151)", y="Percent offers\n",
       subtitle = "Mann Whitney U: ns")+
  my_theme_horiz

ggsave(filename = paste0("mollet_socialsci/figures/fig4a_",
                         Sys.Date(), ".jpeg"))

#B. total citations: above/below median----
cites_data <- fig4_data %>% 
  filter(question == "scholar_citations_all") %>% 
  mutate(median = if_else(response >= 42.0, 
                          "Above the\nmedian (42+)", 
                          "Below the\nmedian (<42)")) %>% 
  distinct()
  

fig4_cites_wilcox <- wilcox.test(perc_offers ~ median,
                              data = cites_data,
                              na.rm=TRUE, paired=FALSE, 
                              exact=FALSE, conf.int=TRUE)

fig4b_plot <- cites_data %>% 
  ggplot(aes(x = median, y = perc_offers, 
             color = median))+
  geom_violin()+
  geom_jitter()+
  coord_flip()+
  scale_color_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="\nGoogle Scholar\nCitations (n=93)", y="Percent offers\n",
       subtitle = "Mann Whitney U: ns")+
  my_theme_horiz

ggsave(filename = paste0("mollet_socialsci/figures/fig4b_",
                         Sys.Date(), ".jpeg"))

#C. years on the job market: above/below median----
years_data <- fig4_data %>% 
  filter(question == "application_cycles") %>% 
  mutate(median = if_else(response >= 2.0, "Above the\nmedian (2+)", 
                          "Below the\nmedian (1)")) %>% 
  distinct()


fig4_years_wilcox <- wilcox.test(perc_offers ~ median,
                                 data = years_data,
                                 na.rm=TRUE, paired=FALSE, 
                                 exact=FALSE, conf.int=TRUE)

fig4c_plot <- years_data %>% 
  ggplot(aes(x = median, y = perc_offers, 
             color = median))+
  geom_violin()+
  geom_jitter()+
  coord_flip()+
  scale_color_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="Application\nCycles (n=183)", 
       y="Percent offers\n",
       subtitle = "Mann Whitney U: ns")+
  my_theme_horiz

ggsave(filename = paste0("mollet_socialsci/figures/fig4c_",
                         Sys.Date(), ".jpeg"))

#D. postdoc fellowship: y/n----
fellow_data <- fig4_data %>% 
  filter(question == "grants_awarded") %>% distinct() %>% 
  select(-question) %>% 
  filter(response != "Transition to Independence Award ") %>% 
  mutate(status = "Yes",
         response = str_remove_all(response, "(?<=Grant).*")) %>% 
  distinct() %>% 
  spread(key = response, value = status) %>% 
  mutate(across(3:5, ~ if_else(is.na(.), "No", .)))

fig4_postfellow_wilcox <- wilcox.test(perc_offers ~ `Postdoctoral Fellowship`,
                              data = fellow_data,
                              na.rm=TRUE, paired=FALSE, 
                              exact=FALSE, conf.int=TRUE)

fig4d_plot <- fellow_data %>% 
  ggplot(aes(x = `Postdoctoral Fellowship`, y = perc_offers, 
             color = `Postdoctoral Fellowship`))+
  geom_violin()+
  geom_jitter()+
  coord_flip()+
  scale_color_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="\nPostdoctoral\nFellowship (n=151)", y="Percent offers\n",
       subtitle = "Mann Whitney U: p<0.05")+
  my_theme_horiz

ggsave(filename = paste0("mollet_socialsci/figures/fig4d_",
                         Sys.Date(), ".jpeg"))

#E. number of papers: above/below median----
papers_data <- fig4_data %>% 
  filter(question == "peer-reviewed_papers") %>% 
  mutate(median = if_else(response >= 5.0, "Above the\nmedian (5+)", 
                          "Below the\nmedian (<5)")) %>% 
  distinct()


fig4_papers_wilcox <- wilcox.test(perc_offers ~ median,
                                 data = papers_data,
                                 na.rm=TRUE, paired=FALSE, 
                                 exact=FALSE, conf.int=TRUE)

fig4e_plot <- papers_data %>% 
  ggplot(aes(x = median, y = perc_offers, 
             color = median))+
  geom_violin()+
  geom_jitter()+
  coord_flip()+
  scale_color_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="Peer-reviewed\nPublications (n=150)", y="Percent offers\n",
       subtitle = "Mann Whitney U: ns")+
  my_theme_horiz

ggsave(filename = paste0("mollet_socialsci/figures/fig4e_",
                         Sys.Date(), ".jpeg"))

#F. H-index: above/below median----
h_data <- fig4_data %>% 
  filter(question == "scholar_hindex") %>% 
  mutate(median = if_else(response >= 4.0, 
                          "Above the\nmedian (4+)", 
                          "Below the\nmedian (<4)")) %>% 
  distinct()


fig4_h_wilcox <- wilcox.test(perc_offers ~ median,
                                 data = h_data,
                                 na.rm=TRUE, paired=FALSE, 
                                 exact=FALSE, conf.int=TRUE)

fig4f_plot <- h_data %>% 
  ggplot(aes(x = median, y = perc_offers, 
             color = median))+
  geom_violin()+
  geom_jitter()+
  coord_flip()+
  scale_color_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="\nGoogle Scholar\nH-index (n=78)", y="Percent offers\n",
       subtitle = "Mann Whitney U: p<0.05")+
  my_theme_horiz

ggsave(filename = paste0("mollet_socialsci/figures/fig4f_",
                         Sys.Date(), ".jpeg"))

#G. 1st author papers: above/below median----
fauth_data <- fig4_data %>% 
  filter(question == "first_author") %>% 
  mutate(median = if_else(response >= 3.0, 
                          "Above the\nmedian (3+)", 
                          "Below the\nmedian (<3)")) %>% 
  distinct()


fig4_fauth_wilcox <- wilcox.test(perc_offers ~ median,
                                 data = fauth_data,
                                 na.rm=TRUE, paired=FALSE, 
                                 exact=FALSE, conf.int=TRUE)

fig4g_plot <- fauth_data %>% 
  ggplot(aes(x = median, y = perc_offers, 
             color = median))+
  geom_violin()+
  geom_jitter()+
  coord_flip()+
  scale_color_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="First-author\npublications (n=145)", y="Percent offers\n",
       subtitle = "Mann Whitney U: ns")+
  my_theme_horiz

ggsave(filename = paste0("mollet_socialsci/figures/fig4g_",
                         Sys.Date(), ".jpeg"))

#H. phd fellowship: y/n----
fig4_phdfellow_wilcox <- wilcox.test(perc_offers ~ `Predoctoral Fellowship`,
                                      data = fellow_data,
                                      na.rm=TRUE, paired=FALSE, 
                                      exact=FALSE, conf.int=TRUE)

fig4h_plot <- fellow_data %>% 
  ggplot(aes(x = `Predoctoral Fellowship`, y = perc_offers, 
             color = `Predoctoral Fellowship`))+
  geom_violin()+
  geom_jitter()+
  coord_flip()+
  scale_color_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="\nPredoctoral\nFellowship (n=151)", y="Percent offers\n",
       subtitle = "Mann Whitney U: ns")+
  my_theme_horiz

ggsave(filename = paste0("mollet_socialsci/figures/fig4h_",
                         Sys.Date(), ".jpeg"))

#generate plot----
Fig4 <- plot_grid(fig4a_plot, fig4b_plot, fig4c_plot, fig4d_plot,
                  fig4e_plot, fig4f_plot, fig4g_plot, fig4h_plot,
                  labels = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'),
                  label_size = 18, ncol = 2)

ggsave(filename = paste0("Figure_4_", Sys.Date(), ".png"), 
       device = 'png', units = "in", scale = 1.75,
       path = 'mollet_socialsci/figures/', width = 10, height = 11)
