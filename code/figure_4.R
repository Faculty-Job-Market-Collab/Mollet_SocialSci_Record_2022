#Figure 4 - eLife fig 5B - metrics vs % offers

#A. teaching experience----
teach_data <- fig4_data_join %>% 
  filter(question == "teaching_status") %>% distinct() %>% 
  filter(!is.na(response))

#fig4_teach_wilcox <- wilcox.test(perc_offers ~ response,
#                            data = teach_data,
#                            na.rm=TRUE, paired=FALSE, 
#                            exact=FALSE, conf.int=TRUE)

fig4a_plot <- teach_data %>% 
  ggplot(aes(x = response, y = perc_offers, 
             color = response))+
  geom_violin()+
  geom_jitter()+
  coord_flip()+
  scale_color_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="Teaching Experience\n(n=174)", y="Percent offers\n",
       subtitle = "Mann Whitney U: ns"
       )+
  my_theme_horiz

ggsave(filename = paste0("mollet_socialsci/figures/fig4a_",
                         Sys.Date(), ".jpeg"))

#B. total citations: above/below median----
cites_data <- fig4_data_join %>% 
  filter(question == "scholar_citations_all") %>% 
  filter(!is.na(response)) %>% 
  mutate(median = if_else(response >= 37.0, 
                          "At/above the\nmedian (37+)", 
                          "Below the\nmedian (<37)")) %>% 
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
  labs(x="\nGoogle Scholar\nCitations (n=103)", y="Percent offers\n",
       subtitle = "Mann Whitney U: ns")+
  my_theme_horiz

ggsave(filename = paste0("mollet_socialsci/figures/fig4b_",
                         Sys.Date(), ".jpeg"))

#C. years on the job market: above/below median----
years_data <- fig4_data_join %>% 
  filter(question == "application_cycles") %>% 
  filter(!is.na(response)) %>% 
  mutate(median = if_else(response >= 2.0, "At/above the\nmedian (2+)", 
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
  labs(x="Application\ncycles (n=175)", 
       y="Percent offers\n",
       subtitle = "Mann Whitney U: ns")+
  my_theme_horiz

ggsave(filename = paste0("mollet_socialsci/figures/fig4c_",
                         Sys.Date(), ".jpeg"))

#D. postdoc fellowship: y/n----
pd_fellow_data <- fig4_data_join %>% 
  filter(question == "postdoctoral_fellow") %>% distinct() %>%
  filter(response != "NR")

fig4_postfellow_wilcox <- wilcox.test(perc_offers ~ response,
                              data = pd_fellow_data,
                              na.rm=TRUE, paired=FALSE, 
                              exact=FALSE, conf.int=TRUE)

fig4d_plot <- pd_fellow_data %>% 
  ggplot(aes(x = response, y = perc_offers, 
             color = response))+
  geom_violin()+
  geom_jitter()+
  coord_flip()+
  scale_color_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="\nPostdoctoral\nFellowship (n=117)", y="Percent offers\n",
       subtitle = "Mann Whitney U: p<0.01")+
  my_theme_horiz

ggsave(filename = paste0("mollet_socialsci/figures/fig4d_",
                         Sys.Date(), ".jpeg"))

#E. number of papers: above/below median----
papers_data <- fig4_data_join %>% 
  filter(question == "peer-reviewed_papers") %>% 
  filter(!is.na(response)) %>% 
  mutate(median = if_else(response >= 4.0, "At/above the\nmedian (4+)", 
                          "Below the\nmedian (<4)")) %>% 
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
  labs(x="Peer-reviewed\nPublications (n=172)", y="Percent offers\n",
       subtitle = "Mann Whitney U: ns")+
  my_theme_horiz

ggsave(filename = paste0("mollet_socialsci/figures/fig4e_",
                         Sys.Date(), ".jpeg"))

#F. H-index: above/below median----
h_data <- fig4_data_join %>% 
  filter(question == "scholar_hindex") %>% 
  filter(!is.na(response)) %>% 
  mutate(median = if_else(response >= 3.5, 
                          "Above the\nmedian (3.5+)", 
                          "Below the\nmedian (<3.5)")) %>% 
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
  labs(x="\nGoogle Scholar\nH-index (n=90)", y="Percent offers\n",
       subtitle = "Mann Whitney U: p<0.01")+
  my_theme_horiz

ggsave(filename = paste0("mollet_socialsci/figures/fig4f_",
                         Sys.Date(), ".jpeg"))

#G. 1st author papers: above/below median----
fauth_data <- fig4_data_join %>% 
  filter(question == "first_author") %>% 
  filter(!is.na(response)) %>% 
  mutate(median = if_else(response >= 2.0, 
                          "At/above the\nmedian (2+)", 
                          "Below the\nmedian (<2)")) %>% 
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
  labs(x="First-author\npublications (n=169)", y="Percent offers\n",
       subtitle = "Mann Whitney U: ns")+
  my_theme_horiz

ggsave(filename = paste0("mollet_socialsci/figures/fig4g_",
                         Sys.Date(), ".jpeg"))

#H. phd fellowship: y/n----
pre_fellow_data <- fig4_data_join %>% 
  filter(question == "predoctoral_fellow") %>% distinct() %>%
  filter(response != "NR")

fig4_phdfellow_wilcox <- wilcox.test(perc_offers ~ response,
                                      data = pre_fellow_data,
                                      na.rm=TRUE, paired=FALSE, 
                                      exact=FALSE, conf.int=TRUE)

fig4h_plot <- pre_fellow_data %>% 
  ggplot(aes(x = response, y = perc_offers, 
             color = response))+
  geom_violin()+
  geom_jitter()+
  coord_flip()+
  scale_color_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  labs(x="\nPredoctoral\nFellowship (n=117)", y="Percent offers\n",
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
