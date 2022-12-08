# Figure 6 Scholarly Metrics by peer

#A. 1st author pubs (Two-tailed Wilcoxon rank sum test)----
fig6A_mwu_data <- fig6a_data %>% 
  filter(question == "first_author") %>% 
  filter(peer != "No Response") %>% distinct()

fig6A_wilcox <- wilcox.test(as.numeric(response) ~ peer,
                            data = fig6A_mwu_data,
                            na.rm=TRUE, paired=FALSE, 
                            exact=FALSE, conf.int=TRUE)

save_wilcox(fig6A_wilcox, 
            file = paste0("mollet_socialsci/figures/fig6a_", Sys.Date(), "_mwu_test.csv"))

fig6a_plot_leg <- fig6a_data %>% 
  filter(question == "first_author_binned") %>% 
  get_plot_summary(., "peer", "response", 
                   binary = FALSE) %>% 
  mutate(response = factor(response, levels = bin_levels_small)) %>% 
  ggplot(aes(x = response, y = percent,
             fill = peer))+
  geom_col(position = "dodge")+
  scale_fill_manual(labels = peer_breaks, 
                    values = peer_color)+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "Number of first-author papers\n",
       y = "Percent of respondents\nby PEER status",
       fill = "PEER status",
       subtitle = "Mann Whitney U: p>0.05")+
  my_theme_leg

fig6a_plot <- fig6a_plot_leg + my_theme

ggsave(paste0("mollet_socialsci/figures/fig6a_", Sys.Date(), ".jpeg"))



#B. H-index (Two-tailed Wilcoxon rank sum test)----
fig6b_mwu_data <-fig6_data %>% 
  filter(question == "scholar_hindex") %>% 
  filter(peer != "No Response") %>% 
  distinct() 

fig6b_wilcox <- wilcox.test(as.numeric(response) ~ peer,
                            data = fig6b_mwu_data,
                            na.rm=TRUE, paired=FALSE, 
                            exact=FALSE, conf.int=TRUE)

save_wilcox(fig6b_wilcox, 
            file = paste0("mollet_socialsci/figures/fig6b_", Sys.Date(), "_mwu_test.csv"))

fig6b_plot <- fig6_data %>% 
  filter(question == "scholar_hindex_binned") %>% 
  get_plot_summary(., "peer", "response", 
                   binary = FALSE) %>% 
  mutate(response = factor(response, 
                           levels = bin_levels_small)) %>% 
  ggplot(aes(x = response, y = percent,
             fill = peer))+
  geom_col(position = "dodge")+
  scale_fill_manual(labels = peer_breaks, 
                    values = peer_color)+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "Applicant Google Scholar H-index\n",
       y = "Percent of respondents\nby PEER status",
       subtitle = "Mann Whitney U: p>0.05")+
  my_theme

ggsave(paste0("mollet_socialsci/figures/fig6b_", Sys.Date(), ".jpeg"))

#C. Total Pubs (Two-tailed Wilcoxon rank sum test) ----
fig6c_mwu_data <-fig6_data %>% 
  filter(question == "peer-reviewed_papers") %>% 
  filter(peer != "No Response") %>% 
  distinct() 

fig6c_wilcox <- wilcox.test(as.numeric(response) ~ peer,
                            data = fig6c_mwu_data,
                            na.rm=TRUE, paired=FALSE, 
                            exact=FALSE, conf.int=TRUE)

save_wilcox(fig6c_wilcox, 
            file = paste0("mollet_socialsci/figures/fig6c_", Sys.Date(), "_mwu_test.csv"))

fig6c_plot <- fig6_data %>% 
  filter(question == "peer-reviewed_papers_binned") %>% 
  get_plot_summary(., "peer", "response", 
                   binary = FALSE) %>% 
  mutate(response = factor(response, 
                           levels = bin_levels_small)) %>% 
  ggplot(aes(x = response, y = percent,
             fill = peer))+
  geom_col(position = "dodge")+
  scale_fill_manual(labels = peer_breaks, 
                    values = peer_color)+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "Number of peer-reviewed papers\n",
       y = "Percent of respondents\nby PEER status",
       subtitle = "Mann Whitney U: p>0.05")+
  my_theme

ggsave(paste0("mollet_socialsci/figures/fig6c_", Sys.Date(), ".jpeg"))

#D. Percent of applicants w/ grants (chi-square)----
fig6d_tab_data <- fig6ef_data %>% 
  filter(peer != "No Response") 

fig6d_table <- table(fig6d_tab_data$grant, 
                     fig6d_tab_data$peer)

fig6d_table <- fig6d_table[,-3]

fig6d_chi <- chisq.test(fig6d_table)

fig6d_plot <- fig6ef_data %>% 
  count(peer, grant) %>% 
  spread(key = grant, value = n) %>% 
  mutate(total = yes + no,
         percent = get_percent(yes, total),
         peer = paste0(peer, "\n(n = ", total, ")")) %>% 
  ggplot(aes(x=peer, y = as.numeric(percent),
             fill = peer))+
  geom_col()+
  coord_flip()+
  scale_fill_manual(labels = peer_breaks, 
                    values = peer_color)+
  scale_y_continuous(expand = c(0,0))+
  labs(y="Percent of responses\nby PEER status", 
       x="Applicants that recieved a grant",
       subtitle = paste0("Pearson's Chi-squared test with\nYates' continuity correction\np>0.05"))+
  my_theme_horiz+
  right_margin

ggsave(paste0("mollet_socialsci/figures/fig6d_", Sys.Date(), ".jpeg"))

#E. All Citations (Two-tailed Wilcoxon rank sum test)----
fig6e_mwu_data <-fig6_data %>% 
  filter(question == "scholar_citations_all") %>% 
  filter(peer != "No Response") %>% 
  distinct() 

fig6e_wilcox <- wilcox.test(as.numeric(response) ~ peer,
                            data = fig6e_mwu_data,
                            na.rm=TRUE, paired=FALSE, 
                            exact=FALSE, conf.int=TRUE)

save_wilcox(fig6e_wilcox, 
            file = paste0("mollet_socialsci/figures/fig6e_", Sys.Date(), "_mwu_test.csv"))

fig6e_plot <- fig6_data %>% 
  filter(question == "scholar_citations_all_binned") %>% 
  get_plot_summary(., "peer", "response", 
                   binary = FALSE) %>% 
  mutate(response = factor(response, levels = bin_levels_big),
         response = fct_explicit_na(response, "No Response")) %>% 
  ggplot(aes(x = response, y = percent,
             fill = peer))+
  geom_col(position = "dodge")+
  scale_fill_manual(labels = peer_breaks, 
                    values = peer_color)+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "Number of Google Scholar Citations\n",
       y = "Percent respondents\nby PEER status",
       subtitle = "Mann Whitney U: p>0.05")+
  my_theme

ggsave(paste0("mollet_socialsci/figures/fig6e_", Sys.Date(), ".jpeg"))

#F. Percent of applicants w/ fellowships (chi-square)----
fig6f_tab_data <- fig6ef_data %>% 
  filter(peer != "No Response") 

fig6f_table <- table(fig6f_tab_data$fellowship, 
                     fig6f_tab_data$peer)

fig6f_table <- fig6f_table[,-3]

fig6f_chi <- chisq.test(fig6f_table)

fig6f_plot <- fig6ef_data %>% 
  count(peer, fellowship) %>% 
  spread(key = fellowship, value = n) %>% 
  mutate(total = yes + no,
         percent = get_percent(yes, total),
         peer = paste0(peer, 
                                "\n(n = ", total, ")")) %>% 
  ggplot(aes(x=peer, y = as.numeric(percent),
             fill = peer))+
  geom_col()+
  coord_flip()+
  scale_fill_manual(labels = peer_breaks, 
                    values = peer_color)+
  scale_y_continuous(expand = c(0,0))+
  labs(y="Percent of responses\nby PEER status", 
       x="PEER status of applicants that received a\npre- or postdoctoral fellowship",
       subtitle = "Pearson's Chi-squared test with\nYates' continuity correction\np>0.05")+
  my_theme_horiz

ggsave(paste0("mollet_socialsci/figures/fig6f_", Sys.Date(), ".jpeg"))

#generate plot----
peer_plot_leg <- get_legend_plot(fig6a_plot_leg)

ggsave("mollet_socialsci/figures/peer_legend.jpeg")

fig6ab <- plot_grid(fig6a_plot, fig6b_plot, peer_plot_leg, 
                    labels = c('A', 'B', ''),
                    rel_widths = c(1, 1, .5),
                    label_size = 18, nrow = 1)

fig6cd <- plot_grid(fig6c_plot, fig6d_plot, 
                    labels = c('C', 'D', ''),
                    label_size = 18, nrow = 1)

fig6ef <- plot_grid(fig6e_plot, fig6f_plot,
                    labels = c('E', 'F'),
                    label_size = 18, nrow = 1)

Fig6 <- plot_grid(fig6ab, fig6cd,
                  fig6ef, ncol = 1)

ggsave(filename = paste0("Figure_6_", Sys.Date(), ".png"), 
       device = 'png', units = "in", scale = 1.75,
       path = 'mollet_socialsci/figures/', 
       width = 8, height = 10)