# Figure 9. Additional Search Influences & Preparation
#A. Gender Percentage attended job market prep workshops from third party sources -- Compare significant differences by race & gender----
fig9a_gen_table <- table(fig9_data$response, fig9_data$simple_gender)

fig9a_gen_table <- fig9a_gen_table[,-2]

fig9a_gen_chi <- chisq.test(fig9a_gen_table)

fig9a_plot <- fig9_data %>% 
  filter(!is.na(response)) %>% 
  count(simple_gender, response) %>% 
  spread(key = response, value = n) %>% 
  mutate(Total = Yes + No,
         Perc = get_percent(Yes, Total),
         gender = paste0(simple_gender, "\n(n=",
                                Total, ")")) %>% 
  ggplot(aes(x = fct_reorder(gender, Perc), y = Perc,
             fill = simple_gender))+
  geom_col()+
  scale_fill_manual(#breaks = simple_gender_breaks, 
                    values = gender_color)+
  scale_y_continuous(expand = c(0,0))+
  labs(y = "Attended third-party job market\nprep workshops (%)\n", x = "Gender\n",
       subtitle = "Pearson's Chi-squared test with Yates' continuity correction\np>0.05")+
  my_theme_horiz

ggsave(paste0("mollet_socialsci/figures/fig9a_", 
              Sys.Date(), ".jpeg"))

#B. Peer----
fig9b_peer_table <- table(fig9_data$response, fig9_data$peer)

fig9b_peer_chi <- chisq.test(fig9b_peer_table)

fig9b_plot <- fig9_data %>% 
  filter(!is.na(response)) %>% 
  count(peer, response) %>% 
  spread(key = response, value = n) %>% 
  mutate(Total = Yes + No,
         Perc = get_percent(Yes, Total),
         peer_num = paste0(peer, "\n(n=", Total, ")")
         ) %>% 
  ggplot(aes(x = fct_reorder(peer_num, Perc), 
             y = Perc, fill = peer))+
  geom_col()+
  scale_fill_manual(#breaks = peer_breaks, 
                    values = c("#F0E442", "#56B4E9"))+
  scale_y_continuous(expand = c(0,0))+
  labs(y = "Attended third-party job market\nprep workshops (%)\n", x = "PEER identity",
       subtitle = "Pearson's Chi-squared test with Yates' continuity correction\np>0.05")+
  my_theme_horiz

ggsave(paste0("mollet_socialsci/figures/fig9b_", 
              Sys.Date(), ".jpeg"))

#generate plot----
plot_grid(fig9a_plot, fig9b_plot, nrow = 2)

ggsave(filename = paste0("Figure_9_", Sys.Date(), ".png"), 
       device = 'png', units = "in", scale = 1.75,
       path = 'mollet_socialsci/figures/', width = 4, height = 6)

