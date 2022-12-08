# Figure 2 Job Application benchmarks and their impact on success

#A. Gender ----
fig2a_wilcox_data <-fig2_data %>% 
  filter(simple_gender != "No Response")

fig2a_apps_wilcox <- wilcox.test(as.numeric(apps_submitted) ~ simple_gender,
                                 data = fig2a_wilcox_data,
                                 na.rm=TRUE, paired=FALSE, 
                                 exact=FALSE, conf.int=TRUE)

fig2a_offers_wilcox <- wilcox.test(as.numeric(faculty_offers) ~ simple_gender,
                                data = fig2a_wilcox_data,
                                na.rm=TRUE, paired=FALSE, 
                                exact=FALSE, conf.int=TRUE)

fig2a_onsite_wilcox <- wilcox.test(as.numeric(on_site_interviews) ~ simple_gender,
                                data = fig2a_wilcox_data,
                                na.rm=TRUE, paired=FALSE, 
                                exact=FALSE, conf.int=TRUE)

fig2a_offsite_wilcox <- wilcox.test(as.numeric(off_site_interviews) ~ simple_gender,
                                data = fig2a_wilcox_data,
                                na.rm=TRUE, paired=FALSE, 
                                exact=FALSE, conf.int=TRUE)

wilcox_2a_list <- c("fig2a_apps_wilcox", "fig2a_offers_wilcox", 
                    "fig2a_onsite_wilcox", "fig2a_offsite_wilcox")

fig2_list <- c("2a apps submitted", "2a faculty offers", 
               "2a onsite offers", "2a offsite wilcox")

wilcox_2a_df <- map2_df(wilcox_2a_list, fig2_list, get_wilcox_tbl)

write_csv(wilcox_2a_df, 
            file = paste0("mollet_socialsci/figures/fig2a_", 
                          Sys.Date(), "_wilcox_test.csv"))

##plot----
fig2a_plot <- fig2_tidy_data %>%
  ggplot(aes(x = simple_gender, fill = simple_gender, 
             y = as.numeric(response)))+
  geom_boxplot(position = "dodge")+
  facet_wrap(~ question, scales = "free_x")+
  coord_flip()+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(breaks = gender_simple_breaks, values = gender_color)+
  labs(y = "Number per applicant", x = "Gender",
       subtitle = "Wilcoxon rank sum test\nwith continuity correction: ns")+
  my_theme_horiz+
  theme(strip.background = element_rect(linetype = "blank"),
        panel.spacing.x = unit(1, "cm"))

ggsave(filename = paste0("mollet_socialsci/figures/fig2a_", 
                         Sys.Date(), ".jpeg"))

#B. Peer----
fig2b_apps_wilcox <- wilcox.test(as.numeric(apps_submitted) ~ peer,
                                 data = fig2_data,
                                 na.rm=TRUE, paired=FALSE, 
                                 exact=FALSE, conf.int=TRUE)

fig2b_offers_wilcox <- wilcox.test(as.numeric(faculty_offers) ~ peer,
                                   data = fig2_data,
                                   na.rm=TRUE, paired=FALSE, 
                                   exact=FALSE, conf.int=TRUE)

fig2b_onsite_wilcox <- wilcox.test(as.numeric(on_site_interviews) ~ peer,
                                   data = fig2_data,
                                   na.rm=TRUE, paired=FALSE, 
                                   exact=FALSE, conf.int=TRUE)

fig2b_offsite_wilcox <- wilcox.test(as.numeric(off_site_interviews) ~ peer,
                                    data = fig2_data,
                                    na.rm=TRUE, paired=FALSE, 
                                    exact=FALSE, conf.int=TRUE)

wilcox_2b_list <- c("fig2b_apps_wilcox", "fig2b_offers_wilcox", 
                    "fig2b_onsite_wilcox", "fig2b_offsite_wilcox")

fig2_list <- c("2b apps submitted", "2b faculty offers", 
               "2b onsite offers", "2b offsite wilcox")

wilcox_2b_df <- map2_df(wilcox_2b_list, fig2_list, get_wilcox_tbl)

write_csv(wilcox_2b_df, 
          file = paste0("mollet_socialsci/figures/fig2b_", 
                        Sys.Date(), "_wilcox_test.csv"))
##plot----
fig2b_plot <- fig2_tidy_data %>% 
  ggplot(aes(x = peer, fill = peer, y = as.numeric(response)))+
  geom_boxplot(position = "dodge")+
  facet_wrap(~ question, scales = "free_x")+
  coord_flip()+
  scale_fill_manual(breaks = peer_breaks, values = peer_color)+
  scale_y_continuous(expand = c(0,0))+
  #scale_x_discrete(expand = c(0,0))+
  labs(y = "Number per applicant", x = "PEER identity",
       subtitle = "Wilcoxon rank sum test\nwith continuity correction: ns")+
  my_theme_horiz+
  theme(strip.background = element_rect(linetype = "blank"),
        panel.spacing.x = unit(1, "cm"))

ggsave(filename = paste0("mollet_socialsci/figures/fig2b_", 
                         Sys.Date(), ".jpeg"))

#generate plot----
plot_grid(fig2a_plot, fig2b_plot, nrow = 2)

ggsave(filename = paste0("Figure_2_", Sys.Date(), ".png"), 
       device = 'png', units = "in", scale = 1.75,
       path = 'mollet_socialsci/figures/', width = 8, height = 8)
