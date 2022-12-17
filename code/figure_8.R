# Figure 8. Job Application benchmarks and their impact on success

#A. ----
fig8a_lm <- lm(off_site_interviews ~ apps_bin_dummy*peer_dummy, 
               data = fig8_data)

sink(paste0("mollet_socialsci/figures/fig8a_summary_", Sys.Date(), ".txt"))
summ(fig8a_lm)
sink()

fig8a_plot <- interact_plot(fig8a_lm, pred = apps_bin_dummy, modx = peer_dummy,
                            data = fig8_data, 
                            modx.labels = c("Yes", "No"),
                            x.label = "Number of applications submitted", 
                            y.label = "Number of off-site interviews",
                            colors = peer_color)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7,
                                8, 9, 10, 11, 12, 13, 14, 15), 
                     labels = c("0", "1", "2", "3", "4", 
                                "5-9", "10-14", "15-19", "20-29",
                                "30-39", "40-49", "50-99",
                                "100-199", "200-299", "300+"))+
  my_theme

ggsave(paste0("mollet_socialsci/figures/fig8a_", Sys.Date(), ".jpeg"))

#B. ----
fig8b_lm <- lm(on_site_interviews ~ apps_bin_dummy*peer_dummy, 
               data = fig8_data)

sink(paste0("mollet_socialsci/figures/fig8b_summary_", Sys.Date(), ".txt"))
summ(fig8b_lm)
sink()

fig8b_plot <- interact_plot(fig8b_lm, pred = apps_bin_dummy, modx = peer_dummy,
                            data = fig8_data, 
                            modx.labels = c("Yes", "No"),
                            x.label = "Number of applications submitted", 
                            y.label = "Number of on-site Interviews",
                            colors = peer_color)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7,
                                8, 9, 10, 11, 12, 13, 14, 15), 
                     labels = c("0", "1", "2", "3", "4", 
                                "5-9", "10-14", "15-19", "20-29",
                                "30-39", "40-49", "50-99",
                                "100-199", "200-299", "300+"))+
  my_theme

ggsave(paste0("mollet_socialsci/figures/fig8b_", Sys.Date(), ".jpeg"))

#C. ----
fig8c_lm <- lm(off_site_interviews ~ apps_bin_dummy*peer_dummy, 
               data = fig8_data)

sink(paste0("mollet_socialsci/figures/fig8c_summary_", Sys.Date(), ".txt"))
summ(fig8c_lm)
sink()

fig8c_plot <- interact_plot(fig8c_lm, pred = apps_bin_dummy, modx = peer_dummy,
                            data = fig8_data, 
                            modx.labels = c("Yes", "No"),
                            x.label = "Number of applications submitted", 
                            y.label = "Number of faculty offers",
                            colors = peer_color)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7,
                                8, 9, 10, 11, 12, 13, 14, 15), 
                     labels = c("0", "1", "2", "3", "4", 
                                "5-9", "10-14", "15-19", "20-29",
                                "30-39", "40-49", "50-99",
                                "100-199", "200-299", "300+"))+
  my_theme

ggsave(paste0("mollet_socialsci/figures/fig8c_", Sys.Date(), ".jpeg"))

#D.----
fig8d_lm <- lm(faculty_offers ~ offsite_bin_dummy*peer_dummy, 
               data = fig8_data)

sink(paste0("mollet_socialsci/figures/fig8a_summary_", Sys.Date(), ".txt"))
summ(fig8d_lm)
sink()

fig8d_plot <- interact_plot(fig8d_lm, pred = offsite_bin_dummy, modx = peer_dummy,
                            data = fig8_data, 
                            modx.labels = c("Yes", "No"),
                            x.label = "Number of off-site interviews", 
                            y.label = "Number of faculty offers",
                            colors = peer_color)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7,
                                8, 9, 10), 
                     labels = c("0", "1", "2", "3", "4", 
                                "5-9", "10-14", "15-19", "20-29",
                                "30-39"))+
  my_theme

ggsave(paste0("mollet_socialsci/figures/fig8d_", Sys.Date(), ".jpeg"))

#E.----
fig8e_lm <- lm(faculty_offers ~ on_site_interviews*peer_dummy, 
               data = fig8_data)

sink(paste0("mollet_socialsci/figures/fig8a_summary_", Sys.Date(), ".txt"))
summ(fig8e_lm)
sink()

fig8e_plot_leg <- interact_plot(fig8e_lm, pred = on_site_interviews, 
                            modx = peer_dummy,
                            data = fig8_data, 
                            modx.labels = c("Yes", "No"),
                            x.label = "Number of on-site interviews", 
                            y.label = "Number of faculty offers",
                            legend.main = "PEER identity",
                            colors = peer_color)+
  scale_y_continuous(expand = c(0,0))+
  base_theme

fig8e_plot <- fig8e_plot_leg + my_theme_horiz

ggsave(paste0("mollet_socialsci/figures/fig8e_", Sys.Date(), ".jpeg"))

#generate plot ----
fig8_peer_leg <- get_legend_plot(fig8e_plot_leg)

ggsave("mollet_socialsci/figures/fig8_peer_leg.jpeg")

fig8ab <- plot_grid(fig8a_plot, fig8b_plot, 
                    labels = c('A', 'B'),
                    label_size = 18, nrow = 1)

fig8c <- plot_grid(fig8c_plot, fig8_peer_leg,
                   labels = c('C', ''),
                   rel_widths = c(1,0.5),
                   label_size = 18, nrow = 1)

fig8de <- plot_grid(fig8d_plot, fig8e_plot, 
                    labels = c('D', 'E'),
                    label_size = 18, nrow = 1)

Fig8 <- plot_grid(fig8ab, fig8c, fig8de,
                  ncol = 1)

ggsave(filename = paste0("Figure_8_", Sys.Date(), ".png"), 
       device = 'png', units = "in", scale = 1.75,
       path = 'mollet_socialsci/figures/', 
       width = 8, height = 10)