# Figure 7. Job Application benchmarks and their impact on success - Gender

#A. Off-site correlations with Application Numbers (Remote, on-site, offers) -- Differences in correlations by gender & race----
fig7a_lm <- lm(off_site_interviews ~ apps_bin_dummy*gender_dummy, 
               data = fig7_data_two)

sink(paste0("mollet_socialsci/figures/fig7a_summary_", Sys.Date(), ".txt"))
summ(fig7a_lm)
sink()

fig7a_plot <- interact_plot(fig7a_lm, pred = apps_bin_dummy, modx = gender_dummy,
              data = fig7_data_two, 
              modx.labels = c("Man", "Woman/Trans/GNC"),
              x.label = "Number of applications submitted", 
              y.label = "Number of off-site interviews",
              colors = gender_color)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7,
                                8, 9, 10, 11, 12, 13, 14, 15), 
                     labels = c("0", "1", "2", "3", "4", 
                                "5-9", "10-14", "15-19", "20-29",
                                "30-39", "40-49", "50-99",
                                "100-199", "200-299", "300+"))+
  annotate("text", x = 5, y = 5, size = 6,
           label="Intercept p=0.15\nApplications p<0.01\nGender p=0.92\nApplications:Gender p=0.63")+
  my_theme

ggsave(paste0("mollet_socialsci/figures/fig7a_", Sys.Date(), ".jpeg"))

#B. Onsite w/ correlations ----

fig7b_lm <- lm(on_site_interviews ~ apps_bin_dummy * gender_dummy, 
               data = fig7_data_two)

sink(paste0("mollet_socialsci/figures/fig7b_summary_", Sys.Date(), ".txt"))
summ(fig7b_lm)
sink()

fig7b_plot <- interact_plot(fig7b_lm, pred = apps_bin_dummy, 
                            modx = gender_dummy,
              data = fig7_data_two, 
              modx.labels = c("Man", "Woman/Trans/GNC"),
              x.label = "Number of applications submitted", 
              y.label = "Number of On-site Interviews",
              colors = gender_color)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7,
                                8, 9, 10, 11, 12, 13, 14, 15), 
                     labels = c("0", "1", "2", "3", "4", 
                                "5-9", "10-14", "15-19", "20-29",
                                "30-39", "40-49", "50-99",
                                "100-199", "200-299", "300+"))+
  annotate("text", x = 5, y = 2.25, size = 6,
           label="Intercept p=0.93\nApplications p<0.05\nGender p=0.72\nApplications:Gender p=0.16")+
  my_theme

ggsave(paste0("mollet_socialsci/figures/fig7b_", Sys.Date(), ".jpeg"))

#C. faculty offers and applications ----
fig7c_lm <- lm(faculty_offers ~ apps_bin_dummy * gender_dummy, 
               data = fig7_data_two)

sink(paste0("mollet_socialsci/figures/fig7c_summary_", Sys.Date(), ".txt"))
summ(fig7c_lm)
sink()

fig7c_plot <- interact_plot(fig7c_lm, pred = apps_bin_dummy, 
                            modx = gender_dummy,
                            data = fig7_data_two, 
                            modx.labels = c("Man", "Woman/Trans/GNC"),
                            x.label = "Number of applications submitted", 
                            y.label = "Number of faculty offers",
                            colors = gender_color)+
  scale_y_continuous(expand = c(0,0), breaks = c(0,1,2))+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7,
                                8, 9, 10, 11, 12, 13, 14, 15), 
                     labels = c("0", "1", "2", "3", "4", 
                                "5-9", "10-14", "15-19", "20-29",
                                "30-39", "40-49", "50-99",
                                "100-199", "200-299", "300+"))+
  annotate("text", x = 5, y = 0.9, size = 6,
           label="Intercept p=0.66\nApplications p=0.23\nGender p=0.92\nApplications:Gender p=0.47")+
  my_theme

ggsave(paste0("mollet_socialsci/figures/fig7c_", Sys.Date(), ".jpeg"))

#D. Correlations between interviews and offers (remote, off-site) ----
fig7d_lm <- lm(faculty_offers ~ offsite_bin_dummy * gender_dummy, 
               data = fig7_data_two)

sink(paste0("mollet_socialsci/figures/fig7d_summary_", Sys.Date(), ".txt"))
summ(fig7d_lm)
sink()

fig7d_plot <- interact_plot(fig7d_lm, pred = offsite_bin_dummy, 
                            modx = gender_dummy,
                            data = fig7_data_two, 
                            modx.labels = c("Man", "Woman/Trans/GNC"),
                            x.label = "Number of off-site interviews", 
                            y.label = "Number of faculty offers",
                            colors = gender_color)+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7,
                                8, 9, 10), 
                     labels = offsite_bin_list)+
  scale_y_continuous(expand = c(0,0), breaks = c(0,1,2))+
  annotate("text", x = 3, y = 1.25, size = 6,
           label="Intercept p=0.86\nInterviews p=0.01\nGender p=0.37\nInterviews:Gender p=0.74")+
  my_theme

ggsave(paste0("mollet_socialsci/figures/fig7d_", Sys.Date(), ".jpeg"))

#E. Onsite & faculty offers----
fig7e_lm <- lm(faculty_offers ~ on_site_interviews * gender_dummy, 
               data = fig7_data_two)

sink(paste0("mollet_socialsci/figures/fig7e_summary_", Sys.Date(), ".txt"))
summ(fig7e_lm)
sink()

fig7e_plot_leg <- interact_plot(fig7e_lm, 
                                pred = on_site_interviews, 
                            modx = gender_dummy,
                            data = fig7_data_two,
                            modx.labels = c("Man", "Woman/Trans/GNC"),
                            x.label = "Number of on-site interviews", 
                            y.label = "Number of faculty offers",
                            legend.main = "Gender",
                            colors = gender_color)+
  scale_y_continuous(expand = c(0,0), breaks = c(0,1,2,3,4))+
  annotate("text", x = 2, y = 2.25, size = 6,
           label="Intercept p=0.41\nInterviews p<0.01\nGender p=0.74\nInterviews:Gender p=0.62")+
  base_theme

fig7e_plot <- fig7e_plot_leg + my_theme_horiz

ggsave(paste0("mollet_socialsci/figures/fig7e_", Sys.Date(), ".jpeg"))

#generate plot ----
fig7_gender_leg <- get_legend_plot(fig7e_plot_leg)

ggsave("mollet_socialsci/figures/fig7_gender_leg.jpeg")

fig7ab <- plot_grid(fig7a_plot, fig7b_plot, 
                    labels = c('A', 'B'),
                    label_size = 18, nrow = 1)

fig7c <- plot_grid(fig7c_plot, fig7_gender_leg,
                   labels = c('C', ''),
                   rel_widths = c(1, .5),
                   label_size = 18, nrow = 1)

fig7de <- plot_grid(fig7d_plot, fig7e_plot, 
                    labels = c('D', 'E', ''),
                    label_size = 18, nrow = 1)

Fig7 <- plot_grid(fig7ab, fig7c, fig7de,
                  ncol = 1)

ggsave(filename = paste0("Figure_7_", Sys.Date(), ".png"), 
       device = 'png', units = "in", scale = 1.75,
       path = 'mollet_socialsci/figures/', 
       width = 8, height = 10)
