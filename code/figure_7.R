# Figure 7. Job Application benchmarks and their impact on success - Gender

#A. Off-site correlations with Application Numbers (Remote, on-site, offers) -- Differences in correlations by gender & race----
offsite_apps_cor_coefs <- cor.test(as.numeric(fig7_data$apps_submitted), 
                                    as.numeric(fig7_data$off_site_interviews))

offsite_apps_man_cor_coefs <- cor.test(as.numeric(fig7_data_men$apps_submitted), 
                                  as.numeric(fig7_data_men$off_site_interviews))

offsite_apps_wom_cor_coefs <- cor.test(as.numeric(fig7_data_women$apps_submitted), 
                                  as.numeric(fig7_data_women$off_site_interviews))


fig7a_plot <- ggplot(data = fig7_data_two, aes(x = as.numeric(fig7_data_two$apps_submitted), 
                             y = as.numeric(fig7_data_two$off_site_interviews))) +
  geom_point(aes(fill = simple_gender, color = simple_gender)) +
  scale_fill_manual(values = gender_color)+
  scale_color_manual(values = gender_color)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  geom_smooth(method=lm, se=TRUE, color="black", fill="#999999") +
  geom_smooth(aes(fill = simple_gender, color = simple_gender), method=lm, se=TRUE) +
  annotate("text", x = 100, y = 16, 
           label = paste0("R: ", round(offsite_apps_cor_coefs$estimate, 2))) +
  annotate("text", x = 100, y = 15, 
           label = paste0("p-value: ", round(offsite_apps_cor_coefs$p.value, 25)))+
  annotate("text", x = 80, y = 5, color = "#D55E00",
           label = paste0("R: ", round(offsite_apps_man_cor_coefs$estimate, 2))) +
  annotate("text", x = 80, y = 4, color = "#D55E00",
           label = paste0("p-value: ", round(offsite_apps_man_cor_coefs$p.value, 10)))+
  annotate("text", x = 130, y = 11, color = "#009E73",
           label = paste0("R: ", round(offsite_apps_wom_cor_coefs$estimate, 2))) +
  annotate("text", x = 130, y = 10, color = "#009E73",
           label = paste0("p-value: ", round(offsite_apps_wom_cor_coefs$p.value, 20)))+
  labs(y = "Number of off-site interviews", 
       x = "Number of applications submitted")+
  my_theme_horiz

ggsave(paste0("mollet_socialsci/figures/fig7a_", Sys.Date(), ".jpeg"))

#B. Onsite w/ correlations ----
onsite_apps_cor_coefs <- cor.test(as.numeric(fig7_data$apps_submitted), 
                                   as.numeric(fig7_data$on_site_interviews))

onsite_apps_man_cor_coefs <- cor.test(as.numeric(fig7_data_men$apps_submitted), 
                                       as.numeric(fig7_data_men$on_site_interviews))

onsite_apps_wom_cor_coefs <- cor.test(as.numeric(fig7_data_women$apps_submitted), 
                                       as.numeric(fig7_data_women$on_site_interviews))


fig7b_plot <- ggplot(data = fig7_data_two, aes(x = as.numeric(fig7_data_two$apps_submitted), 
                             y = as.numeric(fig7_data_two$on_site_interviews))) +
  geom_point(aes(fill = simple_gender, color = simple_gender)) +
  scale_fill_manual(values = gender_color)+
  scale_color_manual(values = gender_color)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  geom_smooth(method=lm, se=TRUE, color="black", fill="#999999") +
  geom_smooth(aes(fill = simple_gender, color = simple_gender), method=lm, se=TRUE)+
  annotate("text", x = 130, y = 2.5, 
           label = paste0("R: ", round(onsite_apps_cor_coefs$estimate, 2))) +
  annotate("text", x = 130, y = 2.3, 
           label = paste0("p-value: ", round(onsite_apps_cor_coefs$p.value, 5)))+
  annotate("text", x = 80, y = .75, color = "#009E73",
           label = paste0("R: ", round(onsite_apps_man_cor_coefs$estimate, 2))) +
  annotate("text", x = 80, y = .25, color = "#009E73",
           label = paste0("p-value: ", round(onsite_apps_man_cor_coefs$p.value, 5)))+
  annotate("text", x = 120, y = 6, color = "#D55E00",
           label = paste0("R: ", round(onsite_apps_wom_cor_coefs$estimate, 2))) +
  annotate("text", x = 120, y = 5.7, color = "#D55E00",
           label = paste0("p-value: ", round(onsite_apps_wom_cor_coefs$p.value, 5)))+
  labs(y = "Number of on-site interviews", x = "Number of applications submitted",
       fill = "Gender", color = "Gender")+
  my_theme_horiz

ggsave(paste0("mollet_socialsci/figures/fig7b_", Sys.Date(), ".jpeg"))

#C. faculty offers and applications ----
off_app_cor_coefs <- cor.test(as.numeric(fig7_data$apps_submitted), 
                              as.numeric(fig7_data$faculty_offers))

off_app_man_cor_coefs <- cor.test(as.numeric(fig7_data_men$apps_submitted), 
                                  as.numeric(fig7_data_men$faculty_offers))

off_app_wom_cor_coefs <- cor.test(as.numeric(fig7_data_women$apps_submitted), 
                                  as.numeric(fig7_data_women$faculty_offers))

fig7c_plot <- ggplot(data = fig7_data_two, aes(x = as.numeric(fig7_data_two$apps_submitted), 
                                 y = as.numeric(fig7_data_two$faculty_offers)))+
  geom_point(aes(fill = simple_gender, color = simple_gender)) +
  scale_fill_manual(values = gender_color)+
  scale_color_manual(values = gender_color)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  geom_smooth(method=lm, se=TRUE, color="black", fill="#999999") +
  geom_smooth(aes(fill = simple_gender, color = simple_gender), method=lm, se=TRUE)+
  annotate("text", x = 100, y = 2.25, 
           label = paste0("R: ", round(off_app_cor_coefs$estimate, 2))) +
  annotate("text", x = 100, y = 2, 
           label = paste0("p-value: ", round(off_app_cor_coefs$p.value, 5)))+
  annotate("text", x = 80, y = .3, color = "#009E73",
           label = paste0("R: ", round(off_app_man_cor_coefs$estimate, 2))) +
  annotate("text", x = 80, y = .15, color = "#009E73",
           label = paste0("p-value: ", round(off_app_man_cor_coefs$p.value, 5)))+
  annotate("text", x = 130, y = .3, color = "#D55E00",
           label = paste0("R: ", round(off_app_wom_cor_coefs$estimate, 2))) +
  annotate("text", x = 130, y = .15, color = "#D55E00",
           label = paste0("p-value: ", round(off_app_wom_cor_coefs$p.value, 5)))+
  labs(y = "Number of faculty offers", 
       x = "Number of applications submitted",
       fill = "Gender:", color = "Gender:")+
  my_theme_horiz

ggsave(paste0("mollet_socialsci/figures/fig7c_", Sys.Date(), ".jpeg"))

#D. Correlations between interviews and offers (remote, off-site) ----
offsite_offs_cor_coefs <- cor.test(as.numeric(fig7_data$faculty_offers), 
                                   as.numeric(fig7_data$off_site_interviews))

offsite_offs_man_cor_coefs <- cor.test(as.numeric(fig7_data_men$faculty_offers), 
                                       as.numeric(fig7_data_men$off_site_interviews))

offsite_offs_wom_cor_coefs <- cor.test(as.numeric(fig7_data_women$faculty_offers), 
                                       as.numeric(fig7_data_women$off_site_interviews))


fig7d_plot <- ggplot(data = fig7_data_two, aes(x = as.numeric(fig7_data_two$faculty_offers), 
                             y = as.numeric(fig7_data_two$off_site_interviews))) +
  geom_point(aes(fill = simple_gender, color = simple_gender)) +
  scale_fill_manual(values = gender_color)+
  scale_color_manual(values = gender_color)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  geom_smooth(method=lm, se=TRUE, color="black", fill="#999999") +
  geom_smooth(aes(fill = simple_gender, color = simple_gender), method=lm, se=TRUE) +
  annotate("text", x = 4.5, y = 16, 
           label = paste0("R: ", round(offsite_offs_cor_coefs$estimate, 2))) +
  annotate("text", x = 4.5, y = 15, 
           label = paste0("p-value: ", round(offsite_offs_cor_coefs$p.value, 5)))+
  annotate("text", x = 2, y = 12, color = "#009E73",
           label = paste0("R: ", round(offsite_offs_man_cor_coefs$estimate, 2))) +
  annotate("text", x = 2, y = 11, color = "#009E73",
           label = paste0("p-value: ", round(offsite_offs_man_cor_coefs$p.value, 5)))+
  annotate("text", x = 4.5, y = 2, color = "#D55E00",
           label = paste0("R: ", round(offsite_offs_wom_cor_coefs$estimate, 2))) +
  annotate("text", x = 4.5, y = 1, color = "#D55E00",
           label = paste0("p-value: ", round(offsite_offs_wom_cor_coefs$p.value, 5)))+
  labs(y = "Number of off-site interviews", x = "Number of faculty offers",
       fill = "Gender:", color = "Gender:")+
  my_theme_horiz

ggsave(paste0("mollet_socialsci/figures/fig7d_", Sys.Date(), ".jpeg"))

#E. Onsite & faculty offers----
onsite_offers_cor_coefs <- cor.test(as.numeric(fig7_data$faculty_offers), 
                                  as.numeric(fig7_data$on_site_interviews))

onsite_offers_man_cor_coefs <- cor.test(as.numeric(fig7_data_men$faculty_offers), 
                                      as.numeric(fig7_data_men$on_site_interviews))

onsite_offers_wom_cor_coefs <- cor.test(as.numeric(fig7_data_women$faculty_offers), 
                                      as.numeric(fig7_data_women$on_site_interviews))


fig7e_plot_leg <- ggplot(data = fig7_data_two, 
                         aes(x = as.numeric(faculty_offers), 
                             y = as.numeric(on_site_interviews))) +
  geom_point(aes(fill = simple_gender, color = simple_gender)) +
  scale_fill_manual(values = gender_color)+
  scale_color_manual(values = gender_color)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  geom_smooth(method=lm, se=TRUE, color="black", fill="#999999") +
  geom_smooth(aes(fill = simple_gender, color = simple_gender), method=lm, se=TRUE) +
  annotate("text", x = 4.5, y = 4, 
           label = paste0("R: ", round(onsite_offers_cor_coefs$estimate, 2))) +
  annotate("text", x = 4.5, y = 3.5, 
           label = paste0("p-value: ", round(onsite_offers_cor_coefs$p.value, 25)))+
  annotate("text", x = 2, y = 1.5, color = "#009E73",
           label = paste0("R: ", round(onsite_offers_man_cor_coefs$estimate, 2))) +
  annotate("text", x = 2, y = 1.25, color = "#009E73",
           label = paste0("p-value: ", round(onsite_offers_man_cor_coefs$p.value, 10)))+
  annotate("text", x = 3.5, y = 6.75, color = "#D55E00",
           label = paste0("R: ", round(onsite_offers_wom_cor_coefs$estimate, 2))) +
  annotate("text", x = 3.5, y = 6.5, color = "#D55E00",
           label = paste0("p-value: ", round(onsite_offers_wom_cor_coefs$p.value, 20)))+
  labs(y = "Number of on-site interviews", x = "Number of faculty offers",
       fill = "Gender:", color = "Gender:")+
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
                    label_size = 18, nrow = 1)

fig7de <- plot_grid(fig7d_plot, fig7e_plot, 
                    labels = c('D', 'E', ''),
                    label_size = 18, nrow = 1)

Fig7 <- plot_grid(fig7ab, fig7c, fig7de,
                  ncol = 1)

ggsave(filename = paste0("Figure_7_", Sys.Date(), ".png"), 
       device = 'png', units = "in", scale = 1.75,
       path = 'mollet_socialsci/figures/', 
       width = 8, height = 8)
