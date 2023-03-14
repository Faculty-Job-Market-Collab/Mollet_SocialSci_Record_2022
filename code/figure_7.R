# Figure 7. Job Application benchmarks and their impact on success - Gender
#covariates: first-gen (undergrad), position, gender, PPER, 
#disability, residence, google scholar citations, and postdoc fellowship


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
  annotate("text", x = 100, y = 18, 
           label = paste0("R: ", round(offsite_apps_cor_coefs$estimate, 2))) +
  annotate("text", x = 100, y = 17, 
           label = paste0("p-value: ", round(offsite_apps_cor_coefs$p.value, 25)))+
  annotate("text", x = 80, y = 5, color = "#D55E00",
           label = paste0("R: ", round(offsite_apps_man_cor_coefs$estimate, 2))) +
  annotate("text", x = 80, y = 4, color = "#D55E00",
           label = paste0("p-value: ", round(offsite_apps_man_cor_coefs$p.value, 10)))+
  annotate("text", x = 110, y = 7, color = "#009E73",
           label = paste0("R: ", round(offsite_apps_wom_cor_coefs$estimate, 2))) +
  annotate("text", x = 110, y = 6, color = "#009E73",
           label = paste0("p-value: ", round(offsite_apps_wom_cor_coefs$p.value, 20)))+
  labs(y = "Number of off-site interviews", 
       x = "Number of applications submitted")+
  my_theme_horiz

#A interaction plot----
fig7a_lm <- lm(off_site_interviews ~ apps_bin_dummy*gender_dummy *
                 disability_dummy * undergrad_dummy * peer_dummy *
               position_dummy * residence_dummy * citation_dummy * 
               fellowship_dummy, data = fig7_data_two)

fig7a_coef <- summ(fig7a_lm)$coeftable

fig7a_coef <- get_wilcox_tbl("fig7a_coef", "7a") %>% 
  filter(!is.na(p)) %>% 
  mutate(p = round(p, digits = 2))

fig7a_lm_plot <- interact_plot(fig7a_lm, pred = apps_bin_dummy, modx = gender_dummy,
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
  annotate("text", x = 5, y = 0, size = 6,
           label=paste0("Intercept p=", fig7a_coef[1,5], 
                        "\nApplications p=", fig7a_coef[2,5], 
                        "\nGender p=", fig7a_coef[3,5], 
                        "\nApplications:Gender p=", fig7a_coef[11,5]))+
  my_theme

ggsave(paste0("mollet_socialsci/figures/fig7a_interaction_", Sys.Date(), ".jpeg"))

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
  annotate("text", x = 115, y = 2.5, 
           label = paste0("R: ", round(onsite_apps_cor_coefs$estimate, 2))) +
  annotate("text", x = 115, y = 2.2, 
           label = paste0("p-value: ", round(onsite_apps_cor_coefs$p.value, 5)))+
  annotate("text", x = 80, y = .75, color = "#009E73",
           label = paste0("R: ", round(onsite_apps_man_cor_coefs$estimate, 2))) +
  annotate("text", x = 80, y = .25, color = "#009E73",
           label = paste0("p-value: ", round(onsite_apps_man_cor_coefs$p.value, 5)))+
  annotate("text", x = 115, y = 7, color = "#D55E00",
           label = paste0("R: ", round(onsite_apps_wom_cor_coefs$estimate, 2))) +
  annotate("text", x = 115, y = 6.7, color = "#D55E00",
           label = paste0("p-value: ", round(onsite_apps_wom_cor_coefs$p.value, 5)))+
  labs(y = "Number of on-site interviews", x = "Number of applications submitted",
       fill = "Gender", color = "Gender")+
  my_theme_horiz

#B interaction plot----
fig7b_lm <- lm(on_site_interviews ~ apps_bin_dummy * gender_dummy *
                 disability_dummy * undergrad_dummy * peer_dummy *
                 position_dummy * residence_dummy * citation_dummy * 
                 fellowship_dummy, data = fig7_data_two)

fig7b_coef <- summ(fig7b_lm)$coeftable

fig7b_coef <- get_wilcox_tbl("fig7b_coef", "7b") %>% 
  filter(!is.na(p)) %>% 
  mutate(p = round(p, digits = 2))

fig7b_lm_plot <- interact_plot(fig7b_lm, pred = apps_bin_dummy, 
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
           label=paste0("Intercept p=", fig7b_coef[1,5], 
                        "\nApplications p=", fig7b_coef[2,5], 
                        "\nGender p=", fig7b_coef[3,5], 
                        "\nApplications:Gender p=", fig7b_coef[11,5]))+
  my_theme

ggsave(paste0("mollet_socialsci/figures/fig7b_interaction_", Sys.Date(), ".jpeg"))

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
  annotate("text", x = 110, y = .3, color = "#D55E00",
           label = paste0("R: ", round(off_app_wom_cor_coefs$estimate, 2))) +
  annotate("text", x = 110, y = .15, color = "#D55E00",
           label = paste0("p-value: ", round(off_app_wom_cor_coefs$p.value, 5)))+
  labs(y = "Number of faculty offers", 
       x = "Number of applications submitted",
       fill = "Gender:", color = "Gender:")+
  my_theme_horiz

#C interaction----
fig7c_lm <- lm(faculty_offers ~ apps_bin_dummy * gender_dummy, 
               data = fig7_data_two)

fig7c_coef <- summ(fig7c_lm)$coeftable

fig7c_coef <- get_wilcox_tbl("fig7c_coef", "7c") %>% 
  filter(!is.na(p)) %>% 
  mutate(p = round(p, digits = 2))

fig7c_lm_plot <- interact_plot(fig7c_lm, pred = apps_bin_dummy, 
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
           label=paste0("Intercept p=", fig7c_coef[1,5], 
                        "\nApplications p=", fig7c_coef[2,5], 
                        "\nGender p=", fig7c_coef[3,5], 
                        "\nApplications:Gender p=", fig7c_coef[4,5]))+
  my_theme

ggsave(paste0("mollet_socialsci/figures/fig7c_interaction_", Sys.Date(), ".jpeg"))

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

#D interaction----
fig7d_lm <- lm(faculty_offers ~ offsite_bin_dummy * gender_dummy, 
               data = fig7_data_two)

fig7d_coef <- summ(fig7d_lm)$coeftable

fig7d_coef <- get_wilcox_tbl("fig7d_coef", "7d") %>% 
  filter(!is.na(p)) %>% 
  mutate(p = round(p, digits = 2))

fig7d_lm_plot <- interact_plot(fig7d_lm, pred = offsite_bin_dummy, 
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
           label=paste0("Intercept p=", fig7d_coef[1,5], 
                        "\nApplications p=", fig7d_coef[2,5], 
                        "\nGender p=", fig7d_coef[3,5], 
                        "\nApplications:Gender p=", fig7d_coef[4,5]))+
  my_theme

ggsave(paste0("mollet_socialsci/figures/fig7d_interaction_", Sys.Date(), ".jpeg"))

#E. Onsite & faculty offers----
onsite_offers_cor_coefs <- cor.test(as.numeric(fig7_data$faculty_offers), 
                                    as.numeric(fig7_data$on_site_interviews))

onsite_offers_man_cor_coefs <- cor.test(as.numeric(fig7_data_men$faculty_offers), 
                                        as.numeric(fig7_data_men$on_site_interviews))

onsite_offers_wom_cor_coefs <- cor.test(as.numeric(fig7_data_women$faculty_offers), 
                                        as.numeric(fig7_data_women$on_site_interviews))

fig7e_plot <- ggplot(data = fig7_data_two, 
                         aes(x = as.numeric(faculty_offers), 
                             y = as.numeric(on_site_interviews))) +
  geom_point(aes(fill = simple_gender, color = simple_gender)) +
  scale_fill_manual(values = gender_color)+
  scale_color_manual(values = gender_color)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  geom_smooth(method=lm, se=TRUE, color="black", fill="#999999") +
  geom_smooth(aes(fill = simple_gender, color = simple_gender), method=lm, se=TRUE) +
  annotate("text", x = 4, y = 3.5, 
           label = paste0("R: ", round(onsite_offers_cor_coefs$estimate, 2))) +
  annotate("text", x = 4, y = 3, 
           label = paste0("p-value: ", round(onsite_offers_cor_coefs$p.value, 25)))+
  annotate("text", x = 2, y = 1.5, color = "#009E73",
           label = paste0("R: ", round(onsite_offers_man_cor_coefs$estimate, 2))) +
  annotate("text", x = 2, y = 1, color = "#009E73",
           label = paste0("p-value: ", round(onsite_offers_man_cor_coefs$p.value, 10)))+
  annotate("text", x = 3.5, y = 7, color = "#D55E00",
           label = paste0("R: ", round(onsite_offers_wom_cor_coefs$estimate, 2))) +
  annotate("text", x = 3.5, y = 6.5, color = "#D55E00",
           label = paste0("p-value: ", round(onsite_offers_wom_cor_coefs$p.value, 20)))+
  labs(y = "Number of on-site interviews", x = "Number of faculty offers",
       fill = "Gender:", color = "Gender:")+
  my_theme

#E interaction plot----
fig7e_lm <- lm(faculty_offers ~ on_site_interviews * gender_dummy, 
               data = fig7_data_two)

fig7e_coef <- summ(fig7e_lm)$coeftable

fig7e_coef <- get_wilcox_tbl("fig7e_coef", "7e") %>% 
  filter(!is.na(p)) %>% 
  mutate(p = round(p, digits = 2))

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
           label=paste0("Intercept p=", fig7e_coef[1,5], 
                        "\nApplications p=", fig7e_coef[2,5], 
                        "\nGender p=", fig7e_coef[3,5], 
                        "\nApplications:Gender p=", fig7e_coef[4,5]))+
  base_theme

fig7e_lm_plot <- fig7e_plot_leg + my_theme_horiz

ggsave(paste0("mollet_socialsci/figures/fig7e_interaction_", Sys.Date(), ".jpeg"))

#generate plots ----
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

#interaction plots
fig7ab_lm <- plot_grid(fig7a_lm_plot, fig7b_lm_plot, 
                    labels = c('A', 'B'),
                    label_size = 18, nrow = 1)

fig7c_lm <- plot_grid(fig7c_lm_plot, fig7_gender_leg,
                   labels = c('C', ''),
                   rel_widths = c(1, .5),
                   label_size = 18, nrow = 1)

fig7de_lm <- plot_grid(fig7d_lm_plot, fig7e_lm_plot, 
                    labels = c('D', 'E', ''),
                    label_size = 18, nrow = 1)

Fig7_lm <- plot_grid(fig7ab_lm, fig7c_lm, fig7de_lm,
                  ncol = 1)

ggsave(filename = paste0("Figure_7_interaction_", Sys.Date(), ".png"), 
       device = 'png', units = "in", scale = 1.75,
       path = 'mollet_socialsci/figures/', 
       width = 8, height = 10)