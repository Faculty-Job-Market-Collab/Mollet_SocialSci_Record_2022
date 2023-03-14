# Figure 8. Job Application benchmarks and their impact on success

#A. ----
offsite_apps_peer_cor_coefs <- cor.test(as.numeric(fig7_data_peer$apps_submitted), 
                                        as.numeric(fig7_data_peer$off_site_interviews))

offsite_apps_nonpeer_cor_coefs <- cor.test(as.numeric(fig7_data_nonpeer$apps_submitted), 
                                           as.numeric(fig7_data_nonpeer$off_site_interviews))

fig8a_plot <- ggplot(data = fig7_data, aes(x = as.numeric(fig7_data$apps_submitted), 
                                           y = as.numeric(fig7_data$off_site_interviews))) +
  geom_point(aes(fill = peer, color = peer)) +
  scale_fill_manual(labels = peer_breaks, values = peer_color)+
  scale_color_manual(labels = peer_breaks, values = peer_color)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  geom_smooth(method=lm, se=TRUE, color="black", fill="#999999") +
  geom_smooth(aes(fill = peer, color = peer), method=lm, se=TRUE) +
  annotate("text", x = 100, y = 16, 
           label = paste0("R: ", round(offsite_apps_cor_coefs$estimate, 2))) +
  annotate("text", x = 100, y = 15, 
           label = paste0("p-value: ", round(offsite_apps_cor_coefs$p.value, 5)))+
  #peer data
  annotate("text", x = 100, y = 5, color = "#E69F00",
           label = paste0("R: ", round(offsite_apps_peer_cor_coefs$estimate, 2))) +
  annotate("text", x = 100, y = 4, color = "#E69F00",
           label = paste0("p-value: ", round(offsite_apps_peer_cor_coefs$p.value, 5)))+
  #nonpeer data
  annotate("text", x = 115, y = 10, color = "#56B4E9",
           label = paste0("R: ", round(offsite_apps_nonpeer_cor_coefs$estimate, 2))) +
  annotate("text", x = 115, y = 9, color = "#56B4E9",
           label = paste0("p-value: ", round(offsite_apps_nonpeer_cor_coefs$p.value, 21)))+  
  labs(y = "Number of off-site interviews", x = "Number of applications submitted",
       fill = "PEER identity:", color = "PEER identity:")+
  my_theme_horiz

#A interaction----
fig8a_lm <- lm(off_site_interviews ~ apps_bin_dummy*peer_dummy, 
               data = fig8_data)

fig8a_coef <- summ(fig8a_lm)$coeftable

fig8a_coef <- get_wilcox_tbl("fig8a_coef", "8a") %>% 
  filter(!is.na(p)) %>% 
  mutate(p = round(p, digits = 2))

fig8a_lm_plot <- interact_plot(fig8a_lm, pred = apps_bin_dummy, modx = peer_dummy,
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
  annotate("text", x = 5, y = 5, size = 6,
           label=paste0("Intercept p=", fig8a_coef[1,5], 
                        "\nApplications p=", fig8a_coef[2,5], 
                        "\nPEER p=", fig8a_coef[3,5], 
                        "\nApplications:PEER p=", fig8a_coef[4,5]))+
  my_theme

ggsave(paste0("mollet_socialsci/figures/fig8a_interaction_", Sys.Date(), ".jpeg"))

#B. ----
onsite_apps_peer_cor_coefs <- cor.test(as.numeric(fig7_data_peer$apps_submitted), 
                                       as.numeric(fig7_data_peer$on_site_interviews))

onsite_apps_nonpeer_cor_coefs <- cor.test(as.numeric(fig7_data_nonpeer$apps_submitted), 
                                          as.numeric(fig7_data_nonpeer$on_site_interviews))

fig8b_plot <- ggplot(data = fig7_data, aes(x = as.numeric(apps_submitted), 
                                           y = as.numeric(on_site_interviews))) +
  geom_point(aes(fill = peer, color = peer)) +
  scale_fill_manual(labels = peer_breaks, values = peer_color)+
  scale_color_manual(labels = peer_breaks, values = peer_color)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  geom_smooth(method=lm, se=TRUE, color="black", fill="#999999")+
  geom_smooth(aes(fill = peer, color = peer), method=lm, se=TRUE)+
  annotate("text", x = 115, y = 3, 
           label = paste0("R: ", round(onsite_apps_cor_coefs$estimate, 2))) +
  annotate("text", x = 115, y = 2.5, 
           label = paste0("p-value: ", round(onsite_apps_cor_coefs$p.value, 10)))+
  #peer data
  annotate("text", x = 95, y = 5, color = "#56B4E9",
           label = paste0("R: ", round(onsite_apps_peer_cor_coefs$estimate, 2))) +
  annotate("text", x = 95, y = 4.5, color = "#56B4E9",
           label = paste0("p-value: ", round(onsite_apps_peer_cor_coefs$p.value, 5)))+
  #nonpeer data
  annotate("text", x = 115, y = 6, color = "#E69F00",
           label = paste0("R: ", round(onsite_apps_nonpeer_cor_coefs$estimate, 2))) +
  annotate("text", x = 115, y = 5.5, color = "#E69F00",
           label = paste0("p-value: ", round(onsite_apps_nonpeer_cor_coefs$p.value, 10)))+  
  labs(y = "Number of on-site interviews", x = "Number of applications submitted",
       fill = "PEER identity:", color = "PEER identity:")+
  my_theme_horiz

#B interaction----
fig8b_lm <- lm(on_site_interviews ~ apps_bin_dummy*peer_dummy, 
               data = fig8_data)

fig8b_coef <- summ(fig8b_lm)$coeftable

fig8b_coef <- get_wilcox_tbl("fig8b_coef", "8b") %>% 
  filter(!is.na(p)) %>% 
  mutate(p = round(p, digits = 2))

fig8b_lm_plot <- interact_plot(fig8b_lm, pred = apps_bin_dummy, modx = peer_dummy,
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
  annotate("text", x = 4, y = 2.25, size = 6,
           label=paste0("Intercept p=", fig8b_coef[1,5], 
                        "\nApplications p=", fig8b_coef[2,5], 
                        "\nPEER p=", fig8b_coef[3,5], 
                        "\nApplications:PEER p=", fig8b_coef[4,5]))+
  my_theme

ggsave(paste0("mollet_socialsci/figures/fig8b_interaction_", Sys.Date(), ".jpeg"))

#C. ----
off_app_peer_cor_coefs <- cor.test(as.numeric(fig7_data_peer$apps_submitted), 
                                   as.numeric(fig7_data_peer$faculty_offers))

off_app_nonpeer_cor_coefs <- cor.test(as.numeric(fig7_data_nonpeer$apps_submitted), 
                                      as.numeric(fig7_data_nonpeer$faculty_offers))

fig8c_plot <- ggplot(data = fig7_data, aes(x = as.numeric(fig7_data$apps_submitted), 
                                           y = as.numeric(fig7_data$faculty_offers))) +
  geom_point(aes(fill = peer, color = peer)) +
  scale_fill_manual(labels = peer_breaks, values = peer_color)+
  scale_color_manual(labels = peer_breaks, values = peer_color)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  geom_smooth(method=lm, se=TRUE, color="black", fill="#999999") +
  geom_smooth(aes(fill = peer, color = peer), method=lm, se=TRUE) +
  annotate("text", x = 100, y = 2, 
           label = paste0("R: ", round(off_app_cor_coefs$estimate, 2))) +
  annotate("text", x = 100, y = 1.75, 
           label = paste0("p-value: ", round(off_app_cor_coefs$p.value, 5)))+
  #peer data
  annotate("text", x = 80, y = 4.5, color = "#56B4E9",
           label = paste0("R: ", round(off_app_peer_cor_coefs$estimate, 2))) +
  annotate("text", x = 80, y = 4.25, color = "#56B4E9",
           label = paste0("p-value: ", round(off_app_peer_cor_coefs$p.value, 5)))+
  #nonpeer data
  annotate("text", x = 115, y = 0.5, color = "#E69F00",
           label = paste0("R: ", round(off_app_nonpeer_cor_coefs$estimate, 2))) +
  annotate("text", x = 115, y = .2, color = "#E69F00",
           label = paste0("p-value: ", round(off_app_nonpeer_cor_coefs$p.value, 5)))+
  labs(y = "Number of faculty offers", x = "Number of applications submitted",
       fill = "PEER identity:", color = "PEER identity:")+
  my_theme_horiz

#C interaction----
fig8c_lm <- lm(off_site_interviews ~ apps_bin_dummy*peer_dummy, 
               data = fig8_data)

fig8c_coef <- summ(fig8c_lm)$coeftable

fig8c_coef <- get_wilcox_tbl("fig8c_coef", "8c") %>% 
  filter(!is.na(p)) %>% 
  mutate(p = round(p, digits = 2))

fig8c_lm_plot <- interact_plot(fig8c_lm, pred = apps_bin_dummy, modx = peer_dummy,
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
  annotate("text", x = 5, y = 5, size = 6,
           label=paste0("Intercept p=", fig8c_coef[1,5], 
                        "\nApplications p=", fig8c_coef[2,5], 
                        "\nPEER p=", fig8c_coef[3,5], 
                        "\nApplications:PEER p=", fig8c_coef[4,5]))+
  my_theme

ggsave(paste0("mollet_socialsci/figures/fig8c_interaction_", Sys.Date(), ".jpeg"))

#D.----
offsite_offers_peer_cor_coefs <- cor.test(as.numeric(fig7_data_peer$faculty_offers), 
                                          as.numeric(fig7_data_peer$off_site_interviews))

offsite_offers_nonpeer_cor_coefs <- cor.test(as.numeric(fig7_data_nonpeer$faculty_offers), 
                                             as.numeric(fig7_data_nonpeer$off_site_interviews))

fig8d_plot <- ggplot(data = fig7_data, aes(x = as.numeric(faculty_offers), 
                                           y = as.numeric(off_site_interviews))) +
  geom_point(aes(fill = peer, color = peer)) +
  scale_fill_manual(labels = peer_breaks, values = peer_color)+
  scale_color_manual(labels = peer_breaks, values = peer_color)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  geom_smooth(method=lm, se=TRUE, color="black", fill="#999999") +
  geom_smooth(aes(fill = peer, color = peer), method=lm, se=TRUE) +
  annotate("text", x = 4.5, y = 13, 
           label = paste0("R: ", round(offsite_offs_cor_coefs$estimate, 2))) +
  annotate("text", x = 4.5, y = 12, 
           label = paste0("p-value: ", round(offsite_offs_cor_coefs$p.value, 5)))+
  #peer data
  annotate("text", x = .5, y = 15, color = "#56B4E9",
           label = paste0("R: ", round(offsite_offers_peer_cor_coefs$estimate, 2))) +
  annotate("text", x = .5, y = 14, color = "#56B4E9",
           label = paste0("p-value: ", round(offsite_offers_peer_cor_coefs$p.value, 5)))+
  #nonpeer data
  annotate("text", x = 4, y = 17, color = "#E69F00",
           label = paste0("R: ", round(offsite_offers_nonpeer_cor_coefs$estimate, 2))) +
  annotate("text", x = 4, y = 16, color = "#E69F00",
           label = paste0("p-value: ", round(offsite_offers_nonpeer_cor_coefs$p.value, 7)))+  
  labs(y = "Number of off-site interviews", x = "Number of faculty offers",
       fill = "PEER identity:", color = "PEER identity:")+
  my_theme_horiz

#D interaction----
fig8d_lm <- lm(faculty_offers ~ offsite_bin_dummy*peer_dummy, 
               data = fig8_data)

fig8d_coef <- summ(fig8d_lm)$coeftable

fig8d_coef <- get_wilcox_tbl("fig8d_coef", "8d") %>% 
  filter(!is.na(p)) %>% 
  mutate(p = round(p, digits = 2))

fig8d_lm_plot <- interact_plot(fig8d_lm, pred = offsite_bin_dummy, modx = peer_dummy,
                            data = fig8_data, 
                            modx.labels = c("Yes", "No"),
                            x.label = "Number of off-site interviews", 
                            y.label = "Number of faculty offers",
                            colors = peer_color)+
  scale_y_continuous(expand = c(0,0), breaks = c(0,1,2))+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7,
                                8, 9, 10), 
                     labels = c("0", "1", "2", "3", "4", 
                                "5-9", "10-14", "15-19", "20-29",
                                "30-39"))+
  annotate("text", x = 3, y = 1.25, size = 6,
           label=paste0("Intercept p=", fig8d_coef[1,5], 
                        "\nApplications p=", fig8d_coef[2,5], 
                        "\nPEER p=", fig8d_coef[3,5], 
                        "\nApplications:PEER p=", fig8d_coef[4,5]))+
  my_theme

ggsave(paste0("mollet_socialsci/figures/fig8d_interaction_", Sys.Date(), ".jpeg"))

#E.----
onsite_offers_peer_cor_coefs <- cor.test(as.numeric(fig7_data_peer$faculty_offers), 
                                         as.numeric(fig7_data_peer$on_site_interviews))

onsite_offers_nonpeer_cor_coefs <- cor.test(as.numeric(fig7_data_nonpeer$faculty_offers), 
                                            as.numeric(fig7_data_nonpeer$on_site_interviews))

fig8e_plot <- ggplot(data = fig7_data, aes(x = as.numeric(faculty_offers), 
                                               y = as.numeric(on_site_interviews))) +
  geom_point(aes(fill = peer, color = peer)) +
  scale_fill_manual(labels = peer_breaks, values = peer_color)+
  scale_color_manual(labels = peer_breaks, values = peer_color)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  geom_smooth(method=lm, se=TRUE, color="black", fill="#999999")+
  geom_smooth(aes(fill = peer, color = peer), method=lm, se=TRUE)+
  annotate("text", x = 4, y = 7.5, 
           label = paste0("R: ", round(onsite_offers_cor_coefs$estimate, 2))) +
  annotate("text", x = 4, y = 7.2, 
           label = paste0("p-value: ", round(onsite_offers_cor_coefs$p.value, 25)))+
  #peer data
  annotate("text", x = 4, y = 2.5, color = "#56B4E9",
           label = paste0("R: ", round(onsite_offers_peer_cor_coefs$estimate, 2))) +
  annotate("text", x = 4, y = 2.20, color = "#56B4E9",
           label = paste0("p-value: ", round(onsite_offers_peer_cor_coefs$p.value, 10)))+
  #nonpeer data
  annotate("text", x = 2.5, y = 5.5, color = "#E69F00",
           label = paste0("R: ", round(onsite_offers_nonpeer_cor_coefs$estimate, 2))) +
  annotate("text", x = 2.5, y = 5.2, color = "#E69F00",
           label = paste0("p-value: ", round(onsite_offers_nonpeer_cor_coefs$p.value, 20)))+  
  labs(y = "Number of on-site interviews", x = "Number of faculty offers",
       fill = "PEER identity:", color = "PEER identity:")+
  my_theme

#E interaction----
fig8e_lm <- lm(faculty_offers ~ on_site_interviews*peer_dummy, 
               data = fig8_data)

fig8e_coef <- summ(fig8e_lm)$coeftable

fig8e_coef <- get_wilcox_tbl("fig8e_coef", "8e") %>% 
  filter(!is.na(p)) %>% 
  mutate(p = round(p, digits = 2))

fig8e_plot_leg <- interact_plot(fig8e_lm, pred = on_site_interviews, 
                            modx = peer_dummy,
                            data = fig8_data, 
                            modx.labels = c("Yes", "No"),
                            x.label = "Number of on-site interviews", 
                            y.label = "Number of faculty offers",
                            legend.main = "PEER identity",
                            colors = peer_color)+
  scale_y_continuous(expand = c(0,0))+
  annotate("text", x = 2.5, y = 2.5, size = 6,
           label=paste0("Intercept p=", fig8e_coef[1,5], 
                        "\nApplications p=", fig8e_coef[2,5], 
                        "\nPEER p=", fig8e_coef[3,5], 
                        "\nApplications:PEER p=", fig8e_coef[4,5]))+
  base_theme

fig8e_lm_plot <- fig8e_plot_leg + my_theme_horiz

ggsave(paste0("mollet_socialsci/figures/fig8e_interaction_", Sys.Date(), ".jpeg"))

#generate plots ----
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

#interaction plots----
fig8_peer_leg <- get_legend_plot(fig8e_plot_leg)

ggsave("mollet_socialsci/figures/fig8_peer_leg.jpeg")

fig8ab_lm <- plot_grid(fig8a_lm_plot, fig8b_lm_plot, 
                    labels = c('A', 'B'),
                    label_size = 18, nrow = 1)

fig8c_lm <- plot_grid(fig8c_lm_plot, fig8_peer_leg,
                   labels = c('C', ''),
                   rel_widths = c(1,0.5),
                   label_size = 18, nrow = 1)

fig8de_lm <- plot_grid(fig8d_lm_plot, fig8e_lm_plot, 
                    labels = c('D', 'E'),
                    label_size = 18, nrow = 1)

Fig8_lm <- plot_grid(fig8ab_lm, fig8c_lm, fig8de_lm,
                  ncol = 1)

ggsave(filename = paste0("Figure_8_interaction_", Sys.Date(), ".png"), 
       device = 'png', units = "in", scale = 1.75,
       path = 'mollet_socialsci/figures/', 
       width = 8, height = 10)