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
  annotate("text", x = 130, y = 10, color = "#56B4E9",
           label = paste0("R: ", round(offsite_apps_nonpeer_cor_coefs$estimate, 2))) +
  annotate("text", x = 130, y = 9, color = "#56B4E9",
           label = paste0("p-value: ", round(offsite_apps_nonpeer_cor_coefs$p.value, 21)))+  
  labs(y = "Number of off-site interviews", x = "Number of applications submitted",
       fill = "PEER identity:", color = "PEER identity:")+
  my_theme_horiz

ggsave(paste0("mollet_socialsci/figures/fig8a_", Sys.Date(), ".jpeg"))

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
  annotate("text", x = 125, y = 3, 
           label = paste0("R: ", round(onsite_apps_cor_coefs$estimate, 2))) +
  annotate("text", x = 125, y = 2.5, 
           label = paste0("p-value: ", round(onsite_apps_cor_coefs$p.value, 10)))+
  #peer data
  annotate("text", x = 95, y = 5, color = "#E69F00",
           label = paste0("R: ", round(onsite_apps_peer_cor_coefs$estimate, 2))) +
  annotate("text", x = 95, y = 4.5, color = "#E69F00",
           label = paste0("p-value: ", round(onsite_apps_peer_cor_coefs$p.value, 5)))+
  #nonpeer data
  annotate("text", x = 130, y = 6, color = "#56B4E9",
           label = paste0("R: ", round(onsite_apps_nonpeer_cor_coefs$estimate, 2))) +
  annotate("text", x = 130, y = 5.5, color = "#56B4E9",
           label = paste0("p-value: ", round(onsite_apps_nonpeer_cor_coefs$p.value, 10)))+  
  labs(y = "Number of on-site interviews", x = "Number of applications submitted",
       fill = "PEER identity:", color = "PEER identity:")+
  my_theme_horiz

ggsave(paste0("mollet_socialsci/figures/fig8b_", Sys.Date(), ".jpeg"))

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
  annotate("text", x = 80, y = 4.5, color = "#E69F00",
           label = paste0("R: ", round(off_app_peer_cor_coefs$estimate, 2))) +
  annotate("text", x = 80, y = 4.25, color = "#E69F00",
           label = paste0("p-value: ", round(off_app_peer_cor_coefs$p.value, 5)))+
  #nonpeer data
  annotate("text", x = 130, y = 0.5, color = "#56B4E9",
           label = paste0("R: ", round(off_app_nonpeer_cor_coefs$estimate, 2))) +
  annotate("text", x = 130, y = .2, color = "#56B4E9",
           label = paste0("p-value: ", round(off_app_nonpeer_cor_coefs$p.value, 5)))+
  labs(y = "Number of faculty offers", x = "Number of applications submitted",
       fill = "PEER identity:", color = "PEER identity:")+
  my_theme_horiz

ggsave(paste0("mollet_socialsci/figures/fig8c_", Sys.Date(), ".jpeg"))

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
  annotate("text", x = .5, y = 1, color = "#E69F00",
           label = paste0("R: ", round(offsite_offers_peer_cor_coefs$estimate, 2))) +
  annotate("text", x = .5, y = 0.25, color = "#E69F00",
           label = paste0("p-value: ", round(offsite_offers_peer_cor_coefs$p.value, 5)))+
  #nonpeer data
  annotate("text", x = 4, y = 17, color = "#56B4E9",
           label = paste0("R: ", round(offsite_offers_nonpeer_cor_coefs$estimate, 2))) +
  annotate("text", x = 4, y = 16, color = "#56B4E9",
           label = paste0("p-value: ", round(offsite_offers_nonpeer_cor_coefs$p.value, 7)))+  
  labs(y = "Number of off-site interviews", x = "Number of faculty offers",
       fill = "PEER identity:", color = "PEER identity:")+
  my_theme_horiz

ggsave(paste0("mollet_socialsci/figures/fig8d_", Sys.Date(), ".jpeg"))

#E.----
onsite_offers_peer_cor_coefs <- cor.test(as.numeric(fig7_data_peer$faculty_offers), 
                                       as.numeric(fig7_data_peer$on_site_interviews))

onsite_offers_nonpeer_cor_coefs <- cor.test(as.numeric(fig7_data_nonpeer$faculty_offers), 
                                          as.numeric(fig7_data_nonpeer$on_site_interviews))


fig8e_plot_leg <- ggplot(data = fig7_data, aes(x = as.numeric(faculty_offers), 
                             y = as.numeric(on_site_interviews))) +
  geom_point(aes(fill = peer, color = peer)) +
  scale_fill_manual(labels = peer_breaks, values = peer_color)+
  scale_color_manual(labels = peer_breaks, values = peer_color)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  geom_smooth(method=lm, se=TRUE, color="black", fill="#999999")+
  geom_smooth(aes(fill = peer, color = peer), method=lm, se=TRUE)+
  annotate("text", x = 4.5, y = 7.5, 
           label = paste0("R: ", round(onsite_offers_cor_coefs$estimate, 2))) +
  annotate("text", x = 4.5, y = 7.2, 
           label = paste0("p-value: ", round(onsite_offers_cor_coefs$p.value, 25)))+
  #peer data
  annotate("text", x = 4.5, y = 2.5, color = "#E69F00",
           label = paste0("R: ", round(onsite_offers_peer_cor_coefs$estimate, 2))) +
  annotate("text", x = 4.5, y = 2.20, color = "#E69F00",
           label = paste0("p-value: ", round(onsite_offers_peer_cor_coefs$p.value, 10)))+
  #nonpeer data
  annotate("text", x = 2.5, y = 5.5, color = "#56B4E9",
           label = paste0("R: ", round(onsite_offers_nonpeer_cor_coefs$estimate, 2))) +
  annotate("text", x = 2.5, y = 5.2, color = "#56B4E9",
           label = paste0("p-value: ", round(onsite_offers_nonpeer_cor_coefs$p.value, 20)))+  
  labs(y = "Number of on-site interviews", x = "Number of faculty offers",
       fill = "PEER identity:", color = "PEER identity:")+
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
                   label_size = 18, nrow = 1)

fig8de <- plot_grid(fig8d_plot, fig8e_plot, 
                    labels = c('D', 'E'),
                    label_size = 18, nrow = 1)

Fig8 <- plot_grid(fig8ab, fig8c, fig8de,
                  ncol = 1)

ggsave(filename = paste0("Figure_8_", Sys.Date(), ".png"), 
       device = 'png', units = "in", scale = 1.75,
       path = 'mollet_socialsci/figures/', 
       width = 8, height = 8)