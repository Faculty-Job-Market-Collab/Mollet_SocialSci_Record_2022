#Fig 3 Target institution type (by gender)
fig1_inst_data %>% 
  get_plot_summary(., "question", "simple_gender", binary = FALSE) %>%
  left_join(fig1_inst_data, ., by = c("question", "simple_gender")) %>% View()
  mutate(question = if_else(question == "R1_apps_submitted_binned", 
                            "To RI institutions", "To PU institutions")) %>%
  ggplot(aes(x=factor(response, bin_levels_small), 
             fill = simple_gender))+
  geom_bar(position = "dodge")+
  scale_fill_manual(breaks = gender_simple_breaks, values = gender_color)+
  coord_flip()+
  facet_wrap(~question)+
  scale_y_continuous(expand = c(0,0))+
  labs(y="Number of responses", x="Number of applications submitted",
       fill = "Gender*", caption = "*GNC=gender non-conforming")+
  base_theme+
  theme(strip.background = element_rect(linetype = "blank"))

ggsave(filename = paste0("Figure_3_", Sys.Date(), ".png"), 
       device = 'png', units = "in", scale = 1.75,
       path = 'mollet_socialsci/figures/', width = 8, height = 6)
