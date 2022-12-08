#Fig 3 Target institution type (by gender)
group_count <- fig1_inst_data %>% 
  count(question, simple_gender) %>% 
  rename("total" = n)

fig3_plot <- fig1_inst_data %>% count(question, simple_gender, response) %>% 
  left_join(., group_count, by = c("question", "simple_gender")) %>% 
  mutate(percent = get_percent(n, total)) %>% 
  mutate(question = if_else(question == "R1_apps_submitted_binned", 
                            "To RI institutions", "To PU institutions")) %>%
  ggplot(aes(x=factor(response, bin_levels_small), y = percent,
             fill = simple_gender))+
  geom_col(position = "dodge")+
  scale_fill_manual(breaks = gender_simple_breaks, values = gender_color)+
  coord_flip()+
  facet_wrap(~question)+
  scale_y_continuous(expand = c(0,0))+
  labs(y="Percent of responses by gender", 
       x="Number of applications submitted",
       fill = "Gender*", caption = "*GNC=gender non-conforming")+
  base_theme+
  theme(strip.background = element_rect(linetype = "blank"))

ggsave(filename = paste0("Figure_3_", Sys.Date(), ".png"), 
       device = 'png', units = "in", scale = 1.75,
       path = 'mollet_socialsci/figures/', width = 8, height = 6)

gender_plot_leg <- get_legend_plot(fig3_plot)

ggsave("mollet_socialsci/figures/gender_legend.jpeg")