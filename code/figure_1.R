#Figure 1. Demographics
#A. Application cycles---- 
# Compare social sciences and humanities (Two-tailed Wilcoxon rank sum test)
fig1a_wilcox <- wilcox.test(num_application_cycles ~ research_category, 
                            data = fig1a_data,
                            na.rm=TRUE, paired=FALSE, 
                            exact=FALSE, conf.int=TRUE)

save_wilcox(fig1a_wilcox, 
            file = paste0("mollet_socialsci/figures/fig1a_mwu_test_",
                          Sys.Date(), ".csv"))

fig1a_plot_leg <- fig1a_data %>% 
  get_plot_summary(., "research_category", 
                   "application_cycles", binary = FALSE) %>% 
  ggplot(aes(x=application_cycles, y = percent,
             fill = research_category))+
  geom_col(position = "dodge")+
  scale_fill_manual(breaks = research_breaks, values = cbPalette)+
  coord_flip()+
  scale_y_continuous(expand = c(0,0))+
  labs(y="Percent Respondents", 
       x="Number of application cycles\n",
       fill = "Field",
       subtitle = paste("Mann-Whitney U: p<0.01"))+
  base_theme

fig1a_plot <- fig1a_plot_leg + my_theme

ggsave(filename = paste0("mollet_socialsci/figures/fig1a_",
                          Sys.Date(), ".jpeg"))

#B. Number of postdoc positions----

fig1b_plot <- fig1b_data %>% 
  get_plot_summary(., "research_category", 
                   "number_postdocs", binary = FALSE) %>% 
  mutate(number_postdocs = fct_explicit_na(as.character(number_postdocs),
                               #levels = c("1", "2", "3", ">3"),
                               na_level = "No\nResponse")) %>% 
  #View()
  ggplot(aes(x=factor(number_postdocs, 
                      levels = c("No\nResponse", "1", "2", "3", ">3")), 
             y = percent,
             fill = research_category))+
  geom_col(position = "dodge")+
  scale_fill_manual(breaks = research_breaks, values = cbPalette)+
  coord_flip()+
  scale_y_continuous(expand = c(0,0))+
  labs(y="Percent Respondents", 
       x="Number of postdoctoral\npositions")+
  my_theme_horiz

ggsave(filename = paste0("mollet_socialsci/figures/fig1b_",
                         Sys.Date(), ".jpeg"))

#C. Mean Applicant Metrics ----

metric_medians <- metric_data %>% 
  mutate(question = case_when(
    question == "first_author" ~ "First-author papers",
    question == "peer-reviewed_papers" ~ "Peer-reviewed papers",
    question == "scholar_citations_all" ~ "Google Scholar citations",
    question == "scholar_hindex" ~ "Google Scholar H index"
  )) %>%
  group_by(question, research_category) %>% 
  summarise(med = median(as.numeric(response), na.rm = TRUE),
            max = max(as.numeric(response), na.rm = TRUE))

fig1c_plot <- metric_data %>% 
  mutate(question = case_when(
    question == "first_author" ~ "First-author papers",
    question == "peer-reviewed_papers" ~ "Peer-reviewed papers",
    question == "scholar_citations_all" ~ "Google Scholar citations",
    question == "scholar_hindex" ~ "Google Scholar H index"
  )) %>%
  group_by(question, research_category) %>% 
  ggplot(aes(x = question, y = as.numeric(response), 
             fill = research_category))+
  #geom_dotplot()+
  geom_boxplot(position = "dodge")+
  scale_fill_manual(breaks = research_breaks, values = cbPalette)+
  facet_wrap(~question, scales = "free")+
  geom_text(data = metric_medians, 
            aes(group = research_category, x = question, 
                y = max*0.8, label = paste("med=", med)), 
            position=position_dodge(width=1), size = 5)+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "", y = "Response value per applicant")+
  my_theme_horiz+
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        strip.background = element_rect(linetype = "blank"))

ggsave(filename = paste0("mollet_socialsci/figures/fig1c_",
                         Sys.Date(), ".jpeg"))

#D. teaching experience: ----
fig1d_group_count <- teaching_data %>% 
  count(research_category) %>% 
  rename("total" = n)

fig1d_plot <- teaching_data %>% 
  count(research_category, question, response) %>% 
  left_join(., fig1d_group_count, 
            by = "research_category") %>% 
  mutate(percent = get_percent(n, total),
         question = if_else(question == "teaching_status",
                            "Teaching experience (n=151)", 
                            "Types of teaching experience")) %>%  
  ggplot(aes(x = response, y = percent,
             fill = research_category))+
  geom_col(position = "dodge")+
  scale_fill_manual(breaks = research_breaks, values = cbPalette)+
  facet_wrap(~question, scale = "free_y", ncol = 1)+
  coord_flip()+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "", 
       y = "Percent Respondents\n(by research field)")+
  my_theme_horiz+
  theme(strip.background = element_rect(linetype = "blank"))


ggsave(filename = paste0("mollet_socialsci/figures/fig1d_",
                         Sys.Date(), ".jpeg"))

#generate plot----
fig1_leg <- get_legend_plot(fig1a_plot_leg)

ggsave("mollet_socialsci/figures/fig1_legend.jpeg")

Fig1ab <- plot_grid(fig1a_plot, fig1b_plot, fig1_leg,
                    labels = c('A', 'B', ''),
                    rel_widths = c(1, 1, .5),
                    label_size = 18, nrow = 1)


Fig1cd <- plot_grid(fig1c_plot, fig1d_plot,
                    labels = c('C', 'D'),
                    label_size = 18, nrow = 1)

Fig1 <- plot_grid(Fig1ab, Fig1cd, nrow = 2,
                  rel_heights = c(.8, 1))


ggsave(filename = paste0("Figure_1_", Sys.Date(), ".png"), 
       device = 'png', units = "in", scale = 1.75,
       path = 'mollet_socialsci/figures/', width = 8, height = 6)
