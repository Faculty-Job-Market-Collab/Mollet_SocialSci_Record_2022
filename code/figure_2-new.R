# Job Application benchmarks and their impact on success

# Fig 4A Subgroup Medians (application, Remote Interview, on-site interview, offer)
# All; Men & Women; URM & Non-URM; 1+ offer & no offer

fig4_tidy_data %>% 
  filter(question != "apps_submitted_binned") %>% 
  ggplot(aes(x = simple_gender, fill = simple_gender, 
             y = as.numeric(response)))+
  geom_boxplot(position = "dodge")+
  facet_wrap(~ question, scales = "free_x")+
  coord_flip()+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = cbPalette)+
  labs(y = "Number", x = "Gender")+
  my_theme_horiz

ggsave("social_science/figures/fig4_gender_outcome_boxplots.jpeg")


fig4_tidy_data %>% 
  filter(question != "apps_submitted_binned") %>% 
  ggplot(aes(x = peer, fill = peer, y = as.numeric(response)))+
  geom_boxplot(position = "dodge")+
  facet_wrap(~ question, scales = "free_x")+
  coord_flip()+
  scale_fill_manual(values = cbPalette)+
  scale_y_continuous(expand = c(0,0))+
  #scale_x_discrete(expand = c(0,0))+
  labs(y = "Number", x = "PEER identity")+
  my_theme_horiz

ggsave("social_science/figures/fig4_peer_outcome_boxplots.jpeg")
