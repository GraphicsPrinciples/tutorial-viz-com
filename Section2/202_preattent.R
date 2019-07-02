my_data <- data.frame(
  Group = c("A", "B", "C", "D","E", "F"),
  perc = c(0.02, 0.05, 0.1, 0.2, 0.25, 0.38),
  perc2 = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
  colour1 = c(1, 1, 0, 0, 0, 0),
  colour2 = c(0, 0, 0,0, 1, 1))

f2c <- my_data %>%
  ggplot(aes(x = Group, y = perc2)) + 
  theme_minimal(base_size=18) +
  geom_bar(width=0.7, stat = "identity", 
           fill = "white", colour = "grey", alpha = 0.8) +
  geom_bar(aes(x = Group, y = perc), 
           fill= "grey69", width=0.7, stat = "identity", alpha = 0.8) +
  scale_y_continuous(breaks=c(0, 5, 10)) + 
 # geom_hline(yintercept = 0, colour = "wheat4", linetype=1, size=1) + 
  coord_flip() +
  theme(legend.position="none",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank()
  )


ggsave(f2c, file=paste(fig_path, "202_a.png"), width = d_width, height = d_height, units = "mm", dpi = d_dpi)


f2d <- my_data %>% 
  ggplot(aes(x = Group, y = perc2)) + 
  theme_minimal(base_size=18) +
  geom_bar(width=0.7, stat = "identity", 
           fill = "white", colour = "grey", alpha = 0.6) +
  scale_y_continuous(breaks=c(0, 5, 10)) + 
  geom_bar(aes(x = Group, y = perc, fill = factor(colour1)), 
           width=0.7, stat = "identity", alpha = 0.8) +  
  #scale_fill_brewer() +
  scale_fill_manual(values=c("grey69", "#CC0000")) +
#  geom_hline(yintercept = 0, colour = "wheat4", linetype=1, size=1)+ 
  coord_flip() +
  theme(legend.position="none",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank()
  )

ggsave(f2d, file=paste(fig_path, "202_b.png"), width = d_width, height = d_height, units = "mm", dpi = d_dpi)



