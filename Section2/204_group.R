##################################
## Set seed 
##################################
set.seed(40)


d_width <- 116.9
d_height <- 82.7

my_data <- data_frame(
  Visit = c(1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8),
  group = c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2),
  Outcome = 6 + rnorm(16) + 0.09 * group
)

summary(my_data)

f3a <- my_data %>%
  ggplot(aes(x = Visit, y = Outcome, shape = factor(group))) + 
  geom_point(size = 5, alpha = 0.9, colour = "grey69") + 
  paper_theme() + 
  scale_x_continuous(limits = c(0.5, 8.5)) +  
  scale_y_continuous(limits = c(4.5, 8)) +  
  theme(panel.border=element_rect(color="grey", size = 0.2)) +
  theme(axis.title=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(panel.grid = element_line(color="grey", size = 0.15))

ggsave(f3a, file=paste(fig_path, "204_a.png"), width = d_width, height = d_height, units = "mm", dpi = d_dpi)


ggsave(f3a, file=paste(fig_path, "204_a.pdf"), 
       width = d_width, 
       height = d_height, 
       units = "mm", 
       dpi = 1000, 
       device = cairo_pdf)


f3b <- my_data %>%
  ggplot(aes(x = Visit, y = Outcome, shape = factor(group), colour = factor(group))) + 
  geom_point(size = 5, alpha = 0.9) + 
  scale_colour_manual(values=c("#CC0000", "grey69")) +
  paper_theme() + 
  scale_x_continuous(limits = c(0.5, 8.5)) +  
  scale_y_continuous(limits = c(4.5, 8)) +  
  theme(panel.border=element_rect(color="grey", size = 0.2)) +
  theme(axis.title=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(panel.grid = element_line(color="grey", size = 0.15))



ggsave(f3b, file=paste(fig_path, "204_b.pdf"), 
       width = d_width, 
       height = d_height, 
       units = "mm", 
       dpi = 1000, 
       device = cairo_pdf)


ggsave(f3b, file=paste(fig_path, "204_b.png"), width = d_width, height = d_height, units = "mm", dpi = d_dpi)


f3c <- my_data %>%
  ggplot(aes(x = Visit, y = Outcome, shape = factor(group), colour = factor(group))) + 
  geom_point(size = 5, alpha = 0.9) + 
  scale_colour_manual(values=c("#CC0000", "grey69")) +
  geom_line(size = 0.6, alpha = 0.6) +
  paper_theme() + 
  scale_x_continuous(limits = c(0.5, 8.5)) +  
  scale_y_continuous(limits = c(4.5, 8)) +  
  theme(panel.border=element_rect(color="grey", size = 0.2)) +
  theme(axis.title=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(panel.grid = element_line(color="grey", size = 0.15))


ggsave(f3c, file=paste(fig_path, "204_c.png"), width = d_width, height = d_height, units = "mm", dpi = d_dpi)

ggsave(f3c, file=paste(fig_path, "204_c.pdf"), 
       width = d_width, 
       height = d_height, 
       units = "mm", 
       dpi = 1000, 
       device = cairo_pdf)



f3 <- grid.arrange(f3a, f3b, f3c, ncol = 3)


ggsave(f3, file=paste(fig_path, "204.pdf"), 
       width = 178, 
       height = 40, 
       units = "mm", 
       dpi = 1000, 
       device = cairo_pdf)

