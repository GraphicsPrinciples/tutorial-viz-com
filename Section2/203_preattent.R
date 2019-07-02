set.seed(19)

my_data <- data_frame(
  group = c(rep(1,64)),
  x = rnorm(64) ,
  y = rnorm(64) 
)
my_data$group[19] <- 0

f2a <- my_data %>%
  ggplot(aes(x = x, y = y, shape = factor(group))) + 
  geom_point(size = 4, colour = "grey69", alpha = 0.9) + 
  scale_shape_manual(values = c(15, 19)) +
  scale_colour_manual(values=c("#CC0000", "grey69")) +
  paper_theme() +
  theme(panel.border=element_rect(color="grey", size = 0.15)) +
  theme(axis.title=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(panel.grid = element_line(color="lightgrey", size = 0.01))


f2a

ggsave(f2a, file=paste(fig_path, "203_a.png"), 
       width = d_width, 
       height = d_height, 
       units = "mm", 
       dpi = d_dpi)

ggsave(f2a, file=paste(fig_path, "203_a.pdf"), 
       width = d_width, 
       height = d_height, 
       units = "mm", 
       dpi = d_dpi, 
       device = cairo_pdf)

f2b <- my_data %>%
  ggplot(aes(x = x, y = y, shape = factor(group), colour = factor(group), fill = factor(group))) + 
  geom_point(size = 4, alpha = 0.9) + 
  geom_point(data=subset(my_data, group==0), size = 4, alpha = 0.9) +
  scale_shape_manual(values = c(19, 19)) +
  scale_colour_manual(values=c("#CC0000", "grey69")) +
  paper_theme() +
  theme(panel.border=element_rect(color="grey", size = 0.15)) +
  theme(axis.title=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(panel.grid = element_line(color="lightgrey", size = 0.01))


ggsave(f2b, file=paste(fig_path, "203_c.png"), 
       width = d_width, 
       height = d_height, 
       units = "mm", 
       dpi = d_dpi)

ggsave(f2b, file=paste(fig_path, "203_c.pdf"), 
       width = d_width, 
       height = d_height, 
       units = "mm", 
       dpi = d_dpi, 
       device = cairo_pdf)

f2 <- grid.arrange(f2a, f2b, f2c, f2d, ncol = 2, nrow = 2)

ggsave(f2, file=paste(fig_path, "203.pdf"), 
       width = 178, 
       height = 178, 
       units = "mm", 
       dpi = 1000, 
       device = cairo_pdf)

