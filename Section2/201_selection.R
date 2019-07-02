####################################################################
# Create a set of figures based on the same data. 
# Illustrate the selection of the graph type is important 
# for effective comparison of the data.
# 
# Some examples are generated using excel. 
# See corresponding spreadsheet.
####################################################################

#######################################
##  Generate data
#######################################
my_data <- data.frame(
  Group = c("A", "B", "C", "D", "E", "F"),
  perc = c(0.13, 0.24, 0.11, 0.26, 0.12, 0.14))


#######################################
##  Plot ordered dot plot
#######################################
ggplot(my_data, aes(x = perc, y = reorder(Group, perc))) +
  geom_point(size = 4, colour = "cornflowerblue") +
  scale_x_continuous(breaks = seq(0, 1, 0.05),limits = c(0.08, 0.30)) +
  paper_theme() +
  theme(panel.grid.major.y = element_line(size = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_text(size=12)
  )

ggsave(file=paste(fig_path, "201_a.png"), width = d_width, height = d_height, units = "mm", dpi = 1000)
ggsave(file=paste(fig_path, "201_a.pdf"), 
       width = d_width, height = d_height, units = "mm", 
       dpi = 1000, device = cairo_pdf)



