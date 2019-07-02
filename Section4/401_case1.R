##############################################################################
## Case example 1 
##############################################################################

page_width <- 178
page_height <- 234

my_data <- read_csv('data/401_case1_PKPDdataset_ard.csv')

##############################################################################
## Scatter plot with default ggplot theme 
##############################################################################
my_data %>%
  filter(CYCLE == 1) %>%
  ggplot(aes(x = AUC, y = sCHG)) + 
  geom_point() + 
  scale_y_continuous(breaks = seq(-800,800,200)) +  
  theme_gray(base_size = 10) +
  labs(x="RESN", y="LIDV", title="" 
       #caption="RESN = AUC0-24h (h*ug/mL); LIDV = FEV1 change from baseline (mL)"
       )

ggsave(file=paste0(fig_path, "401_a.png"), 
       width = 0.95*page_width, height = 0.3*page_height, 
       units = "mm", dpi = d_dpi)

##############################################################################
## Improved scatterplot with better theme,  
## scales, axis labels, and including smooth fit
##############################################################################
lbr <- scales::trans_breaks("log10", function(x) 10^x)
llb <- scales::trans_format("log10", scales::math_format(10^.x))
#theme_set(theme_linedraw(base_size=14))

my_data %>%
  filter(CYCLE == 1) %>%
  ggplot(aes(x = AUC, y = sCHG)) + 
  geom_point(alpha = 0.7) + 
  geom_smooth(method="loess", colour = "red") +
  scale_x_log10(breaks=lbr, labels=llb) + 
  scale_y_continuous(breaks = seq(-800,800,200)) +
  annotation_logticks(sides= "b") +
  labs(x=expression(paste("AUC0-24h (h*",mu,"g/mL)", sep="")),
       y="FEV1 change from baseline (mL)", 
       title="Exposure is positively associated with response",
       subtitle="Loess smoother (95% CI)") +
  paper_theme() +
  theme(panel.border=element_rect(color="grey", fill = NA, size=0.25),
        axis.text.x=element_text(size=9),
        plot.title = element_text(vjust = -1),
        plot.subtitle = element_text(size=9, color = rgb(0.3,0.3,0.3)),
        plot.caption = element_text(size=9, hjust = 1, color = rgb(0.5,0.5,0.5), vjust = 5),
        plot.margin=unit(c(1,4,1.5,1),"mm")) 

ggsave(file=paste0(fig_path, "401_b.png"), 
       width = 0.95*page_width, height = 0.3*page_height, 
       units = "mm", dpi = d_dpi)

##############################################################################
## Scatter plot colored by dose group,
## including linear fits within dose group
##############################################################################

my_data %>%
  filter(CYCLE == 1) %>%
  ggplot(aes(x = AUC, y = sCHG, colour=factor(DOSE))) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method="lm", se=FALSE) + 
  scale_colour_brewer(palette="Set2" , name="Dose (mg)") + 
  scale_x_log10(breaks=lbr, labels=llb) + 
  scale_y_continuous(breaks = seq(-800,800,200)) + 
  annotation_logticks(sides= "b") +
  labs(x=expression(paste("AUC0-24h (h*",mu,"g/mL)", sep="")), 
       y="FEV1 change from baseline (mL)", 
       title="Exposure is not a better predictor of response than dose") + 
  #theme_linedraw(base_size=12)
  paper_theme() +
  theme(panel.border=element_rect(color="grey", fill = NA, size=0.25),
        axis.text.x=element_text(size=9),
        legend.position = c("right"),
        legend.title = element_text(size = 10),
        plot.caption = element_text(size=9, hjust = 0, color = rgb(0.5,0.5,0.5)),
        plot.margin=unit(c(1,4,1.5,1),"mm")) 


ggsave(file=paste0(fig_path, "401_c.png"), 
       width = 0.95*page_width, height = 0.3*page_height, 
       units = "mm", dpi = d_dpi)


##############################################################################
## 
##############################################################################
## Create factor levels for facet labels
# my_data$DOSEL <- factor(paste0(my_data$DOSE, " mg"), levels = c("3 mg", "10 mg",
#                                                  "30 mg", "100 mg",
#                                                  "300 mg"))
# my_data %>%
#   filter (CYCLE == 1) %>%
#   ggplot(aes(x = AUC, y = sCHG)) +
#   theme_minimal(base_size=12) +
#   theme(panel.border=element_rect(color="grey", fill = NA),
#         panel.spacing = unit(1, "lines"),
#         axis.text.x=element_text(size=9),
#         plot.margin=unit(c(1,4,1.5,1),"mm")) +
#   geom_point(data = transform(my_data %>% filter (CYCLE == 1), DOSEL = NULL), colour = "grey80", alpha = 0.6) +
#   geom_point(color = "black", alpha = 0.6) +
#   geom_smooth(method="lm", se=FALSE, colour = "red") +
#   scale_colour_brewer(palette="Set2" , name="Dose (mg)") +
#   scale_x_log10(breaks=lbr, labels=llb) +
#   annotation_logticks(sides= "b") +
#   labs(x=expression(paste("AUC0-24h (h*",mu,"g/mL)", sep="")),
#        y="FEV1 change from baseline (mL)",
#        title="No effect of concentration on response within levels of dose") +
#   facet_wrap(.~DOSEL, ncol = 2, strip.position = "top")
# 
# 
# ggsave(file=paste0(fig_path, "401_d.png"), 
#        width = 200, height = 180, 
#        units = "mm", dpi = d_dpi)

## Boxplot by dose group
gg1 <- my_data %>%
  filter(CYCLE == 1) %>%
  ggplot(aes(x = factor(DOSE), y = sCHG)) +
  geom_boxplot(aes(group=factor(DOSE)))+
  geom_jitter(alpha = 0.5, height = 0, width = 0.1) +
  geom_smooth(aes(x = as.numeric(factor(DOSE)), y = sCHG)) +
  scale_colour_brewer(palette="Set2" , name="Dose (mg)") +
  labs(x=expression(paste("Dose (mg)", sep="")),
       y="FEV1 change from baseline (mL)") +
  scale_y_continuous(breaks = seq(-800,800,200)) +
  paper_theme()  +
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        panel.border=element_rect(color="grey", fill = NA, size=0.25),
        axis.text.x=element_text(size=7),
        axis.text.y=element_text(size=9),
        legend.position = c("right"),
        legend.title = element_text(size = 10),
        plot.caption = element_text(size=9, hjust = 0, color = rgb(0.5,0.5,0.5)),
        plot.margin=unit(c(1,4,1.5,1),"mm"))

## Boxplot by AUC quantile
my_data <- my_data %>%
  mutate(AUC_bin = cut(AUC, quantile(my_data$AUC,seq(0,1,0.2)), include.lowest = TRUE ) ) %>% 
  group_by(AUC_bin) %>%
  mutate(AUC_bin_med = median(AUC)) %>%
  ungroup() 

gg2 <- my_data %>%
  filter(CYCLE == 1) %>%
  ggplot(aes(x = AUC_bin, y = sCHG)) +
  geom_boxplot(aes(group = AUC_bin))+
  geom_jitter(alpha = 0.5, height = 0, width = 0.1) +
  geom_smooth(aes(x = as.numeric(AUC_bin), y = sCHG)) +
  labs(x=expression(paste("Quantiles of AUC0-24h (h*",mu,"g/mL)", sep="")) ,
       y="FEV1 change from baseline (mL)") + 
  scale_colour_brewer(palette="Set2" , name=expression(paste("AUC0-24h (h*",mu,"g/mL)", sep=""))) +
  scale_y_continuous(breaks = seq(-800,800,200)) +
  paper_theme()  +
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        panel.border=element_rect(color="grey", fill = NA, size=0.25),
        axis.text.x=element_text(size=7),
        axis.text.y=element_text(size=9),
        legend.position = c("right"),
        legend.title = element_text(size = 10),
        plot.caption = element_text(size=9, hjust = 0, color = rgb(0.5,0.5,0.5)),
        plot.margin=unit(c(1,4,1.5,1),"mm"))


gg3 <- grid.arrange(textGrob("Exposure is not a better predictor of response than dose", gp=gpar(fontsize=14), hjust = 0.6),
            arrangeGrob(gg1, gg2, nrow = 1), 
            layout_matrix = rbind(c(1), c(2), c(2), c(2), c(2), c(2), c(2), c(2), c(2)))

ggsave(file=paste0(fig_path, "401_d.png"), plot = gg3,
       width = 0.95*page_width, height = 0.3*page_height, 
       units = "mm", dpi = d_dpi)
