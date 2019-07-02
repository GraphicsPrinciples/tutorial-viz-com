##############################################################################
## Case example 4 
##############################################################################

page_width <- 178
page_height <- 234



# Read in data and define order for factors.
# Dervie visit time in weeks from hours
# Define informative label for plotting subgroups
my_data <- read.csv("data/404_case4_PKPDdataset.csv") %>%
  filter(CMT == 3 & DOSE %in% c(0, 150) & PART == 1 & STUDY == 1) %>%
  mutate(
    TRTACT = factor(TRTACT, levels = unique(TRTACT[order(DOSE)])),
    Treatment = factor(TRTACT, levels = levels(TRTACT)),
    Visit = factor(
      plyr::mapvalues(
        round(NOMTIME),
        sort(unique(round(NOMTIME))), 
        c(101,102,103,104,105,106,107,108,199))
    ),
    NOMTIME = NOMTIME/24/7, 
    Biomarker = ifelse(subgroup == 0, "Negative", "Positive")
  )

## Obtain baseline measurement 
base_data <- 
  my_data %>% 
  filter(PROFDAY == 0) %>%
  mutate(BASE = LIDV) %>%
  select(ID, BASE)

## Derive change and percent change from baseline
data_to_plot <- 
  my_data %>%
  filter(PROFDAY != 0) %>%
  left_join(base_data) %>%
  mutate(
    CHG = LIDV - BASE,
    PCHG = 100 * (LIDV - BASE)/LIDV
  ) 


##############################################################################
## Efficacy over time by treatment group
##############################################################################

data_to_plot %>%
  ggplot(aes(x = Visit, 
             y = PCHG, 
             group = Treatment, 
             shape = Treatment, 
             fill = Treatment)
  ) +
  theme_bw(base_size = 12) + 
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar") +
  stat_summary(geom = "line", size = 1, fun.y = mean) + 
  stat_summary(geom = "point", size = 2.5, fun.y = mean, stroke = 1) +
  scale_y_continuous(breaks = seq(-20,20,1)) +
  coord_cartesian(ylim = (c(-10, 5))) +
  labs(x = "Visit", y = "Weight loss (%)") +
  theme(legend.box.spacing = unit(0.01*page_width, "mm"))


ggsave(file=paste0(fig_path, "404_a.png"), 
       width = 0.5*page_width, height = 0.25*page_height, 
       units = "mm", dpi = d_dpi)


ggsave(file=paste0(fig_path, "404_a.pdf"), 
       width = 0.5*page_width, height = 0.25*page_height, units = "mm",  
       dpi = 1000, device = cairo_pdf)





##############################################################################
## Efficacy by subgroup, for treated group only
##############################################################################

data_to_plot %>%
  filter(DOSE > 0) %>%
  ggplot(aes(x = Visit, 
             y = PCHG, 
             group = Biomarker)
  ) +
  theme_bw(base_size = 12) + 
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar") +
  stat_summary(fun.y = mean, geom = "line", 
               aes(linetype = Biomarker), size = 1) + 
  stat_summary(fun.y = mean, geom = "point", size = 2.5, shape = 17) +
  scale_y_continuous(breaks = seq(-20,20,1)) +
  coord_cartesian(ylim = (c(-10, 5))) +
  labs(x = "Visit", 
       y = "Weight loss (%)",
       linetype = "Genetic marker\n(150 mg)") 

ggsave(file=paste0(fig_path, "404_b.png"), 
       width = 0.5*page_width, height = 0.25*page_height,
       units = "mm", dpi = d_dpi)

ggsave(file=paste0(fig_path, "404_b.pdf"), 
       width = 0.5*page_width, height = 0.25*page_height, units = "mm", 
       dpi = 1000, device = cairo_pdf)



##############################################################################
## Efficacy over time by subgroup & treatment
##############################################################################

data_to_plot %>%
  ggplot(aes(x = NOMTIME, 
             y = PCHG, 
             group = interaction(Biomarker,Treatment),
             fill = Treatment
  )
  ) +
  theme_bw(base_size = 12) + 
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0) +
  stat_summary(fun.y = mean, geom = "line", 
               aes(linetype = Biomarker), size = 1) + 
  stat_summary(fun.y = mean, geom = "point", size = 2.5, shape = 21) +
  scale_fill_manual(values = c("white", "black")) +
  scale_y_continuous(breaks = seq(-20,20,1)) +
  scale_x_continuous(breaks = c(0,1,2,3,4,6,8,10,12), minor_breaks = NULL) +
  coord_cartesian(ylim = (c(-10, 5))) +
  labs(x = "Visit", 
       y = "Weight loss (%)\nMean (95% CI)",
       linetype = "Genetic marker") + 
  theme(legend.box.spacing = unit(0.01*page_width, "mm"))


ggsave(file=paste0(fig_path, "404_c.png"), 
       width = 0.5*page_width, height = 0.25*page_height, 
       units = "mm", dpi = d_dpi)

ggsave(file=paste0(fig_path, "404_c.pdf"), 
       width = 0.5*page_width, height = 0.25*page_height, units = "mm",
       dpi = 1000, device = cairo_pdf)



##############################################################################
## Treatment difference by subgroup
##############################################################################


data_to_plot <- data_to_plot %>%
  mutate(
    subgroup = factor(subgroup)
  )

data_to_plot

m2 <- lmer(LIDV ~ BASE + 
             Visit + Treatment + subgroup + 
             BASE*Visit +
             Treatment*Visit +
             subgroup*Treatment +
             subgroup*Visit +
             Treatment*Visit*subgroup + 
             (1 + Visit | ID), 
           data_to_plot,
           REML=FALSE) 

# Comparison by each time-point * subgroup
ests <- emmeans(m2, pairwise ~ Treatment | Visit * subgroup) 

# Obtain visit labels and mapping to nomtime
vis_dat <- data_to_plot %>%
  select(Visit, NOMTIME) %>%
  unique()

# Transform estimates in to data frame
# Derive subgroup label 
# Reverse direction of trt comparison i.e. trt - pbo
# Derive week for displaying subgroups clearly 
# Add visit labels on to data frame
emm1df <- as.data.frame(ests) %>%
  mutate(Visit = contrasts.Visit, 
         subgroup = ifelse(contrasts.subgroup == 0, "Negative", "Positive"),
         y = -1*contrasts.estimate
  ) %>%
  left_join(vis_dat) %>%
  mutate(
    Visit = contrasts.Visit,
    Week = ifelse(subgroup == "Positive", NOMTIME - 0.15, NOMTIME + 0.05 )
  )



## Take out the baseline visit
emm1df %>%
  filter(contrasts.Visit != 101) %>% 
  left_join(vis_dat) %>%
  ggplot(aes(x = NOMTIME, 
             y = y, 
             group = interaction(subgroup, contrasts.contrast))) +
  theme_bw(base_size = 8) + 
  geom_line(size = 1, aes(linetype = subgroup)) + 
  geom_pointrange(aes(ymean = y,
                      ymin = y - 1.96 * contrasts.SE, 
                      ymax = y + 1.96 * contrasts.SE),
                  size = 1) +
  scale_x_continuous(breaks = seq(0, 14 , 1)) +
  scale_y_continuous(breaks = seq(-10, 10 , 1),
                     limits = c(-10, 10)) +  
  labs(x = "Week", 
       y = "Placebo-subtracted weight loss (%)\nMean (95% CI)",
       linetype = "Genetic marker") 



ggsave(file=paste0(fig_path, "404_d.png"), 
       width = 0.5*page_width, height = 0.25*page_height, 
       units = "mm", dpi = d_dpi)


ggsave(file=paste0(fig_path, "404_d.pdf"), 
       width = 0.5*page_width, height = 0.25*page_height, units = "mm",
       dpi = 1000, device = cairo_pdf)



##############################################################################
## Final message graph
##############################################################################

pos <- emm1df %>%
  filter(contrasts.Visit != 101 & subgroup == "Positive")  


emm1df %>%
  filter(contrasts.Visit != 101 & subgroup == "Negative") %>% 
  ggplot(aes(x = Week, 
             y = y, 
             group = contrasts.contrast)) +
  geom_hline(yintercept = 0, size = 0.2, color = "black") +
  geom_line(alpha = 0.1, color = "black") + 
  geom_pointrange(aes(ymean = y,
                      ymin = y - 1.96 * contrasts.SE, 
                      ymax = y + 1.96 * contrasts.SE),
                  alpha = 0.7, color = "black", size = 0.25) +
  geom_line(data = pos, alpha = 0.1, color = "#c0392b") + 
  geom_pointrange(data = pos, 
                  aes(
                    ymean = y,
                    ymin = y - 1.96 * contrasts.SE, 
                    ymax = y + 1.96 * contrasts.SE), 
                  alpha = 0.7, color = "#c0392b", size = 0.25) +
  scale_x_continuous(breaks = c(0, 4, 8, 12)) +
  scale_y_continuous(breaks = c(0,-2.5, -5)) +  
  labs(x = "Week", 
       y = "", 
       title = "Placebo-subtracted weight loss (kg)") +
  paper_theme() +
  theme(plot.title=element_text(size=10, vjust=1.25)) +
  theme(axis.text.x=element_text(size=7)) +
  theme(axis.text.y=element_text(size=7)) +
  theme(axis.title.x=element_text(size=8, vjust=0)) +
  theme(axis.title.y=element_text(size=8, vjust=1.25))





ggsave(file = paste0(fig_path, "404_e.png"), 
       width = 0.65*page_width, height = 0.3*page_height,  
       units = "mm", dpi = 1000)

ggsave(file = paste0(fig_path, "404_e.pdf"), 
       width = 0.68*page_width, height = 0.32*page_height, units = "mm", 
       dpi = 1000, device = cairo_pdf)



