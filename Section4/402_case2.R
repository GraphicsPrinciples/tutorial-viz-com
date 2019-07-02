##############################################################################
## Case example 2 
##############################################################################

page_width <- 178
page_height <- 234

my_data <- read_csv("data/402_case2_PKdataset.csv")


# Define order for factors
my_data$TRTACT <- factor(my_data$TRTACT, levels = unique(my_data$TRTACT[order(my_data$DOSE)]))
my_data$ETHN <- factor(my_data$ETHN)
my_data$ETHN <- factor(my_data$ETHN, levels = rev(levels(my_data$ETHN)))

##############################################################################
## Mean (SD) error bars, linear scale
##############################################################################

data_to_plot <- my_data[my_data$CMT==2&my_data$DOSE==300&my_data$PART==1&my_data$STUDY==1,]
data_to_plot$TRTACT <- factor(data_to_plot$TRTACT, levels = rev(levels(data_to_plot$TRTACT)))

data_to_plot %>%
  ggplot(aes(x = NOMTIME, y = LIDV, group= interaction(ETHN,CYCLE))) + 
  theme_bw(base_size = 8) + 
  stat_summary(geom = "errorbar", width = 2, 
               fun.data = function(y){
                          y <- stats::na.omit(y)
                          data.frame(
                            y = mean(y),
                            ymin = mean(y)-sqrt(stats::var(y)), 
                            ymax = mean(y)+sqrt(stats::var(y)))}) + 
  stat_summary(geom = "line", size = 0.5, fun.y = mean) + 
  stat_summary(geom = "point", size = 1.5, fun.y = mean, aes(fill = ETHN), stroke = 0.5, shape= 21) + 
  scale_fill_manual(values = c("white", "black")) + 
  scale_x_continuous(breaks =c(0,4,8,12,24,36,48,72)) + 
  xlab("Time (hours)") + ylab("Concentration (ng/mL)\nMean (SD)") + 
  theme(legend.title = element_blank(), 
        legend.position = "bottom",
        legend.box.spacing = unit(0, "mm")) 

ggsave(file=paste0(fig_path, "402_a.png"), 
       width = 0.5*page_width, height = 0.3*page_height, 
       units = "mm", dpi = d_dpi)



##############################################################################
## Mean (95% CI) error bars, log scale
##############################################################################
data_to_plot <- my_data[my_data$CMT==2&my_data$DOSE==300&my_data$PART==1&my_data$STUDY==1,]
data_to_plot$TRTACT <- factor(data_to_plot$TRTACT, levels = rev(levels(data_to_plot$TRTACT)))

data_to_plot %>%
  ggplot(aes(x = NOMTIME, y = LIDV, group= interaction(ETHN,CYCLE))) + 
  theme_bw(base_size = 8) + 
  stat_summary(geom = "errorbar", width = 0, 
                        fun.data = function(y){
                          y <- stats::na.omit(y)
                          data.frame(
                            y = mean(y),
                            ymin = mean(y)-qt(0.975,length(y))*sqrt(stats::var(y)/length(y)), 
                            ymax = mean(y)+qt(0.975,length(y))*sqrt(stats::var(y)/length(y)))
                        }) + 
  stat_summary(geom = "line", size = 0.5, fun.y = mean) +
  stat_summary(geom = "point", size = 1.5, fun.y = mean, aes(fill = ETHN), stroke = 0.5, shape= 21) +
  scale_fill_manual(values = c("white", "black")) + 
  scale_x_continuous(breaks =c(0,4,8,12,24,36,48,72)) + 
  xlab("Time (hours)") + ylab("Concentration (ng/mL)\nMean (95% CI)") + 
  guides(color= guide_legend(title="Dose")) + 
  theme(legend.title = element_blank(), legend.position = "bottom",
        legend.box.spacing = unit(0, "mm")) +
  scale_y_log10() + 
  annotation_logticks(base = 10, sides = "l", color = rgb(0.5,0.5,0.5))

ggsave(file=paste0(fig_path, "402_b.png"), 
       width = 0.5*page_width, height = 0.3*page_height, 
       units = "mm", dpi = d_dpi)


##############################################################################
## Cmax, ctrough, AUClast dots (95% CI) in separate panels
##############################################################################

Cmax <- my_data %>% 
  subset(CMT==2&!is.na(LIDV)&DOSE==300&PART==1&STUDY==1) %>%
  group_by(ID,ETHN) %>%
  summarize(Cmax = max(LIDV))

gg1 <- Cmax %>% 
  ggplot(aes(x = ETHN, y = Cmax)) + 
  theme_bw(base_size = 10) + 
  stat_summary(aes(group = ETHN), geom = "errorbar", width = 0, 
               fun.data = function(y){
                 data.frame(y = mean(y), 
                            ymin = mean(y) - qt(0.0975,length(y))*sqrt(stats::var(y)/length(y)) ,
                            ymax = mean(y) + qt(0.0975,length(y))*sqrt(stats::var(y)/length(y))) }) + 
  stat_summary(aes(group = ETHN), shape = 21, fill = "white", size = 0.2) + 
  ggtitle("Cmax (ng/mL)") + xlab("") +ylab("") + labs(subtitle = "Mean (95% CI)") + 
  scale_y_log10(breaks = c(0.3,1,3,10,30,100,300,1000,3000)) + 
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        plot.subtitle = element_text(color = rgb(0.3,0.3,0.3))) +
  annotation_logticks(base = 10, sides = "l", color = rgb(0.5,0.5,0.5)) + 
  coord_cartesian(ylim=c(60,300))

Ctrough <- my_data %>% 
  subset(CMT==2&!is.na(LIDV)&DOSE==300&PART==1&STUDY==1) %>%
  group_by(ID,ETHN) %>%
  summarize(Ctrough = min(LIDV))

gg2 <- gg1 %+% Ctrough %+% aes(y = Ctrough) +
  ggtitle("Ctrough (ng/mL)")  + xlab("") +ylab("") + labs(subtitle = "Mean (95% CI)")+
  coord_cartesian(ylim=c(0.6,9))

AUClast <- my_data[my_data$CMT==2&!is.na(my_data$LIDV)&my_data$DOSE==300&my_data$PART==1&my_data$STUDY==1,]
AUClast <- data.frame(stack(sapply(split(AUClast,AUClast$ID),function(df) trapz(df$TIME,df$LIDV))))
names(AUClast) <- c("AUC","ID")

AUClast$ID <- as.numeric(as.character(AUClast$ID))
AUClast <- AUClast[order(AUClast$ID),]
AUClast <- merge(AUClast, unique(my_data[c("ID","ETHN")]), by = "ID")


gg3 <- gg1 %+% AUClast %+% aes(x = ETHN, y = AUC) + 
  ggtitle("AUClast (h.(ng/mL))") + xlab("") +ylab("")  + labs(subtitle = "Mean (95% CI)") + 
  scale_y_log10(breaks = c(0.5,1,5,10,50,100,500,1000,5000))+ 
  coord_cartesian(ylim=c(500,1500))


gg4 <- grid.arrange(arrangeGrob(gg1,gg2,gg3,nrow=1), nrow = 1)

ggsave(gg4, file=paste0(fig_path, "402_c.png"), 
       width = 0.9*page_width, height = 0.3*page_height,  
       units = "mm", dpi = d_dpi)


##############################################################################
## Forest plot of Ratios of Japanese:Caucasian for Cmax, Ctrough, AUClast
##############################################################################
Cmax2  <- Cmax %>% 
  mutate(DV = Cmax, logDV = log(Cmax), LABEL = "Cmax") %>% 
  select(c("ID","ETHN","DV","logDV","LABEL"))

glimpse(Cmax2)

results <- t.test(logDV ~ ETHN, Cmax2)

PKmetrics <- data.frame( y2.5 = exp(results$conf.int[1]), 
                         y97.5 = exp(results$conf.int[2]), 
                         ymean = exp(as.numeric(results$estimate[1] - results$estimate[2])), 
                         var = "Cmax",
                         unit = "ng/mL")

Ctrough2 <- Ctrough %>% 
  mutate(DV = Ctrough, logDV = log(Ctrough), LABEL = "Ctrough") %>% 
  select(c("ID","ETHN","DV","logDV","LABEL"))

results <- t.test(logDV ~ ETHN, Ctrough2)

PKmetrics <- rbind(PKmetrics, 
                   data.frame( y2.5 = exp(results$conf.int[1]), 
                               y97.5 = exp(results$conf.int[2]), 
                               ymean = exp(as.numeric(results$estimate[1] - results$estimate[2])), 
                               var = "Ctrough",
                               unit = "ng/mL"))

AUClast2 <- AUClast %>% 
  mutate(DV = AUC, logDV = log(AUC), LABEL = "AUClast") %>% 
  select(c("ID","ETHN","DV","logDV","LABEL"))

results <- t.test(logDV ~ ETHN, AUClast2)

PKmetrics <- rbind(PKmetrics, 
                   data.frame( y2.5 = exp(results$conf.int[1]), 
                               y97.5 = exp(results$conf.int[2]), 
                               ymean = exp(as.numeric(results$estimate[1] - results$estimate[2])), 
                               var = "AUClast",
                               unit = "h.ng/mL"))

PKmetrics$var <- factor(PKmetrics$var, levels = c("AUClast","Ctrough","Cmax"))

PKmetrics %>%
  ggplot(aes(x = var, y = ymean, ymin = y2.5, ymax = y97.5)) + 
  paper_theme() +
  geom_hline(yintercept = 1, size = 1, colour = "red", alpha = 0.1) + 
  scale_y_log10(breaks = c(0.25,0.5,1,2,4)) + 
  geom_point() + 
  geom_errorbar(width = 0) + 
  labs(x = "", 
       y = "Ratios between Japanese and Caucasian Mean (95% CI)", 
       title = "Exposure differs by ethnicity") +
  scale_x_discrete(breaks = NULL, labels = NULL) + 
  geom_text(aes(x = var, y = 0.175, label = paste0(var,"\n(",unit,")" ) )) +
  coord_flip(ylim=c(0.15,5)) +
  theme(
    panel.grid.major.x = element_line(color = "gray", size = 0.2), 
    panel.grid.major.y = element_blank(), 
    panel.border = element_blank(),
    axis.title.x=element_text(size=10, hjust=0.5),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 12, hjust = 0.5)
  ) 

ggsave(file=paste0(fig_path, "402_d.png"), 
       width = 1*page_width, height = 0.3*page_height,
       units = "mm", dpi = d_dpi)


