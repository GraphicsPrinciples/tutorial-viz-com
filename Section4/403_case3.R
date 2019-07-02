##############################################################################
## Case example 3 
##############################################################################

page_width <- 178
page_height <- 234

#theme_set(theme_linedraw(base_size=12))
set.seed(111231)
lbr <- scales::trans_breaks("log10", function(x) 10^x)
llb <- scales::trans_format("log10", scales::math_format(10^.x))

n <- 30
r <- 2/3
id <- 1:n

ztext <- factor(c("Placebo", "Active"), levels=c("Placebo","Active"))

dx <- tibble(id = id, 
             x1 = rnorm(n,0,.1),
             x2 = rnorm(n,1,.2),
             z  = c(rep(0,n*(1-r)), rep(1,n*r)),
             a  = rnorm(n, 10, 2),
             bm = -z*0.75,
             b  = rnorm(n, bm, 1))

time <- 0:4
dres <- as_tibble(expand.grid(id=1:n, time=time)) %>% 
  inner_join(dx, by="id") %>%
  mutate(baseline = a + x1 + x2, incrmean=b) %>% 
  arrange(id, z, time)

dres$incr <- with(dres, rnorm(n*5, incrmean, 1))
dres$incr[dres$time==0] <- 0

dres <- dres %>% 
  group_by(id) %>% 
  do({
    ci <- cumsum(.$incr)
    csi <- ci/sqrt(1:length(time))
    y <- .$baseline + csi
    rv <- bind_cols(., data_frame(ci, csi, y))
    rv
    }) %>% 
  ungroup() %>% 
  mutate(cfb = y - baseline)

dres$ztext <- ztext[dres$z+1]

t4dat <- filter(dres, time==4) %>% 
  arrange(cfb) %>% 
  mutate(sort_id=1:n())

##############################################################################
## Waterfall plot of week 4 outcome
##############################################################################

ggplot(t4dat, aes(x=sort_id, y=cfb, fill=factor(ztext))) +
  geom_col(alpha = 0.8) +
  scale_fill_brewer(palette="Dark2", name="Treatment") +
  paper_theme() +
  theme(legend.position=c(0.8,0.2), legend.background=element_blank(),
        legend.title = element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border=element_rect(color="grey", fill = NA)) + 
  labs(x="Subject", y="Change from baseline", 
       title="Week 4 outcome by treatment")

ggsave(filename=paste0(fig_path, "403_a.png"), 
       width = 0.6*page_width, height = 0.3*page_height, 
       units = "mm", dpi = d_dpi)

##############################################################################
## Boxplots of week 4 outcome
##############################################################################


ggplot(data= t4dat, aes(x=ztext, y=cfb, colour=ztext)) + 
  geom_boxplot(width=0.25) + 
  geom_jitter(alpha=0.25, width=0.1) + 
  scale_colour_brewer(palette="Dark2") + 
  labs(x="", y="Change from baseline") + 
  paper_theme() +
  theme(legend.position="none") + 
  labs(x="", y="Change from baseline", 
       title="Week 4 outcome") +
  theme(panel.border=element_rect(color="grey", fill = NA, size=0.25),
        plot.title = element_text(size = 12),
        axis.text.y = element_text(size = 8), 
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.ticks.x=element_blank(), 
        panel.grid.major.x=element_blank())

ggsave(file=paste0(fig_path, "403_b.png"), 
       width = 0.4*page_width, height = 0.3*page_height, 
       units = "mm", dpi = d_dpi)

##############################################################################
## Spaghetti plots & mean +/- SD
##############################################################################


md <- dres %>% 
  group_by(time, ztext) %>% 
  summarise(m=mean(y), s=sd(y), n=n(), se=s/sqrt(n))

# ggplot() +
#   theme(legend.position = c(0.8,0.15), legend.background=element_blank()) +
#   geom_line(data=dres, aes(x=time, y=y, group=id, colour=factor(ztext)), alpha=0.35) +
#   geom_point(data=md, aes(x=time, y=m, colour=factor(ztext)), size=2.5) +
#   geom_line(data=md, aes(x=time, y=m, colour=factor(ztext)), size=1) +
#   scale_y_continuous(limits= c(min(dres$y)-2.5, max(dres$y))) +
#   scale_colour_brewer(palette="Dark2", name="") +
#   labs(x="Week", y="Outcome",
#        title="Longitudinal individual outcomes with group means") +
#   paper_theme() +
#   theme(panel.border=element_rect(color="grey", fill = NA, size=0.25))
#
# ggsave(file=paste0(fig_path, "403_c.png"), width = d_width, height = d_height, units = "mm", dpi = d_dpi)

ggplot() + 
  theme(legend.position = c(0.8,0.65), legend.background=element_blank()) +
  geom_line(data=dres, aes(x=time, y=y, group=id, colour=factor(ztext)), alpha=0.35) +
  geom_line(data=md, aes(x=time, y=m, colour=factor(ztext)), size=1) + #, position=position_dodge(0.3)) + 
  geom_point(data=md, aes(x=time, y=m, ymin=m-s, ymax=m+s, colour=ztext)) +#, position=position_dodge(0.3)) +
  scale_y_continuous(limits= c(min(dres$y)-2.5, max(dres$y))) +
  scale_colour_brewer(palette="Dark2", name="") +
  labs(x="Week", y="Outcome", 
       title="Longitudinal individual outcomes \nwith group means") +
  paper_theme() +
  theme(panel.border=element_rect(color="grey", fill = NA, size=0.25),
        plot.title = element_text(size = 10),
        axis.text.y = element_text(size = 8), 
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        legend.position=c(0.15,0.225), legend.background=element_blank())

ggsave(file=paste0(fig_path, "403_c.png"), 
       width = 0.5*page_width, height = 0.3*page_height,  
       units = "mm", dpi = d_dpi)

##############################################################################
## 
##############################################################################
# 
# 
# ggplot(md, aes(x=time, y=m, ymin=m-s, ymax=m+s, colour=ztext)) + 
#   geom_pointrange(position=position_dodge(0.3)) + 
#   geom_line(stat="smooth", method="lm", position=position_dodge(0.3), se=FALSE, alpha=0.35) + 
#   scale_y_continuous(limits=c(min(md$m-md$s)-1, max(md$m+md$s))) +
#   scale_colour_brewer(palette="Dark2", name="") + 
#   paper_theme() + 
#   theme(legend.position = c(0.2,0.15), legend.background=element_blank()) +
#   theme(panel.border=element_rect(color="grey", fill = NA, size=0.25)) +
#   labs(x="Week", y="Mean outcome (SD)", title="Longitudinal group means with SD")
# 
# ggsave(file=paste0(fig_path, "403_d.png"), width = d_width, height = d_height, units = "mm", dpi = d_dpi)

##############################################################################
## Model fit to longtiduinal data
##############################################################################

mod <- readRDS("data/mod.rds")

adat <- dres %>% 
  filter(time > 0)

if (FALSE) {
mod <- stan_lmer(y ~ time*factor(z) + baseline + x1 + x2 + (time | id), 
                 data=adat, chains=4, iter=500, cores=4)
print(mod)
saveRDS(mod, "data/mod.rds")
}

nd1 <- dres %>% 
  select(id, time, baseline, x1, x2, z) 

nd2 <- dres %>% 
  select(id, time, baseline, x1, x2, z) %>%
  mutate(z=1-z)

nd <- bind_rows(nd1, nd2) %>%
  arrange(id, time, z)

prs <- c(0.05,0.5,0.95)

ppnd <- nd %>% select(id, time, z) %>%
#bind_cols(as_tibble(t(posterior_predict(mod, newdata=nd, re.form=~0)))) %>% 
  bind_cols(as_tibble(t(posterior_linpred(mod, newdata=nd, re.form=~0)))) %>%
  gather(4:1003, key="ppid", value="ypred") %>%
  spread(z, ypred) %>% 
  mutate(contr=`1`-`0`) %>% 
  group_by(ppid, time) %>%
  do({
    m0 <- mean(.$`0`)
    m1 <- mean(.$`1`)
    mc <- mean(.$contr)
    as_tibble(cbind(m0, m1, mc))
    }) %>% 
  ungroup() %>%
  group_by(time) %>%
  do({
    q0 <- t(quantile(.$m0, probs=prs))
    q1 <- t(quantile(.$m1, probs=prs))
    contr <- t(quantile(.$mc, probs=prs))
    as_tibble(rbind(q0, q1, contr)) %>%
      mutate(var=c("Placebo", "Active", "Contrast"))
    })

##############################################################################
## Treatment difference, median & 90% CI errorbars
##############################################################################


ppnd %>%
  filter(var != "Contrast" ) %>%
  ggplot(aes(x=time, y=`50%`, ymin=`5%`, ymax=`95%`, colour=factor(var, levels=c("Placebo","Active")))) +
  geom_pointrange(position=position_dodge(width=0.3)) +
  geom_line(position=position_dodge(width=0.3)) +
  scale_color_brewer(palette="Dark2", name="") +
  paper_theme() +
  theme(legend.position=c(0.15,0.245), legend.background=element_blank()) +
  theme(panel.border=element_rect(color="grey", fill = NA, size=0.25),
        plot.title = element_text(size = 10, margin = rep(unit(0.01*page_height,"mm"),3)),
        plot.subtitle = element_text(size = 8, color = rgb(0.3,0.3,0.3), margin = rep(unit(0.001*page_height,"mm"),3)),
        axis.text.y = element_text(size = 8), 
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10)) +
  labs(x="Week", y="Outcome",
       title="Active group improves over time",
       subtitle = "Posterior median (90% credible interval)")

ggsave(file=paste0(fig_path, "403_d.png"), 
       width = 0.5*page_width, height = 0.3*page_height, 
       units = "mm", dpi = d_dpi)

##############################################################################
## Treatment difference, median & 90% CI ribbon
##############################################################################

ppnd %>% 
  filter(var == "Contrast" & time > 0) %>% 
  ggplot(aes(x=time, y=`50%`)) + 
  geom_ribbon(aes(ymin=`5%`, ymax=`95%`), fill="black", alpha=0.15) + 
  geom_point(size=1.5) + geom_line(size=1)+ 
  geom_hline(yintercept=0, linetype=2)  + 
  labs(x="Week", y="Treatment difference", title="Treatment effect increases over time",
       subtitle = "Posterior median (90% credible interval)") +
  geom_segment(aes(x=0.98, y=-0.01, xend=0.98, yend=-3.5), arrow=arrow(length = unit(0.02*page_height,"mm")), alpha=0.25) + 
  annotate("text", label="Greater benefit", x=1.4, y=-3.4, size=5, alpha=0.75) +
  paper_theme() +
  theme(panel.border=element_rect(color="grey", fill = NA, size=0.25),
        plot.subtitle = element_text(size = 10, color = rgb(0.3,0.3,0.3))) 


ggsave(file=paste0(fig_path, "403_e.png"), 
       width = 0.95*page_width, height = 0.3*page_height, 
       units = "mm", dpi = d_dpi)
