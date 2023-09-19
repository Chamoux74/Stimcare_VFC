library(tibble)
library(purrr)
library(dplyr)
library(readr)
library(TimeDomainHRV)
library(varian)
library(readxl)

dffatigue <- read_xlsx("C:/Users/maxch/Git/VFC/fatigue.xlsx")

dffatigue$Instant <- factor(dffatigue$Instant, levels = c("Pre", "Post", "Post 48"))


plot <- dffatigue %>%  ggplot(aes(x = Instant , y = Fatigue )) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5 , size = 12 , face = "bold") ,
    axis.text = element_text(size = 7) ,
    axis.title = element_text(size = 8 , face = "bold") ,
    legend.position = "right" ,
    legend.title = element_text(size = 8 , face = "bold") ,
    legend.text = element_text(size = 6) ,
    legend.background = element_rect(color = "black" , size = 0.1)
  ) +
  geom_boxplot(aes(fill = Conditon , color = Conditon), alpha = 0.4) +
  #geom_line(aes(color = sujet , group = sujet), linetype = "dotted" , size = 0.8) +
  geom_point(
    aes(
      color = Conditon,
      shape = Conditon ,
      group = Conditon
    ),
    size = 2,
    position = position_dodge2(width = 0.75,
                               preserve = "single")
  ) +
  stat_summary(aes(group = Conditon),
               fun.y = mean ,
               shape = 23  ,
               fill = "black" ,
               size = 1 , position = position_dodge2(width = 0.75,
                                                     preserve = "single")
  )

plot


res.aov1 <- rstatix::anova_test(
  data = dffatigue, dv = Fatigue, wid = Sujet ,
  within = c(Conditon, Instant) , effect.size = "ges",
  detailed = TRUE,
)

get_anova_table(res.aov1 , correction = "auto")

one.way <- dffatigue %>%
  group_by(Instant) %>%
  anova_test(dv = Fatigue, wid = Sujet, within = Conditon) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

one.way1 <- dffatigue %>%
  group_by(Conditon) %>%
  anova_test(dv = Fatigue, wid = Sujet, within = Instant) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way1
