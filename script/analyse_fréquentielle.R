# extraction fréquence patch
dfhrvpatchlf <- c()

for (i in 1:length(listhrvdatapatch)) {
  # Extraction de l'élément LF de la liste TimeAnalysis pour chaque sujet
  dfhrvpatchlf[i] <- listhrvdatapatch[[i]]$FreqAnalysis[[1]]$LF
}

dfhrvpatchhf <- c()

for (i in 1:length(listhrvdatapatch)) {
  # Extraction de l'élément HF de la liste TimeAnalysis pour chaque sujet
  dfhrvpatchhf[i] <- listhrvdatapatch[[i]]$FreqAnalysis[[1]]$HF
}

#extraction fréquence placebo

dfhrvplacebolf <- c()

for (i in 1:length(listhrvdataplacebo)) {
  # Extraction de l'élément LF de la liste TimeAnalysis pour chaque sujet
  dfhrvplacebolf[i] <- listhrvdataplacebo[[i]]$FreqAnalysis[[1]]$LF
}

dfhrvplacebohf <- c()

for (i in 1:length(listhrvdataplacebo)) {
  # Extraction de l'élément HF de la liste TimeAnalysis pour chaque sujet
  dfhrvplacebohf[i] <- listhrvdataplacebo[[i]]$FreqAnalysis[[1]]$HF
}

#bind avec df rmssd

dfhrvpatch <- cbind(dfhrvpatch, dfhrvpatchhf, dfhrvpatchlf)
colnames(dfhrvpatch) <- c("rMSSD", "condition", "sujet", "timing", "HF" , "LF")

dfhrvplacebo <- cbind(dfhrvplacebo, dfhrvplacebohf, dfhrvplacebolf)
colnames(dfhrvplacebo) <- c("rMSSD", "condition", "sujet", "timing", "HF" , "LF")

dfhrvfinal <- rbind(dfhrvpatch, dfhrvplacebo)

# outliers

outHF <-dfhrvfinal %>%  identify_outliers("HF")
outLF <- dfhrvfinal %>% identify_outliers("LF")

dfhrvfinal <- dfhrvfinal %>%
  filter(sujet != "GM")

# plot HF

dfhrvfinal %>% ggplot(aes(x = timing , y = HF)) +
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
  geom_boxplot(aes(fill = condition , color = condition), alpha = 0.4) +
  #geom_line(aes(color = sujet , group = sujet), linetype = "dotted" , size = 0.8) +
  geom_point(
    aes(
      color = condition,
      shape = condition ,
      group = condition
    ),
    size = 2,
    position = position_dodge2(width = 0.75,
                               preserve = "single")
  ) +
  stat_summary(aes(group = condition),
               fun.y = mean ,
               shape = 23  ,
               fill = "black" ,
               size = 1 , position = position_dodge2(width = 0.75,
                                                     preserve = "single")
  )+
  stat_pvalue_manual(
    ttesttime2 ,
    tip.length = 0 ,
    hide.ns = TRUE ,
    label = "p = {p.adj}" , color = "#FC4E07" , y.position = c(3700 , 3400)
  ) +
  stat_pvalue_manual(
    ttesttime3 ,
    tip.length = 0 ,
    hide.ns = FALSE ,
    label = "p = {p.adj}" , color = "#00AFBB" , y.position = c(-100 , -600, -300 )
  )

#plot LF

dfhrvfinal %>% ggplot(aes(x = timing , y = LF)) +
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
  geom_boxplot(aes(fill = condition , color = condition), alpha = 0.4) +
  #geom_line(aes(color = sujet , group = sujet), linetype = "dotted" , size = 0.8) +
  geom_point(
    aes(
      color = condition,
      shape = condition ,
      group = condition
    ),
    size = 2,
    position = position_dodge2(width = 0.75,
                               preserve = "single")
  ) +
  stat_summary(aes(group = condition),
               fun.y = mean ,
               shape = 23  ,
               fill = "black" ,
               size = 1 , position = position_dodge2(width = 0.75,
                                                     preserve = "single")) +
  stat_pvalue_manual(
    ttesttime4 ,
    tip.length = 0 ,
    hide.ns = TRUE ,
    label = "p = {p.adj}" , color = "#FC4E07" , y.position = c(4000 , 5500)
  ) +
  stat_pvalue_manual(
    ttesttime5 ,
    tip.length = 0 ,
    hide.ns = TRUE ,
    label = "p = {p.adj}" , color = "#00AFBB" , y.position = c(-100 , -200 )
  )

# anova

res.aov1 <- rstatix::anova_test(
  data = dfhrvfinal, dv = HF, wid = sujet ,
  within = c(condition, timing) , effect.size = "ges",
  detailed = TRUE,
)

get_anova_table(res.aov1 , correction = "auto")

res.aov <- rstatix::anova_test(
  data = dfhrvfinal, dv = LF, wid = sujet ,
  within = c(condition, timing) , effect.size = "ges",
  detailed = TRUE,
)

get_anova_table(res.aov , correction = "auto")

one.way <- dfhrvfinal %>%
  group_by(condition) %>%
  anova_test(dv = HF, wid = sujet, within = timing) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

one.way1 <- dfhrvfinal %>%
  group_by(condition) %>%
  anova_test(dv = LF, wid = sujet, within = timing) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way1

# test t

ttesttime <- dfhrvfinal %>%
  group_by(condition) %>%
  pairwise_t_test(
    HF ~ timing, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
ttesttime

ttesttime1 <- dfhrvfinal %>%
  group_by(condition) %>%
  pairwise_t_test(
    LF ~ timing, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
ttesttime1


ttesttime2 <- ttesttime[ttesttime$condition == "patch", ] %>% add_xy_position(x = "timing")
ttesttime3 <- ttesttime[ttesttime$condition == "placebo",] %>% add_xy_position(x = "timing")

ttesttime4 <- ttesttime1[ttesttime1$condition == "patch", ] %>% add_xy_position(x = "timing")
ttesttime5 <- ttesttime1[ttesttime1$condition == "placebo",] %>% add_xy_position(x = "timing")
