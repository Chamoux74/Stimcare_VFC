library(RHRV)
library(ggpubr)
library(rstatix)
#load and create data for HRV analysis for patch

listhrvdatapatch <-
  lapply(vfcpatch, function(bob) {
    CreateHRVData() %>%
      SetVerbose(Verbose = FALSE) %>%
      LoadBeatRR(RecordName = bob) %>%
      BuildNIHR() %>%
      FilterNIHR() %>%
      InterpolateNIHR() %>%
      CreateTimeAnalysis() %>%
      CreateFreqAnalysis() %>%
      CalculatePSD(indexFreqAnalysis = 1 ,
                   method = "pgram") %>%
      CalculatePowerBand(shift = 240 ,
                         size = 240 ,
                         type = "fourier")
  })

names(listhrvdatapatch) <- tools::file_path_sans_ext(basename(vfcpatch))

dfhrvpatch <- c()

for (i in 1:length(listhrvdatapatch)) {
  # Extraction de l'élément rMSSD de la liste TimeAnalysis pour chaque sujet
  dfhrvpatch[i] <- listhrvdatapatch[[i]]$TimeAnalysis[[1]]$rMSSD
}

#create and load data pacebo for HRV analysis

listhrvdataplacebo <-
  lapply(vfcplacebo, function(bob) {
    CreateHRVData() %>%
      SetVerbose(Verbose = FALSE) %>%
      LoadBeatRR(RecordName = bob) %>%
      BuildNIHR() %>%
      FilterNIHR() %>%
      InterpolateNIHR() %>%
      CreateTimeAnalysis() %>%
      CreateFreqAnalysis() %>%
      CalculatePSD(indexFreqAnalysis = 1 ,
                   method = "pgram") %>%
      CalculatePowerBand(shift = 240 ,
                         size = 240 ,
                         type = "fourier")})

names(listhrvdataplacebo) <- tools::file_path_sans_ext(basename(vfcplacebo))

dfhrvplacebo <- c()

for (i in 1:length(listhrvdataplacebo)) {
  # Extraction de l'élément rMSSD de la liste TimeAnalysis pour chaque sujet
  dfhrvplacebo[i] <- listhrvdataplacebo[[i]]$TimeAnalysis[[1]]$rMSSD
}

#ajout des vecteur sujet, patch, timing

dfhrvpatch <- as.data.frame(dfhrvpatch)
dfhrvplacebo <- as.data.frame(dfhrvplacebo)
patch <- "patch"
placebo <- "placebo"
timing <- c(rep(c("post48", "post", "pre"), 18 ))
sujet <-
  c(rep(
    c(
      "AL",
      "AB",
      "BM",
      "BR",
      "BA",
      "GA",
      "GM",
      "GS",
      "JS",
      "MA",
      "MF",
      "PE",
      "RF",
      "RO",
      "SP",
      "SL" ,
      "TM",
      "VP"
    ) ,
    each = 3
  ))

dfhrvpatch <- cbind(dfhrvpatch, patch, sujet , timing)
dfhrvplacebo <- cbind(dfhrvplacebo, placebo, sujet, timing)

colnames(dfhrvpatch) <- c("rMSSD", "condition", "sujet", "timing")
colnames(dfhrvplacebo) <- c("rMSSD", "condition", "sujet", "timing")

#plot graph

dfhrvpatch$timing <- factor(dfhrvpatch$timing, levels = c("pre","post","post48"))
dfhrvplacebo$timing <- factor(dfhrvplacebo$timing, levels = c("pre","post","post48"))

dfhrvfinal <- rbind(dfhrvpatch, dfhrvplacebo)

dfhrvfinal %>% ggplot(aes(x = timing , y = rMSSD)) +
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
  ) +
  stat_pvalue_manual(
    ttesttime1 ,
    tip.length = 0 ,
    hide.ns = TRUE ,
    label = "p = {p.adj}" , color = "#FC4E07" , y.position = c(0 , -5)
  ) +
  stat_pvalue_manual(
    ttesttime2 ,
    tip.length = 0 ,
    hide.ns = TRUE ,
    label = "p = {p.adj}" , color = "#00AFBB" , y.position = c(160 , 185 )
  )

# anova

res.aov1 <- rstatix::anova_test(
  data = dfhrvfinal, dv = rMSSD, wid = sujet ,
  within = c(condition, timing) , effect.size = "ges",
  detailed = TRUE,
)

get_anova_table(res.aov1 , correction = "auto")

one.way <- dfhrvfinal %>%
  group_by(timing) %>%
  anova_test(dv = rMSSD, wid = sujet, within = condition) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

one.way1 <- dfhrvfinal %>%
  group_by(condition) %>%
  anova_test(dv = rMSSD, wid = sujet, within = timing) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way1

# test t

ttesttime <- dfhrvfinal %>%
  group_by(condition) %>%
  pairwise_t_test(
    rMSSD ~ timing, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
ttesttime


ttesttime1 <- ttesttime[ttesttime$condition == "patch", ] %>% add_xy_position(x = "timing")
ttesttime2 <- ttesttime[ttesttime$condition == "placebo",] %>% add_xy_position(x = "timing")
