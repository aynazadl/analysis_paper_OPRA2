getwd()
setwd("/Users/Aynaz/Documents/aynaz/libio/stat_result")
###nombre iteration 
df_iter<-read.csv("df_iter.csv",check.names = FALSE)
df_iter
str(df_iter)
####stat descriptive
sum_stat_iter<-df_iter %>%
  group_by(Gen) %>%
  get_summary_stats(nb_iter, type = "mean_sd")
sum_stat_iter
write.csv(sum_stat_iter,"sum_stat_iter.csv", row.names = FALSE)
##
bxp1 <- ggboxplot(df_iter, x = "Gen", y = "nb_iter", add = "point")
bxp1
###valeur aberrants
outliers_iter<-df_iter %>%
  group_by(Gen) %>%
  identify_outliers(nb_iter)
outliers_iter
write.csv(outliers_iter,"outliers_iter.csv", row.names = FALSE)
df_iter1<-df_iter[df_iter$id_audiogram != 3 & df_iter$id_audiogram != 11, ]  
df_iter1
###test de normalité , n'est pas normale et on enleve pas les valeur aberrants
norm_iter<-df_iter %>%
  group_by(Gen) %>%
  shapiro_test(nb_iter)
norm_iter
ggqqplot(df_iter, "nb_iter", facet.by = "Gen")
##
res.aov1 <- anova_test(data = df_iter, dv = nb_iter, wid = id_audiogram, within = Gen)
anova_iter<-get_anova_table(res.aov1)
anova_iter
write.csv(anova_iter,"df_iter_anova.csv", row.names = FALSE)
###
pwc_iter<-df_iter %>%
  pairwise_t_test(
    nb_iter ~ Gen, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc_iter
write.csv(pwc_iter,"pwc_iter.csv", row.names = FALSE)
######
pwc_iter <- pwc_iter %>% add_xy_position(x = "Gen")
bxp1 + 
  stat_pvalue_manual(pwc_iter) +
  labs(
    subtitle = get_test_label(res.aov1, detailed = TRUE),
    caption = get_pwc_label(pwc_iter)
  )
ggsave("df_iter_plot.png")
######nonparamtric
res.fried1 <- df_iter %>% friedman_test(nb_iter ~ Gen |id_audiogram)
res.fried1
write.csv(res.fried1,"fried_iter.csv", row.names = FALSE)
##
pwc_fried_iter <- df_iter %>%
  wilcox_test(nb_iter ~ Gen, paired = TRUE, p.adjust.method = "bonferroni")
pwc_fried_iter
write.csv(pwc_fried_iter,"pwc_fried_iter.csv", row.names = FALSE)
####
pwc_fried_iter <- pwc_fried_iter %>% add_xy_position(x = "Gen")
ggboxplot(df_iter, x = "Gen", y = "nb_iter", add = "point") +
  stat_pvalue_manual(pwc_fried_iter, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried1,  detailed = TRUE),
    caption = get_pwc_label(pwc_fried_iter)
  )
ggsave("df_iter_fried_plot.png")
#####score
df_score<-read.csv("df_score.csv",check.names = FALSE)
df_score=df_score[,-1]
str(df_score)
write.csv(df_score,"df_score.csv", row.names = FALSE)
###
sum_stat_score<-df_score %>%
  group_by(Gen) %>%
  get_summary_stats(score, type = "mean_sd")
sum_stat_score
write.csv(sum_stat_score,"sum_stat_score.csv", row.names = FALSE)
####
bxp2 <- ggboxplot(df_score, x = "Gen", y = "score", add = "point")
bxp2
###
outliers_score<-df_score %>%
  group_by(Gen) %>%
  identify_outliers(score)
outliers_score
df_score1<-df_score[df_score$id_audiogram != 8 & df_score$id_audiogram != 10& df_score$id_audiogram != 12, ]  
df_score1
write.csv(outliers_score,"outliers_score.csv", row.names = FALSE)
###
norm_score<-df_score %>%
  group_by(Gen) %>%
  shapiro_test(score)
norm_score
ggqqplot(df_score, "score", facet.by = "Gen")
####
res.aov2 <- anova_test(data = df_score, dv = score, wid = id_audiogram, within = Gen)
anova_score<-get_anova_table(res.aov2)
anova_score
write.csv(anova_score,"df_score_anova.csv", row.names = FALSE)
####
pwc_score <- df_score %>%
  pairwise_t_test(
    score ~ Gen, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc_score
write.csv(pwc_score,"pwc_score.csv", row.names = FALSE)
####
pwc_score
pwc_score <- pwc_score %>% add_xy_position(x = "Gen")
bxp2 + 
  stat_pvalue_manual(pwc_score) +
  labs(
    subtitle = get_test_label(res.aov2, detailed = TRUE),
    caption = get_pwc_label(pwc_score)
  )
ggsave("df_score_plot.png")
####friedman
res.fried2 <- df_score1 %>% friedman_test(score ~ Gen |id_audiogram)
res.fried2
write.csv(res.fried2,"fried_score.csv", row.names = FALSE)
###
pwc_fried_score <- df_score1 %>%
  wilcox_test(score ~ Gen, paired = TRUE, p.adjust.method = "bonferroni")
pwc_fried_score
pwc_fried_score <- pwc_fried_score %>% add_xy_position(x = "Gen")
ggboxplot(df_score, x = "Gen", y = "score", add = "point") +
  stat_pvalue_manual(pwc_fried_score, hide.ns = FALSE) +
  labs(
    subtitle = get_test_label(res.fried2,  detailed = TRUE),
    caption = get_pwc_label(pwc_fried_score)
  )
ggsave("df_score_fried_plot.png")
write.csv(pwc_fried_score,"pwc_fried_score.csv", row.names = FALSE)
####two way repeated mesures_method
df_method<-read.csv("df_method.csv",check.names = FALSE)
df_method=df_method[,-1]
str(df_method)
df_method <- df_method %>%
  convert_as_factor(id_audiogram, method,Gen)
####
sum_stat_method<-df_method %>%
  group_by(method, Gen) %>%
  get_summary_stats(score, type = "mean_sd")
sum_stat_method
write.csv(sum_stat_method,"sum_stat_method.csv", row.names = FALSE)
###
bxp3 <- ggboxplot(
  df_method, x = "Gen", y = "score",
  color = "method", palette = "jco"
)
bxp3
###
outliers_method<-df_method %>%
  group_by(method, Gen) %>%
  identify_outliers(score)
outliers_method
df_method1<-df_method[df_method$id_audiogram != 8 & df_method$id_audiogram != 10& df_method$id_audiogram != 12, ]  
df_method1
write.csv(outliers_method,"outliers_method.csv", row.names = FALSE)
####
norm_method<-df_method %>%
  group_by(method, Gen) %>%
  shapiro_test(score)
norm_method
ggqqplot(df_method, "score", ggtheme = theme_bw()) +
  facet_grid(Gen ~ method, labeller = "label_both")
###
res.aov3 <- anova_test(
  data = df_method, dv = score, wid = id_audiogram,
  within = c(method, Gen)
)
get_anova_table(res.aov3)

###
anova_method <- df_method %>%
  group_by(Gen) %>%
  anova_test(dv = score, wid = id_audiogram, within = method) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
anova_method
write.csv(anova_method,"df_method_anova.csv", row.names = FALSE)
####
pwc_method <- df_method %>%
  group_by(Gen) %>%
  pairwise_t_test(
    score ~ method, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc_method
write.csv(pwc_method,"pwc_method.csv", row.names = FALSE)
####
pwc_method <- pwc_method %>% add_xy_position(x = "Gen")
bxp3 + 
  stat_pvalue_manual(pwc_method, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov3, detailed = TRUE),
    caption = get_pwc_label(pwc_method)
  )
ggsave("df_method_plot.png")
