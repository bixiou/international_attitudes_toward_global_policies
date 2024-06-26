##### Stated support #####
# support_binary_positive
decrit("gcs_support", us1)
binconf(sum(us1$weight[us1$gcs_support == 'Yes']), nrow(us1), alpha = 0.05)
binconf(sum(eu$weight[eu$gcs_support == 'Yes']), nrow(eu), alpha = 0.05)
decrit("gcs_support", eu)
decrit("nr_support", us1)
decrit("nr_support", eu)
reweighted_estimate("gcs_support", "EU") # 76
reweighted_estimate("gcs_support", "US1") # 53 Assigns a weight 0 to vote_us = PNR/No right
reweighted_estimate("gcs_support", "US1", omit = "vote_us") # 52 Uses all observations and still reweight for vote using e$vote
decrit("gcs_support", us1, which = us1$voted)


# Global wealth tax
# global_tax_sharing_positive, global_tax_global_share_positive, global_tax_global_share_share TODO! combine them
decrit(us2$global_tax_support > 0, us2)
decrit(eu$global_tax_support > 0, eu, which = eu$country == 'FR')
decrit(eu$global_tax_support > 0, eu, which = eu$country == 'DE')
decrit(eu$global_tax_support > 0, eu, which = eu$country == 'ES')
decrit(eu$global_tax_support > 0, eu, which = eu$country == 'UK') 
decrit(all$global_tax_support > 0, all) 
decrit(all$national_tax_support > 0, all)
decrit(us2$global_tax_support > 0, us2, which = us2$global_tax_support != 0) 
decrit(us2$national_tax_support > 0, us2, which = us2$national_tax_support != 0) 
decrit("global_tax_support", us2)
decrit("national_tax_support", eu)
decrit("global_tax_global_share", all, weight = F)
decrit("global_tax_global_share", us2) # 41
decrit(us2$global_tax_global_share > 0)
wtd.mean(eu$global_tax_global_share[eu$country == 'UK'] > 0, weights = eu$weight_country[eu$country == 'UK'])
decrit("global_tax_sharing", all, weight = F)
decrit("global_tax_sharing", us2) # 65% Yes
decrit("global_tax_support", eu, which = eu$country == "FR", weights = eu$weight_country)

# Other global policies
# support_likert_positive, support_likert_share
# support_likert_positive, global_policies_mean, global_policies_positive, global_policies_share

# Foreign aid 
decrit("foreign_aid_raise_support", all)
# healthcare, education
for (c in countries) print(paste(c, round(wtd.mean((d(c)$foreign_aid_reduce_how_healthcare | d(c)$foreign_aid_reduce_how_education), d(c)$weight), 3)))
# taxes
for (c in countries) print(paste(c, round(wtd.mean((d(c)$foreign_aid_reduce_how_income_tax | d(c)$foreign_aid_reduce_how_wealthy | d(c)$foreign_aid_reduce_how_corporations), d(c)$weight), 3)))


# Using ml is advised because the estimator is more efficient, i.e. has tighter CIs. But Graeme Blair wrote in an email "you can use the linear model".
# Using 80% CIs instead of the more common 95% may be a way to address the lower efficiency (which increases the SE by ~46% according to Blair (11, Figure 2), vs. 1.96/1.28=1+53% for taking 80% instead of 95% CIs)
# The estimates, SE, CIs and test (list == direct) do not depend on J if lm is used (I've checked the R code). 
# On the contrary, ml depends on J and cannot be used for our design (with a varying J).
# The lm model applying to a varying J is unbiased to the extent that the four branches are balanced (which they are almost perfectly).
# Also, contrary to the other models, the linear model (coefficient) has a clear interpretation.
# The difference test works by Monte Carlo (cf. below): by computing the difference of 10k draws of direct & indirect support (using their respective normal distributions) and computing the differences' mean and standard deviation.
summary(lm(list_exp ~ branch_list_exp_g*continent, data = all, weights = all$weight))
fit.list <- ictreg(list_exp ~ continent, treat = 'branch_list_exp_g', J = 2 + wtd.mean(all$branch_list_exp_r == T, all$weight), data = all, weights = 'weight', method = "lm")
fit.direct <- glm(gcs_support == 'Yes' ~ continent, data = all[all$wave != "US2",], weights = weight, family = binomial("logit"))
(avg.pred.social.desirability <- predict(fit.list, direct.glm = fit.direct, se.fit = TRUE, level = .8))

summary(lm(list_exp ~ branch_list_exp_g, data = all[all$continent == "Europe",], weights = weight))
fit.list_eu <- ictreg(list_exp ~ 1, treat = 'branch_list_exp_g', J = 2 + wtd.mean(eu$branch_list_exp_r == T, eu$weight), data = all[all$continent == "Europe",], weights = 'weight', method = "lm")
fit.direct_eu <- glm(gcs_support == 'Yes' ~ 1, data = all[all$continent == "Europe",], weights = weight, family = binomial("logit"))
(avg.pred.social.desirability_eu <- predict(fit.list_eu, direct.glm = fit.direct_eu, se.fit = TRUE, level = .8)) 

summary(lm(list_exp ~ branch_list_exp_g, data = all[all$continent == "US",], weights = weight))
fit.list_us <- ictreg(list_exp ~ 1, treat = 'branch_list_exp_g', J = 2 + wtd.mean(us1$branch_list_exp_r == T, us1$weight), data = all[all$continent == "US",], weights = 'weight', method = "lm")
fit.direct_us <- glm(gcs_support == 'Yes' ~ 1, data = all[all$wave == "US1",], weights = weight, family = binomial("logit"))
(avg.pred.social.desirability_us <- predict(fit.list_us, direct.glm = fit.direct_us, se.fit = TRUE, level = .8)) 

same_reg_subsamples(dep.var = "list_exp", dep.var.caption = "Number of supported policies", covariates = c("branch_list_exp_g"), share_na_remove = 0.5,
                    data = all, along = "continent", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE, constant_instead_mean = T,
                    filename = "reg_list_exp_g", folder = "../tables/continents/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T, 
                    add_lines = list(c(11, paste("\\hline  \\\\[-1.8ex] \\textit{Support for GCS} &", round(wtd.mean(all$gcs_support[all$wave != "US2"], weights = all$weight[all$wave != "US2"]), 3), " & ", round(wtd.mean(us1$gcs_support, weights = us1$weight), 3), " & ", round(wtd.mean(eu$gcs_support, weights = eu$weight), 3), "\\\\")),
                                     c(12, paste("\\textit{Social desirability bias} & \\textit{$", round(avg.pred.social.desirability$fit[3,1], 3), "$} & \\textit{$", round(avg.pred.social.desirability_us$fit[3,1], 3), "$} & \\textit{$", round(avg.pred.social.desirability_eu$fit[3,1], 3),  "$}\\\\")),
                                     c(13, paste("\\textit{80\\% C.I. for the bias} & \\textit{ $[", round(avg.pred.social.desirability$fit[3,2], 2), ";", round(avg.pred.social.desirability$fit[3,3], 2), "]$ } & \\textit{ $[", round(avg.pred.social.desirability_us$fit[3,2], 2), ";", round(avg.pred.social.desirability$fit[3,3], 2), "]$} & \\textit{ $[", round(avg.pred.social.desirability_eu$fit[3,2], 2), ";", round(avg.pred.social.desirability$fit[3,3], 2), "]$}\\\\"))))


##### Petition ##### 
wtd.t.test(us1$gcs_support, us1$petition_gcs, weight=us1$weight, drops = "") # rejects equality (p=.046)
t.test(us1$gcs_support, us1$petition_gcs, paired = T) # rejects equality (p=.016)
t.test(us1$gcs_support, us1$petition_gcs, paired = F) # rejects equality (p=.019)
decrit("petition_gcs", us1)
wtd.t.test(us1$gcs_support[us1$branch_petition == "gcs"], us1$petition_gcs[us1$branch_petition == "gcs"], weight=us1$weight[us1$branch_petition == "gcs"]) # cannot reject equality (p=.30)
wtd.t.test(us1$nr_support[us1$branch_petition == "nr"], us1$petition_nr[us1$branch_petition == "nr"], weight=us1$weight[us1$branch_petition == "nr"]) # cannot reject equality (p=.76)
wtd.t.test(eu$gcs_support[eu$branch_petition == "gcs"], eu$petition_gcs[eu$branch_petition == "gcs"], weight=eu$weight[eu$branch_petition == "gcs"]) # rejects equality (p=1e-5)
wtd.t.test(eu$nr_support[eu$branch_petition == "nr"], eu$petition_nr[eu$branch_petition == "nr"], weight=eu$weight[eu$branch_petition == "nr"]) # rejects equality (p=.01)
decrit("petition_gcs", eu)
decrit("petition_nr", eu)


##### Conjoint analyses #####
# conjoint_ab_all_positive
decrit(us1$branch_conjoint_b)
decrit(eu$branch_conjoint_b)
decrit(eu$conjoint_crg_cr, eu)
wtd.t.test(eu$conjoint_rg_r_binary, eu$gcs_support, weight = eu$weight)
wtd.t.test(eu$conjoint_rg_r_binary, eu$gcs_support, weight = eu$weight, drops = "")
wtd.t.test(us1$conjoint_rg_r_binary, us1$gcs_support, weight = us1$weight)
wtd.t.test(us1$conjoint_rg_r_binary, us1$gcs_support, weight = us1$weight, drops = "")
wtd.t.test(us1$conjoint_cr_gr_binary, .5, weight = us1$weight)
wtd.t.test(eu$conjoint_cr_gr_binary, .5, weight = eu$weight)
wtd.t.test(us1$conjoint_cr_gr_binary, .5, weight = us1$weight)


##### Conjoint analysis: Left/Right vote #####
same_reg_subsamples(dep.var = "conjoint_c", dep.var.caption = "Prefers the Progressive platform", covariates = c("branch_c_gcs"), 
                    data = all, along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE, omit.note = T,
                    filename = "conjoint_c_left", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)

same_reg_subsamples(dep.var = "conjoint_c_right", dep.var.caption = "Prefers the Conservative platform", covariates = c("branch_c_gcs"), 
                    data = all, along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE, omit.note = T,
                    filename = "conjoint_c_right", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)

decrit("conjoint_c_none", all)
same_reg_subsamples(dep.var = "conjoint_c_none", dep.var.caption = "Prefers None of the platforms", covariates = c("branch_c_gcs"), 
                    data = all, along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE, omit.note = T,
                    filename = "conjoint_c_none", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)

# Good one
same_reg_subsamples(dep.var = "conjoint_c", dep.var.caption = "Prefers the Progressive platform", covariates = c("branch_c_gcs"), 
                    data = all[all$conjoint_c_none == F & all$wave != "US2",], along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE, omit.note = T,
                    filename = "conjoint_c_wo_none", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)
summary(lm(conjoint_c ~ branch_c_gcs, us1[us1$conjoint_c_none == F,], weights = weight)) # p-value: .13
summary(lm(gcs_support ~ swing_state, us1, weights = weight)) # .012, n=693
summary(lm(conjoint_c ~ branch_c_gcs, us1[us1$conjoint_c_none == F & us1$swing_state == T,], weights = weight)) # .012, n=693
summary(lm(conjoint_c ~ branch_c_gcs, us1[us1$conjoint_c_none == F & us1$swing_state_3pp == T,], weights = weight)) # .006, n=509
summary(lm(conjoint_c_right ~ branch_c_gcs, us1, weights = weight)) # p-value: .0504

# Interaction with political leaning:
summary(lm(conjoint_c ~ branch_c_gcs * vote_factor, d("FR")[d("FR")$conjoint_c_none == F,], weights = weight))
summary(lm(conjoint_c ~ branch_c_gcs * vote3, us1[us1$conjoint_c_none == F,], weights = weight))

same_reg_subsamples(dep.var = "conjoint_c", dep.var.caption = "Prefers the Progressive platform", covariates = c("branch_c_gcs"), weights = "weight_vote", omit.note = T,
                    data = all[all$conjoint_c_none == F & all$wave != "US2",], along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
                    filename = "conjoint_c_wo_none_weight_vote", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)

for (i in c("left", "right", "none", "wo_none", "wo_none_weight_vote")) {
  temp <- readLines(paste0("../tables/country_comparison/conjoint_c_", i, ".tex"))
  writeLines(sub("United Kingdom", "UK", temp), paste0("../tables/country_comparison/conjoint_c_", i, ".tex"))
}

# The effect tends to be driven by right-wing voters
summary(lm(conjoint_c ~ vote_factor*branch_c_gcs, data = all, weights = weight))
summary(lm(conjoint_c ~ vote_factor*branch_c_gcs, data = all[all$conjoint_c_none == F & all$wave != "US2",], weights = weight))
summary(lm(conjoint_c ~ vote_factor*branch_c_gcs, data = eu[eu$conjoint_c_none == F,], weights = weight))
summary(lm(conjoint_c ~ vote_factor*branch_c_gcs, data = us1[us1$conjoint_c_none == F,], weights = weight))
summary(lm(conjoint_c ~ vote_factor*branch_c_gcs, data = eu[eu$country=="FR" & eu$conjoint_c_none == F,], weights = weight))
summary(lm(conjoint_c ~ vote_factor*branch_c_gcs, data = eu[eu$country=="FR" & eu$conjoint_c_none == F,], weights = weight))

# amce is created in conjoint_analysis.R
amce$UK$user.levels # same in all EU. 1: GCS, 2: tax, 3: assembly, 4: aid
amce$FR$estimates$foreignpolicy # 1: .13, 2: .11, 3: .12
amce$UK$estimates$foreignpolicy # 1: .09, 2: .13, 3: .07
amce$ES$estimates$foreignpolicy # 1: .04, 2: .05, 3:-.01
amce$DE$estimates$foreignpolicy # 1: .09, 2: .09, 3: .10
amce$us1$estimates$Foreignpolicy #1: .01, 2: .09, 3: .08

# Multiple hypotheses testing
pvalues_treatment <- list() # adjusted for multiple testing
for (c in c(countries, "all")) {
  pvalues_treatment[[c]] <- list()
  temp <- lm(conjoint_c ~ branch_c_gcs, data = d(c)[d(c)$conjoint_c_none == F & d(c)$wave != "US2",], weights = weight)
  pvalues_treatment[[c]] <- summary(temp)$coefficients[(length(summary(temp)$coefficients[,4])-2):length(summary(temp)$coefficients[,4]), 4]
  }
pvalues_treatment
sort(unlist(pvalues_treatment))
(adjusted_pvalues <- sort(p.adjust(unlist(pvalues_treatment), method = 'fdr')))


largest_effect_amce <- function(model) {
  max <- p_max <- -1
  for (d in names(model$estimates)) for (p in colnames(model$estimates[[d]])) if (model$estimates[[d]][1, p] > max) {
    max <- model$estimates[[d]][1, p]
    p_max <- p
  }
  names(max) <- model$user.levels[[p_max]]
  return(round(max, 3))
}

for (i in c("us1", countries_EU)) {
  print(i)
  print(largest_effect_amce(amce[[i]])) }

# conjoint_left_ag_b_binary_positive
decrit("conjoint_left_ag_b_binary", us1)
decrit("conjoint_left_ag_b_binary", us1, weight = F)
wtd.t.test(eu$conjoint_left_ag_b_binary[eu$country=='UK'], .5, weight = eu$weight_country[eu$country=='UK'])
wtd.t.test(eu$conjoint_left_ag_b_binary[eu$country=='ES'], .5, weight = eu$weight_country[eu$country=='ES'])

# Conjoint 5
same_reg_subsamples(dep.var = "conjoint_d", dep.var.caption = "Prefers the platform", covariates = c("branch_c_gcs"), 
                    data = all[all$conjoint_c_none == F & all$wave != "US2",], along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
                    filename = "conjoint_d", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)



##### Prioritization #####
(mean_points <- sort(setNames(sapply(variables_points_us, function(v) round(wtd.mean(all[[v]], na.rm = T, weights = all$weight), 1)), unname(policies.names.us[sub("points_(.*[0-9]).*", "\\1", variables_points_us), "US"])))) # GCS 9th, tax 4th, assembly 10, trillion inv 5, coal 13, ban ICE 15
(mean_points_us <- sort(setNames(sapply(variables_points_us, function(v) round(wtd.mean(us1[[v]], na.rm = T, weights = us1$weight), 1)), unname(policies.names.us[sub("points_(.*[0-9]).*", "\\1", variables_points_us), "US"])))) # GCS 9th, tax 4th, assembly 10, trillion inv 5, coal 13, ban ICE 15
(mean_points_uk <- sort(setNames(sapply(variables_points, function(v) round(wtd.mean(eu[[v]][eu$country == "UK"], na.rm = T, weights = eu$weight_country[eu$country == "UK"]), 1)), unname(policies.names.us[sub("points_(.*[0-9]).*", "\\1", variables_points), "UK"])))) # tax 5, GCS 8, ass 10, insul 7, LEZ 11, ban 13
(mean_points_de <- sort(setNames(sapply(variables_points, function(v) round(wtd.mean(eu[[v]][eu$country == "DE"], na.rm = T, weights = eu$weight_country[eu$country == "DE"]), 1)), unname(policies.names.us[sub("points_(.*[0-9]).*", "\\1", variables_points), "DE"])))) # GCS 1, tax 2, ass 8, insul 5, solar 7, ban 14
(mean_points_fr <- sort(setNames(sapply(variables_points, function(v) round(wtd.mean(eu[[v]][eu$country == "FR"], na.rm = T, weights = eu$weight_country[eu$country == "FR"]), 1)), unname(policies.names.us[sub("points_(.*[0-9]).*", "\\1", variables_points), "FR"])))) # insul 3, GCS 4, tax 6, ass 8, LEZ 13, ban 15
(mean_points_es <- sort(setNames(sapply(variables_points, function(v) round(wtd.mean(eu[[v]][eu$country == "ES"], na.rm = T, weights = eu$weight_country[eu$country == "ES"]), 1)), unname(policies.names.us[sub("points_(.*[0-9]).*", "\\1", variables_points), "ES"])))) # 100% renew 4, tax 5, GCS 8, insul 10, ass 11, ban 15
decrit("points_foreign1_gcs", data = all) # mean: 17.7 / median: 20
decrit("points_foreign1_gcs", data = eu) # mean: 19.75 / median: 19
decrit("points_foreign1_gcs", data = us1) # mean: 15.4 / median: 11
decrit("points_foreign1_gcs", data = eu, which = eu$country == 'DE') # mean: 23.3 / median: 20
decrit("points_foreign1_gcs", data = eu, which = eu$country == 'FR') # mean: 20.9 / median: 20
decrit("points_foreign1_gcs", data = eu, which = eu$country == 'ES') # mean: 15.7 / median: 13
decrit("points_foreign1_gcs", data = eu, which = eu$country == 'UK') # mean: 17 / median: 16
decrit("points_foreign2_tax_rich", data = us1) # mean: 20.6 / median: 18
decrit("points_foreign2_tax_rich", data = eu, which = eu$country == 'DE') # mean: 22.6 / median: 20
decrit("points_foreign2_tax_rich", data = eu, which = eu$country == 'FR') # mean: 20.1 / median: 18
decrit("points_foreign2_tax_rich", data = eu, which = eu$country == 'ES') # mean: 19.4 / median: 16
decrit("points_foreign2_tax_rich", data = eu, which = eu$country == 'UK') # mean: 19.6 / median: 16


##### Pros and cons #####
decrit(all$gcs_field_pro | all$gcs_field_con, all)
summary(lm(gcs_support ~ branch_gcs, us2, weights = weight))
summary(lm(nr_support ~ branch_gcs, us2, weights = weight))
desc_table(c("gcs_support", "gcs_support", "nr_support", "nr_support"), filename = "branch_gcs", data = us2, indep_vars = c("branch_gcs", covariates), indep_vars_included = list("branch_gcs", c("branch_gcs", covariates), "branch_gcs", c("branch_gcs", covariates)), mean_control = T, model.numbers = T, #!mean_above,
           dep.var.labels = c("Global Climate Scheme", "National Redistribution"), dep.var.caption = c("Support"), digits= 3, robust_SE = T, omit = c("Constant", "Race: Other"), mean_above = T, only_mean = F, keep = "branch_gcs", save_folder = "../tables/US2/", nolabel = F, 
           add_lines = list(c(18, "Includes controls &  & \\checkmark &  & \\checkmark \\\\")))


##### Second-order beliefs ##### 
decrit("gcs_belief", data = us1)
decrit("gcs_belief", data = eu)
decrit(eu$gcs_belief > 50, weights = eu$weight)
decrit(eu$gcs_belief < 76, data = eu) # 78%
decrit(us1$gcs_belief < 54, data = us1) # 53%


##### Universalism #####
# group_defended_agg, group_defended_agg2
decrit("group_defended_agg5", data = all)
decrit("group_defended_agg", data = all, which = all$vote != 0)
decrit("group_defended_agg", data = eu, which = eu$vote != 0)
decrit("group_defended_agg", data = us1, which = us1$vote != 0)
decrit("group_defended_agg", data = all, which = all$vote == -1)
decrit("group_defended_agg", data = eu, which = eu$vote == -1)
decrit("group_defended_agg", data = us1, which = us1$vote == -1)
# all/negotiation
decrit("negotiation", data = all)
wtd.mean(all$problem_climate, weights = all$weight)
wtd.mean(all$problem_poverty, weights = all$weight)
wtd.mean(all$problem_inequality, weights = all$weight)


##### Donation #####
decrit("donation", all)
same_reg_subsamples(dep.var = "donation", dep.var.caption = "Donation to poor people (in \\%)", covariates = c("branch_donation"), 
                    data = all, along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
                    filename = "donation", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)

same_reg_subsamples(dep.var = "donation", dep.var.caption = "Donation to poor people (in \\%)", covariates = c("branch_donation"), 
                    data = all, along = "continent", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
                    filename = "donation", folder = "../tables/continents/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)

same_reg_subsamples(dep.var = "donation", dep.var.caption = "Donation to poor people (in \\%)", covariates = c("branch_donation", "vote_factor", "branch_donation:vote_factor"), 
                    data = all, along = "continent", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
                    filename = "donation_covariates", folder = "../tables/continents/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)

same_reg_subsamples(dep.var = "donation", dep.var.caption = "Donation to poor people (in \\%)", covariates = c("branch_donation", "vote_factor", "branch_donation:vote_factor"), 
                    data = all, along = "continent", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE, keep = c("branch_donation"),
                    filename = "donation_interaction", folder = "../tables/continents/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)

desc_table(dep_vars = "donation", filename = "donation_interaction", data = list(all, us1, us1, eu), dep.var.labels = c("All", "US", "US", "Eu"), dep.var.caption = "Donation to poor people (in \\%)",
           indep_vars = c("branch_donation", "vote_Biden", "branch_donation:vote_Biden"), model.numbers = F, multicolumn = F, mean_above = F, 
           indep_vars_included = list(c(T,F,F), c(T,F,F), c(T,T,T), c(T,F,F)), weights = "weight", save_folder = "../tables/continents/", robust_SE = FALSE, omit = c("Constant", "^  Vote", "^  vote"))

  desc_table(dep_vars = "donation", filename = "donation_interaction", data = list(all, us1, us1, eu), dep.var.labels = c("All", "US", "US", "Eu"), dep.var.caption = "Donation to poor people (in \\%)",
           indep_vars = c("branch_donation", "vote_not_Biden", "branch_donation:vote_not_Biden"), model.numbers = F, multicolumn = F,  mean_above = F, 
           indep_vars_included = list(c(T,F,F), c(T,F,F), c(T,T,T), c(T,F,F)), weights = "weight", save_folder = "../tables/continents/", robust_SE = FALSE, omit = c("Constant", "^  Vote", "^  vote"))

same_reg_subsamples(dep.var = "donation_above_25 ", dep.var.caption = "More than 25\\% donated to poor people", covariates = c("branch_donation"), 
                    data = all, along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
                    filename = "donation_above_25 ", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)

same_reg_subsamples(dep.var = "donation_above_25 ", dep.var.caption = "More than 25\\% donated to poor people", covariates = c("branch_donation"), 
                    data = all, along = "continent", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
                    filename = "donation_above_25 ", folder = "../tables/continents/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)


##### Global millionaire tax #####
same_reg_subsamples(dep.var = "global_tax_sharing", dep.var.caption = "Prefers to share half of global tax with low-income countries", covariates = c("vote_factor"), 
                    data = all[all$country != "US",], along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
                    filename = "global_tax_sharing_vote", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)


##### Methods #####
decrit("survey_biased", data = all)


##### App Determinants #####
same_reg_subsamples(dep.var = "gcs_support", dep.var.caption = "\\makecell{Supports the Global Climate Scheme}", covariates = covariates, 
                    data_list = list(all, us, eu, d("DE"), d("FR"), d("UK"), d("ES")), dep_var_labels = c("All", "United States", "Europe", countries_names), 
                    data = all, along = "country_name", nolabel = F, include.total = T, mean_above = FALSE, only_mean = FALSE, mean_control = FALSE,
                    filename = "gcs_support", folder = "../tables/country_comparison/", digits= 3, model.numbers = F, logit = FALSE, robust_SE = T, print_regs = F, no.space = T)


##### App Representativeness #####
representativeness_table(c("US1", "US2", "Eu"), return_table = F, all = T) 
representativeness_table(countries_EU, return_table = F, all = T, weight_var = "weight_country") 


##### App Attrition analysis #####
desc_table(dep_vars = c("dropout", "dropout_late", "failed_test", "duration", "duration < 4"), weights = NULL, omit = c("Constant", "Race: Other", "vote3NA"),
           dep.var.labels = c("\\makecell{Dropped out}", "\\makecell{Dropped out\\\\after\\\\socio-eco}", "\\makecell{Failed\\\\attention test}", "\\makecell{Duration\\\\(in min)}", "\\makecell{Duration\\\\below\\\\4 min}"),
           filename = "attrition_analysis_vote", save_folder = "../tables/US1/", data = c(list(us1a), list(us1a), list(us1a[us1a$stayed == T,]), list(us1a[us1a$failed_test == F & us1a$stayed == T,]), list(us1a[us1a$failed_test == F & us1a$stayed == T,])), 
           indep_vars = c(quotas_us, "vote3")) 

desc_table(dep_vars = c("dropout", "dropout_late", "failed_test", "duration", "duration < 4"), weights = NULL, omit = c("Constant", "Race: Other", "vote3NA"),
           dep.var.labels = c("\\makecell{Dropped out}", "\\makecell{Dropped out\\\\after\\\\socio-eco}", "\\makecell{Failed\\\\attention test}", "\\makecell{Duration\\\\(in min)}", "\\makecell{Duration\\\\below\\\\4 min}"),
           filename = "attrition_analysis_vote", save_folder = "../tables/US2/", data = c(list(us2a), list(us2a), list(us2a[us2a$stayed == T,]), list(us2a[us2a$failed_test == F & us2a$stayed == T,]), list(us2a[us2a$failed_test == F & us2a$stayed == T,])), 
           indep_vars = c(quotas_us, "vote3")) 

desc_table(dep_vars = c("dropout", "dropout_late", "failed_test", "duration", "duration < 6"),weights = NULL, omit = c("Constant", "Race: Other", "factorNA"),
           dep.var.labels = c("\\makecell{Dropped out}", "\\makecell{Dropped out\\\\after\\\\socio-eco}", "\\makecell{Failed\\\\attention test}", "\\makecell{Duration\\\\(in min)}", "\\makecell{Duration\\\\below\\\\6 min}"),
           filename = "attrition_analysis_vote", save_folder = "../tables/EU/", data = c(list(eua), list(eua), list(eua[eua$stayed == T,]), list(eua[eua$failed_test == F & eua$stayed == T,]), list(eua[eua$failed_test == F & eua$stayed == T,])), 
           indep_vars = c(quotas_eu, "vote_factor")) 
# TODO complete, clean repo and comments, esp. paper/