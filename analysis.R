

################################################################################

#                                Settings

################################################################################

library(magrittr)
library(MASS)
library(plyr)
library(tidyverse)
library(stringr)
library(ggplot2) 
library(scales)
library(data.table)
library(caret)
library(randomForest)
library(klaR)
library(nnet)
library(pROC)
library(xtable)
library(broom)


## Set up an environment to keep all of our models and ROC curves - for tidyness
contextual_consent <- new.env()
## Note that not all functions use the seed (I believe this is the case for the 
## data partition functions, resulting in different train/test/validation groups,
## regardless of whether a seed is used or not).
myseed <- 1
set.seed(myseed)


## Set your working directory to wherever the repository is stored
## Also set up where you want the images saved to
# cc_dir <- "~/..."
# setwd(cc_dir)
# figures_path <- "~/..."

## Alternatively just set these as the current directory if it's executing 
## in the same directory as analysis.R 
cc_dir <- getwd()
figures_path <- paste(cc_dir, "/figs", sep = "")



## Load the process and functions scripts
source("r/process.R")
source("r/functions.R")

## 10-fold Cross Validation repeated 4 times
cv.ctrl <- trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary, savePredictions = TRUE, classProbs = TRUE)

## The metric for model evaluation
myMetric <- "ROC"

## The two formulas
simplified_formula <- is_shared ~ proportion_shared + proportion_shared_audience
formula <- is_shared ~ data_type + education + number_of_friends + proportion_shared + proportion_shared_audience





################################################################################
#                                        
#                     Load, Process and Split Data
#                                        
################################################################################


## Load and preprocess data
df <- cc_process(file_dir = cc_dir)




## We first need to create a list of participants with the proportion of items 
## shared, and then split approx. 60:40 on that list attempting to balance the 
## proportions. Then split again 50:50 on the smaller set, creating a 60:20:20 
## train/validation/test set with a balance of share proportions.

pcpts <- df %>% 
  group_by(user_id, is_shared) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  filter(is_shared == "Yes") %>%
  dplyr::select(user_id, shared = freq)

set.seed(myseed)
inTrain <- createDataPartition(y = pcpts$shared, p = .6, list = F) %>% pcpts[., ]
totalTest <- pcpts %>% filter(!(user_id %in% inTrain$user_id))
set.seed(myseed)
inTest <- createDataPartition(y = totalTest$shared, p = .5, list = F) %>% totalTest[., ]

training <- df %>% filter(user_id %in% inTrain$user_id)
testing <- df %>% filter(user_id %in% inTest$user_id)
validation <- df %>% filter(!(user_id %in% inTrain$user_id) & !(user_id %in% inTest$user_id))

rm(inTrain, totalTest, inTest, pcpts)



################################################################################
#                                        
#             Summary Statistics & Logistic Regression Model
#                                        
################################################################################


training %>% summarise(npcpt = length(unique(user_id)), ndata = n())
testing %>% summarise(npcpt = length(unique(user_id)), ndata = n())
validation %>% summarise(npcpt = length(unique(user_id)), ndata = n())



## Regression - Step 1: Contextual
## backward-step eliminated formula
logreg.formula = is_shared ~ data_type + audience_type + published_timeframe

logreg.raw <- glm(logreg.formula, family = "binomial", na.action = na.pass, data = training)

summary(logreg.raw)


logreg <- tidy(logreg.raw)
logreg$statistic <- NULL
names(logreg) <- c("var", "est", "err", "p")
logreg$exp <- exp(logreg$est)
logreg$sig <- lapply(logreg$p, add_signif)

logreg %>% select(-exp) %>% xtable(digits = c(1, 1, 2, 2, 3, 4))

RsqGLM(model = logreg.raw)



## Regression - Step 2: Contextual & Demographic
## backward-step eliminated formula
logreg.formula = is_shared ~ data_type + audience_type + published_timeframe + education + profile_visibility_setting + number_of_friends 

logreg.raw <- glm(logreg.formula, family = "binomial", na.action = na.pass, data = training)

summary(logreg.raw)


logreg <- tidy(logreg.raw)
logreg$statistic <- NULL
names(logreg) <- c("var", "est", "err", "p")
logreg$exp <- exp(logreg$est)
logreg$sig <- lapply(logreg$p, add_signif)

logreg %>% select(-exp) %>% xtable(digits = c(1, 1, 2, 2, 3, 4))

RsqGLM(model = logreg.raw)



## Regression - Step 3: Contextual & Demographic with Proportion Calculation
## backward-step eliminated formula
logreg.formula = is_shared ~ data_type + education + number_of_friends + proportion_shared + proportion_shared_audience

logreg.raw <- glm(logreg.formula, family = "binomial", na.action = na.pass, data = training)

summary(logreg.raw)


logreg <- tidy(logreg.raw)
logreg$statistic <- NULL
names(logreg) <- c("var", "est", "err", "p")
logreg$exp <- exp(logreg$est)
logreg$sig <- lapply(logreg$p, add_signif)

logreg %>% select(-exp) %>% xtable(digits = c(1, 1, 2, 2, 3, 4))

RsqGLM(model = logreg.raw)






################################################################################
#                                        
#                               Train Models
#                                        
################################################################################


## Train all the models and combinations
em <- evaluate_models(force_retrain = T)
em.t <- evaluate_models(use_threshold = T, force_retrain = T)
em.s <- evaluate_models(c("glm", "nnet", "nb"), use_simple_formula = T, force_retrain = T)
em.st <- evaluate_models(c("glm", "nnet", "nb"), use_simple_formula = T, use_threshold = T, force_retrain = T)


evaluate_models_combination <- em %>%
  rbind(em.t) %>%
  rbind(em.s) %>%
  rbind(em.st) 


roc_curves <- ls(contextual_consent)
roc_curves <- roc_curves[roc_curves %like% "roc_"]
model.roc <- data.table(method = character(), sensitivities = numeric(), specificities = numeric(), formula = character(), cost = character(), auc = numeric())

for (i in roc_curves) {
  
  model.roc <- eval(parse(text = paste0("contextual_consent$", i))) %>% strip.roc(i) %>% rbind(model.roc, .) 
}


# evaluate_models_combination[formula == "Simple", method := "minimal"]
# model.roc[formula == "Simple", method := 'minimal']






################################################################################
#
#                         Results Table of All Models
#
################################################################################


evaluate_models_combination %>%
  filter(cost == "No Adjustment" & formula == "Full") %>%
  select(-burden, -formula, -cost) %>%
  mutate(method = get_full_method_name(method)) %>%
  arrange(desc(auc), desc(fscore)) %T>% 
  View %>% 
  xtable(digits = 3)





################################################################################
#
#                         ROC Curve of All Models
#
################################################################################


temp_roc <- model.roc %>%
  arrange(auc) %>%
  mutate(method = paste0(get_full_method_name(method), ":  ", sprintf("%.2f", auc), "      ")) %>%
  mutate(method = factor(method, levels = rev(unique(.$method[order(.$auc)])), ordered = T)) %>%
  select(-auc) %>%
  filter(cost == "No Adjustment" & formula == "Full")

test_roc <- ggplot(NULL, aes(x = rev(1 - temp_roc$specificities), y = rev(temp_roc$sensitivities))) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), alpha = 0.5) + 
  geom_step(aes(color = rev(temp_roc$method))) +
  scale_x_continuous(name = "False Positive Rate (1 - Specificity)", limits = c(0,1), breaks = seq(0, 1, 0.2), expand = c(0.001,0.001)) + 
  scale_y_continuous(name = "True Positive Rate (Sensitivity)", limits = c(0,1), breaks = seq(0, 1, 0.2), expand = c(0.001, 0.001)) +
  scale_color_discrete(name="Model AUC:\n") +
  guides(color = guide_legend(ncol = 2, byrow=TRUE)) +
  theme_bw() + 
  theme(axis.ticks = element_line(color = "grey80"), plot.title = element_text(hjust = 0.5), legend.title.align = 0.5, legend.position = "bottom", legend.direction = "vertical") + 
  coord_equal() +
  ggtitle("ROC Curve: \nAll models on the Test Set\n")


test_roc

# Write to files
ggsave("test_roc.tiff", test_roc, path = figures_path, width = 4.5, height = 7, units = 'in')





################################################################################
#
#       Sensitivity / Specificity / Burden / Data Minimisation Trade-Offs
#
################################################################################


tp_vs_fp <- evaluate_models_combination %>%
  filter(formula == "Full") %>%
  ggplot(aes(1 - specificity, sensitivity)) +
  geom_point(aes(color = get_full_method_name(method), shape = get_full_method_name(method))) +
  scale_shape_manual(name="Model:", values=1:7) +
  scale_color_discrete(name="Model:") +
  scale_x_continuous("False Positive Rate (1 - Specificity)", limits = c(0,1), breaks = seq(0, 1, by = .25), labels = percent, expand = c(0.001, 0.001)) +
  scale_y_continuous("True Positive Rate (Sensitivity)", limits = c(0,1), labels = percent, expand = c(0.001, 0.001)) +
  theme_bw() +
  theme(panel.spacing = unit(2, "lines"), 
        plot.margin = unit(c(5.5, 55.5, 5.5, 5.5), "points"), 
        plot.title = element_text(hjust = 0.5), 
        legend.title.align = 0.5,
        legend.key.size = unit(1.5, 'lines')) +
  facet_grid( ~ cost) +
  ggtitle("Trade-off between data leaks and sensitivity with a fully automated model")

tp_vs_fp
ggsave("tp_vs_fp.tiff", tp_vs_fp, path = figures_path, width = 10, height = 4, units = 'in')


tp_vs_burden <- evaluate_models_combination %>%
  filter(formula == "Full") %>%
  ggplot(aes(burden, sensitivity)) +
  geom_point(aes(color = get_full_method_name(method), shape = get_full_method_name(method))) +
  scale_shape_manual(name="Model:", values=1:7) +
  scale_color_discrete(name="Model:") +
  scale_x_continuous("Participant Burden", limits = c(0,1), breaks = seq(0, 1, by = .25), labels = percent, expand = c(0.001, 0.001)) +
  scale_y_continuous("Sensitivity", limits = c(0,1), labels = percent, expand = c(0.001, 0.001)) +
  theme_bw() +
  theme(panel.spacing = unit(2, "lines"), 
        plot.margin = unit(c(5.5, 55.5, 5.5, 5.5), "points"), 
        plot.title = element_text(hjust = 0.5), 
        legend.title.align = 0.5,
        legend.key.size = unit(1.5, 'lines')) +
  facet_grid( ~ cost) +
  ggtitle("True Positives and Participant Burden when no data leaked (False Positives = 0%)")

tp_vs_burden
ggsave("tp_vs_burden.tiff", tp_vs_burden, path = figures_path, width = 10, height = 4, units = 'in')





evaluate_models_combination %>%
  filter(method == "nb") %>%
  select(cost, formula, accuracy, precision, sensitivity, specificity, fscore, auc) %>% 
  arrange(cost, formula) %T>% 
  View %>% 
  xtable(digits = 3)







################################################################################
#
#                         Validation results
#
################################################################################


contextual_consent$mdl_nb %>% 
  predict_adjusted_threshhold(., validation, get_threshold(.)) %>% 
  confusionMatrix(validation$is_shared, positive = "Yes")





################################################################################
#
#                 Reasons Analysis
#
################################################################################



context <- fread("csv/cc_context.csv", stringsAsFactors = F)
response <- fread("csv/cc_response.csv", stringsAsFactors = F)


audience_share_proportions_per_pcpt <-df[, head(.SD, 1), by = c("user_id", "audience_type")] %>% 
  select(user_id, audience_type, proportion_shared_audience)


context <- response %>%
  select(user_id, user_question_number, data_type, audience_type, data_visibility_setting) %>%
  left_join(context, ., by = c("user_id", "user_question_number")) %>%
  left_join(audience_share_proportions_per_pcpt, by = c("user_id", "audience_type"))


context %>%
  filter(is_shared == F) %>%
  gather("tag", "val", 6:15) %>% 
  group_by(tag, val) %>%
  summarise(num = n()) %>%
  group_by(tag) %>%
  mutate(total = sum(num)) %>%
  filter(val == T) %>%
  mutate(perc = num / total) %>%
  ggplot(aes(reorder(tag, -num), perc, fill = tag)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  scale_y_continuous(name = "Percentage of Responses Where Consent Not Given", labels = percent) +
  scale_x_discrete(name = "Participant-Selected Reason")


context %>%
  filter(audience_type == "Researcher" ) %>%
  filter(is_shared == F) %>%
  gather("tag", "val", 6:15) %>% 
  group_by(tag, val) %>%
  summarise(num = n()) %>%
  group_by(tag) %>%
  mutate(total = sum(num)) %>%
  filter(val == T) %>%
  mutate(perc = num / total) %>%
  ggplot(aes(reorder(tag, -num), perc, fill = tag)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  scale_y_continuous(name = "Percentage of Responses Where Consent Not Given", labels = percent) +
  scale_x_discrete(name = "Participant-Selected Reason")


context %>%
  filter(audience_type == "Researcher" & proportion_shared_audience > .5) %>%
  filter(is_shared == F) %>%
  gather("tag", "val", 6:15) %>% 
  group_by(tag, val) %>%
  summarise(num = n()) %>%
  group_by(tag) %>%
  mutate(total = sum(num)) %>%
  filter(val == T) %>%
  mutate(perc = num / total) %>%
  ggplot(aes(reorder(tag, -num), perc, fill = tag)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  scale_y_continuous(name = "Percentage of Responses Where Consent Not Given", labels = percent) +
  scale_x_discrete(name = "Participant-Selected Reason")




## Of data that is publicly available, what % should not be shared with researchers?
response %>% 
  filter(audience_type == "Researcher") %>%
  filter(data_visibility_setting != "") %>% 
  filter(data_visibility_setting %in% c("everyone", "Public")) %>% 
  group_by(is_shared) %>% 
  summarise(num = n()) %>%
  mutate(num_total = sum(num)) %>%
  filter(is_shared == F) %>% 
  summarise(proportion_available = num / num_total)



  






################################################################################
#
#                 Ideal Share Proportion Number Analysis
#
################################################################################


freq_analysis <- function(i) {
  val <- validation %>% rbind(testing) %>% calc_proportion_shared(num = i)
  test_model(contextual_consent$mdl_glm, val) %>% 
    mutate(num_rows_used = i) %>%
    select(num_rows_used, accuracy, precision, sensitivity, specificity, fscore, auc)
}

freqs <- seq(1, 80, 1)
freq_analysis_results <- ldply(freqs, freq_analysis)


freq_analysis_results %>%
  ggplot(aes(num_rows_used, auc)) +
  geom_point(alpha = .2) +
  geom_smooth(method = 'loess', span = .5, se = F) +
  scale_x_continuous(name = "\nNumber of Responses Used to Calculate Share Proportions", limits = c(0, 80), breaks = seq(0, 80, by = 20), expand = c(0.001, 0.001)) +
  scale_y_continuous(name = "Area Under the ROC Curve\n", limits = c(0, 1), breaks = seq(0, 1, by = .2), expand = c(0.001, 0.001)) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), legend.title.align = 0.5) +
  ggtitle("How the Area under the ROC Curve Changes\nDepending on the Number of Responses\nUsed to Calculate Participant Share Proportions\n")


## Write to files
ggsave("proportion_shared_analysis.png", path = figures_path, width = 5, height = 5, units = 'in')





