



################################################################################
#                                        
#                                 Functions
#                                        
################################################################################


# Train one or more models, passing information about formulas and cost-sensitive techniques down. Print off performance metrics of these models.
# 
# models:               A string or vector containing the caret method(s) to train
# train_set:            The dataset to be used to train the models
# test_set:             The dataset to be used to test the models
# use_simple_formula:   True if the simple formula should be used (only using share proportion)
# use_weights:          True if the model should train using class weights (75:25)
# use_threshold:        True if the model is evaluated using an adjusted threshold (sensitivity at ~ 95%)
# custom_params:        A string to append to the suffix of your variable names. Use this e.g. "v" for validation testing where you don't want to overwrite the original model
# force_retrain:        True if the model should be trained even if it already exists
# verbose:              True if the function should print out what model is being trained
evaluate_models <- function(models, train_set = training, test_set = testing, use_simple_formula = F, use_weights = F, use_threshold = F, custom_params, force_retrain = F, verbose = T) {
  
  # Defaults
  intl_threshold <- 0.5
  intl_formula <- is_shared ~ data_type + audience_type + published_timeframe + education + proportion_shared + proportion_shared_audience
  intl_weights <- NULL
  intl_trControl <- trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary, savePredictions = TRUE, classProbs = TRUE)
  intl_metric <- "ROC"
  
  intl_model_table <- data.table(method = character(), accuracy = numeric(), precision = numeric(), sensitivity = numeric(), specificity = numeric(), fscore = numeric(), auc = numeric(), burden = numeric())
  
  
  if (missing(models)) {
    
    models <- c("glm", "knn", "rf", "svmRadial", "nb", "nnet")
  }
  
  
  # Which formula should be used?
  if (use_simple_formula) {
    
    intl_formula <- is_shared ~ proportion_shared_audience
  }
  
  # Should weights be used?
  if (use_weights) {
    intl_weights <- ifelse(training$is_shared == "No",
                           (1/table(training$is_shared)[1]) * 0.75,
                           (1/table(training$is_shared)[2]) * 0.25)
    
    weight_compatible_models <- c('vglmAdjCat', 'treebag', 'bagFDA', 'bagEarth', 'bagEarthGCV', 'bayesglm', 'gamboost', 'glmboost', 
                                  'blackboost', 'C5.0', 'rpart', 'rpart1SE', 'rpart2', 'rpartScore', 'chaid', 'cforest', 'ctree', 
                                  'ctree2', 'vglmContRatio', 'C5.0Cost', 'rpartCost', 'vglmCumulative', 'deepboost', 'xgbTree', 
                                  'fda', 'glm', 'glmStepAIC', 'lm', 'lmStepAIC', 'avNNet', 'earth', 'gcvEarth', 'glm.nb', 'nnet', 
                                  'pcaNNet', 'polr', 'pda', 'pda2', 'multinom', 'ppr', 'ranger', 'rlm', 'C5.0Rules', 'C5.0Tree', 'gbm', 'evtree')
    
    models <- models[models %in% weight_compatible_models]
  } 
  
  
  for (i in models) {
    
    # Create variable name based on model and parameters
    suffix <- ""
    if (use_simple_formula) suffix <- paste0(suffix, "s")
    if (use_weights) suffix <- paste0(suffix, "w")
    if(!missing(custom_params)) suffix <- paste0(suffix, custom_params)
    if (suffix != "") suffix <- paste0("_", suffix)
    
    intl_model_var <- paste0("mdl_", i, suffix)
    
    
    if (exists(intl_model_var, envir = contextual_consent) & !force_retrain) {
      if (verbose) print(paste(intl_model_var, "exists already"))
      # The model exists, so copy it locally
      intl_model <- eval(parse(text = paste0("contextual_consent$", intl_model_var)))
      
    } else {
      if (verbose) print(paste("Training", intl_model_var))
      # Don't print the garbage (e.g. nnet's convergence print lines)
      garbage <- capture.output(intl_model <- train(intl_formula, data = train_set, method = i, metric = intl_metric, weights = intl_weights, trControl = intl_trControl))
      intl_roc <- predict(intl_model, newdata = test_set, type = "prob") %>% roc(predictor = .$Yes, response = test_set$is_shared, levels = rev(levels(test_set$is_shared))) 
      
      # Assign globally so we can access it again
      assign(intl_model_var, intl_model, pos = contextual_consent)
      assign(paste0("roc_", i, suffix), intl_roc, pos = contextual_consent)
    } 
    
    if (use_threshold) {
      # Find the threshold which corresponds to .95 specificity
      intl_threshold <- get_threshold(intl_model, .95)
    }
    
    intl_model_table <- intl_model_table %>% rbind(test_model(intl_model, local_testing = test_set, intl_threshold))
  }
  
  if (nrow(intl_model_table) > 0) {
    intl_model_table$formula <- "Full"
    if (use_simple_formula) intl_model_table$formula <- "Simple"
    
    intl_model_table$cost <- "No Adjustment"
    if (use_weights) intl_model_table$cost <- "Weighted Adjustment"
    if (use_threshold) intl_model_table$cost <- "Threshold Adjustment"
    
    if (use_weights & use_threshold) intl_model_table$cost <- "Weights & Threshold"
  }
  
  return(intl_model_table)
}




add_signif <- function(signif) {
  
  to_add <- ""
  if (signif < .05) to_add <- paste0(to_add, "*")
  if (signif < .01) to_add <- paste0(to_add, "*")
  if (signif < .001) to_add <- paste0(to_add, "*")
  
  return(to_add)
}




# Adjust threshold
get_threshold <- function(model, specificity_cutoff = .95) {
  
  # Get only the predictions for the best tuning parameters, and generate a ROC curve
  pred <- inner_join(model$pred, model$bestTune, by = names(model$bestTune), all.y = T)
  roc_score <- roc(predictor = pred$Yes, response = pred$obs)
  
  # Cut the ROC curve data to get the first threshold above the cutoff
  cutoff <- data.frame(sensitivities = roc_score$sensitivities, specificities = roc_score$specificities, thresholds = roc_score$thresholds) %>%
    arrange(specificities) %>%
    filter(specificities - specificity_cutoff >= 0 & is.finite(thresholds)) %>%
    arrange(thresholds) %>%
    head(1)
  
  if (nrow(cutoff) > 0) { 
    return(cutoff$thresholds)
  } else {
    return(.999)
  }
}


# Predict using the threshold as a cut-off
# 
# model:      The model you wish to use to predict
# newdata:    The dataset you wish to use to predict
# threshold:  What probability cutoff should be used? 
predict_adjusted_threshhold <- function(model, newdata, threshold = .5) {
  
  probsTest <- predict(model, newdata = newdata, type = "prob")
  factor( ifelse(probsTest[, "Yes"] > threshold, "Yes", "No") )
}




test_model <- function(model, local_testing = testing, threshold = .5) {
  
  roc_score <- predict(model, newdata = local_testing, type = "prob") %>% roc(predictor = .$Yes, response = local_testing$is_shared, levels = rev(levels(local_testing$is_shared)))
  
  model.pred <- predict_adjusted_threshhold(model, local_testing, threshold) %>% confusionMatrix(local_testing$is_shared, positive = "Yes")
  
  data.table(method = model$method, 
             accuracy = model.pred$overall['Accuracy'], 
             precision = model.pred$byClass['Precision'], 
             sensitivity = model.pred$byClass['Sensitivity'], 
             specificity = model.pred$byClass['Specificity'], 
             burden = model.pred$byClass['Detection Prevalence'],
             fscore = model.pred$byClass['F1'], 
             auc = roc_score$auc[1]) %>% return()
}




# This function calculates share proportion values for the test and validation 
# datasets: partitioning the data, calculating on one of the partitions, then 
# discarding it. The value is joined to the second partition and returned.
# 
# data:             The dataset to calculate share prop for
# frac:             The fraction of data that should be used to calculate share prop
# num:              The number of responses that should be used to calulate share prop
calc_proportion_shared <- function(data, frac = .5, num = NULL) {
  
  suppressWarnings(data$proportion_shared <- NULL)
  suppressWarnings(data$proportion_shared_audience <- NULL)
  data$row_id <- seq(1, nrow(data))
  
  
  if (!is.null(num)) {
    
    # Subset to only include those with num + 10 responses
    set.seed(myseed)
    data_split_calc <- data %>% 
      group_by(user_id) %>% 
      mutate(num_rows = n()) %>% 
      filter(num_rows >= num + 10) %>% 
      group_by(user_id) %>% 
      sample_n(num) %>% 
      select(-num_rows)
  } else {
    
    set.seed(myseed)
    data_split_calc <- data %>% 
      group_by(user_id, data_type, audience_type) %>% 
      sample_frac(frac)
  }
  
  
  data_split_keep <- data %>% filter(!(row_id %in% data_split_calc$row_id))
  
  data_split_calc_full <- data_split_calc %>%
    group_by(user_id, is_shared) %>%
    summarise(share_count = n()) %>%
    group_by(user_id) %>%
    mutate(total_count = sum(share_count)) %>%
    filter(is_shared == "Yes") %>%
    summarise(proportion_shared = share_count / total_count) 
  
  data_split_calc_aud <- data_split_calc %>%
    group_by(user_id, audience_type, is_shared) %>%
    summarise(share_count = n()) %>%
    group_by(user_id, audience_type) %>%
    mutate(total_count = sum(share_count)) %>%
    filter(is_shared == "Yes") %>%
    summarise(proportion_shared_audience = share_count / total_count) 
  
  data <- data_split_keep %>%
    left_join(data_split_calc_full, by="user_id") %>%
    left_join(data_split_calc_aud, by=c("user_id", "audience_type")) %>%
    select(-row_id) %>%
    as.data.table()
  
  data[is.na(proportion_shared), proportion_shared := 0]
  data[is.na(proportion_shared_audience), proportion_shared_audience := 0]
  
  return(data)
}



# https://modtools.wordpress.com/2014/10/30/rsqglm/
RsqGLM <- function(obs = NULL, pred = NULL, model = NULL) {
  # version 1.2 (3 Jan 2015)
  
  model.provided <- ifelse(is.null(model), FALSE, TRUE)
  
  if (model.provided) {
    if (!("glm" %in% class(model))) stop ("'model' must be of class 'glm'.")
    if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
    if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
    obs <- model$y
    pred <- model$fitted.values
    
  } else { # if model not provided
    if (is.null(obs) | is.null(pred)) stop ("You must provide either 'obs' and 'pred', or a 'model' object of class 'glm'")
    if (length(obs) != length(pred)) stop ("'obs' and 'pred' must be of the same length (and in the same order).")
    if (!(obs %in% c(0, 1)) | pred < 0 | pred > 1) stop ("Sorry, 'obs' and 'pred' options currently only implemented for binomial GLMs (binary response variable with values 0 or 1) with logit link.")
    logit <- log(pred / (1 - pred))
    model <- glm(obs ~ logit, family = "binomial")
  }
  
  null.mod <- glm(obs ~ 1, family = family(model))
  loglike.M <- as.numeric(logLik(model))
  loglike.0 <- as.numeric(logLik(null.mod))
  N <- length(obs)
  
  # based on Nagelkerke 1991:
  CoxSnell <- 1 - exp(-(2 / N) * (loglike.M - loglike.0))
  Nagelkerke <- CoxSnell / (1 - exp((2 * N ^ (-1)) * loglike.0))
  
  # based on Allison 2014:
  McFadden <- 1 - (loglike.M / loglike.0)
  Tjur <- mean(pred[obs == 1]) - mean(pred[obs == 0])
  sqPearson <- cor(obs, pred) ^ 2
  
  return(list(CoxSnell = CoxSnell, Nagelkerke = Nagelkerke, McFadden = McFadden, Tjur = Tjur, sqPearson = sqPearson))
}



strip.roc <- function(roc, var_name) {
  
  new.roc <- data.table(specificities = roc$specificities, sensitivities = roc$sensitivities)
  
  new.roc$formula <- "Full"
  new.roc$cost <- "No Adjustment"
  split_var_name <- strsplit(var_name, "_")[[1]]
  
  if (length(split_var_name > 2)) {
    params <- split_var_name[3]
    if (params %like% 's') new.roc$formula <- "Simple"
    if (params %like% 'w') new.roc$cost <- "Weighted Adjustment"
    if (params %like% 't') new.roc$cost <- "Threshold Adjustment"
    if (params %like% 'w' & params %like% 't') new.roc$cost <- "Weight & Threshold Adjustment"
  }

  if (!is.na(split_var_name[2])) {
    new.roc$method <- split_var_name[2]
  } else {
    new.roc$method <- split_var_name[1]
  }
  new.roc$auc <- as.numeric(roc$auc)
  return(new.roc)
}



ggroc <- function(roc, showAUC = TRUE, interval = 0.2, breaks = seq(0, 1, interval)){
  require(pROC)
  if(class(roc) != "roc")
    simpleError("Please provide roc object from pROC package")
  
  ggplot(NULL, aes(x = rev(roc$specificities), y = rev(roc$sensitivities))) +
    geom_segment(aes(x = 0, y = 1, xend = 1,yend = 0), alpha = 0.5) + 
    geom_step(color = "#0B87DA") +
    scale_x_reverse(name = "Specificity",limits = c(1,0), breaks = breaks, expand = c(0.001,0.001)) + 
    scale_y_continuous(name = "Sensitivity", limits = c(0,1), breaks = breaks, expand = c(0.001, 0.001)) +
    theme_bw() + 
    theme(axis.ticks = element_line(color = "grey80"), plot.title = element_text(hjust = 0.5)) +
    coord_equal() + 
    annotate("text", x = interval/2 +.1, y = interval/2, vjust = 0, label = paste("AUC =",sprintf("%.3f",roc$auc)))
}



get_full_method_name <- function(method){
  
  method[method == "glm"] <- "Logistic Regression"
  method[method == "knn"] <- "k-Nearest Neighbour"
  method[method == "rf"] <- "Random Forest"
  method[method == "svmRadial"] <- "Support Vector Machine"
  method[method == "nb"] <- "Na\u{EF}ve Bayes"
  method[method == "nnet"] <- "Neural Network"
  method[method == "minimal"] <- "Minimal Model"
  
  method
}



