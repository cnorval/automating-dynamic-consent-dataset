
# This function reads in the CSVs, handles missing data, handles factors, 
# conducts pre-processing on the data, joins the tables together, and returns a 
# unified data table ready for the analysis of the paper.
# 
# file_dir:       The file location of the analysis scripts and csv directory
cc_process <- function(file_dir) {
  
  
  # Safely handle directories whether they end with / or not
  if (substring(file_dir, nchar(file_dir)) != "/") {
    file_dir <- paste0(file_dir, "/")
  }
  
  
  
  
  
  
  ##############################################################################
  #                                        
  #                               User Data
  #                                        
  ##############################################################################
  
  
  user <- fread(paste0(file_dir, "csv/cc_user.csv"), stringsAsFactors = F)
  
  
  # Very few people over 55, so group them all together
  user[age_category == "55 - 64", age_category := "55+"]
  user[age_category == "65+", age_category := "55+"]
  
  
  # Populate missing Age data with mode
  mode <- user %>% 
    group_by(age_category) %>% 
    summarise(num = length(unique(user_id))) %>% 
    arrange(desc(num)) %>% head(1) %>% 
    .$age_category %>% 
    as.character()
  
  user[is.na(age_category), age_category := mode]
  
  
  # Add an under 35 flag
  user$under35 <- F
  user[age_category %in% c("18 - 24", "25 - 34"), under35 := T]

  
  
  # Populate missing Gender data with mode
  mode <- user %>% 
    group_by(gender) %>% 
    summarise(num = length(unique(user_id))) %>% 
    arrange(desc(num)) %>% head(1) %>% 
    .$gender %>% as.character()
  
  user[!(gender %in% c("Male", "Female")), gender := mode]
  
  
  # Populate missing Education data with mode
  mode <- user %>% 
    group_by(education) %>% 
    summarise(num = length(unique(user_id))) %>% 
    arrange(desc(num)) %>% head(1) %>% 
    .$education %>% 
    as.character()
  
  user[is.na(education) | education == "", education := mode]
  
  
  # Populate missing Account Privacy data with mode
  user[is.na(profile_visibility_setting), profile_visibility_setting := "Empty"]
  
  
  # Populate missing Number of Friends data - Calculate the mean and round up to nearest 10
  user[is.na(number_of_friends), number_of_friends := round_any(mean(user$number_of_friends, na.rm = T), 10)]
  
  
  # Change variables to factors
  user$age_category <- factor(user$age_category, levels = c("18 - 24", "25 - 34", "35 - 44", "45 - 54", "55+"), ordered = F)
  user$gender <- as.factor(user$gender)
  user$education <- factor(user$education, levels = c("Other", "High school", "Undergraduate degree", "Postgraduate degree"), ordered = F)
  user$profile_visibility_setting <- as.factor(user$profile_visibility_setting)
  
  
  
  
  
  
  ##############################################################################
  #                                        
  #                               Response Data
  #                                        
  ##############################################################################
  
  
  response <- fread(paste0(file_dir, "csv/cc_response.csv"), stringsAsFactors = F)
  
  
  # Change the factor levels of is_shared to prevent confusion & allow caret to work
  response$is_shared <- as.character(response$is_shared)
  response[is_shared == 'TRUE', is_shared := "Yes"]
  response[is_shared == 'FALSE', is_shared := "No"]
  

  # Fill missing data_visibility_setting entries as "Empty"
  response[is.na(data_visibility_setting), data_visibility_setting := "Empty"]
  
  
  # Change variables to factors
  response$is_shared <- as.factor(response$is_shared)
  response$data_contains_img <- as.factor(response$data_contains_img)
  response$data_contains_loc <- as.factor(response$data_contains_loc)
  response$published_at_weekend <- as.factor(response$published_at_weekend)
  response$published_at_night <- as.factor(response$published_at_night)

  
  
  
  
  
  ##############################################################################
  #                                        
  #                               Context Data
  #                                        
  ##############################################################################
  
  
  context <- fread(paste0(file_dir, "csv/cc_context.csv"), stringsAsFactors = F) %>%
    select(-is_shared)
  
  
  # Change variables to factors
  context$is_health_related <- as.factor(context$is_health_related)
  
  
  
  
  
  
  ##############################################################################
  #                                        
  #         Calculate misc. proportions, join datasets, and Return
  #                                        
  ##############################################################################
  
  
  data <- response %>%
    left_join(context, by = c("user_id", "user_question_number")) %>%
    left_join(user, by = "user_id") %>%
    as.data.table()
  
  
  # Calculate proportion_health_related
  data <- context %>%
    group_by(user_id, is_health_related) %>%
    summarise(health_count = n()) %>%
    group_by(user_id) %>%
    mutate(total_count = sum(health_count)) %>%
    filter(is_health_related == T) %>%
    summarise(proportion_health_related = health_count / total_count) %>%
    left_join(data, ., by="user_id") %>%
    as.data.table()
  
  data[is.na(proportion_health_related), proportion_health_related := 0]
  
  
  # Share freq
  data <- response %>%
    group_by(user_id, is_shared) %>%
    summarise(share_count = n()) %>%
    group_by(user_id) %>%
    mutate(total_count = sum(share_count)) %>%
    filter(is_shared == "Yes") %>%
    summarise(proportion_shared = share_count / total_count) %>%
    left_join(data, ., by="user_id") %>%
    as.data.table()
  
  
  data[is.na(proportion_shared), proportion_shared := 0]
  
  
  # Share freq audience
  data <- response %>%
    group_by(user_id, audience_type, is_shared) %>%
    summarise(share_count = n()) %>%
    group_by(user_id, audience_type) %>%
    mutate(total_count = sum(share_count)) %>%
    filter(is_shared == "Yes") %>%
    summarise(proportion_shared_audience = share_count / total_count) %>%
    left_join(data, ., by=c("user_id", "audience_type")) %>%
    as.data.table()
  
  data[is.na(proportion_shared_audience), proportion_shared_audience := 0]
  
  
  
  data <- calc_proportion_shared(data, frac = .2)
  
  return(data)
}

