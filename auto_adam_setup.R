#' Algorithm: Create imputation records for missing visits
#' 
#' Create imputation records for missing visits based on visit mapping and observed data
#' 
#' @param visit_map named vector, Visit mapping defined in the spec
#' @param map_name String, Name of the visit number variable in visit_map (default: 'VISITNUM.res')
#' @param map_value String, Name of the visit variable in visit_map (default: 'VISIT.res')
#' @param paramcd_name String, Name of the parameter code variable in visit_map (default: 'PARAMCD.res')
#' @param paramcd Vector, Parameter codes to be imputed
#' @param post_baseline_patients Vector, List of subjects to be imputed
#' @param observed_df Dataframe, Observed data containing visit and parameter code variables
#' @param retain_value String, Method to retain values from observed data ('LAST' or 'FIRST', default: 'LAST')
#' @param join_id Vector, List of variables to join observed data and imputation records (default: c('USUBJID.res', 'PARAMCD.res'))
#' @return A dataframe of imputation records for missing visits
#' @examples
#'  visit_map <- c('Baseline' = 0, 'Week 1' = 7, 'Week 2' = 14, 'Week 4' = 28)
#'  paramcd <- c('PARAM1', 'PARAM2')
#'  post_baseline_patients <- c('SUBJ001', 'SUBJ002')
#'  observed_df <- data.frame(USUBJID.res = c('SUBJ001', 'SUBJ001', 'SUBJ002'),
#'                           VISITNUM.res = c(0, 7, 0),
#'                           VISIT.res = c('Baseline', 'Week 1', 'Baseline'),
#'                           PARAMCD.res = c('PARAM1', 'PARAM1', 'PARAM2'),
#'                           AVAL.res = c(10, 15, 20))
#'  alg_create_imp_df(visit_map, paramcd = paramcd, post_baseline_patients = post_baseline_patients, observed_df = observed_df)
alg_create_imp_df <- function(visit_vec, name = 'VISIT.res', 
                              paramcd_name = 'PARAMCD.res', paramcd, 
                              post_baseline_patients, observed_df, 
                              retain_value = 'LAST', join_id = c('USUBJID.res', 'PARAMCD.res')){
  # Pre processing
  retain_value <- toupper(retain_value)
  
  # Step1: Full visit for each subject
  imp_full_record <- as.data.frame(sapply(visit_vec, function(x) rep(x, length(post_baseline_patients))))
  imp_full_record$USUBJID.res <- post_baseline_patients
  imp_full_record <- pivot_longer(imp_full_record, names(imp_full_record)[-length(names(imp_full_record))], values_to = name, names_to = 'name') %>% select(-name)
  #imp_full_record$VISITNUM.res <- as.numeric(imp_full_record$VISITNUM.res)
  imp_full_record[paramcd] <- NA
  imp_full_record <- pivot_longer(imp_full_record, all_of(paramcd), values_to = "value", names_to = 'PARAMCD.res') %>% select(-value) 
  
  #Step3: select imputation records
  df_impute <- anti_join(imp_full_record, 
                         observed_df %>% select(names(imp_full_record)), 
                         by = names(imp_full_record)) 
  
  if (retain_value == 'LAST'){
    df_impute_fillin <- observed_df %>% 
      group_by(across(all_of(join_id))) %>%
      slice_tail(n = 1) %>%
      ungroup() %>%
      select(-names(df_impute)[which(names(df_impute) %in% names(observed_df) & !names(df_impute) %in% join_id)]) %>% #remove columns which share the same name between 2 df but not used in join
      right_join(df_impute, by = join_id) 
  } else if (retain_value == 'FIRST'){
    df_res_impute_fillin <- observed_df %>% 
      group_by(across(all_of(join_id))) %>%
      slice(n = 1) %>%
      ungroup() %>%
      select(-names(df_impute)[which(names(df_impute) %in% names(observed_df) & !names(df_impute) %in% join_id)]) %>% #remove columns which share the same name between 2 df but not used in join
      right_join(df_impute, by = join_id) 
  } else {
    stop("retain_value should be either 'LAST' or 'FIRST'")
  }
  return(df_impute_fillin)
} 


alg_time <- function(date_var){
  #' 
  
  if (all(!is.na(ymd_hms(date_var, quiet = T)))) {
    log <- 'Date time format: YMD_HMS'
  } else if ((all(!is.na(ymd_hms(date_var, quiet = T, truncated = 3))))){
    log <- 'Date time format: YMD_HMS, Time is missing for some records'
  }
  temp <- strsplit(date_var, 'T')
  res <- sapply(temp, function (x) ifelse(length(x) > 1, x[[2]], NA))
  return(list(res, log))
}

alg_date <- function(date_var){
  #' Algorithm: Convert date variable to Date format
  #' 
  #' Returns variable with Date format
  #' 
  #' Component module for auto_var_process()
  #' Need to be updated, now it's equivalent to as.Date()
  #' @param date_var Vector, Variable to be converted
  #' @return A list of two elements: (1) converted date variable, (2) log of conversion
  #' @examples
  #'   alg_date(c('2020-01-01', '2020-02-01'))

  if (all(is.na(date_var))){
    log <- NA
    return(list(NA, log))
  } else if (is(date_var, 'Date')){
    log <-NA
    return(list(date_var, log))
  } else if (is(date_var, 'POSIXct')){
    log <- NA
    return(list(date_var, log))
  } else if (is(date_var, 'character')){
    date_temp <- as.Date(date_var)
    log <- 'Converted to date by using as.Date()'
    return(list(date_temp, log))
    } else {
    log <- paste0('Date conversion failed for variable of type: ', class(date_var))
    return(list(NA, log))
  }
}

alg_datetime <- function(date_var, truncated_num = 3){
  #' Algorithm: Convert date variable to datetime (POSIXct) format
  #' 
  #' Check variable's format and convert it to YMD_HMS format (POSIXct type)
  #' 
  #' Component module to for auto_var_process, assume unique date format
  #' @param date_var Vector, Variable to be converted
  #' @return A list of two elements: (1) converted date variable, (2) log of conversion
  #' @examples
  #'   alg_datetime(c('2020-01-01T12:00:00', '2020-02-01T13:30:00'))
  
  # check variable type
  if (all(is.na(date_var))){
    log <- NA
    return(list(NA, log))
  } else if (is(date_var, 'Date')){
    log <- NA
    return(list(date_var, log))
  } else if (is(date_var, 'POSIXct')){
    log <-  NA
    return(list(date_var, log))
  } else if (is(date_var, 'character')){
    # try multiple formats
    if (all(!is.na(ymd_hms(date_var, truncated = truncated_num, quiet = TRUE)))){ #If the value follows the patter of YMD_HMS
      date_temp <- ymd_hms(date_var, truncated = truncated_num)
      log <- paste0('Date time format: YMD_HMS; read as YMD_HMS', 'with truncated = ', truncated_num)
    } else if (all(!is.na(mdy_hms(date_var, truncated = truncated_num, quiet = TRUE)))){ #If the value follows the patter of MDY_HMS
      date_temp <- mdy_hms(date_var, truncated = truncated_num)
      log <- paste0('Date time format: MDY_HMS; read as MDY_HMS', 'with truncated = ', truncated_num)
    } else if (all(!is.na(dmy_hms(date_var, truncated = truncated_num, quiet = TRUE)))){ #If the value follows the patter of DMY_HMS
      date_temp <- dmy_hms(date_var, truncated = truncated_num)
      log <- paste0('Date time format: DMY_HMS; read as DMY_HMS', 'with truncated = ', truncated_num)
    } else {
      date_temp <- rep(NA, length(date_var))
      log <- paste0('Date time format: Unknown or contains multiple formats; read as NA')
    }
    return(list(date_temp, log))
  } else {
    log <- paste0('Date conversion failed for variable of type: ', class(date_var))
    return(list(NA, log))
  }
}

#' Algorithm: Variable mapping
#' 
#' Map variable to submission value based on codelist
#' 
#' Component module to for auto_var_process
#' @param sub_var String, Name of the variable to be created (SUBMISSION value defined in the codelist)
#' @param decode_var String, Name of the variable to be mapped (DECODE value defined in the codelist)
#' @param codelist_defined Dataframe, Codelist defined in the spec
#' @param var Vector, Variable to be mapped (DECODE value defined in the codelist)
#' @return A vector of mapped submission values
#' @examples
#'   alg_mapping('APERIODC.res'(variable to be created), 'APERIOD.res'(variable exisited in the dataset), codelist_defined, var)
alg_mapping <- function(sub_var, decode_var = NULL, codelist_defined, var, numeric = FALSE){

  #remove .res if any
  sub_var_code <- ifelse(endsWith(sub_var, '.res'), substr(sub_var, 1, nchar(sub_var)-4), sub_var)
  #decode_var_code <- ifelse(endsWith(decode_var, '.res'), substr(decode_var, 1, nchar(decode_var)-4), decode_var)
  if (is.null(decode_var)){
    decode_var <- deparse(substitute(var)) #get the name of the variable passed to var
  }
  # print(class(codelist_defined))
  # print(codelist_defined)
  codelist_defined_temp <- codelist_defined %>%
    filter(VARIABLE == sub_var_code) %>%
    select(SUBMISSION_VALUE, DECODE) %>%
    mutate(DECODE = gsub(' ', '',as.character(DECODE))) 
  # %>%
  #   rename(!!decode_var := DECODE) 
  
  # df <- as.data.frame(var)
  # names(df) <- decode_var
  # df[[decode_var]] <- gsub(' ', '',as.character(df[[decode_var]]))
  # df_merge <- left_join(df, codelist_defined_temp, by = decode_var)
  code_vec <- codelist_defined_temp$SUBMISSION_VALUE
  names(code_vec) <- codelist_defined_temp$DECODE
  var <- gsub(' ', '',as.character(var))
  res <- code_vec[var]
  if (numeric){
    return(as.numeric(res))
  } else {
    return(res)
  }
}

ut_auto_var_mapping <- function(df, spec, codelist_defined, overwrite = FALSE){
  glob_mapping_log <<- list()
  glob_mapping_log_id <<- 1
  var_log <- data.frame(Var = character(), Algorithm_type = character(), Algorithm = character(), Action = character(), glob_log_id = integer(), Comment = character())
  spec_cut <- spec[spec$ORIGIN == 'Assigned',]
  for (i in 1:nrow(spec_cut)){
    # skip if variable already exists and overwrite = FALSE
    if(!overwrite & paste0(spec_cut[i, 'VARIABLE'], '.res') %in% names(df)){
      var_log[i, 'Var'] <- spec_cut[i, 'VARIABLE']
      var_log[i, 'Action'] <- paste0('Variable: ', spec_cut[i, 'VARIABLE'], '.res already exists. Skip mapping.')
      next
    }
    # get algorithm
    if (!is.na(spec_cut[i,'STUDY_SPECIFIC_ALGORITHM'])) {
      algorithm <- as.character(spec_cut[i,'STUDY_SPECIFIC_ALGORITHM'])
      var_log[i, 'Var'] <- spec_cut[i, 'VARIABLE']
      var_log[i, 'Algorithm_type'] <- 'STUDY_SPECIFIC_ALGORITHM'
      var_log[i, 'Algorithm'] <- algorithm
    } else if (!is.na(spec_cut[i,'ANALYSIS_ALGORITHM'])){
      algorithm <- as.character(spec_cut[i,'ANALYSIS_ALGORITHM'])
      var_log[i, 'Var'] <- spec_cut[i, 'VARIABLE']
      var_log[i, 'Algorithm_type'] <- 'ANALYSIS_ALGORITHM'
      var_log[i, 'Algorithm'] <- algorithm
    } else {
      warning(paste0('Algorithm for ', spec_cut[i, 'VARIABLE'], ' : ', 'Undefined'))
      var_log[i, 'Var'] <- spec_cut[i, 'VARIABLE']
      var_log[i, 'Algorithm_type'] <- 'Undefined'
      next
    }
    
    algorithm <- sub("[\r\n]+$", "", algorithm) #remove trailing period if any
    algorithm <- trimws(algorithm) #remove leading/trailing spaces
    algorithm <- sub("[.,;]$", "", algorithm) #remove trailing semicolon if any
    
    if (grepl("^One to one mapping of \\w+ as defined in the codelist$", algorithm)){
      var_temp <- strsplit(algorithm, ' ')[[1]][6]
      var_name <- ifelse(paste0(var_temp, '.res') %in% colnames(df), paste0(var_temp, '.res'), var_temp)
      #print(var_name)
      res_var_name <- paste0(spec_cut[i, 'VARIABLE'], '.res')
      df[[res_var_name]] <- alg_mapping(res_var_name, decode_var = var_name, codelist_defined, df[[var_name]])
      var_log[i, 'Action'] <- paste0('Submission value derived by codelist with DECODE = ', var_name)
    } else if (grepl("^One to one numeric mapping of \\w+ as defined in the code list$", algorithm)){
      var_temp <- strsplit(algorithm, ' ')[[1]][7]
      var_name <- ifelse(paste0(var_temp, '.res') %in% colnames(df), paste0(var_temp, '.res'), var_temp)
      #print(var_name)
      res_var_name <- paste0(spec_cut[i, 'VARIABLE'], '.res')
      df[[res_var_name]] <- alg_mapping(res_var_name, decode_var = var_name, codelist_defined, df[[var_name]], numeric = TRUE)
      var_log[i, 'Action'] <- paste0('Submission value derived by codelist with DECODE = ', var_name, ' and converted to numeric')
    } else {
      warning(paste0('Algorithm for: ', spec_cut[i, 'VARIABLE'], ' not supported'))
      next
    }
  }
  return(list(df, var_log))
}

ut_auto_var_process <- function(df, spec){
  glob_log <<- list()
  glob_log_id <<- 1
  var_log <- data.frame(Var = character(), Algorithm_type = character(), Algorithm = character(), Action = character(), glob_log_id = integer(), Comment = character())
  spec_cut <- spec[spec$ORIGIN %in% c('Predecessor', 'Derived'),]
  for (i in 1:nrow(spec_cut)){
    if (!is.na(spec_cut[i,'STUDY_SPECIFIC_ALGORITHM'])) {
      algorithm <- as.character(spec_cut[i,'STUDY_SPECIFIC_ALGORITHM'])
      var_log[i, 'Var'] <- spec_cut[i, 'VARIABLE']
      var_log[i, 'Algorithm_type'] <- 'STUDY_SPECIFIC_ALGORITHM'
      var_log[i, 'Algorithm'] <- algorithm
    } else if (!is.na(spec_cut[i,'ANALYSIS_ALGORITHM'])){
      algorithm <- as.character(spec_cut[i,'ANALYSIS_ALGORITHM'])
      var_log[i, 'Var'] <- spec_cut[i, 'VARIABLE']
      var_log[i, 'Algorithm_type'] <- 'ANALYSIS_ALGORITHM'
      var_log[i, 'Algorithm'] <- algorithm
    } else {
      warning(paste0('Algorithm for ', spec_cut[i, 'VARIABLE'], ' : ', 'Undefined'))
      var_log[i, 'Var'] <- spec_cut[i, 'VARIABLE']
      var_log[i, 'Algorithm_type'] <- 'Undefined'
      next
    }
    algorithm <- sub("[\r\n]+$", "", algorithm) #remove trailing period if any
    algorithm <- trimws(algorithm) #remove leading/trailing spaces
    algorithm <- sub("[;.]$", "", algorithm) #remove trailing semicolon if any
    
    if (grepl("^Copied from \\w+\\.\\w+\\.\\w+$", trimws(algorithm))){
      print(paste0('Algorithm for ', spec_cut[i, 'VARIABLE'], ' : ', algorithm))
      var_temp <- strsplit(algorithm, ' ')[[1]][3]
      var_name <- strsplit(var_temp, '\\.')[[1]][3]
      #print(var_name)
      var_df <- strsplit(var_temp, '\\.')[[1]][2]
      res_var_name <- paste0(spec_cut[i, 'VARIABLE'], '.res')
      df[[res_var_name]] <- df[[toupper(var_name)]]
      var_log[i, 'Action'] <- paste0('Copied from ', var_name)
      
    } else if (grepl("^Copy values from \\w+\\.\\w+\\.\\w+$", trimws(algorithm))){
      print(paste0('Algorithm for ', spec_cut[i, 'VARIABLE'], ' : ', algorithm))
      var_temp <- strsplit(algorithm, ' ')[[1]][4]
      var_name <- strsplit(var_temp, '\\.')[[1]][3]
      #print(var_name)
      var_df <- strsplit(var_temp, '\\.')[[1]][2]
      res_var_name <- paste0(spec_cut[i, 'VARIABLE'], '.res')
      df[[res_var_name]] <- df[[toupper(var_name)]]
      var_log[i, 'Action'] <- paste0('Copied from ', var_name)
      
    } else if(grepl("^Convert \\w+\\.\\w+\\.\\w+ to numeric datetime$", algorithm)|
              grepl('^Convert \\w+\\.\\w+\\.\\w+ to numeric datetime. If timepart is missing, set to "00:00:00"',trimws(algorithm))){
      print(paste0('Algorithm for ', spec_cut[i, 'VARIABLE'], ' : ', algorithm))
      var_temp <- strsplit(algorithm, ' ')[[1]][2]
      var_name <- strsplit(var_temp, '\\.')[[1]][3]
      res_var_name <- paste0(spec_cut[i, 'VARIABLE'], '.res')
      temp_list <- alg_datetime(df[[toupper(var_name)]])
      df[[res_var_name]] <- temp_list[[1]]
      var_log[i, 'Action'] <- paste0('Value derived from: ', var_name)
      var_log[i, 'Comment'] <-  temp_list[[2]]

    } else if (grepl("^Convert the date part of \\w+\\.\\w+\\.\\w+ to numeric date$", trimws(algorithm))|
               grepl("^Convert the date portion of \\w+\\.\\w+\\.\\w+ to a numeric date$", trimws(algorithm)) | 
               grepl("^Convert the date part of \\w+\\.\\w+\\.\\w+ to numeric date for observed records$", trimws(algorithm))
               ){
      print(paste0('Algorithm for ', spec_cut[i, 'VARIABLE'], ' : ', algorithm))
      var_temp <- strsplit(algorithm, ' ')[[1]][6]
      var_name <- strsplit(var_temp, '\\.')[[1]][3]
      res_var_name <- paste0(spec_cut[i, 'VARIABLE'], '.res')
      temp_list <- alg_date(df[[toupper(var_name)]])
      df[[res_var_name]] <- temp_list[[1]]
      var_log[i, 'Action'] <- paste0('Value derived from: ', var_name)
      var_log[i, 'Comment'] <-  temp_list[[2]]
    } 
    
    else {
      warning(paste0('Algorithm for: ', spec_cut[i, 'VARIABLE'], ' not supported'))
      next
    }
  }
  return(list(df, var_log))
}

ut_relrec_append <- function(df_append, prim_df_name, keys = c('STUDYID', 'USUBJID', 'SUBJID')){
  prim_df_name <- toupper(prim_df_name)
  if (!'RELREC' %in% names(df_append)){
    warning('No RELREC dataset found')
    return(df_append)
  } else {
    df_relrec <- df_append[['RELREC']] %>%
      filter(RDOMAIN %in% names(df_append))
    df_append[['RELREC']] <- NULL
    for (i in names(df_append)){
      df_relrec_temp <- df_relrec %>%
        filter(RDOMAIN == i)
      if (length(unique(df_relrec_temp$IDVAR)) > 1){
        stop(paste0('Multiple IDVAR found for domain: ', i))
      } else{
        relrec_key <- unique(df_relrec_temp$IDVAR)
        df_relrec_temp <- df_relrec_temp %>%
          pivot_wider(names_from = 'IDVAR', values_from = 'IDVARVAL')
      }
      df_relrec_temp[[relrec_key]] <- as.character(df_relrec_temp[[relrec_key]])
      df_append[[i]][[relrec_key]] <- as.character(df_append[[i]][[relrec_key]])
      df_append[[i]] <- left_join(df_append[[i]], df_relrec_temp, by = c(keys, relrec_key), 
                                 suffix = c('', 'relrec'))
      print(paste0('Merge RELREC to dataset: ', i, ' by ', paste(c(keys, relrec_key), collapse = ', ')))
    }
    
    df_res <- df_append[[prim_df_name]]
    df_append[[prim_df_name]] <- NULL
    
    for (i in names(df_append)){
      print(paste0('Merge dataset: ', i,' to ',prim_df_name, ' by ', paste(c(keys, 'RELID'), collapse = ', ')))
      df_res <- left_join(df_res, df_append[[i]], by = c(keys, 'RELID'), 
                          suffix = c('', paste0('.', tolower(i))), na_matches = 'never')
    }
    
    return(df_res)
  }
}

ut_append_data <- function(df_name, df_list, prim_df, keys = c('STUDYID', 'USUBJID', 'SUBJID')){
  #check number of datasets read
  prim_df_name <- toupper(prim_df)
  df_res_list <- list()
  if (length(df_list) == 0){
    stop('No data to append')
  } else if (length(df_list) == 1){
    return(df_list[[1]])
  } else {
    if (length(prim_df) == 1){
      #df_all <- df_list[[which(tolower(df_name) %in% tolower(prim_df))]]
    } else if (length(prim_df) > 1){
      stop('Multiple primary datasets not supported')
      #df_all <- do.call(rbind, df_list[which(tolower(df_name) %in% tolower(prim_df))])
    } else {
      stop('Primary datasets not defined')
    }
    print(paste0('Primary dataset for merge: ', paste(prim_df, collapse = ', ')))
    
    df_name$domain <- ifelse(str_detect(df_name$name, '^SUPP'), 
                             substr(df_name$name, 5, nchar(df_name$name)), 
                             df_name$name)
    for(i in unique(df_name[df_name$name != 'ADSL', 'domain'])){
      if (nrow(df_name[df_name$domain == i,]) > 1){
        prim_df <- df_list[[df_name[df_name$domain == i & df_name$name == df_name$domain, 'name']]]
        supp_df <- df_list[[df_name[df_name$domain == i & df_name$name != df_name$domain, 'name']]]
        merge_df <- left_join(prim_df, supp_df, by = keys, 
                                        suffix = c(i, paste0('supp', tolower(i))))
        df_res_list[[i]] <- merge_df %>%
                              rowwise()%>%
                              filter(IDVARVAL == get(IDVAR)) %>%
                              ungroup()
      } else {
        df_res_list[[i]] <- df_list[[df_name[df_name$domain == i, 'name']]]
      }
    }
    
    #Append ADSL to primary data
    
    if ('ADSL' %in% df_name$name){
      df_res_list[[prim_df_name]] <- left_join(df_res_list[[prim_df_name]], df_list[['ADSL']], by = keys, 
                                          suffix = c('', 'adsl'))
    }
    
    
    # # remove primay data
    # df_list[[which(tolower(df_name) %in% tolower(prim_df))]] <- NULL
    # df_name <- df_name[-which(tolower(df_name) %in% tolower(prim_df))]
    # 
    # combine
    # for (i in 1:length(df_list)){
    #   if (toupper(substr(df_name[i], 1, 4)) == 'SUPP' | toupper(df_name[i]) %in% c('RELREC', 'ADSL')){
    #     df_all <- left_join(df_all, df_list[[i]], by = keys, 
    #                         suffix = c(ifelse(i == 1, 
    #                                           paste0('.', tolower(prim_df)), 
    #                                           paste0('.', tolower(df_name[i-1]))), 
    #                                    paste0('.', tolower(df_name[i]))))
    #     print(paste0('Merge dataset: ', df_name[i], ' by ', paste(keys, collapse = ', ')))
    #   } else {
    #     specific_key <- c(setNames(keys[1],keys[1]), setNames(keys[2],keys[2]), setNames(keys[3],keys[3]),
    #                       setNames(paste0(toupper(df_name[i]), 'SEQ'), paste0(toupper(prim_df), 'SEQ')))
    #     print(specific_key)
    #     df_all <- left_join(df_all, df_list[[i]], by = specific_key, 
    #                         suffix = c(ifelse(i == 1, 
    #                                           paste0('.', tolower(prim_df)), 
    #                                           paste0('.', tolower(df_name[i-1]))), 
    #                                    paste0('.', tolower(df_name[i]))))
    #     print(paste0('Merge dataset: ', df_name[i], ' by ', paste(specific_key, collapse = ', ')))
    #   }
    # }
    return(df_res_list)
  }
}

ut_load_var <- function(spec){
  algorithm <- c(spec$STUDY_SPECIFIC_ALGORITHM, spec$ANALYSIS_ALGORITHM)
  var <- spec$VARIABLE
  var_needed_raw <- unique(na.exclude(c(unlist(str_extract_all(algorithm, "\\b[a-zA-Z]{2,}\\.[a-zA-Z]{2,}\\.[a-zA-Z0-9]{2,}\\b")),
                                    unlist(str_extract_all(algorithm, "\\b[a-zA-Z]{2,}\\.[a-zA-Z]{2,}\\.[a-zA-Z0-9]{2,}\\b")),
                                    trimws(unlist(str_extract_all(algorithm, "\\b[a-zA-Z]{2,}\\.[a-zA-Z0-9]{2,}\\b"))),
                                    trimws(unlist(str_extract_all(algorithm, "\\b [a-zA-Z]{2,}\\.[a-zA-Z0-9]{2,} \\b")))
                                    )
                                    )
                           )
  var_need_cap <- unique(na.exclude(c(trimws(unlist(str_extract_all(algorithm, "\\b[A-Z0-9]{3,}\\b")))
                                      )
                                    )
                        )
  var_needed_temp <- strsplit(var_needed_raw, '\\.')
  var_needed <- sapply(var_needed_temp, function(x) x[[length(x)]])
  var_res <- c(var, var_needed, var_need_cap)
  var_res <- unique(var_res[!is.na(var_res)])
  for (name in c('SUBJID', 'DOMAIN')){
    if (name %in% var_res == FALSE){
      var_res <- c(var_res, name)
    }
  }
  return(var_res)
}

load_data <- function(adam_path, sdtm_path, df){
  
  # if (nchar(df) >= 4 & substr(tolower(df), 1,2) == 'ad'){ #if the name of the data set start with AD, read as Adam
  #   df_path <- file.path(adam_path, paste0(tolower(df), '.sas7bdat'))
  #   flag <- 'adam'
  # } else {
  #   df_path <- file.path(sdtm_path, paste0(tolower(df), '.sas7bdat'))
  #   flag <- 'sdtm'
  # } 
  
  df_path <- ifelse(df[2] == 'SDTM', 
                    file.path(sdtm_path, paste0(tolower(df[1]), '.sas7bdat')),
                    file.path(adam_path, paste0(tolower(df[1]), '.sas7bdat')))
  flag <- df[2]
  name <- df[1]
  if (file.exists(df_path)){
    df_data <- read_sas(df_path)
    print(paste0('Identify dataset: ', name, ' as: ', flag))
    print(paste0('File path: ', df_path))
    return(df_data)
  } else if (tolower(name) == 'adsl'){
    #Special Case for ADSL
    #Read ADSL_i when ADSL is not found 
    df_alt_path <- file.path(adam_path, paste0('adsl_i', '.sas7bdat'))
    if (file.exists(df_alt_path)){
      df_data <- read_sas(df_alt_path)
      print(paste0('Identify dataset: ', name, ' as: ', flag))
      warning('Original file was not found. Read adsl_i instead')
      print(paste0('File path: ', df_path))
      return(df_data)
    } else{
      print(paste0('Identify dataset: ', name, ' as: ', flag))
      stop(paste0('File not found: ', df_path))
    }
  } else {
    print(paste0('Identify dataset: ', name, ' as: ', flag))
    stop(paste0('File not found: ', df_path))
  }
}

ut_read_data <- function(Adam_path, Sdtm_path, df_vec_df, var_name = NULL){
  df_list <- lapply(1:nrow(df_vec_df), function (x) load_data(Adam_path, Sdtm_path, df_vec_df[x,]))
  names(df_list) <- df_vec_df$name
  if (is.null(var_name)){
    return(df_list)
  } else {
    df_list <- lapply(df_list, function(x) x[, names(x) %in% var_name])
    return(df_list)
  }
}

ut_identify_df_name <- function(spec){
  # read list of data sets needed from spec
  algorithm <- ifelse(!is.na(spec$STUDY_SPECIFIC_ALGORITHM), 
                      spec$STUDY_SPECIFIC_ALGORITHM, 
                      ifelse(!is.na(spec$ANALYSIS_ALGORITHM), 
                             spec$ANALYSIS_ALGORITHM, NA))
  if (any(is.na(algorithm))){
    stop('Algorithm not defined for some variables')
  }
  df_vec_sdtm <- unlist(str_extract_all(algorithm, "\\bSDTM\\.[a-zA-Z]{2,}\\.[a-zA-Z]{2,}\\b"))
  df_vec_sdtm <- unique(df_vec_sdtm[!is.na(df_vec_sdtm)])
  df_vec_sdtm <- unique(sapply(df_vec_sdtm, function(x) strsplit(x, '\\.')[[1]][2]))
  
  df_vec_adam <- unlist(str_extract_all(algorithm, "\\bADAM\\.[a-zA-Z]{2,}\\.[a-zA-Z]{2,}\\b"))
  df_vec_adam <- c(df_vec_adam, trimws(unlist(str_extract_all(algorithm, "[a-zA-Z]{2,}\\.[a-zA-Z]{2,}[), ]"))))
  df_vec_adam <- unique(df_vec_adam[!is.na(df_vec_adam)])
  df_vec_adam <- df_vec_adam[!str_detect(df_vec_adam, 'SDTM')]
  df_vec_adam <- unique(sapply(df_vec_adam, function(x) strsplit(x, '\\.')[[1]][1]))
  
  df_vec_df <- data.frame(name = c(df_vec_sdtm, df_vec_adam),
                          source = c(rep('SDTM', length(df_vec_sdtm)), rep('ADAM', length(df_vec_adam)))) %>%
    arrange(name) %>%
    distinct(name, .keep_all = TRUE) %>%
    arrange(desc(source), name)
  return(df_vec_df)
}

ut_adam_setup <- function(mode, compound, study, lock, domain,
                       env = 'MAC', spec_name = 'amba_adam_specs_lilly.xlsx'){
  library('tidyverse')
  library('haven') #Read SAS dataset
  library('readxl') #Read excel
  library('lubridate') #Date manipulation
  library('Hmisc') #Assign labels to variables
  library('diffdf') # Compare dataframes
  library('labelled')
  library(arsenal)
  library(roxygen2)
  
  if (toupper(env) == 'MAC'){
    root_path <- file.path('/Volumes/lillyce', mode, compound, study, lock)
  } else {
    stop('not supported yet')
  }
  
  Adam_path <<- file.path(root_path, 'data', 'analysis', 'shared')
  Sdtm_path <<- file.path(root_path, 'data', 'observed', 'shared')
  Spec_path <<- file.path(root_path, 'documentation', 'specs', 'analysis')
  Program_path <<- file.path(root_path, 'programs', 'analysis')
  Log_path <<- file.path(root_path, 'logs', 'analysis')
  spec <- read_excel(file.path(Spec_path, spec_name), sheet = toupper(domain)) %>%
    filter(is.na(REMOVE)) %>%
    arrange(as.numeric(ORDER))
  codelist_defined <- read_excel(file.path(Spec_path, spec_name), sheet = toupper('DEFINE_TERMINOLOGY')) %>%
    filter(DATASET == toupper(domain))
  assign('spec', spec, envir = .GlobalEnv)
  assign('codelist_defined', codelist_defined, envir = .GlobalEnv)
  # read list of data sets needed from spec
}
#roxygenise()


# mode = 'qa'
# compound = 'ly3074828'
# study = 'i6t_mc_amba'
# lock = 'safety_review7'
# domain = 'adpr'
# prim_df = 'pr'
# 
# ut_adam_setup(mode, compound, study, lock, domain)
# df_vec_df <- ut_identify_df_name(spec)
# # df_vec <- as.vector(na.exclude(unique(spec$SOURCE_DATASET)))
# #load data
# df_list <- ut_read_data(Adam_path, Sdtm_path, df_vec_df)
# df_append <- ut_append_data(df_vec_df, df_list, prim_df)
# df_relrec <- ut_relrec_append(df_append, prim_df)
# df_res_pred_list <- auto_var_process(df_relrec, spec)
# df_res <- df_res_pred_list[[1]]
# log <- df_res_pred_list[[2]]

