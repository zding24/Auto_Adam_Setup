mode = 'qa'
compound = 'ly3074828'
study = 'i6t_mc_amba'
lock = 'safety_review7'
domain = 'adho'
prim_df = 'ho'

alg_datetime <- function(date_var){
  if (all(is.na(date_var))){
    log <- data.frame(Var = date_var, Action = NA)
    return(list(NA, log))
  } else if (is(date_var, 'Date')){
    log <- data.frame(Var = date_var, Action = NA)
    return(list(date_var, log))
  } else if (is(date_var, 'POSIXct')){
    log <- data.frame(Var = date_var, Action = NA)
    return(list(date_var, log))
  } else if (is(date_var, 'character')){
    date_temp <- c()
    log <- data.frame(Var = character(), Action = character())
    for (i in 1:length(date_var)){
      if (!is.na(ymd_hms(date_var[i], quiet = TRUE))){
        date_temp[i] <- (ymd_hms(date_var[i]))
        log[i,2] <- 'Date time read as YMD_HMS'
      } else if (!is.na(ymd_hm(date_var[i], quiet = TRUE))){
        date_temp[i] <- (ymd_hm(date_var[i]))
        log[i,2] <- 'Date time read as YMD_HM'
      } else if (!is.na(ymd_h(date_var[i], quiet = TRUE))){
        date_temp[i] <- (ymd_h(date_var[i]))
        log[i,2] <- 'Date time read as YMD_H'
      } else if (!is.na(ymd(date_var[i], quiet = TRUE))){
        date_temp[i] <- (ymd(date_var[i]))
        log[i,2] <- 'Date time read as YMD'
      } else if (!is.na(mdy_hms(date_var[i], quiet = TRUE))){
        date_temp[i] <- (mdy_hms(date_var[i]))
        log[i,2] <- 'Date time read as MDY_HMS'
      } else if (!is.na(mdy_hm(date_var[i], quiet = TRUE))){
        date_temp[i] <- (mdy_hm(date_var[i]))
        log[i,2] <- 'Date time read as MDY_HM'
      } else if (!is.na(mdy_h(date_var[i], quiet = TRUE))){
        date_temp[i] <- (mdy_h(date_var[i]))
        log[i,2] <- 'Date time read as MDY_H'
      } else if (!is.na(mdy(date_var[i], quiet = TRUE))){
        date_temp[i] <- (mdy(date_var[i]))
        log[i,2] <- 'Date time read as MDY'
      } else if (!is.na(dmy_hms(date_var[i], quiet = TRUE))){
        date_temp[i] <- (dmy_hms(date_var[i]))
        log[i,2] <- 'Date time read as DMY_HMS'
      } else if (!is.na(dmy_hm(date_var[i], quiet = TRUE))){
        date_temp[i] <- (dmy_hm(date_var[i]))
        log[i,2] <- 'Date time read as DMY_HM'
      } else if (!is.na(dmy_h(date_var[i], quiet = TRUE))){
        date_temp[i] <- (dmy_h(date_var[i]))
        log[i,2] <- 'Date time read as DMY_H'
      } else if (!is.na(dmy(date_var[i], quiet = TRUE))){
        date_temp[i] <- (dmy(date_var[i]))
        log[i,2] <- 'Date time read as DMY'
      } else if (is.na(date_var[i])){
        date_temp[i] <- NA
        log[i,2] <- 'Date time read as NA'
      } else {
        warning(paste0('Date conversion failed for variable of type: ', class(date_var), ' with value: ', date_var[i]))
        date_temp[i] <- NA
        log[i,2] <- 'Date time conversion failed'
      }
    }
    log[,1] <- date_var
    return(list(date_temp, log))
  } else {
    warning(paste0('Date conversion failed for variable of type: ', class(date_var)))
    log <- data.frame(Var = date_var, Action = NA)
    return(list(NULL, log))
  }
}


auto_var_mapping <- function(df, spec){
  spec_cut <- spec[spec$ORIGIN == 'Assigned',]
  for (i in 1:nrow(spec_cut)){
    if (!is.na(spec_cut[i,'STUDY_SPECIFIC_ALGORITHM'])) {
      algorithm <- as.character(spec_cut[i,'STUDY_SPECIFIC_ALGORITHM'])
      print('Using STUDY_SPECIFIC_ALGORITHM')
    } else if (!is.na(spec_cut[i,'ANALYSIS_ALGORITHM'])){
      algorithm <- as.character(spec_cut[i,'ANALYSIS_ALGORITHM'])
      print('Using ANALYSIS_ALGORITHM')
    } else {
      warning(paste0('Algorithm for ', spec_cut[i, 'VARIABLE'], ' : ', 'Undefined'))
      next
    }
    print(paste0('Algorithm for ', spec_cut[i, 'VARIABLE'], ' : ', algorithm))
    
    if (grepl("^One to one mapping of \\w+ as defined in the codelist$", algorithm)){
      var_temp <- strsplit(algorithm, ' ')[[1]][3]
      var_name <- strsplit(var_temp, '\\.')[[1]][3]
      #print(var_name)
      var_df <- strsplit(var_temp, '\\.')[[1]][2]
      res_var_name <- paste0(spec_cut[i, 'VARIABLE'], '.res')
      df[[res_var_name]] <- df[[toupper(var_name)]]
    } else {
      warning(paste0('Algorithm for: ', spec_cut[i, 'VARIABLE'], ' not supported'))
      next
    }
  }
}

auto_var_process <- function(df, spec){
  glob_log <<- list()
  glob_log_id <- 1
  var_log <- data.frame(Var = character(), Algorithm = character(), Action = character(), glob_log_id = integer(), Comment = character())
  spec_cut <- spec[spec$ORIGIN %in% c('Predecessor', 'Derived'),]
  for (i in 1:nrow(spec_cut)){
    if (!is.na(spec_cut[i,'STUDY_SPECIFIC_ALGORITHM'])) {
      algorithm <- as.character(spec_cut[i,'STUDY_SPECIFIC_ALGORITHM'])
      var_log[i, 'Var'] <- spec_cut[i, 'VARIABLE']
      var_log[i, 'Algorithm'] <- 'STUDY_SPECIFIC_ALGORITHM'
    } else if (!is.na(spec_cut[i,'ANALYSIS_ALGORITHM'])){
      algorithm <- as.character(spec_cut[i,'ANALYSIS_ALGORITHM'])
      var_log[i, 'Var'] <- spec_cut[i, 'VARIABLE']
      var_log[i, 'Algorithm'] <- 'ANALYSIS_ALGORITHM'
    } else {
      warning(paste0('Algorithm for ', spec_cut[i, 'VARIABLE'], ' : ', 'Undefined'))
      var_log[i, 'Var'] <- spec_cut[i, 'VARIABLE']
      var_log[i, 'Algorithm'] <- 'Undefined'
      next
    }
    
    if (grepl("^Copied from \\w+\\.\\w+\\.\\w+$", algorithm)){
      print(paste0('Algorithm for ', spec_cut[i, 'VARIABLE'], ' : ', algorithm))
      var_temp <- strsplit(algorithm, ' ')[[1]][3]
      var_name <- strsplit(var_temp, '\\.')[[1]][3]
      #print(var_name)
      var_df <- strsplit(var_temp, '\\.')[[1]][2]
      res_var_name <- paste0(spec_cut[i, 'VARIABLE'], '.res')
      df[[res_var_name]] <- df[[toupper(var_name)]]
      var_log[i, 'Action'] <- paste0('Copied from ', var_name)
      
    } else if(grepl("^Convert \\w+\\.\\w+\\.\\w+ to numeric datetime.", algorithm)){
      print(paste0('Algorithm for ', spec_cut[i, 'VARIABLE'], ' : ', algorithm))
      var_temp <- strsplit(algorithm, ' ')[[1]][2]
      var_name <- strsplit(var_temp, '\\.')[[1]][3]
      res_var_name <- paste0(spec_cut[i, 'VARIABLE'], '.res')
      
      df[[res_var_name]] <- alg_datetime(df[[toupper(var_name)]])[[1]]
      
      var_log[i, 'Action'] <- paste0('Value derived from: ', var_name)
      glob_log[[glob_log_id]] <<- alg_datetime(df[[toupper(var_name)]])[[2]]
      dt_frt <- unique(glob_log[[glob_log_id]]$Action)
      var_log[i, 'Comment'] <-  paste(dt_frt, collapse = ", ")
      if (length(dt_frt) > 1){
        warning(paste0('Multiple formats detected for variable: ', res_var_name, ' - ', paste(dt_frt, collapse = '; ')))
      }
      var_log[i, 'glob_log_id'] <- glob_log_id
      glob_log_id <- glob_log_id + 1
      
    }
    
    else {
      warning(paste0('Algorithm for: ', spec_cut[i, 'VARIABLE'], ' not supported'))
      next
    }
  }
  return(list(df, var_log))
}

append_data <- function(df_name, df_list, prim_df, keys = c('STUDYID', 'USUBJID', 'SUBJID')){
  #check number of datasets read
  if (length(df_list) == 0){
    stop('No data to append')
  } else if (length(df_list) == 1){
    return(df_list[[1]])
  } else {
    if (length(prim_df) == 1){
      df_all <- df_list[[which(tolower(df_name) %in% tolower(prim_df))]]
    } else if (length(prim_df) > 1){
      df_all <- do.call(rbind, df_list[which(tolower(df_name) %in% tolower(prim_df))])
    } else {
      stop('Primary datasets not defined')
    }
    print(paste0('Primary dataset for merge: ', prim_df))
    
    # remove primay data
    df_list[[which(tolower(df_name) %in% tolower(prim_df))]] <- NULL
    df_name <- df_name[-which(tolower(df_name) %in% tolower(prim_df))]
    
    # combine
    for (i in 1:length(df_list)){
      if (toupper(substr(df_name[i], 1, 4)) == 'SUPP' | toupper(df_name[i]) %in% c('RELREC', 'ADSL')){
        df_all <- left_join(df_all, df_list[[i]], by = keys, 
                            suffix = c(ifelse(i == 1, 
                                              paste0('.', tolower(prim_df)), 
                                              paste0('.', tolower(df_name[i-1]))), 
                                       paste0('.', tolower(df_name[i]))))
        print(paste0('Merge dataset: ', df_name[i], ' by ', paste(keys, collapse = ', ')))
      } else {
        specific_key <- c(setNames(keys[1],keys[1]), setNames(keys[2],keys[2]), setNames(keys[3],keys[3]),
                          setNames(paste0(toupper(df_name[i]), 'SEQ'), paste0(toupper(prim_df), 'SEQ')))
        print(specific_key)
        df_all <- left_join(df_all, df_list[[i]], by = specific_key, 
                            suffix = c(ifelse(i == 1, 
                                              paste0('.', tolower(prim_df)), 
                                              paste0('.', tolower(df_name[i-1]))), 
                                       paste0('.', tolower(df_name[i]))))
        print(paste0('Merge dataset: ', df_name[i], ' by ', paste(specific_key, collapse = ', ')))
      }
    }
    return(df_all)
  }
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

identify_df_name <- function(spec){
  # read list of data sets needed from spec
  algorithm <- ifelse(!is.na(spec$STUDY_SPECIFIC_ALGORITHM), 
                      spec$STUDY_SPECIFIC_ALGORITHM, 
                      ifelse(!is.na(spec$ANALYSIS_ALGORITHM), 
                             spec$ANALYSIS_ALGORITHM, NA))
  if (any(is.na(algorithm))){
    stop('Algorithm not defined for some variables')
  }
  df_vec_sdtm <- unlist(str_extract_all(algorithm, "\\bSDTM\\.[a-zA-Z]+\\.[a-zA-Z]+\\b"))
  df_vec_sdtm <- unique(df_vec_sdtm[!is.na(df_vec_sdtm)])
  df_vec_sdtm <- unique(sapply(df_vec_sdtm, function(x) strsplit(x, '\\.')[[1]][2]))
  
  df_vec_adam <- unlist(str_extract_all(algorithm, "\\bADAM\\.[a-zA-Z]+\\.[a-zA-Z]+\\b"))
  df_vec_adam <- c(df_vec_adam, unlist(str_extract_all(algorithm, "\\b[a-zA-Z]+\\.[a-zA-Z]+\\b")))
  df_vec_adam <- unique(df_vec_adam[!is.na(df_vec_adam)])
  df_vec_adam <- df_vec_adam[!str_detect(df_vec_adam, 'SDTM')]
  df_vec_adam <- unique(sapply(df_vec_adam, function(x) strsplit(x, '\\.')[[1]][1]))
  
  df_vec_df <- data.frame(name = c(df_vec_sdtm, df_vec_adam),
                          source = c(rep('SDTM', length(df_vec_sdtm)), rep('ADAM', length(df_vec_adam))))
  return(df_vec_df)
}

adam_setup <- function(mode, compound, study, lock, domain, prim_df,
                       env = 'MAC', spec_name = 'amba_adam_specs_lilly.xlsx'){
  library('tidyverse')
  library('haven') #Read SAS dataset
  library('readxl') #Read excel
  library('lubridate') #Date manipulation
  library('Hmisc') #Assign labels to variables
  library('diffdf') # Compare dataframes
  library('labelled')
  library(arsenal)
  
  if (toupper(env) == 'MAC'){
    root_path <- file.path('/Volumes/lillyce', mode, compound, study, lock)
  } else {
    stop('not supported yet')
  }
  
  Adam_path <- file.path(root_path, 'data', 'analysis', 'shared')
  Sdtm_path <- file.path(root_path, 'data', 'observed', 'shared')
  Spec_path <- file.path(root_path, 'documentation', 'specs', 'analysis')
  Program_path <- file.path(root_path, 'programs', 'analysis')
  
  spec<- read_excel(file.path(Spec_path, spec_name), sheet = toupper(domain)) %>%
    filter(is.na(REMOVE)) %>%
    arrange(as.numeric(ORDER))
  # read list of data sets needed from spec
  df_vec_df <- identify_df_name(spec)
  # df_vec <- as.vector(na.exclude(unique(spec$SOURCE_DATASET)))
  #load data
  df_list <- lapply(1:nrow(df_vec_df), function (x) load_data(Adam_path, Sdtm_path, df_vec_df[x,]))
  df_all <- append_data(df_vec_df$name, df_list, prim_df)
  df_res_pred_list <- auto_var_process(df_all, spec)
}

