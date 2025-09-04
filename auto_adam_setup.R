mode = 'qa'
compound = 'ly3074828'
study = 'i6t_mc_amba'
lock = 'safety_review7'
domain = 'adho'
prim_df = 'ho'

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
  spec_cut <- spec[spec$ORIGIN == 'Predecessor',]
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
    
    if (grepl("^Copied from \\w+\\.\\w+\\.\\w+$", algorithm)){
      var_temp <- strsplit(algorithm, ' ')[[1]][3]
      var_name <- strsplit(var_temp, '\\.')[[1]][3]
      #print(var_name)
      var_df <- strsplit(var_temp, '\\.')[[1]][2]
      res_var_name <- paste0(spec_cut[i, 'VARIABLE'], '.res')
      df[[res_var_name]] <- df[[toupper(var_name)]]
    } else if(grepl("^Convert \\w+\\.\\w+\\.\\w+ to numeric datetime.", algorithm)){
      var_temp <- strsplit(algorithm, ' ')[[1]][2]
      var_name <- strsplit(var_temp, '\\.')[[1]][3]
      res_var_name <- paste0(spec_cut[i, 'VARIABLE'], '.res')
      df[[res_var_name]] <- 
    }
    
    else {
      warning(paste0('Algorithm for: ', spec_cut[i, 'VARIABLE'], ' not supported'))
      next
    }
  }
  return(df)
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
      df_all <- left_join(df_all, df_list[[i]], by = keys, 
                          suffix = c(ifelse(i == 1, 
                                            paste0('.', tolower(prim_df)), 
                                            paste0('.', tolower(df_name[i-1]))), 
                                     paste0('.', tolower(df_name[i]))))
      print(paste0('Merge dataset: ', df_name[i], ' by ', paste(keys, collapse = ', ')))
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
  df_res_pred <- auto_var_process(df_all, spec)
}

