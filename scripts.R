library(foreign)

afro_list = list(
  "r2" = list(
    "con" = "country",
    "con_array" = c( # country
      "Benin",
      "Botswana",
      "Cape Verde",
      "Ghana",
      "Kenya",
      "Lesotho",
      "Madagascar",
      "Malawi",
      "Mali",
      "Mozambique",
      "Namibia",
      "Nigeria",
      "Senegal",
      "South Africa",
      "Tanzania",
      "Uganda",
      "Zambia",
      "Zimbabwe" 
    ),
    "var" = list(
      "no"   = "respno",
      "eth"  = NA,
      "eth1" = NA,
      "lan"  = "q83",
      "lan1" = NA,
      "rel"  = "q85",
      "rel1" = NA,
      "reg"  = "region",
      "reg1"= NA
    )
  ),
  
  
  "r3" = list(
    "con" = "country",
    "con_array" = c( # country
      "Benin",
      "Botswana",
      "Cape Verde",
      "Ghana",
      "Kenya",
      "Lesotho",
      "Madagascar",
      "Malawi",
      "Mali",
      "Mozambique",
      "Namibia",
      "Nigeria",
      "Senegal",
      "South Africa",
      "Tanzania",
      "Uganda",
      "Zambia",
      "Zimbabwe"
    ),
    "var" = list(
      "no"   = "respno",
      "eth"  = "q79",
      "eth1" = NA,
      "lan"  = "q114",
      "lan1" = NA,
      "rel"  = "q91",
      "rel1" = NA,
      "reg"  = "region",
      "reg1"= NA
    )
  ),
  
  "r4" = list(
    "con" = "COUNTRY",
    "con_array" = c( # COUTNRY
      "Benin",
      "Botswana",
      "Burkina Faso",
      "Cape Verde",
      "Ghana",
      "Kenya",
      "Lesotho",
      "Liberia",
      "Madagascar",
      "Malawi",
      "Mali",
      "Mozambique",
      "Namibia",
      "Nigeria",
      "Senegal",
      "South Africa",
      "Tanzania",
      "Uganda",
      "Zambia",
      "Zimbabwe"
    ),
    "var" = list(
      "no"   = "RESPNO",
      "eth"  = "Q79",
      "eth1" = "Q79OTHER",
      "lan"  = "Q114",
      "lan1" = NA,
      "rel"  = "Q90",
      "rel1" = NA,
      "reg"  = "REGION",
      "reg1" ="DISTRICT"
    )
  ),
  
  "r5" = list(
    "con" = "COUNTRY",
    "con_array" = c(
      "Benin",
      "Botswana",
      "Burkina Faso",
      "Cape Verde",
      "Ghana",
      "Kenya",
      "Lesotho",
      "Liberia",
      "Madagascar",
      "Malawi",
      "Mali",
      "Mozambique",
      "Namibia",
      "Nigeria",
      "Senegal",
      "South Africa",
      "Tanzania",
      "Uganda",
      "Zambia",
      "Zimbabwe",
      "Mauritius",
      "Sierra Leone",
      "Niger",
      "Togo",
      "Burundi",
      "Cameroon",
      "Cote d'Ivoire",
      "Guinea",
      "Swaziland",
      "Algeria",
      "Egypt",
      "Morocco",
      "Sudan",
      "Tunisia"
    ),
    "var" = list(
      "no"   = "RESPNO",
      "eth"  = "Q84",
      "eth1" = "Q84OTHER",
      "lan"  = "Q116",
      "lan1" = "Q116OTHER",
      "rel"  = "Q98A",
      "rel1" = "Q98AOTHER",
      "reg"  = "REGION",
      "reg1" = NA
    )
    
  ),
  
  'r6' = list(
    "con" = "COUNTRY",
    "con_array" = c(
      "Algeria",
      "Benin",
      "Botswana",
      "Burkina Faso",
      "Burundi",
      "Cameroon",
      "Cape Verde",
      "Cote d'Ivoire",
      "Egypt",
      "Gabon",
      "Ghana",
      "Guinea",
      "Kenya",
      "Lesotho",
      "Liberia",
      "Madagascar",
      "Malawi",
      "Mali",
      "Mauritius",
      "Morocco",
      "Mozambique",
      "Namibia",
      "Niger",
      "Nigeria",
      "São Tomé and Príncipe",
      "Senegal",
      "Sierra Leone",
      "South Africa",
      "Sudan",
      "Swaziland",
      "Tanzania",
      "Togo",
      "Tunisia",
      "Uganda",
      "Zambia",
      "Zimbabwe"
    ),
    "var" = list(
      "no"   = "RESPNO",
      "eth"  = "Q87",
      "eth1" = "Q87OTHER",
      "lan"  = "Q2",
      "lan1" = "Q2OTHER",
      "rel"  = "Q98A",
      "rel1" = "Q98AOTHER",
      "reg"  = "REGION",
      "reg1" = NA
    )
  )
)


grepcon = function(con_name){
  df = data.frame(matrix(ncol = 10, nrow = 0))
  for(round in names(afro_list)){
    if (con_name %in% afro_list[[round]][["con_array"]]) { # if country exist in dataset
      print(paste0(con_name , " found in Afrobarometer ", round))
      sav = read.spss(paste0(round, ".sav")) # read sav
      dict = afro_list[[round]]
      var_list = dict[["var"]] # read variable list
      con_col = dict[["con"]]  # read corresponding country variable name
      con_bool = sav[[con_col]] == con_name
      n = sum(con_bool)
      print(paste0("Adding ", n, " respondents..."))
      df_add = data.frame(matrix(ncol = 10, nrow = n))
      df_add[,1] = rep(round, n)
      for (i in 1:length(var_list)){
        var_name = var_list[i][[1]]
        if (is.na(var_name) == FALSE) {
          new_col = sav[[var_name]][con_bool]
          df_add[,i+1] = new_col
        }
      }
      df = rbind(df, df_add)
    }else{
      print(paste0(con_name , " not found in Afrobarometer ", round))
    }
  }
  names(df) = c("r", names(afro_list[[1]][["var"]]))
  print("Writing output csv...")
  write.csv(df, paste0(con_name, ".csv"))
  return(df)
}

grepdf = function(dat, patt, col_name_array = names(dat)[-1], reg = F) {
  col_num_array = c() 
  col_name_all = names(dat)
  for (col_name in col_name_array) {
    if (col_name %in% col_name_all){
      col_name_reg = paste0("^", col_name, "$")
      num = grep(col_name_reg, col_name_all)
      col_num_array = c(col_num_array, num)
    }else{
      print(paste0("column <", col_name, "> not found in dataset!"))
    }
  }
  # loop through column
  n = nrow(dat)
  bool_array = rep(F, n) 
  # check if regex is on
  if (reg == F){
    pat = paste0("^", patt, "$")
    note = ""
  } else {
    pat = patt
    note = " (regex is on)"
  }
  pat = tolower(pat)
  for (j in col_num_array) {
    col_check_raw = dat[,j]
    col_check_mod = tolower(gsub("^\\s+|\\s+$", "", col_check_raw))
    bool_new = grepl(pat, col_check_mod)
    bool_array = bool_array + bool_new
  }
  col_output = bool_array > 0
  dat_output = dat[col_output,]
  
  # convert factor to character
  for(j in 1:ncol(dat_output)){
    dat_output[,j] = as.character(dat_output[,j])
  }
  
  N = nrow(dat_output)
  print(paste0("Found ", N, " respondents with pattern: ", patt , note))
  table_output = table(dat_output$r)
  print("Summary:")
  print(table_output)
  return(dat_output)
}

