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
      "Cote d’Ivoire",
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