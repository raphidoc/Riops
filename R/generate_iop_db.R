#' @name generate_iop_db 
#' @import dplyr
#' @import stringr
#' @description Loop over all L2 Station QC above 0 (in iop_processing_log) to load and extract
#' all non-empty list in IOP.fitted.down.RData file.
#' Create in L3 one csv file by instrument.
#' @author Raphael Mabit
#' @export

generate_iop_db <- function(project, mission="XXX", boat=c("")){
  #Generate a DB by instrument in L3 folder
  #From some reading in https://stackoverflow.com/questions/13649979/what-specifically-are-the-dangers-of-evalparse
  #The method used here should be changed ....
  
# Filter setup ------------------------------------------------------------
  
  L2 <- file.path(project, "L2")
  
  LogFile <- list.files(path = file.path(project,"ProLog"), pattern = "iop_processing_log", recursive = F, full.names = T)
  
  ProLog <- data.table::fread(file = LogFile, data.table = F, colClasses = "character")
  
  # List available data point in L2
  dirs <- grep("/IOP$",list.dirs(L2,recursive = T), value = T)
  IOPframe <- data.frame(dirs)
  
  IOPframe <- IOPframe %>%
    mutate(ID = str_extract(dirs, "(?<=/)[[:digit:]]+"))
  
  # Identifies paths with ProLog
  ProLog <- ProLog %>% inner_join(IOPframe, by="ID")
  
  # Filter QC
  ProLog <- ProLog %>% filter(QC > 0)
  

# loop over all dataset in all directories --------------------------------
  
  for (i in seq_along(ProLog$dirs)){
    ID <- ProLog$ID[i]
    DataFile <- file.path(ProLog$dirs[i],"IOP.fitted.down.RData")
    if (file.exists(DataFile)) {
      load(DataFile)
    } else {
      message("File :",DataFile," does not exist")
    }

    
    devices <- purrr::map_lgl(IOP.fitted.down[-1], rlang::is_empty) == F
    device <- devices[devices]
    
    for (i2 in seq_along(device)){
      assign(paste0(names(device[i2]),"_",ID),
             data.frame(ID=rep(ID, length(IOP.fitted.down$Depth)),
                        Depth= IOP.fitted.down$Depth))
      
      for (var_x in names(eval(parse(text=paste0("IOP.fitted.down$",names(device[i2])))))){
        if(var_x == "wl" & names(device[i2]) == "ASPH"){
          if (exists("wl_ASPH") && wl_ASPH != eval(parse(text=paste0("IOP.fitted.down$",names(device[i2]),"$",var_x)))) warning(paste("ASPH wavelength have changed:",ID))
          wl_ASPH <- eval(parse(text=paste0("IOP.fitted.down$",names(device[i2]),"$",var_x)))
          next()
        }
        if(var_x == "wl" & names(device[i2]) == "HS6"){
          wl_HS6 <- eval(parse(text=paste0("IOP.fitted.down$",names(device[i2]),"$",var_x)))
          next()
        }
        eval(parse(text=paste0(names(device[i2]),"_",ID,
                               "<- cbind(", names(device[i2]),"_",ID,
                               ", IOP.fitted.down$",names(device[i2]),"$",var_x,")")))
      }
    }
  }

# Save in L3 --------------------------------------------------------------
  
  L3 <- file.path(project, "L3")

  ASPH_DF <- bind_rows(mget(ls(pattern = "(ASPH)_[[:digit:]]+")))
  names(ASPH_DF) <- c("ID","Depth",paste0("A_",wl_ASPH))
  # Check that no old DB is earased accidentally
  lighthouse::check_l3(project, L3, set="ASPH")
  readr::write_csv(ASPH_DF,
                   path = file.path(L3,"ASPH",
                                    paste0("ASPH_DB_",Sys.Date(),"_",str_c(mission, boat, collapse = "_"),".csv")))
  

  CTD_DF <- bind_rows(mget(ls(pattern = "(CTD)_[[:digit:]]+")))
  names(CTD_DF) <- c("ID","Depth","Temp","PSU")
  lighthouse::check_l3(project, L3, set="CTD")
  readr::write_csv(ASPH_DF,
                   path = file.path(L3,"CTD",
                                    paste0("CTD_DB_",Sys.Date(),"_",str_c(mission, boat, collapse = "_"),".csv")))
  
  FLECO_DF <- bind_rows(mget(ls(pattern = "(FLECO)_[[:digit:]]+")))
  lighthouse::check_l3(project, L3, set="FLECO")
  readr::write_csv(ASPH_DF,
                   path = file.path(L3,"FLECO",
                                    paste0("FLECO_DB_",Sys.Date(),"_",str_c(mission, boat, collapse = "_"),".csv")))
  
  DF_list <- purrr::map(mget(ls(pattern = "(HS6)_[[:digit:]]+")),
             setNames,
             c("ID","Depth",paste0("Bbp_",wl_HS6),paste0("Bb_",wl_HS6),"FDOM","FCHL","Bbp_555","nuP"))
  HS6_DF <- bind_rows(DF_list)
  lighthouse::check_l3(project, L3, set="HS6")
  readr::write_csv(ASPH_DF,
                   path = file.path(L3,"HS6",
                                    paste0("HS6_DB_",Sys.Date(),"_",str_c(mission, boat, collapse = "_"),".csv")))
  

}
