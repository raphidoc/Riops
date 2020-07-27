library(tidyverse)
library(data.table)

generate.IOP.DB <- function(path="./"){
  #Generate a DB by instrument in L3 folder
  #From some reading in https://stackoverflow.com/questions/13649979/what-specifically-are-the-dangers-of-evalparse
  #The method used here should be changed .... 
  
  stations <- list.files(pattern = "IOP.fitted.down.RData", recursive = T)
  for (i in seq_along(stations)){
    load(file.path(getwd(), stations[i]))
    ID <- str_extract(stations[i],"(?<=Station)[:alnum:]*(?=/)")
    
    devices <- map_lgl(IOP.fitted.down[-1], is_empty) == F
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
  ASPH_DF <- bind_rows(mget(ls(pattern = "(ASPH)_[[:digit:]]+")))
  names(ASPH_DF) <- c("ID","Depth",paste0("A_",wl_ASPH))
  write_csv(ASPH_DF, path = "../L3/ASPH/ASPH_DB.csv")
  
  CTD_DF <- bind_rows(mget(ls(pattern = "(CTD)_[[:digit:]]+")))
  names(CTD_DF) <- c("ID","Depth","Temp","PSU")
  write_csv(CTD_DF, path = "../L3/CTD/CTD_DB.csv")
  
  FLECO_DF <- bind_rows(mget(ls(pattern = "(FLECO)_[[:digit:]]+")))
  write_csv(FLECO_DF, path = "../L3/FLECO/FLECO_DB.csv")
  
  DF_list <- purrr::map(mget(ls(pattern = "(HS6)_[[:digit:]]+")),
             setNames,
             c("ID","Depth",paste0("Bbp_",wl_HS6),paste0("Bb_",wl_HS6),"FDOM","FCHL","Bbp_555","nuP"))
  HS6_DF <- bind_rows(DF_list)
  write_csv(HS6_DF, path = "../L3/HS6/HS6_DB.csv")
             
}
