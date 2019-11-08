ReadVarDes_ttf <- function(VarDes,Nlist){
  # for ttf data only 
  # make sure the ttf duration is paired with the ttf event. 
  ## NOTE, should be able to merge this function to ReadVarDes()
  
  dt_time  <- VarDes[which(!is.na(VarDes["ttf_name"])),c("VarName","DataID","ttf_name")]
  dt_event <- VarDes[which(!is.na(VarDes["ttf_event_name"])),c("VarName","DataID","ttf_event_name")]
  
  if(length(dt_time$ttf_name) != length(dt_event$ttf_event_name)){
    stop(cat(paste0("TTF variable ERROR: different number of event and duration: \n No. Event column : ", 
                    length(dt_event$ttf_event_name),
                    " \n No. Duration column : " ,
                    length(dt_event$ttf_event_name))))
  }else if (!all(sort(dt_time$ttf_name) == sort(dt_event$ttf_event_name))){
    stop(cat(paste0("TTF variable ERROR: the event index and duration index doesn't match")))
  }else{
    MakeList1 <- rep( list(NA), Nlist)
    MakeList2 <- rep( list(NA), Nlist)
    MakeList  <- list()
    
    sort.time  <- dt_time[order(dt_time$ttf_name), ]  ## order doesn't work anymore, need to do the exact match
    sort.event <- dt_event[order(dt_event$ttf_event_name),]
    print("Check if the TTF event name and duration name match : ")
    print(paste(c("Event    :", sort.event$VarName),collapse="  "))
    print(paste(c("Duration :", sort.time$VarName),collapse="  "))   ## align ??
    
    for (i in 1:Nlist){
      VN_time   <- sort.time$VarName[sort.time$DataID == i]
      VN_event  <- sort.event$VarName[sort.event$DataID == i]
      
      if(length(VN_time) >0){
        MakeList1[[i]] = VN_time
        MakeList2[[i]] = VN_event
      }
    }
    MakeList[[1]] <- MakeList1
    MakeList[[2]] <- MakeList2
    return(MakeList)
  }
}
