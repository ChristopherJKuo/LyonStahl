PropInfoToContact <- function(t){
    require(dplyr)
    t2 <- arrange(t, Contact.ID)
    exfile <- data.frame(Contact.ID = "", Site.Address.City = "", Site.Address.Zip...4 = "", Site.Street.Address = "", Type.Of.Property.Owned = "", stringsAsFactors = FALSE)
    
    curr <- 1
    exfile$Contact.ID[curr] <- as.character(t2$Contact.ID[1])
    exfile$Site.Address.City[curr] <- as.character(t2$Site.Address.City[1])
    exfile$Site.Address.Zip...4[curr] <- as.character(t2$Site.Address.Zip...4[1])
    exfile$Site.Street.Address[curr] <- as.character(t2$Site.Street.Address[1])
    if (t2$Number.of.Units[1] != 1){
        exfile$Type.Of.Property.Owned[curr] <- "Multi Family"
    } else {
        exfile$Type.Of.Property.Owned[curr] <- "SFR"
    }
    hold <- exfile
    
    #t2 is always on index i
    #curr is the index of the export file
    #1 is the index of hold, a 1 row frame for holding the current working iteration of exfile
    
    for (i in 2:length(t2$Contact.ID)) {
        if (t2$Contact.ID[i] != t2$Contact.ID[i-1]) {
            exfile[curr,] <- ""
            exfile$Contact.ID[curr] <- as.character(hold$Contact.ID[1])
            exfile$Site.Address.City[curr] <- as.character(hold$Site.Address.City[1])
            exfile$Site.Address.Zip...4[curr] <- as.character(hold$Site.Address.Zip...4[1])
            exfile$Site.Street.Address[curr] <- as.character(hold$Site.Street.Address[1])
            exfile$Type.Of.Property.Owned[curr] <- as.character(hold$Type.Of.Property.Owned[1])
            
            curr <- curr + 1
            
            hold$Contact.ID[1] <- as.character(t2$Contact.ID[i])
            hold$Site.Address.City[1] <- as.character(t2$Site.Address.City[i])
            hold$Site.Address.Zip...4[1] <- as.character(t2$Site.Address.Zip...4[i])
            hold$Site.Street.Address[1] <- as.character(t2$Site.Street.Address[i])
        
    #check to see if number of units is not 1, then it's multifamily residence    
            if (t2$Number.of.Units[i] != 1 | is.na(t2$Number.of.Units[i])){
                hold$Type.Of.Property.Owned[1] <- "Multi Family"
            } else {
                hold$Type.Of.Property.Owned[1] <- "SFR"
            }
        
      
        } else {
            hold$Contact.ID[1] <- as.character(t2$Contact.ID[i])
            hold$Site.Address.City[1] <- paste(hold$Site.Address.City, t2$Site.Address.City[i], sep = "; ")
            hold$Site.Street.Address[1] <- paste(hold$Site.Street.Address, t2$Site.Street.Address[i], sep = "; ")
            hold$Site.Address.Zip...4[1] <- paste(hold$Site.Address.Zip...4, t2$Site.Address.Zip...4[i], sep = "; ")
            
        #A person who owns a complex is a multifamily owner, the family residence has no precident over it
        # if (hold$Type.Of.Property.Owned[1] == "Multi Family"){
        # }
        # else if (t2$Number.of.Units[i] != 1 | is.na(t2$Number.of.Units[i])){
        #     hold$Type.Of.Property.Owned[1] <- "Multi Family"
        # }
        
            
        #For multipicklist
            if (hold$Type.Of.Property.Owned[1] == "Multi Family"){
                if (t2$Number.of.Units[i] == 1 & is.na(t2$Number.of.Units[i]) == FALSE){
                    hold$Type.Of.Property.Owned[1] <- "SFR; Multi Family"
                } 
            } else if(hold$Type.Of.Property.Owned[1] == "SFR"){
                if (t2$Number.of.Units[i] != 1 | is.na(t2$Number.of.Units[i])){
                    hold$Type.Of.Property.Owned[1] <- "SFR; Multi Family"
                }
            }
        }
        if (i %% 1000 == 0){
            print(i)
        }
    }
    
    exfile[curr,] <- ""
    exfile$Contact.ID[curr] <- as.character(hold$Contact.ID[1])
    exfile$Site.Address.City[curr] <- as.character(hold$Site.Address.City[1])
    exfile$Site.Address.Zip...4[curr] <- as.character(hold$Site.Address.Zip...4[1])
    exfile$Site.Street.Address[curr] <- as.character(hold$Site.Street.Address[1])
    exfile$Type.Of.Property.Owned[curr] <- as.character(hold$Type.Of.Property.Owned[1])
    exfile
}