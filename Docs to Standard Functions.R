BeverlieToStandard <- function(t, section = ""){
  library(tidyr)
  t <- separate(data = t, col = SITUS.FULL.ADDRESS, into = c("Site.Address.Street", "a", "b"), sep = ",")
  data.frame("Owner.Name"=t$OWNER.MAILING.NAME, "Site.Address.Street"=t$Site.Address.Street, "Site.Address.City"=t$SITUS.CITY, "Site.Address.State"= t$SITUS.STATE,
             "Site.Address.Zip"=t$SITUS.ZIP.CODE, "Mailing.Address.Street"=t$MAILING.STREET.ADDRESS, "Mailing.Address.City"=t$MAIL.CITY, 
             "Mailing.Address.State" = t$MAIL.STATE,"Mailing.Address.Zip" =t$MAIL.ZIP.ZIP.4, "Bedrooms" = t$NUMBER.OF.BEDROOMS, 
             "Bathrooms" = t$NUMBER.OF.BATHS, "Building.Area" = t$LIVING.AREA, "Lot.Area" = t$LOT.AREA, "Year.Built" = t$YEAR.BUILT, 
             "Sale.Price" = t$OT.SALE.PRICE, "Sale.Date" = t$OT.SALE.DATE, "Num.Units" = t$NO..RESIDENTIAL.UNITS, 
             "Zoning" = t$ZONING, "Use.Code" = t$LAND.USE, "Parcel"=t$APN...FORMATTED, "Section" = section, stringsAsFactors = FALSE)
}
GrantToStandard <- function(t, section = ""){
  t$Use.Code.Description[which(t$Use.Code.Description == "Single Family Residential")] <- "SFR" 
  data.frame("Owner.Name" = t$Owner.Name, "Site.Address.Street"=t$Full.Site.Address, "Site.Address.City"=t$Site.Address.City, "Site.Address.Zip"=t$Site.Address.Zip.4, 
             "Site.Address.State" = t$Site.Address.State, "Mailing.Address.Street"=t$Full.Mail.Address, 
             "Mailing.Address.City"=t$Mail.Address.City, "Mailing.Address.State" = t$Mail.Address.State, 
             "Mailing.Address.Zip" =t$Mail.Address.Zip.4, "Bedrooms" = t$Bedrooms, "Bathrooms" = t$Bathrooms, "Building.Area" = t$Building.Area,
             "Lot.Area" = t$Lot.Area.SQFT, "Year.Built" = t$Year.Built, "Sale.Price" = t$Sales.Price, "Sale.Date" = t$Sale.Date, 
             "Num.Units" = t$Number.of.Units,"Zoning" = t$Zoning, "Use.Code" = t$Use.Code.Description, "Parcel" = t$Parcel.Number, "Section" = section, stringsAsFactors = FALSE)
}
RomyToStandard <- function(t, section = ""){
  t$Use.Code.Description[which(t$Use.Code.Description == "Single Family Residential")] <- "SFR" 
  data.frame("Owner.Name" = t$Owner.Name2, "Site.Address.Street"=t$Full.Site.Address, "Site.Address.City"=t$Site.Address.City, "Site.Address.Zip"=t$Site.Address.Zip.4, 
             "Site.Address.State" = t$Site.Address.State, "Mailing.Address.Street"=t$Full.Mail.Address, 
             "Mailing.Address.City"=t$Mail.Address.City, "Mailing.Address.State" = t$Mail.Address.State, 
             "Mailing.Address.Zip" =t$Mail.Address.Zip.4, "Bedrooms" = t$Bedrooms, "Bathrooms" = t$Bathrooms, "Building.Area" = t$Building.Area,
             "Lot.Area" = t$Lot.Area.SQFT, "Year.Built" = t$Year.Built, "Sale.Price" = t$Sales.Price, "Sale.Date" = t$Sale.Date, 
             "Num.Units" = t$Number.of.Units,"Zoning" = t$Zoning, "Use.Code" = t$Use.Code.Description, "Parcel" = t$Parcel.Number, "Section" = section, stringsAsFactors = FALSE)
}
