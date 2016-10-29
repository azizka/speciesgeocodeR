library(ggmap)

#function definition

coder <- function(x){
  resu <- ggmap::geocode(as.character(x[1]), output = "all" , source = "google")
  
  if(resu$status == "OK"){
    out <- data.frame(resu$results[[1]]$geometry$location$lng,
                      resu$results[[1]]$geometry$location$lat,
                      resu$results[[1]]$formatted_address)
    names(out) <- c("long", "lat", "address")
  }else{
    out <- c(NA,NA,NA)
    names(out) <- c("long", "lat", "address")
  }
  return(out)
}

#botanical gardens
gards <- read.csv("C:/Users/alexander.zizka/Dropbox (Antonelli Lab)/Arbeit/Gothenburg/projects/3_speciesgeocoder/speciesgeocodeR/BGCI-Gardens_BCGI_members.csv", 
                  sep = ",", fileEncoding = "UTF-8")

gards$Institution <- gsub("\"", "'", gards$Institution)

gards$Institution <- gsub("í", "i", gards$Institution)
gards$Institution <- gsub("á", "a", gards$Institution)
gards$Institution <- gsub("é", "e", gards$Institution)
gards$Institution <- gsub("ö", "o", gards$Institution)
gards$Institution <- gsub("ä", "a", gards$Institution)
gards$Institution <- gsub("ü", "u", gards$Institution)
gards$Institution <- gsub("ç", "c", gards$Institution)
gards$Institution <- gsub("ã", "a", gards$Institution)
gards$Institution <- gsub("à", "a", gards$Institution)
gards$Institution <- gsub("â", "a", gards$Institution)
gards$Institution <- gsub("ñ", "n", gards$Institution)
gards$Institution <- gsub("è", "e", gards$Institution)
gards$Institution <- gsub("ó", "o", gards$Institution)
gards$Institution <- gsub("ô", "o", gards$Institution)
tools::showNonASCII(gards$Institution)

gards.cod <- apply(gards, 1, "coder")
gards.cod.out <- data.frame(gards, do.call("rbind.data.frame", gards.cod))
BGCI <- gards.cod.out

save(BGCI, file = "BGCI_botanical_gardens.Rdata")

#herbaria
herb <- read.csv("C:/Users/alexander.zizka/Dropbox (Antonelli Lab)/Arbeit/Gothenburg/projects/3_speciesgeocoder/speciesgeocodeR/index_herbariorum.csv")
herb <- herb[herb$Address != "",]
#manually curate the list


#Biodiversity institutions from GrBio, http://grbio.org/
grbio <- read.csv("C:/Users/alexander.zizka/Dropbox (Antonelli Lab)/Arbeit/Gothenburg/projects/3_speciesgeocoder/speciesgeocodeR/grbio_institutions.csv")
grbio <- grbio[,c("Institution.Name", "Mailing.Address.1", "Mailing.Address.2", "Mailing.Address.3", "City.Town.1")]

ser.grbio <- data.frame(paste(as.character(grbio[,1]), as.character(grbio[,5]), sep = ", "))
out.grbio.temp <- apply(data.frame(as.character(ser.grbio[1:2500,])), 1, "coder")

write.table(out.grbio.temp, "grbio_temp_1.txt", sep = "\t")

#GBIF INStitutions





