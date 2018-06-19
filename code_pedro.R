library(XML)
library(bitops)
library(RCurl)
latlon2ft <- function(origin,destination){
  xml.url <- paste0('http://maps.googleapis.com/maps/api/distancematrix/xml?origins=',origin,'&destinations=',destination,'&mode=driving&sensor=false')
  xmlfile <- xmlParse(getURL(xml.url))
  dist <- xmlValue(xmlChildren(xpathApply(xmlfile,"//distance")[[1]])$value)
  distance <- as.numeric(sub(" km","",dist))
  return(distance)
}

transit_data <- transit_relacio_trams
#remove id:530. no coordinates
transit_data<-transit_data[-487,]

correct_version<-NULL
for(i in seq(1, 490)){
  dat1 <- t(data.frame(do.call(rbind, strsplit(transit_data$Coordenades[i], split = " "))))
  dat2 <- data.frame(do.call(rbind, strsplit(dat1, split = ",")))
  colnames(dat2) <- c("Long", "Lat")
  dat2 <- dat2[,c(2,1)]
  dat2$combined2<-paste(dat2$Lat, dat2$Long, sep="+")
  correct_version[[i]]<-dat2
  }

#-----------------------------------------------------------------------
#distance matrix one


dimensions<-NULL

for(i in seq(1,490)){
  dimensions[i]<-dim(correct_version[[i]])[1]
}

#eliminate the ones with dimension of 2

morethan_2<-as.vector(which(dimensions!=2))

sum_mins<-numeric(6)
for(i in head(morethan_2)){
  length_i<-dim(correct_version[[i]])[1]
  g<-gmapsdistance(origin = correct_version[[i]]$combined2, destination = correct_version[[i]]$combined2, combinations = "all", mode="walking",
                   key="AIzaSyCbuAK_ZyIvyeyd1OLqzheRh-ok1xIRW_c")
  distance_matrix<-data.frame(g$Distance)[,-1]
  mins<-NULL
  for(j in seq(1, length_i-1)){
    mins[j]<-min(distance_matrix[,j][which(distance_matrix[,j] > 0)])
  }
  sum_mins[i]<-sum(mins)
}
sum_mins





#------------------------------------------------------------------------
#storing distances and time differences
whole_list_dist<-NULL
whole_list_time<-NULL

for(i in seq(401,490)){  
  length_list <- dim(correct_version[[i]])[1]
  list_dist<-NULL
  # list_time<-NULL
  
  if(length_list==2)
  {
    sum_whole<-gmapsdistance(origin = correct_version[[i]]$combined2[1], destination = correct_version[[i]]$combined2[2], mode = "walking",key="AIzaSyBBgjRDMSrBXOarbUaEiJ-C_fVWfChPiLk")
    whole_list_dist[i]<-sum_whole$Distance
    # whole_list_time[i]<-sum_whole$Time
  }else
  {
    for(j in seq(1, length_list-1)){
      sum_whole<-gmapsdistance(origin = correct_version[[i]]$combined2[j], destination = correct_version[[i]]$combined2[j+1], mode = "walking",key="AIzaSyBBgjRDMSrBXOarbUaEiJ-C_fVWfChPiLk")
      list_dist[j]<-sum_whole$Distance
      # list_time[j]<-sum_whole$Time
      
    }
    whole_list_dist[i]<-sum(list_dist)
    # whole_list_time[i]<-sum(list_time)
  }
  
}





