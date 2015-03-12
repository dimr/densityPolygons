library(maptools)
library(classInt)
source("~/workspace/RData/getCoords.R")
source("~/workspace/RData/utilities.R")
setwd(".")

#single<-readShapePoly("~/Downloads/kalamaria/oikodomika_tetragwna/single.shp")
#kalamaria<-readShapePoly("~/Downloads/kalamaria/oikodomika_tetragwna/oikodomika_tetragwna.shp")

#single<-readShapePoly("/home/dimitris/workspace/RData/kalamaria/oikodomika_tetragwna/single.shp")
kalamaria<-readShapePoly("/home/dimitris/workspace/RData/kalamaria/oikodomika_tetragwna/oikodomika_tetragwna.shp")

bbox<-(expand.grid(as.vector(kalamaria@bbox[1,]),as.vector(kalamaria@bbox[2,])))
rotation.matrix <- function (degrees) {
  rad<-degrees.to.radians(degrees)
  rot<-matrix(c(cos(rad),-sin(rad),sin(rad),cos(rad)),nrow=2)
}


#----------bbox coordinates-----------
bottom.left<-c(bbox$Var1[1],bbox$Var2[1])
bottom.right<-c(bbox$Var1[2],bbox$Var2[2])
top.left<-c(bbox$Var1[3],bbox$Var2[3])
top.right<-c(bbox$Var1[4],bbox$Var2[4])

total.number.of.points<-110
#orizontia
xx.horizontal<-seq(bottom.left[1],bottom.right[1],length.out=total.number.of.points)
xx.vertical<-seq(bottom.left[2],bottom.right[2],length.out=total.number.of.points)

#ka8eti
#yy.horizontal<-seq(bottom.left[1],top.left[1],length.out=total.number.of.points)
#yy.vertical<-seq(bottom.left[2],top.left[2],length.out=total.number.of.points)

yy.horizontal<-seq(bottom.left[1],top.left[1],by=abs(xx.horizontal[1]-xx.horizontal[2]))
yy.vertical<-seq(bottom.left[2],top.left[2],by=abs(xx.horizontal[1]-xx.horizontal[2]))

final.grid<-(expand.grid(xx.horizontal,yy.vertical))
names(final.grid)<-c("longitude","latitude")
temp<-final.grid
coordinates(temp)<-c("longitude","latitude")

#--------------test
final.grid$polygon<-over(temp,kalamaria)$AROT
test.final.grid<-final.grid[which(!is.na(final.grid$polygon)),]


#------------------apo edo------------
test<-is.na(over(temp,kalamaria))
to.plot<-which(test!=TRUE)

size<-c(.2,.3,.4)
#points(temp[to.plot],pch=19,cex=size,col="red")
#-------------------
#addresses<-readShapePoints("~/Downloads/kalamaria/oikodomika_tetragwna/arithmhsh_dromwn.shp")
addresses<-readShapePoints("~/workspace/RData/kalamaria/oikodomika_tetragwna/arithmhsh_dromwn.shp")
ad.points<-as.data.frame(coordinates(addresses))
names(ad.points)<-c("longitude","latitude")
coordinates(ad.points)<-c("longitude","latitude")
ad.per.polygon<-as.data.frame(table(over(ad.points,kalamaria)))
names(ad.per.polygon)<-c("polygon","count")


m<-match(test.final.grid$polygon,ad.per.polygon$polygon)
test.final.grid$counts<-ad.per.polygon$count[m]
test.final.grid$cex<-mapValues(test.final.grid$counts,min(test.final.grid$counts),max(test.final.grid$counts),.2,.7)
brks<-cut(test.final.grid$counts,breaks=c(-1,2,15,41))
test.final.grid$brks<-as.numeric(brks)

plot.kalamaria<-function(){
X11()
plot(kalamaria,border=F,bg="#4F4F4F")
points(test.final.grid$longitude,test.final.grid$latitude,cex=mapValues(test.final.grid$brks,1,3,.2,.7),pch=19,col="orange")
}

plot.kalamaria()


rotate.grid<-function(angles){
  x.center<-kalamaria@bbox[1,1]+(kalamaria@bbox[1,2]-kalamaria@bbox[1,1])/2
  y.center<-kalamaria@bbox[2,1]+(kalamaria@bbox[2,2]-kalamaria@bbox[2,1])/2
  #print(paste( x.center),(y.center))
  # points(x.center,y.center,pch=19,cex=3)
  center<-c(x.center,y.center)
  trans<-matrix(c(final.grid$longitude-center[1],final.grid$latitude-center[2]),ncol=2)
  rad<-degrees.to.radians(angles)
  #rot<-matrix(c(cos(rad),-sin(rad),sin(rad),cos(rad)),nrow=2)
  trans.2<-trans%*%rotation.matrix(45)
  points(trans.2[,1]+center[1],trans.2[,2]+center[2],pch=19,cex=.2,col="orange")
}


rotate.kalamaria<-function(angles){
  x.center<-min(test.final.grid$longitude)+(max(test.final.grid$longitude)-min(test.final.grid$longitude))/2
  y.center<-min(test.final.grid$latitude)+(max(test.final.grid$latitude)-min(test.final.grid$latitude))/2

  #print(paste( x.center),(y.center))
  # points(x.center,y.center,pch=19,cex=3)
  center<-c(x.center,y.center)
  trans<-matrix(c(test.final.grid$longitude-center[1],test.final.grid$latitude-center[2]),ncol=2)
  rad<-degrees.to.radians(angles)
  rot<-matrix(c(cos(rad),-sin(rad),sin(rad),cos(rad)),nrow=2)
  trans.2<-trans%*%rot
  points(trans.2[,1]+center[1],trans.2[,2]+center[2],pch=19,cex=.2,col="orange")
}

