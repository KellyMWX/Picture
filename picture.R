library("jpeg")
setwd("C:\\Users\\马文萱\\Desktop\\")
picture<-readJPEG("picture.jpg")
pic<-0.299*picture[,,1]+0.587*picture[,,2]+0.114*picture[,,3]
newpic<-pic
dim(pic)
w=dim(pic)[1]
h=dim(pic)[2]
intImg<-matrix(0,nrow=w,ncol=h)
s=w/8
t=15
for (i in 1:w)
{
  sum=0
  for (j in 1:h)
  {
    sum=sum+pic[i,j]
    if(i==1)
      intImg[i,j]=sum
    else
      intImg[i,j]=intImg[i-1,j]+sum
  }
}
for (i in 1:w)
{
  for (j in 1:h)
  {
    x1<-max(i-floor(s/2), 2)
    x2<-min(i+floor(s/2), w)
    y1<-max(j-floor(s/2), 2)
    y2<-min(j+floor(s/2), h)
    count<-(x2-x1)*(y2-y1)
    sum<-intImg[x2,y2]-intImg[x2,y1-1]-intImg[x1-1,y2]+intImg[x1-1,y1-1]
    if((pic[i,j]*count)<=(sum*(100-t)/100))
      newpic[i,j]<-0
    else
      newpic[i,j]<-1
  }
}
writeJPEG(newpic,sprintf("pic_after.jpg"))
