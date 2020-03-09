library("jpeg")
setwd("C:\\Users\\马文萱\\Desktop\\")
pic<-readJPEG("picture.jpg")
newpic<-pic
w=dim(pic)[1]
h=dim(pic)[2]
n=dim(pic)[3]
intImg<-array(0,dim=c(w,h,n))
s=w/8
t=10
for (i in 1:w)
{
  sum=0
  for (j in 1:h)
  {
    for(k in 1:n)
    {
      sum=sum+pic[i,j,k]
      if(i==1)
        intImg[i,j,k]=sum
      else
        intImg[i,j,k]=intImg[i-1,j,k]+sum
    }
  }
}
for (i in 1:w)
{
  for (j in 1:h)
  {
    for(k in 1:n)
    {
      x1<-max(i-floor(s/2), 2)
      x2<-min(i+floor(s/2), w)
      y1<-max(j-floor(s/2), 2)
      y2<-min(j+floor(s/2), h)
      z1<-max(k-floor(s/2), 2)
      z2<-min(k+floor(s/2), n)
      count<-(x2-x1)*(y2-y1)*(z2-z1)
      sum<-intImg[x2,y2,z2]-intImg[x1-1,y2,z2]-intImg[x2,y1-1,z2]-intImg[x2,y2,z1-1]+intImg[x1-1,y1-1,z2]+intImg[x2,y1-1,z1-1]+intImg[x1-1,y2,z1-1]-intImg[x1-1,y1-1,z1-1]
      if((pic[i,j,k]*count)<=(sum*(100-t)/100))
        newpic[i,j,k]<-0
      else
        newpic[i,j,k]<-1
    }
  }
}
writeJPEG(newpic,sprintf("picafter.jpg"))
