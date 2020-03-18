Auto <- read.table("//home/cim/pgt/mhac062/Documents/R/nci.data.txt", header=T, na.strings="?")
Auto<-scale(Auto, center=FALSE, scale=TRUE)
x<-t(Auto)
label<- read.table("//home/cim/pgt/mhac062/Documents/R/label.txt", na.strings="?")


hclustering <- function(data,method)
{
  lst<-list()
  for (i in 1:dim(data)[1])
  { lst[i]<-i  }
  u<-0
  v<-1
  vecA<-c()
  vecB<-c()
  print(c("Total number of clusters",length(lst)))
  
  
  while (length(lst)>1)
  {
    d1<-matrix(1, nrow =length(lst) , ncol = length(lst))
    for (i in 1:dim(d1)[1])
    {
      for (j in 1:dim(d1)[1])
      {
        
        if ((length(unlist(lst[i]))>1 | length(unlist(lst[j]))>1) & unlist(lst[i])!=unlist(lst[j]))
        {
          if (method=="single" | method=="complete"  | method=="average" )
          {
            d2<-matrix(1, nrow =length(unlist(lst[i])) , ncol = length(unlist(lst[j])))
            u<-0
            for (k in unlist(lst[i]))
            {
              u<-u+1
              v<-1
              for (l in unlist(lst[j]))
              {
                d2[u,v]<-dist(rbind(data[k,],data[l,]), method = "euclidean"  ) 
                v<-v+1
              }
            }
            if (method=="single")
            {      d1[i,j]<-min(d2[which(d2>0)])   }
            else if(method=="complete")
            { d1[i,j]<-max(d2[which(d2>0)])  }
            else if(method=="average")
            { d1[i,j]<-mean(d2[which(d2>0)])  }
            
          }  
          else if (method=="centroid")
            
          { vecA<-c()
          vecB<-c()
          
          for (y in unlist(lst[i]))
          {vecA<-rbind(vecA,data[y,])}
          for (z in unlist(lst[j]))
          {vecB<-rbind(vecB,data[z,])}
          d1[i,j]<-dist(rbind(rowMeans(vecA),rowMeans(vecB)), method = "euclidean"  )   }
          
        }
        else if (unlist(lst[i])==unlist(lst[j]))
        {      d1[i,j]<-0    }
        else
        {      d1[i,j]<-dist(rbind(data[unlist(lst[i]),],data[unlist(lst[j]),]), method = "euclidean"  )    }
        
        
      }}
    idx<-which(d1 == min(d1[which(d1>0)]), arr.ind = TRUE)[1,]
    lst[length(lst)+1]<-list(c(unlist(lst[idx[1]]),unlist(lst[idx[2]])))
    lst<-lst[-idx]
    print(c("Total number of clusters",length(lst)))
  }
  print(lst)}


hclustering(x,"centroid")
