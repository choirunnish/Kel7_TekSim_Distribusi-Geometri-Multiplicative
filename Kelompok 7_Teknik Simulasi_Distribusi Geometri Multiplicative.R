####Tugas 2 Teknik Simulasi####
###Multiplicative RNG dan bilangan acak geometrik dengan fungsi inverse transformation method###
##Kelompok 7##
##Kamalina Rosyida Supriyanti B2A020062
##Shella Heidy Permatasari B2A020067
##Choirunnisa Hasna B2A020071

#Diketahui a = 45, z0 = 21139, m = 417, n = 150

multiplicative_RNG<-function(a,z0,m,n) {
  xj<-matrix(NA,n,3)
  colnames(xj)<-c("aZ","Xj","Uj")
  for (j in 1:n)
  {
    xj[j,1]<-(a*z0)
    xj[j,2]<-xj[j,1]%%m
    xj[j,3]<-xj[j,2]/m
    z0<-xj[j,2]
  }
  hist(xj[,3]) 
  View(xj)
  p<-0.83
  R<-xj[,3]
  X<-log(1-R)/log(1-p)
  print(X)
  hist(X)
  
}
multiplicative_RNG(45,21139,417,150)