n<-100

z<-0
x<-0
y<-0
x<-rnorm(n)
z<-rnorm(n)
y<-2+x*z+rnorm(n)
mm<-0
zz<-0
tt<-0
ss<-0
aa<-0

a<--2
m<-1
s<-1
t<-1
l<-0

for(j in 1:20)
{
  zz<-((y-a)*x/s^2+m/t^2)/(x^2/s^2+1/t^2)
  vz<-1/(x^2/s^2+1/t^2)
  m<-mean(zz)
  mm[j]<-m
  s<-sqrt(sum((y-a)^2-2*(y-a)*x*zz+x^2*vz+x^2*zz^2)/n)
  ss[j]<-s^2
  t<-sqrt(sum(m^2-2*zz*m+zz^2+vz)/n)
  tt[j]<-t^2
  a<-mean(y-x*zz)
  aa[j]<-a
  
  l[j]<--0.5*sum(log(x^2*t^2+s^2)+(y-a-x*m)^2/(x^2*t^2+s^2))
  
}


