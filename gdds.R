fp=read.csv("BLOOMBERG.csv",header = TRUE)
fp
y=fp[,8]
x1=fp[,3]
x2=fp[,4]
x3=fp[,5]
x4=fp[,6]
model1=glm(y~x2+x4+x3,family="binomial")
ref=glm(y~1,family="binomial")
anova(ref,model1,test="Chisq")
summary(model1)
predictedprob=predict.glm(model1,type="response")
head(predictedprob)
predictedprob
cc=cbind(y,predictedprob)
head(cc)
c0=0
c1=0
z0=rep(0,10)
z1=rep(0,10)
for(i in 1: length(y))
{
  if(y[i]==0)
  {
    c0=c0+1
    z0[c0]=predictedprob[i]
  }
  else
  {
    c1=c1+1
    z1[c1]=predictedprob[i]
  }

}
plot(density(z0),col="red")
lines(density(z1),col="blue")
tsh=0.95
z=rep(0,length(y))

for(i in 1:length(y))
{
  if(predictedprob[i]<tsh)
  {
  z[i]=0
  }
  else 
  {
    z[i]=1
  }
}
table(y,z)
