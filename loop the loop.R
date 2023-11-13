##change function 0-1
change=function(y)
{
if (y==0) {return (1)}
if (y==1) {return (0)}
}
##colour function
l_col=function(x){
if (x==0){return("black")}
if (x==1){return("blue")}
}
##line type function
l_type=function(x){
if (x==0){return(3)}
if (x==1){return(1)}
}
##main plotting starts
par(bg="yellow")
plot(0,0,type="n",xlim=c(0,1),ylim=c(0,1),,xlab="",ylab="",axes=FALSE)
title(sub=list("Click anywhere to start",cex=1.5,font=2),main=list("INFERENCIA",cex=4.5,font=4),col.main=rgb(1,.5,0,.9))
text(.2,1,"Presents",cex=1.5,font=3,col=rgb(0,.5,1,.9))
text(.7,1,"Stochast-o-Liga",cex=1.5,font=3,col=rgb(0,.5,1,.9))
text(.2,.75,"Round-3",cex=1.5,font=3,col=rgb(0,.5,.5,.7))
text(.8,.75,"Brain Games",cex=1.5,font=3,col=rgb(0,.5,.5,.7))
text(.5,.7,"Game-2",cex=1.5,font=3,col=rgb(0,.5,.5,.7))
text(.3,.5,"LOOP",col='green',cex=4,font=3)
text(.5,.4,"the",col='red',cex=3,font=3)
text(.7,.3,"LOOP",col='green',cex=4,font=3)
text(.5,0,"Start when you are asked to do so",cex=1.3)
locator(1)
time1=Sys.time()
plot(0,0,type="n",xlim=c(0,1),ylim=c(0,1),,xlab="",ylab="",axes=FALSE)
title(main="Loop the Loop",col.main="red")
for(i in seq(0,1,.2))
{
points(c(0,1),c(i,i),type="l",lwd=5,lty=3,col="black")
}
for(i in seq(0,1,.2))
{
points(c(i,i),c(0,1),type="l",lwd=5,lty=3,col="black")
}
##defining matrices
vert=matrix(0,6,5)
hori=matrix(0,5,6)
check=matrix(0,6,6)
give_arr=matrix(5,5,5)
xarray=c(1,2,1,2,3,4,5,1,2,5,3,4,5,1,3,4)
yarray=c(1,1,2,2,2,2,2,3,3,3,4,4,4,5,5,5)
values=c(1,2,2,2,3,2,3,3,0,2,0,2,3,1,2,2)
for (i in 1:length(xarray)){
give_arr[xarray[i],yarray[i]]=values[i]
text((2*xarray[i]-1)/10,(2*yarray[i]-1)/10,paste(as.character(values[i])),cex=2,col='blue')
}
take_arr=matrix(0,5,5)
##defining constants
cond1=0
cond2=0
##starting of the main loop
while(cond1==0|cond2==0)
{
line=locator(1)
x=line$x*5+1
y=line$y*5+1
xint=floor(x)
yint=floor(y)
if (x-xint>.9 | x-xint<.1)
{
if(x-xint>.9){xint1=xint+1}
if(x-xint<.1){xint1=xint}
if (y-yint>.9 | y-yint<.1){}
else
{
vert[xint1,yint]=change(vert[xint1,yint])
check[xint1,yint]=check[xint1,yint]+2*(vert[xint1,yint]-.5)
check[xint1,yint+1]=check[xint1,yint+1]+2*(vert[xint1,yint]-.5)
points(c((xint1-1)/5,(xint1-1)/5),c((yint-1)/5,yint/5),col="yellow",type="l",lwd=5)
points(c((xint1-1)/5,(xint1-1)/5),c((yint-1)/5,yint/5),col=l_col(vert[xint1,yint]),type="l",lwd=5,lty=l_type(vert[xint1,yint]))
if(xint1==1)
{
take_arr[xint1,yint]=take_arr[xint1,yint]+2*(vert[xint1,yint]-.5)
}
else if(xint1==6)
{
take_arr[xint1-1,yint]=take_arr[xint1-1,yint]+2*(vert[xint1,yint]-.5)
}
else
{
take_arr[xint1-1,yint]=take_arr[xint1-1,yint]+2*(vert[xint1,yint]-.5)
take_arr[xint1,yint]=take_arr[xint1,yint]+2*(vert[xint1,yint]-.5)
}
}
}
if (y-yint>.9 | y-yint<.1)
{
if(y-yint>.9){yint1=yint+1}
if(y-yint<.1){yint1=yint}
if (x-xint>.9 | x-xint<.1){}
else
{
hori[xint,yint1]=change(hori[xint,yint1])
check[xint,yint1]=check[xint,yint1]+2*(hori[xint,yint1]-.5)
check[xint+1,yint1]=check[xint+1,yint1]+2*(hori[xint,yint1]-.5)
points(c((xint-1)/5,xint/5),c((yint1-1)/5,(yint1-1)/5),col="yellow",type="l",lwd=5)
points(c((xint-1)/5,xint/5),c((yint1-1)/5,(yint1-1)/5),col=l_col(hori[xint,yint1]),type="l",lwd=5,lty=l_type(hori[xint,yint1]))
if(yint1==1)
{
take_arr[xint,yint1]=take_arr[xint,yint1]+2*(hori[xint,yint1]-.5)
}
else if(yint1==6)
{
take_arr[xint,yint1-1]=take_arr[xint,yint1-1]+2*(hori[xint,yint1]-.5)
}
else
{
take_arr[xint,yint1]=take_arr[xint,yint1]+2*(hori[xint,yint1]-.5)
take_arr[xint,yint1-1]=take_arr[xint,yint1-1]+2*(hori[xint,yint1]-.5)
}
}
}
temp1=0
for(i in 1:6)
{
for(j in 1:6)
{
if(check[i,j]!=0 & check[i,j]!=2)
{
temp1=1
break
break
}
}
}
if(temp1==0){cond1=1}
else{cond1=0}

temp2=0
for(i in 1:5)
{
for(j in 1:5)
{
if(give_arr[i,j]!=5)
{
if(give_arr[i,j]!=take_arr[i,j])
{
temp2=1
break
break
}
}
}
}
if(temp2==0){cond2=1}
else{cond2=0}
}
time2=Sys.time()
time=as.numeric(time2-time1,"secs")
t_min=floor(time/60)
t_sec=floor(time-t_min*60)
if(cond1==1 & cond2==1){
if (t_min<5){
score=50
} else if (t_min<10){score=50-(t_min-4)*5
} else {score=0}
} else {score=0}
par(bg='yellow')
plot(0,0,type='n',xlab="",ylab="",xlim=c(0,1),ylim=c(0,1),axes=FALSE,frame.plot=FALSE)
title(main=list("SCORE CARD",cex=3,font=7))
#text(.5,.8,paste('Number of problem solved = ',solve),col='green',cex=1.8,font=3)
text(.5,.5,paste('Total time spent = ',t_min,' min ',t_sec," secs"),col='red',cex=1.5,font=3)
text(.5,.2,paste('Total Score = ',score),col='blue',cex=1.8,font=3)
