set.seed(1)
#defining constants
r=3 ##number of points to be matched
t=1 ##number of points that will appear every move
f_limit=40 ## number of times participant can fail
failure=0
func_num=1
delta=.02
sltfg=5
##starting page
par(bg="yellow")
plot(0,0,type="n",xlim=c(0,1),ylim=c(0,1),,xlab="",ylab="",axes=FALSE)
title(sub=list("Click anywhere to start",cex=1.5,font=2),main=list("INFERENCIA",cex=4.5,font=4),col.main=rgb(1,.5,0,.9))
text(.2,1,"Presents",cex=1.5,font=3,col=rgb(0,.5,1,.9))
text(.7,1,"Stochast-o-Liga",cex=1.5,font=3,col=rgb(0,.5,1,.9))
text(.5,.75,"Round-4",cex=1.5,font=3,col=rgb(0,.5,.5,.7))
text(.3,.5,"POINT",col='GREEN',cex=5,font=3)
text(.7,.3,"HEIST",col='GREEN',cex=5,font=3)
text(.5,0,"Start when you are asked to do so",cex=1.3)
locator(1)
## making the grid
plot_frame=function(t,t1){
mycol=rgb(0,0,0,t)
par(bg=mycol,cex.sub=1.5,col.axis="white")
plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xlab="",ylab="")
title(main=t1,col.main="white")
axis(1,col="white")
axis(2,col="white")
grid(nx = 50, ny = 50)
}
##defining functions
fun=function(i,x){
if (i==1) return (x)
if (i==2) return (x^2)
if (i==3) return (x^4)
if (i==4) return (.3+.9*x*sin(4*x))
if (i==5) return (.4+.8*x*sin(6*x^2-1))
if (i==6) return (.25+.5*x*(sin(5*x))^2)
if (i==7) return (.25*exp(sin(4*x*sin(3*x))))
if (i==8) return (.1*sin(15*x)+x^2+.1)
if (i==9) return (.1+.7*(sin(6*x))^2)
if (i==10) return (sin(10*pi*x)/2+.5)
}
##functon names
fun_name=function(i){
if (i==1) title(sub=expression("the function is y = " * x),col.sub="white")
if (i==2) title(sub=expression("the function is y = " * x^2),col.sub="white")
if (i==3) title(sub=expression("the function is y = " * x^4),col.sub="white")
if (i==4) {title(sub=expression("the function is y = .3+.9 x sin(4x)"),col.sub="white")}
if (i==5) {title(sub=expression("the function is y = .4+.8 x sin(6" * x^2 * "-1)"),col.sub="white")}
if (i==6) {title(sub=expression("the function is y= .25 + .5 x " * sin^2 * "(5x)"),col.sub="white")}
if (i==7) {title(sub=expression("the function is y= .25 " * e^"sin(4xsin(3x))"),col.sub="white")}
if (i==8) {title(sub=expression("the function is y= .1 sin(15x) +" * x^2 * "+.1"),col.sub="white")}
if (i==9) {title(sub=expression("the function is y= .1 + .7" * sin^2 * "(6x)"),col.sub="white")}
if (i==10) {title(sub=expression("the function is y=" * frac("sin(10" * pi * "x)",2) * " + .5"),col.sub="white")}
}
##showing graph
fun_plot=function(num){
iter=500
xval=seq(0,1,length.out=iter)
yval=c()
for (j in 1:iter){
yval=c(yval,fun(num,xval[j]))
}
points(xval,yval,col="white",lwd=5,type="l")
}
##distance function
distance=function(i,x,y,delta){
if ((y-fun(i,x))^2<delta^2){return(1)}
else if((y-fun(i,x-delta))*(y-fun(i,x))<0){return(1)}
else if((y-fun(i,x+delta))*(y-fun(i,x))<0){return(1)}
else {return(0)}
}
##block function
block=function(a,b,x,y){
val=1
for (i in 1:length(a)){
if ((x-a[i])^2+(y-b[i])^2<25*delta^2){val=0}
}
return (val)
}
############################
##############################
##giving inputs and take outputs
solve=0
score=0
##starting of first while loop
while (func_num <=10 && failure<f_limit){
f_index=(f_limit-failure/2)/f_limit
f1_limit=3+2*log(func_num,1.8)
title1=paste("FUNCTION -",func_num,"\nThis function will disappear after ",floor(f1_limit)+1," red dots")
title2=paste("Attempts left ",f_limit-failure)
plot_frame(f_index,title1)
title(sub=title2,col.sub="black")
success=0
failure1=0
xarray=c()
yarray=c()
##starting of second while loop
while ((success<r && failure<f_limit) && failure1<=f1_limit){
xvalue=runif(t)
xarray=c(xarray,xvalue)
yvalue=fun(func_num,xvalue)
yarray=c(yarray,yvalue)
#points(xvalue,yvalue,type="p",lwd=19,col=rgb(0,0,0,1))
points(xvalue,yvalue,pch=16,col='white')
val=0
while(val==0 && success<r)
{
in_pt=locator(1)
if (block(xarray,yarray,in_pt$x,in_pt$y)==1){
if(distance(func_num,in_pt$x,in_pt$y,delta)==1)
{
success=success+1
xarray=c(xarray,in_pt$x)
yarray=c(yarray,in_pt$y)
mycol1=rgb(0,1,0,f_index)
#points(in_pt$x,in_pt$y,type="p",lwd=19,col=rgb(0,0,0,1))
points(in_pt$x,in_pt$y,pch=16,col=mycol1)
}
else{
val=1
failure=failure+1
failure1=failure1+1
score=score-.5
mycol2=rgb(1,0,0,f_index)
points(in_pt$x,in_pt$y,pch=16,col=mycol2)
}
}
}
if (success==r){
solve=solve+1
score=score+1.9*func_num
}
}
title1=paste("FUNCTION -",func_num)
plot_frame(f_index,title1)
fun_name(func_num)
fun_plot(func_num)
Sys.sleep(sltfg)
func_num=func_num+1
}
par(bg='yellow')
plot(0,0,type='n',xlab="",ylab="",xlim=c(0,1),ylim=c(0,1),axes=FALSE,frame.plot=FALSE)
title(main=list("SCORE CARD",cex=3,font=7))
text(.5,.8,paste('Number of problem solved = ',solve),col='green',cex=1.8,font=3)
text(.5,.5,paste('Number of wrong attempts = ',failure),col='red',cex=1.8,font=3)
text(.5,.2,paste('Total Score = ',score),col='blue',cex=1.8,font=3)
