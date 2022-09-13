
######### 1
R = 1               ###  radius of circle
n = 100             #### Population size
alpha = 0.3         #### probability of catching the infection
beta=0.4           ##    probability of recovering from the infection
gama=0.2           ####  probability of losing immunity
days=100
x = runif(n,-R,R)
y= runif(n,-R,R)
############### 2
plot(x,y, pch = 19, bty = 'n', axes = F, xlab = '', ylab = '', xlim = c(-R,R), ylim = c(-R,R))
curve(sqrt(R^2-x^2),-R,R,add=T)
curve(-sqrt(R^2-x^2),-R,R,add=T)
############# 3 An agent who transgresses outside is teleported to an
##  arbitraty location inside.
Outer_point=which(x^2+y^2>R^2)
for(i in Outer_point)
{
  
if(x[i]<0)
{
x[i]=x[i]+R
    
}  
else
{
x[i]=x[i]-R  
    
}  
if(y[i]<0)
{
y[i]=y[i]+R
    
}  
else
{
y[i]=y[i]-R  
    
}
}

plot(x,y, pch = 19, bty = 'n', axes = F, xlab = '', ylab = '', xlim = c(-R,R), ylim = c(-R,R))
curve(sqrt(R^2-x^2),-R,R,add=T)
curve(-sqrt(R^2-x^2),-R,R,add=T)
v1=c(which(x^2+y^2>R^2))
######### Initialization
S0 = n - 2      ## Susceptible
I0 = 2          ## Infected
R0 = 0          ## Recovered
state = c( rep(3,S0),  rep(2, I0),  rep(4, R0))
############ 
sucsesptible=c()
infected=c()
recovred=c()
for(d1 in 1:days)
{
SO=length(which(state==3))
IO=length(which(state==2))
RO=length(which(state==4))
#state = c( rep(3,S0),  rep(2, I0),  rep(4, R0))
length(which(state==3))
#print(state)
plot(x,y, pch = 19, bty = 'n', axes = F, xlab = '', ylab = '', col =
       state, xlim = c(-R,R), ylim = c(-R,R))
curve(sqrt(R^2-x^2),-R,R,add=T)
curve(-sqrt(R^2-x^2),-R,R,add=T)
############ Each agent performs a random walk.
sd = 0.05
x = x + rnorm(n,0,sd)
y = y + rnorm(n,0,sd)
########## 7
#state = c( rep(3,S0),  rep(2, I0),  rep(4, R0))
plot(x,y, pch = 19, bty = 'n', axes = F, xlab = '', ylab = '', col =
       state, xlim = c(-R,R), ylim = c(-R,R))
curve(sqrt(R^2-x^2),-R,R,add=T)
curve(-sqrt(R^2-x^2),-R,R,add=T)
Outer_point=which(x^2+y^2>R^2)
for(i in Outer_point)
{
if(x[i]<0)
{
x[i]=x[i]+R
}  
else
{
x[i]=x[i]-R  
    
}  
if(y[i]<0)
{
y[i]=y[i]+R
    
}  
else
{
y[i]=y[i]-R  
    
}
}

plot(x,y, pch = 19, bty = 'n', axes = F, xlab = '', ylab = '', col =
       state, xlim = c(-R,R), ylim = c(-R,R))
curve(sqrt(R^2-x^2),-R,R,add=T)
curve(-sqrt(R^2-x^2),-R,R,add=T)
####### 8 ,9
r = 0.5    ############## infection radius
statePossible = which(state == 2)
IO=length(statePossible)
################### an S-agent becomes an I-agent with probability ??
for (i in 1:I0){
d = c()
for (j in 1:n){
d[j] = sqrt((x[j]-x[statePossible[i]])^2 +(y[j]-y[statePossible[i]])^2)
}
PossibleInf = which(d<r)
#print(PossibleInf)
if(length(PossibleInf)>0){
for (j in 1:length(PossibleInf)){
if((state[PossibleInf[j]] == 3) & (PossibleInf[j] !=statePossible[i])){
if(sample(c(1,0),1,prob = c(alpha, 1-alpha)) ==1){
state[PossibleInf[j]] = 2
}
}
}
}
}
#state = c( rep(3,S0),  rep(2, I0),  rep(4, R0))
plot(x,y, pch = 19, bty = 'n', axes = F, xlab = '', ylab = '', col =
       state, xlim = c(-R,R), ylim = c(-R,R))
curve(sqrt(R^2-x^2),-R,R,add=T)
curve(-sqrt(R^2-x^2),-R,R,add=T)
######### An I become an R with probability ??
inf = which(state == 2)
if(length(inf)>0){
for (j in 1:length(inf))
{
if(state[inf[j]] == 2)
{
if(sample(c(1,0),1,prob = c(beta, 1-beta)) ==1)
{
state[inf[j]] = 4
}
}
}
}
plot(x,y, pch = 19, bty = 'n', axes = F, xlab = '', ylab = '', col =
       state, xlim = c(-R,R), ylim = c(-R,R))
curve(sqrt(R^2-x^2),-R,R,add=T)
curve(-sqrt(R^2-x^2),-R,R,add=T)

############  An R become an S with probability ??.
recovered= which(state == 4)
for (j in 1:length(recovered))
{
if(state[recovered[j]] == 4)
{
if(sample(c(1,0),1,prob = c(gama, 1-gama)) ==1)
{
state[recovered[j]] = 3
}
}
}

plot(x,y, pch = 19, bty = 'n', axes = F, xlab = '', ylab = '', col =
       state, xlim = c(-R,R), ylim = c(-R,R), main = paste("Day : ", d1))
curve(sqrt(R^2-x^2),-R,R,add=T)
curve(-sqrt(R^2-x^2),-R,R,add=T)

sucsesptible[d1]=length(which(state==3))
infected[d1]=length(which(state==2))
recovred[d1]=length(which(state==4))
Sys.sleep(0.5)
}
################
print(sucsesptible)
print(infected)
print(recovred)
plot(1:days,sucsesptible,type="l",xlab="Time(Days)",ylab="Count",main="SIR Model",col="green",xlim=c(0,days),ylim=c(0,n),sub="for alpha=0.3 & beta=0.4 & gamma=0.2")
lines(1:days,infected,col="red")
lines(1:days,recovred,col="blue")
legend(locator(1),legend=c("sucsesptible","infected","recovred"),fill=c("green","red","blue"))
###################################

