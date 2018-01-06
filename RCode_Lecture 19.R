###############################################################################
##Code for US Crime example. Adapted from Joe Schafer of the US Census Bureau##
###############################################################################

data<-read.table("uscrime.txt", header=TRUE, sep="")
attach(data)

############################################
##Plots of response against each predictor##
############################################

par(mfrow=c(3,3))
plot(Crime~M)
plot(Crime~So)
plot(Crime~Ed)
plot(Crime~Po1)
plot(Crime~Po2)
plot(Crime~LF)
plot(Crime~M.F)
plot(Crime~Pop)
plot(Crime~NW)

par(mfrow=c(2,3))

plot(Crime~U1)
plot(Crime~U2)
plot(Crime~Wealth)
plot(Crime~Ineq)
plot(Crime~Prob)
plot(Crime~Time)

############################################# Codes for all possible regression

##################################################
##STEP 1: Initiation variables. You will need to adjust the values in each commands of Step 1 if you wish to apply this code to other problems. 
##################################################

x<-cbind(Const=1, M, So, Ed, Po1, Po2, LF, M.F, Pop, NW, U1, U2, Wealth, Ineq, Prob, Time) ##Creat the design matrix, which consists of a column of 1s and the columns of all the predictors

y<-Crime ## Response Variable

predictor_names<-c("M", "So", "Ed", "Po1", "Po2", "LF", "M.F", "Pop", "NW", "U1", "U2", "Wealth", "Ineq", "Prob", "Time")  ## The names of predictors

number_predictor<-15 ## The total number of predictors 

n<-nrow(data) ## The total number of samples

###########################################################################################
##STEP 2: Creat a logical matrix to indicate the different models we are going to consider based on whether each predictor will be inlucded or not.
###########################################################################################

logic.list<-vector("list", number_predictor)

for( i in 1:number_predictor){
	
	logic.list[[i]]<-c(F,T)
}

#### The above command create a list of vectors, each vector in this list contains two values "TRUE" and "FALSE", indicating whether the corresponding variables will be included in the model or not. 

names(logic.list)<-predictor_names  ## Assign proper predictor names to each component in the list

models<-as.matrix(expand.grid(logic.list))
 
## This command creates a logical matrix by combinding all the "TRUE" and "FALSE" choice for each variables. 
## Each row of this logical matrix represents a possible model, and there are 2^number_predictor models in total 
## For instance, the first row of this matrix are consists of all "FALSE", and indicates a model that only includes the response variable. 
## The last row of this matrix are consists of all "TRUE", and indicates a model thad includes ALL the predictors. 


#################################################
##STEP 3: Creat the matrix for storing results based on the 7 different criteria
#################################################

results<-matrix(NA,2^number_predictor,7) ## The number of rows is the total number of models we will consider. Each column denotes a different criteria
dimnames(results)<-list(NULL, c("p","R2","R2.adj","PRESS","AIC","BIC","Cp")) ## Names of the 7 criteria

#################################
##STEP 4: Find out the MSE for the "maximum" model, model that includes all the predictors 
#################################

tmp<-lsfit(x,y,intercept=F)
MSE.max<-sum(tmp$res^2)/(n-number_predictor-1)

###################################
##STEP 5: Fit all possible models, and calculate the 7 model selection criteria for each model##
###################################

time1<-Sys.time()  ## Set up a timer

for (i in 1:(2^number_predictor)){ 
	
  which<-c(T, models[i,]) ## Determine which variables should be used in the i th model, also add the intercept term
  tmp<-lsfit(x[,which], y, intercept=F) ##fit the model and compute the criteria
  p<-sum(which) ##number of parameters for chosen model
  SSTo<-(n-1)*var(y)
  MSTo<-var(y)
  SSE<-sum(tmp$res^2)
  MSE<-SSE/(n-p)
  R2<-1-(SSE/SSTo)
  R2.adj<-1-(MSE/MSTo)
  hi<-ls.diag(tmp)$hat ##leverages
  res.PRESS<-tmp$res/(1-hi)
  PRESS<-sum(res.PRESS^2)
  AIC<-n*log(SSE/n)+2*p
  BIC<-n*log(SSE/n)+p*log(n)
  Cp<-(SSE/MSE.max)-n+2*p
  
  ## The model selection criteria will be stored in the matrix named results. Each row is for a different model and different columns refers to different criteria.
  
  results[i,1]<-p
  results[i,2]<-R2
  results[i,3]<-R2.adj
  results[i,4]<-PRESS
  results[i,5]<-AIC
  results[i,6]<-BIC
  results[i,7]<-Cp
  
  ##have R print out the iteration we are at
  print(paste("run",i))}

Sys.time()-time1 ##see how long it takes!

######################
##Step 6: Names each criteria accordingly. 
######################

p<-results[,1]
R2<-results[,2]
R2.adj<-results[,3]
PRESS<-results[,4]
AIC<-results[,5]
BIC<-results[,6]
Cp<-results[,7]

############################################# End of the codes for all possible regression



############################ Which model is the best?

#####################
##Plot R2 against p##
#####################

plot(p,R2)
identify(p,R2, cex=0.6) ##click on the point to identify the model number it corresponds to. press escape on R Console when you are done clicking!

#########################
##Plot R2.adj against p##
#########################

plot(p,R2.adj)
identify(p,R2.adj,cex=0.6)

########################
##Plot PRESS against p##
########################

plot(p,PRESS)
identify(p,PRESS,cex=0.6)

######################
##Plot AIC against p##
######################

plot(p,AIC)
identify(p,AIC,cex=0.6)

######################
##Plot BIC against p##
######################

plot(p,BIC)
identify(p,cex=0.6)

#####################
##Plot CP against p##
#####################

plot(p,Cp)
identify(p,Cp,cex=0.6)

###############################
##Find model with best R2.adj##
###############################

i1<-which.max(R2.adj) ##what is the index of the model with the max R2.adj
i1 ## print the index of the model
models[i1,] ## print the detail of the corresponding model


############################################
##Find model with best PRESS, AIC, BIC, Cp##
############################################


i2<-which.min(PRESS) ##which model has the min PRESS
i2
models[i2,]


i3<-which.min(AIC) ##which model has the min AIC
i3
models[i3,]


i4<-which.min(BIC) ##which model has the min AIC
i4
models[i4,]


i5<-which.min(Cp) ##which model has the min Cp
i5
models[i5,]



i2<-which.min(PRESS)
i2
i3<-which.min(AIC) 
i3


i4<-which.min(BIC) 
i4


i5<-which.min(Cp) 
i5


#########################
##general linear f test##
#########################

Model_13902<-lm(Crime~M+Ed+Po1+U2+Ineq+Prob+M.F+U1)
Model_13326<-lm(Crime~M+Ed+Po1+U2+Ineq+Prob)

anova(Model_13326,Model_13902)
