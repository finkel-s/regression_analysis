*PS2030
*BIVARIATE AND MULTIVARIATE REGRESSION 

***BIVARIATE REGRESSION

capture log close

log using "ps2030-2025.weeks2-3 regression.log", replace text 

*use "ps2030.bank-salaries.dta", clear

**Generate Scatterplot of Salary and Education Level

scatter salnow edlevel,jitter(20) || lfit salnow edlevel

***Bivariate Regression

regress salnow edlevel

***Generate Predicted Values of Y (YHAT) and Residuals (e)

predict predsal , xb  // "xb" is default

**Manual Version:

generate manpred=-7332.471+1563.963*edlevel

predict residsal, resid //ditto

**Manual Version:

generate manresid=salnow-manpred

*****List some cases

list salnow predsal manpred residsal manresid in 1/20

***Generate predicted values for specific X values, and plotting relationships with "margins" and "marginsplot"
margins, at (edlevel=12)
margins, at(edlevel=(8 12 20 21)) vsquish
marginsplot, recastci (rarea)

****Descriptive statistics on Y, Yhat, and Residuals

summarize salnow predsal residsal

***Do the errors of prediction correlate with any other variable?

tabstat residsal, sta(mean) by (sex)

**MULTIPLE REGRESSION


**Use South Africa Data
*use ps2030.safrica.dta, clear

summarize know times educ1

corr know times educ1

*BIVARIATE 
regress know times
regress know educ1

**MANUAL MULTIVARIATE REGRESSION 

**coefficient for educ1

regress know times
predict yresidtimes, resid

regress educ1 times
predict xresidtimes, resid

regress yresidtimes xresidtimes

***HAND CALCULATION

***HAND CALCULATION
display (1.943175/1.3665)*(.562-(.216*.122))/(1-.122^2) // Multivariate Effect of Education
display (1.943175/1.178149)*(.216-(.562*.122))/(1-.122^2) // Multivariate Effect of Civic Education


***BETA COEFFICIENTS AND CALCULATIONS

regress know times educ1   // this is the default
regress know times educ1, beta //this gives you the standardized regression coefficient "beta" instead of confidence intervals


***F* TESTS AND MODEL BUILDING: HYPOTHETICAL 'SOCIAL BACKGROUND' VERSUS 'MOTIVATIONAL' MODEL
regress know age1 educ1 church interest media2 efficacy   //THE 'FULL' MODEL
regress know interest media2 efficacy                     //REDUCED MODEL 1:  MOTIVATIONAL VARIABLES ONLY
regress know age1 educ1 church                            //REDUCED MODEL 2:  SOCIAL BACKGROUND VARIABLES ONLY

regress  know age1 educ1 church interest media2 efficacy
test age1 educ1 church                                   // THE F* FOR THE SOCIAL BACKGROUND MODEL
test interest media2 efficacy                           // THE F* FOR THE MOTIVATIONAL MODEL


*****BASIC RESIDUAL PLOTS

regress know times educ1
predict residknow, resid   //generates a new variable "residknow" that represents the residual from the multiple regression function
predict predknow  //generates a new variable "predknow" that represents the predicted value of Y from the multiple regression function

scatter residknow predknow,  yline(0) jitter(20) //this plots the residuals against the predicted values of Y
rvfplot, yline(0) jitter(20)         // this is the default STATA procedure even if you don't use "predict" to create residuals and predicted values

scatter residknow times, yline(0) jitter(20)  // this polots the residuals against one of the predictor X variables

****GENERATE A HISTOGRAM AND NORMAL PROBABILITY PLOT OF RESIDUALS

histogram residknow 

qnorm residknow

****OUTLIERS AND LEVERAGE TESTS:  LET'S USE THE COUNTRIES DATA HERE
*use ps2030.countries_2002.dta, clear
scatter life urban || lfit life urban
regress life urban
predict liferesid, resid
predict lifepred

**look at normality
hist liferesid, bin(20) // generates a histogram of residuals

qnorm liferesid // generates the normal probabiilty plot

ereturn list // GIVES YOU ALL OF THE SAVED VALUES AFTER ANY REGRESSION MODEL

gen standresid=liferesid/e(rmse)  // THIS GENERATES STANDARDIZED RESIDUALS, USING THE SAVED VALUE OF THE RMSE AS THE STANDARD DEVIATION OF THE RESIDUALS

scatter standresid urban, yline(0)
list country life lifepred liferesid standresid if (standresid <-2|standresid>2) // THIS GIVES ALL THE LARGEST RESIDUALS FOR EXAMINATION

dfbeta urban

generate dfbetacut=abs(_dfbeta_1)>2/sqrt(122)
list country life urban _dfbeta_1 if dfbetacut==1  //THIS GIVES THE CASES THAT HAVE UNDUE LEVERAGE ON THE REGRESSION COEFFICIENT FOR URBAN

regress life urban if dfbetacut==0  //THIS GIVES THE REGRESSION COEFFICIENT AFTER ELIMINATING THE THREE CASES WITH UNDUE LEVERAGE

regress life urban if _dfbeta_1>-.7  //THIS GIVES THE REGRESSION COEFFICIENT AFTER ELIMINATING ONLY BHUTAN, THE CASE WITH THE MOST LEVERAGE

log close


**********NOTE FOR HOMEWORK***********
**To select cases that are not missing on any list of variables, whether or not they are specified in a particular regression command
egen varname=rowmiss(var1 var2 var3 var4 var5)
tab varname
regress depvar var1 var2 if varname==0 /// This will give the regression of depvar on var1 and var2 for only those cases which are not missing on any of the var1 - var 4 variables


*****PLEASE BE SURE TO MAKE A NICE REGRESSION TABLE(S) - TRY USING "OUTREG" OR "ESTOUT"

