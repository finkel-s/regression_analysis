*POL2030
*Weeks 6-7 Error Term Violations:  Heteroskedasticity and Autocorrelation


******************************************HETEROSKEDASTICITY**********************************************

*use "heteroskedasticity.example.dta"

list  // 12 cases, GNP per Capita and Per Capita Welfare Expenditures

**RUN REGRESSION AND LOOK AT SCATTERPLOT OF RESIDUALS

regress welfexp gnppc

rvfplot, yline(0) // APPARENTLY LARGE DEGREE OF HETEROSKEDASTICITY PRESENT

*TESTS FOR STATISTICAL SIGNIFICANCE OF HETEROSKEDASTICITY

*1.  GOLDFIELD-QUANDT TEST IF RESIDUAL VARIANCE RELATED MONOTONICALLY TO X 

tab gnppc  //SO LET'S TAKE CASES LESS THAN 4 AND GREATER THAN 6 FOR OUR TEST

regress welfexp gnppc if gnppc<4
regress welfexp gnppc if gnppc>6

*SO GOLDFIELD-QUANT TEST IS  124.6/7.31, WITH DEGREES OF FREEDOM (2,3)
display 124.6/7.31  // CRITICAL VALUE IS 9.55, SO WE REJECT H0 OF EQUAL RESIDUAL VARIANCE

*2.  WHITE'S TEST IF IT IS UNKNOWN HOW RESIDUAL VARIANCE MAY RELATE TO X
*manually:  regress squared residuals on x and x^2  //WITH MORE THAN ONE X THE TEST INCLUDES ALL CROSS-PRODUCTS AS WELL
regress welfexp gnppc
predict welfres, resid
generate welfressq=welfres^2
generate gnpsq=gnppc^2
regress welfressq gnppc gnpsq

*white=R^2 from this regression*Number of Cases//DISTRIBUTED AS CHI-SQUARE WITH DF = NUMBER OF REGRESSORS (IN THIS CASE 2)

display .9364*12

*OR:  
regress welfressq c.gnppc##c.gnppc
display .9364*12


*from STATA automatically:
regress welfexp gnppc
estat imtest, white

*CORRECTING FOR HETEROSKEDASTICITY

*1.  WEIGHTED LEAST SQUARES IF RESIDUAL VARIANCE RELATED MONOTONICALLY TO X
generate newx=1/gnppc
generate newy=welfexp/gnppc
regress newy newx

rvfplot, yline(0) // LESS HETEROSKEDASTICITY PRESENT, IMTEST SHOWS STILL EXISTS BUT BORDERLINE SIGNIFICANCE"

*USING WEIGHTS IN STATA
generate wt=1/gnppc^2
regress welfexp gnppc [aweight=wt]

*BUT R-SQUARED HERE IS IN WEIGHTED UNITS, SO GENERATE MANUALLY IN ORIGINAL UNITS
predict wtpred
correlate wtpred welfexp
display .938^2

*2.  ROBUST VARIANCE ESTIMATES IF IT IS UNKNOWN HOW RESIDUAL VARIANCE MAY RELATE TO X
*    MORE FLEXIBLE PROCEDURE THAT MANY USE NOW AS A MATTER OF COURSE

regress welfexp gnppc, vce(robust) //OPTION FOR "VARIANCE-COVARIANCE ESTIMATION WITH ROBUST STANDARD ERRORS"

*use "bank-salaries.dta", clear

regress salnow salbeg edlevel age sex minority
rvfplot, yline(0)
imtest,white
regress salnow salbeg edlevel age sex minority, vce(robust) 

regress salnow edlevel age sex minority
regress salnow edlevel age sex minority, vce (cluster jobcat) 
*************************************************************************************************
**********************************AUTOCORRELATION************************************************


*use "PS2030 autocorrelation.dta"

*29 CASES OF A CITY'S CRIME RATE AND POLICE BUDGET

scatter budget crime||lfit budget crime

**IF AUTOCORRELATION, STANDARD ERRORS AND STATISTICAL INFERENCES FROM OLS RESULTS WILL NOT BE VALID

*FIRST TELL STATA THAT WE HAVE TIME SERIES DATA, YEARLY, WITH A VARIABLE CALLED 'YEAR' AS THE TIME MARKER

tsset year, yearly

*OLS REGRESSION

regress budget crime

predict budres,resid //THE RESIDUALS FROM THE BASIC OLS REGRESSION

scatter budres year //CLASSIC AUTOCORRELATION PATTERN:  NEGATIVES FOR SOME TIME, THEN POSITIVES, THEN NEGATIVES, ETC.

rvfplot, yline(0)

dwstat //THE DURBIN-WATSON TEST :  RESULT IS LOWER THAN DL CRITICAL VALUE, THEREFORE AUTOCORRELATION

estat bgodfrey, lags(2)  // THE BREUSCH-GODFREY GENERAL TEST FOR AUTOCORRELATION

*CALCULATION OF RHO, THE AUTOCORRELATION COEFFICIENT

*1.  FROM OLS REGRESSION OF RESIDUALS AT TIME T AND RESIDUALS AT TIME T-1

generate budlag=L.budget
generate crimlag=L.crime // THESE GENERATED LAG VARIABLES, I.E. THE T-1 VALUES FOR A GIVEN CASE
generate budlagres=L.budres
browse // CHECK OUT WHAT THE LAG VALUES LOOK LIKE IN THE DATA SET

regress budres budlagres, beta  //RESULT:  .52 AS ESTIMATE OF RHO

*2.  FROM MANIPULATION OF DURBAN-WATSON 

display (2-.957)/2  //FROM ALGABRAIC MANIPULATION OF FORMULA:  D=2(1-RHO)


*CORRECTING FOR AUTOCORRELATION WITH GENERALIZED LEAST SQUARES
*1. MANUALLY
replace budlag=14.69*(sqrt(1-(.52^2))) if budlag==.
replace crimlag=3.75*(sqrt(1-(.52^2))) if crimlag==. // THIS GIVES THE PRAIS-WINSTON TRANSFORMATION OF THE FIRST CASE

generate newx=crime-.52*crimlag
generate newy=budget-.52*budlag  // THIS GENERATES "PURGED" Y AND X VARIABLES

regress newy newx

*IF WANT TO TRANSFORM INTERCEPT INTO ORIGINAL UNITS:
display 1.74/(1-.52) // = 3.63

*2.  FROM STATA AUTOMATICALLY
prais budget crime, twostep robust // THIS STOPS THE ITERATIONS AFTER TWO ROUNDS AND ADDS ROBUST STANDARD ERROR (OPTIONAL)

*3.  NEWEY-WEST/HAC HETEROSKEDASTICITY-AUTOCORRELATION CONSISTENT STANDARD ERRORS
newey budget crime, lag(5)  // THIS CUTS THE AUTOCORRELATION AT ORDER OF 5 (SOMEWHAT ARBITRARILY)



