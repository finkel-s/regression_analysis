PS2030
Maximum Likelihood Example and Summary Measures of Model Fit

/* use maxlike.probit.xy.dta */
/* DEFAULT Log Likelihood when all Bs are 0 except for the intercept */
probit y
display 5/9                               //THIS IS THE UNCONDITIONAL PROBABILITY OF BEING 1 FOR ALL CASES -- I.E. 5 AT "1", 4 AT '0" = .55 P(Y=1)
display invnorm(5/9)                      //THIS IS THE Z-SCORE ASSOCIATED WITH P(Y=1) OF .55
gen pred0=.1397
gen predprb0=normal(pred0)                // THIS RETURNS .55 AS THE PREDICTED PROB FOR ALL CASES ASSOCIATED WITH THE INTERCEPT ONLY MODEL
gen lnlikep0=ln(predprb0) if y==1         // TAKE THE LN OF .55 IF THE CASE IS A "1"
replace lnlikep0=ln(1-predprb0) if y==0   // TAKE THE LN OF (1-.55) IF THE CASE IS A "0"
tabstat lnlikep0, stat(sum)               // SUM THE LOG-LIKELIHOODS TO GET -6.18

/* Now try other values of B for X as well as intercept */

gen pred1=-.3+1*x
gen predprb1=normal(pred1)
gen lnlikep1=ln(predprb1) if y==1
replace lnlikep1=ln(1-predprb1) if y==0

tabstat lnlikep0 lnlikep1, stat(sum)

gen pred2=-.3+2*x
gen predprb2=normal(pred2)
gen lnlikep2=ln(predprb2) if y==1
replace lnlikep2=ln(1-predprb2) if y==0
tabstat lnlikep0 lnlikep1 lnlikep2, stat(sum)

gen pred3=-.3+3*x
gen predprb3=normal(pred3)
gen lnlikep3=ln(predprb3) if y==1
replace lnlikep3=ln(1-predprb3) if y==0

tabstat lnlikep3, stat(sum)

gen pred4=-.3+3.5*x
gen predprb4=normal(pred4)
gen lnlikep4=ln(predprb4) if y==1
replace lnlikep4=ln(1-predprb4) if y==0
tabstat lnlikep0 lnlikep1 lnlikep2 lnlikep3 lnlikep4, stat(sum)

gen pred5=-.3+4*x
gen predprb5=normal(pred5)
gen lnlikep5=ln(predprb5) if y==1
replace lnlikep5=ln(1-predprb5) if y==0
tabstat lnlikep0 lnlikep1 lnlikep2 lnlikep3 lnlikep4 lnlikep5 ,stat(sum)

gen pred6=-.3+5*x
gen predprb6=normal(pred6)
gen lnlikep6=ln(predprb6) if y==1
replace lnlikep6=ln(1-predprb6) if y==0
tabstat lnlikep0 lnlikep1 lnlikep2 lnlikep3 lnlikep4 lnlikep5 lnlikep6 ,stat(sum)

/* Now get real estimates */

probit y x

/* Long and Freese module for summary statistics */

fitstat

summarize x // to get SD of X

/*Regular Stata's module for correct classifications */

lstat

// Commands to generate last predictive accuracy measure, the "coefficient of Discrimination" (Tjur's D)
predict predproby
ttest predproby, by(y)
// FOR THE 5 CASES ON '1', THE MEAN PREDICTED PROBABILITY IS .782
// FOR THE 4 CASES ON '0', THE MEAN PREDICTED PROBABILITY IS .286  
// TJUR'S D IS THEREFORE .50




************EXAMPLE OF COMPARING TWO MODELS WITH FITSTAT -- USING SOUTH AFRICA DATA

*****COMPARING ALTERNATIVE MODELS USING FITSTAT

****FIRST WE NEED TO MAKE SURE THAT WE ONLY USE VALID CASES FOR *ALL* MODELS

mark nomiss
markout nomiss educ1 groups male black age1 ecocur ecofut ideo partyid dembest demsat
tab nomiss //THIS CREATES A VARIABLE NOMISS THAT EQUAL 1 IF THE CASE IS NOT MISSING ON ALL THE VARIABLES IN THE ABOVE LIST

gen locdich=(locpart>0)
probit locdich educ1 groups age1 male black if nomiss==1  // THIS IS THE FIRST MODEL WE WANT TO COMPARE -- THE "REDUCED" MODEL
fitstat, saving(mod1)  //THIS SAVES THE RESULTS OF THE FITSTAT INTO A SCALAR CALLED "MOD1"
probit locdich educ1 groups age1 male black ecocur ecofut ideo partyid dembest demsat if nomiss==1 // THIS IS THE SECOND MODEL WE WANT TO COMPARE -- THE "FULL" MODEL
fitstat, using(mod1)  //THIS COMPARES THE TWO AND GIVES THE DIFFERENCE IN THE LOG-LIKELIHOODS ASSUMING THEY ARE NESTED MODELS

