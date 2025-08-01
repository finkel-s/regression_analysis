*PS2030 POLITICAL RESEARCH AND ANALYSIS
*WEEK 5:  NON-ADDITIVE AND NON-LINEAR MODELS

*I.  INTERACTIVE MODELS
**use protest.homework1.dta 

*QUESTION:  	 DOES THE EFFECT OF POLICY DISSATISFACTION ON PROTEST INCREASE (DECREASE) AS ONE BELONGS TO MORE PROTEST-ENCOURAGING GROUPS?
*ALTERNATIVELY:  DOES THE EFFECT OF BELONGING TO PROTEST-ENCOURAGING GROUPS ON PROTEST INCREASE (DECREASE) AS ONE HAS HIGHER LEVELS OF POLICY DISSATISFACTION?

generate lrv = vcount*lr

regress legalcount vcount lr // THIS IS THE ADDITIVE MODEL

regress legalcount vcount lr lrv // THIS ADDS THE INTERACTIVE EFFECT

***TESTING THE SIGNIFICANCE OF VCOUNT AT DIFFERENT LEVELS OF LR

lincom vcount+lrv*1   // WHEN LR=1
lincom vcount+lrv*2   // WHEN LR=2
lincom vcount+lrv*5   // WHEN LR=5 

*** MANUAL CALCULATION OF CONDITIONAL STANDARD ERRORS
estat vce // THIS GENERATES THE VARIANCES AND COVARIANCES OF THE REGRESSION COEFFICIENTS B1,B2, B3 (THAT IS, THE SQUARED STANDARD ERRORS ON THE DIAGONALS AND THE COVARIANCES ON THE OFF-DIAGONALS)

display (.00149+.00199777+2*-.00053235)^.5 // THE CONDITIONAL STANDARD ERROR WHEN LR=1
display (.00149+.00199777*2^2+2*2*-.00053235)^.5  // THE CONDITIONAL STANDARD ERROR WHEN LR=2
display (.00149+.00199777*5^2+2*5*-.00053235)^.5  // THE CONDITIONAL STANDARD ERROR WHEN LR=5



***USING MARGINS AND MARGINSPLOT

regress legalcount c.vcount##c.lr
*****FROM PERSEPCTIVE OF CHANGING SLOPE OF VCOUNT FOR EVERY ADDITIONAL GROUP
margins, at (vcount=(0 10) lr=(0(1)7)) // this plots the predicted protest score at every level of lr for vcount=0 and vcount=10
marginsplot, noci  //                     this plots the 8 regression lines

margins, at (lr=(0(1)7)) dydx (vcount) vsquish  // this plots the *slope* for vcount at each level of lr
marginsplot
marginsplot, recast (line) recastci (rarea)  // makes the line a real line, not a connected bunch of dots, and gives the shaded confidence intervals around the predictions

*****FROM PERSEPCTIVE OF CHANGING SLOPE OF LR FOR EVERY ADDITIONAL VCOUNT
margins, at (lr=(0 7) vcount=(0(2) 10))   // this plots the mean salary at every second level of vcount for lr=0 and lr=7
marginsplot, noci                         // this plots the 6 regression lines

margins, at (vcount=(0(1)10)) dydx (lr) vsquish  // this plots the *slope* for lr at each level of vcount
marginsplot
marginsplot, recast (line) recastci (rarea)  // makes the line a real line, not a connected bunch of dots, and gives the shaded confidence intervals around the predictions


****USE COUNTRIES DATA

****POLYNOMIAL REGRESSION

scatter deathrat docs || lfit (deathrat docs) || qfit (deathrat docs)  // THIS GIVES THE LINEAR AND THE QUADRATIC FIT SUPERIMPOSED ON THE RELATIONSHIP SIMULTANEOUSLY

gen docsquared=docs*docs  // COULD ALSO SAY GEN DOCSQUARED=DOCS^2
regress deathrat docs

regress deathrat docs docsquared

***OR WITH 'MARGINS' AND 'MARGINSPLOT' ROUTINES:
regress deathrat c.docs##c.docs  // "c.docs##c.docs" means "put in the non-squared and squared versions of the variable 'docs'"

*WHERE IS THE INFLECTION POINT?

display -_b[docs]/(2*_b[c.docs#c.docs]) // _b[XXX] is the regression coefficient generated for variable XXX in the previous regression


margins, at(docs=(.1(1)43))
marginsplot //plots the predicted Y at all the levels of X you chose in the previous margin command
marginsplot, noci  // no confidence interval
marginsplot, recast (line) recastci (rarea)  // makes the line a real line, not a connected bunch of dots, and gives the shaded confidence intervals around the predictions

**DO THE SAME, ADJUSTING FOR ANOTHER COVARIATE
regress deathrat c.docs##c.docs lngdp
margins, at(docs=(.1(1)43))
marginsplot, recast (line) recastci (rarea)


****LOGARITHMIC REGRESSION
scatter life gdp || lfit life gdp            // THIS WOULD BE A LINEAR ESTIMATED RELATIONSHIP IN WHAT IS CLEARLY NOT A TRUE LINEAR RELATIONSHIP 

gen lggdp=log10(gdp)
gen lglife=log10(life)

scatter lglife lggdp ||lfit lglife lggdp

regress lglife lggdp


***BUT R-SQUARED HERE IS IN TERMS OF LOGARITHM OF LIFE EXPECTANCY -- NEED TO TRANSFORM BACK TO ORIGINAL UNITS
	
predict yhat2

generate yhat_antilog=10^(yhat2)

correlate life yhat_antilog

display .812^2

**PLOT THE LOGARITHMIC FUNCTION
sort gdp
twoway (connected yhat_antilog gdp) (scatter life gdp),xtitle (GDP PER CAPITA) ytitle (LIFE EXPECTANCY)

****LOGARITHMIC REGRESSION WITH NATURAL LOGS
gen lnlife=ln(life)  // NOTE LNGDP IS ALREADY IN THE DATA SET

regress lnlife lngdp

***SAME MODEL ESTIMATED IN "NL" ROUTINE FOR NON-LINEAR MODELS

nl (life={alpha=0}*gdp^{beta=1})  // ALPHA AND BETA ARE THE UNKNOWNS
                                  // AND WE PROVIDE STARTING VALUES FOR THEM


display exp(3.48)= 32.5   // SO THE CONSTANT IN THE STATA NL MODEL IS NEARLY THE SAME VALUE
                          // AS THE 'ANTI-LOG' OF THE CONSTANT FROM OUR MANUAL CONSTANT


***HYPERBOLIC MODELS

nl (life={alpha=70}+{beta=-.5}*1/gdp) if life~=.&gdp~=.  // GIVES STARTING VALUES FOR THE TWO UNKNOWN AND LIMITS
                                                         // ANALYSIS TO THE NON-MISSING CASES AS REQUIRED BY NL


nl (fertrate={alpha=0}+{beta=1}*1/gdp) if fertrate~=.&gdp~=.  // GIVES STARTING VALUES FOR THE TWO UNKNOWN AND LIMITS
                                                         // ANALYSIS TO THE NON-MISSING CASES AS REQUIRED BY NL

