//PS2030
//Political Research and Analysis
//REGRESSION ADJUSTMENT AND PROPENSITY SCORE MATCHING EXAMPLES IN STATA



**USE THE SOUTH AFRICAN CIVIC EDUCATION DATA

g treat =(times>0)                //THIS CREATES A DICHOTOMOUS CIVIC EDUCATION 'TREATMENT' VARIABLE

reg polpart1 treat                //THIS IS THE NAIVE 'TREATMENT' EFFECT ESTIMATE = 1.88

reg polpart1 treat groups interest educ1  //THIS IS THE COVARIATE ADJUSTED EFFECT VIA MULTIPLE REGRESSION = .93

tabstat groups interest educ1, by(treat)   // THIS GIVES THE COVARIATE MEANS FOR EACH LEVEL OF TREATMENT
display 3.144-2.291                        // DIFFERENCE IN GROUP MEMBERSHIPS FOR TREATMENT-CONTROL
display 3.195-2.972                        // DIFFERENCE IN INTEREST
display 2.899-2.861							//DIFFERENCE IN EDUCATION

 ** SO:  1.88-(.666*.853+1.56*.223+.749*.038)=~.93   // WE GET THE COVARIATE ADJUSTED TREATMENT EFFECT BY MULTIPLYING THE
													   // REGRESSION COEFFICIENT FOR THE COVARIATE BY THE DIFFERENCE IN MEANS
													   // FOR THE TREATMENT AND CONTROL GROUPS AND SUBTRACTING THESE QUANTITIES
													  // FROM THE NAIVE ESTIMATE
												  
													
***USING THE TREATMENT EFFECTS ROUTINE IN STATA
**TEFFECTS RA = 'REGRESSION ADJUSTMENT'

teffects ra (polpart1 groups interest educ1)(treat), aequations	                  
					// GIVES THE EQUATIONS FOR TREATMENT AND CONTROL GROUPS AS WELL AS THE TREATMENT EFFECT
gen pred0 = 0
gen pred1=  0																	  
					// PRED0 IS THE CONTROL OUTCOME, PRED1 IS THE TREATMENT OUTCOME

replace pred0 = -3.67 + .68*groups + .97*interest + .63*educ1    if treat==0            
replace pred1 = -7.14 + .61*groups + 2.22*interest + .89*educ1   if treat==1      

					// GIVES THE OBSERVED CONTROL AND TREATMENT OUTCOMES FOR THE TWO GROUPS

replace pred0 = -3.67 + .68*groups + .97*interest + .63*educ1    if treat==1            
replace pred1 = -7.14 + .61*groups + 2.22*interest + .89*educ1   if treat==0      

					// GENERATES COUNTERFACTUAL OUTCOMES FOR THE TWO GROUPS

gen rateffect=pred1-pred0												
					// GENERATES TREATMENT EFFECT FOR EACH INDIVIDUAL 
sum rateffect															

					// (TREATMENT OUTCOME (REAL OR COUNTERFACTUAL) - CONTROL OUTCOME (REAL OR COUNTERFACTUAL)


teffects ra (polpart1 groups interest educ1)(treat), aequations	pomeans  

					// GIVES ACTUAL 'POTENTIAL OUTCOME MEANS' FOR TREATMENT AND CONTROL GROUPS
teffects ra (polpart1 groups interest educ1)(treat), coeflegend
nlcom _b[ATE:r1vs0.treat] / _b[POmean:0.treat]                         

					// PUTS TREATMENT EFFECT INTO PERCENTAGE TERMS FOR MORE INTUITIVE INTERPRETATION

teffects ra (polpart1 groups interest educ1)(treat), atet               

					// GIVES THE 'TREATMENT EFFECT ON THE TREATED', NOT THE 'AVERAGE TREATMENT EFFECT'
                    // THAT IS, THE ADJUSTED DIFFERENCE BETWEEN TREATMENT AND CONTROL USING ONLY THE TREATMENT CASES FOR
					// THE CALCULATIONS, NOT ALL CASES

sum rateffect if treat==1	                                           
					// MANUAL VERSION BASED ON OUR PREVIOUS CALCULATIONS										
													   
*** MANUAL PROPENSITY SCORE ANALYSIS 
logit treat groups interest educ1 media2
predict pspred                                     
					// THIS IS THE PREDICTED PROBABILITY OF TREATMENT FOR EVERYONE, I.E., THE PROPENSITY SCORE
logit treat c.groups##c.interest educ1 media2      

					// THIS IS POSSIBLY A BETTER PROPENSITY SCORE SINCE IT ADDS INTERACTIONS THAT APPEAR RELEVANT
predict pspred2
sum pspred2 if treat==1, detail                     // THIS GIVES THE RANGE OF THE PROPENSITY SCORE FOR THE TREATMENT GROUP
sum pspred2 if treat==0, detail                     // THIS GIVES THE RANGE OF THE PROPENSITY SCORE FOR THE CONTROL GROUP

// BASED ON THE RESULTS, WE CAN KEEP ALL CONTROL RESPONDENTS 
//(SINCE THE LOWEST CONTROL IS STILL HIGHER THAN THE LOWEST TREATMENT UNIT)
// BUT WE NEED TO ELIMINATE THOSE TREATMENT CASES ABOVE .814 
//WHICH IS THE HIGHEST CONTROL PROPENSITY TO BE TREATED.  THIS WILL ELIMINATE 8 CASES ONLY

drop if treat==1&pspred2>.814

egen psstrat=cut(pspred2) , group(5)               //  THIS STRATIFIES THE PROPENSITY SCORE WITHIN THE REGION OF COMMON SUPPORT INTO 5 NEAR-EQUAL QUINTILES
tab psstrat

*//CONSTRUCT MATCHES IN VARIOUS WAYS BASED ON PROPENSITY SCORE AND ASSESS COVARIATE BALANCE


teffects psmatch (polpart1) (treat c.groups##c.interest educ1 media2)        // USE BETTER PROP SCORE -- NEAREST NEIGHBOR DEFAULT

***TEST FOR BALANCE USING TEBALANCE IN TEFFECTS
tebalance summarize, baseline
tebalance summarize
tebalance density groups
tebalance density interest
tebalance density educ1  // NOTE -- THIS VARIABLE WAS BUILT INTO THE SAMPLING SCHEME!!
tebalance density media


***CALCULATE TREATMENT EFFECT BY AGGREGATING EFFECTS OBTAINED FROM REGRESSION WITHIN EACH PROPENSITY SCORE STRATA

by psstrat, sort:  regress polpart1 treat                              // WITHIN STRATA REGRESSION AND THEN AGGREGATING AND AVERAGING RESULTS
                              

display (-.140+-.144+.895+2.232+1.443)/5                                 // AVERAGE VALUE=.857  (ASSUMES EQUAL STRATA WHICH IS SLIGHTLY VIOLATED HERE)

*** OR CAN INCLUDE EDUCATION AND MEDIA2 INTO ANALYSES AS AN ADDITIONAL CONTROL


/// PROPENSITY SCORE WEIGHTING TO ACHIEVE BALANCE AND TO CALCULATE TREATMENT EFFECTS

g atewt2=(treat/pspred2)+(1-treat)/(1-pspred2)                 //  THIS IS THE WEIGHT FOR THE ATE
g attwt2=1 if treat==1                                         //  THE WEIGHT FOR THE ATT FOR TREATED UNITS
replace attwt2 = pspred2/(1-pspred2) if treat==0               //  THE WEIGHT FOR THE ATT FOR CONTROL UNITS


// ASSESS BALANCE WITH PROPENSITY SCORE WEIGHTED DATA
tabstat groups interest educ1 media2 , by (treat)
tabstat groups interest educ1 media2 [weight=atewt2], by (treat)   // AWESOME BALANCING!!!

//CALCULATE TREATMENT EFFECTS WITH INVERSE PROBABILITY WEIGHTING

****WE OBTAIN .76 AS OUR ATT, IN CONTRAST TO .93 FROM MULTIPLE REGRESSION

regress polpart1 treat [weight=atewt2]                                  // REGRESSION WEIGHTED BY THE PROPENSITY SCORE:  ATE

teffects ipw (polpart1) (treat c.groups##c.interest educ1 media2)       // TEFFECTS VERSION OF THIS (DIFFERS BECAUSE OF LPM VERSUS LOGIT)


regress polpart1 treat [weight=attwt2]                                  // REGRESSION WEIGHTED BY THE PROPENSITY SCORE:  ATT

teffects ipw (polpart1) (treat c.groups##c.interest educ1 media2)  , atet     // TEFFECTS VERSION OF THIS



