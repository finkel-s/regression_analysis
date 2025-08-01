PS2030
Endogeneity and Instrumental Variables

*Use "protest.homework1.dta"

***YOU WANT THE EFFECT OF PROTEST ON JOINING PROTEST-ORIENTED GROUPS, BUT YOU THINK THERE IS 
***RECIPROCAL CAUSALITY SUCH THAT GROUP MEMBERSHIPS ALSO CAUSE PROTEST

*** SO YOU NEED INSTRUMENT(S) FOR PROTEST

** YOU DECIDE ON EDUCATION AND POLITICAL EFFICACY AS POTENTIAL INSTRUMENTS FOR PROTEST.  
** YOU ARE PREPARED TO CLAIM THAT THESE VARIABLES CAUSE PROTEST
** BUT NOT GROUP MEMBERSHIPS EXCEPT THROUGH PROTEST, AND YOU ARE PREPARED TO CLAIM THAT THEY ARE EXOGENOUS, I.E.,
** UNRELATED TO THE ERROR TERM IN PROTEST AND GROUP MEMBERSHIP EQUATIONS

**(THIS WOULD NOT WASH IN ALL LIKELIHOOD BUT WE ARE ILLUSTRATING FOR PEDAGOGICAL PURPOSES).

***BASIC IV ANALYSIS
**USE education only

**GENERATE COVARIANCES BETWEEN LR LEGALCOUNT EDUC
corr lr legalcount educ, cov


. corr lr legalcount educ, cov
(obs=713)

             |       lr legalc~t     educ
-------------+---------------------------
          lr |  .588434
  legalcount |  1.03101  6.88319
        educ |  .152302  1.00876  1.21773


display .152/1.00876  // the covariance of lr&educ (the instrument) divided by the covariance of legalcount&educ (the instrument)
**estimate:  .151  // this is the IV estimate of the causal effect of ENDOGENOUS X (LEGALCOUNT) on Y

**ANOTHER WAY TO ESTIMATE THIS IS VIA INDIRECT LEAST SQUARES

regress lr educ     // gives the "reduced form" of Y on Z
regress legalcount educ //gives the "reduced form" of X on Z

display .125/.8283 // = .15091151, our estimate of the causal effect of ENDOGENOUS X (LEGALCOUNT) on Y


**FINAL WAY VIA 2SLS ROUTINE, WHICH CAN BE USED FOR ALL IV MODELS

ivregress 2sls lr (legalcount=educ)

// we see that the RMSE is .6583, so error variance is .6583^2=.433

// So standard error is square root of .433/(713*6.88*.1214)
display (.433/(713*6.88*.1214))^.5 // = .027


//NOTE HUGE DIFFERENCE IN OLS AND IV STANDARD ERROR!!! WE SACRIFICE EFFICIENCY IN 2SLS FOR CONSISTENCY (BUT SOMETIMES MAY NOT BE WORTH IT)
regress lr legalcount
//standard error = .0094

***EXTENDED INDIRECT LEAST SQUARES ANALYSIS, STILL WITH ONLY EDUC AS THE ONE INSTRUMENT FOR LEGALCOUNT
regress lr educ lgrp sex youth a   // gives all reduced form effects on Y
regress legalcount educ lgrp sex youth a // gives all reduced form effects on X

display .1176/.726504 // = .1618711, our estimate of the causal effect of LEGALCOUNT on Y

***2SLS VERSION OF THIS JUST-IDENTIFIED MODEL
ivregress 2sls lr lgrp sex youth a (legalcount=educ)

***2SLS ESTIMATION OF THE OVERIDENTIFIED MODEL WITH EDUCATION AND EFFICACY AS INSTRUMENTS

ivregress 2sls lr lgrp sex youth a  (legalcount=educ il)
estat firststage
estat overid
estat endogenous
***THIS GIVES THE SHEA TEST FOR RELEVANCE OF THE INSTRUMENTS IN THE FIRST STAGE, AND THE SARGAN TEST FOR EXOGENEITY OF THE INSTRUMENTS
***AND THE DURBAN-WU-HAUSMAN TEST FOR ENDOGENEITY OF LEGALCOUNT


***ANTONAKIS ENDOGENEITY EXAMPLE
*********TRUE MODEL IS Y=-.3X + 1Q  (SEE SLIDE IN ENDOGENEITY POWERPOINT)
*********TRUE MODEL HAS M AND N CAUSING X BUT NOT Y, SO THEY ARE PERFECT INSTRUMENTS TO USE FOR ENDOGENOUS X
reg y x   // OMITTED VARIABLE Q SHOULD BE THERE, SO ESTIMATE IS INCONSISTENT
ivregress 2sls y (x=m n), first  // RECOVERS THE TRUE EFFECT!
estat firststage
estat overid
estat endogenous

***ALTERNATIVE ESTIMATION:  "TWO STAGE RESIDUAL INCLUSION"
***PROCEDURE: 1) REGRESS ENDOGENOUS X ON THE INSTRUMENT(S) 2) GENERATE RESIDUAL FROM THIS EQUATION, WHICH REPRESENTS
***              THE "ENDOGENOUS" PORTION OF THE X VARIABLE, I.E., UNRELATED TO THE EXOGENOUS INSTRUMENTS
***           3) REGRESS Y ON X AND THE FIRST STAGE RESIDUAL, SO THAT THE EFFECT OF X IS ESTIMATED BY MANUALLY "CONTROLLING" 
***              FOR THE ENDOGENEITY IN THE ORIGINAL X-Y RELATIONSHIP
***           SEE TERZA ET. AL. "Two-stage residual inclusion estimation: Addressing endogeneity in health econometric modeling"
***           Journal of Health Economics 27 (2008) 531–543.

reg x m n
predict resid1, resid
reg y x resid1

***NOTE .4798 EFFECT OF RESID1 STANDS IN FOR THE CORRELATION BETWEEN U AND E WHICH EXISTS IF Q IS OMITTED FROM THE MODEL

reg y x q  // TRUE MODEL
