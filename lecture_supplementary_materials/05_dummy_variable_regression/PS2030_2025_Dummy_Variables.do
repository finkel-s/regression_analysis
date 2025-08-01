*POL2030
*Dummy Variable Regression

**Use bank-salaries.dta"

**ADDITIVE ("INTERCEPT") DUMMY VARIABLE MODELS
gen male=1-sex
regress salnow edlevel male  


regress salnow male //USING DUMMY VARIABLE REGRESSION TO TEST GROUP DIFFERENCES

**NOTE THAT WE HAVE A 6163.95 DIFFERENCE IN AVERAGE SALARY

**HOW MUCH IS "EXPLAINED" BY EDUCATIONAL DIFFERENCES?

tabstat edlevel, by (male) //AVERAGE EDUCATIONAL DIFFERENCES BY SEX
display 14.43-12.37
**SO THERE IS A 2.06 YEAR MEAN DIFFERENCE IN EDUCATION FOR MEN AND WOMEN

**THIS TRANSLATES INTO:
display 2.06*1356.67 

* OR A 2794.7 OVERALL SALARY DIFFERENCE BETWEEN MEN AND WOMEN THAT IS "ACCOUNTED FOR" BY EDUCATIONAL DIFFERENCES

*WITH 3369.385 OF THE ORIGINAL 6163.95 DIFFERENCE STILL "UNEXPLAINED"

***OR, USING STATA "MARGINS" AND "MARGINSPLOT" COMMANDS TO CALCULATE AND PLOT ADJUSTED MEANS AND SLOPES:
*** MARGINS OPERATES ON THE LAST REGRESSION COMMAND ONLY

regress salnow edlevel i.male // "i" is Stata's way of dealing with categorical variables 

margins, at (edlevel=12 male=1)  // THIS IS THE PREDICTED SALARY FOR MALES WITH 12 YEARS OF EDUCATION, FOR EXAMPLE
margins male                     // THESE ARE THE "ADJUSTED" SALARY MEANS FOR MEN AND WOMEN, CONTROLLING FOR EDUCATION

***Adjusted Y-bar(male)= -6369.78+13.49*1356.67+3369    = 15300 
                         // FROM REG EQUATION, ASSUMING AVERAGE EDUCATION AND ADDING "MALE" EFFECT
***Adjusted Y-bar(female)=-6369.78+13.49*1356.67        = 11931
                         // FROM REG EQUATION, ASSUMING AVERAGE EDUCATION AND NO "MALE" EFFECT
						 
margins male, at((mean) edlevel) // THIS GIVES THE EXACT SAME RESULT AS PREVIOUS COMMAND -- this is what an adjusted mean is
                       
margins male, at (edlevel=(8(1)21)) vsquish  // GIVES THE SALARY MEANS FOR MEN AND WOMEN AT EVERY LEVEL OF EDUCATION
marginsplot, recast (line) recastci (rarea)  // PLOTS THESE EFFECTS WITH CONFIDENCE INTERVALS SHADED, I.E., PLOTS THE REGRESSION LINE FOR MEN
                                             // THEN FOR WOMEN
margins r.male                   // THIS TESTS DIFFERENCES OF CATEGORIES OF MALE FROM 'R'EFERENCE CATEGORY; HERE MALE=1 VERSUS FEMALE=0
                                 // SO IT IS ANOTHER WAY TO TEST THE SIGNIFICANCE OF THE REGRESSION COEFFICIENT FOR 'MALE' IN THIS CASE
  

*** DUMMY VARIABLE EXAMPLE WITH MORE THAN 2 CATEGORIES
tab jobcat
recode jobcat 1=1 2 4 6=2 3 5 7=3, gen(jobcatrec)
tab jobcatrec, gen(jobcatrec)  // CREATES "JOBCATREC1" FOR CLERICAL, "JOBCATREC2" FOR TRAINEE, "JOBCATREC3" FOR OTHER
regress salnow edlevel jobcatrec2 jobcatrec3 // "CLERICAL" IS REFERENCE CATEGORY
lincom jobcatrec3-jobcatrec2 // tests significance of other versus trainee, which is not provided in the earlier regression 
                             // unless you change the reference category
regress salnow edlevel jobcatrec1 jobcatrec3 // changes the reference category to be "trainee"

****OR USE MARGINS TO GET ADJUSTED MEANS
regress salnow edlevel i.jobcatrec
margins jobcatrec                         // gives the adjusted salary means for each jobcat, with edlevel constant, i.e., set at its mean 
pwcompare jobcatrec                      // provides all 'pairwise' comparisons in one command and tests difference in means between them
margins jobcatrec, at (edlevel=(8(1)21)) // gives the salary means for all jobcatrec categories at each level of education
marginsplot, noci                         // plots these marginal means, and thus plots the three regression lines
marginsplot, recast (line) recastci (rarea) // plotes the lines with shaded confidence intervals

***COMPARING MEAN DIFFERENCES IN MODELS WITH MORE THAN ONE DUMMY VARIABLE
tabstat salnow, by(sexrace)

regress salnow male minority

*MAGNITUDE AND SIGNFICIANCE OF DIFFERENCES BETWEEN:
*white male versus white female= level and significance of "male" coefficient
*minority male versus minority female= level and significance of "male" coefficient
*white female versus minority female = level and significance of "minority" coeffient
*white male versus minority male = level and significance of "minority" coefficient
*minority male versus white female=level and significance of "male+minority" versus baseline
lincom male+minority
*white male versus minority female=level and significance of "male - minority" = [(constant+male)-(constant+minority)]
lincom male-minority

***WE CAN GENERATE PREDICTED MEANS WITH "MARGINS" COMMAND IN STATA (AS OPPOSED TO COMPARING "EFFECTS" WITH LINCOM)
regress salnow i.male i.minority  

margins male, at(minority =(0(1)1)) // gives the predicted salary means of every combination of male and minority 

***INTERACTIVE ("SLOPE") DUMMY VARIABLE MODELS
generate agemale=age*male

regress salnow age agemale  // ONLY THE SLOPE DIFFERENCE INCLUDED

lincom age+agemale*1   // IS THE SLOPE FOR MEN DIFFERENT FROM 0?

****OR:
regress salnow age c.age#i.male  // tells STATA that "age" is continuous and "male" is categorical"

margins male, at (age=(23 64))  //  Calculates the predicted means for males and females at age=23 and then age=64)

marginsplot  // Plots the effects, i.e., plots the regression line for men and then for women

***MARGINS CAN ALSO CALCULATE CONDITIONAL SLOPES, NOT ONLY ADJUSTED (AND UNADJUSTED) MEANS
margins, dydx (age) over(male)   // THE SLOPE FOR AGE 'OVER' VALUES OF MALE=0, MALE=1
                                 //THIS REPLICATES THE ORIGINAL REGRESSION AND ALSO THE LINCOM PROCEDURE FOR THE SIGNIFIANCE OF BOTH SLOPES 


*****INTERACTION/CONDITIONAL RELATIONSHIPS WITH TWO DUMMY VARIABLES

regress salnow i.male##i.minority	 // TELLS STATA TO INCLUDE THE INTERACTION TERM, AND THE EXTRA '#' MEANS 
									// "PUT IN THE INDIVIDUAL COMPONENT TERMS TOO"
contrast male#minority				// This is the same as the F* test for the interaction between male and minority 
margins male, at(minority =(0(1)1)) // gives the predicted salary means of every combination of male and minority 
tabstat salnow, by(sexrace)         // note how the 'saturated' model with interaction reproduces the unconditional means for each cominbation

****SLOPE AND DUMMY VARIABLE MODEL
regress salnow male age agemale  // BOTH SLOPE AND INTERCEPT DIFFERENCES INCLUDED

lincom age+agemale*1  // IS THE SLOPE FOR MEN DIFFERENT FROM 0?

test male agemale // ARE THERE MALE-FEMALE DIFFERENCES IN THE OVERALL REGRESSION LINE?


****OR:

regress salnow c.age##i.male 
margins male, at (age=(23 64))  //  Calculates the predicted salary means for males and females at age=23 and then age=64)
marginsplot,  recast (line) recastci (rarea) // Plots the effects, i.e., plots the regression line for men and then for women

***ESTIMATNG CONDITIONAL SLOPES WITH DYDX OPTION
margins, dydx (age) over(male)   // THIS REPLICATES THE ORIGINAL REGRESSION AND ALSO THE LINCOM PROCEDURE FOR THE SIGNIFIANCE OF BOTH SLOPES 



***CAUSAL MEDIATION

**traditional (as above)
regress edlevel sex
regress salnow sex edlevel
regress edlevel male
regress salnow male edlevel
** Female Mean on Education: 12.37
** Male Mean on Education: 	 14.43
** So there is ONE regression equation (i.e. no interaction between X and M)
 -6369 + 1356*EDLEVEL + 3369*MALE

** CALCULATION OF INDIRECT EFFECT IS 1356*(DIFFERENCE IN MALE AND FEMALE EDUCATION MEANS) = 2.06*1356 = 2795
** DIRECT EFFECT IS 3369
** TOTAL EFFECT IS DIRECT+INDIRECT= 2795+3369 = 6164



** Causal Mediation: with interaction between sex and edlevel
gen maleed=sex*edlevel
regress salnow male edlevel maleed

** SO THERE ARE TWO SEPARATE REGRESSIONS PREDICTING SALARY, ONE FOR MALE AND ONE FOR FEMALE
** MALE: 		-7816.46+1690.43*EDLEVEL
** FEMALE:		1774.96 + 698.27*EDLEVEL

*DIRECT AND INDIRECT EFFECTS NOW DIFFER, DEPENDING ON WHICH EQUATION IS USED

* FOR INDIRECT EFFECT, YOU TYPICALLY USE THE MALE REGRESSION EQUATION (I.E., WHERE 'TREATMENT'=1) AND CALCULATE WHAT SALARY WOULD BE IF A PERSON HAD AVERAGE FEMALE EDUCATION COMPARED TO AVERAGE MALE EDUCATION

* FOR DIRECT EFFECT, YOU TYPICALLY USE THE FEMALE MEAN ON EDUCATION (I.E. WHERE 'TREATMENT'=0), AND CALCULATE WHAT SALARY WOULD BE IF PERSON WAS FEMALE (I.E. WITH FEMALE EQUATION REGRESSION COEFFICIENTS) COMPARED TO WHAT IT WOULD BE IF THE PERSON WAS MALE (I.E. WITH MALE EQUATION REGRESSION COEFFICIENTS)

* INDIRECT
* -7816.46+1690.43*14.43=16576.45 -- WTIH MALE MEAN ON EDUCATION
* -7816.46+1690.43*12.37= 13094.16 -- WITH FEMALE MEAN ON EDUCATIONAL
* = 3482.3

*DIRECT
* -7816.46+ 1690.43*12.37 = 13094.16 - WITH MALE REGRESSION COEFFICIENTS
*  1774.96+ 698.27*12.37 = 10412.6   - WITH FEMALE REGRESSION COEFFICIENTS
* = 2681.5

mediate (salnow)(edlevel)(male)
mediate (salnow)(edlevel)(male), all


NIE  = 	3482	= PEARL Total Natural Indirect Effect (TNIE) = IMAI ACME (1)
NDE  = 	2682	= PEARL Pure Natural Direct Effect (PNDE) = IMAI DE (0)

Percentage of total explained by mediation: (3482/(3482+2682)*100) = 56.5%

** YOU CAN ALSO GET ANOTHER INDIRECT EFFECT BY USING THE FEMALE REGRESSION EQUATION COEFICIENTS AND VARYING THE MEDIATOR FROM FEMALE TO MALE. 
** YOU CAN ALSO GET ANOTHER DIRECT EFFECT BY USING THE MALE LEVEL OF EDUCATION AND COMPARING THE OUTCOME USING MALE AND THEN FEMALE REGRESSION COEFFICIENTS
** THESE TWO ESTIMATES WILL ALSO ADD UP TO THE TOTAL EFFECT OF 6164.
