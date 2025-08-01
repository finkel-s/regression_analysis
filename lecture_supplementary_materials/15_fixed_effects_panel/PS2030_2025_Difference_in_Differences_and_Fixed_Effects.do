PS2030
POLITICAL ANALYSIS

WEEK 13:  DIFFERENCE-IN-DIFFERENCE MODELS

EXAMPLE:  KENYA CIVIC EDUCATION DATA (FOLLOWING FINKEL&SMITH 2011, AJPS)

//USE KENYA 2 WAVE "WIDE" DATA

*tabulations of treatment variables
tab1 newtreat_ww0 newtreat_ww1
**tabulations of knowlege variables
tab1 know_w0 know_w1
sum know_w0 know_w1
*generate difference scores
gen knowdif=know_w1-know_w0
gen treatdif=newtreat_ww1-newtreat_ww0
tab1 treatdif knowdif

**THE BASIC FD MODEL
regress knowdif treatdif
regress knowdif newtreat_ww1  // SAME THING

***MARGINAL PREDICTIONS
tabstat know_w1 know_w0, by(treatdif)

****CONTROL GOES FROM 2.17 TO 2.55, WORKSHOP GOES FROM 2.37 TO 3
****SO DIFFERENCE FOR CONTROL IS .38, DIFFERENCE FOR WORKSHOP IS .63
****SO DIFFERENCE IN DIFFERENCE IS .25

**multivariate
gen intdif=interest_w1-interest_w0
regress knowdif treatdif intdif
regress knowdif treatdif intdif sex 
*****ALTERNATIVE DiD ESTIMATION WITH LONG DATA

// USE KENYA 2 WAVE DID "LONG" DATA


//Replicate the 2 wave analysis

xtset id _j // TELLS STATA THAT ID IS THE CASE MARKER AND _J IS THE TIME MARKER
reg know_w treat##_j   //REGRESSION OF KNOWLEDGE WITH TREATMENT GROUP, TIME, AND INTERACTION BETWEEN TREATMENT AND TIME

//RECOVERS THE EFFECT OF TIME AS .38 (FOR THE CONTROL GROUP)
//AND .38+.25 = .63 (FOR THE TREATMENT GROUP)
//SO DIFFERENCE-IN-DIFFERENCE OF .25, WHICH IS THE INTERACTION EFFECT BETWEEN TREATMENT AND TIME
//(AND THE .19 'TREAT' EFFECT IS THE WAVE 1 DIFFERENCE BETWEEN TREATMENT AND CONTROL)



***FIXED EFFECTS EXTENDS THIS LOGIC AND CAN BE APPLIED TO 
***CONTINUOUS TREATMENTS AND MULTI-WAVE DATA AS WELL


//GET 3 WAVE KENYA DATA ("KENYA 3 WAVE LONG DATA")
xtset id wave1
tab wave1, gen(wavdum)
tab id, gen(idum)
**Least Squares Dummy Variables
xtreg polpart_w totalce_w wavdum2 wavdum3 idum* 
**"ABSORB" REGRESSION ABSORBS THE UNIT DUMMIES IN THE ESTIMATION BUT DOESN'T REPORT THEM
areg polpart_w totalce_w wavdum2 wavdum3, absorb(id)

**The Fixed Effects model
xtreg polpart_w totalce_w wavdum2 wavdum3, fe

**VERY IMPORTANT TO CONTROL FOR TIME EFFECT IN THE CONTROL GROUP (I.E., WHEN TOTAL_CE=0)
**OR YOU WILL MISS THE GENERAL TRENDS AND CONFLATE THE TREND WITH THE TREATMENT EFFECTS
xtreg polpart_w totalce_w, fe


