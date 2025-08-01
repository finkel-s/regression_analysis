*PS2030

*use south africa civic education data

* FREQUENCY ON LOCAL PARTICIPATION
tab locpart
* CREATE TRICHOTOMIZED VARIABLE
generate loctri=locpart
recode loctri 0=1 1/2=2 *=3
* BIVARIATE ORDERED PROBIT
oprobit loctri groups
* PREDICTED Y*
predict opred1,xb
* PREDICTED PROBABILITIES OF BEING IN LOW, MEDIUM, HIGH CATEGORIES 
predict lowpar medpar highpar
tabstat opred1 lowpar medpar highpar, stat (mean) by(groups)
* CALCULATING THESE PROBABILITIES "MANUALLY" WITH DISPLAY
oprobit loctri groups
display normal(.42-.24)  // GETS PROBABILITY OF LOW PARTICIPATION FOR GROUPS=1
//or:
display normal(_b[/cut1]-_b[groups]*1)
//or:
margins, at (groups=1) predict(outcome(1))

display normal(1.64-.24)-normal(.42-.24) // GETS PROBABILITY OF MEDIUM PARTICIPATION FOR GROUPS=1
//or:
display normal(_b[/cut2]-_b[groups]*1)-normal(_b[/cut1]-_b[groups]*1)
//or:
margins, at (groups=1) predict(outcome(2))

display 1-normal(1.64-.24) // GETS PROBABILITY OF HIGH PARTICIPATION FOR GROUPS=1
//or:
display 1-normal(_b[/cut2]-_b[groups]*1)
//or:
margins, at (groups=1) predict(outcome(3))

* GRAPHING THE PROBABILITIES OF BEING IN LOW AND HIGH CATEGORIES TO SHOW NON-LINEARITIES
sort groups
twoway connect lowpar groups
* What if someone belonged to 10 groups?
display normal(_b[/cut1]-_b[groups]*10)  // =.02 SHOWS CURVE GETTING FLATTER AND FLATTER AT LOW Ps

twoway connect highpar groups
* What if someone belonged to 10 groups?
display 1-normal(_b[/cut2]-_b[groups]*10) // = .79, showing that curve becomes flatter up there too

*GRAPH ALL THREE PROBABILITIES IN ONE GRAPH
twoway (connected lowpar groups)(connected medpar groups)(connected highpar groups)


// EXTEND TO MULTIPLE INDEPENDENT VARIABLES
fitstat, saving (md1)
oprobit loctri groups educ1 interest know male civiced
//COMPARE FIT OF TWO MODELS:  CONSTRAINED OR REDUCED MODEL WITH B2-B6=0 COMPARED WITH CURRENT MODEL
fitstat ,using (md1)


***GENERATE CHANGES IN PREDICTED PROBABILITIES FOR CHANGES IN ALL INDEPENDENT VARIABLES
***WITH LONG-FRESE COMMAND "MCHANGE"

mchange  // uses observed value method
mchange, centered
mchangeplot, sig(.05)  // plots changes in probability from (default) standard deviation change in all IVs


* EFFECTS OF X ON Y* (USING PROBIT ONLY)

listcoef


* ORDERED LOGIT ANALYSIS
ologit loctri groups

* GENERATE CUMULATIVE PREDICTED LOGITS FOR GROUPS=3  
display .72+(-.42*3) // Cumulative logit for groups=3, category 1
display _b[/cut1]-_b[groups]*3
display 2.77+(-.42*3) // Cumulative logit for groups=3, category 2
display _b[/cut2]-_b[groups]*3

* CUMULATIVE PROBABILITIES FOR GROUPS=3
display exp(-.54)/(1+exp(-.54))
display exp(1.51)/(1+exp(1.51))

* GENERATE CUMULATIVE PREDICTED LOGITS FOR GROUPS=4
display _b[/cut1]-_b[groups]*4 // Cumulative logit for groups=4, category 1 
display _b[/cut2]-_b[groups]*4  // Cumulative logit for groups=4, category 2 

* CUMULATIVE PROBABILITIES FOR GROUPS=4
display exp(-.96)/(1+exp(-.96))
display exp(1.09)/(1+exp(1.09))

*  NOW LET STATA CREATE ALL PREDICTED PROBABILITIES AND COMPARE TO PROBIT PROBABILITIES
predict lowparl medparl highparl

tabstat lowparl medparl highparl lowpar medpar highpar, stat (mean) by (groups)

*ODDS INTERPRETATION

ologit loctri groups
listcoef  //E^XB SAYS THAT ONE UNIT CHANGE IN X PRODUCES A 1.52 FACTOR CHANGE IN THE CUMULATIVE ODDS

*CUMULATIVE ODDS OF BEING AT OR BELOW A GIVEN CATEGORY FOR GROUPS=1
display .576/(1-.576)  //=1.36   FOR CATEGORY 1
display .913/(1-.913) //=10.49   FOR CATEGORY 2

*CUMULATIVE ODDS OF BEING AT OR BELOW A GIVEN CATEGORY FOR GROUPS=2
display .471/(1-.471)  //=.89    FOR CATEGORY 1
display .873/(1-.873) //=6.97    FOR CATEGORY 2

*CUMULATIVE ODDS FOR GROUPS=3
display .369/(1-.369)  //= .58
display .819/(1-.819)  //= 4.52

*CUMULATIVE ODDS FOR GROUPS=4
display .277/(1-.277)  //=.38
display .748/(1-.748) //=2.97

*SO:  CHANGING FROM GROUPS=1 TO GROUPS=2 DECREASED THE CUMULATIVE ODDS OF BEING IN CATEGORY 1 (LOW) OR BELOW 
*      BY A FACTOR OF 1.52 (1.36/.89)
*        CHANGING FROM GROUPS=1 TO GROUPS=2 DECREASED THE CUMULATIVE ODDS OF BEING IN CATEGORY 2 (MEDIUM) OR BELOW
*      BY A FACTOR OF 1.52 (10.49/6.97)

* CHANGING FROM GROUPS=3 TO GROUPS=4 DECREASED THE CUMULATIVE ODDS OF BEING IN CATEGORY 1 OR BELOW BY FACTOR OF 1.52 (.58/.38)
* CHANGING FROM GROUPS=3 TO GROUPS=4 DECREASED THE CUMULATIVE ODDS OF BEING IN CATEGORY 2 OR BELOW BY FACTOR OF 1.52 (4.52/2.97)

*CUMULATIVE ODDS OF BEING ABOVE A GIVEN CATEGORY FOR GROUPS=1
display .424/.576  //=.736   FOR CATEGORY 1
display .087/.913 //= .095   FOR CATEGORY 2

*CUMULATIVE ODDS OF BEING ABOVE A GIVEN CATEGORY FOR GROUPS=2
display .529/.471     //  = 1.12 FOR CATEGORY 1
display .127/.873     //  = .145 FOR CATEGORY 2

**SO:  CHANGING FROM GROUPS=1 TO GROUPS=2 INCREASED THE ODDS OF BEING ABOVE CATEGORY 1 BY 1.12/.736 = 1.52
*******CHANGING FROM GROUPS=1 TO GROUPS=2 INCREASED THE ODDS OF BEING ABOVE CATEGORY 2 BY .145/.095 = 1.52


*SO ORDERED LOGIT IS A "PROPORTIONAL ODDS" MODEL -- CONSTANT FACTOR CHANGE IN THE CUMULATIVE ODDS BUT NONCONSTANT FACTOR
*CHANGE IN THE CUMULATIVE PROBABILITIES


* TESTING PROPORTIONAL ODDS OR PARALLEL REGRESSION ASSUMPTIONS
generate onecat=0
replace onecat=1 if loctri<=1  // DUMMY VARIABLE FOR BEING IN CATEGORY 1 OR BELOW
generate twocat=0
replace twocat=1 if loctri <=2  //DUMMY VARIABLE FOR BEING IN CATEGORY 2 OR BELOW
logit onecat groups
logit twocat groups

ologit loctri groups
brant, detail          // NOTE: Can only run brant after ologit, not oprobit
