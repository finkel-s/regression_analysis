*PS2030
*LINEAR PROBABILITY, LOGIT AND PROBIT MODELS

*use south africa civic education data"
*use "ps2030.safrica.dta"

*GENERATE NEW VARIABLES
recode locpart 0=0 *=1, gen (locdich)
label variable locdich "Participated in Any Local Activity"

* LINEAR PROBABILITY MODEL
scatter locdich educ1, jitter(20) || lfit locdich educ1
regress locdich educ1
predict lpmpred
summarize lpmpred if educ1==1
summarize lpmpred if educ1==3
summarize lpmpred if educ1==5
margins , at(educ1=(1(2)7))
tabstat lpmpred locdich, stats (mean) by (educ1)

predict lpmres,residuals

* SHOW NON-NORMALITY AND HETEROSKEDASTICITY
rvfplot,yline(0) jitter(20)
hist lpmres

* SHOW OUT-OF-BOUNDS PREDICTIONS
regress locdich educ1 groups know times
predict lpmpred2
list lpmpred2 if lpmpred2<=0 | lpmpred2>=1

**GOLDBERGER LPM MODEL WITH WEIGHTED LEAST SQUARES***
generate lpmpred3=lpmpred2
replace lpmpred3=.001 if lpmpred2<0
replace lpmpred3=.995 if lpmpred2>1
generate wt=1/(lpmpred3*(1-lpmpred3))
tab wt
regress locdich educ1 groups know times [weight=wt]
generate lpmpred4=_cons+_b[educ1]*educ1+_b[groups]*groups+_b[know]*know+_b[times]*times  //PREDICTED "LOCAL" IN THE ORIGINAL UNITS, NOT WEIGHTED
correlate lpmpred4 locdich
display .4073^2 //THIS IS THE R-SQUARED IN THE ORIGINAL UNITS OF LOCAL, NOT THE "WEIGHTED" UNITS AS IN THE WLS MODEL


**LOGIT
logit locdich times
predict plogit // PREDICTED PROBABILITIES
predict logodds,xb // PREDICTED LOGITS (INDEX FUNCTION XB)
gen odds1=exp(logodds)

display exp(_b[_cons]+_b[times]*0)/(1+exp(_b[_cons]+_b[times]*0))  //SIMULATES PROBABILITY OF 1 FOR PERSON AT TIMES=0
display exp(_b[_cons]+_b[times]*4)/(1+exp(_b[_cons]+_b[times]*4))  //SIMULATES PROBABILITY OF 1 FOR PERSON AT TIMES=4


tabstat plogit odds1 logodds,by(times) //GIVES PREDICTED PROBABILITY, ODDS AND PREDICTED LOGIT FOR ALL LEVELS OF X

margins, at (times=(0(1)4))       //THIS IS THE STATA MARGINS VERSION OF THIS SAME CALCULATION
marginsplot                     //PLOTS THIS WITH STATA MARGINSPLOT

sort times
twoway (connected plogit times)  //GRAPH NON-LINEAR PROBABILITIES WITH ALTERNATIVE COMMANDS



*WHAT IF INTERCEPT CHANGED FROM -.07 TO -1.0??  
generate plogit1=exp(-1.0+_b[times]*times)/(1+exp(-1.0+_b[times]*times))
tabstat plogit plogit1,by(times)

twoway (connected plogit times)(connected plogit1 times) //IT WILL TAKE *MANY* EXPOSURES TO CE TO GENERATE HIGH Ps

*WHAT IF INTERCEPT CHANGED FROM -.07 TO 1??  
generate plogit2=exp(1.0+_b[times]*times)/(1+exp(1.0+_b[times]*times))
tabstat plogit plogit1 plogit2,by(times)
twoway (connected plogit times)(connected plogit1 times) (connected plogit2 times)

*WHAT IF SLOPE CHANGED FROM .47 to .10?
generate plogit3= exp(_b[_cons]+.1*times)/(1+exp(_b[_cons]+.1*times))
tabstat plogit plogit3 ,by(times)  //MUCH SLOWER CHANGE IN THE PS

*WHAT IF SLOPE CHANGED FROM .47 to 1?
generate plogit4=exp(_b[_cons]+1*times)/(1+exp(_b[_cons]+1*times))
tabstat plogit plogit3 plogit4 ,by(times)  //MUCH FASTER CHANGE IN THE PS

twoway (connected plogit times)(connected plogit3 times) (connected plogit4 times)


**PROBIT

probit locdich times 
predict pprobit  /*Predicted Probabilities */
predict zscore,xb /* Predicted Z-scores (Index Function XB) */

margins, at(times=(0(1)4))    // NEED A PARENTHESES AFTER THE EQUAL SIGN!!!
marginsplot             //GRAPH OF NON-LINEAR PROBABILITIES

display normal(_b[_cons]+_b[times]*0)   //SIMULATES PROBABILITY OF 1 FOR PERSON AT TIMES=0
display normal(_b[_cons]+_b[times]*4)   //SIMULATES PROBABILITY OF 1 FOR PERSON AT TIMES=4


display invnorm(.483)  //GIVES THE Z-SCORE FOR A PERSON WITH A PROBABILITY OF .483, I.E. GOES FROM P TO Z
//NOTE THAT THE RESULT HERE, -.042, IS THE SAME AS THE INTERCEPT TERM IN THE PROBIT REGRESSION, I., WHEN X=0, THE PREDICTED Z SCORE

display invnorm(.5)   //GIVES THE Z-SCORE FOR A PERSON WITH A PROBABILITY OF .5, I.E. GOES FROM P TO Z
tabstat pprobit zscore, by(times) //GIVES PREDICTED PROBABILITY AND PREDICTED Z-SCORE FOR ALL LEVELS OF X
tabstat pprobit plogit, by(times) //COMPARES PREDICTED PROBABILITIES FOR PROBIT AND LOGIT MODEL


****INTERPRETATION OF COEFFICIENTS


***ODDS AND FACTOR CHANGE IN ODDS INTERPRETATION IN LOGIT

logit locdich know
margins, at(know=0)        // SHOWS THE PROBABILITY FOR SOMEONE WITH NO KNOWLEDGE(=0)
display .245/(1-.245)     //GIVES THE "ODDS" FOR A PROBABILITY OF .245,  I.E. GOES FROM P TO P/(1-P)
display ln(.245/(1-.245))  //GIVES THE "LOGIT" FOR A PROBABILITY OF .245, I.E. GOES FROM ODDS TO LOG-ODDS
//NOTE THAT THE RESULT HERE, -1.13, IS THE SAME AS THE INTERCEPT TERM IN THE LOGIT REGRESSION, I.E. WHEN X=0, THE PREDICTED LOG-ODDS

listcoef                   //SHOWS EFFECT OF X ON THE "ODDS" BY EXPONENTIATING THE B FROM THE LOGIT MODEL  -- HERE IT IS 1.615
display exp(.4795)        //VERIFIES THIS


predict logodds2, xb   //GENERATION OF THE PREDICTED LOG-ODDS FOR ALL LEVELS OF KNOWLEDGE
generate odds2=exp(logodds2) //MANUAL GENERATION OF THE PREDICTED ODDS FOR ALL LEVELS OF KNOWLEDGE
tabstat logodds2 odds2,by(know)

// EXERCISE:  VERIFY THE CONSTANT FACTOR CHANGE IN THE ODDS

logit locdich know, or  //OPTION IN STATA TO GO STRAIGHT TO ODDS INTERPRETATION

logit locdich know groups educ1 i.male 
listcoef  //COMPARE SD CHANGE IN ALL VARIABLES IN MULTIPLE REGRESSION IN TERMS OF PREDICTED ODDS


***PROBABILITY AND MARGINAL CHANGE INTERPRETATIONS
probit locdich groups know i.student i.male

**USE MARGINS FOR CHANGES IN PREDICTED PROBABILITIES

//MER:  All VARIABLES AT THEIR MINIMUM
margins, at(groups=0 know=0 student=0 male=0)
//MER:  ALL VARIABLES AT THEIR MAXIMUM
margins, at(groups=5 know=8 student=1 male=1)

//CHANGES IN PREDICTED PROBABILITIES:  MARGINAL CHANGE 
margins, dydx (*) //ALL OTHER VARIABLES AT THIER OBSERVED VALUES -- AND CAUTION IN INTERPRETING THE DUMMY VARIABLES!
margins, dydx (*) atmeans //ALL OTHER VARIABLES SET AT THEIR MEANS -- AND CAUTION IN INTERPRETING THE DUMMY VARIABLES!

//CHANGES IN PREDICTED PROBABILITIES:  DISCRETE CHANGE 
//MER: RANGE OF GROUP MEMBERSHIPS

margins, at(groups=(0 5)) atmeans  // DISCRETE CHANGE FROM 0 to 5 GROUPS USING MARGINAL EFFECTS AT MEAN APPROACH IS: .36

//CHANGES IN PREDICTED PROBABILITIES:  DISCRETE CHANGE USING (DEFAULT) AVERAGE MARGINAL EFFECTS APPROACH

margins, at(groups=(0 5))  // DISCRETE CHANGE FROM 0 to 5 GROUPS, HOLDING OTHER VARS AT OBSERVED SAMPLE VALUES, IS: .33


//SET OTHER VARIABLES TO THEIR MINIMUM AND MAXIMUM
						   						 

margins, at(groups=(0 5) know=0 student=0 male=0) //SO DIFFERENCE BETWEEN MIN AND MAX ON GROUPS, HOLDING OTHER VARS AT MINIMUM, IS:.32

margins, at(groups=(0 5) know=8 student=1 male=1) //SO DIFFERENCE BETWEEN MIN AND MAX ON GROUPS, HOLDING OTHER VARS AT MAX, IS: .15

*WHY IS THIS THE SMALLEST CHANGE FROM MIN TO MAX ON GROUPS?


***GENERATE FULL DISCRETE AND MARGINAL CHANGE INFORMATION WITH MCHANGE

mchange, stats(all)           // OTHER VARIABLES HELD AT OBSERVED SAMPLE VALUES
mchange, stats(all) centered  // FOR DISCRETE CHANGES CENTERED AROUND MEAN
mchange, stats(all) centered atmeans // FOR DISCRETE CHANGES CENTERED AROUND MEAN, OTHER VARIABLES HELD AT MEAN (MEM)

***GIVE PREDICTED PROBABILITIES FOR DIFFERENT CATEGORIES OF DUMMY VARIABLES, REST AT MEAN OR OBSERVED VALUES
probit locdich groups know  i.student i.male
margins male student
margins male student, atmeans
mtable, at(male=(0 1) student=(0 1))  // THIS GIVES THE CROSS-TAB COMBINATION OF MALE STUDENTS/FEMALE STUDENTS, MALE NON-STUDENTS, ETC.


***LATENT VARIABLE Y* INTERPRETATION

probit locdich educ1 know groups i.male student
listcoef

display .19/1.233 //GIVES THE Y-STANDARDIZED COEFFICIENT FOR GROUPS -- ONE ADDITIONAL GROUP LEADS TO .16 STANDARD DEVIATION CHANGE IN Y*
display .168/1.233 //BEING MALE LEADS TO .14 STANDARD DEVIATION CHANGE IN Y*

display .19*1.973/1.233 //GIVES THE FULLY STANDARDIZED COEFFICIENT FOR GROUPS -- AN ADDITIONAL S.D. CHANGE IN GROUPS LEADS TO .33 S.D. CHANGE IN Y*
display .219*1.943/1.233  //SAME FOR KNOWLEDGE
display .168*.48/1.233  //SAME FOR MALE, BUT NOTE DIFFICULTIES IN INTERPRETATION FOR DUMMY VARIABLES, JUST LIKE BETAS IN REGULAR REGRESSION.
						//CAN LOOK IT AS A KIND OF WEIGHTED AVERAGE OF THE EFFECT, WEIGHTED BY PROPORTION MALES/FEMALES IN THE SAMPLE

