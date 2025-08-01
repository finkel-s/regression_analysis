**CALCULATE AND PLOT MARGINAL PREDICTIONS
regress demsat c.educ1##c.educ1
margins, at(educ1=(1(1)7))
marginsplot, recast (line) recastci (rarea)

**CALCULATE AND PLOT MARGINAL SLOPES
margins, at(educ1=(1(1)7)) dydx(educ1)
marginsplot, recast (line) recastci (rarea)
