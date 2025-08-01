regress life c.docs##c.gdp

margins, dydx(docs) at(gdp=(0(2000)20000)) 

marginsplot, ///
    ytitle("Marginal Effect of Doctors on Life Expectancy") ///
    xtitle("GDP per Capita") ///
    title("Marginal Effect of Doctors as GDP Increases") ///
    recast(line) ciopts(recast(rline) lpattern(dash))
	