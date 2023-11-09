import excel "/Users/wilsontai/Downloads/Country Patent ETR.xlsx", sheet("Sheet1") clear

foreach var of varlist * {
   rename `var' `=`var'[1]'
}
drop in 1 
destring, replace


encode Country, gen(country1)
xtset country1 Year 


///Turning data into logs
gen lnquantfrac = log(1 + quantfrac)
gen lnqualfrac = log(1 + qualfrac)
gen lnbackwardfrac = log(backwardfrac)
replace lnbackwardfrac=0 if lnbackwardfrac==.
gen lnCIT = log(1 + CIT)
gen lnETR = log(ETR)
replace lnETR=0 if lnETR==.
gen lGDP = log(GDP)
gen lGDPPerCap = log(GDPPerCap)
gen lResearcher = log(Researcher)
gen lOpeness = log(Openess)
replace lOpeness=0 if lOpeness==.
gen lPopGrowth = log(Population_Growth)
replace lPopGrowth=0 if lPopGrowth==.
gen lCollege = log(College)
replace lCollege =0 if lCollege==.
gen lRandD = log(RandD)




//baseline (table 2)
xtpoisson quantfrac lnETR lGDPPerCap lRandD lPopGrowth lOpeness lResearcher  i.Year, fe i(country1) vce(robust)
eststo reg1
xtpoisson quantfrac L3.(lnETR) i.Year, fe i(country1) vce(robust)
eststo reg2
xtpoisson quantfrac L3.(lnETR) lGDPPerCap lRandD lPopGrowth lOpeness lResearcher i.Year, fe i(country1) vce(robust)
eststo reg3


xtpoisson qualfrac lnETR lGDPPerCap lRandD lPopGrowth lOpeness lResearcher i.Year, fe i(country1) vce(robust)
eststo reg4
xtpoisson qualfrac L3.lnETR i.Year, fe i(country1) vce(robust)
eststo reg5
xtpoisson qualfrac L3.lnETR lGDPPerCap lRandD lPopGrowth lOpeness lResearcher i.Year, fe i(country1) vce(robust)
eststo reg6



esttab reg1 reg2 reg3 reg4 reg5 reg6, b(3) modelwidth(10) star( * 0.10 ** 0.05 *** 0.01) nocons se drop(*.Year) rename (L3.lnETR lnETR lPopGrowth Population_Growth lGDP GDP lGDPPerCap GDPPerCap lResearcher Researchers lCollege College lRandD RandD lOpeness Openess) 

esttab reg1 reg2 reg3 reg4 reg5 reg6 using table.tex, replace b(3) modelwidth(10) star( * 0.10 ** 0.05 *** 0.01) order(lnETR L3.lnETR) rename (lnbackwardfrac BackwardCitation lPopGrowth Population_Growth lGDP GDP lGDPPerCap GDPPerCap lResearcher Researchers lCollege College lRandD RandD lOpeness Openess) addnotes("Robust standard errors in parentheses. All specifications include country and year fixed effects.") nocons se drop(*.Year) label mgroups("Quantity" "Quality", pattern(1 0 0  1 0 0  ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) collabels(none) nomtitles 



///Other lags (table 3)
xtpoisson quantfrac L1.lnETR lGDPPerCap lRandD lPopGrowth lOpeness lResearcher i.Year, fe i(country1) vce(robust)
eststo reg1
xtpoisson quantfrac L2.lnETR lGDPPerCap lRandD lPopGrowth lOpeness lResearcher i.Year, fe i(country1) vce(robust)
eststo reg2
xtpoisson quantfrac L3.lnETR lGDPPerCap lRandD lPopGrowth lOpeness lResearcher i.Year, fe i(country1) vce(robust)
eststo reg3
xtpoisson quantfrac L4.lnETR lGDPPerCap lRandD lPopGrowth lOpeness lResearcher i.Year, fe i(country1) vce(robust)
eststo reg4
xtpoisson quantfrac L5.lnETR lGDPPerCap lRandD lPopGrowth lOpeness lResearcher i.Year, fe i(country1) vce(robust)
eststo reg5
xtpoisson qualfrac L1.lnETR lGDPPerCap lRandD lPopGrowth lOpeness lResearcher i.Year, fe i(country1)  vce(robust)
eststo reg6
xtpoisson qualfrac L2.lnETR lGDPPerCap lRandD lPopGrowth lOpeness lResearcher i.Year, fe i(country1) vce(robust)
eststo reg7
xtpoisson qualfrac L3.lnETR lGDPPerCap lRandD lPopGrowth lOpeness lResearcher i.Year, fe i(country1) vce(robust)
eststo reg8
xtpoisson qualfrac L4.lnETR lGDPPerCap lRandD lPopGrowth lOpeness lResearcher i.Year, fe i(country1) vce(robust)
eststo reg9
xtpoisson qualfrac L5.lnETR lGDPPerCap lRandD lPopGrowth lOpeness lResearcher i.Year, fe i(country1) vce(robust)
eststo reg10
esttab reg1 reg2 reg3 reg4 reg5 reg6 reg7 reg8 reg9 reg10, b(3) star( * 0.10 ** 0.05 *** 0.01) modelwidth(7) se drop(*.Year)
esttab reg1 reg2 reg3 reg4 reg5 reg6 reg7 reg8 reg9 reg10 using table.tex, replace b(3) modelwidth(10) star( * 0.10 ** 0.05 *** 0.01) drop (*.Year  lPopGrowth  lGDPPerCap lResearcher  lCollege  lRandD  lOpeness) addnotes("Robust standard errors in parentheses. All specifications include country and year fixed effects.") nocons se label mgroups("Quantity" "Quality", pattern(1 0 0 0 0 1 0 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) collabels(none) nomtitles



//Star inventors (table 4)
//Heterogenous Industries (table 5 and 6)




//Truncation (table 7)
drop if Year > 2014
xtpoisson quantfrac L3.lnETR lGDPPerCap lRandD lPopGrowth lOpeness lResearcher i.Year, fe i(country1) vce(robust)
eststo reg1
xtpoisson qualfrac L3.lnETR lGDPPerCap lRandD lPopGrowth lOpeness lResearcher i.Year, fe i(country1) vce(robust)
eststo reg2


drop if Year > 2013
xtpoisson quantfrac L3.lnETR lGDPPerCap lRandD lPopGrowth lOpeness lResearcher i.Year, fe i(country1) vce(robust)
eststo reg3
xtpoisson qualfrac L3.lnETR lGDPPerCap lRandD lPopGrowth lOpeness lResearcher i.Year, fe i(country1) vce(robust)
eststo reg4


esttab reg1 reg2 reg3 reg4, b(3) star( * 0.10 ** 0.05 *** 0.01) modelwidth(10) se drop(*.Year) order (L3.lnETR)
esttab reg1 reg2 reg3 reg4 using table.tex, replace b(3) modelwidth(10) star( * 0.10 ** 0.05 *** 0.01) rename (lnbackwardfrac BackwardCitation lPopGrowth Population_Growth lGDP GDP lGDPPerCap GDPPerCap Researcher Researchers lCollege College lRandD RandD lOpeness Openess) addnotes("Robust standard errors in parentheses. All specifications include country and year fixed effects.") nocons se drop(*.Year) label mgroups("Sample Ending 2014" "Sample Ending 2013", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) collabels(none) mtitles("Quantity" "Quality" "Quantity" "Quality") eqlabels(" " " ") nonote







