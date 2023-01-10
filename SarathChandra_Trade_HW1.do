/*
INTERNATIONAL TRADE - HOME WORK 1 

PROFESSOR ARIELL RESHEF 
Paris School of Economics 

Submitted by: MUDIGONDA SARATH CHANDRA - M2 APE 

Date: 20/11/2022

*/

clear all
macro drop _all

global data "/Users/sarathchandra/Desktop/M2/International Trade/Take home 1"
global results "${data}/Results"
ssc install groups 
ssc install estout, replace
ssc install egenmore, replace 
ssc install reghdfe, replace 
ssc install ftools, replace 
ssc install ppmlhdfe
ssc install texsave

clear all

use "$data/DataForAssignment.dta"

// International Trade Homework Assignment 1 

// 2. a. Descriptive statistics 

///// Share of Uni-Directional zero trade dyads:
drop if exporter == importer // droping domestic trade 

gen tick = 0 
replace tick = 1 if trade == 0 & exporter != importer

collapse (count) trade, by(year tick) 
tempfile freq 
save `freq', replace 

clear all 
use "$data/DataForAssignment.dta"

collapse (count) trade, by(year) //gives count of total trade dyads year wise
rename trade tot //for convenience 

merge 1:m year using `freq'
drop if tick==0 //dropping all observations that have non-zero trade 
drop _merge

gen uni_sh_zero = trade/tot 
save `freq', replace 

///// share of bilateral zero trade dyads:

clear all
use "$data/DataForAssignment.dta"
drop if exporter == importer 

collapse (sum) trade, by(year pair_id) // there are only unique pair_id's after this

gen tick = 0 
replace tick = 1 if trade == 0 
collapse (count) trade, by(year tick)

tempfile ran
save `ran', replace 

clear all 
use "$data/DataForAssignment.dta"
drop if exporter == importer 
collapse (count) trade, by(year) // 4761 observations per year 
rename trade tot 

merge 1:m year using `ran' 
drop if tick==0
drop _merge

gen bi_sh_zero = trade/tot
merge 1:1 year using `freq', gen(_mer1)
drop _mer1 tick tot trade 

save "$results/freq.dta", replace 

*generating table of summary of the shares
 estpost summarize bi_sh_zero uni_sh_zero
 esttab using "$results/1ab.tex", cells("count mean sd min max") title("Zero trade dyads- Summary") replace


*graphing uni-directional and bilateral zero trade shares in the same graph 
graph twoway line bi_sh_zero uni_sh_zero year, xtitle("year") title("Shares of zero trade dyads") legend(size(small) label(1 "Bilateral zero trade dyads share") label(2 "Uni-Directional zero trade dyads share"))
gr export "$results/1a.png", replace 

// • Share of countries linked by RTAs, over time
clear all
use "$data/DataForAssignment.dta"

collapse (count) trade, by(year RTA) 
gen RTAshare = trade/4761 if RTA==1
replace RTAshare =0 if RTAshare==.
drop if RTAshare==0 //don't require them for calculating RTA shares so dropping them for convenience temporarily 

tempfile rta
save `rta', replace 

clear all 
use "$data/DataForAssignment.dta"

collapse (sum) trade, by(year)
merge 1:1 year using `rta'
replace RTAshare= 0 if RTAshare==.

drop RTA _merge // for convenience 

texsave using "$results/RTAshare.tex", replace

twoway line trade year, yaxis(2) ylabel( , angle(0) labsize(small))  ytitle("Total Trade") || line RTAshare year, yaxis(1) ylabel( , angle(0)) ytitle("RTAshare")  legend(label(1 "Total trade") label(2 "RTA Share")) title("RTA Shares and Overall Trade over time")
gr export "$results/1b.png", replace 

// •overall international export and import (drop "domestic exports") as ratio to GDP, over time.

clear all
use "$data/DataForAssignment.dta"

drop if exporter == importer 

bysort year : egen tot_trade = total(trade) if exporter != importer
bysort year : egen tot_GDP = total(GDPexp) if exporter == importer		// GDP counted only once for a country 

bys year : egen inter = max(tot_trade)
bys year : egen inter1 = max(tot_GDP)
gen trade_GDP = inter/inter1 //to calculate the shares

* generating graph 

twoway line trade_GDP year, title("Trade to GDP ratio year-wise") ytitle("") xtitle("year")
gr export "$results/1c.png", replace


// •cross-sectional characteristics (i.e., variables that do not change over time).

estpost tabstat DIST CNTG LANG CLNY , c(stat) stat(mean sd min max n) 
esttab using "$results/1de.tex", replace title("Cross-sectional characteristics")  

////// ESTIMATING THE GRAVITY MODEL 

//// USING ONLY 1986 

// b.Without fixed effects, using importer and exporter GDPs, in logs, OLS.

clear all
use "$data/DataForAssignment.dta"

keep if year==1996 

tab RTA // 556 exporter-importer pairs with RTAs so 278 country pairs 

gen logGDPexp = ln(GDPexp)
gen logGDPimp = ln(GDPimp)
gen logTrade = ln(trade) // Stata NA for all log0 observations  
gen logDist = ln(DIST)

count if trade == 0 // 334 observations with 0 trade are omitted here since log0 is undefined

est clear
eststo: reg logTrade logGDPexp logGDPimp logDist RTA CNTG  // OLS without fixed effects
	estadd local ExporterFE "NO"
	estadd local ImporterFE "NO"


// c.Standard model with exporter and importer fixed effects, in logs, OLS.
eststo: reghdfe logTrade logDist RTA CNTG, absorb(exporter importer) // GDP absorbed by fixed effects
	estadd local ExporterFE "YES"
	estadd local ImporterFE "YES"


// d.Standard model with exporter and importer fixed effects, Poisson, using all observations, including all possible zeros. And show that the adding up constraint holds for predicted trade flows.

eststo: ppmlhdfe trade logDist CNTG RTA, absorb(exporter importer)	d //GDP is absorbed by the fixed effects
	estadd local ExporterFE "YES"
	estadd local ImporterFE "YES"
	
* Adding up constraint 
predict pred_1, mu
collapse (sum) pred_1 trade, by(importer)
generate residual = abs(pred_1- trade)
summarize residual // mean 0.012 -> adding up constraint holds as mean of the sum of residuals by importer is 0 
	

// e.	Standard model with exporter and importer fixed effects, Poisson, using all observations, including all possible zeros, using import shares as dependent variable (a la Sotelo). Show that the adding up constraint holds for predicted trade flows.

bysort year importer : egen imp_t = total(trade) //generates total imports per year per importer 
gen import_share = trade/imp_t

eststo: ppmlhdfe import_share logDist CNTG RTA, absorb(exporter importer) d
	estadd local ExporterFE "YES"
	estadd local ImporterFE "YES"

*Adding up constraint 
predict pred_2, mu 

bysort exporter: egen LHS1 = total(pred_2)
* since these are import shares, we want each country's predicted value to sum to 1. 
sum(LHS1) //mean is 1 confirming that for each country the predicted import share adds up to 1. 

// f.Like d, but restricting sample to strictly positive flows.
drop if trade==0 // to consider only positive flows

eststo: ppmlhdfe trade logDist CNTG RTA, absorb(exporter importer)	 //GDP is absorbed by the fixed effects
	estadd local ExporterFE "YES"
	estadd local ImporterFE "YES"


// g.Like e, but restricting sample to strictly positive flows.
eststo: ppmlhdfe import_share logDist CNTG RTA, absorb(exporter importer)
	estadd local ExporterFE "YES"
	estadd local ImporterFE "YES"


// Compiling all the results into one table 

esttab using "$results/table1main.tex", replace b(3) se(3) keep($controls) star(* 0.10 ** 0.05 *** 0.01) scalars(ExporterFE "Exporter fixed effects" ImporterFE "Importerfixed effects") title("Structural Gravity model for 1996")



//// USING THE FULL PANEL

clear all 
use "$data/DataForAssignment.dta"

bysort year importer : egen imp_t = total(trade) //generates total imports per year per importer 
gen import_share = trade/imp_t


//h.	Estimate the RTA coefficient in a sample that includes only international flows (drop "domestic exports"). Do this three times: by OLS (in logs), PPML and PPML-Sotelo.

drop if exporter == importer //dropping domestic trade

* OLS (in logs)
gen logGDPexp = ln(GDPexp)
gen logGDPimp = ln(GDPimp)
gen logTrade = ln(trade)  
gen logDist = ln(DIST)

* Here, exporter and importer variables are coded as string variables. So we use the encode command to convert them to be used as factor variables for our fixed effects 
encode exporter, gen(ex)
encode importer, gen(im)

// OLS in logs 
est clear
eststo: reghdfe logTrade RTA, absorb(ex#im ex#year im#year) 
	estadd local ExporterImporterFE "YES"
	estadd local ExporterYearFE "YES"
	estadd local ImporterYearFE "YES"

// PPML 
eststo: ppmlhdfe trade RTA, absorb(ex#im ex#year im#year) 
	estadd local ExporterImporterFE "YES"
	estadd local ExporterYearFE "YES"
	estadd local ImporterYearFE "YES"

//PPML- Sotelo 

eststo: ppmlhdfe import_share RTA, absorb(ex#im ex#year im#year) 
	estadd local ExporterImporterFE "YES"
	estadd local ExporterYearFE "YES"
	estadd local ImporterYearFE "YES"

// compiling results into one table
esttab using "$results/table2a.tex", replace b(3) se(3) keep($controls) star(* 0.10 ** 0.05 *** 0.01) scalars(ExporterImporterFE "ExporterImporterFE" ExporterYearFE "ExporterYearFE" ImporterYearFE "ImporterYearFE") title("Estimating RTA coefficient for the full panel restricting to international trade") 
	
//i. Estimate the RTA coefficient in a sample that includes all flows (keep "domestic exports"). Do this three times: by OLS (in logs), PPML and PPML-Sotelo.

clear all
use "$data/DataForAssignment.dta"

gen logGDPexp = ln(GDPexp)
gen logGDPimp = ln(GDPimp)
gen logTrade = ln(trade) 
gen logDist = ln(DIST)

* Here, exporter and importer variables are coded as string variables. So we use the encode command to convert them to be used as factor variables for our fixed effects 
encode exporter, gen(ex)
encode importer, gen(im)

// OLS in logs 
est clear
eststo: reghdfe logTrade RTA, absorb(ex#im ex#year im#year) 
	estadd local ExporterImporterFE "YES"
	estadd local ExporterYearFE "YES"
	estadd local ImporterYearFE "YES"

// PPML 
eststo: ppmlhdfe trade RTA, absorb(ex#im ex#year im#year) 
	estadd local ExporterImporterFE "YES"
	estadd local ExporterYearFE "YES"
	estadd local ImporterYearFE "YES"

//PPML- Sotelo 
bysort year importer : egen imp_t = total(trade) //generates total imports per year per importer 
gen import_share = trade/imp_t

eststo: ppmlhdfe import_share RTA, absorb(ex#im ex#year im#year) 
	estadd local ExporterImporterFE "YES"
	estadd local ExporterYearFE "YES"
	estadd local ImporterYearFE "YES"

// compiling results into one table
esttab using "$results/table2i.tex", replace b(3) se(3) keep($controls) star(* 0.10 ** 0.05 *** 0.01) scalars(ExporterImporterFE ExporterYearFE ImporterYearFE) title("Estimating RTA coefficient for the full panel including domestic trade")  


///////// APPENDIX -> 1996 regressions just using DISTANCE instead of Log Distance

clear all
use "$data/DataForAssignment.dta"

keep if year==1996 

tab RTA // 556 exporter-importer pairs with RTAs so 278 country pairs 

gen logGDPexp = ln(GDPexp)
gen logGDPimp = ln(GDPimp)
gen logTrade = ln(trade) // Stata NA for all log0 observations  
gen logDist = ln(DIST)
count if trade == 0 // 334 observations with 0 trade are omitted here since log0 is undefined

est clear
eststo: reg logTrade logGDPexp logGDPimp logDist RTA CNTG 
	estadd local ExporterFE "NO"
	estadd local ImporterFE "NO"


// c.Standard model with exporter and importer fixed effects, in logs, OLS.
eststo: reghdfe logTrade logDist RTA CNTG, absorb(exporter importer) // GDP absorbed by fixed effects
	estadd local ExporterFE "YES"
	estadd local ImporterFE "YES"


// d.Standard model with exporter and importer fixed effects, Poisson, using all observations, including all possible zeros. And show that the adding up constraint holds for predicted trade flows.

eststo: ppmlhdfe trade DIST CNTG RTA, absorb(exporter importer)	d //GDP is absorbed by the fixed effects
	estadd local ExporterFE "YES"
	estadd local ImporterFE "YES"

*adding up constraint 
predict pred_1, mu //this gives the predicated trade values from the regression 

bysort exporter: egen LHS1 = total(pred_1)
bysort importer: egen RHS1 = total(trade) 

gen adding_constraint = RHS1 - LHS1 
sum(adding_constraint) // mean 0 

// e.	Standard model with exporter and importer fixed effects, Poisson, using all observations, including all possible zeros, using import shares as dependent variable (a la Sotelo). Show that the adding up constraint holds for predicted trade flows.

bysort year importer : egen imp_t = total(trade) //generates total imports per year per importer 
gen import_share = trade/imp_t

eststo: ppmlhdfe import_share DIST CNTG RTA, absorb(exporter importer) d
	estadd local ExporterFE "YES"
	estadd local ImporterFE "YES"

*adding up constraint 
predict pred_2, mu 

bysort exporter: egen LHS2 = total(pred_2)

gen adding_constraint1 = RHS1 - LHS2
sum(adding_constraint1) 


// f.Like d, but restricting sample to strictly positive flows.
drop if trade==0 

eststo: ppmlhdfe trade DIST CNTG RTA, absorb(exporter importer)	 //GDP is absorbed by the fixed effects
	estadd local ExporterFE "YES"
	estadd local ImporterFE "YES"


// g.Like e, but restricting sample to strictly positive flows.
eststo: ppmlhdfe import_share DIST CNTG RTA, absorb(exporter importer)
	estadd local ExporterFE "YES"
	estadd local ImporterFE "YES"


// Compiling all the results into one table 

esttab using "$results/table1appendix.tex", replace b(3) se(3) keep($controls) star(* 0.10 ** 0.05 *** 0.01) scalars(ExporterFE "Exporter fixed effects" ImporterFE "Importerfixed effects") title("Structural Gravity model for 1996 - with DISTANCE instead of Log Distance")

