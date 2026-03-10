****************************************************************
*This file is the datapack companion script to produce tables and figures
*included in the main body and online appendix of the paper
*Wage equalization and regional misallocation: Evidence from
*Italian and German Provinces (Boeri, Ichino, Moretti, Posch)
***************************************************************
global path C:/Users/proprietario/Desktop/jvab019_boeri_ichino_moretti_posch_datapack/datapack


set scheme s1color
graph set window fontface "Times New Roman"

use "$path/data/province_data.dta", clear

***************************************
*Section 1: Summary stats for 2010*****
***************************************

*TABLE B-1

generate dummy = 1

preserve
label var cpi_moretti "Local CPI"
label var hp_moretti "Local housing price"
label var wage_unif "Nominal wage - hourly"
label var rwage "Real wage - hourly"

local counter = 1
local filemethod = "replace"
local heading = "h1(nil) h2(\textbf{Italy}& Mean & SD & N  \\ \hline) h3(nil)"
local comma = "0 0 0 0 0"
foreach v of varlist gva cpi_moretti hp_moretti wage_unif ///
rwage nempl_rate nempl_rate_corr   {
    if `counter' > 1 {
     local filemethod = "append"
     local heading  = "h1(nil) h2(nil) h3(nil)"
	 local comma = "1c 2c 0 0 0"
    }
	  local trick : var label `v'
    label define dummy 1 "`trick'", modify
    label val dummy dummy
    tabout dummy using "$path/tables/summary_3.txt" if country==2 & `v'<. & year==2010,  ///
        `filemethod' c(mean `v' sd `v' N `v') ///
        f(`comma') sum `heading' ///
		style(tex) ///
        lines(none) ptotal(none)
*mac shift
    local counter = `counter' + 1
}

label var wage_unif "Nominal wage - daily"
label var rwage "Real wage - daily"
******
local counter = 1
local filemethod = "append"
local heading = "h1(nil) h2([.1 em] \textbf{Germany}&&&\\ \hline ) h3(nil)"
local comma = "0 0 0 0 0"
foreach v of varlist gva cpi_moretti hp_moretti wage_unif  ///
rwage  ///
nempl_rate    {
    if `counter' > 1 {
     local filemethod = "append"
     local heading  = "h1(nil) h2(nil) h3(nil)"
	 local comma = "1c 2c 0 0 0"
    }
	  local trick : var label `v'
    label define dummy 1 "`trick'", modify
    label val dummy dummy
    tabout dummy using "$path/tables/summary_3.txt" if country==1 & `v'<. & year==2010,  ///
        `filemethod' c(mean `v' sd `v' N `v' ) ///
        f(`comma') sum `heading' ///
		style(tex) ///
        lines(none) ptotal(none)
*mac shift
    local counter = `counter' + 1
}

restore
drop dummy

*******************************
*Section 2: MAPS & Histograms**
*******************************

*FIGURES 2, 3, 5, 7, B-3, B-4, B-5

preserve
keep if (year==2010 & country==1) | (year==2010 & country==2)

 foreach var in ///
gva cpi_moretti hp_moretti wage_unif nempl_rate nempl_rate_corr rwage {

	egen avg_`var' = mean(`var') , by(country)
	sum avg_`var' if country==1
	local ger_avg`var' = round(r(mean), 1)
	local ger_avg`var' = floor(`ger_avg`var'')
	sum avg_`var' if country==2
	local ita_avg`var' = round(r(mean), 1)
	local ita_avg`var' = floor(`ita_avg`var'')
	gen mdev_`var' = `var' - avg_`var'
	gen mdevpc_`var' = 100*mdev_`var'/avg_`var'

	quietly sum mdevpc_`var'
	local min=floor(r(min))
	local max=ceil(r(max))

	quietly sum mdevpc_`var'
	local min=floor(r(min))
	local max=ceil(r(max))

	local breaks `" "`min'" " " "'
	forvalues p = 10(10)90 {
		egen p`p'_`var' = pctile(mdevpc_`var'),  p(`p')
		qui sum p`p'_`var'
		local `p'_`var' = round(r(mean),0.01)
		local breaks    `" `breaks' "``p'_`var''" " " "'
	}
	local breaks `" `breaks' "`max'" "'
	local breaks `"`breaks'"'
	dis "w deviations percentile breaks"
	dis `breaks'


if "`var'"=="wage_unif" {
  local lb=-30
  local ub=30
  local hei=.55
}
else if "`var'"=="hp_moretti" {
  local lb=-100
  local ub=100
    local hei=.6
}
else if "`var'"=="cpi_moretti" {
  local lb=-100
  local ub=100
    local hei=.6
}

else if "`var'"=="gva" {
  local lb=-40
  local ub=50
    local hei=.5
}
else if "`var'"=="rwage" {
  local lb=-80
  local ub=80
    local hei=.55
}
else if "`var'"=="nempl_rate" {
  local lb=-40
  local ub=60
    local hei=.55
}
else if "`var'"=="nempl_rate_corr" {
  local lb=-40
  local ub=60
    local hei=.55
}

local width=(`ub'-`lb')/20

twoway (histogram mdevpc_`var' if high_gva==1 & country==1,  width(`width') frac color(midblue)) ///
       (histogram mdevpc_`var' if high_gva==0 & country==1,  ///
	   width(`width') frac fcolor(none) lcolor(black)), ///
     xscale(range(`lb' `ub')) xlabel(`lb'(20)`ub') xtick(`lb'(10)`ub')  ///
     yscale(range(0 `hei')) ylabel(0(.1)`hei') ytick(0(.1)`hei')  ///
	   legend(order(1 "West" 2 "East" )) title("Germany") ///
	   xtitle("Percent deviations from country mean") ///
	   saving("$path/temp/hist_`var'_ger.gph", replace)

twoway (histogram mdevpc_`var' if high_gva==1 & country==2, width(`width') frac color(midblue)) ///
       (histogram mdevpc_`var' if high_gva==0 & country==2, ///
	   width(`width') frac fcolor(none) lcolor(black)), ///
     xscale(range(`lb' `ub')) xlabel(`lb'(20)`ub') xtick(`lb'(10)`ub')  ///
     yscale(range(0 `hei')) ylabel(0(.1)`hei') ytick(0(.1)`hei')  ///
	   legend(order(1 "North" 2 "South" )) title("Italy") ///
	   xtitle("Percent deviations from country mean") ///
	   saving("$path/temp/hist_`var'_ita.gph", replace)

	   graph combine "$path/temp/hist_`var'_ita.gph" "$path/temp/hist_`var'_ger.gph"
	   graph export "$path/figures/hist_`var'.jpg", replace


	spmap mdevpc_`var' using "$path/data/prov2001xy_new" if country==2, ///
	id(map_id) fcolor(Reds2) clmethod(custom) clbreaks(`breaks')  ///
	saving("$path/temp/map_`var'_italy.gph", replace)

	spmap mdevpc_`var' using "$path/data/ror1xy" if country==1, ///
	id(map_id) fcolor(Reds2) clmethod(custom) clbreaks(`breaks') legenda(off) ///
	saving("$path/temp/map_`var'_germany.gph", replace)

	graph combine "$path/temp/map_`var'_italy.gph" "$path/temp/map_`var'_germany.gph" /*, saving(map_`var'_combine.gph, replace)*/
	graph export "$path/figures/map_`var'_combine.jpg", replace

}

rm "$path/figures/map_nempl_rate_corr_combine.jpg"
rm "$path/figures/hist_nempl_rate_corr.jpg"

* FIGURE B-3
graph combine "$path/temp/map_nempl_rate_italy.gph" "$path/temp/map_nempl_rate_corr_italy.gph", ///
title("Uncorrected                       Corrected")
graph export "$path/figures/map_nempl_corrny_ita.jpg", replace

**********

*********

twoway (histogram mdevpc_nempl_rate if high_gva==1 & country==2, width(4) color(midblue)) ///
       (histogram mdevpc_nempl_rate if high_gva==0 & country==2, width(4) ///
	   fcolor(none) lcolor(black)), ///
	   legend(order(1 "North" 2 "South" )) ///
	   xtitle("Percent deviations from country mean") ///
	   title("Uncorrected") ///
	   saving("$path/temp/hist_nempl_rate_s.gph", replace)

twoway (histogram mdevpc_nempl_rate_corr if high_gva==1 & country==2, width(4) color(midblue)) ///
       (histogram mdevpc_nempl_rate_corr if high_gva==0 & country==2, width(4) ///
	   fcolor(none) lcolor(black)), ///
	   legend(order(1 "North" 2 "South" )) ///
	   xtitle("Percent deviations from country mean") ///
	   title("Corrected") ///
	   saving("$path/temp/hist_nempl_rate_corr_s.gph", replace)

graph combine "$path/temp/hist_nempl_rate_s.gph" "$path/temp/hist_nempl_rate_corr_s.gph"
graph export "$path/figures/hist_nempl_corrny_ita.pdf", replace


restore


*FIGURE B-1
preserve
keep if (year==2010 & country==2)
spmap irr_rate using "$path/data/prov2001xy_new" if country==2, ///
id(map_id) fcolor(Reds2) clmethod(custom) clbreaks(7 10 14 20 31)
graph export "$path/figures/map_irr_rate_ita.pdf", replace

twoway (histogram irr_rate if high_gva==1 & country==2, width(1) frac color(midblue)) ///
       (histogram irr_rate if high_gva==0 & country==2, width(1) ///
	    frac fcolor(none) lcolor(black)),  ///
      legend(order(1 "North" 2 "South" )) title("Italy")
     graph export "$path/figures/hist_irr_rate.pdf", replace

restore



***************************************
*Section 3: SCATTER PLOTS**************
***************************************

label var high_gva "\% Difference"

replace nempl_rate_corr=nempl_rate if country==1 //use normal non-employment rate for Germany

foreach x of varlist irr_rate gva cpi_moretti hp_moretti wage_unif nempl_rate nempl_rate_corr rwage {
cap drop l`x'
g l`x'=log(`x')
}


******
*FIGURE 4
twoway lfit lwage_unif lgva if country==1 & year==2010, lp(solid) || ///
scatter lwage_unif lgva if high_gva==0 & country==1 & year==2010,  msymbol(D)  msize(small) mc(orange_red) || ///
scatter lwage_unif lgva if high_gva==1 & country==1 & year==2010,  msymbol(O)  msize(small) mc(blue) ///
ylabel(4(.2)5) legend(order(2 "East" 3 "West")) ///
xtitle("Log value added") ytitle("Log nominal wage")
graph export "$path/figures/lgva_lnomwage_ger.pdf", replace

twoway lfit lwage_unif lgva if country==2 & year==2010, lp(solid) || ///
scatter lwage_unif lgva if high_gva==0 & country==2 & year==2010, msymbol(D)  msize(small) mc(orange_red) || ///
scatter lwage_unif lgva if high_gva==1 & country==2 & year==2010, msymbol(O)  msize(small) mc(blue) ///
ylabel(1.5(.2)2.5) legend(order(2 "South" 3 "North")) ///
xtitle("Log value added") ytitle("Log nominal wage")
graph export "$path/figures/lgva_lnomwage_ita.pdf", replace

**********
*FIGURE 6
twoway lfit lnempl_rate lgva if country==1 & year==2010, lp(solid) || ///
scatter lnempl_rate lgva if high_gva == 0 & country==1 & year==2010, msymbol(D) msize(small) mc(orange_red) || ///
scatter lnempl_rate lgva if high_gva == 1 & country==1 & year==2010, msymbol(O)  msize(small) mc(blue) ///
ylabel(2.8(.2)3.8) legend(order(2 "East" 3 "West")) ///
xtitle("Log value added") ytitle("Log non-employment rate")
graph export "$path/figures/lgva_lnempl_ger.pdf", replace


twoway lfit lnempl_rate lgva if country==2 & year==2010, lp(solid) || ///
scatter lnempl_rate lgva if high_gva == 0 & country==2 & year==2010, msymbol(D) msize(small) mc(orange_red) || ///
scatter lnempl_rate lgva if high_gva == 1 & country==2 & year==2010, msymbol(O)  msize(small) mc(blue) ///
ylabel(3.2(.2)4.2) legend(order(2 "South" 3 "North")) ///
xtitle("Log value added") ytitle("Log non-employment rate")
graph export "$path/figures/lgva_lnempl_ita.pdf", replace

********
*FIGURE 8
twoway lfit lrwage lgva if country==1 & year==2010, lp(solid) || ///
scatter lrwage lgva if high_gva==0 & country==1 & year==2010, msymbol(D)  msize(small) mc(orange_red) || ///
scatter lrwage lgva if high_gva==1 & country==1 & year==2010, msymbol(O)  msize(small) mc(blue) ///
ylabel(4(.2)5) legend(order(2 "East" 3 "West")) ///
xtitle("Log value added") ytitle("Log real wage")
graph export "$path/figures/lgva_lrwage_ger.pdf", replace

twoway lfit lrwage lgva if country==2 & year==2010, lp(solid) || ///
scatter lrwage lgva if high_gva==0 & country==2 & year==2010, msymbol(D)  msize(small) mc(orange_red) || ///
scatter lrwage lgva if high_gva==1 & country==2 & year==2010, msymbol(O)  msize(small) mc(blue) ///
ylabel(1.7(.2)2.7) legend(order(2 "South" 3 "North")) ///
xtitle("Log value added") ytitle("Log real wage")
graph export "$path/figures/lgva_lrwage_ita.pdf", replace
**********

* FIGURE B-2
twoway lfit lnempl_rate_corr lgva if country==2 & year==2010, lp(solid) || ///
scatter lnempl_rate_corr lgva if high_gva==0 & country==2 & year==2010, msymbol(D) msize(small) mc(orange_red) || ///
scatter lnempl_rate_corr lgva if high_gva==1 & country==2 & year==2010, msymbol(O) msize(small) mc(blue) ///
ylabel(3(.2)4) legend(order(2 "South" 3 "North")) ///
xtitle("Log value added") ytitle("Log non-employment")
graph export "$path/figures/lgva_lnempl_corr_ita.pdf", replace
*********


*************************************
*Section 4: REGRESSION TABLES********
*************************************

rename nempl_rate_corr nempl_corr
rename realw_unif_manu rw_manu
rename w_unif_manu w_manu

foreach x in ///
wage_unif nempl_rate rwage nempl_corr irr_rate w_manu rw_manu {
cap drop l`x'
gen l`x'=log(`x')
forvalues i=1/2 {
  if `i'==1 & "`x'"=="irr_rate" {
    continue
  }
  label var high_gva "\% Difference"
eststo `x'`i': reg l`x' high_gva i.year if country==`i'
label var lgva "Log value added"
eststo gva_`x'`i': reg l`x' lgva i.year if country==`i'
eststo gva_area_`x'`i': reg l`x' lgva high_gva i.year if country==`i'
}
}



foreach x in wage_unif_corr rwage_corr {
cap drop l`x'
gen l`x'=log(`x')
eststo `x'2: reg l`x' high_gva i.year if country==2
sum `x' if country==1
local `x'2=r(N)
local `x'2=``x'2'/103

}



*********
***
*TABLE 4
 esttab gva_wage_unif2 gva_area_wage_unif2  gva_wage_unif1 gva_area_wage_unif1 ///
using "$path/tables/table_gva_area_comp_w.tex", ///
se(3) label keep(lgva) ///
nomtit nostar fragment nonum noobs noline nogaps replace

***
*TABLE 5
esttab gva_nempl_rate2 gva_area_nempl_rate2 gva_nempl_corr2 gva_area_nempl_corr2  gva_nempl_rate1 gva_area_nempl_rate1 ///
using "$path/tables/table_gva_area_comp_nempl_comb.tex", ///
se(3) label keep(lgva) ///
nomtit nostar fragment nonum noobs noline nogaps replace

****
*TABLE 6
esttab gva_irr_rate2 gva_area_irr_rate2  ///
using "$path/tables/table_gva_area_comp_irr_rate.tex", ///
se(3) label keep(lgva) ///
nomtit nostar fragment nonum noobs noline nogaps replace
****

*TABLE 7
esttab wage_unif2 rwage2 wage_unif1 rwage1 ///
using "$path/tables/table_w_rw.tex", ///
se(3) label keep(high_gva) ///
nomtit nostar fragment nonum noobs noline nogaps replace

*TABLE B-3
 esttab gva_w_manu2 gva_area_w_manu2  gva_w_manu1 gva_area_w_manu1 ///
using "$path/tables/table_gva_area_comp_w_manu.tex", ///
se(3) label keep(lgva) ///
nomtit nostar fragment nonum noobs noline nogaps replace

*TABLE B-4
esttab w_manu2 rw_manu2 w_manu1 rw_manu1 ///
using "$path/tables/table_w_rw_manu.tex", ///
se(3) label keep(high_gva) ///
nomtit nostar fragment nonum noobs noline nogaps replace
*********

****
*TABLE B-5
esttab wage_unif2 rwage2 wage_unif_corr2 rwage_corr2 ///
using "$path/tables/table_w_rw_corrny_ita.tex", ///
se(3) label keep(high_gva) ///
nomtit nostar fragment nonum noobs noline nogaps replace
****

***************************************
*Section 5: Counterfactual analysis****
***************************************

use "$path/data/province_data.dta", clear

g lgva=log(gva)
g lempl=log(empl_rate)
g lwage=log(wage_unif)

reg lwage lgva i.year if country==1
local slope_wage=_b[lgva]
reg lempl lgva i.year if country==1
local slope_empl=_b[lgva]
keep if country==2
keep if year==2010


rename gva gva_ncorr

keep gva_ncorr empl_rate_corr wage_unif country high_gva name_reg cod_prov name_prov map_id work_age_pop
rename wage_unif wage_orig
rename empl_rate_corr empl_orig


foreach x in gva_ncorr {
    foreach i of numlist 0 5 10 20 {
      foreach c in ncorr {
        foreach m in p50 {
      preserve

      gen temp=-`x'
      sort temp
      gen count=_n
      g top=(count<=`i')
      if `i'==0 {
        replace top=1 if high_gva==1
      }
      drop temp count
      local gva_var="`x'"
if "`x'"=="empl" {
  local gva_var="gva_`c'"
}
sum `gva_var' if top, det
local max_gva=r(`m')
sum wage_orig if top, det
local max_wage=r(`m')
sum empl_orig if top, det
local max_empl=r(`m')

//wage assuming slope=1
g wage_opt1=wage_orig if top
replace wage_opt1=(`gva_var'/`max_gva')*`max_wage' if top==0
g empl_opt1=empl_orig if top
replace empl_opt1=`max_empl' if top==0
//slope like in germany
g wage_opt2=wage_orig if top
replace wage_opt2=(1-`slope_wage'*((`max_gva'-`gva_var')/`max_gva'))*`max_wage' if top==0
g empl_opt2=empl_orig if top
replace empl_opt2=(1-`slope_empl'*((`max_gva'-`gva_var')/`max_gva'))*`max_empl' if top==0

g expinc_orig=160*wage_orig*empl_orig/100 //make monthly labor income

g expinc_opt1=160*wage_opt1*empl_opt1/100 //make monthly labor income

g expinc_opt2=160*wage_opt2*empl_opt2/100 //make monthly labor income

g expinc_delta_opt1=100*(expinc_opt1-expinc_orig)/expinc_orig
g expinc_delta_opt2=100*(expinc_opt2-expinc_orig)/expinc_orig
g wage_delta_opt1=100*(wage_opt1-wage_orig)/wage_orig
g wage_delta_opt2=100*(wage_opt2-wage_orig)/wage_orig
g empl_delta_opt1=100*(empl_opt1-empl_orig)/empl_orig
g empl_delta_opt2=100*(empl_opt2-empl_orig)/empl_orig


g expinc_diff_opt1=(expinc_opt1-expinc_orig)
g expinc_diff_opt2=(expinc_opt2-expinc_orig)
g wage_diff_opt1=(wage_opt1-wage_orig)
g wage_diff_opt2=(wage_opt2-wage_orig)
g empl_diff_opt1=(empl_opt1-empl_orig)
g empl_diff_opt2=(empl_opt2-empl_orig)

g xvar="`x'"
g corr="`c'"
g stat="`m'"
g top_nr=`i'

save "$path/temp/cfct_`x'_`c'_`m'_top`i'", replace

restore
}
}
}
}


************************
*TABLES 9, B-6, B-7, B-8

foreach x in gva_ncorr {
    foreach i of numlist 0 5 10 20 {
      foreach c in  ncorr {
        foreach m in p50 {
          use "$path/temp/cfct_`x'_`c'_`m'_top`i'", clear


reshape long expinc wage empl expinc_delta wage_delta empl_delta expinc_diff wage_diff empl_diff , ///
        i(country high_gva name_reg cod_prov name_prov) j(postfix) string

g scenario=.
replace scenario=0 if regexm(postfix,"orig")
replace scenario=1 if regexm(postfix,"opt2")
replace scenario=2 if regexm(postfix,"opt1")
label define scen 0 "Status quo \hspace{2em}" 1 "Counterfactual 1" 2 "Counterfactual 2"
label val scenario scen
label var scenario "Scenario"

label define high_gva_ita 0 "South" 1 "North"
label val high_gva high_gva_ita


local vars  wage empl expinc
label var expinc "\textbf{Aggregate labor income per capita:} \textit{in Euros per month} "
label var wage "\textbf{Average hourly wage:} \textit{in Euros}"
label var empl "\textbf{Employment rate:} \textit{in \% corrected for informal work}"
label var high_gva "Region"



  foreach w of numlist 1 {
    local counter = 0
    local filemethod = "replace"
    local heading = "h1(nil) h2(& \multicolumn{2}{c}{\textbf{South}} & \multicolumn{2}{c}{\textbf{North}} & \multicolumn{3}{c}{\textbf{Italy}}\\ & (1) & (2) & (3) & (4) & (5) & (6) & (7) \\ &Level&Change  &Level& Change  &Level& Change &Change \% \\ \cmidrule(lr){2-3} \cmidrule(lr){4-5} \cmidrule(lr){6-8} \vspace{-0.3em})"
    local stat= "mean"
    local weight=""
if `w'==1 {
  local weight="[aw=work_age_pop]"
}

foreach v of local vars {
  g `v'_south=`v' if high_gva==0
 g `v'_north=`v' if high_gva==1
 g `v'_diff_south=`v'_diff if high_gva==0
g `v'_diff_north=`v'_diff if high_gva==1
    if `counter' > 0 {
        local filemethod = "append"
        local heading = "h1(nil) h2(nil)"
    }
    local vlabel : var label `v'
    tabout scenario `weight' using "$path/tables/cfct_cr8_`x'_`c'_`m'_top`i'_w`w'.txt", `filemethod' ///
        `heading' h3("\\[-0.7em] \multicolumn{8}{l}{`vlabel'}\\") f(2c) c(`stat' `v'_south `stat' `v'_diff_south `stat' `v'_north `stat' `v'_diff_north `stat' `v' `stat' `v'_diff `stat' `v'_delta) sum style(tex) ptotal(none)
    local counter = `counter' + 1
    drop *south
    drop *north
  }
}
}
}
}
}


*********************
*FIGURES B-6 and B-7

foreach x in gva_ncorr {
    foreach i of numlist 0  {
      foreach c in ncorr {
        foreach m in p50 {
          use "$path/temp/cfct_`x'_`c'_`m'_top`i'", clear

          reshape long expinc wage empl expinc_delta wage_delta empl_delta expinc_diff wage_diff empl_diff, ///
                  i(country high_gva name_reg cod_prov name_prov) j(postfix) string

          g scenario=.
          replace scenario=0 if regexm(postfix,"orig")
          replace scenario=1 if regexm(postfix,"opt2")
          replace scenario=2 if regexm(postfix,"opt1")
          label define scen 0 "Status quo" 1 "Counterfactual 1" 2 "Counterfactual 2"
          label val scenario scen
          label var scenario "Scenario"

foreach var in empl wage {

  replace `var'_delta=. if top==1

  quietly sum `var'_delta
  local min=floor(r(min))
  local max=ceil(r(max))

  local breaks `" "`min'" " " "'
  forvalues p = 10(10)90 {
    egen p`p'_`var' = pctile(`var'_delta),  p(`p')
    qui sum p`p'_`var'
    local `p'_`var' = round(r(mean),0.01)
  }
    forvalues p = 10(10)90 {
    local last=`p'-10
    local next=`p'+10
    local 100_`var'=`max'
    local 0_`var'=`min'
    if ``p'_`var''==0 & ``next'_`var''==0 & ``last'_`var''!=0 {
      local breaks    `" `breaks' "-0.01" " " "'
  }
  else if ``p'_`var''==0 & ``next'_`var''!=0 & ``last'_`var''==0 {
    local breaks    `" `breaks' "0.01" " " "'
  }
  else if ``p'_`var''==0 & ``next'_`var''==0 & ``last'_`var''==0 {
    continue
  }
  else {
    local breaks    `" `breaks' "``p'_`var''" " " "'
  }
  }
  local breaks `" `breaks' "`max'" "'
  local breaks `"`breaks'"'
  dis "w deviations percentile breaks"
  dis `breaks'

          spmap `var'_delta if scenario==1 using "$path/data/prov2001xy_new", ///
          id(map_id) fcolor(Reds2) clmethod(custom) clbreaks(`breaks') ///
          ndlabel("North: Not adjusted")
          graph export "$path/figures/map_cfct_cr8_`x'_`c'_`m'_top`i'_`var'_opt1.pdf", replace

          spmap `var'_delta if scenario==2 using "$path/data/prov2001xy_new", ///
          id(map_id) fcolor(Reds2) clmethod(custom) clbreaks(`breaks') ///
          ndlabel("North: Not adjusted")
          graph export "$path/figures/map_cfct_cr8_`x'_`c'_`m'_top`i'_`var'_opt2.pdf", replace

}
}
}
}
}



***************************************
*Section 6: Collective bargaining*****
***************************************

*TABLE 3
use "$path/data/bargaining.dta", clear

keep jahr region cat1_2_w klausel_2_w klausel_use_2_w kl_pay_use_2_w

keep if region=="Ost" | region=="West"
encode region, gen(eastwest)

sort jahr
label var jahr "Year"

foreach x of varlist cat1_2_w klausel_2_w klausel_use_2_w kl_pay_use_2_w {
replace `x'=100*`x'
}



matrix barg = J(9,4, 0)
matrix colnames barg = West East West East
matrix rownames barg = 1996 1998 2001 2003 2005 2007 2009 2011 2013


local row = 0

foreach y in 1996 1998 2001 2003 2005 2007 2009 2011 2013 {
local row= `row'+1

	   local c = 1
		qui sum cat1_2_w if jahr == `y' & region=="West"
		matrix barg[`row',`c'] = r(mean)

		local c = `c'+2
		qui sum klausel_2_w if jahr == `y' & region=="West"
		matrix barg[`row',`c'] = r(mean)

		local c = 2
		qui sum cat1_2_w if jahr == `y' & region=="Ost"
		matrix barg[`row',`c'] = r(mean)

		local c = `c'+2
		qui sum klausel_2_w if jahr == `y' & region=="Ost"
		matrix barg[`row',`c'] = r(mean)

	}

 matrix list barg, f(%9.2f)

 esttab matrix(barg, fmt(2)) using "$path/tables/table_bargaining.tex", ///
 frag replace nomtit nogaps noobs


***************************************
*Section 7: Amenities******************
***************************************

*TABLE 8

use "$path/data/amenities.dta", clear

*Index of pleasant climate
label var lva_s24 "Index of pleasant climate (2019)"
eststo reg_nofe: reg lvalue lva_s24 if indicatore == "Indice del clima" & year==2019
eststo reg_fe: reg lvalue lva_s24 north if indicatore == "Indice del clima" & year==2019

 esttab reg_nofe reg_fe ///
using "$path/tables/amenities.tex", ///
b(2) se(2) label keep(lva_s24) ///
nomtit nostar fragment nonum noobs noline nogaps replace


* Temperature excursion over the year
label var lva_s24 "Temperature excursion over the year (2018)"
eststo reg_nofe: reg lvalue lva_s24 if indicatore == "Indice climatico di escursione termica" & year==2018
eststo reg_fe: reg lvalue lva_s24 north if indicatore == "Indice climatico di escursione termica" & year==2018

 esttab reg_nofe reg_fe ///
using "$path/tables/amenities.tex", ///
b(2) se(2) label keep(lva_s24) ///
nomtit nostar fragment nonum noobs noline nogaps append


* Air pollution – PM10 in the air
label var lva_s24 "Air pollution – PM10 in the air (2019)"
eststo reg_nofe: reg lvalue lva_s24 if indicatore == "Qualità dell'aria - Pm10" & year==2019
eststo reg_fe: reg lvalue lva_s24 north if indicatore == "Qualità dell'aria - Pm10" & year==2019

 esttab reg_nofe reg_fe ///
using "$path/tables/amenities.tex", ///
b(2) se(2) label keep(lva_s24) ///
nomtit nostar fragment nonum noobs noline nogaps append


* Population density (inhabitants per km2)
label var lva_s24 "Population density (inhabitants per km2) (2017)"
eststo reg_nofe: reg lvalue lva_s24 if indicatore == "Densità abitativa" & year==2017
eststo reg_fe: reg lvalue lva_s24 north if indicatore == "Densità abitativa" & year==2017

 esttab reg_nofe reg_fe ///
using "$path/tables/amenities.tex", ///
b(2) se(2) label keep(lva_s24) ///
nomtit nostar fragment nonum noobs noline nogaps append


* Number of crimes per 100 th. inhabitants
label var lva_s24 "Number of crimes per 100 th. inhabitants (2019)"
eststo reg_nofe: reg lvalue lva_s24 if indic == 89 & year==2019
eststo reg_fe: reg lvalue lva_s24 north if indic == 89 & year==2019

 esttab reg_nofe reg_fe ///
using "$path/tables/amenities.tex", ///
b(2) se(2) label keep(lva_s24) ///
nomtit nostar fragment nonum noobs noline nogaps append


* Number of murders per 100 th. inhabitants
label var lva_s24 "Number of murders per 100 th. inhabitants (1999)"
eststo reg_nofe: reg lvalue lva_s24 if indic == 121 & year==1999
eststo reg_fe: reg lvalue lva_s24 north if indic == 121 & year==1999

 esttab reg_nofe reg_fe ///
using "$path/tables/amenities.tex", ///
b(2) se(2) label keep(lva_s24) ///
nomtit nostar fragment nonum noobs noline nogaps append


*Number of students per class, Primary
label var lva_s24 "Number of students per class (primary, 2017-18)"
gen l_b_class_size_e = log(b_class_size_e)
eststo reg_nofe: reg l_b_class_size_e lva_s24 if indicatore == "Indice del clima" & year==2019 //the selection is only because this variable came as long in the data
eststo reg_fe: reg l_b_class_size_e lva_s24  north if indicatore == "Indice del clima" & year==2019

 esttab reg_nofe reg_fe ///
using "$path/tables/amenities.tex", ///
b(2) se(2) label keep(lva_s24) ///
nomtit nostar fragment nonum noobs noline nogaps append


*Number of students per class, Junior High School
label var lva_s24 "Number of students per class (Junior high school, 2017-18)"
gen l_b_class_size_m = log(b_class_size_m)
eststo reg_nofe: reg l_b_class_size_m lva_s24 if indicatore == "Indice del clima" & year==2019
eststo reg_fe: reg l_b_class_size_m lva_s24  north if indicatore == "Indice del clima" & year==2019

 esttab reg_nofe reg_fe ///
using "$path/tables/amenities.tex", ///
b(2) se(2) label keep(lva_s24) ///
nomtit nostar fragment nonum noobs noline nogaps append


*Number of students per class, High School
label var lva_s24 "Number of students per class (high school, 2017-18)"
gen l_b_class_size_s = log(b_class_size_s)
eststo reg_nofe: reg l_b_class_size_s lva_s24 if indicatore == "Indice del clima" & year==2019
eststo reg_fe: reg l_b_class_size_s lva_s24  north if indicatore == "Indice del clima" & year==2019

 esttab reg_nofe reg_fe ///
using "$path/tables/amenities.tex", ///
b(2) se(2) label keep(lva_s24) ///
nomtit nostar fragment nonum noobs noline nogaps append


* Quality of health system (health related emigration)
label var lva_s24 "Quality of health system (health related emigration, 2019)"
eststo reg_nofe: reg lvalue lva_s24 if indicatore == "Emigrazione ospedaliera" & year==2019
eststo reg_fe: reg lvalue lva_s24 north if indicatore == "Emigrazione ospedaliera" & year==2019

 esttab reg_nofe reg_fe ///
using "$path/tables/amenities.tex", ///
b(2) se(2) label keep(lva_s24) ///
nomtit nostar fragment nonum noobs noline nogaps append

* GP doctors per 1000 inhabitants
label var lva_s24 "GP doctors per 1000 inhabitants (2019)"
eststo reg_nofe: reg lvalue lva_s24 if indicatore == "Medici di medicina generale" & year==2019
eststo reg_fe: reg lvalue lva_s24 north if indicatore == "Medici di medicina generale" & year==2019

 esttab reg_nofe reg_fe ///
using "$path/tables/amenities.tex", ///
b(2) se(2) label keep(lva_s24) ///
nomtit nostar fragment nonum noobs noline nogaps append

*Number of cultural shows per 10 square km
label var lva_s24 "Number of cultural shows per 10 square km (2019)"
eststo reg_nofe: reg lvalue lva_s24 if indicatore == "Densità dell'offerta culturale" & year==2019
eststo reg_fe: reg lvalue lva_s24 north if indicatore == "Densità dell'offerta culturale" & year==2019

 esttab reg_nofe reg_fe ///
using "$path/tables/amenities.tex", ///
b(2) se(2) label keep(lva_s24) ///
nomtit nostar fragment nonum noobs noline nogaps append


*Ranking of local food and wine quality
label var lva_s24 "Ranking of local food and wine quality (2007)"
eststo reg_nofe: reg lvalue lva_s24 if indicatore == "Ranking enogastronomico" & year==2007
eststo reg_fe: reg lvalue lva_s24 north if indicatore == "Ranking enogastronomico" & year==2007

 esttab reg_nofe reg_fe ///
using "$path/tables/amenities.tex", ///
b(2) se(2) label keep(lva_s24) ///
nomtit nostar fragment nonum noobs noline nogaps append
