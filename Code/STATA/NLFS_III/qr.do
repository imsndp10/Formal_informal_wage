sum ln_wage if formal_emp_1==1, detail

gen f90 = r(p90)
gen f50 = r(p50)
gen f10 = r(p10)

sum ln_wage if formal_emp_1==0, detail

gen in90 = r(p90)
gen in50 = r(p50)
gen in10 = r(p10)

gen dif90=f90-in90
gen dif50=f50-in50
gen dif10=f10-in10

kdensity ln_wage if formal_emp_1 ==0, gen(evalm1 densm1) width(0.10) nograph 
kdensity ln_wage if formal_emp_1 ==1, gen(evalf1 densf1) width(0.10) nograph

set scheme s1color

label var evalf1 "Log(wage)"
label var evalm1 "Log(wage)"

graph twoway  (connected densf1 evalf1, msymbol(i) lpattern(dash) clwidth(medium) lc(red) )  /*
*/   (connected densm1  evalm1, msymbol(i) lpattern(longdash) clwidth(medium) lc(blue) )  /*
*/   , ytitle("Density")ylabel(0.0 0.2 0.4 0.6 0.8 1.0) /*
*/   xlabel(1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5)  /* 
*/   legend(pos(7) col(2) lab(1 "Informal")  lab(2 "Formal")    /*
*/   region(lstyle(none)) symxsize(8) keygap(1) textwidth(34) ) /*
*/   saving(nlsy00_dens,replace)

(file nlsy00_dens.gph saved)

* compute RIF for the 10th, 50th and 90th quantiles for formal and informal employees
forvalues qt = 10(40)90 {
gen rif_`qt'=.
}

pctile eval1=ln_wage if formal_emp_1==1 , nq(100)
kdensity ln_wage if formal_emp_1==1, at(eval1) gen(evalf densf) width(0.10) nograph

forvalues qt = 10(40)90 {
local qc = `qt'/100.0
replace rif_`qt'=evalf[`qt']+`qc'/densf[`qt'] if ln_wage>=evalf[`qt'] & formal_emp_1==1
replace rif_`qt'=evalf[`qt']-(1-`qc')/densf[`qt'] if ln_wage<evalf[`qt']& formal_emp_1==1
}

pctile eval2=ln_wage if formal_emp_1==0, nq(100)
kdensity ln_wage if formal_emp_1==0, at(eval1) gen(evalm densm) width(0.10) nograph

forvalues qt = 10(40)90 {
local qc = `qt'/100.0
replace rif_`qt'=evalm[`qt']+`qc'/densm[`qt'] if ln_wage>=evalm[`qt'] & formal_emp_1==0
replace rif_`qt'=evalm[`qt']-(1-`qc')/densm[`qt'] if ln_wage<evalm[`qt']& formal_emp_1==0
}

sort formal_emp_1
by formal_emp_1: sum rif_10 rif_50 rif_90

oaxaca rif_10 $experience $education $caste married_status_1 hh_child gender_1 $occupation $voctrain total_chores_hr overtime_1 $firmsize $industry, by(formal_emp_1) weight(0) swap svy
oaxaca rif_50 $experience $education $caste married_status_1 hh_child gender_1 $occupation $voctrain total_chores_hr overtime_1 $firmsize $industry, by(formal_emp_1) weight(0) swap svy
oaxaca rif_90 $experience $education $caste married_status_1 hh_child gender_1 $occupation $voctrain total_chores_hr overtime_1 $firmsize $industry, by(formal_emp_1) weight(0) swap svy

rqdeco rif_10 $experience $education $caste married_status_1 hh_child gender_1 $occupation $voctrain total_chores_hr overtime_1 $firmsize $industry, by(formal_emp_1) quantiles(0.1 0.5 0.9) vce(none)

counterfactual ln_wage $experience $education $caste married_status_1 hh_child gender_1 $occupation $voctrain total_chores_hr overtime_1 $firmsize $industry, group(formal_emp_1) quantiles(0.1 0.5 0.9)

//perform the MM procedure for these quantiles using Melly's rqdeco
rqdeco ln_wage $experience $education $caste married_status_1 hh_child gender_1 $occupation $voctrain total_chores_hr overtime_1 $firmsize $industry, by(formal_emp_1) quantiles(0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9) vce(none)

##
cdeco ln_wage $experience gender_1 $education $caste married_status_1 hh_child  $occupation $voctrain total_chores_hr overtime_1 $firmsize $industry, group(formal_emp_1) method(logit) quantiles(0.1 0.5 0.9)

*First all elements of the decomposition
mat tot=e(total_difference)
mat char=e(characteristics)
mat coef=e(coefficients)
mat quant=e(quantiles)
svmat tot 
svmat char 
svmat coef
svmat quant
twoway (line tot1 quant1) (line char1 quant1) (line coef1 quant1), ytitle(Quantile Effect) xtitle(Quantile) legend(order(1 "Total difference" 2 "Effects of characteristics" 3 "Effects of coefficients"))

*We plot also the effects of coefficients ("discrimination") with a 95% pointwise confidence interval and a 95% functional confidence interval
gen coef_point_lb=coef1-1.96*coef2
gen coef_point_ub=coef1+1.96*coef2
twoway (rarea coef3 coef4 quant1, bcolor(gs5)) (rarea coef_point_lb coef_point_ub quant1, bcolor(gs10)) (line coef1 quant, lcolor(black)), xtitle("Quantile") ytitle("Quantile Effect") title(Effects of coefficients) legend(order(3 "Point estimates" 1 "Uniform 95% confidence bands" 2 "Pointwise 95% confidence intervals") rows(3))


gen formal if formal_emp_1 ==1

# PLot mmm decomposition

rqdeco ln_wage $experience $education $caste married_status_1 hh_child gender_1 $occupation $voctrain total_chores_hr overtime_1 $firmsize $industry, by(formal_emp_1) quantiles(0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9) vce(bootstrap) reps(5)

matrix list r(results)
matrix list r(se)



matrix results=r(results)
matrix se=r(se)


svmat results, names(col)
svmat se, names(col)



*twoway (line total_differential quantile) (line characteristics quantile) (line coefficients quantile), title(Decomposition of differences in distribution) ytitle(Log wage effects) xtitle(Quantile) legend(order(1 "Total differential" 2 "Effects of characteristics" 3 "Effects of coefficients"))


generate lo_coef=coefficients-1.96*se_coefficients
generate hi_coef=coefficients+1.96*se_coefficients

generate lo_char=characteristics-1.96*se_characteristics
generate hi_char=characteristics+1.96*se_characteristics

generate lo_tot=total_differential-1.96*se_total_differential
generate hi_tot=total_differential+1.96*se_total_differential
		

twoway(rarea hi_tot lo_tot quantile, bcolor(gs13) legend(off))(rarea hi_coef lo_coef quantile, bcolor(gs13) legend(off))(rarea hi_char lo_char quantile, bcolor(gs13) legend(off))(line total_differential quantile, bcolor(gs13))(line coefficients quantile)(line characteristics quantile),title(Decomposition of differences in distribution) ytitle(Log wage effects) xtitle(Quantile) legend(order(1 "Total  differential" 2 "Effects of characteristics" 3 "Effects of coefficients"))




