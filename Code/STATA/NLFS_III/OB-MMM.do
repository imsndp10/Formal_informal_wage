clear
set more off
//cd "/Users/sandeepsharma/Desktop/Research/Data Analysis/Formal informal wage/Formal_informal_wage/Data/Cleaned/Pooled"
cd "D:/Work/Sandeep_research/Data/Cleaned/Pooled"
use Pooled.dta
gen ln_wage = ln(hourly_wage)
label define formal_employment 0 "Informal Employment" 1 "Formal Employment"
//label value formal_emp_1 formality
//svyset [iw = weight], psu(psu) strata(domain217) singleunit(centered)

// Arrange variables as $global variable 

global experience "experience experience_sq" 
global voctrain "voc_train migrated_fr_job"
global hhchar "child_12" 
global indchar "overtime_40 tot_chores_hrs"
global education1 "education_Below_primary education_Primary education_Tenth_grade education_Secondary education_Bachelor education_Masters_above" 
global caste "caste_group_6_Janajati caste_group_6_Adhibasi caste_group_6_Madhesi caste_group_6_Dalit caste_group_6_Others"
global occupation "class_5_Plant_operator class_5_Agri_trade class_5_Clerical_sales class_5_Managers"
global industry "job_sector_Mining_utility job_sector_Construction job_sector_Manufacturing job_sector_Market_services job_sector_Non_Market_services job_sector_Arts_entertain"
global firmsize "sz_workplace_medium_size_firm sz_workplace_large_size_firm"



//## Oaxaca Blinder decomposition
//oaxaca ln_wage $experience $education1 $caste married_status_1 hh_child gender_1 $occupation $voctrain total_chores_hr overtime_1 $firmsize $industry, by(formal_emp_1) weight(1) swap svy

# PLot mmm decomposition

rqdeco ln_wage $experience $education1 $caste married child_12 female $occupation $voctrain $indchar $industry if year == 2008, by(formal_employment) quantiles(0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9) vce(bootstrap) reps(2) 

rqdeco ln_wage $experience $education1 $caste married child_12 female $voctrain $indchar if year == 2018, by(formal_employment) quantiles(0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9) vce(bootstrap) reps(2) 


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




