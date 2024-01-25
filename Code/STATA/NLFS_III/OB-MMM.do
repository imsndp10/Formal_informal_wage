clear
set more off
cd "/Users/sandeepsharma/Desktop/Research/Data Analysis/Formal informal wage/Formal_informal_wage/Data/Cleaned/NLFS_III"
use nlstata1.dta
gen ln_wage = ln(hourly_wage)
label define formality 0 "Informal Employment" 1 "Formal Employment"
label value formal_emp_1 formality
svyset [iw = ilo_wgt], psu(psu) strata(domain217) singleunit(centered)

// Arrange variables as $global variable 

global experience "experience exp_sqr" 
global voctrain "voc_training_1 migration_work_1"
global hhchar "hh_child " 
global indchar "overtime_1 total_chores_hr"
global education "education_Below_primary education_Primary education_Tenth_grade education_Secondary education_Bachelor education_Masters_and_above" 
global caste "caste_group_6_Janajati caste_group_6_Adhibasi caste_group_6_Madhesi caste_group_6_Dalit caste_group_6_Others"
global occupation "class_5_Plant_mach_ope class_5_Skilled_agr class_5_Clerical_service class_5_Manag_prof_tech"
global industry " job_sector_Mining_quarrying job_sector_Agriculture job_sector_Manufacturing job_sector_Market_Services job_sector_Non_Market job_sector_Arts_ent"
global firmsize "bsize_medium_size_firm bsize_large_size_firm"

label variable education "Education"
label variable caste_group_6 "Caste"
label variable muluki_grp "Muluki group"
label variable class_5 "Job class"
label variable job_sector "Job Sector"
label variable firm_type "Type of Firm" 
label variable bsize "Size of business"
label variable gender_1 "Gender"
label variable experience "Experience"
label variable exp_sqr "Experience sq"
label variable married_status_1 "Married status"
label variable urban_place_1 "Urban"
label variable hh_child "Household child"
label variable education_Illiterate "Illiterate"
label variable education_Below_primary "Below primary"
label variable education_Primary "Primary"
label variable education_Tenth_grade "Tenth grade"
label variable education_Secondary "Secondary"
label variable education_Bachelor "Bachelor"
label variable education_Masters_and_above "Masters and above"
label variable caste_group_6_Khas "Khas"
label variable caste_group_6_Janajati "Janajati"
label variable caste_group_6_Adhibasi "Adhibasi"
label variable caste_group_6_Madhesi "madhesi"
label variable caste_group_6_Dalit "Dalit"
label variable caste_group_6_Others "Others"
label variable class_5_Elem_occup "Elementary Occupation"
label variable class_5_Plant_mach_ope "Plant & Machine operators"
label variable class_5_Skilled_agr "Skilled agriculture workers"
label variable class_5_Clerical_service "Clerical service"
label variable class_5_Manag_prof_tech "Managers, professionals & technicians"
label variable job_sector_Mining_quarrying "Mining and quarrying"
label variable job_sector_Construction "Construction"
label variable job_sector_Agriculture "Agriculture"
label variable job_sector_Manufacturing "Manufacturing"
label variable job_sector_Market_Services "Market services"
label variable job_sector_Non_Market "Non-market services"
label variable job_sector_Arts_ent "Arts and entertainment"
label variable bsize_small_size_firm "Small size firm"
label variable bsize_medium_size_firm "Medium size firm"
label variable bsize_large_size_firm "Large size firm"
label variable migration_work_1 "Migrated for work"
label variable overtime_1 "Overtime"
label variable selfprod_chores "Self production chores"
label variable hhld_chores "Household chores"
label variable tot_chores "Total chores"
label variable chores_hr "chores per hour"
label variable hhld_chores_hr "Household chores per hour"
label variable total_chores_hr "Total chores per hour"
label variable voc_training_1 "Vocational training"

## Oaxaca Blinder decomposition
oaxaca ln_wage $experience $education $caste married_status_1 hh_child gender_1 $occupation $voctrain total_chores_hr overtime_1 $firmsize $industry, by(formal_emp_1) weight(1) swap svy

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




