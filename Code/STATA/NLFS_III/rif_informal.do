clear
set more off
cd "/Users/sandeepsharma/Desktop/Research/Data Analysis/Formal informal wage/Formal_informal_wage/Data/Cleaned/Pooled"
//cd "D:/Work/Sandeep_research/Data/Cleaned/Pooled"
use Pooled.dta
gen ln_wage = ln(hourly_wage)
label define formal_employment 0 "Informal Employment" 1 "Formal Employment"

sort psu hhid year
by psu hhid year: egen total_formal = total(formal_employment)
gen HH_formal = total_formal - formal_employment


global exp "experience experience_sq"
global education "education_Below_primary education_Primary education_Tenth_grade education_Secondary education_Bachelor education_Masters_above" 
global caste "caste_group_6_Janajati caste_group_6_Adhibasi caste_group_6_Madhesi caste_group_6_Dalit caste_group_6_Others"

local quantiles = "5 95"

//rif(q(`q'))
//foreach q of local quantiles {
//asdoc oaxaca_rif ln_wage $exp $education $caste hh_size female ///
//married child_12 voc_train migrated_fr_job tot_chores_hrs urban overtime_40 ///
//$class_5 if year == 2008 , by(formal_employment) wgt(1) rif(q(`q')) rwlogit(HH_formal dep_ratio $exp $education $caste) swap save(asdoc.xlsx)
//}

//esttab using "results_quantile_15.csv", replace se

* Initialize an empty local macro
local quantiles ""

* Loop through even numbers from 2 to 100 and add them to the local macro
forvalues i = 2(2)100 {
    local quantiles "`quantiles' `i'"
}

* Display the local macro to verify (optional)
di "`quantiles'"

* Loop through quantiles and perform Oaxaca-RIF decomposition
foreach q of local quantiles {
    * Perform Oaxaca-RIF decomposition and save results using asdoc
    asdoc oaxaca_rif ln_wage $exp $education $caste hh_size female ///
    married child_12 voc_train migrated_fr_job tot_chores_hrs urban overtime_40 ///
    $class_5 if year == 2008 , by(formal_employment) wgt(1) rif(q(`q')) rwlogit(HH_formal dep_ratio $exp $education $caste) swap ///
    save(asdoc.doc)
}

//Loop save trial

foreach q of local quantiles {
    * Perform Oaxaca-RIF decomposition and save results using asdoc
oaxaca_rif ln_wage $exp $education $caste hh_size female ///
married child_12 voc_train migrated_fr_job tot_chores_hrs urban overtime_40 ///
$class_5 if year == 2008, by(formal_employment) wgt(1) rif(q(`q')) rwlogit(HH_formal dep_ratio $exp $education $caste) swap
putexcel set test2.xlsx, sheet(xyz) modify
	putexcel A1 = matrix(r(table)')
	}
