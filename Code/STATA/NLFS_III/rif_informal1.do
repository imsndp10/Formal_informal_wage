* Clear any existing data and load the new dataset
clear
set more off
use Pooled.dta, clear

* Generate the log of hourly wage
gen ln_wage = ln(hourly_wage)

* Define the label for formal employment
label define formal_employment 0 "Informal Employment" 1 "Formal Employment"

* Sort the data by psu, hhid, and year
sort psu hhid year

* Create a total count of formal employment within household and year
by psu hhid year: egen total_formal = total(formal_employment)
gen HH_formal = total_formal - formal_employment

// Step 1: Create a new variable to categorize based on job_sector
egen sector_category = group(class_5), label

// Step 2: Recode sector_category to create nonservice (1, 2, 3) and service (4, 5, 6) categories
recode sector_category (1=1 "elem") (2=1 "elem") (3=1 "elem") (4=2 "cler") (5=3 "managers"), generate(occup)

* Define global macros
global exp "experience experience_sq"
global education "education_Below_primary education_Primary education_Tenth_grade education_Secondary education_Bachelor education_Masters_above" 
global caste "caste_group_6_Janajati caste_group_6_Adhibasi caste_group_6_Madhesi caste_group_6_Dalit caste_group_6_Others"

* Initialize an empty local macro
local quantiles ""

* Loop through the specific quantiles and add them to the local macro
forvalues i = 1(1)99 {
    local quantiles "`quantiles' `i'"
}

* Display the local macro to verify (optional)
di "`quantiles'"

* Initialize Excel sheet
putexcel set elem_male_18.xlsx, sheet(xyz) replace

* Set the column titles
putexcel A1 = ("qnt") B1 = ("O") C1 = ("group") D1 = ("coef") E1 = ("se") F1 = ("h") G1 = ("i") H1 = ("lb") I1 = ("ub")

* Loop through quantiles and perform Oaxaca-RIF decomposition
local row = 2
foreach q of local quantiles {
    * Perform Oaxaca-RIF decomposition and save results using putexcel
    oaxaca_rif ln_wage $exp $education $caste hh_size female ///
    married child_12 voc_train migrated_fr_job tot_chores_hrs urban overtime_40 ///
    if year == 2018 & female == 0 & occup == 1, by(formal_employment) wgt(1) rif(q(`q')) rwlogit(HH_formal dep_ratio $exp $education $caste) swap relax
    
    * Save the results to the Excel sheet
    matrix results = r(table)
    matrix nresult = results[1..6,1..6]
    
    * Save the quantile label for 7 rows
    forvalues i = 0/6 {
        putexcel A`=`row'+`i'' = ("`q'")
    }
    
    * Save the matrix results starting from the next row
    local next_row = `row' + 1
    putexcel B`row' = matrix(nresult', rownames)
    
    * Update row counter for the next quantile block
    local row = `row' + rowsof(nresult)
}
