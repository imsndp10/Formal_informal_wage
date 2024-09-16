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

* Step 1: Create a new variable to categorize based on job_sector
egen sector_category = group(class_5), label

* Step 2: Recode sector_category to create nonservice (1, 2, 3) and service (4, 5, 6) categories
recode sector_category (1=1 "elem") (2=1 "elem") (3=1 "elem") (4=2 "cler") (5=3 "managers"), generate(occup)

* Define global macros
global exp "experience experience_sq"
global education "education_Below_primary education_Primary education_Tenth_grade education_Secondary education_Bachelor education_Masters_above" 
global caste "caste_group_6_Janajati caste_group_6_Adhibasi caste_group_6_Madhesi caste_group_6_Dalit caste_group_6_Others"
global job_sector "job_sector_Mining_utility job_sector_Construction job_sector_Manufacturing job_sector_Market_services job_sector_Non_Market_services"


* Define local macros for your analysis
local formal_employment 0 1 
local years 2008 2018
local quantiles 10 25 50 75 90

* Initialize the Excel file
local excel_file "rif_test.xlsx"
putexcel set `excel_file', replace

* Prepare headers for the sheets
local headers "Quantile Year Employment Variable Coef SE T PValue LL UL"
putexcel set `excel_file', sheet("overall") modify
putexcel A1 = ("Quantile") B1 = ("Year") C1 = ("Employment") D1 = ("Variable") E1 = ("Coef") F1 = ("SE") G1 = ("T") H1 = ("PValue") I1 = ("LL") J1 = ("UL")
putexcel set `excel_file', sheet("p_explained") modify
putexcel A1 = ("Quantile") B1 = ("Year") C1 = ("Employment") D1 = ("Variable") E1 = ("Coef") F1 = ("SE") G1 = ("T") H1 = ("PValue") I1 = ("LL") J1 = ("UL")
putexcel set `excel_file', sheet("specif_err") modify
putexcel A1 = ("Quantile") B1 = ("Year") C1 = ("Employment") D1 = ("Variable") E1 = ("Coef") F1 = ("SE") G1 = ("T") H1 = ("PValue") I1 = ("LL") J1 = ("UL")
putexcel set `excel_file', sheet("p_unexplained") modify
putexcel A1 = ("Quantile") B1 = ("Year") C1 = ("Employment") D1 = ("Variable") E1 = ("Coef") F1 = ("SE") G1 = ("T") H1 = ("PValue") I1 = ("LL") J1 = ("UL")
putexcel set `excel_file', sheet("rwg_error") modify
putexcel A1 = ("Quantile") B1 = ("Year") C1 = ("Employment") D1 = ("Variable") E1 = ("Coef") F1 = ("SE") G1 = ("T") H1 = ("PValue") I1 = ("LL") J1 = ("UL")

* Initialize row counters for each sheet
local row_overall 2
local row_p_explained 2
local row_specif_err 2
local row_p_unexplained 2
local row_rwg_error 2

foreach year of local years {
    foreach employment of local formal_employment {
        foreach q of local quantiles {
            * Run the Oaxaca_RIF
            bootstrap, reps(1000): oaxaca_rif ln_wage $exp $education $caste hh_size female ///
        married child_12 voc_train migrated_fr_job tot_chores_hrs urban overtime_40 ///
        if year == `year' & occup == 1, by(formal_employment) rif(q(`q')) rwlogit(HH_formal dep_ratio $exp $education $caste) swap relax

            * Extract coefficients, SE, t-stats, p-values, lower and upper limits
            matrix coef = r(table)
            matrix list coef

            * Get the column names (variable names)
            local varnames : colnames coef
            local n_vars = colsof(coef)

            * Appending results to "overall" sheet
            putexcel set `excel_file', sheet("overall") modify
            forvalues i = 1/6 {
                local varname : word `i' of `varnames'
                putexcel A`row_overall' = ("`q'") B`row_overall' = ("`year'") C`row_overall' = ("`employment'") D`row_overall' = ("`varname'") ///
                    E`row_overall' = (coef[1, `i']) F`row_overall' = (coef[2, `i']) G`row_overall' = (coef[3, `i']) H`row_overall' = (coef[4, `i']) ///
                    I`row_overall' = (coef[5, `i']) J`row_overall' = (coef[6, `i'])
                local row_overall = `row_overall' + 1
            }
            local additional_indices 8 9 56 57
            foreach i of local additional_indices {
                if `i' <= `n_vars' {
                    local varname : word `i' of `varnames'
                    putexcel A`row_overall' = ("`q'") B`row_overall' = ("`year'") C`row_overall' = ("`employment'") D`row_overall' = ("`varname'") ///
                        E`row_overall' = (coef[1, `i']) F`row_overall' = (coef[2, `i']) G`row_overall' = (coef[3, `i']) H`row_overall' = (coef[4, `i']) ///
                        I`row_overall' = (coef[5, `i']) J`row_overall' = (coef[6, `i'])
                    local row_overall = `row_overall' + 1
                }
            }
            * Appending results to "p_explained" sheet
            putexcel set `excel_file', sheet("p_explained") modify
            forvalues i = 10/31 {
                local varname : word `i' of `varnames'
                putexcel A`row_p_explained' = ("`q'") B`row_p_explained' = ("`year'") C`row_p_explained' = ("`employment'") D`row_p_explained' = ("`varname'") ///
                    E`row_p_explained' = (coef[1, `i']) F`row_p_explained' = (coef[2, `i']) G`row_p_explained' = (coef[3, `i']) H`row_p_explained' = (coef[4, `i']) ///
                    I`row_p_explained' = (coef[5, `i']) J`row_p_explained' = (coef[6, `i'])
                local row_p_explained = `row_p_explained' + 1
            }

            * Appending results to "specif_err" sheet
            putexcel set `excel_file', sheet("specif_err") modify
            forvalues i = 32/54 {
                local varname : word `i' of `varnames'
                putexcel A`row_specif_err' = ("`q'") B`row_specif_err' = ("`year'") C`row_specif_err' = ("`employment'") D`row_specif_err' = ("`varname'") ///
                    E`row_specif_err' = (coef[1, `i']) F`row_specif_err' = (coef[2, `i']) G`row_specif_err' = (coef[3, `i']) H`row_specif_err' = (coef[4, `i']) ///
                    I`row_specif_err' = (coef[5, `i']) J`row_specif_err' = (coef[6, `i'])
                local row_specif_err = `row_specif_err' + 1
            }

            * Appending results to "p_unexplained" sheet
            putexcel set `excel_file', sheet("p_unexplained") modify
            forvalues i = 58/80 {
                local varname : word `i' of `varnames'
                putexcel A`row_p_unexplained' = ("`q'") B`row_p_unexplained' = ("`year'") C`row_p_unexplained' = ("`employment'") D`row_p_unexplained' = ("`varname'") ///
                    E`row_p_unexplained' = (coef[1, `i']) F`row_p_unexplained' = (coef[2, `i']) G`row_p_unexplained' = (coef[3, `i']) H`row_p_unexplained' = (coef[4, `i']) ///
                    I`row_p_unexplained' = (coef[5, `i']) J`row_p_unexplained' = (coef[6, `i'])
                local row_p_unexplained = `row_p_unexplained' + 1
            }

            * Appending results to "rwg_error" sheet
            putexcel set `excel_file', sheet("rwg_error") modify
            forvalues i = 81/102 {
                local varname : word `i' of `varnames'
                putexcel A`row_rwg_error' = ("`q'") B`row_rwg_error' = ("`year'") C`row_rwg_error' = ("`employment'") D`row_rwg_error' = ("`varname'") ///
                    E`row_rwg_error' = (coef[1, `i']) F`row_rwg_error' = (coef[2, `i']) G`row_rwg_error' = (coef[3, `i']) H`row_rwg_error' = (coef[4, `i']) ///
                    I`row_rwg_error' = (coef[5, `i']) J`row_rwg_error' = (coef[6, `i'])
                local row_rwg_error = `row_rwg_error' + 1
            }
        }
    }
}

display "Results have been saved in `excel_file'"
