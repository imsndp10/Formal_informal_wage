cd "E:\Formal Informal wage\Formal_informal_wage\Data\Cleaned\Pooled"

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
local quantiles 5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95

* Initialize the Excel file
local excel_file "regression_bootstrap.xlsx"
putexcel set `excel_file', replace

* Write the header row
putexcel A1 = ("Quantile") B1 = ("Year") C1 = ("Employment") D1 = ("Variable") E1 = ("Coef") F1 = ("SE") G1 = ("T") H1 = ("PValue")

local row = 2

foreach year of local years {
    foreach employment of local formal_employment {
        foreach q of local quantiles {
            * Run the RIF regression
            bootstrap, reps(1000): rifhdreg ln_wage $exp $education hh_size female married child_12 voc_train migrated_fr_job tot_chores_hrs urban overtime_40 $job_sector ///
                if year == `year' & formal_employment == `employment', rif(q(`q')) absorb(dist)

            * Extract coefficients, SE, t-stats, and p-values
            matrix coef = r(table)
            matrix list coef

            * Get the column names (variable names)
            local varnames : colnames coef
            local n_vars = colsof(coef)

            * Loop over each variable in the regression results
            forvalues i = 1/`n_vars' {
                * Get the variable name from the varnames list
                local varname : word `i' of `varnames'

                * Save the quantile, year, employment type, variable name, and the 4 results (coef, se, t, p-value)
                putexcel A`row' = ("`q'") B`row' = ("`year'") C`row' = ("`employment'") D`row' = ("`varname'") ///
                    E`row' = (coef[1, `i']) F`row' = (coef[2, `i']) G`row' = (coef[3, `i']) H`row' = (coef[4, `i'])

                * Move to the next row for the next variable
                local row = `row' + 1
            }
        }
    }
}

display "Results have been saved in `excel_file'"
