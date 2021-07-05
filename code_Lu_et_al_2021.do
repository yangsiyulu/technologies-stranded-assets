/* 

Can plant conversions and abatement technologies prevent asset stranding in the power sector?
Yangsiyu Lu, Francois Cohen, Stephen Smith, Alexander Pfeiffer  
Last updated: 2021-07-05

Data input:
1) Plant-level data:
Power plant data are compiled from:
	(a) Global Coal Plant Tracker, publicly available at: https://endcoal.org/global-coal-plant-tracker/ ; 
	(b) World Electric Power Plants (WEPP) database, purchased from S&P Global Market Intelligence; 
	(c) WRI’s global database of power plants, publicly available at: https://datasets.wri.org/dataset/globalpowerplantdatabase.  

As part of power plant data (WEPP) are license protected, we are not able to share the raw data on power plants.

2) Climate Scenarios:
	a) AMPERE, publicly available at: https://tntcat.iiasa.ac.at/AMPEREDB/dsd?Action=htmlpage&page=about
	b) IPCC SR1.5 scenarios data, publicly available at: https://data.ene.iiasa.ac.at/iamc-1.5c-explorer/
	c) IEA Sustainable Development Scenarios regarding CCS, publicly available at: https://www.iea.org/reports/ccus-in-clean-energy-transitions/ccus-in-the-transition-to-net-zero-emissions
		
	
The purpose of this file is to facilitate the reproducibility of this research. Scripts show the main steps in the analysis.
*/

********************************************************************************
*************    Estimate electricity generation between 2021 and 2100    ******************
********************************************************************************
	use "POWERPLANT.dta", clear //this do file starts from POWERPLANT.dta, which already dealt with power plant raw dataset.
				
	gen max_gen = mw * 24 * 365 
	label var max_gen "Max annual generation [mWh/year]"

* Estimate annual generation
	forvalues i = 2021 (1)2100 {
			
				
		gen gen_`i'= 0
		label var gen_`i' "generation in Year `i' [mWh]" 
		
		*utilization is the utilization rate, year_adj is the online year, retire_adj is the retire year	
		replace gen_`i' = 0.5 * max_gen  * utilization if `i' == year_adj | `i' == retire_adj  //assume generators operate for 6 months in online and retire years 
		replace gen_`i'= max_gen * utilization if `i' > year_adj & `i' < retire_adj 
		}
		
	quietly describe gen_2021 - gen_2100, varlist
	global vars `r(varlist)'
	collapse (sum) $vars, by (fuel_class region category) //fuel_class indicates the fuel of power plant, category indicates whether the plant is operating or in the pipeline

* Reshape data to wide format of fuel
	reshape wide $vars, i(region category) j(fuel_class) string
	
* Reshape data to long format of year
	reshape long COAL_gen GAS_gen OIL_gen  BIO_gen, i(region category) j(year) 

* Combine operating (current) and pipeline electricity generation to the same obs
	quietly describe  COAL_gen GAS_gen OIL_gen BIO_gen, varlist
	global vars2 `r(varlist)'
	reshape wide $vars2, i(region year) j(category) string

	global FFs " "COAL" "GAS" "OIL" "BIO" "

	foreach x in $FFs {
		gen `x'_gen=`x'_gen_current+`x'_gen_pipeline
	}
	}	
	sort region year
	save "plant_database_ready.dta",replace


********************************************************************************
*************    Estimate stranded assets in climate scenarios ******************
********************************************************************************

*---- SA0: not consider plant conversions, used for analysis in Figure 2 and Figure 4 

* Combine climate scenarios and power plant dataset
	use "$use/AMPERE.dta", clear //AMPERE.dta is the dta file contains climate scenarios
	sort region year
	merge m:1 region year using "$use/plant_database_ready.dta", keepusing(COAL* GAS* OIL*) 
	
	
* Set COAL GAS OIL for the loop
	gen COAL=.
	gen GAS=.
	gen OIL=.
	describe COAL-OIL, varlist
	global FFs `r(varlist)'
	
* Generate stranded assets on a yearly basis
	foreach x of varlist $FFs {

	gen `x'_SA0_current=`x'_gen_current -`x'_AMPEREsec_woCCS if `x'_gen_current-`x'_AMPEREsec_woCCS>=0
	replace `x'_SA0_current=0 if `x'_gen_current-`x'_AMPEREsec_woCCS <0 

	gen `x'_SA0_pipeline=`x'_gen_pipeline if  `x'_gen_current-`x'_AMPEREsec_woCCS>=0
	replace `x'_SA0_pipeline=`x'_gen_pipeline-(`x'_AMPEREsec_woCCS-`x'_gen_current) if `x'_gen_pipeline-(`x'_AMPEREsec_woCCS-`x'_gen_current)>0 & (`x'_gen_current-`x'_AMPEREsec_woCCS)<0
	replace `x'_SA0_pipeline=0 if `x'_gen_pipeline-(`x'_AMPEREsec_woCCS-`x'_gen_current)<=0 & (`x'_gen_current-`x'_AMPEREsec_woCCS)<0

	gen `x'_SA0'=`x'_SA0_current+`x'_SA0_pipeline
	}

** Sum up fossil fuel stranded assets annually 
	gen ALL_SA0=COAL_SA0+GAS_SA0+OIL_SA0
	gen ALL_SA0_current=COAL_SA0_current+GAS_SA0_current+OIL_SA0_current
	gen ALL_SA0_pipeline=COAL_SA0_pipeline+GAS_SA0_pipeline+OIL_SA0_pipeline
	}

** Generate the (cumulative) sum of stranded assets from 2021-2100 
	gen pathway_id=model+"_"+scenario+"_"+region //generate unique id for each obs	
	sort pathway_id


	bysort pathway_id: egen sum_COAL_SA0_current=sum(COAL_SA0_current) 
	bysort pathway_id: egen sum_GAS_SA0_current=sum(GAS_SA0_current)
	bysort pathway_id: egen sum_OIL_SA0_current=sum(OIL_SA0_current) 
	bysort pathway_id: egen sum_ALL_SA0_current=sum(ALL_SA0=_current) 

	bysort pathway_id: egen sum_COAL_SA0_pipeline=sum(COAL_SA0_pipeline) 
	bysort pathway_id: egen sum_GAS_SA0_pipeline=sum(GAS_SA0_pipeline)
	bysort pathway_id: egen sum_OIL_SA0_pipeline=sum(OIL_SA0_pipeline) 
	bysort pathway_id: egen sum_ALL_SA0_pipeline=sum(ALL_SA0_pipeline) 

	bysort pathway_id: egen sum_COAL_SA0=sum(COAL_SA) 
	bysort pathway_id: egen sum_GAS_SA0=sum(GAS_SA0)
	bysort pathway_id: egen sum_OIL_SA0=sum(OIL_SA0) 
	bysort pathway_id: egen sum_ALL_SA0=sum(ALL_SA0) 

	gen ALL_gen=COAL_gen+GAS_gen+OIL_gen
	gen ALL_gen_current=COAL_gen_current+GAS_gen_current+OIL_gen_current
	gen ALL_gen_pipeline=COAL_gen_pipeline+GAS_gen_pipeline+OIL_gen_pipeline

	bysort pathway_id: egen sum_COAL_gen=sum(COAL_gen) 
	bysort pathway_id: egen sum_GAS_gen=sum(GAS_gen)
	bysort pathway_id: egen sum_OIL_gen=sum(OIL_gen) 
	bysort pathway_id: egen sum_ALL_gen=sum(ALL_gen) 

	bysort pathway_id: egen sum_ALL_gen_current=sum(ALL_gen_current) 
	bysort pathway_id: egen sum_GAS_gen_current=sum(GAS_gen_current)
	bysort pathway_id: egen sum_OIL_gen_current=sum(OIL_gen_current) 
	bysort pathway_id: egen sum_COAL_gen_current=sum(COAL_gen_current) 

	bysort pathway_id: egen sum_ALL_gen_pipeline=sum(ALL_gen_pipeline) 
	bysort pathway_id: egen sum_GAS_gen_pipeline=sum(GAS_gen_pipeline)
	bysort pathway_id: egen sum_OIL_gen_pipeline=sum(OIL_gen_pipeline) 
	bysort pathway_id: egen sum_COAL_gen_pipeline=sum(COAL_gen_pipeline) 

	collapse (mean) sum_*_SA* sum_*_gen* , by (model region scenario) 
	
	save "Stranded_assets_ready.dta", replace

*************    Consider plant conversions  ******************

	*---- SA1: Coal-to-gas (used in Fig. 3a)
		
	use "$use/AMPERE.dta", clear
	set more off
	sort region year
	merge m:1 region year using "$use/plant_database_gas_retrofit.dta", keepusing(COAL* GAS* OIL*) gen(matched) // "$use/plant_database_gas_retrofit.dta" is the power plant data prepared for gas retrofit, classified power generation into gas-suitable and non-suitable

* OIL: calculate the same way as SA0
	gen OIL_SA1 =(OIL_gen  - OIL_AMPEREsec_woCCS) if OIL_gen >=OIL_AMPEREsec_woCCS
	replace OIL_SA1 =0 if OIL_gen <OIL_AMPEREsec_woCCS

* COAL and GAS: 
	gen GAS_SA1 =(GAS_gen -GAS_AMPEREsec_woCCS) if GAS_gen  >= GAS_AMPEREsec_woCCS
	replace  GAS_SA1 =0 if GAS_gen  < GAS_AMPEREsec_woCCS

* Calculate how much extra from gas can be used for coal -- if there's no gas stranding
	gen GAS_AMPEREsec_extra =GAS_AMPEREsec_woCCS-GAS_gen  if GAS_gen  < GAS_AMPEREsec_woCCS
	replace GAS_AMPEREsec_extra =0 if GAS_gen  >= GAS_AMPEREsec_woCCS

	local retrofit="0 5 20" // retrofit rate
	foreach re of local retrofit{
* Get the amount that could be converted
	gen COAL_conversion_`re'=COAL_gen1*`re'/100 if GAS_AMPEREsec_extra >COAL_gen1*`re'/100   //	suffix "gen1" stands for retrofit suitable, "gen0" stands for retrofit non-suitable
	replace COAL_conversion_`re'=GAS_AMPEREsec_extra  if GAS_AMPEREsec_extra <=COAL_gen1*`re'/100

	gen COAL_SA1 _`re'=COAL_gen -COAL_conversion_`re'-COAL_AMPEREsec_woCCS if COAL_gen -COAL_conversion_`re'-COAL_AMPEREsec_woCCS>0
	replace COAL_SA1 _`re'=0 if COAL_gen -COAL_conversion_`re'-COAL_AMPEREsec_woCCS<=0

* Generate all fossil fuel stranded assets annually 
	gen ALL_SA1_`re'=COAL_SA1_`re'+GAS_SA1+OIL_SA1 
* Sum up stranded assets from all years
	bysort pathway_id: egen sum_ALL_SA1_`re'=sum(ALL_SA1_`re') 
	}
	save  "Coal-to-gas.dta",replace

	*---- SA2: CCS without biomass (used in Fig. 3b)
	
	use "$use/AMPERE.dta", clear
	set more off
	sort region year
	merge m:1 region year using "$use/plant_database_CCS_retrofit.dta", keepusing(COAL* GAS* OIL*) gen(matched) //"$use/plant_database_CCS_retrofit.dta" is the power plant data prepared for CCS retrofit, classified power generation into CCS-suitable and non-suitable

	local retrofit="0 50 100 " // retrofit rate	
	local FFs="COAL GAS OIL " // retrofit rate	

	foreach re of local retrofit{
	foreach x of local FFs {
		*get the conversion amount: should be smaller than convert suitable plants
	gen `x'_conversion_`re'=`x'_gen1*`re'/100 if `x'_AMPEREsec_wCCS>=`x'_gen1*`re'/100  //	suffix "gen1" stands for retrofit suitable, "gen0" stands for retrofit non-suitable
	replace  `x'_conversion_`re'=`x'_AMPEREsec_wCC if `x'_AMPEREsec_wCCS<`x'_gen1*`re'/100 
	gen `x'_SA2_`re'=(`x'_gen1+ `x'_gen0- `x'_conversion_`re'-`x'_AMPEREsec_woCCS) if (`x'_gen1+ `x'_gen0- `x'_conversion_`re'-`x'_AMPEREsec_woCCS)>0
	replace `x'_SA2_`re'=0 if (`x'_gen1+ `x'_gen0- `x'_conversion_`re'-`x'_AMPEREsec_woCCS)<=0
	}
	** Generate all fossil fuel stranded assets annually 
	gen ALL_SA1_`re'=COAL_SA1_`re'+GAS_SA1_`re'+OIL_SA1_`re'
	** Sum up all fossil fuel stranded assets from all years
	bysort pathway_id: egen sum_ALL_SA1 _`re'=sum(ALL_SA1_`re')
	}

	save "CCS_without_biomass.dta",replace

	*---- SA3: CCS with biomass cofiring, used in Fig.3c & 3d
	
	use "$use/AMPERE_FF.dta", clear
	set more off
	sort region year
	merge m:1 region year using "$use/plant_database_CCS_retrofit.dta", keepusing(COAL* GAS* OIL* BIO*) gen(matched) //

* GAS and OIL: assume same percent of CCS retrofit
	global retrofit" "0" "50" "100" "
	local FFs="GAS OIL " // retrofit rate
	foreach re in $retrofit	{
	foreach x of local FFs {	
* Get the conversion amount: should be smaller than convert suitable plants
	gen `x'_conversion_`re'=`x'_gen 1*`re'/100 if `x'_AMPEREsec_wCCS>=`x'_gen 1*`re'/100 
	replace  `x'_conversion_`re'=`x'_AMPEREsec_wCC if `x'_AMPEREsec_wCCS<`x'_gen 1*`re'/100 
	gen `x'_SA3 _`re'=(`x'_gen 1+ `x'_gen 0- `x'_conversion_`re'-`x'_AMPEREsec_woCCS) if (`x'_gen 1+ `x'_gen 0- `x'_conversion_`re'-`x'_AMPEREsec_woCCS)>0
	replace `x'_SA3 _`re'=0 if (`x'_gen 1+ `x'_gen 0- `x'_conversion_`re'-`x'_AMPEREsec_woCCS)<=0
	}
	}
* COAL:
* Calculate how much Biomass without CCS can be allowed
	gen BIO_AMPEREsec_extra _woCCS=BIO_AMPEREsec_woCCS-BIO_gen  if BIO_AMPEREsec_woCCS-BIO_gen  >0
	replace BIO_AMPEREsec_extra =0 if BIO_AMPEREsec_woCCS-BIO_gen <=0

	global retrofit" "0" "50" "100" "
	global cofiring "0 20 50 " // co-firing rate

	gen x2=BIO_AMPEREsec_wCCS+COAL_AMPEREsec_wCCS
		
	foreach re in $retrofit	{
	foreach co in $cofiring	{
	gen x1_`re'_`co'=COAL_gen1*(`re'/100) 
	gen x3_`re'_`co'=COAL_AMPEREsec_wCCS/(100-`co')*100
	egen COAL_converted_wCCS`re'_`co'=rmin(x1_`re'_`co' x2 x3_`re'_`co')
* How much coal is not converted?
	gen COAL_nonconverted_`re'_`co'=COAL_gen -COAL_converted_wCCS`re'_`co'
* How much biomass could co-fire in coal non CCS plants?
	gen BIOMASS_cofire_nc_`re'_`co'=COAL_nonconverted_`re'_`co'*(`co'/100) if COAL_nonconverted_`re'_`co'*(`co'/100)<=BIO_AMPEREsec_extra 
	replace BIOMASS_cofire_nc_`re'_`co'=BIO_AMPEREsec_extra  if COAL_nonconverted_`re'_`co'*(`co'/100)>BIO_AMPEREsec_extra  
* How much is actual biomass co-fring ratio?
	gen BIOMASS_cofire_wCCS`re'_`co'=COAL_converted_wCCS`re'_`co'-COAL_conversion_`re'
	gen cofire_ratio_wCCS`re'_`co'=BIOMASS_cofire_wCCS`re'_`co'/ COAL_converted_wCCS`re'_`co'
	gen cofire_ratio_woCCS`re'_`co'=BIOMASS_cofire_nc_`re'_`co'/COAL_nonconverted_`re'_`co'

* Coal stranded assets
	gen COAL_SA3 _`re'_`co'=COAL_gen -COAL_converted_wCCS`re'_`co'-BIOMASS_cofire_nc_`re'_`co'-COAL_AMPEREsec_woCCS if COAL_gen -COAL_converted_wCCS`re'_`co'-BIOMASS_cofire_nc_`re'_`co'-COAL_AMPEREsec_woCCS >0
	replace COAL_SA3 _`re'_`co'=0 if COAL_gen -COAL_converted_wCCS`re'_`co'-BIOMASS_cofire_nc_`re'_`co'-COAL_AMPEREsec_woCCS <=0

** Generate all fossil fuel stranded assets annually 
	gen ALL_SA3 _`re'_`co'=COAL_SA2 _`re'_`co'+GAS_SA2_`re'+OIL_SA2_`re'

* Sum up stranded assets from all years
	bysort pathway_id: egen sum_ALL_SA2_`re'_`co'=sum(ALL_SA2_`re'_`co') 
	}
	}
	save  "CCS_with_biomass.dta",replace




