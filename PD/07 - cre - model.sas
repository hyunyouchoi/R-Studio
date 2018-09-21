/* clear the log window*/
dm 'log' clear; 
LIBNAME Interim "";
LIBNAME Out "";
LIBNAME BBCN "";

/*ODS RTF FILE="C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\cre_profiles_v2.RTF";*/
ods graphics on;

data out.df_cre_dev;
	set  out.df_boh_final_base (where=(filedate<= '31Mar16'd and portfolio_id="CRE"));
run;


data interim.df_cre_model_vars (keep=y account_id filedate current_balance

loan_spread_0 loan_spread_2
POB POB_5 POB_95 pob_50
DPD3059_0 dpd0129_0 dpd0129_25 dpd3059_5
pfi_nonres_structures

bbb_yield_lag_2_1 bbb_yield_lag_2_2
bbb_yield_lag_1_1 bbb_yield_lag_1_2
tb5yr_1 tb5yr_2
tb5y tb10y

cahpi_ag cahpi_qg cci
djia_1 djia

RGDP_qG
ur ur_9 ur_9P
CAUR CAUR_YD caur_yd_1
caur_yd_lag_3 caur_yd_lag_3_1
hpi hpi_qg hpi_ag 
Cahpi_qg CAHPI_ag
	 
cppi cppi_ag cppi_qg
rdi_ag int_rate interest_rate
ur ngdpg_ag_lag_4 loan_spread_v
year_file q_file
NCREIF_Property_index
NAICS PROPERTY_DESCR2
boh_rating BOH_RATING2
rdi_qg
caur_yd
caur_yd_3
season
caur_yd_lag_3
dija 
dija_qg_3
dija_qg
cahpi_ag_6

property_type
boh_rating1
naics_code
naics_RE
property_type2
property_type
prop_retail
prop_res

ngdp_qg_1
ngdp_qg
				bbb_yield_lag_1_6
				RGDP_qG_lag_2_neg

);
	
	set out.df_cre_dev (where=(filedate>'30Sep07'd)); 

 	bbb_yield_lag_2_1 = (bbb_yield<=4.4);
	bbb_yield_lag_2_2 = max(bbb_yield, 4.4); 
	bbb_yield_lag_1_1 = (bbb_yield_lag_1<=4.3);
	bbb_yield_lag_1_2 = max(BBB_yield_lag_1, 4.3); 

	bbb_yield_lag_1_6 = max(BBB_yield_lag_1, 6); 
	RGDP_qG_lag_2_neg= min(RGDP_qG_lag_2, 0);
	/*
	cppi_2= min(cppi, 226); 
	cppi_3= max(min(cppi, 290),229);
	
	djia_1= min(djia, 9500);
	djia_2= max(min(djia, 14300), 10800);
	djia_3= max(min(djia, 15100), 14354); 
	djia_4= max(min(djia, 16800), 15100); 
	djia_5=(djia>17000);
	*/

	tb5yr_1=min(tb5y, 2.25);
	tb5yr_2 = (tb5y>2.25); 

	djia_1=max(djia, 10250);
	dija_qg_3=min(dija_qg,3); 
	cahpi_ag_6=min(cahpi_ag,6);
	
	/*
	loan_int_1= (loan_int<=7);
	loan_int_2 = ((loan_int>7 and loan_int<=9));
	loan_int_3 = (loan_int>9);
	*/

	loan_spread_0= (loan_spread_v<=2);
	loan_spread_2 = max(loan_spread_v,2);
	if loan_spread_v<0 then delete;
	
	ur_9= MIN(ur, 9);
	ur_9P = (ur>9);
	
	POB_5= min(POB,5);
	POB_95=max(POB,95);
	pob_50=max(pob, 50);
		
	dpd3059_0=(dpd3059=0);
	dpd0129_0= (dpd0129=0);
	dpd0129_25= MIN(DPD0129, 25);
	dpd3059_5= min(dpd3059, 5);
	
	hpi_1 = max(hpi, 170);

	vix_25= (vix<=25);
	vix_40= (vix>=40);
	vix_2540= min(max(vix, 25),40);
	
	rdi_ag_lag_3_1=min(0,rdi_ag_lag_3); 
	rdi_ag_lag_3_2=max(0, rdi_ag_lag_3);

	CAUR_YD_3=MIN(CAUR_YD, 3);
	caur_yd_lag_3_1 = Max(caur_yd_lag_3,-1); 


	SPR10_QD_1=MIN(SPR10, 0);
	SPR10_QD_2=MAX(SPR10, 0);

	original_balance_1=(original_balance<=600000);

	int_rate=100*interest_rate; 
	
	year_file=year(filedate); 
	q_file=qtr(filedate); 

	IF BOH_RATING IN (0,1,2,3) THEN BOH_RATING2= 'R1'; 
	ELSE IF BOH_RATING IN (4,1000) THEN BOH_RATING2= 'R2';
	ELSE IF BOH_RATING IN (2000) THEN BOH_RATING2= 'R3';
	ELSE IF BOH_RATING IN (3000, 4000) THEN BOH_RATING2='R4';  

	if q_file= 1 then season= 'sp';
	else if q_file=2 then season='su';
	else if q_file =3 then season='fa';
	else if q_file =4 then season = 'wi';

	if naics in ('Accomodation & Food', 'Retail')  then naics_code= 'a';
	else if naics = 'Real Estate & Rental' then naics_code='b';
	else if naics in ('Arts & Entertainment', 'Construction',
	'Health Care', 'Information', 'Manufacturing', 'Other', 'Science & Technology',
	'Transportation', 'Waste Management', 'Wholesale Trade') then naics_code = 'c'; 
	/*else if naics = 'Wholesale Trade' then naics_code='c';*/
	/*else if naics = 'Retail' then naics_code='d';*/
	if naics='error' then delete; 
	

	if naics = 'Real Estate & Rental' then naics_RE=1; 

	if boh_rating in (0,1,2,3) then boh_rating1='R1';
	else if boh_rating in (4, 1000) then boh_rating1='R2';
	else if boh_rating in (2000,3000) then boh_rating1 = 'R3';


	prop_res=(property_descr2 in ('Multifamily Residential', '1-4 Residential'));
	prop_retail=(property_descr2 in ('Retail shopping center'));
	

	if property_descr2 in ('Multifamily Residential', '1-4 Residential', 'Retail shopping center') then property_type='a';
	/*else if property_descr2 in ('Retail shopping center') then property_type='b';*/
	else if property_descr2 in ('Land') then property_type='c';
	else if property_descr2 in ('Carwash') then property_type='d';
	else if property_descr2 in ('Restaurant') then property_type='e';
	else if property_descr2 in ('Hotel/Motel') then property_type='f';
	else if property_descr2 in ('Gas station') then property_type='g';
	else if property_descr2 in ('Commercial-industrial') then property_type='h';
	else if property_descr2 in ('Other: single tenant', 'Mixed-use: comm/res', 'Office', 
								'Church/Religious', 'Auto repair shop', 'Medical', 
								'Golf course') then property_type='i';
	else if property_descr2 in ('Other') then property_type='j';

	if property_descr2 in ('Multifamily Residential', '1-4 Residential') then property_type2='a';
	else if property_descr2 in ('Retail shopping center', 'Office','Mixed-use: comm/res', 'Church/Religious', 'Auto repair shop', 'Golf Course', 'Medical') then property_type2='b';
	else if property_descr2 in ('Commercial-industrial', 'Gas station', 'Hotel/Motel', 'Other: single tenant') then property_type2='c';
	else if property_descr2 in ('Church/Religious', 'Auto repair shop', 'Medical', 
								'Golf course') then property_type2='i';
	else if property_descr2 in ('Carwash', 'Land', 'Other') then property_type2='j';

	if property_descr in ('missing', 'None') then delete;
	ngdp_qg_1= max(ngdp_qg, -1);


run; 


/*NAICS CODE*/ 
Proc means data=out.df_cre_dev nway noprint order=formatted ;
	var y;
	Class NAICS ;
	output out=summary_stats mean(y)=default_rate;
run;
proc sort data=summary_stats out=summary_stats; 
	by  NAICS;
run; 
Proc sgplot data=summary_stats;
	SERIES x=NAICS y=default_rate;
	band x=NAICS lower=0 upper=_FREQ_ / fillattrs=ltgray y2axis transparency=0.75 legendlabel='Obs';
run;

proc export data= summary_stats
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\CRE_summary_stats_NAICS.csv'
	dbms = csv
	REPLACE; 
run; 


/*Property Descr*/ 
Proc means data=out.df_cre_dev nway noprint order=formatted ;
	var y;
	Class property_descr2 ;
	output out=summary_stats mean(y)=default_rate;
run;
proc sort data=summary_stats out=summary_stats; 
	by  property_descr2;
run; 
Proc sgplot data=summary_stats;
	SERIES x=property_descr2 y=default_rate;
	band x=property_descr2 lower=0 upper=_FREQ_ / fillattrs=ltgray y2axis transparency=0.75 legendlabel='Obs';
run;

proc export data= summary_stats
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\CRE_summary_stats_property_descr.csv'
	dbms = csv
	REPLACE; 
run; 


/*boh_rating*/ 
Proc means data=out.df_cre_dev nway noprint order=formatted ;
	var y;
	Class boh_rating ;
	output out=summary_stats mean(y)=default_rate;
run;
proc sort data=summary_stats out=summary_stats; 
	by boh_rating;
run; 
Proc sgplot data=summary_stats;
	SERIES x=boh_rating y=default_rate;
	band x=boh_rating lower=0 upper=_FREQ_ / fillattrs=ltgray y2axis transparency=0.75 legendlabel='Obs';
run;

proc export data= summary_stats
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\CRE_summary_stats_boh_rating.csv'
	dbms = csv
	REPLACE; 
run; 

/*  Select validtion and estimation */ 
data interim.df_cre_model_vars ;
	set  interim.df_cre_model_vars ;
	n=ranuni(8);
run;

proc sort data=interim.df_cre_model_vars;
  by n;
run; 

data interim.training_cre interim.testing_cre;
   set interim.df_cre_model_vars nobs=nobs;
   if _n_<=.6*nobs then output interim.training_cre;
    else output interim.testing_cre;
run;

proc LOGISTIC data = interim.training_credesc plots = all 
	outmodel=CRE_model;
	class boh_rating1 ;
	model y =  prop_res 
				prop_retail 
				boh_rating1
				cahpi_ag_6
				caur_yd_3
				POB_95
				RGDP_qG_lag_2_neg;
	output out=out.outreg_cre p=predicted;
run;


proc logistic inmodel=CRE_model;
	score data=interim.testing_cre
 	out=out.outcomes_testing_cre;
run;

/*get in-sample */ 
proc sql; 
	create table interim.cre_PD_historical_newnaics as 
	select filedate, mean(predicted) as pd_mean_hist_training
	from out.outreg_cre
	group by filedate; 
quit; 

Proc sgplot data=interim.cre_PD_historical_newnaics;
	SERIES x=filedate y=pd_mean_hist_training;
run;

proc export data=interim.mean_PD_CRE_historical_training
   outfile='C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\mean_PD_CRE_historical_training.csv'
   dbms=csv
   replace;
run;

/*get historical */ 
proc sql; 
	create table interim.mean_PD_CRE_historical_newnaics as 
	select filedate, mean(y) as pd_mean_hist 
	from interim.df_cre_model_vars
	group by filedate; 
quit; 

Proc sgplot data=interim.mean_PD_CRE_historical_newnaics;
	SERIES x=filedate y=pd_mean_hist;
run;

proc export data=interim.mean_PD_CRE_historical
   outfile='C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\mean_PD_CRE_historical.csv'
   dbms=csv
   replace;
run;

/*get out-of-sample */ 
proc sql; 
	create table interim.mean_PD_CRE_estimated_testing as 
	select filedate, mean(p_1) as pd_mean_est_training
	from out.outcomes_testing_cre
	group by filedate; 
quit; 

Proc sgplot data=interim.mean_PD_CRE_historical_newnaics;
	SERIES x=filedate y=pd_mean_hist;
run;

proc export data=interim.mean_PD_CRE_estimated_training
   outfile='C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\mean_PD_CRE_estimated_training.csv'
   dbms=csv
   replace;
run;



/* in-sample back-test*/ 
Proc means data=out.outreg_cre nway noprint order=formatted ;
	var predicted;
	Class rgdpg_ag;
	format rgdpg_ag rgdpg_agf.;
	output out=summary_stats2 mean(predicted)=default_rate;
	run;

	proc export data= summary_stats2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\in_sample_rgdpg_training.csv'
	dbms = csv
	REPLACE; 
	run;

	
Proc means data=out.outreg_cre nway noprint order=formatted ;
	var predicted;
	Class pob;
	format pob pobf.;
	output out=summary_stats2 mean(predicted)=default_rate;
	run;

	proc export data= summary_stats2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\in_sample_pob_cre_training.csv'
	dbms = csv
	REPLACE; 
	run;


Proc means data=out.outreg_cre nway noprint order=formatted ;
	var predicted;
	Class dpd0129_0;
	/*format dpd0129 dpd0129f.;*/
	output out=summary_stats2 mean(predicted)=default_rate;
	run;

	proc export data= summary_stats2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\in_sample_dpd0129.csv'
	dbms = csv
	REPLACE; 
	run;

Proc means data=out.outreg_cre nway noprint order=formatted ;
	var predicted;
	Class loan_spread_2;
	format loan_spread_2 loan_spread_vf.;
	output out=summary_stats2 mean(predicted)=default_rate;
	run;

	proc export data= summary_stats2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\in_sample_loan_spread_cre_training.csv'
	dbms = csv
	REPLACE; 
	run;

Proc means data=out.outreg_cre nway noprint order=formatted ;
	var predicted;
	Class pfi_nonres_structures;
	format pfi_nonres_structures pfi_nonres_structuresf.;
	output out=summary_stats2 mean(predicted)=default_rate;
	run;

	proc export data= summary_stats2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\in_sample_pfi_nonres_structures_cre_training.csv'
	dbms = csv
	REPLACE; 
	run;


/*out-of-sample back test */
Proc means data=out.outcomes_testing_cre nway noprint order=formatted ;
	var P_1;
	Class rgdpg_ag;
	format rgdpg_ag rgdpg_agf.;
	output out=summary_stats2 mean(P_1)=default_rate;
	run;

	proc export data= summary_stats2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\out-of_sample_rgdpg.csv'
	dbms = csv
	REPLACE; 
	run;

	
Proc means data=out.outcomes_testing_cre nway noprint order=formatted ;
	var P_1;
	Class pob;
	format pob pobf.;
	output out=summary_stats2 mean(P_1)=default_rate;
	run;

	proc export data= summary_stats2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\out-of_sample_pob_cre.csv'
	dbms = csv
	REPLACE; 
	run;


Proc means data=out.outcomes_testing_cre nway noprint order=formatted ;
	var P_1;
	Class dpd0129_0;
	output out=summary_stats2 mean(P_1)=default_rate;
	run;

	proc export data= summary_stats2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\out_of_sample_dpd0129.csv'
	dbms = csv
	REPLACE; 
	run;

Proc means data=out.outcomes_testing_cre nway noprint order=formatted ;
	var P_1;
	Class loan_spread_2;
	format loan_spread_2 loan_spread_vf.;
	output out=summary_stats2 mean(P_1)=default_rate;
	run;

	proc export data= summary_stats2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\out_of_sample_loan_spread.csv'
	dbms = csv
	REPLACE; 
	run;

Proc means data=out.outcomes_testing_cre nway noprint order=formatted ;
	var P_1;
	Class pfi_nonres_structures;
	format pfi_nonres_structures pfi_nonres_structuresf.;
	output out=summary_stats2 mean(P_1)=default_rate;
	run;

	proc export data= summary_stats2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\out_of_sample_pfi_nonres_structures.csv'
	dbms = csv
	REPLACE; 
	run;

/*Forecast*/ 
/* step 1 - generate the data sets*/ 

data out.df_cre_fore_base

 (keep=y account_id filedate current_balance

loan_spread_0 loan_spread_2
POB POB_5 POB_95 pob_50
DPD3059_0 dpd0129_0 dpd0129_25 dpd3059_5
pfi_nonres_structures

bbb_yield_lag_2_1 bbb_yield_lag_2_2
bbb_yield_lag_1_1 bbb_yield_lag_1_2
tb5yr_1 tb5yr_2
tb5y tb10y

cahpi_ag cahpi_qg cci
djia_1 djia

RGDP_qG
ur ur_9 ur_9P
CAUR CAUR_YD caur_yd_1
caur_yd_lag_3 caur_yd_lag_3_1
hpi hpi_qg hpi_ag 
Cahpi_qg CAHPI_ag
	 
cppi cppi_ag cppi_qg
rdi_ag int_rate interest_rate
ur ngdpg_ag_lag_4 loan_spread_v
year_file q_file
NCREIF_Property_index
NAICS PROPERTY_DESCR2
boh_rating BOH_RATING2
rdi_qg
caur_yd
caur_yd_3
season
caur_yd_lag_3
dija 
dija_qg_3
dija_qg
cahpi_ag_6

property_type
boh_rating1
naics_code
naics_RE
property_type2
property_type
prop_retail
prop_res

ngdp_qg_1
ngdp_qg
				bbb_yield_lag_1_6
				RGDP_qG_lag_2_neg

);
	
	set out.df_boh_final_base; 

 	bbb_yield_lag_2_1 = (bbb_yield<=4.4);
	bbb_yield_lag_2_2 = max(bbb_yield, 4.4); 
	bbb_yield_lag_1_1 = (bbb_yield_lag_1<=4.3);
	bbb_yield_lag_1_2 = max(BBB_yield_lag_1, 4.3); 

	bbb_yield_lag_1_6 = max(BBB_yield_lag_1, 6); 
	RGDP_qG_lag_2_neg= min(RGDP_qG_lag_2, 0);
	
	tb5yr_1=min(tb5y, 2.25);
	tb5yr_2 = (tb5y>2.25); 

	djia_1=max(djia, 10250);
	dija_qg_3=min(dija_qg,3); 
	cahpi_ag_6=min(cahpi_ag,6);
	
	
	loan_spread_0= (loan_spread_v<=2);
	loan_spread_2 = max(loan_spread_v,2);
	
	
	ur_9= MIN(ur, 9);
	ur_9P = (ur>9);
	
	POB_5= min(POB,5);
	POB_95=max(POB,95);
	pob_50=max(pob, 50);
		
	dpd3059_0=(dpd3059=0);
	dpd0129_0= (dpd0129=0);
	dpd0129_25= MIN(DPD0129, 25);
	dpd3059_5= min(dpd3059, 5);
	
	hpi_1 = max(hpi, 170);

	vix_25= (vix<=25);
	vix_40= (vix>=40);
	vix_2540= min(max(vix, 25),40);
	
	rdi_ag_lag_3_1=min(0,rdi_ag_lag_3); 
	rdi_ag_lag_3_2=max(0, rdi_ag_lag_3);

	CAUR_YD_3=MIN(CAUR_YD, 3);
	caur_yd_lag_3_1 = Max(caur_yd_lag_3,-1); 


	SPR10_QD_1=MIN(SPR10, 0);
	SPR10_QD_2=MAX(SPR10, 0);

	original_balance_1=(original_balance<=600000);

	int_rate=100*interest_rate; 
	
	year_file=year(filedate); 
	q_file=qtr(filedate); 

	IF BOH_RATING IN (0,1,2,3) THEN BOH_RATING2= 'R1'; 
	ELSE IF BOH_RATING IN (4,1000) THEN BOH_RATING2= 'R2';
	ELSE IF BOH_RATING IN (2000) THEN BOH_RATING2= 'R3';
	ELSE IF BOH_RATING IN (3000, 4000) THEN BOH_RATING2='R4';  

	if q_file= 1 then season= 'sp';
	else if q_file=2 then season='su';
	else if q_file =3 then season='fa';
	else if q_file =4 then season = 'wi';

	if naics in ('Accomodation & Food', 'Retail')  then naics_code= 'a';
	else if naics = 'Real Estate & Rental' then naics_code='b';
	else if naics in ('Arts & Entertainment', 'Construction',
	'Health Care', 'Information', 'Manufacturing', 'Other', 'Science & Technology',
	'Transportation', 'Waste Management', 'Wholesale Trade') then naics_code = 'c'; 


	if naics = 'Real Estate & Rental' then naics_RE=1; 

	if boh_rating in (0,1,2,3) then boh_rating1='R1';
	else if boh_rating in (4, 1000) then boh_rating1='R2';
	else if boh_rating in (2000,3000) then boh_rating1 = 'R3';


	prop_res=(property_descr2 in ('Multifamily Residential', '1-4 Residential'));
	prop_retail=(property_descr2 in ('Retail shopping center'));
	

	if property_descr2 in ('Multifamily Residential', '1-4 Residential', 'Retail shopping center') then property_type='a';
	
	else if property_descr2 in ('Land') then property_type='c';
	else if property_descr2 in ('Carwash') then property_type='d';
	else if property_descr2 in ('Restaurant') then property_type='e';
	else if property_descr2 in ('Hotel/Motel') then property_type='f';
	else if property_descr2 in ('Gas station') then property_type='g';
	else if property_descr2 in ('Commercial-industrial') then property_type='h';
	else if property_descr2 in ('Other: single tenant', 'Mixed-use: comm/res', 'Office', 
								'Church/Religious', 'Auto repair shop', 'Medical', 
								'Golf course') then property_type='i';
	else if property_descr2 in ('Other') then property_type='j';

	if property_descr2 in ('Multifamily Residential', '1-4 Residential') then property_type2='a';
	else if property_descr2 in ('Retail shopping center', 'Office','Mixed-use: comm/res', 'Church/Religious', 'Auto repair shop', 'Golf Course', 'Medical') then property_type2='b';
	else if property_descr2 in ('Commercial-industrial', 'Gas station', 'Hotel/Motel', 'Other: single tenant') then property_type2='c';
	else if property_descr2 in ('Church/Religious', 'Auto repair shop', 'Medical', 
								'Golf course') then property_type2='i';
	else if property_descr2 in ('Carwash', 'Land', 'Other') then property_type2='j';

	ngdp_qg_1= max(ngdp_qg, -1);
run; 

data out.df_cre_fore_adverse
(keep=y account_id filedate current_balance

loan_spread_0 loan_spread_2
POB POB_5 POB_95 pob_50
DPD3059_0 dpd0129_0 dpd0129_25 dpd3059_5
pfi_nonres_structures

bbb_yield_lag_2_1 bbb_yield_lag_2_2
bbb_yield_lag_1_1 bbb_yield_lag_1_2
tb5yr_1 tb5yr_2
tb5y tb10y

cahpi_ag cahpi_qg cci
djia_1 djia

RGDP_qG
ur ur_9 ur_9P
CAUR CAUR_YD caur_yd_1
caur_yd_lag_3 caur_yd_lag_3_1
hpi hpi_qg hpi_ag 
Cahpi_qg CAHPI_ag
	 
cppi cppi_ag cppi_qg
rdi_ag int_rate interest_rate
ur ngdpg_ag_lag_4 loan_spread_v
year_file q_file
NCREIF_Property_index
NAICS PROPERTY_DESCR2
boh_rating BOH_RATING2
rdi_qg
caur_yd
caur_yd_3
season
caur_yd_lag_3
dija 
dija_qg_3
dija_qg
cahpi_ag_6

property_type
boh_rating1
naics_code
naics_RE
property_type2
property_type
prop_retail
prop_res

ngdp_qg_1
ngdp_qg

);
		set out.df_boh_final_adverse; 

 	bbb_yield_lag_2_1 = (bbb_yield<=4.4);
	bbb_yield_lag_2_2 = max(bbb_yield, 4.4); 
	bbb_yield_lag_1_1 = (bbb_yield_lag_1<=4.3);
	bbb_yield_lag_1_2 = max(BBB_yield_lag_1, 4.3); 



	tb5yr_1=min(tb5y, 2.25);
	tb5yr_2 = (tb5y>2.25); 

	djia_1=max(djia, 10250);
	dija_qg_3=min(dija_qg,3); 
	cahpi_ag_6=min(cahpi_ag,6);
	
	loan_spread_0= (loan_spread_v<=2);
	loan_spread_2 = max(loan_spread_v,2);
	
	ur_9= MIN(ur, 9);
	ur_9P = (ur>9);
	
	POB_5= min(POB,5);
	POB_95=max(POB,95);
	pob_50=max(pob, 50);
		
	dpd3059_0=(dpd3059=0);
	dpd0129_0= (dpd0129=0);
	dpd0129_25= MIN(DPD0129, 25);
	dpd3059_5= min(dpd3059, 5);
	
	hpi_1 = max(hpi, 170);

	vix_25= (vix<=25);
	vix_40= (vix>=40);
	vix_2540= min(max(vix, 25),40);
	
	rdi_ag_lag_3_1=min(0,rdi_ag_lag_3); 
	rdi_ag_lag_3_2=max(0, rdi_ag_lag_3);

	CAUR_YD_3=MIN(CAUR_YD, 3);
	caur_yd_lag_3_1 = Max(caur_yd_lag_3,-1); 


	SPR10_QD_1=MIN(SPR10, 0);
	SPR10_QD_2=MAX(SPR10, 0);

	original_balance_1=(original_balance<=600000);

	int_rate=100*interest_rate; 
	
	year_file=year(filedate); 
	q_file=qtr(filedate); 

	IF BOH_RATING IN (0,1,2,3) THEN BOH_RATING2= 'R1'; 
	ELSE IF BOH_RATING IN (4,1000) THEN BOH_RATING2= 'R2';
	ELSE IF BOH_RATING IN (2000) THEN BOH_RATING2= 'R3';
	ELSE IF BOH_RATING IN (3000, 4000) THEN BOH_RATING2='R4';  

	if q_file= 1 then season= 'sp';
	else if q_file=2 then season='su';
	else if q_file =3 then season='fa';
	else if q_file =4 then season = 'wi';

	if naics in ('Accomodation & Food', 'Retail')  then naics_code= 'a';
	else if naics = 'Real Estate & Rental' then naics_code='b';
	else if naics in ('Arts & Entertainment', 'Construction',
	'Health Care', 'Information', 'Manufacturing', 'Other', 'Science & Technology',
	'Transportation', 'Waste Management', 'Wholesale Trade') then naics_code = 'c'; 



	if naics = 'Real Estate & Rental' then naics_RE=1; 

	if boh_rating in (0,1,2,3) then boh_rating1='R1';
	else if boh_rating in (4, 1000) then boh_rating1='R2';
	else if boh_rating in (2000,3000) then boh_rating1 = 'R3';


	prop_res=(property_descr2 in ('Multifamily Residential', '1-4 Residential'));
	prop_retail=(property_descr2 in ('Retail shopping center'));
	

	if property_descr2 in ('Multifamily Residential', '1-4 Residential', 'Retail shopping center') then property_type='a';

	else if property_descr2 in ('Land') then property_type='c';
	else if property_descr2 in ('Carwash') then property_type='d';
	else if property_descr2 in ('Restaurant') then property_type='e';
	else if property_descr2 in ('Hotel/Motel') then property_type='f';
	else if property_descr2 in ('Gas station') then property_type='g';
	else if property_descr2 in ('Commercial-industrial') then property_type='h';
	else if property_descr2 in ('Other: single tenant', 'Mixed-use: comm/res', 'Office', 
								'Church/Religious', 'Auto repair shop', 'Medical', 
								'Golf course') then property_type='i';
	else if property_descr2 in ('Other') then property_type='j';

	if property_descr2 in ('Multifamily Residential', '1-4 Residential') then property_type2='a';
	else if property_descr2 in ('Retail shopping center', 'Office','Mixed-use: comm/res', 'Church/Religious', 'Auto repair shop', 'Golf Course', 'Medical') then property_type2='b';
	else if property_descr2 in ('Commercial-industrial', 'Gas station', 'Hotel/Motel', 'Other: single tenant') then property_type2='c';
	else if property_descr2 in ('Church/Religious', 'Auto repair shop', 'Medical', 
								'Golf course') then property_type2='i';
	else if property_descr2 in ('Carwash', 'Land', 'Other') then property_type2='j';

	ngdp_qg_1= max(ngdp_qg, -1);
run; 


data out.df_cre_fore_severe
(keep=y account_id filedate current_balance

loan_spread_0 loan_spread_2
POB POB_5 POB_95 pob_50
DPD3059_0 dpd0129_0 dpd0129_25 dpd3059_5
pfi_nonres_structures

bbb_yield_lag_2_1 bbb_yield_lag_2_2
bbb_yield_lag_1_1 bbb_yield_lag_1_2
tb5yr_1 tb5yr_2
tb5y tb10y

cahpi_ag cahpi_qg cci
djia_1 djia

RGDP_qG
ur ur_9 ur_9P
CAUR CAUR_YD caur_yd_1
caur_yd_lag_3 caur_yd_lag_3_1
hpi hpi_qg hpi_ag 
Cahpi_qg CAHPI_ag
	 
cppi cppi_ag cppi_qg
rdi_ag int_rate interest_rate
ur ngdpg_ag_lag_4 loan_spread_v
year_file q_file
NCREIF_Property_index
NAICS PROPERTY_DESCR2
boh_rating BOH_RATING2
rdi_qg
caur_yd
caur_yd_3
season
caur_yd_lag_3
dija 
dija_qg_3
dija_qg
cahpi_ag_6

property_type
boh_rating1
naics_code
naics_RE
property_type2
property_type
prop_retail
prop_res

ngdp_qg_1
ngdp_qg

);
	
		set out.df_boh_final_severe; 

 	bbb_yield_lag_2_1 = (bbb_yield<=4.4);
	bbb_yield_lag_2_2 = max(bbb_yield, 4.4); 
	bbb_yield_lag_1_1 = (bbb_yield_lag_1<=4.3);
	bbb_yield_lag_1_2 = max(BBB_yield_lag_1, 4.3); 

	tb5yr_1=min(tb5y, 2.25);
	tb5yr_2 = (tb5y>2.25); 

	djia_1=max(djia, 10250);
	dija_qg_3=min(dija_qg,3); 
	cahpi_ag_6=min(cahpi_ag,6);

	loan_spread_0= (loan_spread_v<=2);
	loan_spread_2 = max(loan_spread_v,2);
	
	ur_9= MIN(ur, 9);
	ur_9P = (ur>9);
	
	POB_5= min(POB,5);
	POB_95=max(POB,95);
	pob_50=max(pob, 50);
		
	dpd3059_0=(dpd3059=0);
	dpd0129_0= (dpd0129=0);
	dpd0129_25= MIN(DPD0129, 25);
	dpd3059_5= min(dpd3059, 5);
	
	hpi_1 = max(hpi, 170);

	vix_25= (vix<=25);
	vix_40= (vix>=40);
	vix_2540= min(max(vix, 25),40);
	
	rdi_ag_lag_3_1=min(0,rdi_ag_lag_3); 
	rdi_ag_lag_3_2=max(0, rdi_ag_lag_3);

	CAUR_YD_3=MIN(CAUR_YD, 3);
	caur_yd_lag_3_1 = Max(caur_yd_lag_3,-1); 


	SPR10_QD_1=MIN(SPR10, 0);
	SPR10_QD_2=MAX(SPR10, 0);

	original_balance_1=(original_balance<=600000);

	int_rate=100*interest_rate; 
	
	year_file=year(filedate); 
	q_file=qtr(filedate); 

	IF BOH_RATING IN (0,1,2,3) THEN BOH_RATING2= 'R1'; 
	ELSE IF BOH_RATING IN (4,1000) THEN BOH_RATING2= 'R2';
	ELSE IF BOH_RATING IN (2000) THEN BOH_RATING2= 'R3';
	ELSE IF BOH_RATING IN (3000, 4000) THEN BOH_RATING2='R4';  

	if q_file= 1 then season= 'sp';
	else if q_file=2 then season='su';
	else if q_file =3 then season='fa';
	else if q_file =4 then season = 'wi';

	if naics in ('Accomodation & Food', 'Retail')  then naics_code= 'a';
	else if naics = 'Real Estate & Rental' then naics_code='b';
	else if naics in ('Arts & Entertainment', 'Construction',
	'Health Care', 'Information', 'Manufacturing', 'Other', 'Science & Technology',
	'Transportation', 'Waste Management', 'Wholesale Trade') then naics_code = 'c'; 

	if naics = 'Real Estate & Rental' then naics_RE=1; 

	if boh_rating in (0,1,2,3) then boh_rating1='R1';
	else if boh_rating in (4, 1000) then boh_rating1='R2';
	else if boh_rating in (2000,3000) then boh_rating1 = 'R3';


	prop_res=(property_descr2 in ('Multifamily Residential', '1-4 Residential'));
	prop_retail=(property_descr2 in ('Retail shopping center'));
	

	if property_descr2 in ('Multifamily Residential', '1-4 Residential', 'Retail shopping center') then property_type='a';

	else if property_descr2 in ('Land') then property_type='c';
	else if property_descr2 in ('Carwash') then property_type='d';
	else if property_descr2 in ('Restaurant') then property_type='e';
	else if property_descr2 in ('Hotel/Motel') then property_type='f';
	else if property_descr2 in ('Gas station') then property_type='g';
	else if property_descr2 in ('Commercial-industrial') then property_type='h';
	else if property_descr2 in ('Other: single tenant', 'Mixed-use: comm/res', 'Office', 
								'Church/Religious', 'Auto repair shop', 'Medical', 
								'Golf course') then property_type='i';
	else if property_descr2 in ('Other') then property_type='j';

	if property_descr2 in ('Multifamily Residential', '1-4 Residential') then property_type2='a';
	else if property_descr2 in ('Retail shopping center', 'Office','Mixed-use: comm/res', 'Church/Religious', 'Auto repair shop', 'Golf Course', 'Medical') then property_type2='b';
	else if property_descr2 in ('Commercial-industrial', 'Gas station', 'Hotel/Motel', 'Other: single tenant') then property_type2='c';
	else if property_descr2 in ('Church/Religious', 'Auto repair shop', 'Medical', 
								'Golf course') then property_type2='i';
	else if property_descr2 in ('Carwash', 'Land', 'Other') then property_type2='j';

	ngdp_qg_1= max(ngdp_qg, -1);
run; 


/*Step 2 - generate the forecasts*/ 

/*base*/ 
proc logistic inmodel=CRE_model;
	score data=out.df_cre_fore_base
 	out=out.outcomes_base_cre;
run;


proc sql; 
	create table out.mean_PD_ci_v5_base as 
	select filedate,sum(current_balance) as sum_bal,sum(current_balance*P_1) as P1_bal, 
			sum(current_balance*P_1)/sum(current_balance) as W_ave_pd_mean_est,
			mean(P_1) as est_default

	from out.outcomes_base_cre
	group by filedate; 
quit; 

Proc sgplot data=out.mean_PD_ci_v5_base;
	SERIES x=filedate y=est_default;
run;

/* Adverse */
proc logistic inmodel=CRE_model;
	score data=out.df_cre_fore_adverse
 	out=out.outcomes_adverse_cre;
run;


proc sql; 
	create table out.mean_PD_ci_v5_adverse_newnaics2 as 
	select filedate,sum(current_balance) as sum_bal,sum(current_balance*P_1) as P1_bal, 
			sum(current_balance*P_1)/sum(current_balance) as W_ave_pd_mean_est,
			mean(P_1) as est_default

	from out.outcomes_adverse_cre
	group by filedate; 
quit; 

Proc sgplot data=out.mean_PD_ci_v5_adverse;
	SERIES x=filedate y=est_default;
run;


proc logistic inmodel=CRE_model;
	score data=out.df_cre_fore_severe
 	out=out.outcomes_severe_cre;
run;


proc sql; 
	create table out.mean_PD_ci_v5_severe as 
	select filedate,sum(current_balance) as sum_bal,sum(current_balance*P_1) as P1_bal, 
			sum(current_balance*P_1)/sum(current_balance) as W_ave_pd_mean_est, 
			mean(P_1) AS est_default
	from out.outcomes_severe_cre
	group by filedate; 
quit; 

Proc sgplot data=out.mean_PD_ci_v5_severe;
	SERIES x=filedate y=est_default;
run;

/* in-sample back-test*/ 
/* property_type*/
data test (keep=predicted prop_res prop_retail pt) ; 
	set out.outreg_cre; 
	if prop_res=1 then pt='G1-Res';
	else if prop_retail=1 then pt='G2-Ret';
	else pt='G3-Oth';
run;

data test2 (keep=y prop_res prop_retail pt) ; 
	set interim.df_cre_model_vars; 
	if prop_res=1 then pt='G1-Res';
	else if prop_retail=1 then pt='Ge-Ret';
	else pt='G3-Oth';
run;
 
Proc means data=test 
	nway noprint order=formatted ;
	var predicted;
	Class pt;
	output out=summary_stats2 mean(predicted)=default_rate;
run;

Proc means data=test2 
	nway noprint order=formatted ;
	var y;
	Class pt;
	output out=summary_stats2 mean(y)=default_rate;
run;



proc export data= summary_stats2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\in_sample_rgdpg_training.csv'
	dbms = csv
	REPLACE; 
run;


/*GDP*/
Proc means data=out.outreg_cre
			nway noprint order=formatted ;
	var predicted;
	Class RGDP_qG_lag_2_neg;
	format RGDP_qG_lag_2_neg rgdp_qg_lag_2f.;
	output out=summary_stats2 mean(predicted)=default_rate;
	run;

	proc export data= summary_stats2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\in_sample_rgdpg_training.csv'
	dbms = csv
	REPLACE; 
	run;

/* caur_yd*/	
Proc means data=out.outreg_cre 
					nway noprint order=formatted ;
	var predicted;
	Class CAUR_YD_3;
	format CAUR_YD_3 caur_adf.;
	output out=summary_stats2 mean(predicted)=default_rate;
	run;

	proc export data= summary_stats2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\in_sample_CAUR_YD_CRE_training.csv'
	dbms = csv
	REPLACE; 
	run;
	/*cahpi*/
Proc means data=out.outreg_cre
				nway noprint order=formatted ;
	var predicted;
	Class cahpi_ag_6;
	format cahpi_ag_6 cahpi_agf.;
	output out=summary_stats2 mean(predicted)=default_rate;
	run;

proc export data= summary_stats2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\in_sample_cahpi.csv'
	dbms = csv
	REPLACE; 
	run;

	/*POB*/
Proc means data=out.outreg_cre
					nway noprint order=formatted ;
	var predicted;
	Class POB;
	format POB POBf.;
	output out=summary_stats2 mean(predicted)=default_rate;
	run;

proc export data= summary_stats2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\in_sample_pob_cre_training.csv'
	dbms = csv
	REPLACE; 
	run;

	/* Proerty Type*/
	/*Residential*/
Proc means data=out.outreg_cre
					nway noprint order=formatted ;
	var predicted;
	Class prop_res;
	output out=summary_stats2 mean(predicted)=default_rate;
	run;

Proc means data=out.outreg_cre
					nway noprint order=formatted ;
	var predicted;
	Class prop_retail;
	output out=summary_stats2 mean(predicted)=default_rate;
	run;

/*boh_rating*/ 
Proc means data=out.outreg_cre
					nway noprint order=formatted ;
	var predicted;
	Class boh_rating;
	output out=summary_stats2 mean(predicted)=default_rate;
	run;

proc export data= summary_stats2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\in_sample_boh_rating_cre_training.csv'
	dbms = csv
	REPLACE; 
	run;

Proc means data=out.outreg_cre
					nway noprint order=formatted ;
	var predicted;
	Class boh_rating1;
	output out=summary_stats2 mean(predicted)=default_rate;
	run;

proc export data= summary_stats2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\in_sample_boh_rating1_cre_training.csv'
	dbms = csv
	REPLACE; 
	run;

Proc means data=interim.df_cre_model_vars 
					nway noprint order=formatted ;
	var y;
	Class boh_rating1;
	output out=summary_stats2 mean(y)=default_rate;
run;

proc export data= summary_stats2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\hist_boh_rating1_cre.csv'
	dbms = csv
	REPLACE; 
run;

Proc means data=interim.df_cre_model_vars 
					nway noprint order=formatted ;
	var y;
	Class boh_rating;
	output out=summary_stats2 mean(y)=default_rate;
run;

proc export data= summary_stats2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\hist_boh_rating_cre.csv'
	dbms = csv
	REPLACE; 
	run;


/* out-of-sample backtest*/
	
/* out-of-sample back-test*/ 
/* property_tuype*/ 
data test (keep=P_1 prop_res prop_retail pt) ; 
	set out.outcomes_testing_cre ; 
	if prop_res=1 then pt='G1-Res';
	else if prop_retail=1 then pt='Ge-Ret';
	else pt='G3-Oth';
run;

data test2 (keep=y prop_res prop_retail pt) ; 
	set interim.df_cre_model_vars; 
	if prop_res=1 then pt='G1-Res';
	else if prop_retail=1 then pt='Ge-Ret';
	else pt='G3-Oth';
run;
 
Proc means data=test 
			nway noprint order=formatted ;
	var p_1;
	Class pt;
	output out=summary_stats2 mean(P_1)=default_rate;
run;

Proc means data=test2 
			nway noprint order=formatted ;
	var y;
	Class pt;
	output out=summary_stats2 mean(y)=default_rate;
run;



Proc means data=out.outcomes_testing_cre 
			nway noprint order=formatted ;
	var P_1;
	Class RGDP_qG_lag_2_neg;
	format RGDP_qG_lag_2_neg rgdp_qg_lag_2f.;
	output out=summary_stats2 mean(P_1)=default_rate;
	run;

proc export data= summary_stats2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\out_sample_rgdpg_training.csv'
	dbms = csv
	REPLACE; 
	run;

/* caur_yd*/	
Proc means data=out.outcomes_testing_cre 
					nway noprint order=formatted ;
	var P_1;
	Class CAUR_YD_3;
	format CAUR_YD_3 caur_adf.;
	output out=summary_stats2 mean(P_1)=default_rate;
	run;

	proc export data= summary_stats2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\out_sample_CAUR_YD_CRE.csv'
	dbms = csv
	REPLACE; 
	run;
	/*cahpi*/
Proc means data=out.outcomes_testing_cre
				nway noprint order=formatted ;
	var P_1;
	Class cahpi_ag_6;
	format cahpi_ag_6 cahpi_agf.;
	output out=summary_stats2 mean(P_1)=default_rate;
	run;

proc export data= summary_stats2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\out_sample_cahpi_cre.csv'
	dbms = csv
	REPLACE; 
	run;

	/*POB*/
Proc means data=out.outcomes_testing_cre
					nway noprint order=formatted ;
	var P_1;
	Class POB;
	format POB POBf.;
	output out=summary_stats2 mean(P_1)=default_rate;
	run;

proc export data= summary_stats2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\out_sample_pob_cre.csv'
	dbms = csv
	REPLACE; 
	run;

	/* Proerty Type*/
	/*Residential*/
Proc means data=out.outcomes_testing_cre
					nway noprint order=formatted ;
	var P_1;
	Class prop_res;
	output out=summary_stats2 mean(P_1)=default_rate;
	run;

Proc means data=out.outcomes_testing_cre
					nway noprint order=formatted ;
	var P_1;
	Class prop_retail;
	output out=summary_stats2 mean(P_1)=default_rate;
	run;

/*boh_rating*/ 
Proc means data=out.outcomes_testing_cre
					nway noprint order=formatted ;
	var P_1;
	Class boh_rating;
	output out=summary_stats2 mean(P_1)=default_rate;
	run;

proc export data= summary_stats2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\out_sample_boh_rating_cre.csv'
	dbms = csv
	REPLACE; 
	run;

Proc means data=out.outcomes_testing_cre
					nway noprint order=formatted ;
	var P_1;
	Class boh_rating1;
	output out=summary_stats2 mean(P_1)=default_rate;
	run;

proc export data= summary_stats2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\out_sample_boh_rating1_cre.csv'
	dbms = csv
	REPLACE; 
	run;






/*for Fry run*/ 

/*get the data for the dry run*/ 
/*Base*/ 
data out.cre_dryrun_base; 
 	set out.outcomes_base_cre (keep= filedate account_ID P_1); 
	if filedate<= '31Mar16'd then delete; 
	rename P_1=PD;
run; 


**2016Q2;
Data out.cre_dryrun_base_q1;
	set out.cre_dryrun_base(WHERE=(FILEDATE='01jUN16'd));
RUN;
	
proc export data= out.cre_dryrun_base_q1
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\cre_dryrun_base_q1.csv'
	dbms = csv
	REPLACE;
run; 

**2016Q3;
Data out.cre_dryrun_base_q2;
	set out.cre_dryrun_base(WHERE=(FILEDATE='01SEP16'd));
RUN;
	
proc export data= out.cre_dryrun_base_q2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\cre_dryrun_base_q2.csv'
	dbms = csv
	REPLACE;
run; 

**2016Q4;
Data out.cre_dryrun_base_q3;
	set out.cre_dryrun_base(WHERE=(FILEDATE='01DEC16'd));
RUN;
	
proc export data= out.cre_dryrun_base_q3
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\cre_dryrun_base_q3.csv'
	dbms = csv
	REPLACE;
run;

**2017Q1;
Data out.cre_dryrun_base_q4;
	set out.cre_dryrun_base(WHERE=(FILEDATE='01MAR17'd));
RUN;
	
proc export data= out.cre_dryrun_base_q4
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\cre_dryrun_base_q4.csv'
	dbms = csv
	REPLACE;
run;

**2017Q2;
Data out.cre_dryrun_base_q5;
	set out.cre_dryrun_base(WHERE=(FILEDATE='01JUN17'd));
RUN;
	
proc export data= out.cre_dryrun_base_q5
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\cre_dryrun_base_q5.csv'
	dbms = csv
	REPLACE;
run;

**2017Q3;
Data out.cre_dryrun_base_q6;
	set out.cre_dryrun_base(WHERE=(FILEDATE='01SEP17'd));
RUN;
	
proc export data= out.cre_dryrun_base_q6
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\cre_dryrun_base_q6.csv'
	dbms = csv
	REPLACE;
run;

**2017Q4;
Data out.cre_dryrun_base_q7;
	set out.cre_dryrun_base(WHERE=(FILEDATE='01DEC17'd));
RUN;
	
proc export data= out.cre_dryrun_base_q7
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\cre_dryrun_base_q7.csv'
	dbms = csv
	REPLACE;
run;

**2018Q1;
Data out.cre_dryrun_base_q8;
	set out.cre_dryrun_base(WHERE=(FILEDATE='01MAR18'd));
RUN;
	
proc export data= out.cre_dryrun_base_q8
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\cre_dryrun_base_q8.csv'
	dbms = csv
	REPLACE;
run;

**2018Q2;
Data out.cre_dryrun_base_q9;
	set out.cre_dryrun_base(WHERE=(FILEDATE='01JUN18'd));
RUN;
	
proc export data= out.cre_dryrun_base_q9
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\cre_dryrun_base_q9.csv'
	dbms = csv
	REPLACE;
run;

**********ADVERSE;
data out.cre_dryrun_adverse ; 
 	set out.outcomes_adverse_cre (keep= filedate account_ID P_1);
	if filedate<= '31Mar16'd then delete; 
	rename P_1 = PD;
run; 


**2016Q2;
Data out.cre_dryrun_adverse_q1;
	set out.cre_dryrun_adverse(WHERE=(FILEDATE='01jUN16'd));
RUN;
	
proc export data= out.cre_dryrun_adverse_q1
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\cre_dryrun_adverse_q1.csv'
	dbms = csv
	REPLACE;
run; 

**2016Q3;
Data out.cre_dryrun_adverse_q2;
	set out.cre_dryrun_adverse(WHERE=(FILEDATE='01SEP16'd));
RUN;
	
proc export data= out.cre_dryrun_adverse_q2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\cre_dryrun_adverse_q2.csv'
	dbms = csv
	REPLACE;
run; 

**2016Q4;
Data out.cre_dryrun_adverse_q3;
	set out.cre_dryrun_adverse(WHERE=(FILEDATE='01DEC16'd));
RUN;
	
proc export data= out.cre_dryrun_adverse_q3
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\cre_dryrun_adverse_q3.csv'
	dbms = csv
	REPLACE;
run;

**2017Q1;
Data out.cre_dryrun_adverse_q4;
	set out.cre_dryrun_adverse(WHERE=(FILEDATE='01MAR17'd));
RUN;
	
proc export data= out.cre_dryrun_adverse_q4
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\cre_dryrun_adverse_q4.csv'
	dbms = csv
	REPLACE;
run;

**2017Q2;
Data out.cre_dryrun_adverse_q5;
	set out.cre_dryrun_adverse(WHERE=(FILEDATE='01JUN17'd));
RUN;
	
proc export data= out.cre_dryrun_adverse_q5
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\cre_dryrun_adverse_q5.csv'
	dbms = csv
	REPLACE;
run;

**2017Q3;
Data out.cre_dryrun_adverse_q6;
	set out.cre_dryrun_adverse(WHERE=(FILEDATE='01SEP17'd));
RUN;
	
proc export data= out.cre_dryrun_adverse_q6
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\cre_dryrun_adverse_q6.csv'
	dbms = csv
	REPLACE;
run;

**2017Q4;
Data out.cre_dryrun_adverse_q7;
	set out.cre_dryrun_adverse(WHERE=(FILEDATE='01DEC17'd));
RUN;
	
proc export data= out.cre_dryrun_adverse_q7
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\cre_dryrun_adverse_q7.csv'
	dbms = csv
	REPLACE;
run;

**2018Q1;
Data out.cre_dryrun_adverse_q8;
	set out.cre_dryrun_adverse(WHERE=(FILEDATE='01MAR18'd));
RUN;
	
proc export data= out.cre_dryrun_adverse_q8
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\cre_dryrun_adverse_q8.csv'
	dbms = csv
	REPLACE;
run;

**2018Q2;
Data out.cre_dryrun_adverse_q9;
	set out.cre_dryrun_adverse(WHERE=(FILEDATE='01JUN18'd));
RUN;
	
proc export data= out.cre_dryrun_adverse_q9
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\cre_dryrun_adverse_q9.csv'
	dbms = csv
	REPLACE;
run;

**SEVERE;
data out.cre_dryrun_severe; 
 	set out.outcomes_severe_cre (keep= filedate account_ID P_1);
	if filedate<= '31Mar16'd then delete; 
	rename P_1 = PD;
run; 


**2016Q2;
Data out.cre_dryrun_severe_q1;
	set out.cre_dryrun_severe(WHERE=(FILEDATE='01jUN16'd));
RUN;
	
proc export data= out.cre_dryrun_severe_q1
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\cre_dryrun_severe_q1.csv'
	dbms = csv
	REPLACE;
run; 

**2016Q3;
Data out.cre_dryrun_severe_q2;
	set out.cre_dryrun_severe(WHERE=(FILEDATE='01SEP16'd));
RUN;
	
proc export data= out.cre_dryrun_severe_q2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\cre_dryrun_severe_q2.csv'
	dbms = csv
	REPLACE;
run; 

**2016Q4;
Data out.cre_dryrun_severe_q3;
	set out.cre_dryrun_severe(WHERE=(FILEDATE='01DEC16'd));
RUN;
	
proc export data= out.cre_dryrun_severe_q3
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\cre_dryrun_severe_q3.csv'
	dbms = csv
	REPLACE;
run;

**2017Q1;
Data out.cre_dryrun_severe_q4;
	set out.cre_dryrun_severe(WHERE=(FILEDATE='01MAR17'd));
RUN;
	
proc export data= out.cre_dryrun_severe_q4
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\cre_dryrun_severe_q4.csv'
	dbms = csv
	REPLACE;
run;

**2017Q2;
Data out.cre_dryrun_severe_q5;
	set out.cre_dryrun_severe(WHERE=(FILEDATE='01JUN17'd));
RUN;
	
proc export data= out.cre_dryrun_severe_q5
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\cre_dryrun_severe_q5.csv'
	dbms = csv
	REPLACE;
run;

**2017Q3;
Data out.cre_dryrun_severe_q6;
	set out.cre_dryrun_severe(WHERE=(FILEDATE='01SEP17'd));
RUN;
	
proc export data= out.cre_dryrun_severe_q6
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\cre_dryrun_severe_q6.csv'
	dbms = csv
	REPLACE;
run;

**2017Q4;
Data out.cre_dryrun_severe_q7;
	set out.cre_dryrun_severe(WHERE=(FILEDATE='01DEC17'd));
RUN;
	
proc export data= out.cre_dryrun_severe_q7
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\cre_dryrun_severe_q7.csv'
	dbms = csv
	REPLACE;
run;

**2018Q1;
Data out.cre_dryrun_severe_q8;
	set out.cre_dryrun_severe(WHERE=(FILEDATE='01MAR18'd));
RUN;
	
proc export data= out.cre_dryrun_severe_q8
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\cre_dryrun_severe_q8.csv'
	dbms = csv
	REPLACE;
run;

**2018Q2;
Data out.cre_dryrun_severe_q9;
	set out.cre_dryrun_severe(WHERE=(FILEDATE='01JUN18'd));
RUN;
	
proc export data= out.cre_dryrun_severe_q9
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\cre_dryrun_severe_q9.csv'
	dbms = csv
	REPLACE;
run;


