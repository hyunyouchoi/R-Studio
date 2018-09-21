/* clear the log window*/
dm 'log' clear; 
LIBNAME Interim "";
LIBNAME Out "";

/*ODS RTF FILE="C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\ci_profiles_v2.RTF";*/
ods graphics on;

data out.df_ci_dev;
	set  out.df_boh_final_base (where=(filedate<= '31Mar16'd and portfolio_id="CI"));
run;




data  out.df_ci_dev; 
	set  out.df_ci_dev;
	year_orig= year(origination_date); 
	q_orig= qtr(origination_date); 
run;

/*read in the origination macro file*/ 
proc import out = interim.caur_hpi
            datafile = "C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\origination_caur_cahpi.csv" 
            dbms = csv replace;
run;


proc sql;
	create table out.df_ci_dev_orig as 
	select a.*, b.caur as orig_caur, b.cahpi as orig_cahpi from 
	 out.df_ci_dev as a
	left join 
	interim.caur_hpi as b 
	on a.year_orig=b.year
	and a.q_orig=b.q;
quit; 


data out.df_ci_dev_orig; 
	set out.df_ci_dev_orig;
	delta_caur= caur-orig_caur; 
	delta_hpi= cahpi-orig_cahpi;
run;

/* Scenarios*/ 
/*base*/ 
data out.df_ci_fore_base; 
	set out.df_ci_fore_base ;
	year_orig= year(origination_date); 
	q_orig= qtr(origination_date); 
run;


proc sql;
	create table out.df_ci_dev_orig_base as 
	select a.*, b.caur as orig_caur, b.cahpi as orig_cahpi from 
	out.df_ci_fore_base as a
	left join 
	interim.caur_hpi as b 
	on a.year_orig=b.year
	and a.q_orig=b.q;
quit; 


data out.df_ci_dev_orig_base; 
	set out.df_ci_dev_orig_base;
	delta_caur= caur-orig_caur; 
	delta_hpi= cahpi-orig_cahpi;
run;


/*adverse*/ 
data out.df_ci_fore_adverse; 
	set out.df_ci_fore_adverse ;
	year_orig= year(origination_date); 
	q_orig= qtr(origination_date); 
run;


proc sql;
	create table out.df_ci_dev_orig_adverse as 
	select a.*, b.caur as orig_caur, b.cahpi as orig_cahpi from 
	out.df_ci_fore_adverse as a
	left join 
	interim.caur_hpi as b 
	on a.year_orig=b.year
	and a.q_orig=b.q;
quit; 


data out.df_ci_dev_orig_adverse; 
	set out.df_ci_dev_orig_adverse;
	delta_caur= caur-orig_caur; 
	delta_hpi= cahpi-orig_cahpi;
run;


/*severe*/ 
data out.df_ci_fore_severe; 
	set out.df_ci_fore_severe ;
	year_orig= year(origination_date); 
	q_orig= qtr(origination_date); 
run;


proc sql;
	create table out.df_ci_dev_orig_severe as 
	select a.*, b.caur as orig_caur, b.cahpi as orig_cahpi from 
	out.df_ci_fore_severe as a
	left join 
	interim.caur_hpi as b 
	on a.year_orig=b.year
	and a.q_orig=b.q;
quit; 


data out.df_ci_dev_orig_severe; 
	set out.df_ci_dev_orig_severe;
	delta_caur= caur-orig_caur; 
	delta_hpi= cahpi-orig_cahpi;
run;


/*loan rating profile */

Proc means data=out.df_ci_dev_cleaned4 nway noprint order=formatted ;
	var y;
	Class boh_rating;
	output out=summary_stats mean(y)=default_rate;
	run;

Proc sgplot data=summary_stats;

series x=boh_rating y=default_rate;
band x=boh_rating lower=0 upper=_FREQ_ / fillattrs=ltgray y2axis transparency=0.75 legendlabel='Obs';
	run;



/* NAICS*/ 

Proc means data=out.df_ci_dev_cleaned3 nway noprint order=formatted ;
	var y;
	Class naics;
	output out=summary_stats mean(y)=default_rate;
	run;

Proc sgplot data=summary_stats;
series x=naics y=default_rate;
band x=naics lower=0 upper=_FREQ_ / fillattrs=ltgray y2axis transparency=0.75 legendlabel='Obs';
run;

proc export data= summary_stats
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\CI_summary_stats.csv'
	dbms = csv
	REPLACE;
run;

/*seasons */ 
Proc means data=out.df_ci_dev_cleaned3 nway noprint order=formatted ;
	var y;
	Class q;
	output out=summary_stats mean(y)=default_rate;
run;

Proc sgplot data=summary_stats;
series x=q y=default_rate;
band x=q lower=0 upper=_FREQ_ / fillattrs=ltgray y2axis transparency=0.75 legendlabel='Obs';
run;

proc export data= summary_stats
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\CI_season_summary_stats.csv'
	dbms = csv
	REPLACE;
run;


	/* Models */ 
data out.df_ci_model_vars_v5 (KEEP = Y  naics naics_code boh_rating1 naics_code2 naics_code3 boh_rating2 naics boh_rating

				loan_spread_v loan_spread_8 loan_spread_8_max

				CAUR CAUR_YD_lag_3 CAUR_YD_Lag_3_neg CAUR_YD_Lag_3_pos
				delta_caur_max1 delta_caur_min1 delta_caur
				rdi_qg_lag_3 rdi_qg_lag_3_6
				ndi_qg_lag_3
				ngdp_qg_4
				cci_ag_n9
				KOGDP_ag_4
				cahpi_ag
				cahpi_ag_1 cahpi_ag_2 cahpi_ag_3
				cahpi_ag_lag_3_n12
				vix_ad_n15
				cppi_ag_1 cppi_ag_2 
				cppi hpi vix CAHPI

				naics_code_Retail
				naics_code_const
				naics_code_manu

				loan_spread2_4
				loan_spread2_7

				i10y_qd_neg i10y_qd_pos
				bbb_yield_ad
				bbb_yield_qd
				boh_rating
				bbb_yield_qd_1 bbb_yield_qd_2 bbb_yield_ad_1 bbb_yield_ad_2
								POB_5 POB_50 POB_50max  dpd3059_0 dpd0129_0 
				filedate current_balance account_id q season 
				pob dpd0129 CAUR_yd_lag_3 bbb_yield_qd cahpi_ag_lag_3 cahpi_ag);
		set out.df_ci_dev_orig (where=(filedate>='31Mar08'd)); 
	
	if naics in ('Waste Management', 'Construction') then naics_code = 'a';
	else if naics in ('Real Estate & Rental') then naics_code='b'; 
	else if naics in ('Accomodation & Food', 'Retail', 'Transportation') then naics_code='c';
	else if naics in ('Wholesale Trade') then naics_code ='d'; 
	else if naics in ('Arts & Entertainment', 'Science & Technology', 'Information') then naics_code='e';
	else if naics in ('Health Care') then naics_code='f';
	else if naics in ('Manufacturing') then naics_code='g'; 
	else if naics in ('Other') then naics_code='h';

	if naics in ('Waste Management', 'Construction') then naics_code2 = 'a';
	else if naics in ('Accomodation & Food', 'Retail', 'Transportation') then naics_code2='c';
	else if naics in ('Wholesale Trade') then naics_code2 ='d'; 
	else if naics in ('Manufacturing') then naics_code2='g'; 
	else if naics in ('Other', 'Health Care', 'Real Estate & Rental', 'Arts & Entertainment', 'Science & Technology', 'Information') then naics_code2='h';

	if naics in ('Accomodation & Food', 'Retail') then naics_code3='c';
	else if naics in ('Wholesale Trade') then naics_code3 ='d'; 
	else if naics in ('Manufacturing') then naics_code3='g'; 
	else if naics in ('Other', 'Health Care', 'Real Estate & Rental'
						, 'Arts & Entertainment', 'Science & Technology', 
						'Information', 'Transportation', 
						'Waste Management', 'Construction') then naics_code3='h';

		if naics='error' then delete; 
	if loan_spread_v<0 then delete;

	if boh_rating in (1,2) then boh_rating1='R1';
	else if boh_rating in (0,3,4,1000) then boh_rating1='R2';
	else if boh_rating in (2000) then boh_rating1='R3';
	else if boh_rating in (3000, 4000) then boh_rating1='R4';	

		if boh_rating in (1,2) then boh_rating2='R1';
	else if boh_rating in (0,3,4,1000) then boh_rating2='R2';
	else if boh_rating in (2000, 3000, 4000) then boh_rating2='R3';
	
	rdi_qg_lag_3_6= min(rdi_qg_lag_3, 6);

	ngdp_qg_4= Max(ngdp_qg_4,4);
	i10y_qd_neg= min(i10y_qd, 0);
	i10y_qd_pos= max(i10y_qd,0);

	vix_ad_n15=min(vix_ad, -15);

	CAUR_yd_lag_3_neg = min(CAUR_YD_Lag_3, 0);
	CAUR_YD_Lag_3_pos = max(CAUR_YD_Lag_3, 0);  
	delta_caur_max1=max(delta_caur, 1); 
	delta_caur_min1=min(delta_caur, 1);

	cci_ag_n9= min(cci_ag, -9);
	KOGDP_ag_4=max(KOGDP_ag,4);
	
	cppi_ag_1=max(cppi_ag, 11);
	cppi_ag_2= min(cppi_ag-7);

	cahpi_ag_3=max(cahpi_ag, -2);
	cahpi_ag_2=min(max(cahpi_ag, -2), -15);
	cahpi_ag_1=(cahpi_ag<-15);
	cahpi_ag_lag_3_n12=max(cahpi_ag_lag_3, -12);


	loan_spread_8=min(loan_spread_v,8);
	loan_spread_8_max=(loan_spread_v>8); 
	
	loan_spread2_4= min(loan_spread_v,4);
	loan_spread2_7=max(loan_spread_v,4);


	bbb_yield_qd_1= min(bbb_yield_qd,-0.5);
	bbb_yield_qd_2= max(bbb_yield_qd,-0.5);

	bbb_yield_ad_1= min(bbb_yield_ad,1.1);
	bbb_yield_ad_2= max(bbb_yield_ad,1.1);
	
	POB_5 = min(pob, 5);
	POB_50= max(min(50, POB),5);
	POB_50max=max(POB, 50);

	dpd3059_0=(dpd3059=0);
	dpd0129_0= (DPD0129=0);
	naics_code_const= (naics in ('Waste Management', 'Construction'));
	naics_code_Retail= (naics in ('Accomodation & Food', 'Retail', 'Transportation'));
	naics_code_Manu= (naics in ('Manufacturing'));

	if q=1 then season='sp';
	else if q=2 then season='su';
	else if q=3 then season='fa';
	else if q=4 then season = 'wi';
run; 

data out.df_ci_model_vars_v5 ;
	set  out.df_ci_model_vars_v5 ;
	n=ranuni(8);
run;

proc sort data=out.df_ci_model_vars_v5;
  by n;
run; 

data out.training_ci_v5 out.testing_ci_v5;
   set out.df_ci_model_vars_v5 nobs=nobs;
   if _n_<=.6*nobs then output out.training_ci_v5;
    else output out.testing_ci_v5;
run;

proc LOGISTIC data = out.training_ci_v5 desc plots = all outmodel=out.model_ci_v5_training;
class   naics_code3 (ref='h') boh_rating1 (ref='R3') season (ref='sp');
	model y = 	boh_rating1
				naics_code3
				season
				CAUR_YD_Lag_3
				cahpi_ag_lag_3_n12
				dpd0129_0	POB_5 POB_50
	/selection =  stepwise;
	output out=out.outreg_ci_v5_training p=predicted;
run;

/* score the data */
proc logistic inmodel=out.model_ci_v5_training;
	score data=out.testing_ci_v5
 	out=out.outcomes_testing_ci_v5;
run;


/*get historical in sample */ 
proc sql; 
	create table out.mean_PD_ci_v5_hist_training as 
	select filedate,sum(current_balance*y)/sum(current_balance)  as W_ave_pd_mean_hist, 
					 sum(current_balance*predicted)/sum(current_balance) as W_ave_pd_mean_est,
					 mean(predicted) as mean_y
	from out.outreg_ci_v5_training
	group by filedate; 
quit; 

Proc sgplot data=out.mean_PD_ci_v5_hist_training;
	series x=filedate y= W_ave_pd_mean_est;
run;


proc export data=out.mean_PD_ci_v5_hist_training
   outfile='C:\Users\sghassemi\Documents\Projects\Bank of Hope\PD Models\PD C&I\mean_PD_Ci_in-sample_v5.csv'
   dbms=csv
   replace;
run;

/*testing sample*/
proc sql; 
	create table out.mean_PD_ci_v5_hist_testing as 
	select filedate, sum(current_balance*y)/sum(current_balance)  as W_ave_pd_mean_hist, 
					sum(current_balance*P_1)/sum(current_balance) as W_ave_pd_mean_est
	from out.outcomes_testing_ci_v5
	group by filedate; 
quit; 

Proc sgplot data=out.mean_PD_ci_v5_hist_testing;
	series x=filedate y= W_ave_pd_mean_est;
	/* band x=&var. lower=0 upper=_FREQ_ / fillattrs=ltgray y2axis transparency=0.5 legendlabel='Obs';*/
	run;


proc export data=out.mean_PD_ci_v5_hist_testing
   outfile='C:\Users\sghassemi\Documents\Projects\Bank of Hope\PD Models\PD C&I\mean_PD_Ci_historical_testing_v5.csv'
   dbms=csv
   replace;
run;




/*get the data for the dry run*/ 
/*Base*/ 
data out.ci_dryrun_base; 
 	set out.outcomes_base_ci_v5 (keep= filedate account_ID P_1); 
	if filedate<= '31Mar16'd then delete; 
	rename P_1=PD;
run; 


**2016Q2;
Data out.ci_dryrun_base_q1;
	set out.ci_dryrun_base(WHERE=(FILEDATE='01jUN16'd));
RUN;
	
proc export data= out.ci_dryrun_base_q1
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\ci_dryrun_base_q1.csv'
	dbms = csv
	REPLACE;
run; 

**2016Q3;
Data out.ci_dryrun_base_q2;
	set out.ci_dryrun_base(WHERE=(FILEDATE='01SEP16'd));
RUN;
	
proc export data= out.ci_dryrun_base_q2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\ci_dryrun_base_q2.csv'
	dbms = csv
	REPLACE;
run; 

**2016Q4;
Data out.ci_dryrun_base_q3;
	set out.ci_dryrun_base(WHERE=(FILEDATE='01DEC16'd));
RUN;
	
proc export data= out.ci_dryrun_base_q3
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\ci_dryrun_base_q3.csv'
	dbms = csv
	REPLACE;
run;

**2017Q1;
Data out.ci_dryrun_base_q4;
	set out.ci_dryrun_base(WHERE=(FILEDATE='01MAR17'd));
RUN;
	
proc export data= out.ci_dryrun_base_q4
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\ci_dryrun_base_q4.csv'
	dbms = csv
	REPLACE;
run;

**2017Q2;
Data out.ci_dryrun_base_q5;
	set out.ci_dryrun_base(WHERE=(FILEDATE='01JUN17'd));
RUN;
	
proc export data= out.ci_dryrun_base_q5
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\ci_dryrun_base_q5.csv'
	dbms = csv
	REPLACE;
run;

**2017Q3;
Data out.ci_dryrun_base_q6;
	set out.ci_dryrun_base(WHERE=(FILEDATE='01SEP17'd));
RUN;
	
proc export data= out.ci_dryrun_base_q6
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\ci_dryrun_base_q6.csv'
	dbms = csv
	REPLACE;
run;

**2017Q4;
Data out.ci_dryrun_base_q7;
	set out.ci_dryrun_base(WHERE=(FILEDATE='01DEC17'd));
RUN;
	
proc export data= out.ci_dryrun_base_q7
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\ci_dryrun_base_q7.csv'
	dbms = csv
	REPLACE;
run;

**2018Q1;
Data out.ci_dryrun_base_q8;
	set out.ci_dryrun_base(WHERE=(FILEDATE='01MAR18'd));
RUN;
	
proc export data= out.ci_dryrun_base_q8
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\ci_dryrun_base_q8.csv'
	dbms = csv
	REPLACE;
run;

**2018Q2;
Data out.ci_dryrun_base_q9;
	set out.ci_dryrun_base(WHERE=(FILEDATE='01JUN18'd));
RUN;
	
proc export data= out.ci_dryrun_base_q9
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\ci_dryrun_base_q9.csv'
	dbms = csv
	REPLACE;
run;

**********ADVERSE;
data out.ci_dryrun_adverse ; 
 	set out.outcomes_adverse_ci_v5 (keep= filedate account_ID P_1);
	if filedate<= '31Mar16'd then delete; 
	rename P_1 = PD;
run; 


**2016Q2;
Data out.ci_dryrun_adverse_q1;
	set out.ci_dryrun_adverse(WHERE=(FILEDATE='01jUN16'd));
RUN;
	
proc export data= out.ci_dryrun_adverse_q1
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\ci_dryrun_adverse_q1.csv'
	dbms = csv
	REPLACE;
run; 

**2016Q3;
Data out.ci_dryrun_adverse_q2;
	set out.ci_dryrun_adverse(WHERE=(FILEDATE='01SEP16'd));
RUN;
	
proc export data= out.ci_dryrun_adverse_q2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\ci_dryrun_adverse_q2.csv'
	dbms = csv
	REPLACE;
run; 

**2016Q4;
Data out.ci_dryrun_adverse_q3;
	set out.ci_dryrun_adverse(WHERE=(FILEDATE='01DEC16'd));
RUN;
	
proc export data= out.ci_dryrun_adverse_q3
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\ci_dryrun_adverse_q3.csv'
	dbms = csv
	REPLACE;
run;

**2017Q1;
Data out.ci_dryrun_adverse_q4;
	set out.ci_dryrun_adverse(WHERE=(FILEDATE='01MAR17'd));
RUN;
	
proc export data= out.ci_dryrun_adverse_q4
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\ci_dryrun_adverse_q4.csv'
	dbms = csv
	REPLACE;
run;

**2017Q2;
Data out.ci_dryrun_adverse_q5;
	set out.ci_dryrun_adverse(WHERE=(FILEDATE='01JUN17'd));
RUN;
	
proc export data= out.ci_dryrun_adverse_q5
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\ci_dryrun_adverse_q5.csv'
	dbms = csv
	REPLACE;
run;

**2017Q3;
Data out.ci_dryrun_adverse_q6;
	set out.ci_dryrun_adverse(WHERE=(FILEDATE='01SEP17'd));
RUN;
	
proc export data= out.ci_dryrun_adverse_q6
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\ci_dryrun_adverse_q6.csv'
	dbms = csv
	REPLACE;
run;

**2017Q4;
Data out.ci_dryrun_adverse_q7;
	set out.ci_dryrun_adverse(WHERE=(FILEDATE='01DEC17'd));
RUN;
	
proc export data= out.ci_dryrun_adverse_q7
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\ci_dryrun_adverse_q7.csv'
	dbms = csv
	REPLACE;
run;

**2018Q1;
Data out.ci_dryrun_adverse_q8;
	set out.ci_dryrun_adverse(WHERE=(FILEDATE='01MAR18'd));
RUN;
	
proc export data= out.ci_dryrun_adverse_q8
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\ci_dryrun_adverse_q8.csv'
	dbms = csv
	REPLACE;
run;

**2018Q2;
Data out.ci_dryrun_adverse_q9;
	set out.ci_dryrun_adverse(WHERE=(FILEDATE='01JUN18'd));
RUN;
	
proc export data= out.ci_dryrun_adverse_q9
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\ci_dryrun_adverse_q9.csv'
	dbms = csv
	REPLACE;
run;

**SEVERE;
data out.ci_dryrun_severe; 
 	set out.outcomes_severe_ci_v5 (keep= filedate account_ID P_1);
	if filedate<= '31Mar16'd then delete; 
	rename P_1 = PD;
run; 


**2016Q2;
Data out.ci_dryrun_severe_q1;
	set out.ci_dryrun_severe(WHERE=(FILEDATE='01jUN16'd));
RUN;
	
proc export data= out.ci_dryrun_severe_q1
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\ci_dryrun_severe_q1.csv'
	dbms = csv
	REPLACE;
run; 

**2016Q3;
Data out.ci_dryrun_severe_q2;
	set out.ci_dryrun_severe(WHERE=(FILEDATE='01SEP16'd));
RUN;
	
proc export data= out.ci_dryrun_severe_q2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\ci_dryrun_severe_q2.csv'
	dbms = csv
	REPLACE;
run; 

**2016Q4;
Data out.ci_dryrun_severe_q3;
	set out.ci_dryrun_severe(WHERE=(FILEDATE='01DEC16'd));
RUN;
	
proc export data= out.ci_dryrun_severe_q3
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\ci_dryrun_severe_q3.csv'
	dbms = csv
	REPLACE;
run;

**2017Q1;
Data out.ci_dryrun_severe_q4;
	set out.ci_dryrun_severe(WHERE=(FILEDATE='01MAR17'd));
RUN;
	
proc export data= out.ci_dryrun_severe_q4
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\ci_dryrun_severe_q4.csv'
	dbms = csv
	REPLACE;
run;

**2017Q2;
Data out.ci_dryrun_severe_q5;
	set out.ci_dryrun_severe(WHERE=(FILEDATE='01JUN17'd));
RUN;
	
proc export data= out.ci_dryrun_severe_q5
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\ci_dryrun_severe_q5.csv'
	dbms = csv
	REPLACE;
run;

**2017Q3;
Data out.ci_dryrun_severe_q6;
	set out.ci_dryrun_severe(WHERE=(FILEDATE='01SEP17'd));
RUN;
	
proc export data= out.ci_dryrun_severe_q6
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\ci_dryrun_severe_q6.csv'
	dbms = csv
	REPLACE;
run;

**2017Q4;
Data out.ci_dryrun_severe_q7;
	set out.ci_dryrun_severe(WHERE=(FILEDATE='01DEC17'd));
RUN;
	
proc export data= out.ci_dryrun_severe_q7
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\ci_dryrun_severe_q7.csv'
	dbms = csv
	REPLACE;
run;

**2018Q1;
Data out.ci_dryrun_severe_q8;
	set out.ci_dryrun_severe(WHERE=(FILEDATE='01MAR18'd));
RUN;
	
proc export data= out.ci_dryrun_severe_q8
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\ci_dryrun_severe_q8.csv'
	dbms = csv
	REPLACE;
run;

**2018Q2;
Data out.ci_dryrun_severe_q9;
	set out.ci_dryrun_severe(WHERE=(FILEDATE='01JUN18'd));
RUN;
	
proc export data= out.ci_dryrun_severe_q9
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun\ci_dryrun_severe_q9.csv'
	dbms = csv
	REPLACE;
run;


/* in-sample and out-of -sample testing */ 

%macro profile(var,format,xmin);
	Proc sort data=out.df_ci_model_vars_v5; 
	by &var.;
	run;
	Proc means data=out.df_ci_dev_orig nway noprint order=formatted ;
	var y;
	Class &var. ;
	format &var. &format..;
	output out=summary_stats mean(y)=default_rate;
	run;

	proc export data= summary_stats
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\PD Models\PD C&I\in and out back test\CI_summary_stats_profile.csv'
	dbms = csv
	REPLACE;

	Proc sgplot data=summary_stats;
	xaxis min=&xmin.;
	series x=&var. y=default_rate;
	band x=&var. lower=0 upper=_FREQ_ / fillattrs=ltgray y2axis transparency=0.5 legendlabel='Obs';
	run;
%mend;



%macro profile_in(var,format,xmin);
	Proc sort data=out.outreg_ci_v5_training; 
	by &var.;
	run;
	Proc means data=out.outreg_ci_v5_training nway noprint order=formatted ;
	var predicted;
	Class &var. ;
	format &var. &format..;
	output out=summary_stats mean(predicted)=default_rate;
	run;

	proc export data= summary_stats
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\PD Models\PD C&I\in and out back test\CI_summary_stats_insample.csv'
	dbms = csv
	REPLACE;

	Proc sgplot data=summary_stats;
	xaxis min=&xmin.;
	series x=&var. y=default_rate;
	band x=&var. lower=0 upper=_FREQ_ / fillattrs=ltgray y2axis transparency=0.5 legendlabel='Obs';
	run;
	%mend;


%macro profile_out(var,format,xmin);
	Proc sort data=out.outcomes_testing_ci_v5; 
	by &var.;
	run;
	Proc means data=out.outcomes_testing_ci_v5 nway noprint order=formatted ;
	var P_1;
	Class &var. ;
	format &var. &format..;
	output out=summary_stats mean(P_1)=default_rate;
	run;

	proc export data= summary_stats
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\PD Models\PD C&I\in and out back test\CI_summary_stats_outsample.csv'
	dbms = csv
	REPLACE;

	Proc sgplot data=summary_stats;
	xaxis min=&xmin.;
	series x=&var. y=default_rate;
	band x=&var. lower=0 upper=_FREQ_ / fillattrs=ltgray y2axis transparency=0.5 legendlabel='Obs';
	run;
	%mend;

/* rating*/ 
/*profile*/
Proc means data=out.df_ci_dev_orig nway noprint order=formatted ;
	var y;
	Class boh_rating;
	output out=summary_stats mean(y)=default_rate;
	run;

Proc sgplot data=summary_stats;

series x=boh_rating y=default_rate;
band x=boh_rating lower=0 upper=_FREQ_ / fillattrs=ltgray y2axis transparency=0.75 legendlabel='Obs';
run;

proc export data= summary_stats
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\PD Models\PD C&I\in and out back test\boh_rating_profile.csv'
	dbms = csv
	REPLACE;
run;

/*in-sample*/
Proc means data=out.outreg_ci_v5_training nway noprint order=formatted ;
	var predicted;
	Class boh_rating;
	output out=summary_stats mean(predicted)=default_rate;
	run;

Proc sgplot data=summary_stats;

series x=boh_rating y=default_rate;
band x=boh_rating lower=0 upper=_FREQ_ / fillattrs=ltgray y2axis transparency=0.75 legendlabel='Obs';
run;

proc export data= summary_stats
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\PD Models\PD C&I\in and out back test\boh_rating_in_sample.csv'
	dbms = csv
	REPLACE;
run;

/*out_of_sample*/
Proc means data= out.outcomes_testing_ci_v5 nway noprint order=formatted ;
	var P_1;
	Class boh_rating;
	output out=summary_stats mean(P_1)=default_rate;
	run;

Proc sgplot data=summary_stats;

series x=boh_rating y=default_rate;
band x=boh_rating lower=0 upper=_FREQ_ / fillattrs=ltgray y2axis transparency=0.75 legendlabel='Obs';
run;

proc export data= summary_stats
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\PD Models\PD C&I\in and out back test\boh_rating_ousample_summary_stats.csv'
	dbms = csv
	REPLACE;
run;
/* naics */ 
/*profile*/

Proc means data=out.df_ci_dev_orig nway noprint order=formatted ;
	var y;
	Class naics;
	output out=summary_stats mean(y)=default_rate;
	run;

Proc sgplot data=summary_stats;

series x=naics y=default_rate;
band x=naics lower=0 upper=_FREQ_ / fillattrs=ltgray y2axis transparency=0.75 legendlabel='Obs';
run;

proc export data= summary_stats
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\PD Models\PD C&I\in and out back test\naics_profile.csv'
	dbms = csv
	REPLACE;
run;

/*in-sample*/
Proc means data=out.outreg_ci_v5_training nway noprint order=formatted ;
	var predicted;
	Class naics;
	output out=summary_stats mean(predicted)=default_rate;
	run;

Proc sgplot data=summary_stats;

series x=naics y=default_rate;
band x=naics lower=0 upper=_FREQ_ / fillattrs=ltgray y2axis transparency=0.75 legendlabel='Obs';
run;

proc export data= summary_stats
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\PD Models\PD C&I\in and out back test\naics_in_sample.csv'
	dbms = csv
	REPLACE;
run;

/*out_of_sample*/
Proc means data= out.outcomes_testing_ci_v5 nway noprint order=formatted ;
	var P_1;
	Class naics;
	output out=summary_stats mean(P_1)=default_rate;
	run;

Proc sgplot data=summary_stats;

series x=naics y=default_rate;
band x=naics lower=0 upper=_FREQ_ / fillattrs=ltgray y2axis transparency=0.75 legendlabel='Obs';
run;

proc export data= summary_stats
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\PD Models\PD C&I\in and out back test\naics_outsample_summary_stats.csv'
	dbms = csv
	REPLACE;
run;

/* naics_code3 */ 

Proc means data=out.df_ci_model_vars_v5 nway noprint order=formatted ;
	var y;
	Class naics_code3;
	output out=summary_stats mean(y)=default_rate;
	run;

Proc sgplot data=summary_stats;

series x=naics_code3 y=default_rate;
band x=naics_code2 lower=0 upper=_FREQ_ / fillattrs=ltgray y2axis transparency=0.75 legendlabel='Obs';
run;

proc export data= summary_stats
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\PD Models\PD C&I\in and out back test\naics_code2_profile.csv'
	dbms = csv
	REPLACE;
run;

/*in-sample*/
Proc means data=out.outreg_ci_v5_training nway noprint order=formatted ;
	var predicted;
	Class naics_code3;
	output out=summary_stats mean(predicted)=default_rate;
	run;

Proc sgplot data=summary_stats;

series x=naics_code3 y=default_rate;
band x=naics_code2 lower=0 upper=_FREQ_ / fillattrs=ltgray y2axis transparency=0.75 legendlabel='Obs';
run;

proc export data= summary_stats
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\PD Models\PD C&I\in and out back test\naics_code2_in_sample.csv'
	dbms = csv
	REPLACE;
run;

/*out_of_sample*/
Proc means data= out.outcomes_testing_ci_v5 nway noprint order=formatted ;
	var P_1;
	Class naics_code3;
	output out=summary_stats mean(P_1)=default_rate;
	run;

Proc sgplot data=summary_stats;

series x=naics_code3 y=default_rate;
band x=naics_code2 lower=0 upper=_FREQ_ / fillattrs=ltgray y2axis transparency=0.75 legendlabel='Obs';
run;

proc export data= summary_stats
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\PD Models\PD C&I\in and out back test\naics_code2_outsample_summary_stats.csv'
	dbms = csv
	REPLACE;
run;

/*q*/
/*profile*/

Proc means data=out.df_ci_dev_orig nway noprint order=formatted ;
	var y;
	Class q;
	output out=summary_stats mean(y)=default_rate;
	run;

Proc sgplot data=summary_stats;

series x=q y=default_rate;
band x=q lower=0 upper=_FREQ_ / fillattrs=ltgray y2axis transparency=0.75 legendlabel='Obs';
run;

proc export data= summary_stats
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\PD Models\PD C&I\in and out back test\season_profile.csv'
	dbms = csv
	REPLACE;
run;

/*in-sample*/
Proc means data=out.outreg_ci_v5_training nway noprint order=formatted ;
	var predicted;
	Class season;
	output out=summary_stats mean(predicted)=default_rate;
run;

Proc sgplot data=summary_stats;

series x=season y=default_rate;
band x=season lower=0 upper=_FREQ_ / fillattrs=ltgray y2axis transparency=0.75 legendlabel='Obs';
run;

proc export data= summary_stats
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\PD Models\PD C&I\in and out back test\season_in_sample.csv'
	dbms = csv
	REPLACE;
run;

/*out_of_sample*/
Proc means data= out.outcomes_testing_ci_v5 nway noprint order=formatted ;
	var P_1;
	Class season;
	output out=summary_stats mean(P_1)=default_rate;
	run;

Proc sgplot data=summary_stats;

series x=season y=default_rate;
band x=season lower=0 upper=_FREQ_ / fillattrs=ltgray y2axis transparency=0.75 legendlabel='Obs';
run;

proc export data= summary_stats
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\PD Models\PD C&I\in and out back test\season_outsample_summary_stats.csv'
	dbms = csv
	REPLACE;
run;

/*POB*/
Proc format;
value POBf		 	 	Low-5= 'a'
						5<-10 = 'b'
						10<-20 = 'c'
						20<-30 = 'c1'
						30<-35 ='d'
						35<-50= 'e'
						50<-60= 'f'
						60<-70= 'g'
						70<-80= 'h'
						80<-90= 'i'
						90-High = 'j'
	;
%profile(pob,pobf,0)

%profile_in(POB,POBf,0);

%profile_out(POB,POBf,0);

/*DPD0129*/ 
Proc format; 
	value dpd01f			Low-0  = '0'
							1<-3	='1-3'
							/*1<-2	='2'*/	
							3<-6 = '4-6'
							6-High = '6-Max'
	;
%profile(dpd0129,dpd01f,0);
%profile_in(dpd0129,dpd01f,0);
%profile_out(dpd0129,dpd01f,0);


/* dpd0129_0 */ 
/*profile*/
Proc means data=out.df_ci_model_vars_v5 nway noprint order=formatted ;
	var y;						
	Class dpd0129_0;
	output out=summary_stats mean(y)=default_rate;
	run;

Proc sgplot data=summary_stats;

series x=dpd0129_0 y=default_rate;
band x=dpd0129_0 lower=0 upper=_FREQ_ / fillattrs=ltgray y2axis transparency=0.75 legendlabel='Obs';
run;

proc export data= summary_stats
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\PD Models\PD C&I\in and out back test\dpd0129_0_profile.csv'
	dbms = csv
	REPLACE;
run;

/*in-sample*/
Proc means data=out.outreg_ci_v5_training nway noprint order=formatted ;
	var predicted;
	Class dpd0129_0;
	output out=summary_stats mean(predicted)=default_rate;
	run;

Proc sgplot data=summary_stats;

series x=dpd0129_0 y=default_rate;
band x=dpd0129_0 lower=0 upper=_FREQ_ / fillattrs=ltgray y2axis transparency=0.75 legendlabel='Obs';
run;

proc export data= summary_stats
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\PD Models\PD C&I\in and out back test\dpd0129_0_in_sample.csv'
	dbms = csv
	REPLACE;
run;

/*out_of_sample*/
Proc means data= out.outcomes_testing_ci_v5 nway noprint order=formatted ;
	var P_1;
	Class dpd0129_0;
	output out=summary_stats mean(P_1)=default_rate;
	run;

Proc sgplot data=summary_stats;

series x=dpd0129_0 y=default_rate;
band x=dpd0129_0 lower=0 upper=_FREQ_ / fillattrs=ltgray y2axis transparency=0.75 legendlabel='Obs';
run;

proc export data= summary_stats
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\PD Models\PD C&I\in and out back test\dpd0129_0_outsample_summary_stats.csv'
	dbms = csv
	REPLACE;
run;


/*CAhpi_ag_lag_3*/
	Proc format; 
		value cahpi_agf				Low - -24 = 'a'
									-24<--15 = 'a1'
									-15<--12 = 'a2'
									-12<--5= 'a3'
									-5<--2 = 'b'
									-2<--0 = 'd'
									0<-1 = 'e'
									1<-3 = 'f'
									3<-5 = 'g'
									5<-7 = 'h'
									7<-10 = 'i'
									10-High = 'j'
		;
	%profile(cahpi_ag_lag_3,cahpi_agf,-30);
%profile_in(cahpi_ag_lag_3,cahpi_agf,-30);
%profile_out(cahpi_ag_lag_3,cahpi_agf,-30);

/*CAUR_YD*/

Proc format; 
	value CAur_ydf			Low - -1.25 = 'L1: Low- (-1.25)'
						-1.25<--0.75 = 'L2: (-1.25)-(-0.75)'
						-0.75<-0 = 'L3: (-0.75)-0'
						0<-1 ='L4: 0-1'
						1<-2 = 'L5: 1-2'
						2<-3 = 'L6: 2-3'
						3-High = 'L7: 3-Max'
	;
%profile(CAur_yd_lag_3,CAur_ydf,-2);
%profile_in(CAur_yd_lag_3,CAur_ydf,-2);
%profile_out(CAur_yd_lag_3,CAur_ydf,-2);

/*ROC Curves*/ 
Data out.CI_ROC_insample; 
	set out.outreg_ci_v5_training (keep= filedate account_id y predicted); 
run; 

proc sort data=out.CI_ROC_insample out=out.CI_ROC_insample; 
	by descending predicted; 
run; 

Data out.CI_ROC_outofsample; 
	set out.outcomes_testing_ci_v5 (keep= filedate account_id y P_1); 
run; 

proc sort data=out.CI_ROC_outofsample out=out.CI_ROC_outofsample; 
	by descending P_1; 
run; 

proc export data= out.CI_ROC_insample
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\PD Models\PD C&I\in and out back test\ROC_in_sample.csv'
	dbms = csv
	REPLACE;
run;

proc export data= out.CI_ROC_outofsample
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\PD Models\PD C&I\in and out back test\ROC_out_sample.csv'
	dbms = csv
	REPLACE;
run;
