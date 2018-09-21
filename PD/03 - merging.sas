/* clear the log window*/
dm 'log' clear; 
LIBNAME Interim "";
LIBNAME Out "";
LIBNAME BBCN "";


data interim.df_boh_merged2; 
	set interim.df_final_wilshire2 interim.df_final_bbcn2;
run;

data interim.df_boh_merged2; 
	retain filedate account_id boh_id acquired_identifier
			portfolio_id original_balance origination_date maturity_date
			current_balance interest_rate interest_rate_type loan_spread_v
			loan_age_q ttm_q POB term boh_rating dcr property_descr collateral_descr collateral_value org_ltv
			dpd90 dpd0129 dpd3059 dpd6089 first_nonacc_date collateral_descr
			naics property_descr tb1m tb3m tb6m tb1y tb2y
			tb3y tb5y tb7y tb10y tb20y tb30y year q month y;
	set interim.df_boh_merged2 (drop= ttm_m);
run;

data interim.df_boh_merged2;
	set interim.df_boh_merged2; 
	format property_descr2 $char40.; 
	if property_descr in ('1-4 Residential' , '1-4 Res') then property_descr2 = '1-4 Residential'; 
	else if property_descr in ('Church / religous facility', 'Church/Religious') then property_descr2 = 'Church/Religious';
	else if property_descr in ('Auto repair shop', 'Auto Repair') then property_descr2 = 'Auto repair shop';
	else if property_descr in ('Carwash') then property_descr2 = 'Carwash';
	else if property_descr in ('Gas Station', 'Gas station') then property_descr2 = 'Gas station';
	else if property_descr in ('Restaurant', 'Restaurants') then property_descr2 = 'Restaurant';
	else if property_descr in ('Hotel and motel: flag', 'Hotel and motel: non-flag', 'Hotel/Motel') then property_descr2 = 'Hotel/Motel';
	else if property_descr in ('Mixed-use commercial/multifamily', 'Mixed-use other', 'Mixed-use: Comm/Res') then property_descr2 = 'Mixed-use: comm/res';
	else if property_descr in ('Land leased', 'Land vacant developed',  'Land vacant raw', 'Land') then property_descr2 = 'Land';
	else if property_descr in ('Golf course') then property_descr2 = 'Golf course';
	else if property_descr in ('Retail shopping','Retail Single Tenant', 'Retail multiple Tenant','Outlet mall',
								'Regional mall') then property_descr2 = 'Retail shopping center';
	else if property_descr in ('Office','Office Condo', 'Mixed-use office/retail') then property_descr2 = 'Office';
	else if property_descr in ('Industrial warehouse single Tenant','Industrial warehouse multiple Tenant', 
								'Industrial manufacturing','Industrial warehouse/condo', 
								'Commercial/Industrial', 'Recycling') then property_descr2 = 'Commercial-industrial';
	else if property_descr in ('Office Medical/Dental', 'Convalescent Facility', 'Medical') then property_descr2 = 'Medical';
	else if property_descr in ('5+ Residential', 'Multifamily Res') then property_descr2 = 'Multifamily Residential';
	else if property_descr in ('All other single tenant', 'self-storage','Mobile home/ RV park',
								'Health clubs', 'Private schools') then property_descr2 = 'Other: single tenant';
	else if property_descr in ('Other', 'Other real property') then property_descr2 = 'Other';
	else if property_descr in ('Null', 'None') then property_descr2 = 'None';
	else if property_descr in ('missing', 'Missing') then property_descr2 = 'Missing';
run; 

PROC SQL; 
	CREATE TABLE TEST AS SELECT DISTINCT PROPERTY_DESCR2, COUNT(*) FROM interim.df_boh_merged
	GROUP BY 1; 
QUIT; 

PROC SQL; 
	CREATE TABLE TEST AS SELECT DISTINCT NAICS, COUNT(*) FROM interim.df_boh_merged
	GROUP BY 1; 
QUIT; 

/*
*Accounts with zero Risk Rating;
PROC SQL; 
	CREATE TABLE INTERIM.RISK_RATING_ZERO AS
	SELECT ACCOUNT_ID, PORTFOLIO_ID, boh_rating
	FROM interim.df_boh_merged
	WHERE BOH_RATING=0
	AND CURRENT_BALANCE>1000; 
QUIT; 

PROC SQL; 
	CREATE TABLE ACCOUNTS_ZERO_RATING AS 
	SELECT DISTINCT ACCOUNT_ID 
	FROM INTERIM.RISK_RATING_ZERO;
QUIT;  

PROC SQL; 
	CREATE TABLE ACCOUNTS_ZERO_RATING2 AS 
	SELECT A.ACCOUNT_ID, B.CLASS_CODE
	FROM 
	ACCOUNTS_ZERO_RATING AS A
	LEFT JOIN 
	WILSHIRE AS B
	on a.ACCOUNT_ID=b.note_number;
quit;

proc sort data=ACCOUNTS_ZERO_RATING2 out=ACCOUNTS_ZERO_RATING3;
   by ACCOUNT_ID class_code;
run;
proc sort data=ACCOUNTS_ZERO_RATING3 nodupkey;
   by ACCOUNT_ID class_code;
run;

data ACCOUNTS_ZERO_RATING3; 
	set ACCOUNTS_ZERO_RATING3; 
	if class_code in (94,99,49) then delete; 
run; 
 
proc sql; 
	create table zero_ratinf_stat as 
	select distinct class_code, count(distinct account_id) as cnt
	from ACCOUNTS_ZERO_RATING3
	group by 1;
quit; 

proc export data=ACCOUNTS_ZERO_RATING3
   outfile='C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\boh_accounts_with_zero_rating.csv'
   dbms=csv
   replace;
run;

proc export data=zero_ratinf_stat
   outfile='C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\boh_accounts_with_zero_rating_stat.csv'
   dbms=csv
   replace;
run;

*/

********************************;
*expanding the data to 2018Q2
********************************;
data indx; 
	set interim.df_boh_merged2 (where=(filedate='31Mar16'd));
run; 

data indx2; 
	set indx (where=(maturity_date>'31Mar16'd));
	payment= mort(current_balance, . , interest_rate/4, ttm_q);
	cnt=intck('Quarter',  '31Mar16'd, maturity_date); 
	if cnt>9 then cnt=9;
run; 


data indx3; 
	set indx2; 
	do n = 1 to cnt;
	   output;
	end;
run; 

data indx3; 
	set indx3; 
	format new_date MMDDYY10.;
	new_date= intnx('month',filedate,n*3);
	new_bal= current_balance - n*payment;
	new_ttm_q= intck('Quarter',  new_date, maturity_date);
	new_loan_age_q= intck('Quarter', origination_date, new_date);
	new_POB= 100*new_loan_age_q/term_q;
run;

data indx4; 
	set indx3(drop= filedate  current_balance ttm_q loan_age_q POB);
	if new_bal<0 then new_bal=0;
run;

data indx5 (drop=mn_file);  
	set indx4 (drop=tb1m tb3m tb6m tb1y tb2y tb3y tb5y tb7y tb10y tb20y tb30y);
	rename  new_date = filedate; 
	rename new_bal= current_balance; 
	rename new_ttm_q = ttm_q;
	rename new_loan_age_q = loan_age_q;
	rename new_pob = pob; 
run; 

data interim.df_boh_expanded18 (drop=payment cnt n mn_file);
	set indx5;
	year=year(filedate);
	mn_file= month(filedate);
	if mn_file in (1,2,3) then q=1;
	else if mn_file in (4,5,6) then q=2;
	else if mn_file in (7,8,9) then q=3;
	else if mn_file in (10,11,12) then q=4; 
run;

/* add this to the original file*/ 
data out.df_boh_2018; 
 set interim.df_boh_merged2 interim.df_boh_expanded18;
run; 

/*baseline*/

/* Input the macro data */
proc import out = Interim.macro_base
            datafile = "~\macro_base.csv" 
            dbms = csv replace;
run;

proc sql; 
	create table out.df_boh_final_base
	as select * from
	out.df_boh_2018 as a
	left join
	Interim.macro_base as b
	on a.year=b.year
	and a.q = b.quarter;
quit; 

/*adverse*/

/* Input the macro data */
proc import out = Interim.macro_adverse
            datafile = "~\macro_adverse.csv" 
            dbms = csv replace;
run;

proc sql; 
	create table out.df_boh_final_adverse
	as select * from
	out.df_boh_2018 as a
	left join
	 Interim.macro_adverse as b
	on a.year=b.year
	and a.q = b.quarter;
quit; 

/*severe*/
proc import out = Interim.macro_severe
            datafile = "~\macro_severe.csv" 
            dbms = csv replace;
run;

proc sql; 
	create table out.df_boh_final_severe
	as select * from
	out.df_boh_2018 as a
	left join
	Interim.macro_severe as b
	on a.year=b.year
	and a.q = b.quarter;
quit; 


/*Cre and C&I files*/

data out.df_boh_cre_;
	set out.df_boh_final_base (where=(portfolio_id="CRE"));
run;


data out.df_boh_ci;
	set out.df_boh_final (where=(portfolio_id="CI"));
run;
/* clear the log window*/
dm 'log' clear; 
LIBNAME Interim "";
LIBNAME Out "";
LIBNAME BBCN "";


data interim.df_boh_merged2; 
	set interim.df_final_wilshire2 interim.df_final_bbcn2;
run;

data interim.df_boh_merged2; 
	retain filedate account_id boh_id acquired_identifier
			portfolio_id original_balance origination_date maturity_date
			current_balance interest_rate interest_rate_type loan_spread_v
			loan_age_q ttm_q POB term boh_rating dcr property_descr collateral_descr collateral_value org_ltv
			dpd90 dpd0129 dpd3059 dpd6089 first_nonacc_date collateral_descr
			naics property_descr tb1m tb3m tb6m tb1y tb2y
			tb3y tb5y tb7y tb10y tb20y tb30y year q month y;
	set interim.df_boh_merged2 (drop= ttm_m);
run;

data interim.df_boh_merged2;
	set interim.df_boh_merged2; 
	format property_descr2 $char40.; 
	if property_descr in ('1-4 Residential' , '1-4 Res') then property_descr2 = '1-4 Residential'; 
	else if property_descr in ('Church / religous facility', 'Church/Religious') then property_descr2 = 'Church/Religious';
	else if property_descr in ('Auto repair shop', 'Auto Repair') then property_descr2 = 'Auto repair shop';
	else if property_descr in ('Carwash') then property_descr2 = 'Carwash';
	else if property_descr in ('Gas Station', 'Gas station') then property_descr2 = 'Gas station';
	else if property_descr in ('Restaurant', 'Restaurants') then property_descr2 = 'Restaurant';
	else if property_descr in ('Hotel and motel: flag', 'Hotel and motel: non-flag', 'Hotel/Motel') then property_descr2 = 'Hotel/Motel';
	else if property_descr in ('Mixed-use commercial/multifamily', 'Mixed-use other', 'Mixed-use: Comm/Res') then property_descr2 = 'Mixed-use: comm/res';
	else if property_descr in ('Land leased', 'Land vacant developed',  'Land vacant raw', 'Land') then property_descr2 = 'Land';
	else if property_descr in ('Golf course') then property_descr2 = 'Golf course';
	else if property_descr in ('Retail shopping','Retail Single Tenant', 'Retail multiple Tenant','Outlet mall',
								'Regional mall') then property_descr2 = 'Retail shopping center';
	else if property_descr in ('Office','Office Condo', 'Mixed-use office/retail') then property_descr2 = 'Office';
	else if property_descr in ('Industrial warehouse single Tenant','Industrial warehouse multiple Tenant', 
								'Industrial manufacturing','Industrial warehouse/condo', 
								'Commercial/Industrial', 'Recycling') then property_descr2 = 'Commercial-industrial';
	else if property_descr in ('Office Medical/Dental', 'Convalescent Facility', 'Medical') then property_descr2 = 'Medical';
	else if property_descr in ('5+ Residential', 'Multifamily Res') then property_descr2 = 'Multifamily Residential';
	else if property_descr in ('All other single tenant', 'self-storage','Mobile home/ RV park',
								'Health clubs', 'Private schools') then property_descr2 = 'Other: single tenant';
	else if property_descr in ('Other', 'Other real property') then property_descr2 = 'Other';
	else if property_descr in ('Null', 'None') then property_descr2 = 'None';
	else if property_descr in ('missing', 'Missing') then property_descr2 = 'Missing';
run; 

PROC SQL; 
	CREATE TABLE TEST AS SELECT DISTINCT PROPERTY_DESCR2, COUNT(*) FROM interim.df_boh_merged
	GROUP BY 1; 
QUIT; 

PROC SQL; 
	CREATE TABLE TEST AS SELECT DISTINCT NAICS, COUNT(*) FROM interim.df_boh_merged
	GROUP BY 1; 
QUIT; 

/*
*Accounts with zero Risk Rating;
PROC SQL; 
	CREATE TABLE INTERIM.RISK_RATING_ZERO AS
	SELECT ACCOUNT_ID, PORTFOLIO_ID, boh_rating
	FROM interim.df_boh_merged
	WHERE BOH_RATING=0
	AND CURRENT_BALANCE>1000; 
QUIT; 

PROC SQL; 
	CREATE TABLE ACCOUNTS_ZERO_RATING AS 
	SELECT DISTINCT ACCOUNT_ID 
	FROM INTERIM.RISK_RATING_ZERO;
QUIT;  

PROC SQL; 
	CREATE TABLE ACCOUNTS_ZERO_RATING2 AS 
	SELECT A.ACCOUNT_ID, B.CLASS_CODE
	FROM 
	ACCOUNTS_ZERO_RATING AS A
	LEFT JOIN 
	WILSHIRE AS B
	on a.ACCOUNT_ID=b.note_number;
quit;

proc sort data=ACCOUNTS_ZERO_RATING2 out=ACCOUNTS_ZERO_RATING3;
   by ACCOUNT_ID class_code;
run;
proc sort data=ACCOUNTS_ZERO_RATING3 nodupkey;
   by ACCOUNT_ID class_code;
run;

data ACCOUNTS_ZERO_RATING3; 
	set ACCOUNTS_ZERO_RATING3; 
	if class_code in (94,99,49) then delete; 
run; 
 
proc sql; 
	create table zero_ratinf_stat as 
	select distinct class_code, count(distinct account_id) as cnt
	from ACCOUNTS_ZERO_RATING3
	group by 1;
quit; 

proc export data=ACCOUNTS_ZERO_RATING3
   outfile='C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\boh_accounts_with_zero_rating.csv'
   dbms=csv
   replace;
run;

proc export data=zero_ratinf_stat
   outfile='C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\boh_accounts_with_zero_rating_stat.csv'
   dbms=csv
   replace;
run;

*/

********************************;
*expanding the data to 2018Q2
********************************;
data indx; 
	set interim.df_boh_merged2 (where=(filedate='31Mar16'd));
run; 

data indx2; 
	set indx (where=(maturity_date>'31Mar16'd));
	payment= mort(current_balance, . , interest_rate/4, ttm_q);
	cnt=intck('Quarter',  '31Mar16'd, maturity_date); 
	if cnt>9 then cnt=9;
run; 


data indx3; 
	set indx2; 
	do n = 1 to cnt;
	   output;
	end;
run; 

data indx3; 
	set indx3; 
	format new_date MMDDYY10.;
	new_date= intnx('month',filedate,n*3);
	new_bal= current_balance - n*payment;
	new_ttm_q= intck('Quarter',  new_date, maturity_date);
	new_loan_age_q= intck('Quarter', origination_date, new_date);
	new_POB= 100*new_loan_age_q/term_q;
run;

data indx4; 
	set indx3(drop= filedate  current_balance ttm_q loan_age_q POB);
	if new_bal<0 then new_bal=0;
run;

data indx5 (drop=mn_file);  
	set indx4 (drop=tb1m tb3m tb6m tb1y tb2y tb3y tb5y tb7y tb10y tb20y tb30y);
	rename  new_date = filedate; 
	rename new_bal= current_balance; 
	rename new_ttm_q = ttm_q;
	rename new_loan_age_q = loan_age_q;
	rename new_pob = pob; 
run; 

data interim.df_boh_expanded18 (drop=payment cnt n mn_file);
	set indx5;
	year=year(filedate);
	mn_file= month(filedate);
	if mn_file in (1,2,3) then q=1;
	else if mn_file in (4,5,6) then q=2;
	else if mn_file in (7,8,9) then q=3;
	else if mn_file in (10,11,12) then q=4; 
run;

/* add this to the original file*/ 
data out.df_boh_2018; 
 set interim.df_boh_merged2 interim.df_boh_expanded18;
run; 

/*baseline*/

/* Input the macro data */
proc import out = Interim.macro_base
            datafile = "~\macro_base.csv" 
            dbms = csv replace;
run;

proc sql; 
	create table out.df_boh_final_base
	as select * from
	out.df_boh_2018 as a
	left join
	Interim.macro_base as b
	on a.year=b.year
	and a.q = b.quarter;
quit; 

/*adverse*/

/* Input the macro data */
proc import out = Interim.macro_adverse
            datafile = "~\macro_adverse.csv" 
            dbms = csv replace;
run;

proc sql; 
	create table out.df_boh_final_adverse
	as select * from
	out.df_boh_2018 as a
	left join
	 Interim.macro_adverse as b
	on a.year=b.year
	and a.q = b.quarter;
quit; 

/*severe*/
proc import out = Interim.macro_severe
            datafile = "~\macro_severe.csv" 
            dbms = csv replace;
run;

proc sql; 
	create table out.df_boh_final_severe
	as select * from
	out.df_boh_2018 as a
	left join
	Interim.macro_severe as b
	on a.year=b.year
	and a.q = b.quarter;
quit; 


/*Cre and C&I files*/

data out.df_boh_cre_;
	set out.df_boh_final_base (where=(portfolio_id="CRE"));
run;


data out.df_boh_ci;
	set out.df_boh_final (where=(portfolio_id="CI"));
run;
