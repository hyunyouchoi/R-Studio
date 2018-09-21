/* clear the log window*/
dm 'log' clear; 
LIBNAME Interim "";
LIBNAME Out "";

/* CRE */ 
/* For CRE model we need the following attributes: 
1- boh rating 
2- property type 
3- POB.
*/

proc contents data=out.df_cre_dev out=meta (keep=NAME) ; 
run ; 
proc print data=meta ; run ;

data interim.superloans_cre; 
	set out.df_cre_dev; 
	if origination_date<'31Mar2014'd then delete; 
run; 

/* rating*/ 
proc sql; 
	create table test as 
	select distinct boh_rating, count(distinct account_id)
	from interim.superloans_cre
	group 1; 
run;

proc univariate data=interim.superloans_cre modes;
          var boh_rating;
          output out=test2 mode=mode;
run;

/* property type */ 
proc sql; 
	create table test as 
	select distinct property_descr2, count(distinct account_id)
	from interim.superloans_cre
	group 1; 
run;
* the mode is the retail *; 

proc sql; 
	create table test as 
	select distinct pob, count(distinct account_id)
	from interim.superloans_cre
	group 1; 
run;
* the mode is the retail *; 
Data super_loan_base; 
	set out.outcomes_base_cre_dec13; 
	if filedate<='30Sep2016'd then delete; 
	if prop_retail=0 then delete; 
	if boh_rating1 ne 'R	1' then delete; 
	if POB_95 >95 then delete; 
run; 

Proc sql; 
	create table interim.CRE_superloans as 
	select distinct filedate, prop_retail,boh_rating1,POB_95,
		P_1, P_0 
	from out.outcomes_severe_cre_dec13
	where filedate>'30Sep2016'd
	and prop_retail=1
	and boh_rating1='R2'
	and POB_95=95
	group by 1;
quit;

 
/* CI */ 
/* For CI model we need the following attributes: 
1- boh rating 
2- naics code 
*/

proc contents data=out.df_ci_dev out=meta (keep=NAME) ; 
run ; 
proc print data=meta ; run ;

data interim.superloans_ci; 
	set out.outcomes_base_cI_dec13; 
	if filedate<'30Sep2014'd then delete;
	if boh_rating ne 3 then delete;
	if naics ne 'Wholesale Trade' then delete; 
run; 

/* rating*/ 
proc sql; 
	create table test as 
	select distinct boh_rating, count(distinct account_id)
	from interim.superloans_ci
	group 1; 
run; 
/* mode is 3*/ 

proc univariate data=interim.superloans_ci modes;
          var boh_rating;
          output out=test2 mode=mode;
run;

/* naices_code */ 
proc sql; 
	create table test as 
	select distinct naics, count(distinct account_id)
	from interim.superloans_ci
	group 1; 
run;
* the mode is Wholesale Trade *; 

/* for POB we have to create 9 loans here*/ 
data get1; 
	set interim.superloans_ci(where=(account_id=81544803));
run; 
Data sl1 (drop=origination_date loan_age P_0 P_1 I_y F_y); 
	set get1; 
	account_id =1; 
	origination_date= '15Nov2016'd;
	loan_age= intck('Quarter', origination_date , fileDate);
	pob=100*loan_age/12; 
	POB_5 = max(min(pob, 5),0);
	POB_50= max(min(50, POB),5);
run;

Data sl2 (drop=origination_date loan_age P_0 P_1 I_y F_y); 
	set get1; 
	account_id =2; 
	if filedate<'01Jan2017'd then delete;
	origination_date= '15Feb2017'd;
	loan_age= intck('Quarter', origination_date , fileDate);
	pob=100*loan_age/12; 
	POB_5 = max(min(pob, 5),0);
	POB_50= max(min(50, POB),5);
run;

Data sl3 (drop=origination_date loan_age P_0 P_1 I_y F_y); 
	set get1; 
	account_id =3; 
	if filedate<'01Apr2017'd then delete;
	origination_date= '15May2017'd;
	loan_age= intck('Quarter', origination_date , fileDate);
	pob=100*loan_age/12; 
	POB_5 = max(min(pob, 5),0);
	POB_50= max(min(50, POB),5);
run;

Data sl4 (drop=origination_date loan_age P_0 P_1 I_y F_y); 
	set get1; 
	account_id =4; 
	if filedate<'01Jul2017'd then delete;
	origination_date= '15Aug2017'd;
	loan_age= intck('Quarter', origination_date , fileDate);
	pob=100*loan_age/12; 
	POB_5 = max(min(pob, 5),0);
	POB_50= max(min(50, POB),5);
run;

Data sl5 (drop=origination_date loan_age P_0 P_1 I_y F_y); 
	set get1; 
	account_id =5; 
	if filedate<'01oct2017'd then delete;
	origination_date= '15nov2017'd;
	loan_age= intck('Quarter', origination_date , fileDate);
	pob=100*loan_age/12; 
	POB_5 = max(min(pob, 5),0);
	POB_50= max(min(50, POB),5);
run;

Data sl6 (drop=origination_date loan_age P_0 P_1 I_y F_y); 
	set get1; 
	account_id =6; 
	if filedate<'01jan2018'd then delete;
	origination_date= '15feb2018'd;
	loan_age= intck('Quarter', origination_date , fileDate);
	pob=100*loan_age/12; 
	POB_5 = max(min(pob, 5),0);
	POB_50= max(min(50, POB),5);
run;

Data sl7 (drop=origination_date loan_age P_0 P_1 I_y F_y); 
	set get1; 
	account_id =7; 
	if filedate<'01apr2018'd then delete;
	origination_date='15May2018'd;
	loan_age= intck('Quarter', origination_date , fileDate);
	pob=100*loan_age/12; 
	POB_5 = max(min(pob, 5),0);
	POB_50= max(min(50, POB),5);
run;

Data sl8 (drop=origination_date loan_age P_0 P_1 I_y F_y); 
	set get1; 
	account_id =8; 
	if filedate<'01Jul2018'd then delete;
	origination_date='15Aug2018'd;
	loan_age= intck('Quarter', origination_date , fileDate);
	pob=100*loan_age/12; 
	POB_5 = max(min(pob, 5),0);
	POB_50= max(min(50, POB),5);
run;

Data sl9 (drop=origination_date loan_age P_0 P_1 I_y F_y); 
	set get1; 
	account_id =9; 
	if filedate<'01Oct2018'd then delete;
	origination_date='15Nov2018'd;
	loan_age= intck('Quarter', origination_date , fileDate);
	pob=100*loan_age/12; 
	POB_5 = max(min(pob, 5),0);
	POB_50= max(min(50, POB),5);
run;

data interim.ci_superloans_base; 
	set sl1 sl2 sl3 sl4 sl5 sl6 sl7 sl8 sl9; 
run; 

proc logistic inmodel=out.model_ci_v5_training;
	score data=interim.ci_superloans_base
 	out=out.outcomes_base_ci_superloans;
run;

/*Adverse*/ 
data interim.superloans_ci; 
	set out.outcomes_adverse_cI_dec13; 
	if filedate<'30Sep2014'd then delete;
	if boh_rating ne 3 then delete;
	if naics ne 'Wholesale Trade' then delete; 
run; 

data get1; 
	set interim.superloans_ci(where=(account_id=81544803));
run; 
Data sl1 (drop=origination_date loan_age P_0 P_1 I_y F_y); 
	set get1; 
	account_id =1; 
	origination_date= '15Nov2016'd;
	loan_age= intck('Quarter', origination_date , fileDate);
	pob=100*loan_age/12; 
	POB_5 = max(min(pob, 5),0);
	POB_50= max(min(50, POB),5);
run;

Data sl2 (drop=origination_date loan_age P_0 P_1 I_y F_y); 
	set get1; 
	account_id =2; 
	if filedate<'01Jan2017'd then delete;
	origination_date= '15Feb2017'd;
	loan_age= intck('Quarter', origination_date , fileDate);
	pob=100*loan_age/12; 
	POB_5 = max(min(pob, 5),0);
	POB_50= max(min(50, POB),5);
run;

Data sl3 (drop=origination_date loan_age P_0 P_1 I_y F_y); 
	set get1; 
	account_id =3; 
	if filedate<'01Apr2017'd then delete;
	origination_date= '15May2017'd;
	loan_age= intck('Quarter', origination_date , fileDate);
	pob=100*loan_age/12; 
	POB_5 = max(min(pob, 5),0);
	POB_50= max(min(50, POB),5);
run;

Data sl4 (drop=origination_date loan_age P_0 P_1 I_y F_y); 
	set get1; 
	account_id =4; 
	if filedate<'01Jul2017'd then delete;
	origination_date= '15Aug2017'd;
	loan_age= intck('Quarter', origination_date , fileDate);
	pob=100*loan_age/12; 
	POB_5 = max(min(pob, 5),0);
	POB_50= max(min(50, POB),5);
run;

Data sl5 (drop=origination_date loan_age P_0 P_1 I_y F_y); 
	set get1; 
	account_id =5; 
	if filedate<'01oct2017'd then delete;
	origination_date= '15nov2017'd;
	loan_age= intck('Quarter', origination_date , fileDate);
	pob=100*loan_age/12; 
	POB_5 = max(min(pob, 5),0);
	POB_50= max(min(50, POB),5);
run;

Data sl6 (drop=origination_date loan_age P_0 P_1 I_y F_y); 
	set get1; 
	account_id =6; 
	if filedate<'01jan2018'd then delete;
	origination_date= '15feb2018'd;
	loan_age= intck('Quarter', origination_date , fileDate);
	pob=100*loan_age/12; 
	POB_5 = max(min(pob, 5),0);
	POB_50= max(min(50, POB),5);
run;

Data sl7 (drop=origination_date loan_age P_0 P_1 I_y F_y); 
	set get1; 
	account_id =7; 
	if filedate<'01apr2018'd then delete;
	origination_date='15May2018'd;
	loan_age= intck('Quarter', origination_date , fileDate);
	pob=100*loan_age/12; 
	POB_5 = max(min(pob, 5),0);
	POB_50= max(min(50, POB),5);
run;

Data sl8 (drop=origination_date loan_age P_0 P_1 I_y F_y); 
	set get1; 
	account_id =8; 
	if filedate<'01Jul2018'd then delete;
	origination_date='15Aug2018'd;
	loan_age= intck('Quarter', origination_date , fileDate);
	pob=100*loan_age/12; 
	POB_5 = max(min(pob, 5),0);
	POB_50= max(min(50, POB),5);
run;

Data sl9 (drop=origination_date loan_age P_0 P_1 I_y F_y); 
	set get1; 
	account_id =9; 
	if filedate<'01Oct2018'd then delete;
	origination_date='15Nov2018'd;
	loan_age= intck('Quarter', origination_date , fileDate);
	pob=100*loan_age/12; 
	POB_5 = max(min(pob, 5),0);
	POB_50= max(min(50, POB),5);
run;

data interim.ci_superloans_adverse; 
	set sl1 sl2 sl3 sl4 sl5 sl6 sl7 sl8 sl9; 
run; 

proc logistic inmodel=out.model_ci_v5_training;
	score data=interim.ci_superloans_adverse
 	out=out.outcomes_adverse_ci_superloans;
run;

/*Severe*/ 
data interim.superloans_ci; 
	set out.outcomes_severe_cI_dec13; 
	if filedate<'30Sep2014'd then delete;
	if boh_rating ne 3 then delete;
	if naics ne 'Wholesale Trade' then delete; 
run; 

data get1; 
	set interim.superloans_ci(where=(account_id=81544803));
run; 
Data sl1 (drop=origination_date loan_age P_0 P_1 I_y F_y); 
	set get1; 
	account_id =1; 
	origination_date= '15Nov2016'd;
	loan_age= intck('Quarter', origination_date , fileDate);
	pob=100*loan_age/12; 
	POB_5 = max(min(pob, 5),0);
	POB_50= max(min(50, POB),5);
run;

Data sl2 (drop=origination_date loan_age P_0 P_1 I_y F_y); 
	set get1; 
	account_id =2; 
	if filedate<'01Jan2017'd then delete;
	origination_date= '15Feb2017'd;
	loan_age= intck('Quarter', origination_date , fileDate);
	pob=100*loan_age/12; 
	POB_5 = max(min(pob, 5),0);
	POB_50= max(min(50, POB),5);
run;

Data sl3 (drop=origination_date loan_age P_0 P_1 I_y F_y); 
	set get1; 
	account_id =3; 
	if filedate<'01Apr2017'd then delete;
	origination_date= '15May2017'd;
	loan_age= intck('Quarter', origination_date , fileDate);
	pob=100*loan_age/12; 
	POB_5 = max(min(pob, 5),0);
	POB_50= max(min(50, POB),5);
run;

Data sl4 (drop=origination_date loan_age P_0 P_1 I_y F_y); 
	set get1; 
	account_id =4; 
	if filedate<'01Jul2017'd then delete;
	origination_date= '15Aug2017'd;
	loan_age= intck('Quarter', origination_date , fileDate);
	pob=100*loan_age/12; 
	POB_5 = max(min(pob, 5),0);
	POB_50= max(min(50, POB),5);
run;

Data sl5 (drop=origination_date loan_age P_0 P_1 I_y F_y); 
	set get1; 
	account_id =5; 
	if filedate<'01oct2017'd then delete;
	origination_date= '15nov2017'd;
	loan_age= intck('Quarter', origination_date , fileDate);
	pob=100*loan_age/12; 
	POB_5 = max(min(pob, 5),0);
	POB_50= max(min(50, POB),5);
run;

Data sl6 (drop=origination_date loan_age P_0 P_1 I_y F_y); 
	set get1; 
	account_id =6; 
	if filedate<'01jan2018'd then delete;
	origination_date= '15feb2018'd;
	loan_age= intck('Quarter', origination_date , fileDate);
	pob=100*loan_age/12; 
	POB_5 = max(min(pob, 5),0);
	POB_50= max(min(50, POB),5);
run;

Data sl7 (drop=origination_date loan_age P_0 P_1 I_y F_y); 
	set get1; 
	account_id =7; 
	if filedate<'01apr2018'd then delete;
	origination_date='15May2018'd;
	loan_age= intck('Quarter', origination_date , fileDate);
	pob=100*loan_age/12; 
	POB_5 = max(min(pob, 5),0);
	POB_50= max(min(50, POB),5);
run;

Data sl8 (drop=origination_date loan_age P_0 P_1 I_y F_y); 
	set get1; 
	account_id =8; 
	if filedate<'01Jul2018'd then delete;
	origination_date='15Aug2018'd;
	loan_age= intck('Quarter', origination_date , fileDate);
	pob=100*loan_age/12; 
	POB_5 = max(min(pob, 5),0);
	POB_50= max(min(50, POB),5);
run;

Data sl9 (drop=origination_date loan_age P_0 P_1 I_y F_y); 
	set get1; 
	account_id =9; 
	if filedate<'01Oct2018'd then delete;
	origination_date='15Nov2018'd;
	loan_age= intck('Quarter', origination_date , fileDate);
	pob=100*loan_age/12; 
	POB_5 = max(min(pob, 5),0);
	POB_50= max(min(50, POB),5);
run;

data interim.ci_superloans_severe; 
	set sl1 sl2 sl3 sl4 sl5 sl6 sl7 sl8 sl9; 
run; 

proc logistic inmodel=out.model_ci_v5_training;
	score data=interim.ci_superloans_severe
 	out=out.outcomes_severe_ci_superloans;
run;

/*Generate the data sets for ALM*/ 
/*Base*/ 
data out.CI_dryrun_superloan_base; 
 	set out.outcomes_base_ci_superloans (keep= filedate account_ID P_1); 
	if filedate<= '30Sep16'd then delete; 
	rename P_1=PD;
run; 

Data out.CI_dryrun_superloan_base_q1;
	set out.CI_dryrun_superloan_base(WHERE=(FILEDATE='01DEC16'd));
RUN;

proc export data= out.CI_dryrun_superloan_base_q1
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun_superloan\CI_dryrun_superloan_base_q1.csv'
	dbms = csv
	REPLACE;
run;

Data out.CI_dryrun_superloan_base_q2;
	set out.CI_dryrun_superloan_base(WHERE=(FILEDATE='01MAR17'd));
RUN;

proc export data= out.CI_dryrun_superloan_base_q2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun_superloan\CI_dryrun_superloan_base_q2.csv'
	dbms = csv
	REPLACE;
run;

Data out.CI_dryrun_superloan_base_q3;
	set out.CI_dryrun_superloan_base(WHERE=(FILEDATE='01JUN17'd));
RUN;

proc export data= out.CI_dryrun_superloan_base_q3
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun_superloan\CI_dryrun_superloan_base_q3.csv'
	dbms = csv
	REPLACE;
run;

Data out.CI_dryrun_superloan_base_q4;
	set out.CI_dryrun_superloan_base(WHERE=(FILEDATE='01SEP17'd));
RUN;

proc export data= out.CI_dryrun_superloan_base_q4
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun_superloan\CI_dryrun_superloan_base_q4.csv'
	dbms = csv
	REPLACE;
run;

Data out.CI_dryrun_superloan_base_q5;
	set out.CI_dryrun_superloan_base(WHERE=(FILEDATE='01DEC17'd));
RUN;

proc export data= out.CI_dryrun_superloan_base_q5
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun_superloan\CI_dryrun_superloan_base_q5.csv'
	dbms = csv
	REPLACE;
run;

Data out.CI_dryrun_superloan_base_q6;
	set out.CI_dryrun_superloan_base(WHERE=(FILEDATE='01Mar18'd));
RUN;

proc export data= out.CI_dryrun_superloan_base_q6
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun_superloan\CI_dryrun_superloan_base_q6.csv'
	dbms = csv
	REPLACE;
run;

Data out.CI_dryrun_superloan_base_q7;
	set out.CI_dryrun_superloan_base(WHERE=(FILEDATE='01Jun18'd));
RUN;

proc export data= out.CI_dryrun_superloan_base_q7
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun_superloan\CI_dryrun_superloan_base_q7.csv'
	dbms = csv
	REPLACE;
run;

Data out.CI_dryrun_superloan_base_q8;
	set out.CI_dryrun_superloan_base(WHERE=(FILEDATE='01Sep18'd));
RUN;

proc export data= out.CI_dryrun_superloan_base_q8
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun_superloan\CI_dryrun_superloan_base_q8.csv'
	dbms = csv
	REPLACE;
run;

Data out.CI_dryrun_superloan_base_q9;
	set out.CI_dryrun_superloan_base(WHERE=(FILEDATE='01DEC18'd));
RUN;

proc export data= out.CI_dryrun_superloan_base_q9
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun_superloan\CI_dryrun_superloan_base_q9.csv'
	dbms = csv
	REPLACE;
run;

/* Adverse*/
data out.CI_dryrun_superloan_adverse; 
 	set out.outcomes_adverse_ci_superloans (keep= filedate account_ID P_1); 
	if filedate<= '30Sep16'd then delete; 
	rename P_1=PD;
run; 

Data out.CI_dryrun_superloan_adverse_q1;
	set out.CI_dryrun_superloan_adverse(WHERE=(FILEDATE='01DEC16'd));
RUN;

proc export data= out.CI_dryrun_superloan_adverse_q1
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun_superloan\CI_dryrun_superloan_adverse_q1.csv'
	dbms = csv
	REPLACE;
run;

Data out.CI_dryrun_superloan_adverse_q2;
	set out.CI_dryrun_superloan_adverse(WHERE=(FILEDATE='01MAR17'd));
RUN;

proc export data= out.CI_dryrun_superloan_adverse_q2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun_superloan\CI_dryrun_superloan_adverse_q2.csv'
	dbms = csv
	REPLACE;
run;

Data out.CI_dryrun_superloan_adverse_q3;
	set out.CI_dryrun_superloan_adverse(WHERE=(FILEDATE='01JUN17'd));
RUN;

proc export data= out.CI_dryrun_superloan_adverse_q3
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun_superloan\CI_dryrun_superloan_adverse_q3.csv'
	dbms = csv
	REPLACE;
run;

Data out.CI_dryrun_superloan_adverse_q4;
	set out.CI_dryrun_superloan_adverse(WHERE=(FILEDATE='01SEP17'd));
RUN;

proc export data= out.CI_dryrun_superloan_adverse_q4
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun_superloan\CI_dryrun_superloan_adverse_q4.csv'
	dbms = csv
	REPLACE;
run;

Data out.CI_dryrun_superloan_adverse_q5;
	set out.CI_dryrun_superloan_adverse(WHERE=(FILEDATE='01DEC17'd));
RUN;

proc export data= out.CI_dryrun_superloan_adverse_q5
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun_superloan\CI_dryrun_superloan_adverse_q5.csv'
	dbms = csv
	REPLACE;
run;

Data out.CI_dryrun_superloan_adverse_q6;
	set out.CI_dryrun_superloan_adverse(WHERE=(FILEDATE='01Mar18'd));
RUN;

proc export data= out.CI_dryrun_superloan_adverse_q6
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun_superloan\CI_dryrun_superloan_adverse_q6.csv'
	dbms = csv
	REPLACE;
run;

Data out.CI_dryrun_superloan_adverse_q7;
	set out.CI_dryrun_superloan_adverse(WHERE=(FILEDATE='01Jun18'd));
RUN;

proc export data= out.CI_dryrun_superloan_adverse_q7
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun_superloan\CI_dryrun_superloan_adverse_q7.csv'
	dbms = csv
	REPLACE;
run;

Data out.CI_dryrun_superloan_adverse_q8;
	set out.CI_dryrun_superloan_adverse(WHERE=(FILEDATE='01Sep18'd));
RUN;

proc export data= out.CI_dryrun_superloan_adverse_q8
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun_superloan\CI_dryrun_superloan_adverse_q8.csv'
	dbms = csv
	REPLACE;
run;

Data out.CI_dryrun_superloan_adverse_q9;
	set out.CI_dryrun_superloan_adverse(WHERE=(FILEDATE='01DEC18'd));
RUN;

proc export data= out.CI_dryrun_superloan_adverse_q9
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun_superloan\CI_dryrun_superloan_adverse_q9.csv'
	dbms = csv
	REPLACE;
run;

/* Severe */
data out.CI_dryrun_superloan_Severe; 
 	set out.outcomes_severe_ci_superloans (keep= filedate account_ID P_1); 
	if filedate<= '30Sep16'd then delete; 
	rename P_1=PD;
run; 

Data out.CI_dryrun_superloan_Severe_q1;
	set out.CI_dryrun_superloan_Severe(WHERE=(FILEDATE='01DEC16'd));
RUN;

proc export data= out.CI_dryrun_superloan_Severe_q1
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun_superloan\CI_dryrun_superloan_Severe_q1.csv'
	dbms = csv
	REPLACE;
run;

Data out.CI_dryrun_superloan_Severe_q2;
	set out.CI_dryrun_superloan_Severe(WHERE=(FILEDATE='01MAR17'd));
RUN;

proc export data= out.CI_dryrun_superloan_Severe_q2
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun_superloan\CI_dryrun_superloan_Severe_q2.csv'
	dbms = csv
	REPLACE;
run;

Data out.CI_dryrun_superloan_Severe_q3;
	set out.CI_dryrun_superloan_Severe(WHERE=(FILEDATE='01JUN17'd));
RUN;

proc export data= out.CI_dryrun_superloan_Severe_q3
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun_superloan\CI_dryrun_superloan_Severe_q3.csv'
	dbms = csv
	REPLACE;
run;

Data out.CI_dryrun_superloan_Severe_q4;
	set out.CI_dryrun_superloan_Severe(WHERE=(FILEDATE='01SEP17'd));
RUN;

proc export data= out.CI_dryrun_superloan_Severe_q4
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun_superloan\CI_dryrun_superloan_Severe_q4.csv'
	dbms = csv
	REPLACE;
run;

Data out.CI_dryrun_superloan_Severe_q5;
	set out.CI_dryrun_superloan_Severe(WHERE=(FILEDATE='01DEC17'd));
RUN;

proc export data= out.CI_dryrun_superloan_Severe_q5
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun_superloan\CI_dryrun_superloan_Severe_q5.csv'
	dbms = csv
	REPLACE;
run;

Data out.CI_dryrun_superloan_Severe_q6;
	set out.CI_dryrun_superloan_Severe(WHERE=(FILEDATE='01Mar18'd));
RUN;

proc export data= out.CI_dryrun_superloan_Severe_q6
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun_superloan\CI_dryrun_superloan_Severe_q6.csv'
	dbms = csv
	REPLACE;
run;

Data out.CI_dryrun_superloan_Severe_q7;
	set out.CI_dryrun_superloan_Severe(WHERE=(FILEDATE='01Jun18'd));
RUN;

proc export data= out.CI_dryrun_superloan_Severe_q7
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun_superloan\CI_dryrun_superloan_Severe_q7.csv'
	dbms = csv
	REPLACE;
run;

Data out.CI_dryrun_superloan_Severe_q8;
	set out.CI_dryrun_superloan_Severe(WHERE=(FILEDATE='01Sep18'd));
RUN;

proc export data= out.CI_dryrun_superloan_Severe_q8
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun_superloan\CI_dryrun_superloan_Severe_q8.csv'
	dbms = csv
	REPLACE;
run;

Data out.CI_dryrun_superloan_Severe_q9;
	set out.CI_dryrun_superloan_Severe(WHERE=(FILEDATE='01DEC18'd));
RUN;

proc export data= out.CI_dryrun_superloan_Severe_q9
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\dryrun_superloan\CI_dryrun_superloan_Severe_q9.csv'
	dbms = csv
	REPLACE;
run;


