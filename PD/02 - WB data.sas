/* clear the log window*/
dm 'log' clear; 
LIBNAME Interim "";
LIBNAME Out "";
LIBNAME data "";

ods graphics on; 
/* Input the data*/
proc import out = wilshire
            datafile = "C:\Users\sghassemi\Documents\Projects\Bank of Hope\Data - FTP\wilshire\df_final_wilshire_sorted.csv" 
            dbms = csv replace;
run;

data wilshire;
	set wilshire; 
	rename 'Name.1'n =borrower;
	rename 'Note.Number'n=note_number;
	rename 'Class.Code'n=class_code;
	rename 'Non.Accrual.Code'n=non_accural_code;
run;

/* Wilshire Chargeoff files*/ 
proc import out= wilshire_chargeoffs
			datafile= "~\wilshire charge offs.csv" 
			dbms=csv replace;
run; 

/* Treasury rates*/ 
proc import out = Interim.rates
            datafile = "~\rates.csv" 
            dbms = csv replace;
run;

/* Acquired Loans */ 
/* input the acquired loan identifiers. These are the loansd would label as aquired*/
proc import out = interim.acquired_wilshire_raw1
            datafile = "~\Wilshire_aquired_list_20090930.csv" 
            dbms = csv replace;
run;

Data wilshire_acquired_idx1; 
	set interim.acquired_wilshire_raw1 (where=(Mirae="M"));
run;

proc import out = interim.acquired_wilshire_raw2
            datafile = "~\Wilshire_aquired_list_20131231.csv" 
            dbms = csv replace;
run;

Data wilshire_acquired_idx2; 
	set interim.acquired_wilshire_raw2 (where=(Bank in ("A", "S", "M")));
run;

proc sort data=wilshire_acquired_idx1 out= wilshire_acquired_idx1;
by note_number; 
run; 
proc sort data=wilshire_acquired_idx2 out= wilshire_acquired_idx2;
by note_number; 
run;

proc sql;
	select distinct note_number into :acq1 separated by ',' 
	from wilshire_acquired_idx1;
quit;

proc sql;
	select distinct note_number into :acq2 separated by ',' 
	from wilshire_acquired_idx2;
quit;

/* create a label in Wilshire for acquired loans.*/ 
data Wilshire; 
	set Wilshire; 
	format acquired_identifier $char32.;
	acquired_identifier="Wilshire_originated";
	if Note_number in (&acq1, &acq2) then acquired_identifier="acquired_wilshire";
run;

/*add the charge off*/
/*This file requires running the R code for Wilshire charge offs and cleaning the data*/ 
proc sql;
	create table wilshire_ch as 
	select a.*, b.co_ind, b.first_co_date
	from wilshire as a
	left join wilshire_chargeoffs as b
	on a.note_number=b.note_number; 
quit;  


data wilshire_ch; 
	set wilshire_ch; 
		/* create default event*/
	if non_accural_code in(2,4) or co_ind=1 then y = 1; *y=1 represents default; 
	else if non_accural_code in(0,9) and co_ind ne 1 then y=0;  * y=0 is no default;
	else y=111;  * to check if there are any errors; 

	temp_num = input(substrn(naics_Code, 1, 2), 2.);
	format naics $char24.;
	if temp_num= 23 then naics = "Construction";
	else if temp_num in (31,32,33)then naics = "Manufacturing";
	else if temp_num= 42 then naics = "Wholesale Trade";
	else if temp_num in (44,45) then naics = "Retail";
	else if temp_num in (48,49) then naics = "Transportation";
	else if temp_num= 51 then naics = "Information";
	else if temp_num= 53 then naics = "Real Estate & Rental";
	else if temp_num= 54 then naics = "Science & Technology";
	else if temp_num= 56 then naics = "Waste Management";
	else if temp_num= 62 then naics = "Health Care";
	else if temp_num= 71 then naics = "Arts & Entertainment";
	else if temp_num= 72 then naics = "Accomodation & Food";
	else if temp_num=0 then naics = "error";
	else if  temp_num not in (0, 23, 31,32,33,42,44,45,48,49,51,53,54,56,62,71,72) then do;
			naics = "Other";
	end;
	
	/* Dates */
	Yr_origination= year(originationdate); 
	mn_origination=month(originationdate);
	Yr_maturity= year(maturitydate); 
	mn_maturity=month(maturitydate);
	yr_file= year(filedate);
	mn_file= month(filedate);
	if mn_file in (1,2,3) then q_file=1;
	else if mn_file in (4,5,6) then q_file=2;
	else if mn_file in (7,8,9) then q_file=3;
	else if mn_file in (10,11,12) then q_file=4;

	/* identify the first year and month taht goes to our models */ 
	first_yr_kpmg = max(2007, yr_origination); 
	if yr_origination<=2007 then first_mn_kpmg =03;
		else if yr_origination>=2008 then first_mn_kpmg =mn_origination;
	format first_date_kpmg MMDDYY10.;
	format dummy MMDDYY10.;
	dummy='31Mar07'd;
	first_date_kpmg = max(dummy, originationdate); 
	/* basically either 2007Q1 or if it is orginated
	after the 2007Q1, then the origination date. */
	ttm_m=  12*(Yr_maturity - Yr_file) + (mn_maturity-mn_file); 
	ttm_q=intck('Quarter', fileDate, maturitydate);
	loan_age_q=intck('Quarter', originationdate , fileDate); 
	ttm_q=intck('Quarter', fileDate, maturitydate);
	term_q=intck('Quarter', originationdate, maturitydate);
	POB= 100*(loan_age_q)/term_q;

	rename 'Rate.Over.Split'n= interestRate;
Run;
proc sql;
	create table indx_wilshire as 
	select distinct note_number, min(filedate) as min_non_acc_date format=MMDDYY10.
	from wilshire_ch
	where y=1
	group by note_number; 
quit; 

/*merge it with the data set*/ 
proc sql; 
create table wilshire_ch2 as 
	select a.*, b.min_non_acc_date from 
		wilshire_ch as a 
	left join 
		indx_wilshire as b 
	on a.note_number=b.note_number;
quit;

data wilshire_ch2; 
	set wilshire_ch2;
	format f_non_acc_date MMDDYY10.; 
	if  first_co_date = . THEN f_non_acc_date= min_non_acc_date; 
	ELSE f_non_acc_date=min(min_non_acc_date, first_co_date); 
run;

*merge with the rate data set; 
proc sql; 
create table wilshire_rates as 
	select a.*, b.* from 
	wilshire_ch2 as a, interim.rates as b 
	where a.yr_file=b.year
	and a.q_file=b.q;
quit;

/* Clean up the data set*/
data df_final_1;
	set wilshire_rates; 
	if filedate>first_co_date and first_co_date ne . then delete;
	else if filedate>min_non_acc_date and min_non_acc_date ne . then delete; 
	if yr_maturity<=2006 then delete;
	else if yr_maturity<yr_file then delete; 
	else if yr_maturity=yr_file and mn_file-mn_maturity>2 then delete;
run; 

data df_final_4;
	set df_final_1; 
	format boh_id $char12.;
	boh_id='wilshire';
run; 

/* Portfolio ID - CRE and C&I*/
data df_final_4; 
	set df_final_4; 
	format class_code2 4.; 
	class_code2=input(class_code, 4.); 
run; 

data df_final_5;
	set df_final_4 (where=(class_code2 in (2,3,5,6,10,13,20, 21,30,31,32,33,34,35,
			36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,59,60,61,63,99)));
run; 

data df_final_5; 
	set df_final_5; 
	rename 'Net.Active.Principal.Balance'n= et_active_principal_balance;
	rename 'nap.-.naip.-.naip.in.gl'n= WB_balance;
	if WB_balance=0 or WB_balance = . then delete; 
run;

/* missing interest rates;
proc sql; 
	CREATE TABLE OUT.WILSHRIRE_MISSING_INTEREST AS 
	select * from df_final_5
	where interestrate=0 
	or interestrate=.; 
quit;

DATA OUT.WILSHRIRE_MISSING_INTEREST; 
	SET OUT.WILSHRIRE_MISSING_INTEREST (KEEP=FILEDATE portfolio NOTE_NUMBER class_code WB_balance interestrate y); 
	rename WB_balance = current_balance; 
run; 

proc export data=OUT.WILSHRIRE_MISSING_INTEREST
   outfile='~\missing_intyerest_rate_wilshire.csv'
   dbms=csv
   replace;
run;
*/

data df_final_7;
	set df_final_5; 
	if interestrate =0 or interestrate =. then delete;
run; 

data df_final_7;
	set  df_final_7; 
	format fixvar2 $char10.;
	rename 'Fixed.or.Variable.Interest.Rate'n=fixed_or_variable_interest_rate;
	if fixed_or_variable_interest_rate =: 'F' then fixvar2 = "Fixed";
	else if fixed_or_variable_interest_rate  =: 'V' then fixvar2 = "Variable";
	else if fixed_or_variable_interest_rate =: 'H' then fixvar2 = "Hybrid";
	else if fixed_or_variable_interest_rate =: 'N' then fixvar2 = "NA";
run; 

/* fill in some of the missing interest rate types*/
data df_final_7;
	set  df_final_7; 
	format indx Best8.;
	if fixvar2 = "Fixed" then indx=7;
	else if fixvar2 = "Variable" then indx=11;
	else if fixvar2 = "Hybrid" then indx=23;
	else if fixvar2 = "NA" then indx=100;
run;

proc sql; 
	create table indx11 as 
	select note_number, maturitydate, sum(distinct indx) as sum_indx, count(distinct fixvar2) as cnt_fv, 
					count(distinct interestRate) as cnt_i
	from df_final_7
	group by 1,2
	order by 1 desc;
quit; 

proc sql; 
	create table df_final_8 as
	select a.*, b.sum_indx, b.cnt_fv, b.cnt_i from 
	df_final_7 as a 
	left join 
	indx11 as b
	on a.Note_number=b.note_number
	and a.maturitydate=b.maturitydate;
quit; 

data df_final_9; 
	set df_final_8; 
	format fixvar3 $char2.;
	if sum_indx in (11,111) then fixvar3='V'; 
	else if cnt_i>1 and sum_indx in (18,118) then fixvar3='V'; 
	else if cnt_i>=2 and sum_indx in (7,107) then fixvar3='V';
	else if cnt_i=1 and sum_indx in (7,107) then fixvar3='F';
	else if cnt_i>1 and sum_indx =100 then fixvar3='V';
	else if cnt_i=1 and sum_indx =100 then fixvar3='F';
	else if cnt_i=1 and sum_indx in (18,118) then fixvar3='F';
	else if cnt_i>1 and sum_indx in (18,118) then fixvar3='V';
	else if cnt_i=1 and sum_indx in (100) then fixvar3='F';
	else if cnt_i>1 and sum_indx in (100) then fixvar3='V';
	else if sum_indx in (30,130) and fixvar2="Fixed" then fixvar3='HF';
	else if sum_indx in (30,130) and fixvar2 in ("Hybrid", 'NA') then fixvar3='HV';
	else if sum_indx in (23,123, 34,134) then fixvar3='HV';
	else fixvar3='E';
run; 


data df_final_10; 
	set df_final_9; 
	format loan_spread_v best8.2; 
	if fixvar3 = 'E' then loan_spread_v=.; 
	*these are loans that the fixed or variable nature of their interest rate
		is not specified (missing). So we cannot calculate a spread for them.; 
	else if fixvar3 in ('V' , 'HV') then loan_spread_v = 100*interestRate;
	else if fixvar3 in ('F', 'HF') and ttm_m<=1 then loan_spread_v = 100*interestRate-tb1m; 
	else if fixvar3 in ('F', 'HF') and ttm_m<=4 then loan_spread_v = 100*interestRate-tb3m; 
	else if fixvar3 in ('F', 'HF') and ttm_m<9 then loan_spread_v = 100*interestRate-tb6m;
	else if fixvar3 in ('F', 'HF') and ttm_m<18 then loan_spread_v = 100*interestRate-tb1y; 
	else if fixvar3 in ('F', 'HF') and ttm_m<30 then loan_spread_v = 100*interestRate-tb2y;
	else if fixvar3 in ('F', 'HF') and ttm_m<48 then loan_spread_v = 100*interestRate-tb3y;
	else if fixvar3 in ('F', 'HF') and ttm_m<72 then loan_spread_v = 100*interestRate-tb5y;
	else if fixvar3 in ('F', 'HF') and ttm_m<102 then loan_spread_v = 100*interestRate-tb7y;
	else if fixvar3 in ('F', 'HF') and ttm_m<180 then loan_spread_v = 100*interestRate-tb10y;
	else if fixvar3 in ('F', 'HF') and ttm_m<300 then loan_spread_v = 100*interestRate-tb20y;
	else if fixvar3 in ('F', 'HF') and ttm_m>=300 then loan_spread_v = 100*interestRate-tb30y;
run;

/* Correction for property types of some loans*/
proc import out =Interim.w_property_code_correction
            datafile = "~\correction - Missing Property Types Code.csv" 
            dbms = csv replace;
run;

data Interim.w_property_code_correction; 
	set Interim.w_property_code_correction; 
	rename 'Note.Number'n=note_number; 
run;
proc sql; 
	create table df_final_11 as 
	select a.*, b.New_code from 
	df_final_10 as a 
	left join 
	Interim.w_property_code_correction as b
	on a.note_number=b.note_number
	and a.filedate=b.filedate;
quit; 

data df_final_11; 
	SET df_final_11; 
	format  property_type_code2 best5.;
	if new_code ne . then property_type_code2 = new_code; 
	else if new_code = . then property_type_code2 = input('Property.Type.Code'n, Best5.);
run; 

proc sql; 
		create table test as 
		select note_number, filedate, property_type_code2, count(distinct property_type_code2)as cnt
		from df_final_11
		group by 1
		order by  1, 2 desc;
quit;


data test2; set test (where = (cnt>0)); run;

data test3 (drop=filledx); set test2; 
	retain filledx; 
	if not missing(property_type_code2) then filledx= property_type_code2;
	 property_type_code2=filledx;
run;
proc sql; 
	create table test4 as 
	select distinct note_number,sum(distinct property_type_code2) as sum1, max(property_type_code2) as max1 from test3
	group by 1;  
quit; 

proc sql; 
	create table test5 as 
	select a.*, b.sum1, b.max1 from 
	test3 as a
	left join 
	test4 as b
	on a.note_number=b.note_number;
quit; 

data test5; set test5;
	if cnt= 2 and property_type_code2=0 then property_type_code2= sum1; 
run;

proc sql; 
	create table df_final_11 as 
	select a.*, b.property_type_code2 as property_type_code4, b.cnt
	from df_final_11 as a
	left join 
	test5 as b
	on a.Note_number=b.note_number  
	and a.filedate=b.filedate;
quit;


proc sql; 
	create table test6 as 
	select note_number, property_type_code4,cnt,filedate, count(*) as cnt1
	from df_final_11
	group by 1,2
	order by 1,3 desc; 
quit;

data test7; set test6(where=(cnt>1));run;
proc sort data = test7 out = test8; 
by descending note_number descending cnt1;
run; 

data test9; 
	set test8; 
	retain dummy1;
	if  cnt1>1 then dummy1=property_type_code4;
	property_type_code4=dummy1;
run; 

proc sql; 
	create table df_final_11 as 
	select a.*, b.property_type_code4 as property_type_code5
	from df_final_11 as a
	left join 
	test9 as b
	on a.note_number=b.Note_number 
	and a.filedate=b.filedate; 
quit; 

data df_final_11 (drop=property_type_code4) ; 
	set df_final_11 (drop = new_code property_type_code2 cnt property_type_code5); 
	if property_type_code5 =. then property_type_code5=property_type_code4; 
run;
data df_final_11;
	set df_final_11;
	rename property_type_code5 = property_type_code;
run;


DATA df_final_11;
	SET df_final_11;
	format property_descr $char35.; 
	if property_type_code = 8 then property_descr = 'Carwash'; 
	else if property_type_code = 7 then property_descr = 'Gas Station';
	else if property_type_code = 6 then property_descr = 'Restaurant';
	else if property_type_code = 5 then property_descr = 'Hotel/Motel';
	else if property_type_code = 9 then property_descr = 'Church/Religious';
	else if property_type_code = 14 then property_descr = 'Mixed-use: Comm/Res';
	else if property_type_code = 15 then property_descr = 'Land';
	else if property_type_code = 16 then property_descr = 'Auto Repair';
	else if property_type_code = 17 then property_descr = 'Golf course';
	else if property_type_code = 1 then property_descr = 'Retail shopping';
	else if property_type_code = 2 then property_descr = 'Office';
	else if property_type_code = 3 then property_descr = 'Commercial/Industrial';
	else if property_type_code = 4 then property_descr = 'Medical';
	else if property_type_code = 12 then property_descr = 'Multifamily Res';
	else if property_type_code = 11 then property_descr = '1-4 Res';
	else if property_type_code = 10 then property_descr = 'All other single tenant';
	else if property_type_code in(13,18) then property_descr = 'Other real property'; /*there is no description for 18 */
	else if property_type_code = 0 then property_descr = 'None';
	else if property_type_code = . then property_descr = 'missing';
	else property_descr = 'Error';
run; 

DATA df_final_11;
	SET df_final_11;

	format portfolio_id $char5.;
	if class_code2 in (2,3,5,6,10,13,20) then portfolio_id ="CRE";
	else if class_code2 in (21,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,
							46,47,48,49,50,59,60,61,63,99) then portfolio_id ="CI";
	else portfolio_id ="error";

	format account_id BEST20.;
	account_id = note_number;

run;

data df_final_12; 
	set df_final_11;
	if portfolio_id ="error" then delete;
run;

data df_final_12; 
	set df_final_12; 
	rename 'Collateral.Code'n=collateral_code;
run;

data df_final_12; 
	set df_final_12; 
	format collateral_code2 $char3.;
	collateral_code2=collateral_code;
	if NOTE_NUMBER IN (323882, 611423,619781,723450,724350, 1204335, 8121293, 8234228) 
				and collateral_code='29' then collateral_code2='290';

	if note_number in (142227,334615,3038873) and collateral_code='29' 
				then collateral_code2='292';
	if collateral_code='37' then collateral_code2='371';
run;

DATA df_final_12;
	SET df_final_12;
	format collateral_descr $char30.; 
	if collateral_code2 in('0','1','2','3','4') then collateral_descr = 'Unsecured'; 
	else if collateral_code2 = '55' then collateral_descr = 'Security Agreement';
	else if collateral_code2 in ('71','72', '371') then collateral_descr = 'SFR 1st T - C';
	else if collateral_code2 in ('290') then collateral_descr = 'SFR 2nd T - C';
	else if collateral_code2 in ('73') then collateral_descr = '5+ Res 1st T- C';
	else if collateral_code2 in ('75') then collateral_descr = 'Construction trust deed';
	else if collateral_code2 in ('292') then collateral_descr = '5+ Res 2nd T- C';
	else if collateral_code2 in ('76') then collateral_descr = 'Mutual Funds';
	else if collateral_code2 in ('80', '83') then collateral_descr = 'Commercial 1st T - C';
	else if collateral_code2 in ('81') then collateral_descr = 'Commercial 2nd T - C';
	else if collateral_code2 in ('82') then collateral_descr = 'Commercial 3rd T - C';
	else if collateral_code2 in ('89') then collateral_descr = 'Vacant land - C';
	else if collateral_code2 in ('98') then collateral_descr = 'Other collateral';
	else if collateral_code2 in ('99') then collateral_descr = 'Multiple Collateral';
	else if collateral_code2 ='NA' then collateral_descr = 'NA/missing - C';
	else collateral_descr = 'Error';
run;

/*change the loan Rating system*/ 
data interim.df_final_12_wilshire;
	set df_final_12;
	rename 'Loan.Rating.Code1'n=Loan_rating_code1;
run;

data interim.df_final_12_wilshire;
	set interim.df_final_12_wilshire;
	format boh_rating 4.;
	if Loan_rating_code1 = 0 then boh_rating = 0;
	else if Loan_rating_code1 = 1000 then boh_rating = 1;
	else if Loan_rating_code1 = 2000 then boh_rating = 2;
	else if Loan_rating_code1 = 3000 then boh_rating = 3;
	else if Loan_rating_code1 = 4000 then boh_rating = 4;
	else if Loan_rating_code1 = 5000 then boh_rating = 4;
	else if Loan_rating_code1 = 6000 then boh_rating = 1000;
	else if Loan_rating_code1 = 7000 then boh_rating = 2000;
	else if Loan_rating_code1 = 8000 then boh_rating = 3000;
	else if Loan_rating_code1 = 9000 then boh_rating = 4000;
	else  boh_rating = 111; /*error identifier*/
run;

/*
data test;
	set interim.df_final_12_wilshire (where=(boh_rating = 111));
	run;
*/


proc contents data=interim.df_final_wilshire out=meta (keep=NAME) ; 
run ; 
proc print data=meta ; run ;


data interim.df_final_wilshire;
	set interim.df_final_12_wilshire; 
	rename WB_balance = current_balance;
	rename original_note_amount=original_balance;
	rename originationdate= origination_date;
	rename maturitydate= maturity_date;
	rename interestrate=interest_rate;
	rename fixvar3 = interest_rate_type;
	rename 'Times.Past.Due.01.To.29.Days'n=dpd0129_2;
	rename 'Times.Past.Due.30.To.59.Days'n=dpd3059_2;
	rename 'Times.Past.Due.60.To.89.Days'n=dpd6089_2;
	rename 'Times.Past.Due.Over.90.Days'n=dpd90_2;
	rename f_NON_ACC_date = first_nonacc_date;
	rename amt_co = amtChargedOff;
run; 

data interim.df_final_wilshire;
	set interim.df_final_wilshire;
	if dpd0129_2 = 'NA' then dpd0129_2='0'; 
	if dpd3059_2 = 'NA' then dpd3059_2='0';
	if dpd6089_2 = 'NA' then dpd6089_2='0';
	if dpd90_2 = 'NA' then dpd90_2='0';
run; 

data interim.df_final_wilshire (drop=dpd0129_2 dpd3059_2 dpd6089_2 dpd90_2);
	set interim.df_final_wilshire;
	dpd0129 = input(dpd0129_2, 4.);
	dpd3059 = input(dpd3059_2, 4.);
	dpd6089 = input(dpd6089_2, 4.); 
	dpd90 = input(dpd90_2, 4.);
run; 


DATA interim.df_final_wilshire;
SET interim.df_final_wilshire ;
if loan_spread_v>100 then delete; 
RUN;

data interim.df_final_wilshire2(keep= POB account_id acquired_identifier amtChargedOff
	boh_id boh_rating collateral_descr current_balance dpd90
	dpd0129 dpd3059 dpd6089 filedate first_nonacc_date loan_age_q
	loan_spread_v maturity_date month naics 
	origination_date portfolio_id property_descr 
	q tb10y tb1m tb1y tb20y tb2y tb30y
	tb3m tb3y tb5y tb6m tb7y term_q ttm_m ttm_q y year interest_rate );
	set interim.df_final_wilshire;

	rename co_amt= amtChargedOff;
run;

proc contents data=interim.df_final_wilshire2 out=meta (keep=NAME) ; 
run ; 
proc print data=meta ; run ;

