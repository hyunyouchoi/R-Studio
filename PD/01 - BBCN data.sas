/* clear the log window*/
dm 'log' clear; 
LIBNAME Interim "";
LIBNAME Out "";
LIBNAME data "";

ods graphics on;
/* Input the BBCN Bank data*/
proc import out = BBCN
            datafile = "C:\Users\sghassemi\Documents\Projects\Bank of Hope\Data - FTP\BBCN\data request bottom-up.csv" 
            dbms = csv replace;
run;

/* get the nonaccrual date */ 
data bbcn (drop=impairedDate nonAccrualDate nonAccrualDate2 collateralPropertyType); 
	set bbcn; 
	collateralPropertyType2= input(collateralPropertyType, 8.);
	nonAccrualDate2 = input(nonAccrualDate, mmddyy10.);
	format nonAccrualDate3 date9.;
	nonAccrualDate3=nonAccrualDate2;

	format impairedDate2 date9.;
	impairedDate2=input(impairedDate, mmddyy10.);

run;

data bbcn; 
	set bbcn; 
	rename collateralPropertyType2= collateralPropertyType;
	rename nonAccrualDate3 = nonAccrualDate;
	rename impairedDate2= impairedDate;
run;

/* input the treasury rates; This data is downloaded from Bloomberg terminal */ 
proc import out = Interim.rates
            datafile = "~\rates.csv" 
            dbms = csv replace;
run;

/* input the acquired loan identifiers. These are the loans to be labeled as aquired*/
proc import out = interim.acquired_BBCN_raw
            datafile = "\acquired loan identifier bbcn.csv" 
            dbms = csv replace;
run;

proc sql;
	create table acquired_loans as 
	select distinct note_number from interim.acquired_bbcn_raw;
quit;

proc sort data=acquired_loans out= acquired_loans;
	by note_number; 
run; 

/* if i create a mcaro with all note numbers it exceeds the SAS limit. SO I wiull create 
a few macro lists and combine them later. 
acq1= lines 1-9000
acq2= lines 9001 - 15000
acq3= lines 15001 - 20000
acq4= lines 20001 - 25000
acq5= lines 25001 - 27059
*/
proc sql;
	select distinct note_number into :acq1 separated by ',' 
	from acquired_loans
	where note_number<=614019;
quit;

proc sql;
	select distinct note_number into :acq2 separated by ',' 
	from acquired_loans
	where note_number>614019
	and note_number<=1531524;
quit;

proc sql;
	select distinct note_number into :acq3 separated by ',' 
	from acquired_loans
	where note_number>1531524
	and note_number<=20544035;
quit;

proc sql;
	select distinct note_number into :acq4 separated by ',' 
	from acquired_loans
	where note_number>20544035
	and note_number<=910747949;
quit;

proc sql;
	select distinct note_number into :acq5 separated by ',' 
	from acquired_loans
	where note_number>910747949
	and note_number<=919270000001;
quit;

/* Create a label in BBCN for acquired loans. */ 
data bbcn2; 
	set bbcn; 
	format acquired_identifier $char32.;
	acquired_identifier="bbcn_originated";
	if accountNo in (&acq1, &acq2, &acq3, &acq4, &acq5) then acquired_identifier="acquired_bbcn";
run;

*******************************************;
/*
proc sql; 
	create table test as 
	select distinct accountNo from bbcn
	where acquired_identifier="acquired_bbcn"; 
quit;

/* there are 25264 which is correct. because 1750 acquired loans from Foster Bank are missing
from the dataset.
so the label is correct.*/ 
*******************************************;

/* default event*/ 
data bbcn2; 
	set bbcn2;  
	/* create default event*/
	if amtChargedOff>0 then y = 1; * y=1 represents default; 
	else if amtChargedOff=0 and nonAccrualFlag in (1,2,3,4,9) then y=1; 
	else y=0; * y=0 is no default; 
run; 

data bbcn2_2 (drop=dummy nonaccrualdate temp_num); 
	set bbcn2; 
	/* Dates. */
	format non_acc_date MMDDYY10.; 
	non_acc_date=  Nonaccrualdate;
	Yr_origination= year(originationDate); 
	mn_origination=month(originationDate);
	Yr_maturity= year(maturityDate); 
	mn_maturity=month(maturityDate);
	yr_file= year(filedate);
	mn_file= month(filedate);
	if mn_file in (1,2,3) then q_file=1;
	else if mn_file in (4,5,6) then q_file=2;
	else if mn_file in (7,8,9) then q_file=3;
	else if mn_file in (10,11,12) then q_file=4; 

	/* identify the first year and month taht goes to our models */ 
	first_yr_kpmg = max(2007, yr_origination); 
	if yr_origination<=2007 then first_mn_kpmg =12;
		else if yr_origination>=2008 then first_mn_kpmg =mn_origination;
	
	/* identify the first date that matters to our models */ 
	format first_date_kpmg MMDDYY10.;
	dummy='31Dec07'd;
	first_date_kpmg = max(dummy, originationdate); 
	/* basically either 2007Q4 or if it is orginated
	after the 2007Q4, then the origination date. */

	/*TIME TO MATURITY */
	ttm_m=  12*(Yr_maturity - Yr_file) + (mn_maturity-mn_file); 
	ttm_q=intck('Quarter', fileDate, maturitydate);
	term_q=intck('Quarter', originationdate, maturitydate);
	loan_age_q=intck('Quarter', originationdate , fileDate); 
	POB= 100*(loan_age_q)/term_q;

	* create a naics code variable; 
	temp_num = input(substrn(naicsCode, 1, 2), 2.);
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
Run;

/* merge with the rate data set */ 
proc sql; 
create table bbcn_rates as 
	select a.*, b.* from 
	bbcn2_2 as a, interim.rates as b 
	where a.yr_file=b.year
	and a.q_file=b.q;
quit;

/*create spread variable */ 
data bbcn_rates; 
	set bbcn_rates; 
	format loan_spread_v best8.2; 
	if fixvar='N' then loan_spread_v = 10000; 
	*these are loans that the fixed or variable nature of their interest rate
		is not specified (missing). So we cannot calculate a spread for them.; 
	else if fixvar='V' then loan_spread_v = 100*interestRate;
	else if fixvar='F' and ttm_m<=1 then loan_spread_v = 100*interestRate-tb1m; 
	else if fixvar='F' and ttm_m<=4 then loan_spread_v = 100*interestRate-tb3m; 
	else if fixvar='F' and ttm_m<9 then loan_spread_v = 100*interestRate-tb6m;
	else if fixvar='F' and ttm_m<18 then loan_spread_v = 100*interestRate-tb1y; 
	else if fixvar='F' and ttm_m<30 then loan_spread_v = 100*interestRate-tb2y;
	else if fixvar='F' and ttm_m<48 then loan_spread_v = 100*interestRate-tb3y;
	else if fixvar='F' and ttm_m<72 then loan_spread_v = 100*interestRate-tb5y;
	else if fixvar='F' and ttm_m<102 then loan_spread_v = 100*interestRate-tb7y;
	else if fixvar='F' and ttm_m<180 then loan_spread_v = 100*interestRate-tb10y;
	else if fixvar='F' and ttm_m<300 then loan_spread_v = 100*interestRate-tb20y;
	else if fixvar='F' and ttm_m>=300 then loan_spread_v = 100*interestRate-tb30y;
run; 

/* Accounts with fixed interest rates and -0- interest rate (Story loans)*/ 
dATA OUT.BBCN_FIXED_ZERO; 
	SET bbcn_rates(where=(fixvar='F' and interestRate=0));
RUN; 
proc export data=OUT.BBCN_FIXED_ZERO 
   outfile='~\bbcn_fixed_0_percent.csv'
   dbms=csv
   replace;
run;

/* find the date for the first default event */ 
proc sql;
	create table indx_bbcn as 
	select distinct accountno, min(non_acc_date) as min_non_acc_date format=MMDDYY10.
	from bbcn2_2
	where y=1
	group by accountno; 
quit; 

/*merge it with the data set*/ 
proc sql; 
create table bbcn22 as 
	select a.*, b.min_non_acc_date from 
		bbcn_rates as a 
	left join 
		indx_bbcn as b 
	on a.accountno=b.accountno;
quit;

* clean up the dataset.; 
data bbcn3;
	set bbcn22; 
	if yr_maturity<=2006 then delete;
	else if yr_maturity<yr_file then delete; 
	else if yr_maturity=yr_file and mn_file-mn_maturity>2 then delete;
	if currentnetbookbal=0 then delete;
	*also create the year and month for deletion; 
	yr_min_non_acc_date=year(min_non_acc_date);
	mn_min_non_acc_date=month(min_non_acc_date);
run;

/* Create CRE/C&I portfolio ID*/ 
data bbcn4; 
	set bbcn3; 
	format portfolio_id $char5.;	 
	if callReportCodeDescr = "COMMERCIAL (GENERAL PLEDGE)" 
		and loantypeDescr in("Commercial Line (18)", "Commercial Line (18)","Commercial Line (18)",
							"Commercial Line (18)", "Commercial Term Loan (20)",
							"Commercial Term Loan (20)","Commercial Term Loan (20)",
							"Commercial Term Loan (20)","Comml LOC - Other Gov Gty (19)",
							"Comml LOC - Other Gov Gty (19)","Discounted Acceptance (33)",
							"Export Working Capital Program (3","Performance Bond L/C (44)",
							"Purchase Advance (31)","Purchase Advance (31)",
							"Purchase Advance (31)","SBA 172 Loan (66)",
							"SBA ARC Loans (62)","SBA ARC Loans (62)",
							"SBA ARC Loans (62)","SBA Express LOC (64)",
							"SBA Express LOC (64)", "SBA SOHO Loan (65)",
							"SBA Term Loans (61)","SBA Term Loans (61)",
							"SBA Term Loans (61)","Standby L/C (43)",
							"Standby L/C (43)","Trust Receipt (30)",
							"Working Capital Advance (37)","Working Capital Advance (37)",
							"Working Capital Advance (37)") THEN portfolio_id ="CI";

	else if  callReportCodeDescr = "COMMERCIAL (GENERAL PLEDGE)" 
		and loantypeDescr in("Commercial Real Estate (71)", "SBA Real Estate (60)", 
							"SBA Real Estate (60)", "SBA Real Estate (60)") 
			then portfolio_id ="CRE";

	else if  callReportCodeDescr  in ("CONVENTIONAL 5+ RESIDENTIAL","Conv 5+ Residential Prop",
							"NON-FARM NON -RESIDENTIAL","Other nonfarm nonresi property", "Owner-occupied nonfarm nonresi",
							"SECURED BY FARMEDLAND") then portfolio_id ="CRE";

	else if callReportCodeDescr  in ("Check Credit & Rev Credit Plan","Com'l Loan - International Dpt",
							"Com'l Loans - Borrowing Based") THEN portfolio_id ="CI";

	else if callReportCodeDescr  in ("Commercial Loans") 
			and loantypeDescr in("Bankers Health Group","Commercial Lease (25)",
								"Commercial Line (18)",
								"Commercial Term Loan (20)",
								"Comml Asset-Based LOC (22)",
								"Comml LOC - Other Gov Gty (19)",
								"Comml Term - Other Gov Gty (21)",
								"Discounted Acceptance (33)",
								"Export Working Capital Program (3",
								"Express Line (26)",
								"Master Comm LOC (01)",
								"Master Comm LOC Sublimit (03)",
								"Master ILOC (02)",
								"Master ILOC Sublimit (04)",
								"ODP LOC - Business",
								"Performance Bond L/C (44)",
								"Professional Line of Credit (51)",
								"Professional Term Loan (50)",
								"Purchase Advance (31)",
								"Purchase Advance-Comm (27)",
								"SBA 172 Loan (66)",
								"SBA ARC Loans (62)",
								"SBA Express LOC (64)",
								"SBA Express Loan (63)", 
								"SBA SOHO Loan (65)",
								"SBA Small Loan Advantage",
								"SBA Term Loans (61)",
								"Signature Line (11)",
								"Simple Line of Credit (24)",
								"Simple Loan - Commercial (23)",
								"Standby L/C (43)",
								"Syndicated Leveraged Lending",
								"Trust Receipt (30)",
								"Working Capital Advance (37)",
								"Working Capital Advance-Comm (28)") THEN portfolio_id ="CI";

	else if callReportCodeDescr  in ("Commercial Loans") 
			and loantypeDescr in("Comm RE - Revolving LOC (74)","Commercial Real Estate (71)",
								"SBA Real Estate (60)") then portfolio_id ="CRE";

	else if callReportCodeDescr  in ("INTERNATIONAL","Other Installment loans") 
			then portfolio_id ="CI";
	else portfolio_id ="error"; 
run; 

*delete the non CRE CI observations;
data bbcn4; 
	set bbcn4;
	if portfolio_id ="error" then delete; 
run; 

* delete the observations after the default date; 
data bbcn6; 
	set bbcn4; 
	if yr_file>yr_min_non_acc_date and yr_min_non_acc_date ne . then delete; 
	if yr_file=yr_min_non_acc_date and mn_file-mn_min_non_acc_date>2 and yr_min_non_acc_date ne . then delete;
run; 

proc sql; 
	create table indx as 
	select distinct accountno, sum(y) as sum_def
	from bbcn6
	group by 1
	order by 2 desc; 
quit;

proc sql; 
	create table interim.bbcn_sum_def_error
	as select * from indx
	where sum_def>1
	ORDER BY 1;
quit;

data bbcn8; 
	set bbcn6; 
	if accountno in (26506643, 23506889) and yr_file>=2009 and mn_file>03 then delete;
	if accountno in (26506643, 23506889) then do; 
		min_non_acc_date= '31Mar09'd;
		yr_min_non_acc_date=2009;
		mn_min_non_acc_date=03;
	end;
run; 

/* the nonaccrual date attribute for this loan is NULL and hence the logic
	fails for two quarters. I will delete the extra obs mannually.
	the first default happens ion 03/31/2009*/

data bbcn10; 
	set bbcn8;
	format account_id 20.;
	account_id = accountno;

	format boh_id $char12.;
	boh_id='bbcn';
	indx = collateralpropertytype;
RUN; 

/*Collateral */ 
Data bbcn10; 
	SET BBCN10; 
	Format collateral_descr $char35.; 

	if indx = 10 then collateral_descr = '1-4 Res'; 
	else if indx = 11 then collateral_descr = 'Multifamily Res';
	else if indx = 12 then collateral_descr = 'Office Condo';
	else if indx = 13 then collateral_descr = 'Office Medical/Dental';
	else if indx = 14 then collateral_descr = 'Office';
	else if indx = 15 then collateral_descr = 'Retail Single Tenant';
	else if indx = 16 then collateral_descr = 'Retail multiple Tenant';
	else if indx = 17 then collateral_descr = 'Outlet mall';
	else if indx = 18 then collateral_descr = 'Regional mall';
	else if indx = 19 then collateral_descr = 'Industrial warehouse single Tenant';
	else if indx = 20 then collateral_descr = 'Industrial warehouse multiple Tenant';
	else if indx = 21 then collateral_descr = 'Industrial manufacturing';
	else if indx = 22 then collateral_descr = 'self-storage';
	else if indx = 23 then collateral_descr = 'Industrial warehouse/condo';
	else if indx = 24 then collateral_descr = 'Mixed-use commercial/multifamily';
	else if indx = 25 then collateral_descr = 'Mixed-use office/retail';
	else if indx = 26 then collateral_descr = 'Mixed-use other';
	else if indx = 27 then collateral_descr = 'Mobile home/ RV park';
	else if indx = 28 then collateral_descr = 'Hotel and motel: flag';
	else if indx = 29 then collateral_descr = 'Hotel and motel: non-flag';
	else if indx = 30 then collateral_descr = 'Restaurant';
	else if indx = 31 then collateral_descr = 'Health clubs';
	else if indx = 32 then collateral_descr = 'Private schools';
	else if indx = 33 then collateral_descr = 'Gas Station';
	else if indx = 34 then collateral_descr = 'Auto Repair';
	else if indx = 35 then collateral_descr = 'Convalescent Facility';
	else if indx = 36 then collateral_descr = 'Carwash';
	else if indx = 37 then collateral_descr = 'Land leased';
	else if indx = 38 then collateral_descr = 'Land vacant developed';
	else if indx = 39 then collateral_descr = 'Land vacant raw';
	else if indx = 40 then collateral_descr = 'Church/Religious';
	else if indx = 41 then collateral_descr = 'Recycling';
	else if indx = 42 then collateral_descr = 'Golf course';
	else if indx = 99 then collateral_descr = 'Other real property';
	else if indx = 0 then collateral_descr = 'missing';
	else if indx = . then collateral_descr = 'missing';
	else collateral_descr = 'Error';
run;


/* Map the Risk Rating to the new one*/ 
data bbcn10; 
	set bbcn10; 
	format boh_rating 4.;
	if loanratingdescr2 = 'Substa' then boh_rating = 2000;
	else if loanratingdescr2 = 'Sp Men' then boh_rating = 1000;
	else if loanratingdescr2 = 'Doubtf' then boh_rating = 3000;
	else if loanratingdescr2 = 'Loss' then boh_rating = 4000;
	else if loanratingdescr2 = 'Pass-4' then boh_rating = 4;
	else if loanratingdescr2 = 'Pass-3' then boh_rating = 3;
	else if loanratingdescr2 in ('Pass-2', ' Pass-2','Pass-2 ', ' Pass-2 ') then boh_rating = 2;
	else if loanratingdescr2 = 'Pass-1' then boh_rating = 1;
	else  boh_rating = 111; /*error identifier*/
run; 

data test; set bbcn10(where= (boh_rating=111));run;
proc sql; create table test2 as select distinct loanratingdescr2 from test; quit;


/*clean up DCR*/ 
data bbcn10; 
	set bbcn10; 
	format DCR2 7.3;
	dcr2 = input (trim(dcr), 7.3);
	if dcr2 in (0.001,0.002,0.003,0.004,0.005,0.007) then dcr2=1000*dcr2; 
run; 

proc sql; 
	create table test as select distinct account_id, filedate,  acquired_identifier, dcr2, count(distinct dcr2) as cnt
	from bbcn10
	group by 1 
	order by 1, 4 desc;
quit; 

data test22; set test (where= (cnt=0 and acquired_identifier="acquired_bbcn")); run;
data test2; set test (where= (cnt>1)); run;
proc sort data = test2;
by account_id descending filedate;
run;

data test3 (drop=filleddcr2); 
	set test2; 
	retain filledDCR2; 
	if NOT missing(DCR2) then filleddcr2 = dcr2;
	dcr2=filleddcr2; 
run; 

proc sql; 
	create table bbcn11 as
	select a.*, b.dcr2 as dcr3
	from bbcn10 as a
	left join 
	test3 as b
	on a.account_id=b.account_id
	and a.filedate = b.filedate;
quit; 
data bbcn11; 
	set bbcn11; 
	if dcr3=. then dcr3=dcr2; 
run;

/*remove the letter of credit observations*/
data bbcn11; 
	set bbcn11 (where=(productDescr not in ('Letter of C')));
run; 

data bbcn11; 
	set bbcn11; 
	format interest_rate 7.4; 
			interest_rate=interestrate;
run;

proc contents data=bbcn11 out=meta (keep=NAME) ; 
run ; 

proc print data=meta ; run ;

data interim.df_final_bbcn;
	set bbcn11 ; 
	rename currentNetbookbal = current_balance;
	rename originalbal=original_balance;
	rename originationdate= origination_date;
	rename maturitydate= maturity_date;
	rename fixvar=interest_rate_type;
	rename timespd01to29d=dpd0129;
	rename timespd30to59d=dpd3059;
	rename timespd60to89d=dpd6089;
	rename timespdover90d=dpd90;
	rename min_non_acc_date = first_nonacc_date;
	rename collateral_descr =property_descr;
	rename origloantovalue = org_ltv;
	rename totalcollateralvalue=collateral_value;
	rename dcr3 = dcr;
run;

data interim.df_final_bbcn2;
	set interim.df_final_bbcn (keep= POB
		account_id acquired_identifier amtChargedOff
		boh_id boh_rating property_descr
		current_balance dpd90 dpd0129 dpd3059
		dpd6089 filedate first_nonacc_date
		loan_age_q loan_spread_v maturity_date month naics
		origination_date portfolio_id  q tb10y tb1m
		tb1y tb20y tb2y tb30y tb3m tb3y tb5y tb6m tb7y term_q ttm_m
		ttm_q y year interest_rate);
run; 

/* BAsed on the BOH Correction file for rates; */
data interim.df_final_bbcn2;
	set interim.df_final_bbcn2;
	if account_id=101257 and filedate='31Mar08'd then do; 
				interest_rate=0.2; 
				interest_rate_type='F';
				end;
	if account_id=861608  and filedate='31Dec09'd then DO; 
				interest_rate=0.05;
				interest_rate_type='V';
				end;
	if account_id=1515672 then do;
				interest_rate=0.03;
				interest_rate_type='F';
				end;
	if account_id = 80520061 and filedate='31Dec09'd then do; 
				interest_rate=0.0525;
				interest_rate_type='V';
				end;
	if account_id = 199900294 and filedate='31Mar09'd then do; 
				interest_rate=0.0525;
				interest_rate_type='V';
				end;
	if account_id = 200200276 and filedate in('31Mar12'd,'30Jun12'd) then do; 
				interest_rate=0.0425;
				interest_rate_type='V';
				end;
	if account_id = 200300870 and filedate='31Dec13'd then do; 
				interest_rate=0.08;
				interest_rate_type='F';
				end;
	if account_id = 3311084630 and filedate='31Dec11'd then do; 
				interest_rate=0.05;
				interest_rate_type='V';
				end;
run;

data interim.df_final_bbcn; 
	set interim.df_final_bbcn;
	if loan_spread_v>100 then delete;
run;

/* the sba portion*/
proc sql; 
	create table bbcn_sba as 
	select distinct filedate, sum(currentnetbookbal) as bal_bbcn
	from bbcn11
	where acquired_identifier="bbcn_originated"
	and loantypedescr like '%SBA%'
	group by 1;
quit; 

proc sql; 
	create table bbcn_sba as 
	select distinct filedate, sum(currentnetbookbal) as bal_bbcn
	from bbcn11
	where acquired_identifier="acquired_bbcn"
	and loantypedescr like '%SBA%'
	group by 1;
quit; 

proc sql; 
	create table bbcn_sba as 
	select distinct filedate, sum(currentnetbookbal) as bal_bbcn
	from bbcn11
	where acquired_identifier="bbcn_originated"
	and loantypedescr like '%SBA%'
	group by 1;
quit; 

proc sql; 
	create table bbcn_sba as 
	select distinct filedate, sum(currentnetbookbal) as bal_bbcn
	from bbcn11
	where acquired_identifier="acquired_bbcn"
	and loantypedescr like '%SBA%'
	group by 1;
quit; 

/*investigate the missing property types for Doug */ 
proc sql; 
	create table test as 
	select distinct property_descr, count (*) 
	from interim.df_final_bbcn2
	where portfolio_id="CRE"
	group by 1; 
quit; 

proc sql; 
	create table test2 as 
	select distinct property_descr, sum(current_balance) as Total_Balance, 
	mean(current_balance) as Average_Balance
	from interim.df_final_bbcn2
	where portfolio_id="CRE"
	group by 1; 
quit; 

proc sql; 
	create table test3 as 
	select distinct filedate, sum(current_balance) as total_balance, 
					mean(current_balance) as Average_Balance, 
					count(distinct account_id) as cnt
	from interim.df_final_bbcn2
	where portfolio_id="CRE"
	and property_descr="missing"
	group by 1; 
quit; 

proc sql; 
	create table test4 as 
	select distinct filedate, sum(current_balance) as total_balance, 
					mean(current_balance) as Average_Balance, 
					count(distinct account_id) as cnt
	from interim.df_final_bbcn2
	where portfolio_id="CRE"
	and property_descr="missing"
	and acquired_identifier="bbcn_originated"
	group by 1; 
quit; 
