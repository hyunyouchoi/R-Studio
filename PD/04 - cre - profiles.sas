/* clear the log window*/
dm 'log' clear; 
LIBNAME Interim "";
LIBNAME Out "";

ods graphics on;
%macro profile(var,format,xmin);
	Proc sort data=out.df_cre_dev_cleaned3; 
	by &var.;
	run;
	Proc means data=out.df_cre_dev_cleaned2 nway noprint order=formatted ;
	var y;
	Class &var. ;
	format &var. &format..;
	output out=summary_stats mean(y)=default_rate;
	run;
	proc export data= summary_stats
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\summary_stats.csv'
	dbms = csv
	REPLACE; 
	run; 
	Proc sgplot data=summary_stats;
	xaxis min=&xmin.;
	series x=&var. y=default_rate;
	band x=&var. lower=0 upper=_FREQ_ / fillattrs=ltgray y2axis transparency=0.5 legendlabel='Obs';
	run;
%mend;

%macro profile2(var,xmin);
	Proc sort data=out.df_cre_dev_cleaned3; 
	by &var.;
	run;
	Proc means data=interim.df_cre_model_vars2 nway noprint order=formatted ;
	var y;
	Class &var. ;
	output out=summary_stats mean(y)=default_rate;
	run;
	proc sort data=summary_stats out=summary_stats; 
	by  &var.;
	run; 
	Proc sgplot data=summary_stats;
	xaxis min=&xmin.;
	scatter x=&var. y=default_rate;
	band x=&var. lower=0 upper=_FREQ_ / fillattrs=ltgray y2axis transparency=0.75 legendlabel='Obs';
	run;
%mend;


data out.df_cre_dev;
	set  out.df_boh_final_base (where=(filedate<= '31Mar16'd and portfolio_id="CRE"));
run;

data out.df_cre_dev_cleaned3; 
	set out.df_cre_dev (where=(filedate>='31Dec07'd)); 
	if naics='error' then delete; 
	if loan_spread_v<0 then delete;
	if property_descr in ('missing', 'None') then delete;
run;

/*NAICS CODE*/ 
Proc means data=out.df_cre_dev_cleaned2 nway noprint order=formatted ;
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
Proc means data=out.df_cre_dev_cleaned2 nway noprint order=formatted ;
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
Proc means data=out.df_cre_dev_cleaned2 nway noprint order=formatted ;
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



Proc format; 
	value rgdp_qgf			Low - -2 = 'L1:Low-(-2)' 
							-2<- -0.5 = 'L2:(-2)-(-0.5)'	
							-0.5<-1.25 = 'L3:(-0.5)-1.25'
							1.25<-1.95 = 'L4:1.25-1.95'
							1.95<- 2.75 = 'L5: 1.95-2.75'
							2.75-High	= 'L6: 2.75-Max'
	;
%profile(RGDP_qG,rgdp_qgf,-4.05);



Proc format;
value POBf  			Low-5= 'L1: 0-5'
						5<-25 = 'L2: 5-25' 
						25<-50 = 'L3: 25-50'
						50<-75 = 'L4: 50-75'
						75<-85 = 'L7: 75-85'
						85<-95= 'L8: 85-95'
						95-High = 'L9:95-Max'
	;
%profile(POB,POBf,0);

Proc format; 
	value dpd01f			Low-0  = 'L0:0'
							0<-2 ='L1: 0-2'
							2<-4 = 'L2:2-4'
							4<-8 = 'L3:4-8'
							8<-10 = 'L4: 8-25'
							10<-15 = 'L5: 10-15'
							15<-20 = 'L6: 15-20' 
							20-High = 'L6:20-Max'
	;
%profile(dpd0129_0,dpd01f,0);

Proc format;
	value loan_spread_vf 	Low-2.1= 'a:0-2'
							/*1<-2 = 'b:1-2'*/
							2.1<-3.25 = 'd:2-3.25'
							3.25<-4 ='e:3.25-4'
							4<-5.5 ='f:4-5.5'
							/*5<-6 ='g:5-6'*/
							5.5<-7.5= 'h:5.5-7.5'
							7.5-High = 'i:7.5-Max'
	;	

%profile(loan_spread_v,loan_spread_vf,0);


Proc format; 
	value pfi_nonres_structuresF				Low - 365 = '340-365'
												365<- 450 = '365-450'
												450<-485 ='450-485'
												485-High = '485-Max'
						;
%profile(pfi_nonres_structures,pfi_nonres_structuresf,350);

******************************************************************************************************************;




proc LOGISTIC data = interim.df_cre_model_vars2 desc plots = all outmodel=out.model_cre_v13;
** Indicate that var is a categorical variable;

** Specify the independent variables here;
	model y =   POB_1 POB_100
				loan_spread_v
				dpd0129_0							
				RGDPg_AG
				pfi_nonres_structures
				
/selection = stepwise;
	output out=out.outreg_cre_v13 p=predicted;
run;




proc LOGISTIC data = interim.df_cre_model_vars2 desc plots = all outmodel=out.model_cre_v14;
** Indicate that var is a categorical variable;

** Specify the independent variables here;
	model y =   POB_1 POB_100
				loan_spread_0 loan_spread_2
				dpd0129_0							
				RGDPg_AG
				
				cppi_ag
/selection = stepwise;
	output out=out.outreg_cre_v14 p=predicted;
run;




proc LOGISTIC data = interim.df_cre_model_vars desc plots = all outmodel=out.model_cre_v7;
** Indicate that var is a categorical variable;

** Specify the independent variables here;
	model y = 
				POB POB_1 POB_100

				bbb_yield_lag_2_2
				
				tb5yr_1 tb5yr_2

				
				djia_1 djia

				RGDPg_AG
				CAUR_YD
				
				Cahpi_qg CAHPI_ag
					 
				cppi


/selection = stepwise;
	output out=out.outreg_cre_v7 p=predicted;
run;

proc LOGISTIC data = interim.df_cre_model_vars desc plots = all outmodel=out.model_cre_v6;
** Indicate that var is a categorical variable;

** Specify the independent variables here;
	model y = 
POB_1 POB_100

tb5yr_1 
djia
djia_1 
RGDPg_AG
caur_yd 

Cahpi_qg cahpi_ag
	 
cppi/selection = stepwise;
	output out=out.outreg_cre_v6 p=predicted;
run;
/*
proc LOGISTIC data = interim.df_cre_model_vars desc plots = all outmodel=out.model_cre_v3;
** Indicate that var is a categorical variable;

** Specify the independent variables here;
	model y = cahpi_ag cahpi_qg tb5yr_1 tb5yr_2	CCI djia_1 CAUR_YD cppi rgdpg_ag pob_100
				DPD3059_0/selection = stepwise;
	output out=out.outreg_cre_v3 p=predicted;
run;

proc LOGISTIC data = interim.df_cre_model_vars desc plots = all outmodel=test;
** Indicate that var is a categorical variable;

** Specify the independent variables here;
	model y = cci rgdpg_ag CAUR_YD cppi djia DPD3059_0 dpd0129_0 pob_100/selection = stepwise;
	output out=test10 p=predicted;
run;
*/


Data indx101; 
	set out.outreg_cre_v5 (keep= filedate account_id current_balance predicted);
run; 

proc sql; 
	CREATE TABLE out.w_average_PD_Cre as 
	select filedate, sum(current_balance*predicted)/sum(current_balance) as w_ave_pd
	from indx101
	group by filedate;
quit; 
	
proc export data= out.w_average_PD_Cre
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Output\w_average_PD_Cre.csv'
	dbms = csv
	replace; 
run; 

***********************************************************Profiles***************************************************;

ODS HTML CLOSE; 
ODS HTML; 

Proc format; 
	value CPPIF				Low - 165 = '150-165'
							165<- 180 = '165-180'
							180<- 195 = '180-195'
							195<-220 = '195-220'
							220-High = '220-Max'
	;
%profile(CPPI,CPPIf,150);


Proc format; 
	value pfi_nonres_structuresF				Low - 375 = '350-375'
												375<- 400 = '375-410'
												400<- 430 = '410-450'
												430<-475 = '450-500'
												475<-525 ='475-525'
												525<-575 ='525-575'
												575-High = '575-Max'
						;
%profile(pfi_nonres_structures,pfi_nonres_structuresf,350);



Proc format; 
	value pfi_nonres_structuresF				Low - 365 = '340-365'
												365<- 450 = '365-450'
												450<-485 ='450-485'
												485-High = '485-Max'
						;
%profile(pfi_nonres_structures,pfi_nonres_structuresf,350);

proc format; 
	value NCREIF_property_indexf	  Low-1500 = '1450-1500'
										1500<-1700 = '1500-1700'
										1700<-2000 = '1700 -2000'
										2000<-2250 = '2000-2250'
										2250<-2500 = ' 2250-2500'
										2500-High = '2500-Max'
	;

%profile(NCREIF_property_index,NCREIF_property_indexf,1400);




Proc format; 
value vixf					Low - 20 = '17-20'
							20<- 25 = '20-25'
							25<- 27 = '25-27'
							27 <-30 ='27-30'
							30<-35 = '30-35'
							35<-40 = '35-40'	
							40-High = '40-Max'
	;
%profile(vix,vixf,17);


%profile2(vix_qd,-20);
Proc format; 
value vix_qdf				Low - -15 = 'L1: min-(-15)'
							-15<- -5 = 'L2: (-15)-(-5)'
							-5<-0 = 'L3: (-5)-0'
							0 <-5 ='L4: 0-5'
							5<-10 = 'L5: 5-10'
							10<-20 = 'L6: 10-20'	
							20-High = 'L7: 20-Max'
	;
%profile(vix_qd,vix_qdf,-50);


Proc format;
value POBf  			Low-2= 'L1: 0-2'
						2<-3 = 'L2: 2-3' 
						3<-6 = 'L3: 3-6'
						6<-25 = 'L4: 6-25'	
						25<-50 = 'L5: 25-50'
						50<-75 = 'L6: 50-75'
						75<-95 = 'L7: 75-95'
						95-High = 'L8:95-Max'
	;
%profile(POB,POBf,0);


Proc format; 
value djiaf					Low - 11000 = 'L1: 8044-11000'
							11000<- 13000 = 'L2: 11000-13000'
							13000<- 15000 = 'L3: 13000-15000'
							15000<-20000 = 'L4: 15000-20000'
							20000-High = 'L5: 20000-Max'
	;
%profile(djia,djiaf,8044.2);

Proc format; 
value djiaf					Low - 9500 = 'L1: 8044-9500'
							9500<-10250 = 'L2: 9500-10250'
							10250<-11000 = 'L3: 10250 - 11000'
							11000<- 13000 = 'L4: 11000-13000'
							13000<- 15000 = 'L5: 13000-15000'
							15000<-20000 = 'L6: 15000-20000'
							20000-High = 'L7: 20000-Max'
	;
%profile(djia,djiaf,8044.2);

Proc format; 
value djia_qgf					Low - -5 = 'L1: 8044-11000'
								-5<- -2 = 'L2: 11000-13000'
								-2<- 0  = 'L3: 13000-15000'
								0<-3 = 'L4: 15000-20000'
								3<-6 ='L5: 3-6'
								6-High = 'L6: 20000-Max'
	;
%profile(djia_qg,djia_qgf,-40);


Proc format; 
	value dpd01f			Low-0  = 'L0:0'
							0<-2 ='L1: 0-2'
							2<-4 = 'L2:2-4'
							4<-8 = 'L3:4-8'
							8<-10 = 'L4: 8-25'
							10<-15 = 'L5: 10-15'
							15<-20 = 'L6: 15-20' 
							20-High = 'L6:20-Max'
	;
%profile(dpd0129,dpd01f,0);

Proc format; 
	value dpd30f			Low-0  = 'L1:0'
							0<-2 = 'L2:0-2'
							2<-5 = 'L3: 2-5'
							5-High = 'L4:5-Max'
	;
%profile(dpd3059,dpd30f,0);


Proc format; 
	value dpd60f			Low-1  = 'L1:0-1'
							1-High = 'L2:1-Max'
	;
%profile(dpd6089,dpd60f,0);

	Proc format; 
	value dpd90f			Low-1  = 'L1:0-1'
							1-High = 'L2:1-Max'
	;
%profile(dpd90,dpd90f,0);


Proc format; 
	value rdi_qgf		Low - -2    = 'A: Low-(-2)'
						-2 <- -1 = 'B: (-2)-(-1)'
						-1<-0 ='C: (-1)-0'
						0<-1.5 = 'E: 0-1.5'
						1.5<-2.25 = 'G: 1.5-2.25'
						2.25<-2.5 = 'H: 2.25-2.5'
						2.5<-2.75 = 'I: 2.5-2.75'
						2.75<-3 = 'J: 2.75-3'
						3-High = 'K: 3-Max'
	;
%profile(rdi_qg,rdi_qgf,-2.93);


Proc format; 
	value rgdp_qgf			Low - -5 = 'a:(-8.5)-(-5)'
							-5 <- -3 = 'b:(-5)-(-3)'
							-3<--1.5 = 'c:(-3)-(-1.5)'
							-1.5<--0.5 = 'd: (-1.5)-(-0.5)'
							-0.5<- 0.25 = 'e: (-0.5)-0.25'
							0.25<- 1 = 'f: 0.25-1'
							1<-2.25 = 'g: 1-2.25'
							2.25<-3 = 'h: 2.50-3'
							3-High = 'k: 3-Max'
	;
%profile(RGDP_qG,rgdp_qgf,-8.5);

Proc format; 
	value rgdpg_qg_lag_2f			Low - -5 = 'a:(-8.5)-(-5)'
							-5 <- -4 = 'b:(-5)-(-4)'
							-4<--3 = 'c: (-4)-(-3)'
							-3<--1.5 = 'd: (-3)-(-1.5)'
							-1.5<--0.5 = 'e: (-1.5)-(-0.5)'
							-0.5<- 0.25 = 'f: (-0.5)-0.25'
							0.25<- 1 = 'g: 0.25-1'
							1<-2.25 = 'h: 1-2.25'
							2.25<-3 = 'i: 2.50-3'
							3-High = 'j: 3-Max'
	;

%profile(RGDPg_qG_lag_2,rgdpg_qg_lag_2f,-8.5);

Proc format; 
	value rgdp_qg_lag_2f			Low - -7 = 'a:(-8.5)-(-5)'
							-7 <- -6 = 'b:(-5)-(-4)'
							-6<--5 = 'c: (-4)-(-3)'
							-5<--1.5 = 'd: (-3)-(-1.5)'
							-1.5<--0.5 = 'e: (-1.5)-(-0.5)'
							-0.5<- 0.25 = 'f: (-0.5)-0.25'
							0.25<- 1 = 'g: 0.25-1'
							1<-2.25 = 'h: 1-2.25'
							2.25<-3 = 'i: 2.50-3'
							3-High = 'j: 3-Max'
	;

%profile(RGDP_qG_lag_2,rgdp_qg_lag_2f,-8.5);




Proc format; 
	value CAUR_ydf			Low- -1 = 'L1:(-2)-(-1)'	
							-1<-0 = 'L2:(-1)-0'
							0<-2.5 = 'L4:0-2.5'
							2.5-High	= 'L6:3.5-Max'
	;
%profile(caur_yd_lag_3,caur_ydf,-2);


Proc format; 
	value CAURf			Low- 6 = 'L1:5-6'	
							6<-7 = 'L2:6-7'
							7<-8 = 'L3:7-8'
							8<-9 = 'L4:8-9'
							9<-10 = 'L5:9-10'
							10<-11 ='L6: 10-11'
							11-High	= 'L7:11-Max'
	;
%profile(caur,caurf,5);

Proc format; 
	value CAUR_qdf			Low- 0 = 'L1:5-6'	
							0<-0.25= 'L2:6-7'
							0.25<-0.75 = 'L3:7-8'
							0.75<-1 = 'L4:8-9'
							1-High	= 'L7:11-Max'
	;
%profile(caur_qd,caur_qdf,-1);



proc format; 
	value NYURf             Low-5 ='4.75-5.2'
							5<-6 = '5.2 -6'
							6<-7 = '6-7'
							7<-8 = '7-8'
							8-High = '8-Max'
	;
%profile(nyur,nyurf,4.75);

Proc format;
	value loan_spread_vf 	Low-2.1= 'a:0-1'
							/*1<-2 = 'b:1-2'*/
							2.1<-3.25 = 'd:2-3'
							3.25<-4 ='e:3-4'
							4<-5.5 ='f:4-5'
							/*5<-6 ='g:5-6'*/
							5.5<-7.5= 'h:6-7.5'
							7.5-High = 'i:7.5-Max'
	;	

%profile(loan_spread_v,loan_spread_vf,0);

Proc format; 
value SPR10F				Low - 2 = '1'
							2<- 2.25 = '2'
							2.25<-2.5 = '3'
							2.5<-2.75 = '4'
							2.75<-3 = '5'
							3<-3.25 = '6'
							3.25<-3.75 = '7'
							3.75-High = '8'

	;
%profile(SPR10,SPR10f,1.3);

Proc format; 
value SPR10_QDF				Low - -0.6 = 'L1: min-(-0.6)'
							-0.6<- -0.1 = 'L2: (-0.6)-(-0.1)'
							-0.1<- 0.15 = 'L3: (-0.1)-0.15'
							0.15<-0.5 = 'L4: 0.15-0.4'
							0.5-High = 'L6: 0.6-Max'
	;
%profile(SPR10_QD,SPR10_QDF,-1.5);


	Proc format; 
	value t10y				Low-2.17  = '1.65-2.17'
							2.17<-2.5 = '2.17-2.71'
							2.5<-3 = '2.71-3.47'
							3<-3.5 = '3- 3.5'
							3.5<-4 = '3.5-3.75'
							4-High = '3.75-Max'
	;
%profile(tb10y,t10y,1.65);

Proc format; 
	value termf					Low - 36 = 'a'
								36<-84 = 'b'
								84<-100  = 'c'
								100<-120 ='d'
								120<-180 ='e'
								180-High = 'g'
	;
%profile(term,termf,0);




Proc format; 
	value bbb_yieldf			Low - 4.1 = '3.9-4.1'
								4.1<-4.2 = '4.1-4.2'
								4.2<-4.3 = '4.2-4.3'
								4.3<-4.4 = '4.3-4.4'
								4.4<-4.5 = '4.4<-4.5'
								4.5<-4.75  = '4.5-4.75'
								4.75<-5 = '4.75-5'
								5<-5.5 = '5-5.5'
								5.5<-6 = '5.5-6'
								6-High = '6-Max'
	;
%profile(bbb_yield_lag_2,bbb_yieldf,3.9);


Proc format;
value loan_spread_vf  	Low-4= '0-4'
						4<-6 = '4-6'
						6<-7.5= '6-7.5'
						7.5-High = '7.5-Max'
	;
%profile(loan_spread_v,loan_spread_vf,-1);




	Proc format; 
	value loan_bal		Low-75000  = 'L1:0-75k'
						75000<-200000  = 'L2:75k-200k'
						200000<-300000  = 'L3:150k-300k'
						300000-High = 'L4:300k-Max'
	;

%profile(current_balance,loan_bal,0);
%profile2(original_balance,0);


	Proc format; 
	value TTM			Low-16  = '0-16'
						16<-32 = '16-32'
						32<-48 = '32-48'
						48-High = '48-Max'
	;
%profile(TTM_Q,TTM,0);

	Proc format; 
	value loan_age			Low-12  = '0-12'
							12<-28 = '12-28'
							28<-40 = '28-40'
							40-High = '40-Max'
	;
%profile(loan_age_Q,loan_age,0);

	Proc format; 
	value t1m			Low-0.15  = '0-0.15'
							0.15<-0.75 = '0.15-0.75'
							0.75<-1.5 = '0.75-1.5'
							1.5-High = '1.5-Max'
	;
%profile(tb1m,t1m,0);
	Proc format; 
	value t1m			Low-0.15  = '0-0.15'
						0.15<-2 = '0.15-2'
						2-High = '2-Max'
	;
%profile(tb1m,t1m,0);

Proc format; 
	value t3m				Low-0.15  = '0-0.15'
							0.15<-2 = '0.15-0.2'
							2-High = '2-Max'
	;
%profile(tb3m,t3m,0);


	Proc format; 
	value t6m				Low-0.15  = '0-0.15'
							0.15<-2 = '0.15-2'
							2-High = '2-Max'
	;
%profile(tb6m,t6m,0);


	Proc format; 
	value t1y				Low-0.20  = '0.1-0.20'
							0.20<-0.32 = '0.20-0.32'
							0.32<-0.59 = '0.32-0.59'
							0.59-High = '0.59-Max'
	;
%profile(tb1y,t1y,0.1);


/*this one is a bit tailored from quintiles*/
	Proc format; 
	value t2y				Low-0.75  = '0.23-0.75'
							0.75<-0.90 = '0.75-0.90'
							0.90<-1.35 = '0.90-1.35'
							1.35-High = '1.35-Max'
	;
%profile(tb2y,t2y,0.23);

	Proc format; 
	value t2y				Low-0.4  = '0.23-0.75'
							0.4<-0.75 = '0.5-0.75'
							0.75<-0.90 = '0.75-0.90'
							0.90<-1.35 = '0.90-1.35'
							1.35-High = '1.35-Max'
	;
%profile(tb2y,t2y,0.23);

/*this one is a bit tailored from quintiles*/
	Proc format; 
	value t3y				Low-0.99  = '0.30-0.99'
							0.99<-1.3 = '0.99-1.3' 
							1.3<-1.64 = '1.3-1.64'
							1.64-High = '1.64-Max'
	;
%profile(tb3y,t3y,0.31);

	Proc format; 
	value t5y				Low-1.37  = '0.6-1.37'
							1.37<-1.74 = '1.37-1.74'
							1.74<-2.54 = '1.74-2.54'
							2.54-High = '2.54-Max'
	;
%profile(tb5y,t5y,0.62);


	Proc format; 
	value t5y				Low-1  = '0.6-1'
							1<-1.25 = '1-1.25'
							1.25<-1.50 = '1.25-1.50'
							1.50<-1.75 = '1.50-1.75' 
							1.75<-2= '1.75-2'
							2<-2.25 = '2-2.25'
							2.25<-2.6= '2.25-2.6'
							2.5-High = '2.5-Max'
	;
%profile(tb5y,t5y,0.62);
	Proc format; 
	value t7y				Low-1.75  = '1.04-1.75'
							1.75<-2.22 = '1.75-2.22'
							2.22<-2.93 = '2.22-2.93'
							2.93-High = '2.93-Max'
	;
%profile(tb7y,t7y,1.04);

	Proc format; 
	value t10y				Low-2.17  = '1.65-2.17'
							2.17<-2.71 = '2.17-2.71'
							2.71<-3.47 = '2.71-3.47'
							3.47-High = '3.47-Max'
	;
%profile(tb10y,t10y,1.65);

	Proc format; 
	value t20y				Low-2.665  = '2.2-2.665'
							2.665<-3.345 = '2.665-3.345'
							3.345<-4.295 = '3.345-4.295'
							4.295-High = '4.295-Max'
	;
%profile(tb20y,t20y,2.2);

	Proc format; 
	value t30y				Low-2.90  = '2.54-2.90'
							2.90<-3.56 = '2.90-3.56'
							3.56<-4.38 = '3.56-4.38'
							4.38-High = '4.38-Max'
	;
%profile(tb30y,t30y,2.54);


	Proc format; 
	value dpd01f			Low-2  = 'L1:0-2'
							2<-4 = 'L2:2-4'
							4<-8 = 'L3:4-8'
							8-High = 'L4:8-Max'
	;
%profile(dpd0129,dpd01f,0);

	Proc format; 
	value dpd30f			Low-1  = 'L1:0-1'
							1<-2 = 'L2:1-2'
							2-High = 'L4:2-Max'
	;
%profile(dpd3059,dpd30f,0);


	Proc format; 
	value dpd60f			Low-1  = 'L1:0-1'
							1-High = 'L2:1-Max'
	;
%profile(dpd6089,dpd60f,0);

Proc format; 
	value dpd90f			Low-1  = 'L1:0-1'
							1-High = 'L2:1-Max'
	;
%profile(dpd90,dpd90f,0);

Proc format; 
	value rgdpg_agf			Low - -0.5 = 'L1:-1.5 to -0.5' 
							-0.5<-1.25 = 'L2:-0.5 to 1.25'
							1.25<-1.95 = 'L3:1.25 to 1.95'
							1.95<- 2.75 = 'L4: 1.95 to 2.75'
							2.75-High	= 'L5: 2.75 to Max'
	;
%profile(RGDPg_AG,rgdpg_agf,-4.05);


Proc format; 
	value ngdp_qgf			Low - -1 = 'L1:(-3.20)-(-1)'
							-1<- 0.25 = 'L2:(-1)-0.25'
							0.25<- 1.5 = 'L3:0.25-1.5'
							1.5 <- 3 = 'L4:1.5-3'
							3-High = 'L5:3-Max'
	;

%profile(ngdp_qg,ngdp_qgf,-3.20);

	Proc format; 
	value rdi_agf		Low - 0.5    = '-3-0.5'
						0.5<-2 = '0.5-2'
						2<-4 ='2-4'
						4-High = '4-Max'
	;
%profile(rdi_ag,rdi_agf,-2.93);

Proc format; 
	value rdi_qgf		Low - -3    = '-16- -3'
						-3<-0 = '-3-0'
						0<-2 = '0-2'
						2<-3.75 = '2-3.75'
						3.75-High = '3.75-Max'
	;
%profile(rdi_qg,rdi_qgf,-15.90);



Proc format; 
value ccif					Low - 60 = '27-50'
							60<- 65 = '60-65'
							65<-70 = '65-70'
							70<-75 = '70-75'
							75<-85 = '75-85'
							85<-95 = '85-95'
							95-High = '95-Max'
	;
%profile(cci,ccif,26.9);


	Proc format; 
	value ndi_agf		Low - 0.25    = 'L1:(-1.74)-0.25'
						0.25<-2  = 'L2:0.25-2'
						2<-3.5 = 'L3:2-3.5'
						3.5<-5 = 'L4:3.5-5'
						5-High = 'L6:5-Max'
	;
%profile(ndi_ag,ndi_agf,-1.74);

	Proc format; 
	value urf			Low - 6    = '4.5-6'
						6<-8 = '6-8'
						8<-9.5 ='8-9.5'
						9.5-High = '9.5-Max'
	;
%profile(ur,urf,4.5);

Proc format; 
	value urf			Low - 5    = '4.5-5'
						5<-6 = '5-6'
						6<-7 = '6-7'
						7<-8 = '7-8'
						8<-8.5 ='8-8.5'
						8.5<-9 = '8.5-9'
						9<-9.5 ='9-9.5'
						9.5-High = '9.5-Max'
	;
%profile(ur,urf,4.5);




	Proc format; 
	value urf			Low - 6    = '4.5-6'
						6<-8 = '6-8'
						8<-9.5 ='8-9.5'
						9.5-High = '9.5-Max'
	;
%profile(nyur,urf,5);


	Proc format; 
	value ur_adf			Low - -1 = 'L1:(-1.3)-(-1)'
							-1<-0 = 'L2:(-1)-0'
							0<-1.4  = 'L3: 0-1.4'
							1.4<-3.3 ='L4: 1.4-3.3'
							3.3-High = 'L5: 3.3-Max'
	;
%profile(ur_ad,ur_adf,-1.3);

Proc format; 
	value caur_adf			Low - -1.40 = 'L1:(-1.5)-(-1.4)'
							-1.40<--1.2 = 'L2:(-1.4)-(-1.2)'
							-1.2<-0.4  = 'L3: (-1.2)-0.4'
							0.4<-3 ='L4: 0.4-3'
							3-High = 'L5: 3-Max'
	;
%profile(caur_yd,caur_adf,-1.5);

Proc format; 
	value caur_qdf			Low - -0.35 = 'L1:(-0.4)-(-0.35)'
							-0.35<--0.2 = 'L2:(-0.35)-(-0.2)'
							-0.2<-0.1 = 'L3: (-0.2)-0.1'
							0.1<-0.4 ='L4: 0.1-0.4'
						0.4-High = 'L5: 0.4-Max'
	;
%profile(caur_qd,caur_qdf,-0.4);

Proc format; 
	value nyur_qdf			Low - -0.35 = 'L1:(-0.4)-(-0.35)'
							-0.35<--0.15 = 'L2:(-0.35)-(-0.15)'
							-0.15<-0.1 = 'L3: (-0.15)-0.1'
							0.1<-0.8 ='L4: 0.1-0.8'
						0.8-High = 'L5: 0.8-Max'
	;
%profile(nyur_qd,nyur_qdf,-0.4);

Proc format; 
	value nyur_adf			Low - -1 = 'L1:(-1.4)-(-1)'
							-1<--0.5 = 'L2:(-1)-(-0.5)'
							-0.5<-0.5  = 'L3: (-0.5)-0.4'
							0.5<-1.6 ='L4: 0.4-1.6'
							1.6-High = 'L5: 1.6-Max'
	;
%profile(nyur_yd,nyur_adf,-1.4);


Proc format; 
	value njur_qdf			Low - -0.4 = 'L1:(-0.5)-(-0.4)'
							-0.4<--0.1 = 'L2:(-0.4)-(-0.1)'
							-0.1<-0.1 = 'L3: (-0.1)-0.1'
							0.1<-1 ='L4: 0.1-1'
						1-High = 'L5: 1-Max'
	;
%profile(njur_qd,njur_qdf,-0.5);

Proc format; 
	value njur_adf			Low - -1.3 = 'L1:(-1.9)-(-1.3)'
							-1.3<--0.4 = 'L2:(-1.3)-(-0.4)'
							-0.4<-0  = 'L3: (-0.4)-0'
							0<-2 ='L4: 0-2'
							2-High = 'L5: 2-Max'
	;
%profile(njur_yd,njur_adf,-1.9);

Proc format; 
	value hpif					Low - 150 = '0-150'
								150<- 160 = '150-160'
								160<-170 = '160-170'
								170-High = '170-Max'
	;
%profile(hpi,hpif,137.6);


Proc format; 
	value cahpif				Low - 400 = '375-400'
								400<- 450 = '400-450'
								450<-500 = '450-500'
								500<-525 = '500-525'
								525-High = '525-Max'
	;
%profile(cahpi,cahpif,375);

%profile2(cahpi,0);


Proc format; 
	value njhpif					Low - 455 = '0-455'
								455<- 470 = '455-470'
								470<-480 = '470-480'
								480<-510 = '480-510'
								510-High = '510-Max'
	;
%profile(njhpi,njhpif,450.38);


	Proc format; 
	value nyhpif					Low -563 = '0-563'
								563<- 580 = '563-580'
								580<-600 = '580-600'							
								600-High = '600-Max'
	;
%profile(nyhpi,nyhpif,555.71);




	Proc format; 
	value cahpi_qgf					Low - -2 = 'L1: min-(-2)'
								-2<- 0.25 = 'L2: (-2)-0.25'
								0.25<-1.5 = 'L3: 0.25-1.5'
								1.5<-2.75 = 'L4: 1.5-2.75'
								2.75-High = 'L5: 2.75-Max'
	;
%profile(cahpi_qg_lag_3,cahpi_qgf,-8.45);


Proc format; 
	value cahpi_agf					Low - -6 = 'L1: min-(-6)'
								-6<-0.9 = 'L2: (-6)-0.9'
								0.9<-6 = 'L3: 0.9-6'
								6-High = 'L4: 6-Max'
	;
%profile(cahpi_ag,cahpi_agf,-25.94);

	Proc format; 
	value nyhpi_qgf					Low - -0.5 = 'L1: min-(-0.5)'
								-0.5<- 0.2 = 'L2: (-0.5)-0.2'
								0.2<-1 = 'L3: 0.2-1'
								1-High = 'L5: 1-Max'
	;
%profile(nyhpi_qg,nyhpi_qgf,-2.64);


	Proc format; 
	value nyhpi_agf				Low - -4 = 'L1: min-(-4)'
								-4<--0.25 = 'L2: (-4)-(-0.25)'
								-0.25<-2.5 = 'L3: (-0.25)-2.5'
								2.5<-3.5 = 'L4: 2.5-3.5'
								3.5-High = 'L5: 3.5-Max'
	;
%profile(nyhpi_ag,nyhpi_agf,-6.18);

	Proc format; 
	value njhpi_qgf					Low - -1 = 'L1: min-(-1)'
								-1<- 0.1 = 'L2: (-1)-0.1'
								0.1<-0.65 = 'L3: 0.1-0.65'
								0.65-High = 'L4: 0.65-Max'
	;
%profile(njhpi_qg,njhpi_qgf,-3.27);


	Proc format; 
	value njhpi_agf				Low - -4 = 'L1: min-(-4)'
								-4<--0.5 = 'L2: (-4)-(-0.5)'
								-0.5<-1.25 = 'L3: (-0.5)-1.25'
								1.25<-3.24 ='L4: 1.25-3.24'
								3.24-High = 'L5: 3.24-Max'
	;
%profile(njhpi_ag,njhpi_agf,-7.46);

	Proc format; 
	value hpi_QGf				Low - -1 = 'L1: min-(-1)'
								-1<- 0 = 'L2: (-1)-0'
								0<-0.75 = 'L3: 0-0.75'
								0.75<-1.60 = 'L4: 0.75-1.6'
								1.60-High = 'L5: 1.6-Max'
	;
%profile(hpi_QG,hpi_QGf,-5.15);


Proc format; 
value hpi_AGf				Low - -2 = 'L1: min-(-2)'
							-2<- 0 = 'L2: (-2)-0'
							0<-4 = 'L3:0-4'
							4-High = 'L4: 4-Max'
	;
%profile(hpi_AG,hpi_AGf,-17.6);


Proc format; 
	value CPPIF				Low - 165 = '150-165'
							165<- 180 = '165-180'
							180<- 195 = '180-195'
							195<-220 = '195-220'
							220-High = '220-Max'
	;
%profile(CPPI,CPPIf,150);


Proc format; 
value CPPI_AGf				Low- -5 = 'L1: min-(-5)'
							-5<--2 = 'L2: (-5)-(-2)'
							-2<-0 = 'L3: (-2) - 0'
							0<-2 ='L4: 0-2'
							2<-4 = 'L5: 2-4'
							4-High = 'L6: 4-Max'
	;
%profile(CPPI_AG_lag_3,CPPI_AGf,-11);
%profile2(CPPI_AG, -6)

Proc format; 
value CPPI_qGf				Low - -4 = 'L1: min-(-4)'
							-4<- 0 = 'L2: (-2)-0'
							0<- 2 = 'L3: 0-2'
							2<-5= 'L4: 2-5' 
							5-High = 'L6: 5-Max'
	;
%profile(CPPI_qG,CPPI_qGf,-8);
%profile2(cppi_qg, -10);


Proc format; 
value djia_qgf				Low - -10 = 'L1:8044-11000'
							-10<- -0.5 = 'L2:11000-13000'
							-0.5<- 2.25 = 'L313000-15000'
							2.25<-7 = 'L4:15000-20000'
							7-High = 'L5:20000-Max'
	;
%profile(djia_qg,djia_qgf,-26.68);

Proc format; 
value djia_agf				Low - -10 = 'L1:8044-11000'
							-10<- -2 = 'L2:11000-13000'
							-2<- 8 = 'L313000-15000'
							8<-25 = 'L4:15000-20000'
							25-High = 'L5:20000-Max'
	;
%profile(djia_ag,djia_agf,-50.16);

Proc format; 
value NCREIF_Property_Indexf					
							Low-1620 = '1400-1600'
							1620<- 1900 = '1600-1900'
							1900<-2150 = '1900-2100'
							2150<-2450 = '2100-2450'
							2450-High = '2450-Max'
	;
%profile(NCREIF_Property_Index,NCREIF_Property_Indexf,1401.33);

Proc format; 
value NCREIF_Property_Index_qgf					
							Low--2 = '1400-1600'
							-2<- 2.35 = '1600-1900'
							2.35<-2.7 = '1900-2100'
							2.7<-3.3 = '2100-2450'
							3.3-High = '2450-Max'
	;
%profile(NCREIF_Property_Index_qg,NCREIF_Property_Index_qgf,-8.65);

Proc format; 
value NCREIF_Property_Index_agf					
							Low--2 = '1400-1600'
							-2<- 5 = '1600-1900'
							5<-10= '1900-2100'
							10<-12.75 = '2100-2450'
							12.75-High = '2450-Max'
	;
%profile(NCREIF_Property_Index_ag,NCREIF_Property_Index_agf,-24.97);




Proc format; 
value KOGDP_agf					Low - 3 = 'L1:8044-11000'
							3<- 4.5 = 'L2:11000-13000'
							4.5<- 6.5 = 'L313000-15000'
							6.5<-8.5 = 'L4:15000-20000'
							8.5-High = 'L5:20000-Max'
	;
%profile(KOGDP_ag,KOGDP_agf,1.77);

Proc format; 
value KOGDP_qgf					Low - 0.1 = '-0.1'
							0.1<- 0.6 = '0.1-0.6'
							0.6<- 1.1 = '0.6-1.1'
							1.1<-2.15 = '1.1-2.15'
							2.15-High = '2.15-Max'
	;
%profile(KOGDP_qg,KOGDP_qgf,-2.26);

Proc format; 
value cci_qgf					Low - -15 = '27-50'
							-15<- -2 = '50-70'
							-2<- 5 = '60-70'
							5<-25 = '70-95'
							25-High = '95-Max'
	;
%profile(cci_qg,cci_qgf,-46.32);


Proc format; 
value SPR10_QDF				Low - -0.6 = 'L1: min-(-0.6)'
							-0.6<- -0.1 = 'L2: (-0.6)-(-0.1)'
							-0.1<- 0.15 = 'L3: (-0.1)-0.15'
							0.15<-0.5 = 'L4: 0.15-0.4'
							0.5-High = 'L6: 0.6-Max'
	;
%profile(SPR10_QD,SPR10_QDF,-1.5);



Proc format; 
value CPPI_QGf				Low - -5 = 'L1: min-(-5)'
							-5<- -2 = 'L2: (-5)-0.25'
							-2<-2 = 'L4: 1.77-4'
							2<-4 = 'L5: 1.77-4'
							4-High = 'L6: 4-Max'
	;
%profile(CPPI_QG,CPPI_QGf,-16.06);


Proc format; 
value SPR10F				Low - 2 = '1.3-1.5'
							2<- 2.5 = '1.8-2.2'
							2.5<-4 = '2.7-4'
							4-High = '4-Max'

	;
%profile(SPR10,SPR10f,1.3);

Proc format; 
value SPR10_QDF				Low - -0.6 = 'L1: min-(-0.6)'
							-0.6<- -0.1 = 'L2: (-0.6)-(-0.1)'
							-0.1<- 0.15 = 'L3: (-0.1)-0.15'
							0.15<-0.5 = 'L4: 0.15-0.4'
							0.5-High = 'L6: 0.6-Max'
	;
%profile(SPR10_QD,SPR10_QDF,-1.5);



Proc format; 
value SPR10_QDF				Low - -0.6 = 'L1: min-(-0.6)'
							-0.6<- -0.1 = 'L2: (-0.6)-(-0.1)'
							-0.1<- 0.15 = 'L3: (-0.1)-0.15'
							0.15<-0.5 = 'L4: 0.15-0.4'
							0.5-High = 'L6: 0.6-Max'
	;
%profile(SPR10_QD,SPR10_QDF,-1.5);

Proc format; 
value SPR10_aDF				Low - 0 = 'L1: min-0'
							0<- 0.5 = 'L2: 0-0.5'
							0.5<- 1 = 'L3: 0.5-1'
							1-High = 'L4: 1-Max'
	;
%profile(SPR10_aD,SPR10_aDF,-1.5);
