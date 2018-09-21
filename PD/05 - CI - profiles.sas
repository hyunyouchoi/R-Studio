/* clear the log window*/
dm 'log' clear; 
LIBNAME Interim "";
LIBNAME Out "";

ods graphics on;

data out.df_ci_dev;
	set  out.df_boh_final_base (where=(filedate<= '31Mar16'd and portfolio_id="CI"));
run;

%macro profile(var,format,xmin);
	Proc sort data=out.df_ci_dev_cleaned2; 
	by &var.;
	run;
	Proc means data=out.df_ci_dev_cleaned2 nway noprint order=formatted ;
	var y;
	Class &var. ;
	format &var. &format..;
	output out=summary_stats mean(y)=default_rate;
	run;

	proc export data= summary_stats
	outfile= 'C:\Users\sghassemi\Documents\Projects\Bank of Hope\SAS Interim 2\CI_summary_stats.csv'
	dbms = csv
	REPLACE;

	Proc sgplot data=summary_stats;
	xaxis min=&xmin.;
	series x=&var. y=default_rate;
	band x=&var. lower=0 upper=_FREQ_ / fillattrs=ltgray y2axis transparency=0.5 legendlabel='Obs';
	run;
	%mend;

%macro profile2(var,xmin);
	Proc sort data=out.df_ci_dev_cleaned2; 
	by &var.;
	run;
	Proc means data=out.df_ci_dev_cleaned2 nway noprint order=formatted ;
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

/*loan rating profile */

Proc means data=out.df_ci_dev nway noprint order=formatted ;
	var y;
	Class boh_rating;
	output out=summary_stats mean(y)=default_rate;
	run;

Proc sgplot data=summary_stats;

series x=boh_rating y=default_rate;
band x=boh_rating lower=0 upper=_FREQ_ / fillattrs=ltgray y2axis transparency=0.75 legendlabel='Obs';
	run;


/* NAICS*/ 

Proc means data=out.df_ci_dev nway noprint order=formatted ;
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

/* Loan Spread */ 
Proc format;
value loan_spread_vf  	Low-1= '0-1'
						1<-2 = '1-2'
						2<-3 = '2-3'
						3<-4 = '3-4'
						4<-5 = '4-5'
						5<-6 ='5-6'
						6<-7= '6-7'
						7<-8 = '7-8'
						8<-9 = '8-9'
						9<-10 = '9-10'
						10-High = 'a: 11-Max'
	;
%profile(loan_spread_v,loan_spread_vf,-1)

/* DJIA */

Proc format; 
value djiaf					Low - 9500 = 'L1: 8044-9500'
							9500<- 10000 = 'L2: 9500-10000'							
							10000<- 12000 = 'L3: 10000-12000'
							12000<- 14000 = 'L4: 12000-14000'
							14000<- 16000 = 'L5: 14000-16000'
							16000<-20000 = 'L6: 16000-20000'
							20000-High = 'L7: 20000-Max'
	;
%profile(djia,djiaf,8044.2);


%profile2(djia_ag, -40);


Proc format; 
value djia_qgf					Low - -10 = 'L1: Low - -20'
							-10<- -5 = 'L2: -20 - -5'							
							-5<- -2 = 'L3: -5 - -2'
							-2<- 0 = 'L4: -2 - 0'
							0<- 2  = 'L5: 0-2'
							2<-4 = 'L6: 2-4'
							4<-6 = 'L7: 4-6'
							6<-8 = 'L8: 6-8'
							8-High = 'L9: 8-Max'
	;

%profile(djia_qg_lag_2,djia_qgf,-30);

/* CAUR */
Proc format; 
	value CAurf			Low - 6 = 'L1: 5.5-6'
						6<-7 = 'L2: 6-7'
						7<-8 = 'L3: 7-8'
						8<-9 ='L4: 8-9'
						9<-11 = 'L5: 9-11'
						11-High = 'L6: 11-Max'
	;
%profile(CAur,CAurf,5.5);


Proc format; 
	value CAur_ydf			Low - -1.25 = 'L1: Low- (-1.5)'
						-1.25<--0.75 = 'L2: (-1.5)-(-1)'
						-0.75<-0 = 'L3: (-1)-0'
						0<-1 ='L5: 0-1'
						1<-2 = 'L6: 1-1.5'
						2<-3 = 'L7: 1.5-2.25'
						3-High = 'L9: 3-Max'
	;
%profile(CAur_yd_lag_3,CAur_ydf,-2);

/*RDI*/ 

Proc format; 
	value rdi_qgf		Low - 0 = 'L1: Low-0'
					/*	-1<-0 = 'L2: (-1)-0' */
						0<-1= 'L2: 0-1'
						1<-1.5 = 'L3: 1-1.5'
						1.5<-2.5 = 'L4: 1.5-2.5'
						2.5<-3.5 ='L6: 2.5-3.5'
						3.5<-5 ='L7: 3.5-5'
						5<-7 = 'L8: 5-7'
						7-High = 'L9: 7-Max'
	;
%profile(rdi_qg_lag_3,rdi_qgf,0);


Proc format; 
	value ndi_qgf		Low - -3 = 'L1: Low-(-2)'
						-3<--1 = 'L2: (-3)-(-1)'
						-1<-0 = 'L3: (-1)-0'
						0<-0.5= 'L4: 0-1'
						0.5<-1 = 'L5: 0.5-1'
						1<-1.5 = 'L6: 1-1.5'
						1.5<-2.5 = 'L5: 2-3'
						2.5<-4 ='L6: 3-4'
						4<-6 ='L7: 4-6'
						6-High = 'L8: 6-Max'
	;
%profile(ndi_qg_lag_3,ndi_qgf,-3);

/* GDP*/

%profile2(ngdp_qg, -5);
	Proc format; 
	value rgdp_qgf			Low - -2 = 'L1: min-(-2)'
							-2 <- -1 = 'L2:(-2)-(-1)'
							-1<-0 = 'L3:(-1)-0'
							0<- 1 = 'L4:0-1'
							1<-2 = 'L5: 1-2'
							2<-3 ='L6: 2-3'
							3-High = 'L9: 3-Max'
	;
%profile(rgdp_qg,rgdp_qgf,-3);

Proc format; 
	value ngdp_qgf			Low - 0 = 'L1: min-(0)'
							0<- 1 = 'L2:0-1'
							1<-2.5 = 'L3:1-2.5'
							2.5<- 3 = 'L4:2.5-3'
							3<-3.5 = 'L5: 3-3.5'
							3.5<-4 = 'L6: 3-4'
							4<-5 = 'L7: 4-5'
							5<-6 ='L8: 5-6'
							6<-7='L9: 6-6.75' 
							7-High = 'L92: 6.75-Max'
	;
%profile(ngdp_qg,ngdp_qgf,-1);








Proc format; 
	value CPPIF				Low - 175 = '150-175'
							175<- 200 = '175-200'
							200<-220 = '200-220'
							220-High = '220-Max'
	;
%profile(CPPI,CPPIf,150);


Proc format; 
	value CPPI_agF				Low - 175 = '150-175'
							175<- 200 = '175-200'
							200<-220 = '200-220'
							220-High = '220-Max'
	;
%profile(CPPI,CPPI_agf,150);

/*spr10*/
Proc format; 
	value spr10F			Low - 1 = '0-1'
							1<- 2 = '1-2'
							2<-2.75 = '2-2.75'
							2.75<-3.25 = '2.75-3.25'
							3.25-High = '3.25-Max'
	;
%profile(Spr_10,spr10f,0);

Proc format; 
	value spr10_qdF			Low - -0.5 = 'L1: Min-(-1)'
							/*-1<- -0.5 = 'L2: (-1)-(-0.5)'*/
							-0.5<-0 = 'L3: (-0.5)-0'
							0<-0.5 = 'L4: 0-0.5'
							0.5-High= 'L5: 0.5-Max'
	;
%profile(spr10_qd,spr10_qdf,-2);

/*tb10y*/ 
Proc format; 
	value t10y				Low-2.17  = '1.65-2.17'
							2.17<-2.71 = '2.17-2.71'
							2.71<-3.25 = '2.71-3.25'
							3.25-High = '3.25-Max'
	;
%profile(i10y,t10y,1.65);

Proc format; 
	value t10y_qd			Low- -0.5  = 'L1: (-1)-(-0.5)'
							-0.5<--0.25 = 'L2: (-0.5)-(-0.25)'
							-0.25<-0 = 'L3: (-0.25)-0'
							0<-0.25 = 'L4: 0-0.25'
							0.25<-0.5 = 'L5: 0.25-0.5'
							0.5-High = 'L6: 0.5-Max'
	;
%profile(i10y_qd,t10y_qd,-1);

Proc format; 
	value t10y_ad			Low- -1  = 'L1: Min-(-0.5)'
							-1<--0.5 = 'L2: (-1) - (-0.5)'
							-0.5<--0.1 = 'L3: (-0.5)-(-0.25)'
							-0.1<-0.1 = 'L4: (-0.25)-0'
							0.1<-0.5 = 'L5: 0-0.25'
							0.5-High = 'L7: 0.5-Max'
	;
%profile(i10y_ad,t10y_ad,-1);

/*CAHPI*/ 

Proc format; 
	value cahpif				Low -390 = '375-400'
								390<- 425 = '400-425'
								425<-450 = '425-450'
								450<-475 = '450-475'
								475<-500 = '475-500'
								500-High = '500-Max'
	;
%profile(cahpi,cahpif,375);



Proc format; 
	value cahpi_qgf					Low - -2 = 'L1: min-(-2)'
								-2<- 0.25 = 'L2: (-2)-0.25'
								0.25<-1.5 = 'L3: 0.25-1.5'
								1.5<-2.75 = 'L4: 1.5-2.75'
								2.75-High = 'L5: 2.75-Max'
	;
%profile(cahpi_qg,cahpi_qgf,-8.45);


Proc format; 
	value cahpi_agf				Low - -5 = 'L1: min-(-5)'
								-5<--2 = 'L2: (-5)-(-2)'
								-2<--1 = 'L3: (-2)-(-1)'
								-1<-0 = 'L4: (-1)-0'
								0<-5 = 'L5: 0-5'
								5<-10 = 'L6: 5-10'
								10-High = 'L7: 10-Max'
	;
%profile(cahpi_ag,cahpi_agf,-5);
%profile_in (cahpi_ag,cahpi_agf,-5);


/*BBB_yield*/


Proc format; 
	value bbb_yieldf			Low - 4 = '3.9-4'
								4<-4.2 = '4-4.2'
								4.2<-4.4 = '4.2-4.4'
								4.4<-4.6  = '4.4-4.6'
								4.6<-4.8 = '4.6-4.8'
								4.8<-5 = '4.8-5'
								5<-5.2 = '5-5.2'
								5.2<-5.4 = '5.2-5.4'
								5.4<-5.6 = '5.4-5.6'
								5.6<-5.8 = '5.6-5.8'
								5.8<-6='5.8-6' 
								6<-7 = '6-7'
								7-High = '7-Max'
	;
%profile(bbb_yield,bbb_yieldf,3.9);

Proc format; 
	value bbb_yield_qdf			Low - -1 = 'L1:(-1.4)-(-0.5)'
								-1<--0.25 = 'L2:(-0.5)-0'
								-0.25<-0.3 = 'L3:0-0.3'
								0.3<-0.8 ='L4:0.3-0.8'
								0.8-High = 'L5:0.8-Max'
	;
%profile(bbb_yield_qd,bbb_yield_qdf,-1.4);

Proc format; 
	value bbb_yield_adf			Low - -0.7 = 'L1: (-3.3)-(-0.7)'
								-0.7<-0 = 'L2: (-0.7)-0'
								0<-0.5 = 'L3: 0-0.5'
								0.5<-1.1 = 'L4: 0.5-1.1'
								1.1-High = 'L5: 1.1-Max'
	;
%profile(bbb_yield_ad,bbb_yield_adf,-3.3);

/*DPD*/
Proc format; 
	value dpd01f			Low-0  = '0'
							0<-3	='1-3'
							/*1<-2	='2'*/	
							3<-6 = '4-6'
							6-High = '6-Max'
	;
%profile(dpd0129,dpd01f,0);


Proc format; 
	value dpd30f			Low-0  = '0'
							0<-1 = '1'
							1<-2 ='2' 
							2-High = '3-Max'
	;
%profile(dpd3059,dpd30f,0);


Proc format; 
value gross_pdi_equipmentf				Low - 800 = '600-800'
										800<-900 ='800-900'
										900<-990 ='900-1000'
										990-High = '990-Max'
					;
%profile(gross_pdi_equipment,gross_pdi_equipmentf,600);







%profile2(gross_pdi_equipment_lag_2,0);

Proc format; 
value ccif				Low - 35 = '27-30'
						35<-40 ='30-37'
						40<-45 ='37-45'
						45<-50 ='45-50'
						50<- 60 = '50-60'
						60<- 70 = '60-70'
						70<-80 = '70-80'
						80<-95 = '80-95'
						95-High = '95-Max'
	;
%profile(cci,ccif,26.9);


Proc format; 
value pobf				Low - 5 = 'a: 0-5'
						5<-	10 = 'b: 5-10'
						10<-20 ='c: 10-20'
						20<-30 ='d: 20-30'
						30<-35 = 'e: 30-35'
						35<-50 ='f: 35-50'
						50<- 60 = 'g: 50-60'
						60<- 75 = 'h: 60-75'
						75<-85 = 'i: 75-85'
						85<-90 = 'j: 80-90'
						/*90<-95 = 'm: 90-95'*/
						90-High = 'k: 95-Max'
	;
%profile(pob,pobf,0);


%profile2(caur_yd,-2);


Proc format; 
	value hpif					Low - 150 = '0-150'
								150<- 160 = '150-160'
								160<-170 = '160-170'
								170-High = '170-Max'
	;
%profile(hpi,hpif,137.6);


Proc format; 
value SPR10F				Low - 1.75 = '1.3-1.75'
							1.75<- 2.25 = '1.75-2.25'
							2.25<-3.25 = '2.25-3.25'
							3.25-High = '3.25-Max'

	;
%profile(SPR10,SPR10f,1.3);

Proc format; 
value SPR10F				Low - 1.5 = '1.3-1.5'
							1.5<- 1.75 = '1.5-1.75'
							1.75<-2 = '1.75-2'
							2<-2.25 = '2-2.25'
							2.25<-2.5 = '2.25-2.5'
							2.5<-2.75 = '2.5-2.75'
							2.75<-3 = '2.75-3'
							3<-3.25 = '3-3.25'
							3.25-High = '3.25-Max'

	;
%profile(SPR10,SPR10f,1.3);


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
value NCREIF_Property_Index_agf					
							Low--2 = 'L1: (-25)- (-2)'
							-2<- 5 = 'L2: (-2)-5'
							5<-10= 'L3: 5-10'
							10-High = 'L4: 10-Max'
	;
%profile(NCREIF_Property_Index_ag,NCREIF_Property_Index_agf,-24.97);


Proc format; 
value KOGDP_agf					Low - 3 = '1.77-3'
							3<- 4.5 = '3-4.5'
							4.5<- 6.5 = '4.5-6.5'
							6.5<-8.5 = '6.5-8.5'
							8.5-High = '8.5-Max'
	;
%profile(KOGDP_ag,KOGDP_agf,1.77);



Proc format;
value loan_spread_vf  	Low-4= '0-4'
						4<-6 = '4-6'
						6<-7= '6-7'
						7-High = '7-Max'
	;
%profile(loan_spread_v,loan_spread_vf,-1);

%profile2(loan_spread_v, -3);
;

Proc format;
	value loan_intf 	Low-4.5= '0-4.5'
						4.5<-6.25 = '4.5-6.25'
						6.25<-8.25  = '6.25-8.25'
						8.25-High = '8.25-Max'
	;
%profile(loan_int,loan_intf,0);


	Proc format; 
	value t2y				Low-0.75  = '0.23-0.75'
							0.75<-0.90 = '0.75-0.90'
							0.90<-1.35 = '0.90-1.35'
							1.35-High = '1.35-Max'
	;
%profile(tb2y,t2y,0.23);


	Proc format; 
	value t3y				Low-0.99  = '0.30-0.99'
							0.99<-1.4 = '0.99-1.4' 
							1.4<-1.95 = '1.4-1.95'
							1.95-High = '1.95-Max'
	;
%profile(tb3y,t3y,0.31);


	Proc format; 
	value t7y				Low-1.75  = '1.04-1.75'
							1.75<-2.15 = '1.75-2.15'
							2.15<-3 = '2.15-3'
							3-High = '3-Max'
	;
%profile(tb7y,t7y,1.04);







	Proc format; 
	value dpd60f			Low-1  = 'L1: 0-1'
							1-High = 'L2: 1-Max'
	;
%profile(dpd6089,dpd60f,0);

	Proc format; 
	value dpd90f			Low-1  = 'L1:0-1'
							1-High = 'L2:1-Max'
	;
%profile(dpd90,dpd90f,0);




%profile2(bbb_yield,0);



Proc format; 
	value hpif					Low - 150 = '0-150'
								150<- 160 = '150-160'
								160<-170 = '160-170'
								170-High = '170-Max'
	;
%profile(hpi,hpif,137.6);


	Proc format; 
	value njhpi_qgf				Low - -1 = 'L1: min-(-1)'
								-1<- 0.1 = 'L2: (-1)-0.1'
								0.1<-0.65 = 'L3: 0.1-0.65'
								0.65-High = 'L4: 0.65-Max'
	;
%profile(njhpi_qg,njhpi_qgf,-3.27);



Proc format; 
	value hpi_QGf				Low - -1 = 'L1: min-(-1)'
								-1<- 0 = 'L2: (-1)-0'
								0<-0.75 = 'L3: 0-0.75'
								0.75<-1.60 = 'L4: 0.75-1.6'
								1.60-High = 'L5: 1.6-Max'
	;
%profile(hpi_QG,hpi_QGf,-5.15);


Proc format; 
value KOGDP_qgf					Low - 0.1 = '-0.1'
							0.1<- 0.6 = '0.1-0.6'
							0.6<- 1.1 = '0.6-1.1'
							1.1<-2.15 = '1.1-2.15'
							2.15-High = '2.15-Max'
	;
%profile(KOGDP_qg,KOGDP_qgf,-2.26);

Proc format; 
value ccif				Low - 50 = '27-50'
							50<- 60 = '50-70'
							60<- 70 = '60-70'
							70<-95 = '70-95'
							95-High = '95-Max'
	;
%profile(cci,ccif,26.9);

Proc format; 
	value t1m			Low-0.10  = '0-0.15'
							0.10<-1 = '0.15-0.75'
							1<-2 = '0.75-1.5'
							2-High = '1.5-Max'
	;
%profile(tb1m,t1m,0);
Proc format; 
	value t1m			Low-0.15  = '0-0.15'
						0.15<-2 = '0.15-2'
						2-High = '2-Max'
	;
%profile(tb1m,t1m,0);

	Proc format; 
	value t1y				Low-0.20  = '0.1-0.20'
							0.20<-0.32 = '0.20-0.32'
							0.32<-0.59 = '0.32-0.59'
							0.59-High = '0.59-Max'
	;
%profile(tb1y,t1y,0.1);

	Proc format; 
	value t5y				Low-1.37  = '0.6-1.37'
							1.37<-1.74 = '1.37-1.74'
							1.74<-2.54 = '1.74-2.54'
							2.54-High = '2.54-Max'
	;
%profile(tb5y,t5y,0.62);

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
	value rdi_qgf		Low - -4 = 'L3: Low-(-4)'
						-4<-0.75 = 'L4:(-3)-0.5'
						0.75<-2.5 = 'L5:0.5-2.25'
						2.5<-3.75 = 'L6:2.25-3.75'
						3.75-High = 'L7:3.75-Max'
	;
%profile(rdi_qg,rdi_qgf,-15.90);


Proc format; 
	value ndi_qgf		Low - -1    = 'L1:(-14.70)-(-1)'
						-1<-1 = 'L3:(-1)-2.25'
						1<-3= 'L4:2.25-4'
					
						3-High = 'L7:6-Max'
	;
%profile(ndi_qg,ndi_qgf,-14.70);


	Proc format; 
	value ndi_agf		Low - 0.25    = 'L1:(-1.74)-0.25'
						0.25<-2  = 'L2:0.25-2'
						2<-3.5 = 'L3:2-3.5'
						3.5<-5 = 'L4:3.5-5'
						5-High = 'L6:5-Max'
	;
%profile(ndi_ag,ndi_agf,-1.74);

Proc format; 
	value ur_qdf			Low - -0.2    = '4.5-5'
						
						-0.2<-0 = '6-7.25'
						0<-0.25= '7.25-8.6'
						0.25<-0.7  = '8-6'
						0.7<-1 ='8.6-9.5'
						1-High = '9.5-Max'
	;
%profile(ur_qd,ur_qdf,-0.5);


Proc format; 
	value caurf			Low - 7    = '4.5-6.25'
						7<-8 = '6.25-8'
						8<-9.25 ='8-9.25'
						9.25-High = '9.25-Max'
	;
%profile(caur,urf,5);


Proc format; 
	value ur_adf			Low - 0 = 'L1:(-1)-0'
							0<-0.5 = 'L2: 0-0.5'
							0.5<-1  = 'L3: 0.5-1'
							1<-1.5 ='L4: 1-1.5'
							1.5<-2 = 'L5: 1.5-2'
							2<-2.75 = 'L6: 2-2.75'
							2.75<-3.3 = 'L7: 2.75-3.3'
							3.3-High = 'L8: 3.3-Max'
	;
%profile(ur_ad,ur_adf,-1.3);

Proc format; 
	value caur_adf			Low - -1.40 = 'L1:(-1.5)-(-1.4)'
							-1.40<--1.2 = 'L2:(-1.4)-(-1.2)'
							-1.2<--0.5  = 'L3: (-1.2)-(-0.5)'
							-0.5<-0 ='L4: (-0.5)-0'
							0<-0.5 = 'L5: 0-1'
							0.5<-1 = 'L6:0.5-1'
							1<-3 ='L7: 1-3'
							3-High = 'L8: 3-Max'
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
	value nyur_adf			Low - -1.2 = 'L1:(-1.4)-(-1)'
							-1.2<--0.5 = 'L2:(-1)-(-0.5)'
							-0.5<-1  = 'L3: (-0.5)-0.4'
							1<-2 ='L4: 0.4-1.6'
							2-High = 'L5: 1.6-Max'
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

%profile2(cahpi_ag,-10);



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
	value njhpi_agf				Low - -4 = 'L1: min-(-4)'
								-4<--0.5 = 'L2: (-4)-(-0.5)'
								-0.5<-1.25 = 'L3: (-0.5)-1.25'
								1.25<-3.24 ='L4: 1.25-3.24'
								3.24-High = 'L5: 3.24-Max'
	;
%profile(njhpi_ag,njhpi_agf,-7.46);

Proc format; 
value CPPI_AGf				Low - -5 = 'L1: min-(-5)'
							-5<- 0 = 'L2: (-5)-0'
							0<- 5 = 'L3: 0-4'
							5-High = 'L6: 4-Max'
	;
%profile(CPPI_AG,CPPI_AGf,-41.41);

Proc format; 
value CPPI_QGf				Low - -5 = 'L1: min-(-5)'
							-5<- -2 = 'L2: (-5)-0.25'
							-2<-2 = 'L4: 1.77-4'
							2<-4 = 'L5: 1.77-4'
							4-High = 'L6: 4-Max'
	;
%profile(CPPI_QG,CPPI_QGf,-16.06);

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


Proc format; 
value djia_qgf					Low - -10 = 'L1:8044-11000'
							-10<- -0.5 = 'L2:11000-13000'
							-0.5<- 2.25 = 'L313000-15000'
							2.25<-7 = 'L4:15000-20000'
							7-High = 'L5:20000-Max'
	;
%profile(djia_qg,djia_qgf,-26.68);

Proc format; 
value djia_agf					Low - -12 = 'L1:8044-11000'
							-12<- -3 = 'L2:11000-13000'
							-3<- 3 = 'L313000-15000'
							3<-12 = 'L4:15000-20000'
							12-High = 'L5:20000-Max'
	;
%profile(djia_ag,djia_agf,-50.16);

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
value vixf					Low - 20 = 'L1:8044-11000'
							20<- 24 = 'L2:11000-13000'
							24<- 27 = 'L313000-15000'
							27<-35 = 'L4:15000-20000'
							35-High = 'L5:20000-Max'
	;
%profile(vix,vixf,17);

Proc format; 
value vix_qdf					Low - -7 = 'L1:8044-11000'
							-7<- -0.5 = 'L2:11000-13000'
							-0.5<- 5 = 'L313000-15000'
							5<-10 = 'L4:15000-20000'
							10-High = 'L5:20000-Max'
	;
%profile(vix_qd,vix_qdf,-24.2);


Proc format; 
value vix_adf					Low - -7 = 'L1:8044-11000'
							-7<- 0 = 'L2:11000-13000'
							0<- 3 = 'L313000-15000'
							3<-15 = 'L4:15000-20000'
							15-High = 'L5:20000-Max'
	;
%profile(vix_ad,vix_adf,-50.2);

Proc format; 
value cci_qgf					Low - -15 = '27-50'
							-15<- -2 = '50-70'
							-2<- 5 = '60-70'
							5<-25 = '70-95'
							25-High = '95-Max'
	;
%profile(cci_qg,cci_qgf,-46.32);


proc format; 
	value NIPA_Exports_qdf     Low- -50 = 'L1: (-68)-(-50)'
								-50<--30 = 'L2: (-50)-(-30)'
								-30<--20 = 'L3: (-30)-(-20)'
								-20<--10 = 'L4: (-20)-(-10)' 
								-10<-0 ='L5: (-10)-0'
								0<-10 = 'L6: 0-10'
								10<-20 = 'L7: 10-20'
								20<-40 = 'L8: 20-40'
								40-High = 'L9: 40-Max'
	;
%profile(NIPA_Exports_qd, NIPA_Exports_qdf, -70);



