libname samp 'C:\Users\Tshih\OneDrive\Private dental clinics analysis in Riyadh\Round 3';

PROC IMPORT OUT= WORK.one
            DATAFILE= "C:\Users\Tshih\OneDrive\Private dental clinics analysis in Riyadh\Round 3\Group A Dist.xlsx" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

PROC IMPORT OUT= WORK.one 
            DATAFILE= "C:\Users\Tshih\OneDrive\Private dental clinics analysis in Riyadh\Round 3\Group A Dist.xlsx"
            DBMS=EXCEL REPLACE;
     RANGE="Sheet1$";
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

proc print data=one;run;

/*Sorting the data randomly*/
data one;
	set one;
	call streaminit(100);
	RNG=rand('Normal',0,1);
	run;

proc print data=one;run;

proc sort data=one; by RNG;run;

proc print data=one;run;

proc sort data=one; by District_Number;run;

/*Simple random sample without replacement and seed specified*/
proc surveyselect data=one out=two SAMPrate=0.1
method=SRS seed=100 stats;
strata District_Number;
run;

proc sort data=two;by id;run;

proc print data=two;run;

