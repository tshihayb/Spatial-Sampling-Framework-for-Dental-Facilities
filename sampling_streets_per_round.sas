/*======================================================================
  Spatial Sampling Framework for Dental Facilities - Riyadh City
  Stratified random street sampling used to plan the field census.

  Method: draw a 10% simple random sample (SRS) of streets WITHOUT
  replacement, STRATIFIED by district, with a fixed seed so the sample
  is reproducible.

  Author:  Talal S. Alshihayb
  License: MIT (see LICENSE)

  INPUT
    A per-round street list (one row per street) with a district
    identifier (District_Number) and a street id (id).
    The per-round working workbooks (e.g. "Group A Dist.xlsx") are not
    part of the public release; the equivalent published street frame is
    District_population_street_data_stripped.xlsx, sheets
    "Random selection of streets" and "Clean deduplicated streets".

  HOW TO RUN
    Set &indir to the folder holding your input workbook and &infile to
    its name, then submit.
======================================================================*/

%let indir  = .;                 /* folder containing the input workbook */
%let infile = Group A Dist.xlsx; /* per-round street list                */

libname samp "&indir";

/* Import the street list (one row per street) */
proc import out = work.one
            datafile = "&indir./&infile"
            dbms = xlsx replace;
    sheet = "Sheet1";
    getnames = yes;
run;

proc print data = work.one (obs = 10); run;

/* Randomly shuffle, then draw the 10% stratified SRS (fixed seed = 100) */
data work.one;
    set work.one;
    call streaminit(100);
    RNG = rand('Normal', 0, 1);
run;

proc sort data = work.one; by RNG;             run;
proc sort data = work.one; by District_Number; run;

proc surveyselect data = work.one
                  out    = work.two
                  samprate = 0.10
                  method = srs
                  seed   = 100
                  stats;
    strata District_Number;
run;

proc sort  data = work.two; by id; run;
proc print data = work.two;         run;
