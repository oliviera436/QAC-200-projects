/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Monday, January 12, 2015     TIME: 7:51:18 PM
PROJECT: OlivieriA_SAS_project_01122015
PROJECT PATH: P:\QAC\qac200\students\aolivieri\Assignments\OlivieriA_SAS_project_01122015.egp
---------------------------------------- */

/* Library assignment for Local.MEPS */
Libname MEPS V9 'P:\QAC\qac200\students\aolivieri\Assignments' ;
/* Library assignment for Local.MEPS */
Libname MEPS V9 'P:\QAC\qac200\students\aolivieri\Assignments' ;


/* Conditionally delete set of tables or views, if they exists          */
/* If the member does not exist, then no action is performed   */
%macro _eg_conditional_dropds /parmbuff;
	
   	%local num;
   	%local stepneeded;
   	%local stepstarted;
   	%local dsname;
	%local name;

   	%let num=1;
	/* flags to determine whether a PROC SQL step is needed */
	/* or even started yet                                  */
	%let stepneeded=0;
	%let stepstarted=0;
   	%let dsname= %qscan(&syspbuff,&num,',()');
	%do %while(&dsname ne);	
		%let name = %sysfunc(left(&dsname));
		%if %qsysfunc(exist(&name)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;

			%end;
				drop table &name;
		%end;

		%if %sysfunc(exist(&name,view)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;
			%end;
				drop view &name;
		%end;
		%let num=%eval(&num+1);
      	%let dsname=%qscan(&syspbuff,&num,',()');
	%end;
	%if &stepstarted %then %do;
		quit;
	%end;
%mend _eg_conditional_dropds;


/* Build where clauses from stored process parameters */

%macro _eg_WhereParam( COLUMN, PARM, OPERATOR, TYPE=S, MATCHALL=_ALL_VALUES_, MATCHALL_CLAUSE=1, MAX= , IS_EXPLICIT=0);

  %local q1 q2 sq1 sq2;
  %local isEmpty;
  %local isEqual isNotEqual;
  %local isIn isNotIn;
  %local isString;
  %local isBetween;

  %let isEqual = ("%QUPCASE(&OPERATOR)" = "EQ" OR "&OPERATOR" = "=");
  %let isNotEqual = ("%QUPCASE(&OPERATOR)" = "NE" OR "&OPERATOR" = "<>");
  %let isIn = ("%QUPCASE(&OPERATOR)" = "IN");
  %let isNotIn = ("%QUPCASE(&OPERATOR)" = "NOT IN");
  %let isString = (%QUPCASE(&TYPE) eq S or %QUPCASE(&TYPE) eq STRING );
  %if &isString %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%");
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq D or %QUPCASE(&TYPE) eq DATE %then 
  %do;
    %let q1=%str(%");
    %let q2=%str(%"d);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq T or %QUPCASE(&TYPE) eq TIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"t);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq DT or %QUPCASE(&TYPE) eq DATETIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"dt);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else
  %do;
    %let q1=;
    %let q2=;
	%let sq1=;
    %let sq2=;
  %end;
  
  %if "&PARM" = "" %then %let PARM=&COLUMN;

  %let isBetween = ("%QUPCASE(&OPERATOR)"="BETWEEN" or "%QUPCASE(&OPERATOR)"="NOT BETWEEN");

  %if "&MAX" = "" %then %do;
    %let MAX = &parm._MAX;
    %if &isBetween %then %let PARM = &parm._MIN;
  %end;

  %if not %symexist(&PARM) or (&isBetween and not %symexist(&MAX)) %then %do;
    %if &IS_EXPLICIT=0 %then %do;
		not &MATCHALL_CLAUSE
	%end;
	%else %do;
	    not 1=1
	%end;
  %end;
  %else %if "%qupcase(&&&PARM)" = "%qupcase(&MATCHALL)" %then %do;
    %if &IS_EXPLICIT=0 %then %do;
	    &MATCHALL_CLAUSE
	%end;
	%else %do;
	    1=1
	%end;	
  %end;
  %else %if (not %symexist(&PARM._count)) or &isBetween %then %do;
    %let isEmpty = ("&&&PARM" = "");
    %if (&isEqual AND &isEmpty AND &isString) %then
       &COLUMN is null;
    %else %if (&isNotEqual AND &isEmpty AND &isString) %then
       &COLUMN is not null;
    %else %do;
	   %if &IS_EXPLICIT=0 %then %do;
           &COLUMN &OPERATOR %unquote(&q1)&&&PARM%unquote(&q2)
	   %end;
	   %else %do;
	       &COLUMN &OPERATOR %unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2))
	   %end;
       %if &isBetween %then 
          AND %unquote(&q1)&&&MAX%unquote(&q2);
    %end;
  %end;
  %else 
  %do;
	%local emptyList;
  	%let emptyList = %symexist(&PARM._count);
  	%if &emptyList %then %let emptyList = &&&PARM._count = 0;
	%if (&emptyList) %then
	%do;
		%if (&isNotin) %then
		   1;
		%else
			0;
	%end;
	%else %if (&&&PARM._count = 1) %then 
    %do;
      %let isEmpty = ("&&&PARM" = "");
      %if (&isIn AND &isEmpty AND &isString) %then
        &COLUMN is null;
      %else %if (&isNotin AND &isEmpty AND &isString) %then
        &COLUMN is not null;
      %else %do;
	    %if &IS_EXPLICIT=0 %then %do;
            &COLUMN &OPERATOR (%unquote(&q1)&&&PARM%unquote(&q2))
	    %end;
		%else %do;
		    &COLUMN &OPERATOR (%unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2)))
		%end;
	  %end;
    %end;
    %else 
    %do;
       %local addIsNull addIsNotNull addComma;
       %let addIsNull = %eval(0);
       %let addIsNotNull = %eval(0);
       %let addComma = %eval(0);
       (&COLUMN &OPERATOR ( 
       %do i=1 %to &&&PARM._count; 
          %let isEmpty = ("&&&PARM&i" = "");
          %if (&isString AND &isEmpty AND (&isIn OR &isNotIn)) %then
          %do;
             %if (&isIn) %then %let addIsNull = 1;
             %else %let addIsNotNull = 1;
          %end;
          %else
          %do;		     
            %if &addComma %then %do;,%end;
			%if &IS_EXPLICIT=0 %then %do;
                %unquote(&q1)&&&PARM&i%unquote(&q2) 
			%end;
			%else %do;
			    %unquote(%nrstr(&sq1))&&&PARM&i%unquote(%nrstr(&sq2)) 
			%end;
            %let addComma = %eval(1);
          %end;
       %end;) 
       %if &addIsNull %then OR &COLUMN is null;
       %else %if &addIsNotNull %then AND &COLUMN is not null;
       %do;)
       %end;
    %end;
  %end;
%mend;

/* save the current settings of XPIXELS and YPIXELS */
/* so that they can be restored later               */
%macro _sas_pushchartsize(new_xsize, new_ysize);
	%global _savedxpixels _savedypixels;
	options nonotes;
	proc sql noprint;
	select setting into :_savedxpixels
	from sashelp.vgopt
	where optname eq "XPIXELS";
	select setting into :_savedypixels
	from sashelp.vgopt
	where optname eq "YPIXELS";
	quit;
	options notes;
	GOPTIONS XPIXELS=&new_xsize YPIXELS=&new_ysize;
%mend;

/* restore the previous values for XPIXELS and YPIXELS */
%macro _sas_popchartsize;
	%if %symexist(_savedxpixels) %then %do;
		GOPTIONS XPIXELS=&_savedxpixels YPIXELS=&_savedypixels;
		%symdel _savedxpixels / nowarn;
		%symdel _savedypixels / nowarn;
	%end;
%mend;

/* ---------------------------------- */
/* MACRO: enterpriseguide             */
/* PURPOSE: define a macro variable   */
/*   that contains the file system    */
/*   path of the WORK library on the  */
/*   server.  Note that different     */
/*   logic is needed depending on the */
/*   server type.                     */
/* ---------------------------------- */
%macro enterpriseguide;
%global sasworklocation;
%local tempdsn unique_dsn path;

%if &sysscp=OS %then %do; /* MVS Server */
	%if %sysfunc(getoption(filesystem))=MVS %then %do;
        /* By default, physical file name will be considered a classic MVS data set. */
	    /* Construct dsn that will be unique for each concurrent session under a particular account: */
		filename egtemp '&egtemp' disp=(new,delete); /* create a temporary data set */
 		%let tempdsn=%sysfunc(pathname(egtemp)); /* get dsn */
		filename egtemp clear; /* get rid of data set - we only wanted its name */
		%let unique_dsn=".EGTEMP.%substr(&tempdsn, 1, 16).PDSE"; 
		filename egtmpdir &unique_dsn
			disp=(new,delete,delete) space=(cyl,(5,5,50))
			dsorg=po dsntype=library recfm=vb
			lrecl=8000 blksize=8004 ;
		options fileext=ignore ;
	%end; 
 	%else %do; 
        /* 
		By default, physical file name will be considered an HFS 
		(hierarchical file system) file. 
		*/
		%if "%sysfunc(getoption(filetempdir))"="" %then %do;
			filename egtmpdir '/tmp';
		%end;
		%else %do;
			filename egtmpdir "%sysfunc(getoption(filetempdir))";
		%end;
	%end; 
	%let path=%sysfunc(pathname(egtmpdir));
    %let sasworklocation=%sysfunc(quote(&path));  
%end; /* MVS Server */
%else %do;
	%let sasworklocation = "%sysfunc(getoption(work))/";
%end;
%if &sysscp=VMS_AXP %then %do; /* Alpha VMS server */
	%let sasworklocation = "%sysfunc(getoption(work))";                         
%end;
%if &sysscp=CMS %then %do; 
	%let path = %sysfunc(getoption(work));                         
	%let sasworklocation = "%substr(&path, %index(&path,%str( )))";
%end;
%mend enterpriseguide;

%enterpriseguide

ODS PROCTITLE;
OPTIONS DEV=ACTIVEX;
GOPTIONS XPIXELS=0 YPIXELS=0;
FILENAME EGSRX TEMP;
ODS tagsets.sasreport13(ID=EGSRX) FILE=EGSRX
    STYLE=HtmlBlue
    STYLESHEET=(URL="file:///C:/Program%20Files/SASHome/SASEnterpriseGuide/6.1/Styles/HtmlBlue.css")
    NOGTITLE
    NOGFOOTNOTE
    GPATH=&sasworklocation
    ENCODING=UTF8
    options(rolap="on")
;

/*   START OF NODE: Assign Project Library (MEPS)   */
%LET _CLIENTTASKLABEL='Assign Project Library (MEPS)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\aolivieri\Assignments\OlivieriA_SAS_project_01122015.egp';
%LET _CLIENTPROJECTNAME='OlivieriA_SAS_project_01122015.egp';

GOPTIONS ACCESSIBLE;
LIBNAME MEPS  "P:\QAC\qac200\students\aolivieri\Assignments" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\aolivieri\Assignments\OlivieriA_SAS_project_01122015.egp';
%LET _CLIENTPROJECTNAME='OlivieriA_SAS_project_01122015.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\Data\MEPS";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 7:50:05 PM
   By task: Data Set Attributes

   Input Data: P:\QAC\qac200\Data\MEPS\meps_fullyr_2012.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsFormeps_fullyr_2012);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=ECLIB000.meps_fullyr_2012 OUT=WORK.SUCOUT1;

RUN;

DATA WORK.CONTContentsFormeps_fullyr_2012(LABEL="Contents Details for meps_fullyr_2012");
   SET WORK.SUCOUT1;
RUN;

PROC DELETE DATA=WORK.SUCOUT1;
RUN;

%LET _LINESIZE=%SYSFUNC(GETOPTION(LINESIZE));

PROC SQL;
CREATE VIEW WORK.SCVIEW AS 
	SELECT DISTINCT memname LABEL="Table Name", 
			memlabel LABEL="Label", 
			memtype LABEL="Type", 
			crdate LABEL="Date Created", 
			modate LABEL="Date Modified", 
			nobs LABEL="Number of Obs.", 
			charset LABEL="Char. Set", 
			protect LABEL="Password Protected", 
			typemem LABEL="Data Set Type" FROM WORK.CONTContentsFormeps_fullyr_2012
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.CONTContentsFormeps_fullyr_2012 OUT=WORK.CONTContentsFormeps_fullyr_2012;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.CONTContentsFormeps_fullyr_2012
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

PROC REPORT DATA=WORK.SCTABLE NOWINDOWS; 
   FORMAT TYPE _EG_VARTYPE.; 
   DEFINE LABEL / DISPLAY WIDTH=&_LINESIZE; 
   LABEL NAME="Name" LABEL="Label" TYPE="Type" LENGTH="Length" INFORMAT="Informat" FORMAT="Format"; 
   BY memname NOTSORTED;  
   COLUMN name varnum type format label length;  
 QUIT;  

PROC SQL;
	DROP TABLE WORK.SCTABLE;
	DROP VIEW WORK.SCVIEW;
QUIT;

PROC CATALOG CATALOG=WORK.FORMATS;
   DELETE _EG_VARTYPE / ENTRYTYPE=FORMAT;
RUN;
OPTIONS BYLINE;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: assign 1 variables   */
%LET _CLIENTTASKLABEL='assign 1 variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\aolivieri\Assignments\OlivieriA_SAS_project_01122015.egp';
%LET _CLIENTPROJECTNAME='OlivieriA_SAS_project_01122015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MEPS.MEPS_FULLYR_2012_subset);

PROC SQL;
   CREATE TABLE MEPS.MEPS_FULLYR_2012_subset(label="MEPS_FULLYR_2012_subset") AS 
   SELECT t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.ALIMP12X, 
          t1.AMASST12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.CANCERDX, 
          t1.CHDDX, 
          t1.CSHIMP12, 
          t1.DSCHNV53, 
          t1.DUPERSID, 
          t1.DVGEN12, 
          t1.DVORTH12, 
          t1.DVTOT12, 
          t1.EDUCYR, 
          t1.EDUYRDEG, 
          t1.EMPHDX, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.ERDEXP12, 
          t1.ERFEXP12, 
          t1.ERTOT12, 
          t1.FAMINC12, 
          t1.HIBPDX, 
          t1.HIDEG, 
          t1.INSCOV12, 
          t1.INSURC12, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.K6SUM42, 
          t1.LEUKAGED, 
          t1.LEUKREMS, 
          t1.LUNGAGED, 
          t1.LUNGREMS, 
          t1.LYMPAGED, 
          t1.LYMPREMS, 
          t1.MARRY12X, 
          t1.MCAID12, 
          t1.MCS42, 
          t1.MIDX, 
          t1.OHRTAGED, 
          t1.OHRTDX, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.RACEAX, 
          t1.RACEBX, 
          t1.RACETHX, 
          t1.RACEV1X, 
          t1.RACEVER, 
          t1.RACEWX, 
          t1.REGION12, 
          t1.SAQELIG, 
          t1.SEX, 
          t1.SFFLAG42, 
          t1.SPOUIN12, 
          t1.STRKDX, 
          t1.UNEMP12X, 
          t1.DEPDNT12, 
          t1.NEVILL42, 
          t1.WRHLTH42, 
          t1.EDRECODE
      FROM EC100008.meps_fullyr_2012 t1
      WHERE t1.AGE12X >= 18;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: SAS program code   */
%LET SYSLAST=MEPS.MEPS_FULLYR_2012_SUBSET;
%LET _CLIENTTASKLABEL='SAS program code';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\aolivieri\Assignments\OlivieriA_SAS_project_01122015.egp';
%LET _CLIENTPROJECTNAME='OlivieriA_SAS_project_01122015.egp';
%LET _SASPROGRAMFILE='P:\QAC\qac200\students\aolivieri\Assignments\SAS code\SAS program code.sas';

GOPTIONS ACCESSIBLE;

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 08, 2015 at 11:29:25 AM
   By task: Data Set Attributes1

   Input Data: Local:MEPS.MEPS_FULLYR_2012_SUBSET
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsForMEPS_FULLYR_2012_);
TITLE "Data set attributes for subset data set";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=MEPS.MEPS_FULLYR_2012_SUBSET ;

RUN;



GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;


/*   START OF NODE: One-Way Frequencies for Adult MEPS subset   */
%LET _CLIENTTASKLABEL='One-Way Frequencies for Adult MEPS subset';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\aolivieri\Assignments\OlivieriA_SAS_project_01122015.egp';
%LET _CLIENTPROJECTNAME='OlivieriA_SAS_project_01122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 7:50:06 PM
   By task: One-Way Frequencies for Adult MEPS subset

   Input Data: Local:MEPS.MEPS_FULLYR_2012_SUBSET
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:MEPS.MEPS_FULLYR_2012_SUBSET
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.ADAPPT42, T.ADCAPE42, T.ADCLIM42, T.ADCMPD42, T.ADCMPM42, T.ADCMPY42, T.ADDAYA42, T.ADDOWN42, T.ADDPRS42, T.ADDRBP42, T.ADEFRT42, T.ADEGMC42, T.ADEXPL42, T.ADEZUN42, T.ADFFRM42, T.ADFHLP42, T.ADGENH42, T.ADHECR42, T.ADHOPE42
		     , T.ADILCR42, T.ADILWW42, T.ADINSA42, T.ADINSB42, T.ADINST42, T.ADINTR42, T.ADLANG42, T.ADLIST42, T.ADMALS42, T.ADMWLM42, T.ADNDCR42, T.ADNERV42, T.ADNRGY42, T.ADNSMK42, T.ADOVER42, T.ADPAIN42, T.ADPALS42, T.ADPRTM42, T.ADPRX42
		     , T.ADPWLM42, T.ADRESP42, T.ADREST42, T.ADRISK42, T.ADRTCR42, T.ADRTWW42, T.ADSAD42, T.ADSMOK42, T.ADSOCA42, T.ADSPEC42, T.ADSPRF42, T.ADTLHW42, T.ADWRTH42, T.AGE12X, T.ALIMP12X, T.AMASST12, T.AMCHIR12, T.AMDRC12, T.CANCERDX
		     , T.CHDDX, T.CSHIMP12, T.DSCHNV53, T.DVGEN12, T.DVORTH12, T.DVTOT12, T.EDUCYR, T.EDUYRDEG, T.EMPHDX, T.EMPST31, T.EMPST42, T.EMPST53, T.ERDEXP12, T.ERFEXP12, T.ERTOT12, T.FAMINC12, T.HIBPDX, T.HIDEG, T.INSCOV12, T.INSURC12
		     , T.IPDIS12, T.IPNGTD12, T.K6SUM42, T.LEUKAGED, T.LEUKREMS, T.LUNGAGED, T.LUNGREMS, T.LYMPAGED, T.LYMPREMS, T.MARRY12X, T.MCAID12, T.MCS42, T.MIDX, T.OHRTAGED, T.OHRTDX, T.PCS42, T.PHQ242, T.RACEAX, T.RACEBX, T.RACETHX
		     , T.RACEV1X, T.RACEVER, T.RACEWX, T.REGION12, T.SAQELIG, T.SEX, T.SFFLAG42, T.SPOUIN12, T.STRKDX, T.UNEMP12X, T.DEPDNT12, T.NEVILL42, T.WRHLTH42
	FROM MEPS.MEPS_FULLYR_2012_SUBSET(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for 2012 Adult MEPS subset";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Andrew Olivieri";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES ADAPPT42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCAPE42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCLIM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPD42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPY42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDAYA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDOWN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDPRS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDRBP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEFRT42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEGMC42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEXPL42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEZUN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADFFRM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADFHLP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADGENH42 / MISSPRINT  SCORES=TABLE;
	TABLES ADHECR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADHOPE42 / MISSPRINT  SCORES=TABLE;
	TABLES ADILCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADILWW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINSA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINSB42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINTR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADLANG42 / MISSPRINT  SCORES=TABLE;
	TABLES ADLIST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADMALS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADMWLM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNDCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNERV42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNRGY42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNSMK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADOVER42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPAIN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPALS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPRTM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPRX42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPWLM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRESP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADREST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRISK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRTCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRTWW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSAD42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSMOK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSOCA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSPEC42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSPRF42 / MISSPRINT  SCORES=TABLE;
	TABLES ADTLHW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADWRTH42 / MISSPRINT  SCORES=TABLE;
	TABLES AGE12X / MISSPRINT  SCORES=TABLE;
	TABLES ALIMP12X / MISSPRINT  SCORES=TABLE;
	TABLES AMASST12 / MISSPRINT  SCORES=TABLE;
	TABLES AMCHIR12 / MISSPRINT  SCORES=TABLE;
	TABLES AMDRC12 / MISSPRINT  SCORES=TABLE;
	TABLES CANCERDX / MISSPRINT  SCORES=TABLE;
	TABLES CHDDX / MISSPRINT  SCORES=TABLE;
	TABLES CSHIMP12 / MISSPRINT  SCORES=TABLE;
	TABLES DSCHNV53 / MISSPRINT  SCORES=TABLE;
	TABLES DVGEN12 / MISSPRINT  SCORES=TABLE;
	TABLES DVORTH12 / MISSPRINT  SCORES=TABLE;
	TABLES DVTOT12 / MISSPRINT  SCORES=TABLE;
	TABLES EDUCYR / MISSPRINT  SCORES=TABLE;
	TABLES EDUYRDEG / MISSPRINT  SCORES=TABLE;
	TABLES EMPHDX / MISSPRINT  SCORES=TABLE;
	TABLES EMPST31 / MISSPRINT  SCORES=TABLE;
	TABLES EMPST42 / MISSPRINT  SCORES=TABLE;
	TABLES EMPST53 / MISSPRINT  SCORES=TABLE;
	TABLES ERDEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES ERFEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES ERTOT12 / MISSPRINT  SCORES=TABLE;
	TABLES FAMINC12 / MISSPRINT  SCORES=TABLE;
	TABLES HIBPDX / MISSPRINT  SCORES=TABLE;
	TABLES HIDEG / MISSPRINT  SCORES=TABLE;
	TABLES INSCOV12 / MISSPRINT  SCORES=TABLE;
	TABLES INSURC12 / MISSPRINT  SCORES=TABLE;
	TABLES IPDIS12 / MISSPRINT  SCORES=TABLE;
	TABLES IPNGTD12 / MISSPRINT  SCORES=TABLE;
	TABLES K6SUM42 / MISSPRINT  SCORES=TABLE;
	TABLES LEUKAGED / MISSPRINT  SCORES=TABLE;
	TABLES LEUKREMS / MISSPRINT  SCORES=TABLE;
	TABLES LUNGAGED / MISSPRINT  SCORES=TABLE;
	TABLES LUNGREMS / MISSPRINT  SCORES=TABLE;
	TABLES LYMPAGED / MISSPRINT  SCORES=TABLE;
	TABLES LYMPREMS / MISSPRINT  SCORES=TABLE;
	TABLES MARRY12X / MISSPRINT  SCORES=TABLE;
	TABLES MCAID12 / MISSPRINT  SCORES=TABLE;
	TABLES MCS42 / MISSPRINT  SCORES=TABLE;
	TABLES MIDX / MISSPRINT  SCORES=TABLE;
	TABLES OHRTAGED / MISSPRINT  SCORES=TABLE;
	TABLES OHRTDX / MISSPRINT  SCORES=TABLE;
	TABLES PCS42 / MISSPRINT  SCORES=TABLE;
	TABLES PHQ242 / MISSPRINT  SCORES=TABLE;
	TABLES RACEAX / MISSPRINT  SCORES=TABLE;
	TABLES RACEBX / MISSPRINT  SCORES=TABLE;
	TABLES RACETHX / MISSPRINT  SCORES=TABLE;
	TABLES RACEV1X / MISSPRINT  SCORES=TABLE;
	TABLES RACEVER / MISSPRINT  SCORES=TABLE;
	TABLES RACEWX / MISSPRINT  SCORES=TABLE;
	TABLES REGION12 / MISSPRINT  SCORES=TABLE;
	TABLES SAQELIG / MISSPRINT  SCORES=TABLE;
	TABLES SEX / MISSPRINT  SCORES=TABLE;
	TABLES SFFLAG42 / MISSPRINT  SCORES=TABLE;
	TABLES SPOUIN12 / MISSPRINT  SCORES=TABLE;
	TABLES STRKDX / MISSPRINT  SCORES=TABLE;
	TABLES UNEMP12X / MISSPRINT  SCORES=TABLE;
	TABLES DEPDNT12 / MISSPRINT  SCORES=TABLE;
	TABLES NEVILL42 / MISSPRINT  SCORES=TABLE;
	TABLES WRHLTH42 / MISSPRINT  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: RECODE VARIABLES   */
%LET _CLIENTTASKLABEL='RECODE VARIABLES';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\aolivieri\Assignments\OlivieriA_SAS_project_01122015.egp';
%LET _CLIENTPROJECTNAME='OlivieriA_SAS_project_01122015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.SUBSET_MEPS_2012_MANAGED);

PROC SQL;
   CREATE TABLE WORK."SUBSET_MEPS_2012_MANAGED"n AS 
   SELECT t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.CANCERDX, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.HIBPDX, 
          t1.EMPHDX, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.CHDDX, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.MIDX, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.STRKDX, 
          t1.ADILWW42, 
          t1.WRHLTH42, 
          t1.NEVILL42, 
          t1.DEPDNT12, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.DUPERSID, 
          t1.EDUCYR, 
          t1.EDUYRDEG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.HIDEG, 
          t1.K6SUM42, 
          t1.MARRY12X, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.RACEAX, 
          t1.RACEBX, 
          t1.RACETHX, 
          t1.RACEV1X, 
          t1.RACEVER, 
          t1.RACEWX, 
          t1.REGION12, 
          t1.SAQELIG, 
          t1.SEX, 
          t1.SFFLAG42, 
          t1.SPOUIN12, 
          t1.DSCHNV53, 
          t1.LEUKAGED, 
          t1.LEUKREMS, 
          t1.LUNGAGED, 
          t1.LUNGREMS, 
          t1.LYMPAGED, 
          t1.LYMPREMS, 
          t1.OHRTAGED, 
          t1.OHRTDX, 
          t1.INSCOV12, 
          t1.INSURC12, 
          t1.MCAID12, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.AMASST12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.DVGEN12, 
          t1.DVORTH12, 
          t1.DVTOT12, 
          t1.ERDEXP12, 
          t1.ERFEXP12, 
          t1.ERTOT12, 
          t1.ALIMP12X, 
          t1.CSHIMP12, 
          t1.FAMINC12, 
          t1.UNEMP12X, 
          t1.EDRECODE, 
          /* HEALTH_IN_GENERAL */
            (CASE 
               WHEN -1 = t1.ADGENH42 THEN .
               WHEN -8 = t1.ADGENH42 THEN .
               WHEN -9 = t1.ADGENH42 THEN .
               ELSE t1.ADGENH42
            END) LABEL="Health in general (recoded missing)" AS HEALTH_IN_GENERAL, 
          /* HEALTH_LIMITS_MOD_ACTIVITIES */
            (CASE 
               WHEN -1 = t1.ADDAYA42 THEN .
               WHEN -9 = t1.ADDAYA42 THEN .
               ELSE t1.ADDAYA42
            END) LABEL="Health limits mod activities (recoded values)" AS HEALTH_LIMITS_MOD_ACTIVITIES, 
          /* HLTH_LIMITS_STAIRS */
            (CASE 
               WHEN -1 = t1.ADCLIM42 THEN .
               WHEN -8 = t1.ADCLIM42 THEN .
               WHEN -9 = t1.ADCLIM42 THEN .
               ELSE t1.ADCLIM42
            END) LABEL="Health limits stairs (recoded values)" AS HLTH_LIMITS_STAIRS, 
          /* ACCMP_LESS_PHY */
            (CASE 
               WHEN -1 = t1.ADPALS42 THEN .
               WHEN -9 = t1.ADPALS42 THEN .
               ELSE t1.ADPALS42
            END) LABEL="Accmp less bc of phy (recoded value)" AS ACCMP_LESS_PHY, 
          /* WORK_LIMIT_PHY */
            (CASE 
               WHEN -1 = t1.ADPWLM42 THEN .
               WHEN -9 = t1.ADPWLM42 THEN .
               ELSE t1.ADPWLM42
            END) LABEL="Work limit 4wks PHY (recoded values)" AS WORK_LIMIT_PHY, 
          /* ACCMP_LESS_MNT */
            (CASE 
               WHEN -1 = t1.ADMALS42 THEN .
               WHEN -9 = t1.ADMALS42 THEN .
               ELSE t1.ADMALS42
            END) LABEL="Accmp less bc of mnt prbs (recoded values)" AS ACCMP_LESS_MNT, 
          /* WORK_LIMIT_MNT */
            (CASE 
               WHEN -1 = t1.ADMWLM42 THEN .
               WHEN -7 = t1.ADMWLM42 THEN .
               WHEN -8 = t1.ADMWLM42 THEN .
               WHEN -9 = t1.ADMWLM42 THEN .
               ELSE t1.ADMWLM42
            END) LABEL="Work limit bc mnt probs (recoded values)" AS WORK_LIMIT_MNT, 
          /* PAIN_LIMITS_NORMAL_WORK */
            (CASE 
               WHEN -1 = t1.ADPAIN42 THEN .
               WHEN -9 = t1.ADPAIN42 THEN .
               ELSE t1.ADPAIN42
            END) LABEL="Pain limits normal work (recoded values)" AS PAIN_LIMITS_NORMAL_WORK, 
          /* FELT_CALM_PEACEFUL */
            (CASE 
               WHEN -1 = t1.ADCAPE42 THEN .
               WHEN -8 = t1.ADCAPE42 THEN .
               WHEN -9 = t1.ADCAPE42 THEN .
               ELSE t1.ADCAPE42
            END) LABEL="Felt calm peaceful (recoded values)" AS FELT_CALM_PEACEFUL, 
          /* HAD_ALOT_ ENRGY */
            (CASE 
               WHEN -1 = t1.ADNRGY42 THEN .
               WHEN -9 = t1.ADNRGY42 THEN .
               ELSE t1.ADNRGY42
            END) LABEL="had a lot of energy (recoded values)" AS 'HAD_ALOT_ ENRGY'n, 
          /* FELT_DOWNHEARTED_DEPR */
            (CASE 
               WHEN -1 = t1.ADDOWN42 THEN .
               WHEN -8 = t1.ADDOWN42 THEN .
               WHEN -9 = t1.ADDOWN42 THEN .
               ELSE t1.ADDOWN42
            END) LABEL="Felt downhearted depressed (recoded values)" AS FELT_DOWNHEARTED_DEPR, 
          /* HLTH_STOPPED_SOC_ACTIV */
            (CASE 
               WHEN -1 = t1.ADSOCA42 THEN .
               WHEN -9 = t1.ADSOCA42 THEN .
               ELSE t1.ADSOCA42
            END) LABEL="health stopped soc activ (recoded values)" AS HLTH_STOPPED_SOC_ACTIV, 
          /* MARITAL_STATUS */
            (CASE 
               WHEN -7 = t1.MARRY12X THEN .
               WHEN -9 = t1.MARRY12X THEN .
               ELSE t1.MARRY12X
            END) LABEL="Marital status (recoded values)" AS MARITAL_STATUS, 
          /* MARITAL_STATUS_W_SPOUSE */
            (CASE 
               WHEN -9 = t1.SPOUIN12 THEN .
               ELSE t1.SPOUIN12
            END) LABEL="Marital status with spouse (recoded values)" AS MARITAL_STATUS_W_SPOUSE, 
          /* YEARS_EDU_FIRST_ENTER_MEPS */
            (CASE 
               WHEN -1 = t1.EDUCYR THEN .
               WHEN -7 = t1.EDUCYR THEN .
               WHEN -8 = t1.EDUCYR THEN .
               WHEN -9 = t1.EDUCYR THEN .
               ELSE t1.EDUCYR
            END) LABEL="Years of educ when first entered MEPS (recoded values)" AS YEARS_EDU_FIRST_ENTER_MEPS, 
          /* YEARS_EDU_HIGHEST_DEGREE */
            (CASE 
               WHEN -1 = t1.EDUYRDEG THEN .
               WHEN -7 = t1.EDUYRDEG THEN .
               WHEN -8 = t1.EDUYRDEG THEN .
               WHEN -9 = t1.EDUYRDEG THEN .
               ELSE t1.EDUYRDEG
            END) LABEL="Year of education or highest degree (recoded values)" AS YEARS_EDU_HIGHEST_DEGREE, 
          /* HIGHEST_DEGREE_FIRST_ENTERED */
            (CASE 
               WHEN -1 = t1.HIDEG THEN .
               WHEN -7 = t1.HIDEG THEN .
               WHEN -8 = t1.HIDEG THEN .
               WHEN -9 = t1.HIDEG THEN .
               ELSE t1.HIDEG
            END) LABEL="Highest degree first entered MEPS (recoded values)" AS HIGHEST_DEGREE_FIRST_ENTERED, 
          /* EMPLOYMENT_STATUS_3_1 */
            (CASE 
               WHEN -1 = t1.EMPST31 THEN .
               WHEN -7 = t1.EMPST31 THEN .
               WHEN -8 = t1.EMPST31 THEN .
               WHEN -9 = t1.EMPST31 THEN .
               ELSE t1.EMPST31
            END) LABEL="Employment status 3/1 (recoded values)" AS EMPLOYMENT_STATUS_3_1, 
          /* EMPLOYMENT_STATUS_4_2 */
            (CASE 
               WHEN -1 = t1.EMPST42 THEN .
               WHEN -7 = t1.EMPST42 THEN .
               WHEN -8 = t1.EMPST42 THEN .
               WHEN -9 = t1.EMPST42 THEN .
               ELSE t1.EMPST42
            END) LABEL="Employment status 4/2 (recoded values)" AS EMPLOYMENT_STATUS_4_2, 
          /* EMPLOYMENT_STATUS_5_3 */
            (CASE 
               WHEN -7 = t1.EMPST53 THEN .
               WHEN -8 = t1.EMPST53 THEN .
               WHEN -9 = t1.EMPST53 THEN .
               ELSE t1.EMPST53
            END) LABEL="Employment status 5/3 (recoded values)" AS EMPLOYMENT_STATUS_5_3, 
          /* CURRENTLY_SMOKE */
            (CASE 
               WHEN -1 = t1.ADSMOK42 THEN .
               WHEN -9 = t1.ADSMOK42 THEN .
               ELSE t1.ADSMOK42
            END) LABEL="Currently smoke (recoded values)" AS CURRENTLY_SMOKE, 
          /* EASY_GETTING_MED_CARE */
            (CASE 
               WHEN -1 = t1.ADEGMC42 THEN .
               WHEN -9 = t1.ADEGMC42 THEN .
               ELSE t1.ADEGMC42
            END) LABEL="Easy getting needed med care (recoded values)" AS EASY_GETTING_MED_CARE, 
          /* NEED_CARE_TEST_TREATMENT */
            (CASE 
               WHEN -1 = t1.ADNDCR42 THEN .
               WHEN -8 = t1.ADNDCR42 THEN .
               WHEN -9 = t1.ADNDCR42 THEN .
               ELSE t1.ADNDCR42
            END) LABEL="need any care, test, treatment (recoded values)" AS NEED_CARE_TEST_TREATMENT, 
          /* ILL_INJURY_IMMED_CARE */
            (CASE 
               WHEN -1 = t1.ADILCR42 THEN .
               WHEN -9 = t1.ADILCR42 THEN .
               ELSE t1.ADILCR42
            END) LABEL="ill injury needing immed care (recoded values)" AS ILL_INJURY_IMMED_CARE, 
          /* MORE_LIKELY_TAKE_RISKS */
            (CASE 
               WHEN -1 = t1.ADRISK42 THEN .
               WHEN -7 = t1.ADRISK42 THEN .
               WHEN -8 = t1.ADRISK42 THEN .
               WHEN -9 = t1.ADRISK42 THEN .
               ELSE t1.ADRISK42
            END) LABEL="more likely to take risks (recoded values)" AS MORE_LIKELY_TAKE_RISKS, 
          /* CAN_OVERCOME_ILLS_WO_MED */
            (CASE 
               WHEN -1 = t1.ADOVER42 THEN .
               WHEN -7 = t1.ADOVER42 THEN .
               WHEN -8 = t1.ADOVER42 THEN .
               WHEN -9 = t1.ADOVER42 THEN .
               ELSE t1.ADOVER42
            END) LABEL="can overcome ill without med help (recoded values)" AS CAN_OVERCOME_ILLS_WO_MED, 
          /* OVERALL_RATING_FEELINGS */
            (CASE 
               WHEN -1 = t1.PHQ242 THEN .
               WHEN -9 = t1.PHQ242 THEN .
               ELSE t1.PHQ242
            END) LABEL="Overall rating of feelings (recoded values)" AS OVERALL_RATING_FEELINGS, 
          /* HOW_OFTEN_FELT_NERVOUS */
            (CASE 
               WHEN -1 = t1.ADNERV42 THEN .
               WHEN -7 = t1.ADNERV42 THEN .
               WHEN -8 = t1.ADNERV42 THEN .
               WHEN -9 = t1.ADNERV42 THEN .
               ELSE t1.ADNERV42
            END) LABEL="How often felt nervous 30 days (recoded values)" AS HOW_OFTEN_FELT_NERVOUS, 
          /* VISITS_TO_MED_OFF */
            (CASE 
               WHEN -1 = t1.ADAPPT42 THEN .
               WHEN -8 = t1.ADAPPT42 THEN .
               WHEN -9 = t1.ADAPPT42 THEN .
               ELSE t1.ADAPPT42
            END) LABEL="number of visits to med off for care (recoded values)" AS VISITS_TO_MED_OFF, 
          /* GOT_MED_APPT_WHEN_WANTED */
            (CASE 
               WHEN -1 = t1.ADRTWW42 THEN .
               WHEN -9 = t1.ADRTWW42 THEN .
               ELSE t1.ADRTWW42
            END) LABEL="got med appt when wanted (recoded values)" AS GOT_MED_APPT_WHEN_WANTED, 
          /* OTHER_HEART_DISEASE_DIAG */
            (CASE 
               WHEN -7 = t1.OHRTDX THEN .
               WHEN -8 = t1.OHRTDX THEN .
               WHEN -9 = t1.OHRTDX THEN .
               ELSE t1.OHRTDX
            END) LABEL="other heart disease diag (recoded values)" AS OTHER_HEART_DISEASE_DIAG, 
          /* CANCER_DIAGNOSIS  */
            (CASE 
               WHEN -7 = t1.CANCERDX THEN .
               WHEN -8 = t1.CANCERDX THEN .
               WHEN -9 = t1.CANCERDX THEN .
               ELSE t1.CANCERDX
            END) LABEL="Cancer diagnosis (recoded values)" AS 'CANCER_DIAGNOSIS 'n, 
          /* EMPHYSEMA_DIAGNOSIS */
            (CASE 
               WHEN -7 = t1.EMPHDX THEN .
               WHEN -8 = t1.EMPHDX THEN .
               WHEN -9 = t1.EMPHDX THEN .
               ELSE t1.EMPHDX
            END) LABEL="Emphysema diagnosis (recoded values)" AS EMPHYSEMA_DIAGNOSIS, 
          /* HEART_ATTACK_DIAG */
            (CASE 
               WHEN -7 = t1.MIDX THEN .
               WHEN -8 = t1.MIDX THEN .
               WHEN -9 = t1.MIDX THEN .
               ELSE t1.MIDX
            END) LABEL="Heart Attack diag (recoded values)" AS HEART_ATTACK_DIAG, 
          /* CORONARY_HRT_DISEASE_DIAG */
            (CASE 
               WHEN -7 = t1.CHDDX THEN .
               WHEN -8 = t1.CHDDX THEN .
               WHEN -9 = t1.CHDDX THEN .
               ELSE t1.CHDDX
            END) LABEL="Coronary heart disease diag (recoded values)" AS CORONARY_HRT_DISEASE_DIAG, 
          /* HIGH_BLOOD_PRESSURE_DIAG */
            (CASE 
               WHEN -7 = t1.HIBPDX THEN .
               WHEN -8 = t1.HIBPDX THEN .
               WHEN -9 = t1.HIBPDX THEN .
               ELSE t1.HIBPDX
            END) LABEL="High blood pressure diag (recoded values)" AS HIGH_BLOOD_PRESSURE_DIAG, 
          /* STROKE_DIAG */
            (CASE 
               WHEN -7 = t1.STRKDX THEN .
               WHEN -8 = t1.STRKDX THEN .
               WHEN -9 = t1.STRKDX THEN .
               ELSE t1.STRKDX
            END) LABEL="stroke diag (recoded values)" AS STROKE_DIAG, 
          /* PERSON_IS_FLAGGED_DEPENDENT  */
            (CASE 
               WHEN -1 = t1.DEPDNT12 THEN .
               ELSE t1.DEPDNT12
            END) LABEL="Person is flagged a dependent (recoded values)" AS 'PERSON_IS_FLAGGED_DEPENDENT 'n, 
          /* NEVER_BEEN_SERIOUSLY_ILL */
            (CASE 
               WHEN -1 = t1.NEVILL42 THEN .
               ELSE t1.NEVILL42
            END) LABEL="Never been seriously ill (recoded values)" AS NEVER_BEEN_SERIOUSLY_ILL, 
          /* WORRY_MORE _ABOUT_HEALTH  */
            (CASE 
               WHEN -1 = t1.WRHLTH42 THEN .
               ELSE t1.WRHLTH42
            END) LABEL="Worry more about health (recoded values)" AS 'WORRY_MORE _ABOUT_HEALTH 'n, 
          /* MRRY12X_RECODE */
            (CASE 
               WHEN -7 = t1.MARRY12X THEN .
               WHEN -9 = t1.MARRY12X THEN .
               ELSE t1.MARRY12X
            END) LABEL="Marital status (recode)" AS MRRY12X_RECODE, 
          /* EDRECODE_RECODE */
            (CASE 
               WHEN -7 = t1.EDRECODE THEN .
               WHEN -8 = t1.EDRECODE THEN .
               WHEN -9 = t1.EDRECODE THEN .
               ELSE t1.EDRECODE
            END) LABEL="Education recode " AS EDRECODE_RECODE
      FROM MEPS.MEPS_FULLYR_2012_SUBSET t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Reverse code SF12 variables   */
%LET _CLIENTTASKLABEL='Reverse code SF12 variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\aolivieri\Assignments\OlivieriA_SAS_project_01122015.egp';
%LET _CLIENTPROJECTNAME='OlivieriA_SAS_project_01122015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_2012_SF12_REVCODE);

PROC SQL;
   CREATE TABLE WORK."MEPS_2012_SF12_REVCODE"n AS 
   SELECT t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.CANCERDX, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.HIBPDX, 
          t1.EMPHDX, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.CHDDX, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.MIDX, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.STRKDX, 
          t1.ADILWW42, 
          t1.WRHLTH42, 
          t1.NEVILL42, 
          t1.DEPDNT12, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.DUPERSID, 
          t1.EDUCYR, 
          t1.EDUYRDEG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.HIDEG, 
          t1.K6SUM42, 
          t1.MARRY12X, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.RACEAX, 
          t1.RACEBX, 
          t1.RACETHX, 
          t1.RACEV1X, 
          t1.RACEVER, 
          t1.RACEWX, 
          t1.REGION12, 
          t1.SAQELIG, 
          t1.SEX, 
          t1.SFFLAG42, 
          t1.SPOUIN12, 
          t1.DSCHNV53, 
          t1.LEUKAGED, 
          t1.LEUKREMS, 
          t1.LUNGAGED, 
          t1.LUNGREMS, 
          t1.LYMPAGED, 
          t1.LYMPREMS, 
          t1.OHRTAGED, 
          t1.OHRTDX, 
          t1.INSCOV12, 
          t1.INSURC12, 
          t1.MCAID12, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.AMASST12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.DVGEN12, 
          t1.DVORTH12, 
          t1.DVTOT12, 
          t1.ERDEXP12, 
          t1.ERFEXP12, 
          t1.ERTOT12, 
          t1.ALIMP12X, 
          t1.CSHIMP12, 
          t1.FAMINC12, 
          t1.UNEMP12X, 
          t1.HEALTH_IN_GENERAL, 
          t1.HEALTH_LIMITS_MOD_ACTIVITIES, 
          t1.HLTH_LIMITS_STAIRS, 
          t1.ACCMP_LESS_PHY, 
          t1.WORK_LIMIT_PHY, 
          t1.ACCMP_LESS_MNT, 
          t1.WORK_LIMIT_MNT, 
          t1.PAIN_LIMITS_NORMAL_WORK, 
          t1.FELT_CALM_PEACEFUL, 
          t1.'HAD_ALOT_ ENRGY'n, 
          t1.FELT_DOWNHEARTED_DEPR, 
          t1.HLTH_STOPPED_SOC_ACTIV, 
          t1.MARITAL_STATUS, 
          t1.MARITAL_STATUS_W_SPOUSE, 
          t1.YEARS_EDU_FIRST_ENTER_MEPS, 
          t1.YEARS_EDU_HIGHEST_DEGREE, 
          t1.HIGHEST_DEGREE_FIRST_ENTERED, 
          t1.EMPLOYMENT_STATUS_3_1, 
          t1.EMPLOYMENT_STATUS_4_2, 
          t1.EMPLOYMENT_STATUS_5_3, 
          t1.CURRENTLY_SMOKE, 
          t1.EASY_GETTING_MED_CARE, 
          t1.NEED_CARE_TEST_TREATMENT, 
          t1.ILL_INJURY_IMMED_CARE, 
          t1.MORE_LIKELY_TAKE_RISKS, 
          t1.CAN_OVERCOME_ILLS_WO_MED, 
          t1.OVERALL_RATING_FEELINGS, 
          t1.HOW_OFTEN_FELT_NERVOUS, 
          t1.VISITS_TO_MED_OFF, 
          t1.GOT_MED_APPT_WHEN_WANTED, 
          t1.OTHER_HEART_DISEASE_DIAG, 
          t1.CANCER_DIAGNOSIS, 
          t1.EMPHYSEMA_DIAGNOSIS, 
          t1.HEART_ATTACK_DIAG, 
          t1.CORONARY_HRT_DISEASE_DIAG, 
          t1.HIGH_BLOOD_PRESSURE_DIAG, 
          t1.STROKE_DIAG, 
          t1.PERSON_IS_FLAGGED_DEPENDENT, 
          t1.NEVER_BEEN_SERIOUSLY_ILL, 
          t1.'WORRY_MORE _ABOUT_HEALTH'n, 
          t1.MRRY12X_RECODE, 
          t1.EDRECODE_RECODE, 
          /* ADGEN42_reverse */
            (6-t1.HEALTH_IN_GENERAL) LABEL="SF12 Health in general (reverse coded)" AS ADGEN42_reverse, 
          /* ADPAIN42_reverse */
            (6-t1.PAIN_LIMITS_NORMAL_WORK) LABEL="SF12 Pain limits normal work (reverse code)" AS ADPAIN42_reverse, 
          /* ADCAPE42_reverse */
            (6-t1.FELT_CALM_PEACEFUL) LABEL="SF12 Felt calm and peaceful (reverse coded)" AS ADCAPE42_reverse, 
          /* ADNRGY42_reverse */
            (6-t1.'HAD_ALOT_ ENRGY'n) LABEL="SF12 had a lot of energy (reverse code)" AS ADNRGY42_reverse, 
          /* ADRISK42_reverse */
            (6-t1.MORE_LIKELY_TAKE_RISKS) LABEL="New aggregate More likely to take risks (reverse coded)" AS 
            ADRISK42_reverse, 
          /* NEVILL42_REVERSE */
            (6-t1.NEVER_BEEN_SERIOUSLY_ILL) LABEL="HEALTH RISK Never been seriously ill" AS NEVILL42_REVERSE
      FROM WORK.SUBSET_MEPS_2012_MANAGED t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: SF12 Query Builder   */
%LET _CLIENTTASKLABEL='SF12 Query Builder';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\aolivieri\Assignments\OlivieriA_SAS_project_01122015.egp';
%LET _CLIENTPROJECTNAME='OlivieriA_SAS_project_01122015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.SUBSET_MEPS_2012_SF12_REVCODE);

PROC SQL;
   CREATE TABLE WORK."SUBSET_MEPS_2012_SF12_REVCODE"n AS 
   SELECT t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.CANCERDX, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.HIBPDX, 
          t1.EMPHDX, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.CHDDX, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.MIDX, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.STRKDX, 
          t1.ADILWW42, 
          t1.WRHLTH42, 
          t1.NEVILL42, 
          t1.DEPDNT12, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.DUPERSID, 
          t1.EDUCYR, 
          t1.EDUYRDEG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.HIDEG, 
          t1.K6SUM42, 
          t1.MARRY12X, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.RACEAX, 
          t1.RACEBX, 
          t1.RACETHX, 
          t1.RACEV1X, 
          t1.RACEVER, 
          t1.RACEWX, 
          t1.REGION12, 
          t1.SAQELIG, 
          t1.SEX, 
          t1.SFFLAG42, 
          t1.SPOUIN12, 
          t1.DSCHNV53, 
          t1.LEUKAGED, 
          t1.LEUKREMS, 
          t1.LUNGAGED, 
          t1.LUNGREMS, 
          t1.LYMPAGED, 
          t1.LYMPREMS, 
          t1.OHRTAGED, 
          t1.OHRTDX, 
          t1.INSCOV12, 
          t1.INSURC12, 
          t1.MCAID12, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.AMASST12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.DVGEN12, 
          t1.DVORTH12, 
          t1.DVTOT12, 
          t1.ERDEXP12, 
          t1.ERFEXP12, 
          t1.ERTOT12, 
          t1.ALIMP12X, 
          t1.CSHIMP12, 
          t1.FAMINC12, 
          t1.UNEMP12X, 
          t1.HEALTH_IN_GENERAL, 
          t1.HEALTH_LIMITS_MOD_ACTIVITIES, 
          t1.HLTH_LIMITS_STAIRS, 
          t1.ACCMP_LESS_PHY, 
          t1.WORK_LIMIT_PHY, 
          t1.ACCMP_LESS_MNT, 
          t1.WORK_LIMIT_MNT, 
          t1.PAIN_LIMITS_NORMAL_WORK, 
          t1.FELT_CALM_PEACEFUL, 
          t1.'HAD_ALOT_ ENRGY'n, 
          t1.FELT_DOWNHEARTED_DEPR, 
          t1.HLTH_STOPPED_SOC_ACTIV, 
          t1.MARITAL_STATUS, 
          t1.MARITAL_STATUS_W_SPOUSE, 
          t1.YEARS_EDU_FIRST_ENTER_MEPS, 
          t1.YEARS_EDU_HIGHEST_DEGREE, 
          t1.HIGHEST_DEGREE_FIRST_ENTERED, 
          t1.EMPLOYMENT_STATUS_3_1, 
          t1.EMPLOYMENT_STATUS_4_2, 
          t1.EMPLOYMENT_STATUS_5_3, 
          t1.CURRENTLY_SMOKE, 
          t1.EASY_GETTING_MED_CARE, 
          t1.NEED_CARE_TEST_TREATMENT, 
          t1.ILL_INJURY_IMMED_CARE, 
          t1.MORE_LIKELY_TAKE_RISKS, 
          t1.CAN_OVERCOME_ILLS_WO_MED, 
          t1.OVERALL_RATING_FEELINGS, 
          t1.HOW_OFTEN_FELT_NERVOUS, 
          t1.VISITS_TO_MED_OFF, 
          t1.GOT_MED_APPT_WHEN_WANTED, 
          t1.OTHER_HEART_DISEASE_DIAG, 
          t1.CANCER_DIAGNOSIS, 
          t1.EMPHYSEMA_DIAGNOSIS, 
          t1.HEART_ATTACK_DIAG, 
          t1.CORONARY_HRT_DISEASE_DIAG, 
          t1.HIGH_BLOOD_PRESSURE_DIAG, 
          t1.STROKE_DIAG, 
          t1.PERSON_IS_FLAGGED_DEPENDENT, 
          t1.NEVER_BEEN_SERIOUSLY_ILL, 
          t1.'WORRY_MORE _ABOUT_HEALTH'n, 
          t1.ADGEN42_reverse, 
          t1.ADPAIN42_reverse, 
          t1.ADCAPE42_REVERSE LABEL="sf12 Felt calm and peaceful (reverse coded)", 
          t1.ADNRGY42_REVERSE LABEL="SF12 Had a lot of energy (reverse coded)", 
          /* SUM_SF12_VARIABLES */
            
            (SUM(t1.ADGEN42_reverse,t1.ADPAIN42_reverse,t1.ADCAPE42_REVERSE,t1.ADNRGY42_REVERSE,t1.HEALTH_LIMITS_MOD_ACTIVITIES,t1.HLTH_LIMITS_STAIRS,t1.ACCMP_LESS_PHY,t1.WORK_LIMIT_PHY,t1.ACCMP_LESS_MNT,t1.WORK_LIMIT_MNT,t1.FELT_DOWNHEARTED_DEPR,t1.HLTH_STOPPED_SOC_ACTIV)) 
            LABEL="Sum of SF-12 variables (included reverse coded)" AS SUM_SF12_VARIABLES
      FROM WORK.MEPS_2012_SF12_REVCODE t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data for SF12 variables and Aggregate   */
%LET _CLIENTTASKLABEL='List Data for SF12 variables and Aggregate';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\aolivieri\Assignments\OlivieriA_SAS_project_01122015.egp';
%LET _CLIENTPROJECTNAME='OlivieriA_SAS_project_01122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 7:50:06 PM
   By task: List Data for SF12 variables and Aggregate

   Input Data: Local:WORK.SUBSET_MEPS_2012_SF12_REVCODE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_SF12_REVCODE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ACCMP_LESS_MNT, T.WORK_LIMIT_PHY, T.FELT_DOWNHEARTED_DEPR, T.WORK_LIMIT_MNT, T.HLTH_STOPPED_SOC_ACTIV, T.ACCMP_LESS_PHY, T.HLTH_LIMITS_STAIRS, T.HEALTH_LIMITS_MOD_ACTIVITIES, T.SUM_SF12_VARIABLES, T.ADGEN42_reverse
		     , T.ADPAIN42_reverse, T.ADCAPE42_reverse, T.ADNRGY42_reverse
	FROM WORK.SUBSET_MEPS_2012_SF12_REVCODE as T
;
QUIT;
TITLE;
TITLE1 "Check aggregate variables coding";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=50)
	OBS="Row number"
	LABEL
	;
	VAR ACCMP_LESS_MNT WORK_LIMIT_PHY FELT_DOWNHEARTED_DEPR WORK_LIMIT_MNT HLTH_STOPPED_SOC_ACTIV ACCMP_LESS_PHY HLTH_LIMITS_STAIRS HEALTH_LIMITS_MOD_ACTIVITIES SUM_SF12_VARIABLES ADGEN42_reverse ADPAIN42_reverse ADCAPE42_reverse ADNRGY42_reverse;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies   */
%LET _CLIENTTASKLABEL='One-Way Frequencies';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\aolivieri\Assignments\OlivieriA_SAS_project_01122015.egp';
%LET _CLIENTPROJECTNAME='OlivieriA_SAS_project_01122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 7:50:06 PM
   By task: One-Way Frequencies

   Input Data: Local:WORK.SUBSET_MEPS_2012_SF12_REVCODE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_SF12_REVCODE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.SUM_SF12_VARIABLES
	FROM WORK.SUBSET_MEPS_2012_SF12_REVCODE as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES SUM_SF12_VARIABLES /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Summary Statistics for Aggregate Health   */
%LET _CLIENTTASKLABEL='Summary Statistics for Aggregate Health';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\aolivieri\Assignments\OlivieriA_SAS_project_01122015.egp';
%LET _CLIENTPROJECTNAME='OlivieriA_SAS_project_01122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 7:50:06 PM
   By task: Summary Statistics for Aggregate Health

   Input Data: Local:WORK.SUBSET_MEPS_2012_SF12_REVCODE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_SF12_REVCODE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SUM_SF12_VARIABLES
	FROM WORK.SUBSET_MEPS_2012_SF12_REVCODE as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MODE 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR SUM_SF12_VARIABLES;

RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis for Aggregate Health   */
%LET _CLIENTTASKLABEL='Distribution Analysis for Aggregate Health';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\aolivieri\Assignments\OlivieriA_SAS_project_01122015.egp';
%LET _CLIENTPROJECTNAME='OlivieriA_SAS_project_01122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 7:50:07 PM
   By task: Distribution Analysis for Aggregate Health

   Input Data: Local:WORK.SUBSET_MEPS_2012_SF12_REVCODE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
ODS GRAPHICS ON;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS_2012_SF12_REVCODE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SUM_SF12_VARIABLES
	FROM WORK.SUBSET_MEPS_2012_SF12_REVCODE(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: SUM_SF12_VARIABLES";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Andrew Olivieri";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR SUM_SF12_VARIABLES;
	HISTOGRAM   SUM_SF12_VARIABLES / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;
ODS GRAPHICS OFF;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Categorical variable for Aggregate   */
%LET _CLIENTTASKLABEL='Categorical variable for Aggregate';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\aolivieri\Assignments\OlivieriA_SAS_project_01122015.egp';
%LET _CLIENTPROJECTNAME='OlivieriA_SAS_project_01122015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_SUBSET_MEPS_2012_SF12_);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_SUBSET_MEPS_2012_SF12_ AS 
   SELECT /* SUM_SF12_VARIABLES_CATEGORICAL */
            (CASE  
               WHEN t1.SUM_SF12_VARIABLES>=2 and t1.SUM_SF12_VARIABLES <=41
               THEN 1
            WHEN t1.SUM_SF12_VARIABLES>=42 and t1.SUM_SF12_VARIABLES <=52
               THEN 2
            WHEN t1.SUM_SF12_VARIABLES>=53 and t1.SUM_SF12_VARIABLES <=57
               THEN 3
               ELSE 4
            END) LABEL="Health rating " AS SUM_SF12_VARIABLES_CATEGORICAL, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.CANCERDX, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.HIBPDX, 
          t1.EMPHDX, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.CHDDX, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.MIDX, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.STRKDX, 
          t1.ADILWW42, 
          t1.WRHLTH42, 
          t1.NEVILL42, 
          t1.DEPDNT12, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.DUPERSID, 
          t1.EDUCYR, 
          t1.EDUYRDEG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.HIDEG, 
          t1.K6SUM42, 
          t1.MARRY12X, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.RACEAX, 
          t1.RACEBX, 
          t1.RACETHX, 
          t1.RACEV1X, 
          t1.RACEVER, 
          t1.RACEWX, 
          t1.REGION12, 
          t1.SAQELIG, 
          t1.SEX, 
          t1.SFFLAG42, 
          t1.SPOUIN12, 
          t1.DSCHNV53, 
          t1.LEUKAGED, 
          t1.LEUKREMS, 
          t1.LUNGAGED, 
          t1.LUNGREMS, 
          t1.LYMPAGED, 
          t1.LYMPREMS, 
          t1.OHRTAGED, 
          t1.OHRTDX, 
          t1.INSCOV12, 
          t1.INSURC12, 
          t1.MCAID12, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.AMASST12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.DVGEN12, 
          t1.DVORTH12, 
          t1.DVTOT12, 
          t1.ERDEXP12, 
          t1.ERFEXP12, 
          t1.ERTOT12, 
          t1.ALIMP12X, 
          t1.CSHIMP12, 
          t1.FAMINC12, 
          t1.UNEMP12X, 
          t1.HEALTH_IN_GENERAL, 
          t1.HEALTH_LIMITS_MOD_ACTIVITIES, 
          t1.HLTH_LIMITS_STAIRS, 
          t1.ACCMP_LESS_PHY, 
          t1.WORK_LIMIT_PHY, 
          t1.ACCMP_LESS_MNT, 
          t1.WORK_LIMIT_MNT, 
          t1.PAIN_LIMITS_NORMAL_WORK, 
          t1.FELT_CALM_PEACEFUL, 
          t1.'HAD_ALOT_ ENRGY'n, 
          t1.FELT_DOWNHEARTED_DEPR, 
          t1.HLTH_STOPPED_SOC_ACTIV, 
          t1.MARITAL_STATUS, 
          t1.MARITAL_STATUS_W_SPOUSE, 
          t1.YEARS_EDU_FIRST_ENTER_MEPS, 
          t1.YEARS_EDU_HIGHEST_DEGREE, 
          t1.HIGHEST_DEGREE_FIRST_ENTERED, 
          t1.EMPLOYMENT_STATUS_3_1, 
          t1.EMPLOYMENT_STATUS_4_2, 
          t1.EMPLOYMENT_STATUS_5_3, 
          t1.CURRENTLY_SMOKE, 
          t1.EASY_GETTING_MED_CARE, 
          t1.NEED_CARE_TEST_TREATMENT, 
          t1.ILL_INJURY_IMMED_CARE, 
          t1.MORE_LIKELY_TAKE_RISKS, 
          t1.CAN_OVERCOME_ILLS_WO_MED, 
          t1.OVERALL_RATING_FEELINGS, 
          t1.HOW_OFTEN_FELT_NERVOUS, 
          t1.VISITS_TO_MED_OFF, 
          t1.GOT_MED_APPT_WHEN_WANTED, 
          t1.OTHER_HEART_DISEASE_DIAG, 
          t1.CANCER_DIAGNOSIS, 
          t1.EMPHYSEMA_DIAGNOSIS, 
          t1.HEART_ATTACK_DIAG, 
          t1.CORONARY_HRT_DISEASE_DIAG, 
          t1.HIGH_BLOOD_PRESSURE_DIAG, 
          t1.STROKE_DIAG, 
          t1.PERSON_IS_FLAGGED_DEPENDENT, 
          t1.NEVER_BEEN_SERIOUSLY_ILL, 
          t1.'WORRY_MORE _ABOUT_HEALTH'n, 
          t1.ADGEN42_reverse, 
          t1.ADPAIN42_reverse, 
          t1.ADCAPE42_reverse, 
          t1.ADNRGY42_reverse, 
          t1.SUM_SF12_VARIABLES
      FROM WORK.SUBSET_MEPS_2012_SF12_REVCODE t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies1   */
%LET _CLIENTTASKLABEL='One-Way Frequencies1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\aolivieri\Assignments\OlivieriA_SAS_project_01122015.egp';
%LET _CLIENTPROJECTNAME='OlivieriA_SAS_project_01122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 7:50:07 PM
   By task: One-Way Frequencies1

   Input Data: Local:WORK.QUERY_FOR_SUBSET_MEPS_2012_SF12_
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_SUBSET_MEPS_2012_SF12_
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.SUM_SF12_VARIABLES_CATEGORICAL, T.SUM_SF12_VARIABLES
	FROM WORK.QUERY_FOR_SUBSET_MEPS_2012_SF12_ as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES SUM_SF12_VARIABLES_CATEGORICAL /  SCORES=TABLE;
	TABLES SUM_SF12_VARIABLES /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis   */
%LET _CLIENTTASKLABEL='Table Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\aolivieri\Assignments\OlivieriA_SAS_project_01122015.egp';
%LET _CLIENTPROJECTNAME='OlivieriA_SAS_project_01122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 7:50:07 PM
   By task: Table Analysis

   Input Data: Local:WORK.QUERY_FOR_SUBSET_MEPS_2012_SF12_
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_SUBSET_MEPS_2012_SF12_
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SUM_SF12_VARIABLES, T.SUM_SF12_VARIABLES_CATEGORICAL
	FROM WORK.QUERY_FOR_SUBSET_MEPS_2012_SF12_ as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES SUM_SF12_VARIABLES * SUM_SF12_VARIABLES_CATEGORICAL /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Health Risk Query Builder   */
%LET _CLIENTTASKLABEL='Health Risk Query Builder';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\aolivieri\Assignments\OlivieriA_SAS_project_01122015.egp';
%LET _CLIENTPROJECTNAME='OlivieriA_SAS_project_01122015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_2012_HEALTH_RISK_AGGREGATE);

PROC SQL;
   CREATE TABLE WORK."MEPS_2012_HEALTH_RISK_AGGREGATE"n AS 
   SELECT t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.CANCERDX, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.HIBPDX, 
          t1.EMPHDX, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.CHDDX, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.MIDX, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.STRKDX, 
          t1.ADILWW42, 
          t1.WRHLTH42, 
          t1.NEVILL42, 
          t1.DEPDNT12, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.DUPERSID, 
          t1.EDUCYR, 
          t1.EDUYRDEG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.HIDEG, 
          t1.K6SUM42, 
          t1.MARRY12X, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.RACEAX, 
          t1.RACEBX, 
          t1.RACETHX, 
          t1.RACEV1X, 
          t1.RACEVER, 
          t1.RACEWX, 
          t1.REGION12, 
          t1.SAQELIG, 
          t1.SEX, 
          t1.SFFLAG42, 
          t1.SPOUIN12, 
          t1.DSCHNV53, 
          t1.LEUKAGED, 
          t1.LEUKREMS, 
          t1.LUNGAGED, 
          t1.LUNGREMS, 
          t1.LYMPAGED, 
          t1.LYMPREMS, 
          t1.OHRTAGED, 
          t1.OHRTDX, 
          t1.INSCOV12, 
          t1.INSURC12, 
          t1.MCAID12, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.AMASST12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.DVGEN12, 
          t1.DVORTH12, 
          t1.DVTOT12, 
          t1.ERDEXP12, 
          t1.ERFEXP12, 
          t1.ERTOT12, 
          t1.ALIMP12X, 
          t1.CSHIMP12, 
          t1.FAMINC12, 
          t1.UNEMP12X, 
          t1.HEALTH_IN_GENERAL, 
          t1.HEALTH_LIMITS_MOD_ACTIVITIES, 
          t1.HLTH_LIMITS_STAIRS, 
          t1.ACCMP_LESS_PHY, 
          t1.WORK_LIMIT_PHY, 
          t1.ACCMP_LESS_MNT, 
          t1.WORK_LIMIT_MNT, 
          t1.PAIN_LIMITS_NORMAL_WORK, 
          t1.FELT_CALM_PEACEFUL, 
          t1.'HAD_ALOT_ ENRGY'n, 
          t1.FELT_DOWNHEARTED_DEPR, 
          t1.HLTH_STOPPED_SOC_ACTIV, 
          t1.MARITAL_STATUS, 
          t1.MARITAL_STATUS_W_SPOUSE, 
          t1.YEARS_EDU_FIRST_ENTER_MEPS, 
          t1.YEARS_EDU_HIGHEST_DEGREE, 
          t1.HIGHEST_DEGREE_FIRST_ENTERED, 
          t1.EMPLOYMENT_STATUS_3_1, 
          t1.EMPLOYMENT_STATUS_4_2, 
          t1.EMPLOYMENT_STATUS_5_3, 
          t1.CURRENTLY_SMOKE, 
          t1.EASY_GETTING_MED_CARE, 
          t1.NEED_CARE_TEST_TREATMENT, 
          t1.ILL_INJURY_IMMED_CARE, 
          t1.MORE_LIKELY_TAKE_RISKS, 
          t1.CAN_OVERCOME_ILLS_WO_MED, 
          t1.OVERALL_RATING_FEELINGS, 
          t1.HOW_OFTEN_FELT_NERVOUS, 
          t1.VISITS_TO_MED_OFF, 
          t1.GOT_MED_APPT_WHEN_WANTED, 
          t1.OTHER_HEART_DISEASE_DIAG, 
          t1.CANCER_DIAGNOSIS, 
          t1.EMPHYSEMA_DIAGNOSIS, 
          t1.HEART_ATTACK_DIAG, 
          t1.CORONARY_HRT_DISEASE_DIAG, 
          t1.HIGH_BLOOD_PRESSURE_DIAG, 
          t1.STROKE_DIAG, 
          t1.PERSON_IS_FLAGGED_DEPENDENT, 
          t1.NEVER_BEEN_SERIOUSLY_ILL, 
          t1.'WORRY_MORE _ABOUT_HEALTH'n, 
          t1.ADGEN42_reverse, 
          t1.ADPAIN42_reverse, 
          t1.ADCAPE42_reverse, 
          t1.ADNRGY42_reverse, 
          t1.ADRISK42_reverse, 
          /* SUM_HEALTH_RISK */
            (SUM(t1.NEVILL42_REVERSE,t1.ADRISK42_reverse,t1.CAN_OVERCOME_ILLS_WO_MED,t1.GOT_MED_APPT_WHEN_WANTED)) 
            LABEL="SUM of Potential health risk variables" AS SUM_HEALTH_RISK, 
          t1.NEVILL42_REVERSE
      FROM WORK.MEPS_2012_SF12_REVCODE t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data of Aggregate Health Risks   */
%LET _CLIENTTASKLABEL='List Data of Aggregate Health Risks';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\aolivieri\Assignments\OlivieriA_SAS_project_01122015.egp';
%LET _CLIENTPROJECTNAME='OlivieriA_SAS_project_01122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 7:50:07 PM
   By task: List Data of Aggregate Health Risks

   Input Data: Local:WORK.MEPS_2012_HEALTH_RISK_AGGREGATE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_2012_HEALTH_RISK_AGGREGATE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.GOT_MED_APPT_WHEN_WANTED, T.CAN_OVERCOME_ILLS_WO_MED, T.NEVILL42_REVERSE, T.ADRISK42_reverse, T.SUM_HEALTH_RISK
	FROM WORK.MEPS_2012_HEALTH_RISK_AGGREGATE(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Report Listing of Health Risk variables";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Andrew Olivieri";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=50)
	OBS="Row number"
	LABEL
	;
	VAR GOT_MED_APPT_WHEN_WANTED CAN_OVERCOME_ILLS_WO_MED NEVILL42_REVERSE ADRISK42_reverse SUM_HEALTH_RISK;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Summary Statistics of Aggregate Health Risks   */
%LET _CLIENTTASKLABEL='Summary Statistics of Aggregate Health Risks';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\aolivieri\Assignments\OlivieriA_SAS_project_01122015.egp';
%LET _CLIENTPROJECTNAME='OlivieriA_SAS_project_01122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 7:50:07 PM
   By task: Summary Statistics of Aggregate Health Risks

   Input Data: Local:WORK.MEPS_2012_HEALTH_RISK_AGGREGATE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_2012_HEALTH_RISK_AGGREGATE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SUM_HEALTH_RISK
	FROM WORK.MEPS_2012_HEALTH_RISK_AGGREGATE as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MODE 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR SUM_HEALTH_RISK;

RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis of Aggregate Health Risks   */
%LET _CLIENTTASKLABEL='Distribution Analysis of Aggregate Health Risks';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\aolivieri\Assignments\OlivieriA_SAS_project_01122015.egp';
%LET _CLIENTPROJECTNAME='OlivieriA_SAS_project_01122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 7:50:08 PM
   By task: Distribution Analysis of Aggregate Health Risks

   Input Data: Local:WORK.MEPS_2012_HEALTH_RISK_AGGREGATE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
ODS GRAPHICS ON;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_2012_HEALTH_RISK_AGGREGATE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SUM_HEALTH_RISK
	FROM WORK.MEPS_2012_HEALTH_RISK_AGGREGATE as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: SUM_HEALTH_RISK";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Andrew Olivieri";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR SUM_HEALTH_RISK;
	HISTOGRAM   SUM_HEALTH_RISK / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;
ODS GRAPHICS OFF;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Categorical Query Builder   */
%LET _CLIENTTASKLABEL='Categorical Query Builder';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\aolivieri\Assignments\OlivieriA_SAS_project_01122015.egp';
%LET _CLIENTPROJECTNAME='OlivieriA_SAS_project_01122015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.MEPS_2012_HEALTH_RISK_);

PROC SQL;
   CREATE TABLE WORK."MEPS_2012_HEALTH_RISK_"n AS 
   SELECT t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.CANCERDX, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.HIBPDX, 
          t1.EMPHDX, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.CHDDX, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.MIDX, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.STRKDX, 
          t1.ADILWW42, 
          t1.WRHLTH42, 
          t1.NEVILL42, 
          t1.DEPDNT12, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.DUPERSID, 
          t1.EDUCYR, 
          t1.EDUYRDEG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.HIDEG, 
          t1.K6SUM42, 
          t1.MARRY12X, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.RACEAX, 
          t1.RACEBX, 
          t1.RACETHX, 
          t1.RACEV1X, 
          t1.RACEVER, 
          t1.RACEWX, 
          t1.REGION12, 
          t1.SAQELIG, 
          t1.SEX, 
          t1.SFFLAG42, 
          t1.SPOUIN12, 
          t1.DSCHNV53, 
          t1.LEUKAGED, 
          t1.LEUKREMS, 
          t1.LUNGAGED, 
          t1.LUNGREMS, 
          t1.LYMPAGED, 
          t1.LYMPREMS, 
          t1.OHRTAGED, 
          t1.OHRTDX, 
          t1.INSCOV12, 
          t1.INSURC12, 
          t1.MCAID12, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.AMASST12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.DVGEN12, 
          t1.DVORTH12, 
          t1.DVTOT12, 
          t1.ERDEXP12, 
          t1.ERFEXP12, 
          t1.ERTOT12, 
          t1.ALIMP12X, 
          t1.CSHIMP12, 
          t1.FAMINC12, 
          t1.UNEMP12X, 
          t1.HEALTH_IN_GENERAL, 
          t1.HEALTH_LIMITS_MOD_ACTIVITIES, 
          t1.HLTH_LIMITS_STAIRS, 
          t1.ACCMP_LESS_PHY, 
          t1.WORK_LIMIT_PHY, 
          t1.ACCMP_LESS_MNT, 
          t1.WORK_LIMIT_MNT, 
          t1.PAIN_LIMITS_NORMAL_WORK, 
          t1.FELT_CALM_PEACEFUL, 
          t1.'HAD_ALOT_ ENRGY'n, 
          t1.FELT_DOWNHEARTED_DEPR, 
          t1.HLTH_STOPPED_SOC_ACTIV, 
          t1.MARITAL_STATUS, 
          t1.MARITAL_STATUS_W_SPOUSE, 
          t1.YEARS_EDU_FIRST_ENTER_MEPS, 
          t1.YEARS_EDU_HIGHEST_DEGREE, 
          t1.HIGHEST_DEGREE_FIRST_ENTERED, 
          t1.EMPLOYMENT_STATUS_3_1, 
          t1.EMPLOYMENT_STATUS_4_2, 
          t1.EMPLOYMENT_STATUS_5_3, 
          t1.CURRENTLY_SMOKE, 
          t1.EASY_GETTING_MED_CARE, 
          t1.NEED_CARE_TEST_TREATMENT, 
          t1.ILL_INJURY_IMMED_CARE, 
          t1.MORE_LIKELY_TAKE_RISKS, 
          t1.CAN_OVERCOME_ILLS_WO_MED, 
          t1.OVERALL_RATING_FEELINGS, 
          t1.HOW_OFTEN_FELT_NERVOUS, 
          t1.VISITS_TO_MED_OFF, 
          t1.GOT_MED_APPT_WHEN_WANTED, 
          t1.OTHER_HEART_DISEASE_DIAG, 
          t1.CANCER_DIAGNOSIS, 
          t1.EMPHYSEMA_DIAGNOSIS, 
          t1.HEART_ATTACK_DIAG, 
          t1.CORONARY_HRT_DISEASE_DIAG, 
          t1.HIGH_BLOOD_PRESSURE_DIAG, 
          t1.STROKE_DIAG, 
          t1.PERSON_IS_FLAGGED_DEPENDENT, 
          t1.NEVER_BEEN_SERIOUSLY_ILL, 
          t1.'WORRY_MORE _ABOUT_HEALTH'n, 
          t1.ADGEN42_reverse, 
          t1.ADPAIN42_reverse, 
          t1.ADCAPE42_reverse, 
          t1.ADNRGY42_reverse, 
          t1.ADRISK42_reverse, 
          t1.SUM_HEALTH_RISK, 
          t1.NEVILL42_REVERSE, 
          /* SUM_HEALTH_RISK_CATEGORICAL */
            (CASE  
               WHEN t1.SUM_HEALTH_RISK >=1 and t1.SUM_HEALTH_RISK <=6
               THEN 1
               WHEN t1.SUM_HEALTH_RISK >=7 and t1.SUM_HEALTH_RISK <=10
               THEN 2
               WHEN t1.SUM_HEALTH_RISK >=11 and t1.SUM_HEALTH_RISK <=14
               THEN 3
               ELSE 4
            END) LABEL="Categorical values of health risk" AS SUM_HEALTH_RISK_CATEGORICAL
      FROM WORK.MEPS_2012_HEALTH_RISK_AGGREGATE t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies2   */
%LET _CLIENTTASKLABEL='One-Way Frequencies2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\aolivieri\Assignments\OlivieriA_SAS_project_01122015.egp';
%LET _CLIENTPROJECTNAME='OlivieriA_SAS_project_01122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 7:50:08 PM
   By task: One-Way Frequencies2

   Input Data: Local:WORK.MEPS_2012_HEALTH_RISK_
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_2012_HEALTH_RISK_
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.SUM_HEALTH_RISK, T.SUM_HEALTH_RISK_CATEGORICAL
	FROM WORK.MEPS_2012_HEALTH_RISK_ as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES SUM_HEALTH_RISK /  SCORES=TABLE;
	TABLES SUM_HEALTH_RISK_CATEGORICAL /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis1   */
%LET _CLIENTTASKLABEL='Table Analysis1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\aolivieri\Assignments\OlivieriA_SAS_project_01122015.egp';
%LET _CLIENTPROJECTNAME='OlivieriA_SAS_project_01122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 7:50:08 PM
   By task: Table Analysis1

   Input Data: Local:WORK.MEPS_2012_HEALTH_RISK_
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.MEPS_2012_HEALTH_RISK_
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SUM_HEALTH_RISK, T.SUM_HEALTH_RISK_CATEGORICAL
	FROM WORK.MEPS_2012_HEALTH_RISK_ as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES SUM_HEALTH_RISK * SUM_HEALTH_RISK_CATEGORICAL /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Categorical for EDRECODE and MRRY12X   */
%LET _CLIENTTASKLABEL='Categorical for EDRECODE and MRRY12X';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\aolivieri\Assignments\OlivieriA_SAS_project_01122015.egp';
%LET _CLIENTPROJECTNAME='OlivieriA_SAS_project_01122015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_2012_SF12_REVCODE);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_2012_SF12_REVCODE AS 
   SELECT t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.CANCERDX, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.HIBPDX, 
          t1.EMPHDX, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.CHDDX, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.MIDX, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.STRKDX, 
          t1.ADILWW42, 
          t1.WRHLTH42, 
          t1.NEVILL42, 
          t1.DEPDNT12, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.DUPERSID, 
          t1.EDUCYR, 
          t1.EDUYRDEG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.HIDEG, 
          t1.K6SUM42, 
          t1.MARRY12X, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.RACEAX, 
          t1.RACEBX, 
          t1.RACETHX, 
          t1.RACEV1X, 
          t1.RACEVER, 
          t1.RACEWX, 
          t1.REGION12, 
          t1.SAQELIG, 
          t1.SEX, 
          t1.SFFLAG42, 
          t1.SPOUIN12, 
          t1.DSCHNV53, 
          t1.LEUKAGED, 
          t1.LEUKREMS, 
          t1.LUNGAGED, 
          t1.LUNGREMS, 
          t1.LYMPAGED, 
          t1.LYMPREMS, 
          t1.OHRTAGED, 
          t1.OHRTDX, 
          t1.INSCOV12, 
          t1.INSURC12, 
          t1.MCAID12, 
          t1.IPDIS12, 
          t1.IPNGTD12, 
          t1.AMASST12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.DVGEN12, 
          t1.DVORTH12, 
          t1.DVTOT12, 
          t1.ERDEXP12, 
          t1.ERFEXP12, 
          t1.ERTOT12, 
          t1.ALIMP12X, 
          t1.CSHIMP12, 
          t1.FAMINC12, 
          t1.UNEMP12X, 
          t1.HEALTH_IN_GENERAL, 
          t1.HEALTH_LIMITS_MOD_ACTIVITIES, 
          t1.HLTH_LIMITS_STAIRS, 
          t1.ACCMP_LESS_PHY, 
          t1.WORK_LIMIT_PHY, 
          t1.ACCMP_LESS_MNT, 
          t1.WORK_LIMIT_MNT, 
          t1.PAIN_LIMITS_NORMAL_WORK, 
          t1.FELT_CALM_PEACEFUL, 
          t1.'HAD_ALOT_ ENRGY'n, 
          t1.FELT_DOWNHEARTED_DEPR, 
          t1.HLTH_STOPPED_SOC_ACTIV, 
          t1.MARITAL_STATUS, 
          t1.MARITAL_STATUS_W_SPOUSE, 
          t1.YEARS_EDU_FIRST_ENTER_MEPS, 
          t1.YEARS_EDU_HIGHEST_DEGREE, 
          t1.HIGHEST_DEGREE_FIRST_ENTERED, 
          t1.EMPLOYMENT_STATUS_3_1, 
          t1.EMPLOYMENT_STATUS_4_2, 
          t1.EMPLOYMENT_STATUS_5_3, 
          t1.CURRENTLY_SMOKE, 
          t1.EASY_GETTING_MED_CARE, 
          t1.NEED_CARE_TEST_TREATMENT, 
          t1.ILL_INJURY_IMMED_CARE, 
          t1.MORE_LIKELY_TAKE_RISKS, 
          t1.CAN_OVERCOME_ILLS_WO_MED, 
          t1.OVERALL_RATING_FEELINGS, 
          t1.HOW_OFTEN_FELT_NERVOUS, 
          t1.VISITS_TO_MED_OFF, 
          t1.GOT_MED_APPT_WHEN_WANTED, 
          t1.OTHER_HEART_DISEASE_DIAG, 
          t1.CANCER_DIAGNOSIS, 
          t1.EMPHYSEMA_DIAGNOSIS, 
          t1.HEART_ATTACK_DIAG, 
          t1.CORONARY_HRT_DISEASE_DIAG, 
          t1.HIGH_BLOOD_PRESSURE_DIAG, 
          t1.STROKE_DIAG, 
          t1.PERSON_IS_FLAGGED_DEPENDENT, 
          t1.NEVER_BEEN_SERIOUSLY_ILL, 
          t1.'WORRY_MORE _ABOUT_HEALTH'n, 
          t1.MRRY12X_RECODE, 
          t1.EDRECODE_RECODE, 
          t1.ADGEN42_reverse, 
          t1.ADPAIN42_reverse, 
          t1.ADCAPE42_reverse, 
          t1.ADNRGY42_reverse, 
          t1.ADRISK42_reverse, 
          t1.NEVILL42_REVERSE, 
          /* MRRY12X_CATEGORICAL */
            (CASE  
               WHEN t1.MRRY12X_RECODE=1
               THEN 1
               WHEN t1.MRRY12X_RECODE>=2 and t1.MRRY12X_RECODE<=4
               THEN 2
               WHEN t1.MRRY12X_RECODE>=5 and t1.MRRY12X_RECODE<=6
               THEN 3
               ELSE 4
            END) LABEL="Marital Status categorical " AS MRRY12X_CATEGORICAL, 
          /* EDRECODE_CATEGORICAL */
            (CASE  
               WHEN t1.EDRECODE_RECODE>=0 and t1.EDRECODE_RECODE<=8
               THEN 1
               WHEN t1.EDRECODE_RECODE>=9 and t1.EDRECODE_RECODE<=13
               THEN 2
               WHEN t1.EDRECODE_RECODE>=14 and t1.EDRECODE_RECODE<=15
               THEN 3
               WHEN t1.EDRECODE_RECODE>=16 
               THEN 4
               ELSE 5
            END) LABEL="Education recode for grade school, HS, college and post-grad" AS EDRECODE_CATEGORICAL
      FROM WORK.MEPS_2012_SF12_REVCODE t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis of Categorical EDRECODE and MRRY12X   */
%LET _CLIENTTASKLABEL='Table Analysis of Categorical EDRECODE and MRRY12X';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\aolivieri\Assignments\OlivieriA_SAS_project_01122015.egp';
%LET _CLIENTPROJECTNAME='OlivieriA_SAS_project_01122015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 7:50:08 PM
   By task: Table Analysis of Categorical EDRECODE and MRRY12X

   Input Data: Local:WORK.QUERY_FOR_MEPS_2012_SF12_REVCODE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_2012_SF12_REVCODE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.EDRECODE_CATEGORICAL, T.MRRY12X_CATEGORICAL, T.EDRECODE_RECODE, T.MRRY12X_RECODE
	FROM WORK.QUERY_FOR_MEPS_2012_SF12_REVCODE as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES EDRECODE_RECODE * EDRECODE_CATEGORICAL /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES MRRY12X_RECODE * MRRY12X_CATEGORICAL /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
