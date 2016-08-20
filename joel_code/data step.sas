libname qcstudy "c:\SAS\QC Study";

data qcstudy.Midas19862012;
	set heredevl.Midas19862012;

run;

proc sort data=qcstudy.Midas19862012;
	by patient_id descending admdat;
run;

*Creates a table with a record for every day a patient was in a particular hospital on a particular day;
data qcstudy.qc1;
	set qcstudy.Midas19862012 (keep = patient_id status zip admdat dschdat hosp recdid newdtd patbdte sex race);

	by patient_id;
	retain next_adm next_recdid;

	if first.patient_id then do;
		next_adm = admdat;
		next_recdid = recdid;
		readm30day = 0;
		end;
	else do;
		if 1 < next_adm - dschdat < 30 then do; *did the patient get readmitted within 30 days?;
			readm30day = 1; 
			readminrecdid = next_recdid;
			priorday = admdat - 1; *marks the day before the patient arrived for initial admission;
			end;
		else readm30day = 0;
		next_adm = admdat;
		next_recdid = recdid;
		end;

	do i = admdat to dschdat; *output a record for every day the patient was in the hospital;
		dayup = i;
		if newdtd NE . and (newdtd - dschdat) <= 30 then dthcnt = 1; else dthcnt = 0;
			*the patient died within 30 days after initial admission;
		pooroutcome = dthcnt + readm30day;
		hospup = hosp;
		daycnt = 1;
		output;
	end;

	drop i status zip /*admdat dschdat recdid*/ hosp next_recdid;
run;

/*proc sort data=qcstudy.qc1;*/
/*	by dayup hospup;*/
/*run;*/
/**/
/**/
/*proc means data = qcstudy.qc1 sum noprint nway;*/
/*	var daycnt dthcnt readm30day;*/
/*	class dayup hospup;*/
/*	output out = qcstudy.qc2 sum(daycnt)=totdays sum(dthcnt)=totdth  */
/*		sum(readm30day)=totreadm30day sum(pooroutcome)=totpooroutcome;*/
/*run;*/

proc sort data = qcstudy.allinfections;
	by dayup hospup;
run;

*determine patients who had infections only on readmission;
data qcstudy.allinfections2;
	set qcstudy.allinfections;

	if readmininf_flag = 1 and originf_flag = 0 then postadmininf_flag = 1; else postadmininf_flag = 0;
run;

proc means data = qcstudy.allinfections2 sum noprint nway;
	var daycnt dthcnt readm30day originf_flag readmininf_flag postadmininf_flag;
	class dayup hospup;
	output out = qcstudy.qc2 sum(daycnt)=totdays sum(dthcnt)=totdth  
		sum(readm30day)=totreadm30day sum(readmininf_flag)=totreadmininf_readm30day  
		sum(originf_flag)=totorigininf_readm30day sum(postadmininf_flag)= totpostadmininf_flag
		sum(pooroutcome)=totpooroutcome;
run;


data qcstudy.qc3;
	set qcstudy.qc2;

	where 1997 <= year(dayup) < 2012;

	dateup = put(dayup, date9.);
	yearup = year(dayup); 
run;

proc means data = qcstudy.qc3 sum mean std noprint nway p25 p40 p50 p60 p70 p75 p80 p90 median;
	var totdays;
	class hospup yearup;
	output out = qcstudy.hospaggdata sum(totdays)=sumdays mean(totdays)=meandays std(totdays)=stddays
	p25(totdays)=p25days  p40(totdays)=p40days p50(totdays)=p50days p60(totdays)=p60days p70(totdays)=p70days 
	p75(totdays)=p75days p80(totdays)=p80days p90(totdays)=p90days median(totdays)=mediandays ;
run;

proc sort data=qcstudy.hospaggdata;
	by hospup;
run;

proc sort data=qcstudy.hospdata;
	by hospup;
run;


data qcstudy.hospdataall;
	length hospsizecat hosprangecat $20;
	merge qcstudy.hospaggdata (in=a) qcstudy.hospdata (in=b);
	by hospup;
	if a;

	interq = p75days - p25days;
	perrange = (interq/p25days)*100;

	if 0 <= mediandays <= 100 then hospsizecat = '1 Small';
	else if mediandays <= 167 then hospsizecat = '2 Medium';
	else if mediandays <= 233 then hospsizecat = '3 Large';
	else hospsizecat = '4 Very Large';

	if 0 <= perrange <= 16.7 then hosprangecat = '1 Low Range';
	else if perrange <= 19.8 then hosprangecat = '2 Moderate Range';
	else if perrange <= 24.2 then hosprangecat = '3 Wide Range';
	else hosprangecat = '4 Very Wide Range';

	drop _type_ _freq_;

run;

proc sort data=qcstudy.qc3;
	by hospup yearup;
run;

proc sort data=qcstudy.hospdataall;
	by hospup yearup;
run;


data qcstudy.qc4;
	length loadcat5 $20;

	merge qcstudy.qc3 (in=a) qcstudy.hospdataall (in=b);
	by hospup yearup;
	if a;

	load = (totdays/beds)*100;
	dthrate = (totdth/totdays)*100;
	readminrate = (totreadm30day/totdays)*100;
	readmininf_rate = (totreadmininf_readm30day /totdays)*100;
	originf_rate = (totorigininf_readm30day/totdays)*100;
	postadmininf_rate = (totpostadmininf_flag/totdays)*100;
	pooroutcomerate = (totpooroutcome/totdays)*100;

	if yearup < 2002 then yearcat = '1 97-01';
	else if yearup < 2007 then yearcat = '2 02-06';
	else yearcat = '3 07-11';

	if 0 <= totdays <= p40days then loadcat5 = '1 LE 40%ile';
	else if 0 <= totdays <= p60days then loadcat5 = '3 LE 60%ile';
	else if 0 <= totdays <= p80days then loadcat5 = '5 LE 80%ile';
	else loadcat5 = '7 GT 80%ile';
	
	drop _type_ _freq_;

run;

proc freq data=qcstudy.qc4;
	tables loadcat3;
/*	where hospup = '001' and yearup = 2008;*/
run;


proc univariate data=qcstudy.qc4;
	var zload;
	class hospup yearup;
	where hospup = '001';
run;

proc means data=qcstudy.qc4 median mean min max p25 p75;
	var load dthrate readminrate pooroutcomerate mediandays perrange;
/*	by hospup;*/
run;

proc genmod  data=qcstudy.qc4 descending;
		class hospup yearup (ref='1997') /param=ref;
		 model readmininf_rate = yearup
			/ dist = poisson link = id;

		repeated subject = hospup / type = indep;

		where dayup < 18962;

	estimate '1997' intercept 1;
	estimate '1998' intercept 1 yearup 1 0 0 0 0 0 0 0 0 0 0 0 0 0;
	estimate '1999' intercept 1 yearup 0 1 0 0 0 0 0 0 0 0 0 0 0 0;
	estimate '2000' intercept 1 yearup 0 0 1 0 0 0 0 0 0 0 0 0 0 0;
	estimate '2001' intercept 1 yearup 0 0 0 1 0 0 0 0 0 0 0 0 0 0;
	estimate '2002' intercept 1 yearup 0 0 0 0 1 0 0 0 0 0 0 0 0 0;
	estimate '2003' intercept 1 yearup 0 0 0 0 0 1 0 0 0 0 0 0 0 0;
	estimate '2004' intercept 1 yearup 0 0 0 0 0 0 1 0 0 0 0 0 0 0;
	estimate '2005' intercept 1 yearup 0 0 0 0 0 0 0 1 0 0 0 0 0 0;
	estimate '2006' intercept 1 yearup 0 0 0 0 0 0 0 0 1 0 0 0 0 0;
	estimate '2007' intercept 1 yearup 0 0 0 0 0 0 0 0 0 1 0 0 0 0;
	estimate '2008' intercept 1 yearup 0 0 0 0 0 0 0 0 0 0 1 0 0 0;
	estimate '2009' intercept 1 yearup 0 0 0 0 0 0 0 0 0 0 0 1 0 0;
	estimate '2010' intercept 1 yearup 0 0 0 0 0 0 0 0 0 0 0 0 1 0;
	estimate '2011' intercept 1 yearup 0 0 0 0 0 0 0 0 0 0 0 0 0 1;

run;

proc genmod  data=qcstudy.qc4 descending;
		class hospup yearup (ref='1997') /param=ref;
		 model postadmininf_rate = yearup
			/ dist = poisson link = id;

		repeated subject = hospup / type = indep;

		where dayup < 18962;

	estimate '1997' intercept 1;
	estimate '1998' intercept 1 yearup 1 0 0 0 0 0 0 0 0 0 0 0 0 0;
	estimate '1999' intercept 1 yearup 0 1 0 0 0 0 0 0 0 0 0 0 0 0;
	estimate '2000' intercept 1 yearup 0 0 1 0 0 0 0 0 0 0 0 0 0 0;
	estimate '2001' intercept 1 yearup 0 0 0 1 0 0 0 0 0 0 0 0 0 0;
	estimate '2002' intercept 1 yearup 0 0 0 0 1 0 0 0 0 0 0 0 0 0;
	estimate '2003' intercept 1 yearup 0 0 0 0 0 1 0 0 0 0 0 0 0 0;
	estimate '2004' intercept 1 yearup 0 0 0 0 0 0 1 0 0 0 0 0 0 0;
	estimate '2005' intercept 1 yearup 0 0 0 0 0 0 0 1 0 0 0 0 0 0;
	estimate '2006' intercept 1 yearup 0 0 0 0 0 0 0 0 1 0 0 0 0 0;
	estimate '2007' intercept 1 yearup 0 0 0 0 0 0 0 0 0 1 0 0 0 0;
	estimate '2008' intercept 1 yearup 0 0 0 0 0 0 0 0 0 0 1 0 0 0;
	estimate '2009' intercept 1 yearup 0 0 0 0 0 0 0 0 0 0 0 1 0 0;
	estimate '2010' intercept 1 yearup 0 0 0 0 0 0 0 0 0 0 0 0 1 0;
	estimate '2011' intercept 1 yearup 0 0 0 0 0 0 0 0 0 0 0 0 0 1;

run;

proc genmod  data=qcstudy.qc4 descending;
		class hospup yearup (ref='1997') /param=ref;
		 model dthrate = yearup
			/ dist = poisson link = id;

		repeated subject = hospup / type = indep;

		where dayup < 18962;

	estimate '1997' intercept 1;
	estimate '1998' intercept 1 yearup 1 0 0 0 0 0 0 0 0 0 0 0 0 0;
	estimate '1999' intercept 1 yearup 0 1 0 0 0 0 0 0 0 0 0 0 0 0;
	estimate '2000' intercept 1 yearup 0 0 1 0 0 0 0 0 0 0 0 0 0 0;
	estimate '2001' intercept 1 yearup 0 0 0 1 0 0 0 0 0 0 0 0 0 0;
	estimate '2002' intercept 1 yearup 0 0 0 0 1 0 0 0 0 0 0 0 0 0;
	estimate '2003' intercept 1 yearup 0 0 0 0 0 1 0 0 0 0 0 0 0 0;
	estimate '2004' intercept 1 yearup 0 0 0 0 0 0 1 0 0 0 0 0 0 0;
	estimate '2005' intercept 1 yearup 0 0 0 0 0 0 0 1 0 0 0 0 0 0;
	estimate '2006' intercept 1 yearup 0 0 0 0 0 0 0 0 1 0 0 0 0 0;
	estimate '2007' intercept 1 yearup 0 0 0 0 0 0 0 0 0 1 0 0 0 0;
	estimate '2008' intercept 1 yearup 0 0 0 0 0 0 0 0 0 0 1 0 0 0;
	estimate '2009' intercept 1 yearup 0 0 0 0 0 0 0 0 0 0 0 1 0 0;
	estimate '2010' intercept 1 yearup 0 0 0 0 0 0 0 0 0 0 0 0 1 0;
	estimate '2011' intercept 1 yearup 0 0 0 0 0 0 0 0 0 0 0 0 0 1;

run;

proc genmod  data=qcstudy.qc4 descending;
		class hospup yearup (ref='1997') /param=ref;
		 model pooroutcomerate = yearup
			/ dist = poisson link = id;

		repeated subject = hospup / type = indep;

		where dayup < 18962;

	estimate '1997' intercept 1;
	estimate '1998' intercept 1 yearup 1 0 0 0 0 0 0 0 0 0 0 0 0 0;
	estimate '1999' intercept 1 yearup 0 1 0 0 0 0 0 0 0 0 0 0 0 0;
	estimate '2000' intercept 1 yearup 0 0 1 0 0 0 0 0 0 0 0 0 0 0;
	estimate '2001' intercept 1 yearup 0 0 0 1 0 0 0 0 0 0 0 0 0 0;
	estimate '2002' intercept 1 yearup 0 0 0 0 1 0 0 0 0 0 0 0 0 0;
	estimate '2003' intercept 1 yearup 0 0 0 0 0 1 0 0 0 0 0 0 0 0;
	estimate '2004' intercept 1 yearup 0 0 0 0 0 0 1 0 0 0 0 0 0 0;
	estimate '2005' intercept 1 yearup 0 0 0 0 0 0 0 1 0 0 0 0 0 0;
	estimate '2006' intercept 1 yearup 0 0 0 0 0 0 0 0 1 0 0 0 0 0;
	estimate '2007' intercept 1 yearup 0 0 0 0 0 0 0 0 0 1 0 0 0 0;
	estimate '2008' intercept 1 yearup 0 0 0 0 0 0 0 0 0 0 1 0 0 0;
	estimate '2009' intercept 1 yearup 0 0 0 0 0 0 0 0 0 0 0 1 0 0;
	estimate '2010' intercept 1 yearup 0 0 0 0 0 0 0 0 0 0 0 0 1 0;
	estimate '2011' intercept 1 yearup 0 0 0 0 0 0 0 0 0 0 0 0 0 1;

run;

proc genmod  data=qcstudy.qc4 descending;
		class hospup area (ref='S') hospsizecat (ref = '4 Very Large') yearup (ref='1997')
			teaching (ref = 'N')  loadcat5 (ref = '7 GT 80%ile')/param=ref;
		 model readmininf_rate = area /*yearup*/ hospsizecat teaching loadcat5 area*loadcat5
			/ dist = poisson link = id;

		where dayup < 18962;

/*		where teaching NE 'T';*/

/*		where hospup in ('083' '118' '096' '002' '119' '039' '092' '084' '019');*/
/*		where hospup in ('028');*/
/*		where hospup in ('005' '051' '057' '047' '015' '050' '034' '108' '010' '058');*/

		repeated subject = hospup / type = indep;

/*		Estimate 'I, N, VL v. S T Sm @40%' area 1 0 0 teaching 0 1 hospsizecat 0 0 1;*/
/*		Estimate 'I, N, VL v. S T Sm @>80%' area 1 0 0 teaching 0 1 hospsizecat 0 0 1 loadcat5 0 0 1;*/

/*		Estimate 'I v. S' area 1 0 0 Area*loadcat5 1 0 0 0 0 0 0 0 0/exp;*/
/*		estimate 'I @40% Load' intercept 1 area 1 0 0 loadcat5 1 0 0 Area*loadcat5 1 0 0 0 0 0 0 0 0/exp;*/
/*		estimate 'I @90% Load' intercept 1 area 1 0 0 loadcat5 0 0 0 Area*loadcat5 0 0 0 0 0 0 0 0 0/exp;*/
/*		estimate 'S' intercept 1;*/

/*		Estimate '50-60% Load' intercept 1 loadcat 0 1 0 0 0;*/
/*		Estimate '60-70% Load' intercept 1 loadcat 0 0 1 0 0;*/
/*		Estimate '70-80% Load' intercept 1 loadcat 0 0 0 0 0;*/
/*		Estimate '80-90% Load' intercept 1 loadcat 0 0 0 1 0;*/
/*		Estimate '>90% Load' intercept 1 loadcat 0 0 0 0 1;*/
/**/
/*		Estimate '<50% v. 70-80% Load' loadcat 1 0 0 0 0;*/
/*		Estimate '>90% v. 70-80% Load' loadcat 0 0 0 0 1/exp;*/

run;

proc genmod  data=qcstudy.qc4 descending;
		class hospup area (ref='S') hospsizecat (ref = '4 Very Large') yearup (ref='1997')
			teaching (ref = 'N')  loadcat5 (ref = '7 GT 80%ile')/param=ref;
		 model postadmininf_rate = area /*yearup*/ hospsizecat teaching loadcat5 area*loadcat5
			/ dist = poisson link = id;

		where dayup < 18962;

/*		where teaching NE 'T';*/

/*		where hospup in ('083' '118' '096' '002' '119' '039' '092' '084' '019');*/
/*		where hospup in ('028');*/
/*		where hospup in ('005' '051' '057' '047' '015' '050' '034' '108' '010' '058');*/

		repeated subject = hospup / type = indep;

/*		Estimate 'I, N, VL v. S T Sm @40%' area 1 0 0 teaching 0 1 hospsizecat 0 0 1;*/
/*		Estimate 'I, N, VL v. S T Sm @>80%' area 1 0 0 teaching 0 1 hospsizecat 0 0 1 loadcat5 0 0 1;*/

/*		Estimate 'I v. S' area 1 0 0 Area*loadcat5 1 0 0 0 0 0 0 0 0/exp;*/
/*		estimate 'I @40% Load' intercept 1 area 1 0 0 loadcat5 1 0 0 Area*loadcat5 1 0 0 0 0 0 0 0 0/exp;*/
/*		estimate 'I @90% Load' intercept 1 area 1 0 0 loadcat5 0 0 0 Area*loadcat5 0 0 0 0 0 0 0 0 0/exp;*/
/*		estimate 'S' intercept 1;*/

/*		Estimate '50-60% Load' intercept 1 loadcat 0 1 0 0 0;*/
/*		Estimate '60-70% Load' intercept 1 loadcat 0 0 1 0 0;*/
/*		Estimate '70-80% Load' intercept 1 loadcat 0 0 0 0 0;*/
/*		Estimate '80-90% Load' intercept 1 loadcat 0 0 0 1 0;*/
/*		Estimate '>90% Load' intercept 1 loadcat 0 0 0 0 1;*/
/**/
/*		Estimate '<50% v. 70-80% Load' loadcat 1 0 0 0 0;*/
/*		Estimate '>90% v. 70-80% Load' loadcat 0 0 0 0 1/exp;*/

run;

proc genmod  data=qcstudy.qc4 descending;
		class hospup area (ref='S') hospsizecat (ref = '1 Small') 
			yearcat (ref='1 97-01') loadcat5 (ref = '7 GT 80%ile') 
			teaching (ref = 'T') /param=ref;
		 model dthrate = area yearcat hospsizecat teaching loadcat5 area*loadcat5
			/ dist = poisson link = id;

		where dayup < 18962;

		repeated subject = hospup / type = indep;

run;

proc genmod  data=qcstudy.qc4 descending;
		class hospup yearcat (ref='1 97-01') /param=ref;
		 model readmininf_rate = yearcat 
			/ dist = poisson link = id;

		where dayup < 18962;

		repeated subject = hospup / type = indep;

run;

proc genmod  data=qcstudy.qc4 descending;
		class hospup area (ref='S') hospsizecat (ref = '1 Small') 
			yearcat (ref='1 97-01') loadcat5 (ref = '7 GT 80%ile') 
			teaching (ref = 'T') /param=ref;
		 model pooroutcomerate = area yearcat hospsizecat teaching loadcat5 
			area*yearcat area*loadcat5 yearcat*loadcat5 area*yearcat*loadcat5
			/ dist = poisson link = log;

		where dayup < 18962;

		repeated subject = hospup / type = indep;

		estimate '(A)I 40% 07-11' intercept 1 area 1 0 0 loadcat5 1 0 0 yearcat 0 1 
			area*yearcat 0 1 0 0 0 0 yearcat*loadcat5 0 0 0 1 0 0 area*loadcat5 1 0 0 0 0 0 0 0 0
			area*yearcat*loadcat5 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0/exp;

		estimate '(B)I >80% 07-11' intercept 1 area 1 0 0 loadcat5 0 0 0 yearcat 0 1 
			area*yearcat 0 1 0 0 0 0 yearcat*loadcat5 0 0 0 0 0 0 area*loadcat5 0 0 0 0 0 0 0 0 0
			area*yearcat*loadcat5 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0/exp;

		estimate 'A/B' loadcat5 1 0 0 yearcat*loadcat5 0 0 0 1 0 0/exp;


/*		Estimate 'I 40%' intercept 1 area 1 0 0 yearcat 0 1 loadcat5 1 0 0 area*loadcat5 1 0 0 0 0 0 0 0 0;*/
/*		Estimate 'I >80%' intercept 1 area 1 0 0 yearcat 0 1 loadcat5 0 0 0 area*loadcat5 0 0 0 0 0 0 0 0 0;*/
/**/
/*		Estimate 'U 40%' intercept 1 area 0 0 1 yearcat 0 1 loadcat5 1 0 0 area*loadcat5 0 0 0 0 0 0 1 0 0;*/
/*		Estimate 'U >80%' intercept 1 area 0 0 1 yearcat 0 1 loadcat5 0 0 0 area*loadcat5 0 0 0 0 0 0 0 0 0;*/
run;

proc genmod  data=qcstudy.qc4 descending;
		class hospup area (ref='S') hospsizecat (ref = '1 Small') 
			yearcat (ref='1 97-01') loadcat5 (ref = '7 GT 80%ile') 
			teaching (ref = 'T') /param=ref;
		 model pooroutcomerate = area yearcat hospsizecat teaching loadcat5 area*loadcat5
			/ dist = poisson link = id;

		where dayup < 18962;

		repeated subject = hospup / type = indep;

		Estimate 'I 40%' intercept 1 area 1 0 0  loadcat5 1 0 0 area*loadcat5 1 0 0 0 0 0 0 0 0;
		Estimate 'I 90%' intercept 1 area 1 0 0  loadcat5 0 0 0 area*loadcat5 0 0 0 0 0 0 0 0 0;

		Estimate 'U 40%' intercept 1 area 0 0 1  loadcat5 1 0 0 area*loadcat5 0 0 0 0 0 0 1 0 0;
		Estimate 'U 90%' intercept 1 area 0 0 1  loadcat5 0 0 0 area*loadcat5 0 0 0 0 0 0 0 0 0;
run;

*>80% common;
proc genmod  data=qcstudy.qc4 descending;
		class hospup loadcat4 (ref = '3')/param=ref;

		model readminrate = loadcat4
			/ dist = poisson link = ident;

/*		where area not in ('I' 'U');*/
/*		where teaching NE 'T';*/

/*		where hospup in ('083' '118' '096' '002' '119' '039' '092' '084' '019');*/
		where hospup in ('052') ;
/*		where hospup in ('005' '051' '057' '047' '015' '050' '034' '108' '010' '058');*/

/*		repeated subject = hospup / type = indep;*/

		Estimate '<50% Load' intercept 1 loadcat4 1 0 0 0;
		Estimate '50-60% Load' intercept 1 loadcat4 0 1 0 0;
		Estimate '60-70% Load' intercept 1 loadcat4 0 0 0 0;
		Estimate '70-80% Load' intercept 1 loadcat4 0 0 1 0;
		Estimate '>80% Load' intercept 1 loadcat4 0 0 0 1;
/**/
/*		Estimate '<50% v. 70-80% Load' loadcat 1 0 0 0 0;*/
		Estimate '>80% v. 60-70% Load' loadcat4 0 0 0 1;

run;

*>70% common;
proc genmod  data=qcstudy.qc4 descending;
		class hospup loadcat4 (ref = '2')/param=ref;

		model readminrate = loadcat4
			/ dist = poisson link = ident;

/*		where area not in ('I' 'U');*/
/*		where teaching NE 'T';*/

/*		where hospup in ('083' '118' '096' '002' '119' '039' '092' '084' '019');*/
		where hospup in ('019') and loadcat < 5;
/*		where hospup in ('005' '051' '057' '047' '015' '050' '034' '108' '010' '058');*/

/*		repeated subject = hospup / type = indep;*/

		Estimate '<50% Load' intercept 1 loadcat4 1 0 0;
		Estimate '50-60% Load' intercept 1 loadcat4 0 0 0;
		Estimate '60-70% Load' intercept 1 loadcat4 0 1 0;
		Estimate '70-80% Load' intercept 1 loadcat4 0 0 1;
/**/
/*		Estimate '<50% v. 70-80% Load' loadcat 1 0 0 0 0;*/
		Estimate '70-80% v. 50-60% Load' loadcat4 0 0 1;

run;

proc freq data=qcstudy.qc4;
	tables hospup*loadcat4/nopct norow nocol;
/*	where area in ('S');*/

run;

proc genmod  data=test4a descending;
		class hospup loadcat (ref = '2')/param=ref;
		 model dthrate = loadcat
			/ dist = poisson link = ident;

/*		where hospup in ('083' '018' '020' '118' '096' '002' '119' '039' '092' '084' '019');*/
/*		where hospup in ('005' '051' '057' '047' '015' '050' '034' '108' '010' '058');*/

		repeated subject = hospup / type = indep;

run;

proc genmod  data=test4a descending;
		class hospup loadcat (ref = '2')/param=ref;
		 model pooroutcomerate = loadcat
			/ dist = poisson link = ident;

/*		where hospup in ('083' '018' '020' '118' '096' '002' '119' '039' '092' '084' '019');*/
/*		where hospup in ('005' '051' '057' '069' '015' '050' '034' '115' '010' '058');*/

		repeated subject = hospup / type = indep;

run;

proc genmod  data=qcstudy.qc4 descending;
		class hospup /param=ref;
		 model readminrate = zload
			/ dist = poisson link = ident;


		where hospup in ('083' '018' '020' '118' '096' '002' '119' '039' '092' '084' '019');
/*		where hospup in ('005' '051' '057' '069' '015' '050' '034' '115' '010' '058');*/

		repeated subject = hospup / type = indep;

run;

proc sql;
	select count(patient_id) from submidas
		where admdat <= 17901 <= dschdat and hosp = '001';

quit;

proc means data = test4 sum mean min max nway;
	var totdays totdth;
	class hospup;

	where 17898 <= dayup <= 18262;

	output out = test5 sum(totdays)=sumptdays mean(totdays)=meanpt min(totdays)=minpt 
		max(totdays)=maxpt sum(totdth)=sumdth mean(totdth)=meandth min(totdth)=mindth 
		max(totdth)=maxdth sum(totreadm30day)=sumtotreadm30day 
		sum(totpooroutcome)=sumtotpooroutcome;

run;

proc sort data=test5;
	by hospup;
run;

proc sort data=hospitals;
	by hospup;
run;

data test6;
	merge test5 (in=a) hospitals (in=b);
	by hospup;
	if a;

	avgload = meanpt/beds;
	lowload = minpt/beds;
	highload = maxpt/beds;
	dthrate = sumdth/sumptdays;
	readminrate = sumtotreadm30day/sumptdays;
	pooroutcomerate = sumtotpooroutcome/sumptdays;

run;

proc sort data=test6;
	by name;
run;

proc print data=test6;
	var hospup name beds meanpt avgload minpt lowload maxpt highload
		dthrate readminrate pooroutcomerate;
run;

proc genmod  data=qcstudy.qc4 descending;
		class hospup hospsizecat (ref = '4 Very Large') yearup (ref='2008')
			teaching (ref = 'N')  loadcat5 (ref = '7 GT 80%ile')/param=ref;
		 model postadmininf_rate = yearup hospsizecat teaching loadcat5
			/ dist = poisson link = id;

		where 17538 < dayup < 18962 and area = 'I';

/*		where teaching NE 'T';*/

/*		where hospup in ('083' '118' '096' '002' '119' '039' '092' '084' '019');*/
/*		where hospup in ('028');*/
/*		where hospup in ('005' '051' '057' '047' '015' '050' '034' '108' '010' '058');*/

		repeated subject = hospup / type = indep;

/*		Estimate 'I, N, VL v. S T Sm @40%' area 1 0 0 teaching 0 1 hospsizecat 0 0 1;*/
/*		Estimate 'I, N, VL v. S T Sm @>80%' area 1 0 0 teaching 0 1 hospsizecat 0 0 1 loadcat5 0 0 1;*/

/*		Estimate 'I v. S' area 1 0 0 Area*loadcat5 1 0 0 0 0 0 0 0 0/exp;*/
/*		estimate 'I @40% Load' intercept 1 area 1 0 0 loadcat5 1 0 0 Area*loadcat5 1 0 0 0 0 0 0 0 0/exp;*/
/*		estimate 'I @90% Load' intercept 1 area 1 0 0 loadcat5 0 0 0 Area*loadcat5 0 0 0 0 0 0 0 0 0/exp;*/
/*		estimate 'S' intercept 1;*/

/*		Estimate '50-60% Load' intercept 1 loadcat 0 1 0 0 0;*/
/*		Estimate '60-70% Load' intercept 1 loadcat 0 0 1 0 0;*/
/*		Estimate '70-80% Load' intercept 1 loadcat 0 0 0 0 0;*/
/*		Estimate '80-90% Load' intercept 1 loadcat 0 0 0 1 0;*/
/*		Estimate '>90% Load' intercept 1 loadcat 0 0 0 0 1;*/
/**/
/*		Estimate '<50% v. 70-80% Load' loadcat 1 0 0 0 0;*/
/*		Estimate '>90% v. 70-80% Load' loadcat 0 0 0 0 1/exp;*/

run;

data qcstudy.Miqcdatafullcomorb;
	set heredevl.Miqcdatafullcomorb;

run;
	
