*create table with recdid of 30 day readmissions;
proc sql;
	create table qcstudy.readmins as
		select distinct readminrecdid from qcstudy.qc1
			where readm30day = 1;

quit;

*create table of the original recdid from the admission prior to the 30 day readmission;
proc sql;
	create table qcstudy.origadmins as
		select distinct recdid from qcstudy.qc1
			where readm30day = 1;

quit;

*pull out all the dx codes for the readmission record; 
proc sql;
	create table qcstudy.allreadminicd9codes as
		select recdid, dx from heredevl.alldxs
			where recdid in (
				select readminrecdid from qcstudy.readmins);

quit;

*pull out all the dx codes for the original admission record; 
proc sql;
	create table qcstudy.allorigicd9codes as
		select recdid, dx from heredevl.alldxs
			where recdid in (
				select recdid from qcstudy.origadmins);

quit;

*create a file with the dx's that were infections from the readmissions;
proc sql;
	create table qcstudy.readmininfections as (
		select distinct recdid, 1 as readmininf_flag from qcstudy.allreadminicd9codes
			where dx like '48%' or dx like '59%' or dx like '77%' or (dx between '001' and '13999') or dx like '46%'
				or dx like '73%' or dx like '99%'  or dx like '68%');

quit;

*create a file with the dx's that were infections from the original admissions;
proc sql;
	create table qcstudy.originfections as (
		select distinct recdid, 1 as originf_flag from qcstudy.allreadminicd9codes
			where dx like '48%' or dx like '59%' or dx like '77%' or (dx between '001' and '13999') or dx like '46%'
				or dx like '73%' or dx like '99%'  or dx like '68%');

quit;

*add on the flag for infections at initial;
proc sort data=qcstudy.qc1;
	by recdid;
run;

proc sort data=qcstudy.originfections;
	by recdid;
run;

data qcstudy.allinfections;
	merge qcstudy.qc1 (in=a) qcstudy.originfections (in=b);
	by recdid;

	if originf_flag = . then originf_flag = 0;
run;


*add on the flag for infections on readmission;
proc sort data=qcstudy.allinfections;
	by readminrecdid;
run;

proc sort data=qcstudy.readmininfections;
	by recdid;
run;

data qcstudy.allinfections;
	merge qcstudy.allinfections (in=a) qcstudy.readmininfections (in=b rename=(recdid=readminrecdid));
	by readminrecdid;

	if readmininf_flag = . then readmininf_flag = 0;
run;

proc freq data=qcstudy.readmininfections;
	tables inf_flag;
	where readminrecdid is not null;
run;

proc sql;
	update qcstudy.readmininfections
		set inf_flag = '0'
			where inf_flag is null;
quit;

proc sql;
	update qcstudy.readmininfections
		set inf_flag = '2'
			where readminrecdid is not null and inf_flag = '0';
quit;

proc freq data=qcstudy.qc4;
	tables area*hospup*loadcat5;
	where area in ('I' 'U');
run;
