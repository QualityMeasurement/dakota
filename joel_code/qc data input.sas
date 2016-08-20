data dmpe.Miqcdatafull;
	format NEWDTD PATBDTE date9.;
	set heredevl.Miqcdatafull
		(sasdatefmt=(
		FIRSTMIDATE='date9.'
		FIRSTMIDSCDATE='date9.'
		SECONDMIDATE='date9.'
		SECONDMIDSCDATE='date9.'
		THIRDMIDATE='date9.'
		THIRDMIDSCDATE='date9.'
		FIRSTREADMINDATE='date9.'
		SECONDREADMINDATE='date9.'
		THIRDREADMINDATE='date9.'));

	if ('I210' <= cause <= 'I219') or substr(cause,1,3) = '410' 
		then MIDEATH = 1; else MIDEATH = 0;

run;

data dmpe.Miqcdata;
	format NEWDTD PATBDTE date9.;
	set dmpe.Miqcdatafull;
	
	firstmiyr = year(FIRSTMIDATE);

run;

proc freq data=dmpe.Miqcdatafull;
	tables firstmihosp*firstmiyr/nocol norow nopct;
	where firstmiyr >= 2004 and firstmiyr < 2010;
run;

proc freq data=dmpe.Miqcdatafull;
	tables mideath;
run;

