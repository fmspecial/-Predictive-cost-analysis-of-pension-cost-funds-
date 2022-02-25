

/*QUESTION 1*/
data  pfarg06;
set perm.pfarg06;
Y = 1000 * ( B3 + B4 + B6 + B7 + B8)/A1;
PER2 = A2/A1;
PER3 = A3/A1;
PER4 = A4/A1;
PER5 = A5/A1;
PER6 = A6/A1;
PER7 = A7/A1;
run;





/*mean and standard deviation PER6 PER7; */
proc means data = pfarg06 ;
var Y A1 PER2 PER3 PER4 PER5 PER6 PER7;
run;




/* frequency distribution */
proc freq data = pfarg06;
tables c1 c2 c3 c4 c5 c6 c7 c8 / nocum ;
run;




/*saving perm.pfarg in mydata library*/
data mydata.pfarg;
set pfarg06;
run;


/*parameterizing using dummy variable*/
proc reg data = pfarg ;
model Y =  c1a c1b c1c C2 C3 C5 C6 
						C7 C8 A1 PER2 PER3 PER4 PER5 PER6 PER7;
plot student. * (p.  c1a c1b c1c C2 C3 C5 C6 C7 C8 A1 PER2 PER3 PER4 PER5 PER6 PER7) ;
output out= FITTED p= pred student= sresid rstudent= Dresid;
run;
quit;
proc univariate data= FITTED noprint;
histogram sresid/normal (noprint);
qqplot sresid;
run;
symbol1 v = plus c = black;
symbol2 v = circle c = black;
proc gplot data = FITTED;
plot (Sresid Dresid) * Pred / overlay vref = 0;
run;
quit;
symbol v = plus c = black;
/*transformation of variables*/
data  pfarg01;
set pfarg;
LY = log(Y); LA1 = log(A1); LPer2 = log(Per2); LPer3 = log(per3); LPER4 = log(PER4); 
					LPER5= Log(per5) ;Lper6 = log(per6);  LPer7 = log(Per7);
					
run;

/*DIAGNOSTIC ANALYSIS OF TRANSFORMED DATASET*/
proc reg data = pfarg01 NOPRINT;

model LY =  c1a c1b c1c C2 C3 C5 C6 
						C7 C8  LA1 LPER2	LPER3 LPER4 LPER5 LPER6 LPER7;
plot student. * (p.  c1a c1b c1c C2 C3 C5 C6 C7 C8 LA1 LPER2	LPER3 LPER4 LPER5 LPER6 LPER7) ;
output out= FITTED2 p= pred student= sresid rstudent= Dresid;
run;
quit;
/*HISTOGRAM OF RESIDUAL AND Q-Q PLOT*/
proc univariate data= FITTED2 noprint;
histogram sresid/normal (noprint);
qqplot sresid;
run;
proc gplot data = FITTED2;
plot (Sresid Dresid) * Pred / overlay vref = 0;
run;
quit;
symbol v = plus c = black;
/*QUESTION 4
model selection */
Proc glm data= mydata.pfarg noprint;
class  c1;

model Y = c1 c2 c3 c5 c6 c7 c8 a1 per2 per3 per4 per5 per6 per7 / ss3 solution ;
/*plot student. * (p. c1 c2 c3 c5 c6 c7 c8 a1 per2 per3 per4 per5 per6 per7);*/

output out= FITS p= pred student= sresid rstudent= Dresid;
run;
quit;
/*model of the entire variable*/
proc glm data= pfarg01;
class c1;
model ly = c1 c2 c3  c5 c6 c7 c8 la1 lper2 lper3 lper4 lper5 lper6 lper7 / ss3 solution;
run;
/*reduced model*/
proc glm data= pfarg01;
class c1;
model ly = c1 c2     la1 lper2 lper3 / ss3 solution;
run;
/*backward elimination*/
proc reg data = pfarg01;

model LY =  c1a c1b c1c C2 C3 C5 C6 C7 C8  LA1 LPER2	LPER3 LPER4 LPER5 LPER6 LPER7
						/ selection = forward slentry= 0.05 sltay = 0.05;
run;

data pfarg;
set pfarg01;
if c1 = 1 then c1a = 1; else c1a = 0;
if c1= 2 then c1b = 1; else c1b = 0;
if c1 = 3 then c1c = 1; else c1c = 0;
run;
/* added variable plots fro question 5*/
proc reg data = pfarg01 noprint;

model ly = c1a c1b c1c c2 la1 lper2 lper3;
plot ly * p.;
run;
quit;
proc gplot data = pfarg01;
plot ly*(c1a c1b c1c c2 la1 lper2 lper3);
run;
quit;
/*added  variable plot for la1*/
proc reg data = pfarg01 noprint;
model ly = c1a c1b c1c c2 lper2 lper3 ;
output out = FITS1 r = Residy;
run;
quit;

proc reg data = FITS1 noprint;
model la1 =  c1a c1b c1c c2 lper2 lper3;
output out = FITS2 r = Residx;
run;
quit;

data FITS2;set FITS2;
label Residy = 'Residy'
      Residx = 'Residx';
run;

proc gplot data = FITS2;
plot Residy*Residx;
run;
quit;

/* Added Variable Plot for lper2 */

proc reg data = pfarg01 noprint;
model ly = c1a c1b c1c c2 la1 lper3 ;
output out = FITS1 r = Residy;
run;
quit;

proc reg data = FITS1 noprint;
model lper2 =  c1a c1b c1c c2 la1 lper3;
output out = FITS2 r = Residx;
run;
quit;

data FITS2;set FITS2;
label Residy = 'Residy'
      Residx = 'Residx';
run;

proc gplot data = FITS2;
plot Residy*Residx;
run;
quit;


/* Added Variable Plot for lper3 */

proc reg data = pfarg01 noprint;
model ly = c1a c1b c1c c2 la1 lper2 ;
output out = FITS1 r = Residy;
run;
quit;

proc reg data = FITS1 noprint;
model lper3 =  c1a c1b c1c c2 la1 lper2;
output out = FITS2 r = Residx;
run;
quit;

data FITS2;set FITS2;
label Residy = 'Residy'
      Residx = 'Residx';
run;

proc gplot data = FITS2;
plot Residy*Residx;
run;
quit;
data mydata.pfarg01;
set pfarg ;
run;


/* QUESTION 6 -influencial points */
proc reg data =  pfarg01 noprint;
model LY =  C1A  C1B  C1C  C2   LA1  LPER2  LPER3;
output out = INFL1 p = Pred dffits = Dff;
run;
quit;

data INFL1; set INFL1;
Adff = abs(Dff);
run;

proc sort data = INFL1;
by descending Adff;
run;

data INFL1; set INFL1;
Index = _N_;
run;

proc gplot data = INFL1;
plot ADff*Index / vref = 0.73 vref = 2;
run;
quit;

proc print data = INFL1;
var  Pred LY Dff;
where Index <= 6;
run;

/* Further Diagnostics on Potential Influential Points */

proc reg data = INFL1;
model  LY =  C1A  C1B  C1C  C2   LA1  LPER2  LPER3 / influence;
output out = INFL2 h = H rstudent = Dresid covratio = C;
run;
quit;

proc print data = INFL2;
var LY  Dff H Dresid C;
where Index <= 6;
run;
/*question 7: confidence limit for predicted cost per active member,
and an appropriate corresponding 95% confidence interval*/
proc reg data = pfarg01 noprint;
model  LY =  C1a c1b c1c  C2   LA1  LPER2  LPER3;
output out = PRED2 p = Pred  lcl = LowerM ucl = UpperM;
run;
quit;
proc print data= pred2 ;
var c1a c1b c1c c2 LA1 LPER2 LPER3 pred lowerM upperM;
run;



/*using proc glm*/
proc glm data = pfarg01 ;
class c1;
model LY =    c1 C2 C3 C5 C6 
						C7 C8  LA1 LPER2	LPER3 LPER4 LPER5 LPER6 LPER7 / ss3 solution;
output out= FITTED p= pred student= sresid rstudent= Dresid;
run;
quit;
/*using proc glm*/
proc glm data = pfarg01 ;
class c1;
model LY =    c1 C2  
						  LA1 LPER2	LPER3    / ss3 solution;
output out= FITTED p= pred student= sresid rstudent= Dresid;
run;
quit;
 

proc reg data = pfarg01 NOPRINT;

model LY =  c1a c1b c1c C2 C3 C5 C6 
						C7 C8  LA1 LPER2	LPER3 LPER4 LPER5 LPER6 LPER7;
plot student. * (p.  c1a c1b c1c C2 C3 C5 C6 C7 C8 LA1 LPER2	LPER3 LPER4 LPER5 LPER6 LPER7) ;
output out= FITTED2 p= pred student= sresid rstudent= Dresid;
run;
quit;
proc univariate data= FITTED2 noprint;
histogram sresid/normal (noprint);
qqplot sresid;
run;
proc gplot data = FITTED2;
plot (Sresid Dresid) * Pred / overlay vref = 0;
run;
quit;
symbol v = plus c = black;


/*backward elimination*/
proc reg data = pfarg01;

model lY =  c1a c1b c1c C2 C3 C5 C6 C7 C8  lA1 lPER2	lPER3 lPER4 lPER5 lPER6 lPER7
						/ selection = backward slentry= 0.05 sltay = 0.05;
run;

/*using proc glm*/
proc glm data = pfarg01 ;
class c1;
model LY =    c1 C2 C3 C5 C6 
						C7 C8  LA1 LPER2	LPER3 LPER4 LPER5 LPER6 LPER7 / ss3;

run;
quit;
/*lper6*/
proc glm data = pfarg01 ;
class c1;
model LY =    c1 C2 C3 C5 C6 
						C7 C8  LA1 LPER2	LPER3 LPER4 LPER5  LPER7 / ss3 ;

run;
quit;
/*c8*/
proc glm data = pfarg01 ;
class c1;
model LY =    c1 C2 C3 C5 C6 
						C7   LA1 LPER2	LPER3 LPER4 LPER5  LPER7 / ss3 ;

run;
quit;
/*c7*/
proc glm data = pfarg01 ;
class c1;
model LY =    c1 C2 C3 C5 C6 
						   LA1 LPER2	LPER3 LPER4 LPER5  LPER7 / ss3 ;
run;
quit;
/*per5*/
proc glm data = pfarg01 ;
class c1;
model LY =    c1 C2 C3 C5 C6 
						   LA1 LPER2	LPER3 LPER4  LPER7 / ss3 ;
run;
quit;
/*c5*/
proc glm data = pfarg01 ;
class c1;
model LY =    c1 C2 C3  C6 
						   LA1 LPER2	LPER3 LPER4  LPER7 / ss3 ;
output out= FITTED p= pred student= sresid rstudent= Dresid;
run;
quit;
/*c6*/
proc glm data = pfarg01 ;
class c1;
model LY =    c1 C2 C3   
						   LA1 LPER2	LPER3 LPER4  LPER7 / ss3 ;
run;
quit;
/*per7*/
proc glm data = pfarg01 ;
class c1;
model LY =    c1 C2 C3   
						   LA1 LPER2	LPER3 LPER4  / ss3 ;
run;
quit;
/*per4*/
proc glm data = pfarg01 ;
class c1;
model LY =    c1 C2 C3   
						   LA1 LPER2	LPER3   / ss3 ;
run;
quit;
/*c3*/
proc glm data = pfarg01 ;
class c1;
model LY =    c1 C2    
						   LA1 LPER2 	LPER3   / ss3 ;
run;
quit;
proc reg data = PRED1 noprint;
model Sales = Pot Adv Share Accts;
output out = PRED2 p = Pred h = H lcl = Lower ucl = Upper;
run;
quit;
