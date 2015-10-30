
*-restart .\t\a1


*option MCP=gamschk;

$onecho > %system.fn%.gck
blockpic
blocklist
analysis
displaycr
nonopt
postopt
$offecho

set times /1*30/;

Parameter
year(times)
demand_inc(times);

year(times)=ord(times);
scalar a;
a =150/card(times);
***final demand should increase by 150times to match with the supply mid scenario
**initial demand 200 k ton from 10,000 ha >>>>   1.5mil. ha *20 ton/ha = 30 mil. ton
****final demand have to increase 150 times from the above calculation.

**linear increase of demand
*demand_inc(times)=a*ord(times);

**logistic increase of demand like supply
demand_inc(times)=149*1/(1+exp(-alpha*((ord(times)+1)-T_zero)));
demand_inc(times)=demand_inc(times)+1;
demand_inc('1')=1;

parameter diffusion(times);
*149/2
diffusion(times) = 149*(1/(1+exp(-alpha*(ord(times)-T_zero))));
diffusion(times) = diffusion(times)+1;
diffusion('1')=1;
parameter areatt(one,times);


Peak_Area=150;

areatt(one,times)
 =      exp(
         area_int(one,'ligno')
         +elastsp(one,'ligno','ligno')*log(PD.l(one,'ligno'))
         +elast_landpr(one,'ligno') *log(LANDPRICE.l(one)))
        *((Peak_Area-1)*(1/(1+exp(-alpha*(ord(times)-T_zero))))+1)
;


display year,demand_inc,diffusion,areatt,Peak_Area;

*$stop



set
ligno
/high,mid,low/
*mid,low /

compe
 / Shift_LB_only, Shift_all/


compe_se(compe)
 / Shift_LB_only,Shift_all/

ligno_se(ligno)
 /high,mid,low/

*Land_exp,Land_exp_LP_feedback/

scen  /1*1000/
;



parameter scen_area(ligno);

scen_area('high') =250;
scen_area('mid') = 150;
scen_area('low') = 100;

set map_scenario(compe,ligno,scen);

SCALAR COUNT /0/;

$onOrder
*Scen
$offOrder

Loop((compe_se,ligno_se),
 COUNT=COUNT+1;
map_scenario(compe_se,ligno_se,scen)
  $(ord(scen)=COUNT)
  = yes; );



display map_scenario;




Parameter report(compe,ligno,times,one,*,*);

loop(map_scenario(compe_se,ligno_se,scen),


calib =0;

If(sameas(compe_se,'Shift_LB_only'),
competition = 1;
LP_feedback = 0;

elseif sameas(compe_se,'Shift_all'),
competition = 0;
LP_feedback = 0;

else
competition = 1;
LP_feedback = 1;
  );



ALAREA.l(one,'crop') =  11358;
ALAREA.l(one,'grass') =  4574;
ALAREA.l(one,'ligno') =    10;

HDEM.l(one,biomass) = ALAREA.l(one,biomass)* Yield(one,biomass);

*beta.l(one,biomass)=1;
PD.l(one,'crop')=  200;
PD.l(one,'grass')= 100;
PD.l(one,'ligno')= 80;

landsupply.l(one) = sum(biomass,ALAREA.l(one,biomass));
landprice.l(one) = 200;

Diff.l(one,biomass) = 1;

*Diff.l(one,'ligno') = 1*exp(-1*(LANDPRICE.l(one)-200)/200)/(1+exp(-alpha*(1-T_zero)));

$ontext
IF (sameas(ligno_se,'high'),
Diff.l(one,'ligno') = Peak_Area*exp(-1*(LANDPRICE.l(one)-200)/200)/(1+exp(-alpha*(1-T_zero)));

elseif sameas(ligno_se,'mid'),
Diff.l(one,'ligno') = Peak_Area*exp(-1*(LANDPRICE.l(one)-200)/200)/(1+exp(-alpha*(1-T_zero)));

elseif sameas(ligno_se,'low'),
Diff.l(one,'ligno') = Peak_Area*exp(-1*(LANDPRICE.l(one)-200)/200)/(1+exp(-alpha*(1-T_zero)));
);
$offtext

display diff.l;

*area_int(one,biomass)
* =     [ log(ALAREA.l(one,biomass))
*         - elastsp(one,biomass,biomass)*log(PD.l(one,biomass))
*         - elast_landpr(one,biomass)*log(landprice.l(one))
*        - log(Diff.l(one,biomass))
*     ];

*display area_int;





*Peak_Area = scen_area(ligno_se);

loop (times,


ALAREA.LO(one,biomass) = 0;
ALAREA.UP(one,biomass) = inf;

PD.LO(one,biomass) = 0;
landprice.LO(one) = 0;



time= year(times);

subs_shift(one,'ligno')=demand_inc(times);



if((time eq 1),
Peak_Area=1;

);


if((time gt 2),

Peak_Area=scen_area(ligno_se)
*Peak_Area=1;

);


display Peak_Area,time,subs_shift;


SOLVE toymodel using MCP;


report(compe_se,ligno_se,times,one,'area',biomass) = ALAREA.l(one,biomass);
report(compe_se,ligno_se,times,one,'landPr',biomass) = LANDPRICE.l(one);
report(compe_se,ligno_se,times,one,'Diff',biomass) = diff.l(one,biomass);
report(compe_se,ligno_se,times,one,'price',biomass) = PD.l(one,biomass);
report(compe_se,ligno_se,times,one,'demand',biomass) = hdem.l(one,biomass);
*report(compe_se,ligno_se,times,one,'supply',biomass) = ALAREA.l(one,biomass)*yield(one,biomass);
*report(compe_se,ligno_se,times,one,'demand',biomass) = HDEM.l(one,biomass);

 );
);


report(compe_se,ligno_se,times,one,'area','total')
 =sum(biomass,report(compe_se,ligno_se,times,one,'area',biomass))
;

report(compe_se,ligno_se,times,one,'Price','index')
 $sum(biomass$(not sameas(biomass,'ligno')),
  report(compe_se,ligno_se,'1',one,'Area',biomass)*report(compe_se,ligno_se,'1',one,'price',biomass))
 =
 sum(biomass $(not sameas(biomass,'ligno')),
  report(compe_se,ligno_se,'1',one,'Area',biomass)*report(compe_se,ligno_se,times,one,'price',biomass))
 /sum(biomass$(not sameas(biomass,'ligno')),
  report(compe_se,ligno_se,'1',one,'Area',biomass)*report(compe_se,ligno_se,'1',one,'price',biomass))*100

;

report(compe_se,ligno_se,times,one,'area','total')
 = report(compe_se,ligno_se,times,one,'area','total')
  - sum(biomass,base_area(biomass))
;

report(compe_se,ligno_se,times,one,'area','gap')
 = Q_NEW1(one)-report(compe_se,ligno_se,times,one,'area','total')

;


report(compe_se,ligno_se,times,one,'area',biomass)
 = report(compe_se,ligno_se,times,one,'area',biomass)
  - base_area(biomass)
;


report(compe_se,ligno_se,times,one,'landPr',biomass)
 = report(compe_se,ligno_se,times,one,'landPr',biomass)
 -200;

*$ontext




*$offtext


option report:2:4:1
display report;

Parameter total_land(compe,ligno_se,one);

*total_land(compe,ligno_se,one)
* = report(compe,ligno_se,'30',one,'area','total');

display Peak_Area;
*display total_land;



Execute_unload ".\input\results",report;
