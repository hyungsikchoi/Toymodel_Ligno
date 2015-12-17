$if not dexist .\t  $call mkdir .\t
********************
*-save .\t\a1

*********************************************************
***
*** toy model for lignocellosic biomass growth in Germany
***
***               hyung-sik choi, 2015.10.20
*********************************************************

set
biomass/crop,grass,ligno/
it(biomass) /crop,grass/
one /germany/
;

alias (biomass,biomass1);

Parameter
area_int(one,biomass)          scaling paramter for land
elastsp(one,biomass,biomass)   crop price elasticity of area
elast_landpr(one,biomass)       land price elasticity of area
elasthd(one,biomass,biomass)    price elasticity of demand
bend_ld1(one)                  land bending parameter
shift_ld(one)                  land shift paprameter
Q_NEW1(one)                    total land available
ini_area(one,biomass)
base_area(biomass)            intial area for biomnass    (1000ha)
Yield(one,biomass)            fixed yield parameter       (1000ha)
hdem_int(one,biomass)         scaling factor for consumption
subs_shift(one,biomass)       demand shifter for biomass
;

Variable
ALAREA(one,biomass)           area
LANDPRICE(one)                land price
LANDSUPPLY(one)               supply of biomass
Diff(one,biomass)             diffusion variable
beta(one,biomass)             distribution
PD(one,biomass)               biomass price
HDEM(one,biomass)             demand of biomass
;

equation
Eq_Area(one,biomass)
EQ_A1(one)
EQ_LANDMRKT1(one)
EQ_Diff(one,biomass)
EQ_beta(one,biomass)
EQ_price(one,biomass)
Eq_Demand(one,biomass)
Eq_Market(one,biomass)
Eq_HDEM(one,biomass)
;

scalar
Peak_Area
alpha
time
T_zero
base_grass
base_crop_grass
calib
LP_feedback
;

*************************
*****Parameter assignment
*************************

elastsp(one,'crop','crop') =   0.5;
elastsp(one,'grass','grass') = 0.1;
****grass changed from 0.06 to 0.1
elastsp(one,'ligno','ligno') = 0.3;


elast_landpr(one,'crop') = -0.075;
***crop inital -0.04
elast_landpr(one,'grass') = -0.02;
***grass inital -0.004
elast_landpr(one,'ligno') = -0.03;


elasthd(one,'crop','crop') = -0.15;
***-0.05
elasthd(one,'grass','grass') = -0.6;
***-0.05
elasthd(one,'ligno','ligno') = -0.05;

**********************************
*****Land data example from ESIM**
**********************************

$ontext
Table land_data(cc,land_st_) Data for calibration land supply function
          landprice  shift_land   max_land_use
AT        200        2            3225.26000
BE        185        3            1446.55980
DK        290        5            2798.36920
FI        150        4            2228.50000
FR        123        60           28660.39000
GE        200        40           17145.95410
;
$offtext

shift_ld(one) = 40 ;
Q_NEW1(one) =   17145;

Q_NEW1(one) = Q_NEW1(one) - 207*0.5;
*****207 is set aside area

display Q_NEW1;

********************************************
*****Diffusion logistic funtion parameters**
********************************************


Peak_Area = 1;
alpha     = 0.5;
time      = 1;
T_zero    = 16;

*********************************
*****Initial value assignment  **
*********************************

ALAREA.l(one,'crop') =  11358;
ALAREA.l(one,'grass') =  4574;
ALAREA.l(one,'ligno') =    10;

Yield(one,'crop') =  8;
Yield(one,'grass') = 6;
Yield(one,'ligno') = 20;

base_area(biomass) = ALAREA.l('germany',biomass);

landsupply.l(one) = sum(biomass,ALAREA.l(one,biomass));

landprice.l(one) = 200;

PD.l(one,'crop')=  200;
PD.l(one,'grass')= 100;
PD.l(one,'ligno')= 80;

HDEM.l(one,biomass) = ALAREA.l(one,biomass)* Yield(one,biomass);

*******intial Diffusion factor value set to 1.648 for the model calibration to LB base area 10,000 in Germany
*Diff.l(one,'ligno') = 2.747;
Diff.l(one,'crop') =  1;
Diff.l(one,'grass') =  1;
*Diff.l(one,'ligno') = 1/(1+exp(-alpha*(time-T_zero)));

beta.l(one,biomass)=1;

subs_shift(one,biomass)=1;



********************************
**** scenario setting parameters
********************************
scalar competition,area_change
;
competition = 1;
LP_feedback = 0;
calib=1;

*****************************
****Parameter initialization
*****************************

area_int(one,biomass)
 =
    [ log(ALAREA.l(one,biomass))
         - elastsp(one,biomass,biomass)*log(PD.l(one,biomass))
         - elast_landpr(one,biomass)*log(landprice.l(one))
*        - log(Peak_Area*Diff.l(one,biomass))
     ]
;

hdem_int(one,biomass)
 =  [     log(HDEM.l(one,biomass))
         - elasthd(one,biomass,biomass)*log(PD.l(one,biomass))
     ]
;


bend_ld1(one) = (Q_NEW1(one) - landsupply.l(one)) * (shift_ld(one) + landprice.l(one))
;



display area_int, bend_ld1,Diff.l,landprice.l,landsupply.l,hdem_int;




*0) Human demand  (checked)

Eq_HDEM(one,biomass)..     0    =E=
         [-HDEM(one,biomass)  +
         (subs_shift(one,biomass)*exp(hdem_int(one,biomass) +
         elasthd(one,biomass,biomass)*log(PD(one,biomass))))
         ]
;


Eq_Market(one,biomass)..     0    =E=
        [ -HDEM(one,biomass) +
           ALAREA(one,biomass)*Yield(one,biomass)
        ]
;



Eq_Area(one,biomass)..      0  =E=

         [-ALAREA(one,biomass) +
         exp(
         area_int(one,biomass)
         +elastsp(one,biomass,biomass)*log(PD(one,biomass))
         +elast_landpr(one,biomass) *log(LANDPRICE(one)))
         ]$calib
        +
         [-ALAREA(one,biomass) +
         exp(
         area_int(one,biomass)
         +elastsp(one,biomass,biomass)*log(PD(one,biomass))
         +elast_landpr(one,biomass) *log(LANDPRICE(one)))*Diff(one,biomass)
         ]$(not calib)

;


*Eq_Area_Diff(one,biomass)..   0  =E=
*         [-ALAREA_diff(one,biomass) +
*           ALAREA(one,biomass)*
*         ]
*;




EQ_Diff(one,biomass)..    0 =E=

*      [
*       -Diff(one,biomass) + Peak_Area*exp(-1*(LANDPRICE(one)-200)/200)/(1+exp(-alpha*(time-T_zero)))
*      ]$( sameas(biomass,'ligno') and not calib)
*     +
      [
       -Diff(one,biomass) + exp(-1*(LANDPRICE(one)-200)/200)/(1+exp(-alpha*(time-T_zero)))
      ]$(sameas(biomass,'ligno') and LP_feedback and not calib)
      +
      [
       -Diff(one,biomass) + ((Peak_Area-1)*(1/(1+exp(-alpha*(time-T_zero))))+1)
      ]$(sameas(biomass,'ligno') and  not LP_feedback and not calib)
      +
      [
       -Diff(one,biomass) + 1
      ]$(sameas(biomass,'crop') or sameas(biomass,'grass') and competition and not calib)


      +
      [
*       -Diff(one,biomass) + (1-(ALAREA(one,'ligno')-base_area('ligno'))*beta(one,biomass)/base_area(biomass))
       -Diff(one,biomass) + (1-( ALAREA(one,'ligno')-base_area('ligno'))*beta(one,biomass)/base_area(biomass))

      ]$(sameas(biomass,'crop') or sameas(biomass,'grass') and not competition and not calib)

      +
      [
       -Diff(one,biomass) + 1
      ]$(calib)


;

*### beta ratio determination between grass, crop

EQ_beta(one,biomass)$(not sameas(biomass,'ligno'))..    0 =E=
              [
*-beta(one,biomass) + PD(one,'grass')/(PD(one,'grass')+PD(one,'crop'))
           -beta(one,biomass) + base_area('crop')/(base_area('crop')+base_area('grass'))

              ]$( not calib and sameas(biomass,'crop'))
           +
              [
* -beta(one,biomass) + PD(one,'crop')/(PD(one,'grass')+PD(one,'crop'))
     -beta(one,biomass) + base_area('grass')/(base_area('crop')+base_area('grass'))

              ]$( not calib and sameas(biomass,'grass'))
           +
              [   -beta(one,biomass)  + 1
              ]$( calib)

;

*### )  land supply
EQ_A1(one)..   0  =E=
         [-LANDSUPPLY(one)
          + Q_NEW1(one) - bend_ld1(one)/(shift_ld(one) +  LANDPRICE(one))]
;


*### )  land demand
EQ_LANDMRKT1(one)..    0  =E=
         [-LANDSUPPLY(one)
*           + SUM(crops, ALAREA(one,crops))];
           + (SUM(biomass, ALAREA(one,biomass)))]
;






model toymodel mcp model structure
/
Eq_HDEM.HDEM
Eq_Market.PD
Eq_Area.ALAREA

EQ_Diff.Diff
EQ_beta.beta
EQ_A1.LANDPRICE
EQ_LANDMRKT1.LANDSUPPLY
*EQ_price.PD
/;



SOLVE toymodel using MCP;

display ALAREA.l,diff.l,LANDPRICE.l,diff.l,beta.l,PD.l,hdem.l;



*SOLVE toymodel using MCP;  ;


*display ALAREA.l,diff.l,LANDPRICE.l,diff.l,beta.l,PD.l,hdem.l;

*ini_area(one,biomass) = ALAREA.l(one,biomass);
