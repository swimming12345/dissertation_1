//saralamba@gmail.com

#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;
//logistic growth assumed
// double popfn(double t, double K, double P0, double r){
//   return(K*P0*exp(r*((t+1999)-1998))/(K+P0*(exp(r*((t+1999)-1998))-1)));
// }

double popfn(double time, double K, double P0, double r){
  return(K*P0*exp(r*(time-1998))/(K+P0*(exp(r*(time-1998))-1)));
}

// [[Rcpp::export]]
List PanHepC(double time, NumericVector state, List parms){

  //get the value of each parameter from parms
  //population parameter
  double K = parms["K"];
  double P0 = parms["P0"];
  double r = parms["r"];
  double flowin = parms["flowin"];
  //  double caset0 = parms["caset0"];
  // screen and treat parameters
  //standard treatment from 2004, DAAs from 2019
  double standard_start = parms["standard_start"];
  double new_start = parms["new_start"];
  double nscr = parms["nscr"];
  double scr_yr = parms["scr_yr"];
  double scr_cov = parms["scr_cov"];
  double sens = parms["sens"];
  double pF0scr = parms["pF0scr"];
  double pF1scr = parms["pF1scr"];
  double pF2scr = parms["pF2scr"];
  double pF3scr = parms["pF3scr"];
  double pC1scr = parms["pC1scr"];
  double pC2scr = parms["pC2scr"];
  double pC3scr = parms["pC3scr"];
  double pC4scr = parms["pC4scr"];
  //standard treatment allocation
  double F0std = parms["F0std"];
  double F1std = parms["F1std"];
  double F2std = parms["F2std"];
  double F3std = parms["F3std"];
  double C1std = parms["C1std"];
  //disease progression
  double f0f1 = parms["f0f1"];
  double f1f2 = parms["f1f2"];
  double f2f3 = parms["f2f3"];
  double f3c1 = parms["f3c1"];
  double c1c2 = parms["c1c2"];
  double c2c3 = parms["c2c3"];
  double c3c4 = parms["c3c4"];
  double c1bA = parms["c1bA"];
  double c1bB = parms["c1bB"];
  double c1bC = parms["c1bC"];
  double c1bD = parms["c1bD"];
  double c2bA = parms["c2bA"];
  double c2bB = parms["c2bB"];
  double c2bC = parms["c2bC"];
  double c2bD = parms["c2bD"];
  double c3bD = parms["c3bD"];
  double c4bD = parms["c4bD"];
  double dthc1 = parms["deathc1"];
  double dthc2 = parms["deathc2"];
  double dthc3 = parms["deathc3"];
  double dthc4 = parms["deathc4"];
  double dthbA = parms["deathbA"];
  double dthbB = parms["deathbB"];
  double dthbC = parms["deathbC"];
  double dthbD = parms["deathbD"];
  double dthtrn = parms["deathtran"];
  double tranc4 = parms["tranc4"];
  double tranbA = parms["tranbA"];
  double tranbB = parms["tranbB"];
  //natural cause of death
  double natdeath = parms["natdeath"];

  //SEXP stddist = parms["std_dist"];
  //NumericVector std_dist(stddist);


  double beta = parms["beta"];

  double std_cureF0 = parms["std_cureF0"];
  double std_cureF1 = parms["std_cureF1"];
  double std_cureF2 = parms["std_cureF2"];
  double std_cureF3 = parms["std_cureF3"];
  double std_cureC1 = parms["std_cureC1"];

  double new_cureF0 = parms["new_cureF0"];
  double new_cureF1 = parms["new_cureF1"];
  double new_cureF2 = parms["new_cureF2"];
  double new_cureF3 = parms["new_cureF3"];
  double new_cureC1 = parms["new_cureC1"];
  double new_cureC2 = parms["new_cureC2"];
  double new_cureC3 = parms["new_cureC3"];
  double new_cureC4 = parms["new_cureC4"];


  //get the state values
  double S = state["S"];
  double C1 = state["C1"];
  double C2 = state["C2"];
  double C3 = state["C3"];
  double C4 = state["C4"];
  double F0 = state["F0"];
  double F1 = state["F1"];
  double F2 = state["F2"];
  double F3 = state["F3"];

  double D = state["D"];

  double HCC_A = state["HCC_A"];
  double HCC_B = state["HCC_B"];
  double HCC_C = state["HCC_C"];
  double HCC_D = state["HCC_D"];

  double C1std_cured = state["C1std_cured"];
  double C1new_cured = state["C1new_cured"];
  double C2new_cured = state["C2new_cured"];
  double C3new_cured = state["C3new_cured"];
  double C4new_cured = state["C4new_cured"];

  //double newdeath = state["newdeath"];
  double dthC14 = state["dthC14"];
  double dthHCC = state["dthHCC"];

  double pop;
  pop = popfn(time, K, P0, r);

  double scr = 0;
  if((time >= new_start) && (time < new_start+scr_yr))
    scr = scr_cov;
  else
    scr = 0;

  double F0scr = pF0scr*scr;
  double F1scr = pF1scr*scr;
  double F2scr = pF2scr*scr;
  double F3scr = pF3scr*scr;
  double C1scr = pC1scr*scr;
  double C2scr = pC2scr*scr;
  double C3scr = pC3scr*scr;
  double C4scr = pC4scr*scr;

  double treat_std;
  if((new_start>time) && (time >= standard_start))
    treat_std = (1000+(time-standard_start)*200);
  else
    treat_std = 0;

  double F0new, F1new, F2new, F3new;
  if((time < new_start+scr_yr) && (time >= new_start))
  {
    F0new = sens*F0scr;
    F1new = sens*F1scr;
    F2new = sens*F2scr;
    F3new = sens*F3scr;
  }
  else
  {
    F0new = 0;
    F1new = 0;
    F2new = 0;
    F3new = 0;
  }

  double C1new, C2new, C3new, C4new;
  if((new_start+scr_yr>time) && (time >= new_start))
  {
    C1new = sens*C1scr+nscr*C1;
    C2new = sens*C2scr+nscr*C2;
    C3new = sens*C3scr+nscr*C3;
    C4new = sens*C4scr+nscr*C4;
  }

  else if(time >= new_start)
  {
    C1new = nscr*C1;
    C2new = nscr*C2;
    C3new = nscr*C3;
    C4new = nscr*C4;
  }
  else
  {
    C1new = 0;
    C2new = 0;
    C3new = 0;
    C4new = 0;
  }

  double treat_new = F0new+F1new+F2new+F3new+C1new+C2new+C3new+C4new;
  double infect = (F0+F1+F2+F3+C1+C2+C3+C4+HCC_A+HCC_B+HCC_C+HCC_D);
  double lam0 = beta*infect/pop;

  double dS = flowin*pop-lam0*S;

  double dF0;
  if(F0>=0)
    dF0 = -f0f1*F0+lam0*S-(F0std*treat_std*std_cureF0)-(F0new*new_cureF0)-natdeath*F0;
  else dF0 = lam0*S;

  double dF1;
  if(F1>=0)
    dF1 = f0f1*F0 -f1f2*F1-(F1std*treat_std*std_cureF1)-(F1new*new_cureF1)-natdeath*F1;
  else dF1 = 0;
  double dF2;
  if(F2>=0)
    dF2 = f1f2*F2 -f2f3*F2 -(F2std*treat_std*std_cureF2)-(F2new*new_cureF2)-natdeath*F2;
  else dF2 = 0;
  double dF3;
  if(F3>=0)
    dF3 = f2f3*F2 -f3c1*F3 -(F3std*treat_std*std_cureF3)-(F3new*new_cureF3)-natdeath*F3;
  else dF3 = 0;
  double dC1;
  if(C1>=0)
    dC1 = f3c1*F3 -dthc1*C1 -c1c2*C1 -(C1std*treat_std*std_cureC1)-(C1new*new_cureC1) -(c1bA + c1bB + c1bC + c1bD)*C1 -natdeath*C1;
  else dC1 = 0;
  double dC2;
  if(C2>=0)
    dC2 = c1c2*C1 -dthc2*C2 -c2c3*C2 -(C2new*new_cureC2) -(c2bA + c2bB + c2bC + c2bD)*C2 -natdeath*C2;
  else dC2 = 0;
  double dC3;
  if(C3>=0)
    dC3 = c2c3*C2 -dthc3*C3 -c3c4*C3 -(C3new*new_cureC3) -c3bD*C3 -natdeath*C3;
  else dC3 = 0;
  double dC4;
  if(C4>=0)
    dC4 = c3c4*C3 -dthc4*C4 -(C4new*new_cureC4) - c4bD*C4 -natdeath*C4;
  else dC4 = 0;
  double dHCC_A;
  dHCC_A = c1bA*(C1+C1std_cured+C1new_cured) + c2bA*(C2+C2new_cured) -dthbA*HCC_A -tranbA*HCC_A -natdeath*HCC_A;
  double dHCC_B;
  dHCC_B = c1bB*(C1+C1std_cured+C1new_cured) + c2bB*(C2+C2new_cured) -dthbB*HCC_B -tranbB*HCC_B -natdeath*HCC_B;
  double dHCC_C;
  dHCC_C = c1bC*(C1+C1std_cured+C1new_cured) + c2bC*(C2+C2new_cured) -dthbC*HCC_C -natdeath*HCC_C;
  double dHCC_D;
  dHCC_D = c1bD*(C1+C1std_cured+C1new_cured) + c2bD*(C2+C2new_cured) + c3bD*(C3+C3new_cured) + c4bD*(C4+C4new_cured) -dthbD*HCC_D -natdeath*HCC_D;
  double dD;
  dD = dthc1*C1 + dthc2*C2 + dthc3*C3 + dthc4*C4 + dthbA*HCC_A + dthbB*HCC_B + dthbC*HCC_C + dthbD*HCC_D;
  double ddthC14;
  ddthC14 = dthc1*C1 + dthc2*C2 + dthc3*C3 + dthc4*C4;
  double ddthHCC;
  ddthHCC = dthbA*HCC_A + dthbB*HCC_B + dthbC*HCC_C + dthbD*HCC_D;
  double dC1std_cured;
  dC1std_cured = (C1std*treat_std*std_cureC1)-natdeath*C1std_cured-(c1bA+c1bB+c1bC+c1bD)*C1std_cured;
  double dC1new_cured;
  dC1new_cured = (C1new*new_cureC1)-natdeath*(C1new_cured)-(c1bA+c1bB+c1bC+c1bD)*C1new_cured;
  double dC2new_cured;
  dC2new_cured = (C2new*new_cureC2)-natdeath*(C2new_cured)-(c2bA+c2bB+c2bC+c2bD)*C2new_cured;
  double dC3new_cured;
  dC3new_cured = (C3new*new_cureC3)-natdeath*(C3new_cured)-c3bD*C3new_cured;
  double dC4new_cured;
  dC4new_cured = (C4new*new_cureC4)-natdeath*(C4new_cured)-c4bD*C4new_cured;

  double prev = 100*(infect/pop);
  double total_infection = F0+F1+F2+F3+C1+C2+C3+C4+HCC_A+HCC_B+HCC_C+HCC_D;
  double total_HCC = HCC_A+HCC_B+HCC_C+HCC_D;
  double total_HCV = F0+F1+F2+F3+C1+C2+C3+C4;
  //newdeath
  double newdeath;
  if (C1+C2+C3+total_HCC > 0)
    newdeath = dthc1*C1 + dthc2*C2 + dthc3*C3 + dthc4*C4 + dthbA*HCC_A + dthbB*HCC_B + dthbC*HCC_C + dthbD*HCC_D;
  else newdeath = 0;

  double incHCC = c1bA*(C1+C1std_cured+C1new_cured)+c2bA*(C2+C2new_cured)+c1bB*(C1+C1std_cured+C1new_cured)+
    c2bB*(C2+C2new_cured)+c1bC*(C1+C1std_cured+C1new_cured)+c2bC*(C2+C2new_cured)+c1bD*(C1+C1std_cured+C1new_cured)+
    c2bD*(C2+C2new_cured)+c3bD*(C3+C3new_cured)+c4bD*(C4+C4new_cured);

  double new_tranLiv = tranc4*((C4-C4new*new_cureC4)+C4new*(1-new_cureC4))+tranbA*HCC_A+tranbB*HCC_B;

  double lamda = beta*infect/pop;
  double newF03 = lamda*S+f0f1*F0+f1f2*F2+f2f3*F2;
  double newC12 = f3c1*F3+c1c2*C1;
  double newC34 = c2c3*C2+c3c4*C3;

  double fibrosis = F1+F2+F3;
  double compensate = C1+C2;
  double decompensate = C3+C4;

  NumericVector compartments(21);
  compartments[0] = dS;
  compartments[1] = dF0;
  compartments[2] = dF1;
  compartments[3] = dF2;
  compartments[4] = dF3;
  compartments[5] = dC1;
  compartments[6] = dC2;
  compartments[7] = dC3;
  compartments[8] = dC4;
  compartments[9] = dHCC_A;
  compartments[10] = dHCC_B;
  compartments[11] = dHCC_C;
  compartments[12] = dHCC_D;
  compartments[13] = dD;
  compartments[14] = ddthC14;
  compartments[15] = ddthHCC;
  compartments[16] = dC1std_cured;
  compartments[17] = dC1new_cured;
  compartments[18] = dC2new_cured;
  compartments[19] = dC3new_cured;
  compartments[20] = dC4new_cured;

  List outlist(15);
  outlist[0] = compartments;  //this will be passed into the ode solver.
  outlist["prevalence"] = prev;
  outlist["population"] = pop;
  outlist["total_infection"] = total_infection;
  outlist["total_HCC"] = total_HCC;
  outlist["total_HCV"] = total_HCV;
  outlist["newdeath"] = newdeath;
  outlist["incidenceHCC"] = incHCC;
  outlist["new_tranLiv"] = new_tranLiv;
  outlist["treat_standard"] = treat_std;
  outlist["treat_new"] = treat_new;
  outlist["screen"] = scr;
  outlist["fibrosis"] = fibrosis;
  outlist["compensate"] = compensate;
  outlist["decompensate"] = decompensate;

  return outlist;


}


/*** R

*/
