parms_base <- 
  list(
    P0= 61623143,       #popstat(YEAR=1999)
    K= 68508515,        #Maximum population (carrying capacity)
    r = 0.16,        #Population growth rate (logistic growth curve)
    flowin = 1.15*(10^-8),
    caset0 =  61623143*0.03,      #0.03*P0,
    standard_start = 2004,
    new_start = 2019,
    nscr = 0.005,
    scr_yr = 0,
    scr_cov = 0,      #0.9/scr_yr,
    sens = 0.985,
    pF0scr = 0.1439,
    pF1scr = 0.2969,
    pF2scr=0.1098,
    pF3scr=0.1466,
    pC1scr=0.1998,
    pC2scr=0.0819,
    pC3scr=0.0096,
    pC4scr=0.0011,
    
    #Natural rate of death
    natdeath=0.04, #Unrelated to cirrhosis and hepatitis C
    
    beta= 0.02,              #Transmission coefficient
    
    #standard treatment allocation
    F0std = 0.05,
    F1std = 0.05,
    F2std = 0.3,
    F3std = 0.3,
    C1std = 0.3,
    
    std_cureF0=0.7,
    std_cureF1=0.7,
    std_cureF2=0.7,
    std_cureF3=0.7,
    std_cureC1=0.7,
    new_cureF0=0,
    new_cureF1=0,
    new_cureF2=0,
    new_cureF3=0,
    new_cureC1=0,
    new_cureC2=0,
    new_cureC3=0,
    new_cureC4=0,
    
    #Progression of fibrosis
    f0f1= 0.117,       #Fibrosis stage F0 to F1
    f1f2= 0.085,       #Fibrosis stage F1 to F2
    f2f3= 0.12,        #Fibrosis stage F2 to F3
    f3c1= 0.116,       #Fibrosis stage F3 to C1
    
    #Progression of cirrhosis
    c1c2=0.044,       #Fibrosis stage C1 to C2
    c2c3=0.044,       #Fibrosis stage C2 to C3
    c3c4=0.076,       #Fibrosis stage C3 to C4
    
    #Incidence of developing HCC
    c1bA= 0.0068,      #Fibrosis stage C1 to bA
    c1bB= 0.0099,      #Fibrosis stage C1 to bB
    c1bC= 0.0029,      #Fibrosis stage C1 to bC
    c1bD= 0.0068,      #Fibrosis stage C1 to bD
    
    c2bA= 0.0068,      #Fibrosis stage C2 to bA
    c2bB= 0.0099,      #Fibrosis stage C2 to bB
    c2bC= 0.0029,      #Fibrosis stage C2 to bC
    c2bD= 0.0068,      #Fibrosis stage C2 to bD
    
    c3bD= 0.0664,      #Fibrosis stage C3 to bD
    c4bD= 0.0664,      #Fibrosis stage C4 to bD
    
    #Death rate from cirrhosis and HCC
    deathc1=0.01,         #Death rate for Cirrhosis Child-Pugh class A
    deathc2=0.01,         #Death rate for Cirrhosis Child-Pugh class B
    deathc3=0.2,          #Death rate for Cirrhosis Child-Pugh class C
    deathc4=0.57,         #Death rate for Cirrhosis Child-Pugh class D
    
    deathbA=1/(36/12),    #Death rate for HCC_BCLC_A
    deathbB=1/(16/12),    #Death rate for HCC_BCLC_B
    deathbC=1/(6/12),     #Death rate for HCC_BCLC_C
    deathbD=1/(3/12),     #Death rate for HCC_BCLC_D
    
    deathtran=1/(240/12),
    
    #Transplantation
    tranc4=0.0015, #Transplantation rate in cirrhosis stage C4
    tranbA=0.0015, #Transplantation rate in HCC_BCLC_A
    tranbB=0.0015 #Transplantation rate in HCC_BCLC_B
    
    
    
  )