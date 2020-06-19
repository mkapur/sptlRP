#SS-V3.24Z
#_data_and_control_files: dat_7Fishery.ss // control_7Fishery.ss
#_SS-V3.24S-safe;_07/24/2013;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_10.1
1  #_N_Growth_Patterns
1 #_N_Morphs_Within_GrowthPattern 
#_Cond 1 #_Morph_between/within_stdev_ratio (no read if N_morphs=1)
#_Cond  1 #vector_Morphdist_(-1_in_first_val_gives_normal_approx)
#
1 #  N recruitment designs goes here if N_GP*nseas*area>1
0 #  placeholder for recruitment interaction request
#GP seas area for each recruitment assignment
1 1 1 
#
2 # N_movement_definitions goes here if N_areas > 1
0 # first age that moves (real age at begin of season, not integer) also cond on do_migration>0
#_Cond 1 1 1 2 4 10 # example move definition for seas=1, morph=1, source=1 dest=2, age1=4, age2=10
1 1 1 1 0 7  
1 1 1 2 0 7

#
0 #_Nblock_Patterns
#_Cond 0 #_blocks_per_pattern 
# begin and end years of blocks
#
0.5 #_fracfemale 
0 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate
#4 #_reference age for Lorenzen M; read 1P per morph
1 # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_speciific_K; 4=not implemented
1 #_Growth_Age_for_L1
999 #_Growth_Age_for_L2 (999 to use as Linf)
0.0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
1 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
3 #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity by GP; 4=read age-fecundity by GP; 5=read fec and wt from wtatage.ss; 6=read length-maturity by GP
#_Age_Maturity by growth pattern for females
#0	1	2	3	4	5	6	7	8	9	10	11	12	13	14	15	
0	0	0.1	0.25	0.4	0.5	1	1	1	1	1	1	1	1	1	1	 # hake
2 #_First_Mature_Age
1 #_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 #_hermaphroditism option:  0=none; 1=age-specific fxn
1 #_parameter_offset_approach (1=none, 2= M, G, CV_G as offset from female-GP1, 3=like SS2 V1.x)
1 #_env/block/dev_adjust_method (1=standard; 2=logistic transform keeps in base parm bounds; 3=standard w/ no bound check)
#
#_growth_parms
# Lo     Hi     Init    Prior   Prior   Prior   Param   Env     Use     Dev     Dev     Dev     Block   block
# bnd    bnd    value   mean    type    SD      phase   var     dev     minyr   maxyr   SD      design  switch
### Mortality
0.05     0.4    0.2     0.2 	-1     	99     	-5      0       0       0       0       0       0       0       # M
### Growth parameters 
2       16      12      5       -1      99      -5      0       0       0       0       0       0       0       # A0
45      70      53.4    50      -1      99      -3      0       0       0       0       0       0       0       # Linf
0.2     0.4     0.25    0.3     -1      99      -3      0       0       0       0       0       0       0       # VBK
0.03    0.16    0.1     0.1     -1      99      -5      0       0       0       0       0       0       0       # CV of len@age 0
0.03    0.16    0.1     0.1     -1      99      -5      0       0       0       0       0       0       0       # CV of len@age inf
# W-L, maturity and fecundity parameters
# Female placeholders
-3      3       7.0E-06 7.0E-06 -1      99      -50     0       0       0       0       0       0       0       # F W-L slope
-3      3       2.9624  2.9624  -1      99      -50     0       0       0       0       0       0       0       # F W-L exponent
# Maturity 
-3      43      36.89   36.89   -1      99      -50     0       0       0       0       0       0       0       # L at 50% maturity
-3      3       -0.48   -0.48   -1      99      -50     0       0       0       0       0       0       0       # F Logistic maturity slope
# No fecundity relationship
-3      3       1.0     1.0     -1      99      -50     0       0       0       0       0       0       0       # F Eggs/gm intercept
-3      3       0.0     0.0     -1      99      -50     0       0       0       0       0       0       0       # F Eggs/gm slope
# Unused recruitment interactions
0       2       0       1       -1      99      -50     0       0       0       0       0       0       0       # RecrDist_GP_1
0       2       0       1       -1      99      -50     0       0       0       0       0       0       0       # RecrDist_Area_1
0       2       0       1       -1      99      -50     0       0       0       0       0       0       0       # RecrDist_Area_2
0       2       0       1       -1      99      -50     0       0       0       0       0       0       0       # RecrDist_Seas_1
0 		4		1		1 		-1 		99		-50		0 		0 		0		0       0       0       0       # CohortGrowDev
# Movement 
-4 		4		0		0 		-1 		99		-50		0 		0 		0		0       0       0       0       # MoveParm_A_GP_1from_1to_1
-8 		4		-6.6846	0 		-1 		99		-50		0 		0 		0		0       0       0       0       # MoveParm_B_GP_1from_1to_1
-4 		4		-1.3863	0 		-1 		99		-50		0 		0 		0		0       0       0       0       # MoveParm_A_GP_1from_1to_2
-4 		4		0.22214	0 		-1 		99		-50		0 		0 		0		0       0       0       0       # MoveParm_B_GP_1from_1to_2
 
#
#_Cond 0  #custom_MG-env_setup (0/1)
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no MG-environ parameters
#
#_Cond 0  #custom_MG-block_setup (0/1)
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no MG-block parameters
#_Cond No MG parm trends 
#
#_seasonal_effects_on_biology_parms
 0 0 0 0 0 0 0 0 0 0 #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters
#
#_Cond -4 #_MGparm_Dev_Phase
#
#_Spawner-Recruitment
6 #_SR_function: 2=Ricker; 3=std_B-H; 4=SCAA; 5=Hockey; 6=B-H_flattop; 7=survival_3Parm
#_LO HI INIT PRIOR PR_type SD PHASE
13      17      15      15      -1      99      -1      # Ln(R0)
0.2     1       0.86    0.777   -1      99      -4      # Steepness with Myers' prior
0.8     1.6     1.4     1.1     -1      99      -6      # Sigma-R
-5      5       0       0       -1      99      -50     # Env link coefficient
-5      5       0       0       -1      99      -50     # Initial equilibrium recruitment offset
 0      2       0       1       -1      99      -50     # Autocorrelation in rec devs
0 #_SR_env_link
0 #_SR_env_target_0=none;1=devs;_2=R0;_3=steepness
1 #do_recdev:  0=none; 1=devvector; 2=simple deviations
1980 # first year of main recr_devs; early devs can preceed this era
2016 # last year of main recr_devs; forecast devs start in following year
1 #_recdev phase 
1 # (0/1) to read 13 advanced options
 -10 #_recdev_early_start (0=none; neg value makes relative to recdev_start)
 3 #_recdev_early_phase
 0 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
 1 #_lambda for Fcast_recr_like occurring before endyr+1
 1900 #_last_early_yr_nobias_adj_in_MPD
 1901 #_first_yr_fullbias_adj_in_MPD
 2016 #_last_yr_fullbias_adj_in_MPD
 2017 #_first_recent_yr_nobias_adj_in_MPD
 1 #_max_bias_adj_in_MPD (-1 to override ramp and set biasadj=1.0 for all estimated recdevs)
 0 #_period of cycles in recruitment (N parms read below)
 -8 #min rec_dev
 8 #max rec_dev
 0 #_read_recdevs
#_end of advanced SR options
#
#_placeholder for full parameter lines for recruitment cycles
# read specified recr devs
#_Yr Input_value
#
# all recruitment deviations
#DisplayOnly -1.67109 # Early_InitAge_10
#DisplayOnly -1.73555 # Early_InitAge_9
#DisplayOnly -1.79689 # Early_InitAge_8
#DisplayOnly -1.85417 # Early_InitAge_7
#DisplayOnly -1.90632 # Early_InitAge_6
#DisplayOnly -1.9521 # Early_InitAge_5
#DisplayOnly -1.99007 # Early_InitAge_4
#DisplayOnly -2.01863 # Early_InitAge_3
#DisplayOnly -2.03593 # Early_InitAge_2
#DisplayOnly -2.04062 # Early_InitAge_1
#DisplayOnly -2.03341 # Early_RecrDev_1952
#DisplayOnly -1.77822 # Main_RecrDev_1953
#DisplayOnly -1.77538 # Main_RecrDev_1954
#DisplayOnly -1.76575 # Main_RecrDev_1955
#DisplayOnly -1.78639 # Main_RecrDev_1956
#DisplayOnly -1.68952 # Main_RecrDev_1957
#DisplayOnly -1.52948 # Main_RecrDev_1958
#DisplayOnly -1.38652 # Main_RecrDev_1959
#DisplayOnly -1.13667 # Main_RecrDev_1960
#DisplayOnly -1.23018 # Main_RecrDev_1961
#DisplayOnly -0.819563 # Main_RecrDev_1962
#DisplayOnly -0.633763 # Main_RecrDev_1963
#DisplayOnly -0.358896 # Main_RecrDev_1964
#DisplayOnly -0.0718126 # Main_RecrDev_1965
#DisplayOnly 0.0443595 # Main_RecrDev_1966
#DisplayOnly 0.183674 # Main_RecrDev_1967
#DisplayOnly 0.296532 # Main_RecrDev_1968
#DisplayOnly 0.328039 # Main_RecrDev_1969
#DisplayOnly 0.359676 # Main_RecrDev_1970
#DisplayOnly 0.36159 # Main_RecrDev_1971
#DisplayOnly 0.363159 # Main_RecrDev_1972
#DisplayOnly 0.366797 # Main_RecrDev_1973
#DisplayOnly 0.370192 # Main_RecrDev_1974
#DisplayOnly 0.371833 # Main_RecrDev_1975
#DisplayOnly 0.379332 # Main_RecrDev_1976
#DisplayOnly 0.383489 # Main_RecrDev_1977
#DisplayOnly 0.382833 # Main_RecrDev_1978
#DisplayOnly 0.3882 # Main_RecrDev_1979
#DisplayOnly 0.389464 # Main_RecrDev_1980
#DisplayOnly 0.38946 # Main_RecrDev_1981
#DisplayOnly 0.389485 # Main_RecrDev_1982
#DisplayOnly 0.389994 # Main_RecrDev_1983
#DisplayOnly 0.390047 # Main_RecrDev_1984
#DisplayOnly 0.390114 # Main_RecrDev_1985
#DisplayOnly 0.389309 # Main_RecrDev_1986
#DisplayOnly 0.386479 # Main_RecrDev_1987
#DisplayOnly 0.385536 # Main_RecrDev_1988
#DisplayOnly 0.370171 # Main_RecrDev_1989
#DisplayOnly 0.258614 # Main_RecrDev_1990
#DisplayOnly 0.308626 # Main_RecrDev_1991
#DisplayOnly 1.57096 # Main_RecrDev_1992
#DisplayOnly 0.672207 # Main_RecrDev_1993
#DisplayOnly 0.825983 # Main_RecrDev_1994
#DisplayOnly 0.532177 # Main_RecrDev_1995
#DisplayOnly 0.0876765 # Main_RecrDev_1996
#DisplayOnly -0.359244 # Main_RecrDev_1997
#DisplayOnly -0.667951 # Main_RecrDev_1998
#DisplayOnly -0.752207 # Main_RecrDev_1999
#DisplayOnly -0.400371 # Main_RecrDev_2000
#DisplayOnly 0.256594 # Main_RecrDev_2001
#DisplayOnly 0.268668 # Main_RecrDev_2002
#DisplayOnly 0.352735 # Main_RecrDev_2003
#DisplayOnly -0.278545 # Main_RecrDev_2004
#DisplayOnly 0.17353 # Main_RecrDev_2005
#DisplayOnly -0.130686 # Main_RecrDev_2006
#DisplayOnly -0.340431 # Main_RecrDev_2007
#DisplayOnly -0.275591 # Main_RecrDev_2008
#DisplayOnly 0.0508671 # Main_RecrDev_2009
#DisplayOnly 2.74652 # Main_RecrDev_2010
#DisplayOnly 2.61225 # Main_RecrDev_2011
#
#Fishing Mortality info 
0.1 # F ballpark for annual F (=Z-M) for specified year
-1999 # F ballpark year (neg value to disable)
2 # F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)
4 # max F or harvest rate, depends on F_Method
# no additional F input needed for Fmethod 1
# if Fmethod=2; read overall start F value; overall phase; N detailed inputs to read
# if Fmethod=3; read N iterations for tuning for Fmethod 3
 0.1 1 0 # overall start F value; overall phase; N detailed inputs to read
#Fleet Year Seas F_value se phase (for detailed setup of F_Method=2)

#
#_initial_F_parms
#_LO HI INIT PRIOR PR_type SD PHASE
 0 4.9 0 0 0 10000 -1 # InitF_1FisheryArea2
 
#_Q_setup
 # Q_type options:  
# A=do power: 0=skip, survey is prop. to abundance, 1= add par for non-linearity
# B=env. link: 0=skip, 1= add par for env. effect on Q
# C=extra SD: 0=skip, 1= add par. for additive constant to input SE (in ln space)
# D=type: <0=mirror lower abs(#) fleet, 0=no par Q is median unbiased, 1=no par Q is mean unbiased, 2=estimate par for ln(Q)
#          3=ln(Q) + set of devs about ln(Q) for all years. 4=ln(Q) + set of devs about Q for indexyr-1
#_for_env-var:_enter_index_of_the_env-var_to_be_linked
#_Den-dep  env-var  extra_se  Q_type
 0 0 0 0 # 1 FisheryArea2
 0 0 0 2 # 2 SurveyArea1
 0 0 0 2 # 3 SurveyArea2
#
#_Cond 0 #_If q has random component, then 0=read one parm for each fleet with random q; 1=read a parm for each year of index
#_Q_parms(if_any);Qunits_are_ln(q)
-3 3 0 0 -1 99 -4 # LnQ_base_SurveyArea1
-3 3 0 0 -1 99 -4 # LnQ_base_SurveyArea2
#
#_size_selex_types
#discard_options:_0=none;_1=define_retention;_2=retention&mortality;_3=all_discarded_dead
#_Pattern Discard Male Special
  1 0 0 0 # 1 FisheryArea2
  0 0 0 0 # 2 SurveyArea1
  0 0 0 0 # 3 SurveyArea2
#
#_age_selex_types
#_Pattern ___ Male Special
 11 0 0 0 # 1 FisheryArea2
 11 0 0 0 # 2 SurveyArea1
 11 0 0 0 # 3 SurveyArea2
#_LO HI INIT PRIOR PR_type SD PHASE env-var use_dev dev_minyr dev_maxyr dev_stddev Block Block_Fxn
#  4.5 70 25 0 -1 0 -4 0 0 0 0 0 0 0 # SizeSel_1P_1_F1FisheryArea1(peak size) ; age 2-3 (18-27) movement rate to area 2 is low <0.2 (20-42)
#-20 15 -2 0 -1 0 -4 0 0 0 0 0 0 0 # SizeSel_1P_2_F1FisheryArea1 (top logistic) # from -ln(((0.99*100-Peak-1)/(100-Peak-1))-1); Peak+1+(0.99*100-Peak-1)/(1+exp(-P2)); for asympotic, using 100 as top difference, parameter at 4.13855 
#-20 15 3 0 -1 0 -4 0 0 0 0 0 0 0 # SizeSel_1P_3_F1FisheryArea1 (ascending limb width - exp) # age 0 fish selectivity = exp(-((0-P1)^2/exp(P3))) 0.01; parameter = ln(-((0-P1)^2 / ln(age0 selectivity))) parameters range from 2.3 to 5 for P1 = 4; round(runif(1, 2.3, 5),digit =1)
#-20 15 4 0 -1 0 -4 0 0 0 0 0 0 0 # SizeSel_1P_4_F1FisheryArea1 (descending limb width - exp) # fish > 160 cm is not selected (6 - 10 year old) parameters range from -2 to 0 ; for asympotic, parameter fixed at 8
#-999 -999 -999 0 -1 0 -4 0 0 0 0 0 0 0 # SizeSel_1P_5_F1FisheryArea1 (initial S - at first age bin)
#-999 -999 -999 0 -1 0 -4 0 0 0 0 0 0 0 # SizeSel_1P_6_F1FisheryArea1 (final S - at last age bin)

4.5 70 30 0 -1 0 -4 0 0 0 0 0 0 0 # SizeSel_1P_1_FisheryArea2 (size at inflection); age 5-6 (39-43) movement rate about 0.5
0 50 8 0 -1 0 -4 0 0 0 0 0 0 0 # SizeSel_1P_2_FisheryArea2 (width for 95%selcetion); # sel = 1./(1.+exp(-ln19*(len-P1)/P2))

#  4.5 99.5 50 0 -1 0 -4 0 0 0 0 0 0 0 # SizeSel_3P_1_F2area2 (peak size) ; source area 2 at age 12 (50-68)
#-20 15 4.13855 0 -1 0 -4 0 0 0 0 0 0 0 # SizeSel_3P_2_F2area2 (top logistic) # from -ln(((0.99*100-Peak-1)/(100-Peak-1))-1); Peak+1+(0.99*100-Peak-1)/(1+exp(-P2)); for asympotic, using 100 as top difference, parameter at 4.13855 
#-20 15 3.7 0 -1 0 -4 0 0 0 0 0 0 0 # SizeSel_3P_3_F2area2 (ascending limb width - exp) # age 0 fish selectivity = exp(-((0-P1)^2/exp(P3))) 0.01; parameter = ln(-((0-P1)^2 / ln(age0 selectivity))) parameters range from 2.3 to 5 for P1 = 4; round(runif(1, 2.3, 5),digit =1)
#-20 15 8 0 -1 0 -4 0 0 0 0 0 0 0 # SizeSel_3P_4_F2area2 (descending limb width - exp) # fish > 160 cm is not selected (6 - 10 year old) parameters range from -2 to 0 ; for asympotic, parameter fixed at 8
#-999 -999 -999 0 -1 0 -4 0 0 0 0 0 0 0 # SizeSel_3P_5_F2area2 (initial S - at first age bin)
#-999 -999 -999 0 -1 0 -4 0 0 0 0 0 0 0 # SizeSel_3P_6_F2area2 (final S - at last age bin)

#  0 15 3 0 -1 0 -4 0 0 0 0 0 0 0 # AgeSel_1P_1_F1area1 (peak size) ; age 3- 6 for source area 1
#-20 15 4.13855 0 -1 0 -4 0 0 0 0 0 0 0 # AgeSel_1P_2_F1area1 (top logistic) # from -ln(((0.99*30-Peak-1)/(30-Peak-1))-1); Peak+1+(0.99*30-Peak-1)/(1+exp(-P2)); for asympotic, using 30 as top difference, parameter at 4.13855 
#-20 15 3.7 0 -1 0 -4 0 0 0 0 0 0 0 # AgeSel_1P_3_F1area1 (ascending limb width - exp) # age 0 fish selectivity = exp(-((0-P1)^2/exp(P3))) from 0.2 to 0.9; parameter = ln(-((0-P1)^2 / ln(age0 selectivity))) parameters range from 2.3 to 5 for P1 = 4; round(runif(1, 2.3, 5),digit =1)
#-20 15 8 0 -1 0 -4 0 0 0 0 0 0 0 # AgeSel_1P_4_F1area1 (descending limb width - exp) # fish > 160 cm is not selected (6 - 10 year old) parameters range from -2 to 0 ; for asympotic, parameter fixed at 8
#-999 -999 -999 0 -1 0 -4 0 0 0 0 0 0 0 # AgeSel_1P_5_F1area1 (initial S - at first age bin)
#-999 -999 -999 0 -1 0 -4 0 0 0 0 0 0 0 # AgeSel_1P_6_F1area1 (final S - at last age bin)


#  0 15 7 0 -1 0 -4 0 0 0 0 0 0 0 # AgeSel_2P_1_F2area2 (peak size) ; age 6- 9 for source area 2
#-20 15 4.13855 0 -1 0 -4 0 0 0 0 0 0 0 # AgeSel_2P_2_F2area2 (top logistic) # from -ln(((0.99*30-Peak-1)/(30-Peak-1))-1)  for asympotic, using 30 as top difference, parameter at 4.13855 
#-20 15 1.96 0 -1 0 -4 0 0 0 0 0 0 0 # AgeSel_2P_3_F2area2 (ascending limb width - exp) # age 0 fish selectivity = exp(-((0-P1)^2/exp(P3))) 0.001; parameter = ln(-((0-P1)^2 / ln(age0 selectivity))) parameters range from 1.96 to 2.46
#-20 15 8 0 -1 0 -4 0 0 0 0 0 0 0 # AgeSel_2P_4_F2area2 (descending limb width - exp) ; for asympotic, parameter fixed at 8
#-999 -999 -999 0 -1 0 -4 0 0 0 0 0 0 0 # AgeSel_2P_5_F2area2 (initial S - at first age bin)
#-999 -999 -999 0 -1 0 -4 0 0 0 0 0 0 0 # AgeSel_2P_6_F2area2 (final S - at last age bin)

 0 1 0 1 0 25 -99 0 0 0 0 0 0 0 # AgeSel_1P_1_FisheryArea2
 10 20 15 15 0 25 -99 0 0 0 0 0 0 0 # AgeSel_1P_2_FisheryArea2
 0 1 0 1 0 25 -99 0 0 0 0 0 0 0 # AgeSel_2P_1_SurveyArea1
 10 20 15 15 0 25 -99 0 0 0 0 0 0 0 # AgeSel_2P_2_SurveyArea1
 0 1 0 1 0 25 -99 0 0 0 0 0 0 0 # AgeSel_3P_1_SurveyArea2 
 10 20 15 15 0 25 -99 0 0 0 0 0 0 0 # AgeSel_3P_2_SurveyArea2
 
 
#_Cond 0 #_custom_sel-env_setup (0/1) 
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no enviro fxns
#_Cond 0 #_custom_sel-blk_setup (0/1) 
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no block usage
#_Cond No selex parm trends 
#_Cond -4 # placeholder for selparm_Dev_Phase
#_Cond 0 #_env/block/dev_adjust_method (1=standard; 2=logistic trans to keep in base parm bounds; 3=standard w/ no bound check)
#
# Tag loss and Tag reporting parameters go next
0  # TG_custom:  0=no read; 1=read if tags exist
#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters
#
1 #_Variance_adjustments_to_input_values
#_fleet: 1 2 3 
0 0 0 #_add_to_survey_CV
0 0 0 #_add_to_discard_stddev
0 0 0 #_add_to_bodywt_CV
1 1 1 #_mult_by_lencomp_N
1 1 1 #_mult_by_agecomp_N
1 1 1 #_mult_by_size-at-age_N
#
1 #_maxlambdaphase
1 #_sd_offset
#
11 # number of changes to make to default Lambdas (default value is 1.0)
# Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 7=sizeage; 8=catch; 9=init_equ_catch; 
# 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin; 17=F_ballpark
#like_comp fleet/survey  phase  value  sizefreq_method
 1 2 1 1 0
 1 3 1 1 0
 4 1 1 1 0
 4 2 1 1 0
 4 3 1 1 0 
 5 2 1 1 0
 5 3 1 1 0
 8 2 1 0 0
 8 3 1 0 0
 9 1 1 0 0 
 11 1 1 0 0
#
# lambdas (for info only; columns are phases)
#  0 #_CPUE/survey:_1
#  1 #_CPUE/survey:_2
#  1 #_lencomp:_1
#  0 #_lencomp:_2
#  1 #_agecomp:_1
#  0 #_agecomp:_2
#  0 #_init_equ_catch
#  1 #_recruitments
#  0 #_parameter-priors
#  1 #_parameter-dev-vectors
#  1 #_crashPenLambda
#  0 # F_ballpark_lambda
0 # (0/1) read specs for more stddev reporting 
 # 0 1 -1 5 1 5 1 -1 5 # placeholder for selex type, len/age, year, N selex bins, Growth pattern, N growth ages, NatAge_area(-1 for all), NatAge_yr, N Natages
 # placeholder for vector of selex bins to be reported
 # placeholder for vector of growth ages to be reported
 # placeholder for vector of NatAges ages to be reported
999

