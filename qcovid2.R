avoncap_calculate_qcovid = function(avoncap_df) {
  
  v = avoncap_df %>% get_value_sets()
  
  mapperBase = expression(
    b2_82 = admission.on_immunosuppression == v$admission.on_immunosuppression$yes,
    b2_leukolaba = FALSE,
    b2_prednisolone = FALSE, # does not separate out
    b_AF = comorbid.af == v$comorbid.af$yes,
    b_CCF = comorbid.ccf == v$comorbid.ccf$yes,
    b_asthma = comorbid.asthma == v$comorbid.asthma$yes,
    b_bloodcancer = comorbid.no_haemotological_cancer == v$comorbid.no_haemotological_cancer$no,
    b_cerebralpalsy = FALSE,
    b_chd = comorbid.ihd == v$comorbid.ihd$yes | comorbid.previous_mi == v$comorbid.previous_mi$yes,
    b_cirrhosis = comorbid.liver_disease == v$comorbid.liver_disease$`Liver disease with failure`,
    b_congenheart = comorbid.congenital_heart_dx == v$comorbid.congenital_heart_dx$yes,
    b_copd = comorbid.copd == v$comorbid.copd$yes,
    b_dementia = comorbid.dementia == v$comorbid.dementia$yes,
    b_epilepsy = FALSE,
    b_fracture4 = FALSE,
    b_hiv = comorbid.HIV == v$comorbid.HIV$yes | comorbid.AIDS == v$comorbid.AIDS$yes,
    b_ibd = FALSE,
    b_manicschiz = FALSE,
    b_neurorare = FALSE,
    b_parkinsons = FALSE,
    b_pulmhyper = comorbid.pulmonary_hypertension == v$comorbid.pulmonary_hypertension$yes,
    b_pulmrare = FALSE,
    b_pvd = comorbid.periph_vasc_dx == v$comorbid.periph_vasc_dx$yes,
    b_ra_sle = comorbid.connective_tissue_dx == v$comorbid.connective_tissue_dx$yes,
    b_respcancer = FALSE,
    b_scid = comorbid.immunodeficiency == v$comorbid.immunodeficiency$yes,# integer
    b_sicklecelldisease = FALSE,# integer
    b_stroke = comorbid.cva == v$comorbid.cva$yes,# integer
    b_vte = FALSE, # thromboembolism
    
    chemocat = 1, #rep(1,length(admission.BMI)), # integer ?chemotherapy
    ethrisk = case_when(
      # 1 Unknown
      # 2 White
      demog.ethnicity == v$demog.ethnicity$`White British` | demog.ethnicity == v$demog.ethnicity$`White other` ~ 2,
      # 3 Indian (1.26)
      demog.ethnicity == v$demog.ethnicity$Asian ~ 3,
      # 4 Pakistani (1.28)
      # 5 Bangladeshi (1.52)
      # 6 Other Asian (1.01)
      # 7 Caribbean (1.04)
      # 8 Black African (1.07)
      demog.ethnicity == v$demog.ethnicity$Black ~ 8,
      # Chinese (1.62)
      # Other (0.94)
      # default to Unknown (1)
      TRUE ~ 1
    ),# integer ?ethnicity
    homecat = case_when(
      demog.care_home_resident == v$demog.care_home_resident$yes ~ 2,
      TRUE ~ 1
    ),# integer ?carehome
    learncat = case_when(
      comorbid.cognitive_impairment == v$comorbid.cognitive_impairment ~ 2,
      TRUE ~ 1
    ),# integer ? learning diasbility
    p_marrow6 = 
      comorbid.transplant_recipient == v$comorbid.transplant_recipient & 
      comorbid.no_haemotological_cancer == v$comorbid.no_haemotological_cancer$no,
    p_radio6 = FALSE, # integer - radiotherapy in last 6 months
    p_solidtransplant =
      comorbid.transplant_recipient == v$comorbid.transplant_recipient & 
      comorbid.no_haemotological_cancer == v$comorbid.no_haemotological_cancer$yes,
    
    renalcat = case_when(
      comorbid.ckd == v$comorbid.ckd$`Mild (CKD 1-3)` ~ 3,
      comorbid.ckd == v$comorbid.ckd$`Moderate or Severe CKD (CKD 4+)` ~ 4,
      TRUE ~ 1
    ),
    
    town = demog.townsend_score,# double
    
    typeonecat = case_when(
      comorbid.diabetes == v$comorbid.diabetes$`Type 1 - no complications` ~ 2,
      comorbid.diabetes == v$comorbid.diabetes$`Type 1 - complications` ~ 3,
      TRUE ~ 1
    ),
    
    typetwocat = case_when(
      comorbid.diabetes == v$comorbid.diabetes$`Type 2 - no complications` ~ 2,
      comorbid.diabetes == v$comorbid.diabetes$`Type 2 - complications` ~ 3,
      TRUE ~ 1
    )
  )
  
  mapperFull = c(
    expression(
      age = demog.age,
      bmi = admission.BMI# double
    ),
    mapperBase
  )
  
  mapperNormalised = c(
    expression(
      age = NA_real_,
      bmi = NA_real_
    ),
    mapperBase
  )
  
  # integrate townsend scores for deprivation
  # needed for the QCovid2 score
  imd2townsend = readr::read_csv(here::here("reference-data/imd-to-townsend-map.csv")) %>% 
    rename(demog.imd_decile = imd_decile, demog.townsend_score = mean_townsend)
  
  avoncap_df = avoncap_df %>%  
    left_join(imd2townsend, by="demog.imd_decile",.messages=NULL, .headline=NULL) 
  
  out = avoncap_df %>% mutate(
    qcovid2.log_hazard = ifelse(
        demog.gender == v$demog.gender$Female,
        # Female
        .positive_death_female(!!!as.list(mapperFull)),
        # Male
        .positive_death_male(!!!as.list(mapperFull))
    ),
    qcovid2.log_comorbid_hazard = ifelse(
      demog.gender == v$demog.gender$Female,
      # Female
      .positive_death_female(!!!as.list(mapperNormalised)),
      # Male
      .positive_death_male(!!!as.list(mapperNormalised))
    )
  )
  
  out = out %>% mutate(
    qcovid2.hazard_ratio = exp(qcovid2.log_hazard),
    qcovid2.comorbid_hazard_ratio = exp(qcovid2.log_comorbid_hazard)
  )
  
}




.naz = function(x) {
  ifelse(is.na(x),0,x)
}

.na1 = function(x) {
  ifelse(is.na(x),1,x)
}

.na.default = function(x,default) {
  ifelse(is.na(x),default,x)
}

## QCOVID2 death given positive test - unvaccinated female: ----

# QCOVID © Copyright, Oxford University 2021.
# All Rights Reserved. The author, being Professor Julia Hippsley-Cox, has asserted their moral right.

# This model is described in 
# Hippisley-Cox J, Coupland CA, Mehta N, et al. Risk prediction of covid-19 related death and hospital admission in adults after covid-19 vaccination: national prospective cohort study. BMJ 2021;374:n2244. doi:10.1136/bmj.n2244
# Supplementary Figure 9 QCOVID2 Adjusted cause specific hazard ratios (95% CI) for risk of COVID-19
# death in unvaccinated men and women during the second wave in England, mutually adjusted and also
# adjusted for fractional polynomial terms for age and BMI among those with a SARS-CoV-2 positive test.

.positive_death_female = function(
    age, # integer - age
    b2_82, # integer - immunosuppressant medication
    b2_leukolaba, #integer - Leukotriene or long acting beta agonist
    b2_prednisolone, # integer - Oral steroids
    b_AF, # integer - Hx of AF
    b_CCF,# integer - Hx of CCF
    b_asthma,# integer - Hx of asthma
    b_bloodcancer,# integer
    b_cerebralpalsy,# integer
    b_chd,# integer
    b_cirrhosis,# integer
    b_congenheart,# integer
    b_copd,# integer
    b_dementia,# integer
    b_epilepsy,# integer
    b_fracture4,# integer - Hx osteoporotic fracture
    b_hiv,# integer
    b_ibd,# integer
    b_manicschiz,# integer
    b_neurorare,# integer - Hs Rare neurological conditions ?
    b_parkinsons,# integer
    b_pulmhyper,# integer
    b_pulmrare,# integer - Hx Rare lung conditions ?
    b_pvd,# integer
    b_ra_sle,# integer
    b_respcancer,# integer
    b_scid,# integer
    b_sicklecelldisease,# integer
    b_stroke,# integer
    b_vte,# integer
    bmi,# double
    chemocat,# integer ?chemotherapy
    ethrisk,# integer ?ethnicity
    homecat,# integer ?carehome
    learncat,# integer ? learning diasbility
    p_marrow6,# integer - bone marrow transplant in last 6 months
    p_radio6,# integer - radiotherapy in last 6 months
    p_solidtransplant,# integer - solid organ transplant 
    renalcat,# integer
    town,# double
    typeonecat,# integer
    typetwocat# integer
  )
{
  
  Ichemocat = c(
    0, # No Chemotherapy
    0.7739043918776356001387740, # Chemotherapy grade A (2.17)
    1.2612665876398869713170825, # Chemotherapy grade B (3.53)
    1.6840675045171940027444180 #  Chemotherapy grade C (5.39)
  );
  Iethrisk = c(
    0, # Unknown
    0, # White
    0.2347751626618447551297919, # Indian (1.26)
    0.2461504823576133316187509, # Pakistani (1.28)
    0.4167628522482510478042173, # Bangladeshi (1.52)
    0.0126183151504324301411808, # Other Asian (1.01)
    0.0429009258393597825431698, # Caribbean (1.04)
    0.0679146695312937004329612, # Black African (1.07)
    0.4798760717256936314534244, # Chinese (1.62)
    -0.0613430916753555793841102 # Other (0.94)
  );
  Ihomecat = c(
    0, # Lives at home
    -0.0431722591304066483086643, # Care home (0.96)
    0.9833946874402230697853611 # Homeless (2.67)
  );
  Ilearncat = c(
    0, # No learning disability
    0.1064209644113387059904952, # Learning Disability (1.11)
    2.5358524518536733616258516 # Down's syndrome (12.63)
  );
  Irenalcat = c(
    0, # No CKD
    0,
    0.1391774831508708343275060, # CKD stage 3 (1.15)
    0.5986297943095169049598780, # CKD stage 4 (1.82)
    0.5033355428237018536208325, # CKD stage 5 (1.65)
    1.1105597193725642579664736, # CKD stage 5 + dialysis (3.04)
    1.4190696770277835270235300 # CKD stage 5 + transplant (4.13)
  );
  Itypeonecat = c(
    0, # no Type 1
    0.8818144571873287596019964, # Type 1 HBA1C < 59% (2.42)
    1.5322430398330351142277550 # Type 1 HBA1C >= 59% (4.63)
  );
  Itypetwocat = c(
    0,
    0.2086233438989026078846933, # Type 2 HBA1C >= 59% (1.23)
    0.4197276534832414807141276 # Type 2 HBA1C >= 59% (1.52)
  );
  
  dage = age;
  dage = dage/10;
  age_2 = dage^3*log(dage);
  age_1 = dage^3;
  dbmi = ifelse(is.na(age), NA, .na.default(bmi, 27.30));
  dbmi = dbmi/10;
  bmi_2 = log(dbmi)^2;
  bmi_1 = log(dbmi);
  
  mage_1 = 111.719131469726562;
  mage_2 = 175.622024536132812;
  mbmi_1 = 0.980837404727936;
  mbmi_2 = 0.962042033672333;
  mtown = 0.272704988718033;
  
  age_1 = age_1 - mage_1;
  age_2 = age_2 - mage_2;
  bmi_1 = bmi_1 - mbmi_1;
  bmi_2 = bmi_2 - mbmi_2;
  town = .na.default(town,-0.17) - mtown;
  
  a = rep(0,length(chemocat));
  a = a + .naz(Ichemocat[.na1(chemocat)]);
  a = a + .naz(Iethrisk[.na1(ethrisk)]);
  a = a + .naz(Ihomecat[.na1(homecat)]);
  a = a + .naz(Ilearncat[.na1(learncat)]);
  a = a + .naz(Irenalcat[.na1(renalcat)]);
  a = a + .naz(Itypeonecat[.na1(typeonecat)]);
  a = a + .naz(Itypetwocat[.na1(typetwocat)]);
  
  cage_1 = 0.0788990689213801693613348;
  cage_2 = -0.0305211927153145011482049;
  cbmi_1 = -10.4862948374280477992215310;
  cbmi_2 = 5.8034416018720360597171748;
  ctown = 0.0120903926083225157772638;
  
  a = a + .naz(age_1 * cage_1);
  a = a + .naz(age_2 * cage_2);
  a = a + .naz(bmi_1 * cbmi_1);
  a = a + .naz(bmi_2 * cbmi_2);
  a = a + .naz(town * ctown);
  
  cb2_82 = 0.4086911314144368034817489;
  cb2_leukolaba = 0.2459549851861309033296266;
  cb2_prednisolone = 0.4935190578667876248886159;
  cb_AF = 0.0604146401119656228928534;
  cb_CCF = 0.2079718699888926658481125;
  cb_asthma = -0.0153562917324365916238449;
  cb_bloodcancer = 0.2535383490589838695328240;
  cb_cerebralpalsy = 0.6351401598122743408225688;
  cb_chd = 0.1400085779157225762681094;
  cb_cirrhosis = 0.8949828116415821099849381;
  cb_congenheart = 0.2199607627543534449365836;
  cb_copd = 0.2711118862351662439635902;
  cb_dementia = 0.2113854981573877744871481;
  cb_epilepsy = 0.4116849423247115336010893;
  cb_fracture4 = 0.1334874069333920609814470;
  cb_hiv = 0.7933301170987097084008610;
  cb_ibd = 0.0658367860196869963962385;
  cb_manicschiz = 0.5494909628981651250612117;
  cb_neurorare = 0.6726011845559383139914189;
  cb_parkinsons = 0.3102885424377260226158626;
  cb_pulmhyper = 0.0754724261844043342595256;
  cb_pulmrare = 0.1566848356421702559426734;
  cb_pvd = 0.1934427716204898117791089;
  cb_ra_sle = 0.2158858766203974599573456;
  cb_respcancer = 0.2204970451882501958351668;
  cb_scid = 0.9270326589251016713433273;
  cb_sicklecelldisease = 1.3489217890778015807029533;
  cb_stroke = 0.0931921150549636645443741;
  cb_vte = 0.2002615162807588888860266;
  cp_marrow6 = 1.1996239395564705088048640;
  cp_radio6 = 0.7221364731255899283013377;
  cp_solidtransplant = 1.3064986170312842261864716;
  
  a = a + .naz(b2_82 * cb2_82);
  a = a + .naz(b2_leukolaba * cb2_leukolaba);
  a = a + .naz(b2_prednisolone * cb2_prednisolone);
  a = a + .naz(b_AF * cb_AF);
  a = a + .naz(b_CCF * cb_CCF);
  a = a + .naz(b_asthma * cb_asthma);
  a = a + .naz(b_bloodcancer * cb_bloodcancer);
  a = a + .naz(b_cerebralpalsy * cb_cerebralpalsy);
  a = a + .naz(b_chd * cb_chd);
  a = a + .naz(b_cirrhosis * cb_cirrhosis);
  a = a + .naz(b_congenheart * cb_congenheart);
  a = a + .naz(b_copd * cb_copd);
  a = a + .naz(b_dementia * cb_dementia);
  a = a + .naz(b_epilepsy * cb_epilepsy);
  a = a + .naz(b_fracture4 * cb_fracture4);
  a = a + .naz(b_hiv * cb_hiv);
  a = a + .naz(b_ibd * cb_ibd);
  a = a + .naz(b_manicschiz * cb_manicschiz);
  a = a + .naz(b_neurorare * cb_neurorare);
  a = a + .naz(b_parkinsons * cb_parkinsons);
  a = a + .naz(b_pulmhyper * cb_pulmhyper);
  a = a + .naz(b_pulmrare * cb_pulmrare);
  a = a + .naz(b_pvd * cb_pvd);
  a = a + .naz(b_ra_sle * cb_ra_sle);
  a = a + .naz(b_respcancer * cb_respcancer);
  a = a + .naz(b_scid * cb_scid);
  a = a + .naz(b_sicklecelldisease * cb_sicklecelldisease);
  a = a + .naz(b_stroke * cb_stroke);
  a = a + .naz(b_vte * cb_vte);
  a = a + .naz(p_marrow6 * cp_marrow6);
  a = a + .naz(p_radio6 * cp_radio6);
  a = a + .naz(p_solidtransplant * cp_solidtransplant);
  
  cage_1_bmi_1 = 0.0881011501841798549961027;
  cage_1_bmi_2 = -0.0469360832786057877163977;
  cage_2_bmi_1 = -0.0321592497178678216052106;
  cage_2_bmi_2 = 0.0168967404194000046790958;
  
  a = a + .naz(age_1 * bmi_1 * cage_1_bmi_1);
  a = a + .naz(age_1 * bmi_2 * cage_1_bmi_2);
  a = a + .naz(age_2 * bmi_1 * cage_2_bmi_1);
  a = a + .naz(age_2 * bmi_2 * cage_2_bmi_2);
  
  return(a)
}

## QCOVID2 death given positive test - unvaccinated male ----

# QCOVID © Copyright, Oxford University 2021.
# All Rights Reserved. The author, being Professor Julia Hippsley-Cox, has asserted their moral right.

.positive_death_male = function(
  age,b2_82,b2_leukolaba,b2_prednisolone,b_AF,b_CCF,b_asthma,b_bloodcancer,b_cerebralpalsy,b_chd,
  b_cirrhosis,b_congenheart,b_copd,b_dementia,b_epilepsy,b_fracture4,b_hiv,b_ibd,b_manicschiz,
  b_neurorare,b_parkinsons,b_pulmhyper,b_pulmrare,b_pvd,b_ra_sle,b_respcancer,b_scid,b_sicklecelldisease,
  b_stroke,b_vte,bmi,chemocat,ethrisk,homecat,learncat,p_marrow6,
  p_radio6,p_solidtransplant,renalcat,town,typeonecat,typetwocat
) {

  Ichemocat = c(
    0,
    0.5343092180534551038917357,
    0.8924429879067947712911746,
    2.0633756139005408059006186
  );
  Iethrisk = c(
    0,
    0,
    0.2903944450374050800789405,
    0.3925630814792830935999746,
    0.4522971650899274553836449,
    0.4422570042007126223104763,
    0.0972900112379559145470154,
    0.3492368408585162509183419,
    0.8236245087272154874469265,
    0.3333285746128640258945097
  );
  Ihomecat = c(
    0,
    0.0635610372241375148893994,
    0.4048727718599258729170742
  );
  Ilearncat = c(
    0,
    0.2534658183192641245362609,
    2.4148944176803257555263826
  );
  Irenalcat = c(
    0,
    0,
    0.1804059172284968037835995,
    0.5146855327621756570621869,
    0.5429517125841122338059108,
    0.5911801880431366562618223,
    1.1474771504017717838053159
  );
  Itypeonecat = c(
    0,
    0.3562433314065954204075126,
    1.1296507386499647918043365
  );
  Itypetwocat = c(
    0,
    0.1699624442157874337766543,
    0.3444945069393105518962273
  );
  
  dage = age;
  dage=dage/10;
  age_2 = (dage^3)*log(dage);
  age_1 = dage^3;
  dbmi = ifelse(is.na(age), NA, .na.default(bmi, 27.30));
  dbmi=dbmi/10;
  bmi_2 = dbmi^(-.5);
  bmi_1 = dbmi^(-1);
  
  mage_1 = 105.356376647949219;
  mage_2 = 163.560455322265625;
  mbmi_1 = 0.376875340938568;
  mbmi_2 = 0.613901734352112;
  mtown = 0.381615489721298;
  
  age_1 = age_1 - mage_1;
  age_2 = age_2 - mage_2;
  bmi_1 = bmi_1 - mbmi_1;
  bmi_2 = bmi_2 - mbmi_2;
  town = .na.default(town,-0.17) - mtown;
  
  a = rep(0,length(chemocat));
  a = a + .naz(Ichemocat[.na1(chemocat)]);
  a = a + .naz(Iethrisk[.na1(ethrisk)]);
  a = a + .naz(Ihomecat[.na1(homecat)]);
  a = a + .naz(Ilearncat[.na1(learncat)]);
  a = a + .naz(Irenalcat[.na1(renalcat)]);
  a = a + .naz(Itypeonecat[.na1(typeonecat)]);
  a = a + .naz(Itypetwocat[.na1(typetwocat)]);
  
  cage_1 = 0.0751693532213748571990664;
  cage_2 = -0.0290210713474857440186128;
  cbmi_1 = 70.0834183217586854652836337;
  cbmi_2 = -90.3008773718579789147042902;
  ctown = 0.0103933885412884514931608;
  
  a = a + .naz(age_1 * cage_1);
  a = a + .naz(age_2 * cage_2);
  a = a + .naz(bmi_1 * cbmi_1);
  a = a + .naz(bmi_2 * cbmi_2);
  a = a + .naz(town * ctown);
  
  cb2_82 = 0.3004078585711824622705990;
  cb2_leukolaba = 0.1842407594964516059921067;
  cb2_prednisolone = 0.2966906134055413701844373;
  cb_AF = 0.0430824801343868829528461;
  cb_CCF = 0.2729906181565130429689248;
  cb_asthma = -0.1187180434392925837405741;
  cb_bloodcancer = 0.2191507010848094960664412;
  cb_cerebralpalsy = 1.0961500986470236185255089;
  cb_chd = 0.0548491196962335725295645;
  cb_cirrhosis = 0.5739852810803836025144165;
  cb_congenheart = 0.0513343014153226639972871;
  cb_copd = 0.1975208151772390174905070;
  cb_dementia = 0.3312015942179418193092033;
  cb_epilepsy = 0.3236733347417863893014101;
  cb_fracture4 = -0.0067646606883543600879927;
  cb_hiv = 0.6885233790920272101132582;
  cb_ibd = 0.2085113986420802367760530;
  cb_manicschiz = 0.4263531864789826819617247;
  cb_neurorare = 0.6488521026273492697811207;
  cb_parkinsons = 0.3205896091073205944610436;
  cb_pulmhyper = 0.1446786759789511489504577;
  cb_pulmrare = 0.0281503798145750737391690;
  cb_pvd = 0.2622286300907206713084463;
  cb_ra_sle = 0.2816103965754137572474747;
  cb_respcancer = 0.4281092981379201467539986;
  cb_scid = 0.8601136685197863229745963;
  cb_sicklecelldisease = 0.6026795128971084158919780;
  cb_stroke = 0.1806482267590676371593617;
  cb_vte = 0.1175354506758646000452728;
  cp_marrow6 = 0.0173109573323377251286015;
  cp_radio6 = 0.5885030571210743133292453;
  cp_solidtransplant = 0.3497252314688003571596653;
  
  a = a + .naz(b2_82 * cb2_82);
  a = a + .naz(b2_leukolaba * cb2_leukolaba);
  a = a + .naz(b2_prednisolone * cb2_prednisolone);
  a = a + .naz(b_AF * cb_AF);
  a = a + .naz(b_CCF * cb_CCF);
  a = a + .naz(b_asthma * cb_asthma);
  a = a + .naz(b_bloodcancer * cb_bloodcancer);
  a = a + .naz(b_cerebralpalsy * cb_cerebralpalsy);
  a = a + .naz(b_chd * cb_chd);
  a = a + .naz(b_cirrhosis * cb_cirrhosis);
  a = a + .naz(b_congenheart * cb_congenheart);
  a = a + .naz(b_copd * cb_copd);
  a = a + .naz(b_dementia * cb_dementia);
  a = a + .naz(b_epilepsy * cb_epilepsy);
  a = a + .naz(b_fracture4 * cb_fracture4);
  a = a + .naz(b_hiv * cb_hiv);
  a = a + .naz(b_ibd * cb_ibd);
  a = a + .naz(b_manicschiz * cb_manicschiz);
  a = a + .naz(b_neurorare * cb_neurorare);
  a = a + .naz(b_parkinsons * cb_parkinsons);
  a = a + .naz(b_pulmhyper * cb_pulmhyper);
  a = a + .naz(b_pulmrare * cb_pulmrare);
  a = a + .naz(b_pvd * cb_pvd);
  a = a + .naz(b_ra_sle * cb_ra_sle);
  a = a + .naz(b_respcancer * cb_respcancer);
  a = a + .naz(b_scid * cb_scid);
  a = a + .naz(b_sicklecelldisease * cb_sicklecelldisease);
  a = a + .naz(b_stroke * cb_stroke);
  a = a + .naz(b_vte * cb_vte);
  a = a + .naz(p_marrow6 * cp_marrow6);
  a = a + .naz(p_radio6 * cp_radio6);
  a = a + .naz(p_solidtransplant * cp_solidtransplant);
  
  cage_1_bmi_1 = -0.3503898360120490940161631;
  cage_1_bmi_2 = 0.4838244063059173871721441;
  cage_2_bmi_1 = 0.1012775205039646020388489;
  cage_2_bmi_2 = -0.1438807402406489244217624;
  
  a = a + .naz(age_1 * bmi_1 * cage_1_bmi_1);
  a = a + .naz(age_1 * bmi_2 * cage_1_bmi_2);
  a = a + .naz(age_2 * bmi_1 * cage_2_bmi_1);
  a = a + .naz(age_2 * bmi_2 * cage_2_bmi_2);
  
  return(a)
}


.test_qcovid= function() {
  
  
  
}
