##########################################################################
##
## functions for managing variable names 
##
##########################################################################

background_vars <- function() {
  
  return(c(
    nomem_encr = "Number of household member encrypted",
    nohouse_encr = "Number of household encrypted",
    wave = "Year and month of the field work period",
    positie = "Position within the household",
    lftdcat = "Age in CBS (Statistics Netherlands) categories",
    lftdhhh = "Age of the household head",
    aantalhh = "Number of household members",
    aantalki = "Number of living-at-home children in the household, children of the household head or his/her partner",
    partner = "The household head lives together with a partner (wedded or unwedded)",
    burgstat = "Civil status",
    woonvorm = "Domestic situation",
    woning = "Type of dwelling that the household inhabits",
    belbezig = "Primary occupation",
    brutoink = "Personal gross monthly income in Euros",
    nettoink = "Personal net monthly income in Euros (incl. nettocat)",
    brutocat = "Personal gross monthly income in categories",
    nettocat = "Personal net monthly income in categories",
    oplzon = "Highest level of education irrespective of diploma",
    oplmet = "Highest level of education with diploma",
    oplcat = "Level of education in CBS (Statistics Netherlands) categories",
    doetmee = "Household member participates in the panel",
    sted = "Urban character of place of residence",
    simpc = "Does the household have a simPC?",
    brutoink_f = "Personal gross monthly income in Euros, imputed",
    netinc = "Personal net monthly income in Euros",
    nettoink_f = "Personal net monthly income in Euros, imputed",
    brutohh_f = "Gross household income in Euros",
    nettohh_f = "Net household income in Euros",
    werving = "From which recruitment wave the household originates",
    birthyear_imp = "Year of birth [imputed by PreFer organisers] (based on original gebjaar variable)",
    gender_imp = "Gender [imputed by PreFer organisers] (based on original geslacht variable)",
    migration_background_imp = "Origin [imputed by PreFer organisers] (based on original herkomstgroep variable)",
    age_imp = "Age of the household member [imputed by PreFer organisers] (based on original leeftijd variable)"
  ))
  
}

fixed_predictors <- function() {
  
  
  return(c(
    "birthyear_bg",
    "gender_bg",
    "migration_background_bg",
    "age_bg"
  ))
  
}

fertility_intentions_predictors <- function() {
  return(c(
    "cf08a128",
    "cf09b128",
    "cf10c128",
    "cf11d128",
    "cf12e128",
    "cf13f128",
    "cf14g128",
    "cf15h128",
    "cf16i128",
    "cf17j128",
    "cf18k128",
    "cf19l128",
    "cf20m128",
    "cf08a129",
    "cf09b129",
    "cf10c129",
    "cf11d129",
    "cf12e129",
    "cf13f129",
    "cf14g129",
    "cf15h129",
    "cf16i129",
    "cf17j129",
    "cf18k129",
    "cf19l129",
    "cf20m129",
    "cf08a130",
    "cf09b130",
    "cf10c130",
    "cf11d130",
    "cf12e130",
    "cf13f130",
    "cf14g130",
    "cf15h130",
    "cf16i130",
    "cf17j130",
    "cf18k130",
    "cf19l130",
    "cf20m130"
  ))
} 

varying_predictors <- function() {
  
  # these covars have
  return(c(
    "partner",
    "woonvorm",
    "burgstat",
    "woning",
    "sted",
    "brutohh_f",
    "nettohh_f",
    "belbezig",
    "brutoink",
    "nettoink",
    "oplzon",
    "oplmet",
    "oplcat",
    "brutoink_f",
    "netinc",
    "nettoink_f"
  ))
  
}

fert_predictors <- function() {
  #Have you had any children?(2008-2014)
  everfert1 <- c("cf08a035", "cf09b035" , "cf10c035" , "cf11d035" , "cf12e035" , "cf13f035" , "cf14g035")
  #Did you ever have any children? (2015-2020 )
  everfert2 <- c( "cf15h454", "cf16i454", "cf17j454", "cf18k454", "cf19l454", "cf20m454")
  #How many children have you had in total? (2008-2014)
  quantum_fert1 <- c("cf08a036", "cf09b036", "cf10c036", "cf11d036", "cf12e036", "cf13f036", "cf14g036")
  #How many living children do you have in total? (2015-2020 )
  quantum_fert2 <- c("cf15h455", "cf16i455", "cf17j455", "cf18k455", "cf19l455", "cf20m455")
  
  #Grouping vars on fert intentions
  # Do you think you will have children in the future? -- AND -- Do you think you will have [more] children in the future? c(yes = 1, no = 2, `I don't know` = 3)
  fert_int <- c("cf08a128", "cf09b128", "cf10c128", "cf11d128", "cf12e128", "cf13f128", "cf14g128", "cf15h128", "cf16i128", "cf17j128", "cf18k128", "cf19l128", "cf20m128")
  #How many children do you think you will have in the future?" Numeric responses
  fert_num <- c("cf08a129", "cf09b129", "cf10c129", "cf11d129", "cf12e129", "cf13f129", "cf14g129", "cf15h129", "cf16i129", "cf17j129", "cf18k129", "cf19l129", "cf20m129")
  #Within how many years do you hope to have your (first-next) child?
  fert_time <- c("cf08a130", "cf09b130", "cf10c130", "cf11d130", "cf12e130", "cf13f130", "cf14g130", "cf15h130", "cf16i130", "cf17j130", "cf18k130", "cf19l130", "cf20m130")
  
  ## years since birth by parity vars
  ysb_p1 <- c("cf08a037", "cf09b037", "cf10c037", "cf11d037", "cf12e037", "cf13f037", "cf14g037", "cf15h456", "cf16i456", "cf17j456", "cf18k456", "cf19l456", "cf20m456")
  ysb_p2 <- c("cf08a038", "cf09b038", "cf10c038", "cf11d038", "cf12e038", "cf13f038", "cf14g038", "cf15h457", "cf16i457", "cf17j457", "cf18k457", "cf19l457", "cf20m457")
  ysb_p3 <- c("cf08a039", "cf09b039", "cf10c039", "cf11d039", "cf12e039", "cf13f039", "cf14g039", "cf15h458", "cf16i458", "cf17j458", "cf18k458", "cf19l458", "cf20m458")
  ysb_p4 <- c("cf08a040", "cf09b040", "cf10c040", "cf11d040", "cf12e040", "cf13f040", "cf14g040", "cf15h459", "cf16i459", "cf17j459", "cf18k459", "cf19l459", "cf20m459")
  ysb_p5 <- c("cf08a041", "cf09b041", "cf10c041", "cf11d041", "cf12e041", "cf13f041", "cf14g041", "cf15h460", "cf16i460", "cf17j460", "cf18k460", "cf19l460", "cf20m460")
  ysb_p6 <- c("cf08a042", "cf09b042", "cf10c042", "cf11d042", "cf12e042", "cf13f042", "cf14g042", "cf15h461", "cf16i461", "cf17j461", "cf18k461", "cf19l461", "cf20m461")
  ysb_p7 <- c("cf08a043", "cf09b043", "cf10c043", "cf11d043", "cf12e043", "cf13f043", "cf14g043", "cf15h462", "cf16i462", "cf17j462", "cf18k462", "cf19l462", "cf20m462")
  ysb_p8 <- c("cf08a044", "cf09b044", "cf10c044", "cf11d044", "cf12e044", "cf13f044", "cf14g044", "cf15h463", "cf16i463", "cf17j463", "cf18k463", "cf19l463", "cf20m463")
  ysb_p9 <- c("cf08a045", "cf09b045", "cf10c045", "cf11d045", "cf12e045", "cf13f045", "cf14g045", "cf15h464", "cf16i464", "cf17j464", "cf18k464", "cf19l464", "cf20m464")
  ysb_p10 <- c("cf08a046", "cf09b046", "cf10c046", "cf11d046", "cf12e046", "cf13f046", "cf14g046", "cf15h465", "cf16i465", "cf17j465", "cf18k465", "cf19l465", "cf20m465")
  ysb_p11 <- c("cf08a047", "cf09b047", "cf10c047", "cf11d047", "cf12e047", "cf13f047", "cf14g047", "cf15h466", "cf16i466", "cf17j466", "cf18k466", "cf19l466", "cf20m466")
  ysb_p12 <- c("cf08a048", "cf09b048", "cf10c048", "cf11d048", "cf12e048", "cf13f048", "cf14g048", "cf15h467", "cf16i467", "cf17j467", "cf18k467", "cf19l467", "cf20m467")
  ysb_p13 <- c("cf08a049", "cf09b049", "cf10c049", "cf11d049", "cf12e049", "cf13f049", "cf14g049", "cf15h468", "cf16i468", "cf17j468", "cf18k468", "cf19l468", "cf20m468")
  ysb_p14 <- c("cf08a050", "cf09b050", "cf10c050", "cf11d050", "cf12e050", "cf13f050", "cf14g050", "cf15h469", "cf16i469", "cf17j469", "cf18k469", "cf19l469", "cf20m469")
  ysb_p15 <- c("cf08a051", "cf09b051", "cf10c051", "cf11d051", "cf12e051", "cf13f051", "cf14g051", "cf15h470", "cf16i470", "cf17j470", "cf18k470", "cf19l470", "cf20m470")
  
  
  return(lst(everfert1,
             everfert2,
             quantum_fert1,
             quantum_fert2,
             fert_int, fert_num, fert_time,
             ysb_p1, ysb_p2, ysb_p3, ysb_p4, ysb_p5,
             ysb_p6, ysb_p7, ysb_p8, ysb_p9, ysb_p10,
             ysb_p11, ysb_p12, ysb_p13, ysb_p14, ysb_p15))
  
}

educ_predictors <- function() {
  
  
  
  highest_ed_vars <- c('oplzon_2007', 'oplzon_2008','oplzon_2009','oplzon_2010','oplzon_2011','oplzon_2012','oplzon_2013',
                       'oplzon_2014','oplzon_2015', 'oplzon_2016', 'oplzon_2017', 'oplzon_2018', 'oplzon_2019', 'oplzon_2020')
  
  edu_satisfaction_vars <- c( 'cw08a031', 'cw09b031', 'cw10c031', 'cw11d031', 'cw12e031', 'cw13f031', 'cw14g031', 'cw15h031', 'cw16i031', 'cw17j031',
                              'cw18k031', 'cw19l031', 'cw20m031')
  
  return(lst(highest_ed_vars,
             edu_satisfaction_vars))
}


split_predictors <- function() {
  var_split_play <- c("cf08a198", "cf09b198", "cf10c198", "cf11d198", "cf12e198", "cf13f198", "cf14g198", "cf15h198", "cf16i198", "cf17j198", "cf18k198", "cf19l198", "cf20m527")
  var_split_fetching <- c("cf08a199", "cf09b199", "cf10c199", "cf11d199", "cf12e199", "cf13f199", "cf14g199", "cf15h199", "cf16i199", "cf17j199", "cf18k199", "cf19l199", "cf20m528")
  var_split_emosupport <- c("cf08a200", "cf09b200", "cf10c200", "cf11d200", "cf12e200", "cf13f200", "cf14g200", "cf15h200", "cf16i200", "cf17j200", "cf18k200", "cf19l200", "cf20m529")
  var_split_outings <- c("cf08a201", "cf09b201", "cf10c201", "cf11d201", "cf12e201", "cf13f201", "cf14g201", "cf15h201", "cf16i201", "cf17j201", "cf18k201", "cf19l201", "cf20m530")
  
  var_split_diapers <- c("cf08a202", "cf09b202", "cf10c202", "cf11d202", "cf12e202", "cf13f202", "cf14g202", "cf15h202", "cf16i202", "cf17j202", "cf18k202", "cf19l202", "cf20m202")
  var_split_bedtime <- c("cf08a203", "cf09b203", "cf10c203", "cf11d203", "cf12e203", "cf13f203", "cf14g203", "cf15h203", "cf16i203", "cf17j203", "cf18k203", "cf19l203", "cf20m203")
  var_split_washing <- c("cf08a204", "cf09b204", "cf10c204", "cf11d204", "cf12e204", "cf13f204", "cf14g204", "cf15h204", "cf16i204", "cf17j204", "cf18k204", "cf19l204", "cf20m204")
  var_split_doctor <- c("cf08a205", "cf09b205", "cf10c205", "cf11d205", "cf12e205", "cf13f205", "cf14g205", "cf15h205", "cf16i205", "cf17j205", "cf18k205", "cf19l205", "cf20m205")
  var_split_sick <- c("cf08a206", "cf09b206", "cf10c206", "cf11d206", "cf12e206", "cf13f206", "cf14g206", "cf15h206", "cf16i206", "cf17j206", "cf18k206", "cf19l206", "cf20m206")
  
  var_hhsplit_cook <- c("cf15h483", "cf16i483", "cf17j483", "cf18k483", "cf19l483", "cf20m483")
  var_hhsplit_clothes <- c("cf15h484", "cf16i484", "cf17j484", "cf18k484", "cf19l484", "cf20m484")
  var_hhsplit_clean <- c("cf15h485", "cf16i485", "cf17j485", "cf18k485", "cf19l485", "cf20m485")
  var_hhsplit_other <- c("cf15h486", "cf16i486", "cf17j486", "cf18k486", "cf19l486", "cf20m486")
  var_hhsplit_fin <- c("cf15h487", "cf16i487", "cf17j487", "cf18k487", "cf19l487", "cf20m487")
  var_hhsplit_shop <- c("cf15h488", "cf16i488", "cf17j488", "cf18k488", "cf19l488", "cf20m488")
  
  return(lst(var_split_play,
             var_split_fetching,
             var_split_emosupport,
             var_split_outings,
             var_split_diapers,
             var_split_bedtime,
             var_split_washing,
             var_split_doctor,
             var_split_sick,
             var_hhsplit_cook,
             var_hhsplit_clothes,
             var_hhsplit_clean,
             var_hhsplit_other,
             var_hhsplit_fin,
             var_hhsplit_shop))
}

misc_predictors <- function() {
  
  cur_domestic_vars <-  c("woonvorm_2007", "woonvorm_2008", "woonvorm_2009", "woonvorm_2010", "woonvorm_2011", "woonvorm_2012", "woonvorm_2013", 
                          "woonvorm_2014", "woonvorm_2015", "woonvorm_2016", "woonvorm_2017", "woonvorm_2018", "woonvorm_2019", "woonvorm_2020")
  
  var_urbanicity <- c("sted_2007", "sted_2008", "sted_2009", "sted_2010", "sted_2011", "sted_2012", "sted_2013", "sted_2014", "sted_2015", "sted_2016", "sted_2017", "sted_2018", "sted_2019", "sted_2020")
  var_housing <- c("woning_2007", "woning_2008", "woning_2009", "woning_2010", "woning_2011", "woning_2012", "woning_2013", "woning_2014", "woning_2015", "woning_2016", "woning_2017", "woning_2018", "woning_2019", "woning_2020")
  
  var_ownincome <- paste0('brutoink_f_', 2007:2020)
  var_hhincome <- paste0('brutohh_f_', 2007:2020)  
  
  return(lst(cur_domestic_vars,
             var_urbanicity,
             var_housing,
             var_ownincome,
             var_hhincome))
}

select_relevant_cols <- function(data) {
  
  ## THESE THREE are the original ones
  fp <- fixed_predictors()
  vp <- varying_predictors()
  fip <- fertility_intentions_predictors()
  
  ## THESE are all added w/ artesanal feature engineering
  fert_vars <- fert_predictors()
  educ_vars <- educ_predictors()
  split_vars <- split_predictors()
  misc_vars <- misc_predictors()
  
  other_cols <- c(fert_vars,
                  educ_vars,
                  split_vars,
                  misc_vars) %>%
    unlist(use.names=FALSE)
  
  return(data %>%
           select(gender_bg,
                  # fixed predictors
                  all_of(fp),
                  # other cols
                  all_of(other_cols),
                  # varying predictors
                  # (which appear with _2008, _2009, etc)
                  contains(vp),
                  # fertility intentions predictors
                  contains(fip),
                  # individual id
                  nomem_encr,
                  # hh id
                  nohouse_encr,
                  # whether or not any new child in hh
                  any_new_child_in_hh,
                  # outcome
                  new_child))
}
