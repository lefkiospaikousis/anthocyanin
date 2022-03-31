#' This script holds dimension names, labels , and other global values
#' that will be need in the analysis or the plotting

# Variables ---------------------------------------------------------------


vars_indep <- c("humidity", "rain", "altitude", "temp_max", "temp_min", "temp_avg")

vars_dep <- c("cyanidol3G", "delphinidol3G", "peonidol3G", "petunidol3G", "malvidol3G")

vars_weather <- c("Humidity" = "humidity", 
                  "Rain" = "rain",
                  "Temperature" = "temp_avg")

# Variable labels ---------------------------------------------------------

levels_gender                  <- c("Άντρας","Γυναίκα")

levels_age                     <- c("40-59", "60-69", "70-79", "80-89", "90+")

levels_education               <- c("Καμιά εκπαίδευση","Δημοτικό","Απολυτήριο Λυκείου","Δίπλωμα","Πτυχίο")

levels_family_status           <- c("Παντρεμένος/η","Διαζυγευμένος/η","Χήρος/α","Άγαμος/η")

levels_employment              <- c("Πλήρης Απασχόληση","Μερική Απασχόληση","Περιστασιακή Απασχόληση","Καμία Απασχόληση","Συνταξιούχος")

levels_income                  <- c("0 έως 15.000€", "15.000€ έως 25.000€", "25.000€ και άνω")

levels_participation_time      <- c("Μέχρι και 6 μήνες", "7-12 μήνες","13-18 μήνες","19-24 μήνες", "2 -3 χρόνια", "3-4 χρόνια")

levels_visits_frequency        <- c("1 φορά τον μήνα","1 φορά στις 15 ημέρες","1 φορά την εβδομάδα","2-3 φορές την εβδομάδα","4-5 φορές την εβδομάδα")

levels_responses               <- c("Διαφωνώ απόλυτα", "Διαφωνώ", "Ούτε συμφωνώ/ούτε διαφωνώ", "Συμφωνώ",  "Συμφωνώ απόλυτα")


vars_rev <- paste0("service_",c(3,5,9,11,13,16,18,20,22,25,26,28))


#Scales-------------------------------------------------------------------

# Ποιότητα της παρεχόμενης υπηρεσίας

vars_emotional     <- paste0("service_",c(1,2,6,9,14,15,17,30))
vars_respect       <- paste0("service_",c(3,5,13,18,20,22,25,26,28))
vars_security      <- c(paste0("service_", c(12,19,24,27)), paste0("qol_", 1:6))
vars_qol           <- paste0("satisfaction_",c(1:6))

vars_service_total <- c(vars_emotional,
                        vars_respect,
                        vars_security,
                        vars_qol
)

vars_all <- c(
  paste0("service_", 1:30),
  paste0("satisfaction_", 1:6),
  paste0("qol_", 1:6)
)

#Scales labels-------------------------------------------------------------

service_labelsGR <- list(
  
  emotional      = "Ψυχολογική υποστήριξη/ Ενδυνάμωση/ Επικοινωνία",
  respect        = "Σεβασμός/ Αναγνώριση αναγκών",
  security       = "Αίσθημα ασφάλειας/ εμπιστοσύνης/ ικανοποίησης",
  qol            = "Ποιότητας Ζωής",
  service_total  = "Σύνολο Ποιότητας Υπηρεσίας"
  
)

service_labelsEN <- list(
  
  emotional = "Ψυχολογική υποστήριξη/ Ενδυνάμωση/ Επικοινωνία",
  respect   = "Σεβασμός/ Αναγνώριση αναγκών",
  security  = "Αίσθημα ασφάλειας/ εμπιστοσύνης/ ικανοποίησης",
  qol       = "Ποιότητας Ζωής",
  service_total  = "Σύνολο Ποιότητας Υπηρεσίας"
  
)

#dimensions

dim_service <- names(service_labelsEN)




