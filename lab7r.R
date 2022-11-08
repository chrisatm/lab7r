
init <- function() {
  print("Imported required dataset.")
  library(readxl)
  ICUAdmissionsParsed <- read_excel("C:/Users/chris/Downloads/ICUAdmissions.xls")
  return (ICUAdmissionsParsed)
}

ICUAdmissions <- init()


state_hypothesis <- function(nullVal, statType, unitType, testType) {
  
  # nullVal - float or int
  # statType - µ or p
  # unitType - "dollars" or ""
  # testType - > or < or /=
  
  # print null hypothesis
  null_h <- paste("Ho: µ =", nullVal, unitType)
  print(null_h)
  
  # print alternative hypothesis
  alternate_h <- paste("Ha: µ", testType, nullVal, unitType)
  print(alternate_h)
}

#1
state_hypothesis(0.15, "p", "", ">")


get_sample_proportion <- function(data) {
  data_table <- table(data)
  proportion_table <- prop.table(data_table)
  proportion_second_value <- proportion_table[2]
  print(proportion_second_value)
}

#2
print(paste("The proportion of patients that died in the ICU is", get_sample_proportion(ICUAdmissions$Status)))


get_bootstrap <- function(bootSamples, sampleAmount, probability, diffMode) {
  boot_stats <- 0
  for (i in 1:2000) {
    if (diffMode == FALSE) {
      boot_samples <- sample(bootSamples, sampleAmount, replace = TRUE, prob = c(probability, 1 - probability))
      boot_table <- table(boot_samples)
      proportion_table <- prop.table(boot_table)
      boot_stats[i] <- proportion_table[1]
    } else {
      
      boot_samples1 <- sample(bootSamples, sampleAmount, replace = TRUE, prob = c(probability, 1 - probability))
      boot_table1 <- table(boot_samples1)
      proportion_table1 <- prop.table(boot_table1)
      proportion_val1 <- proportion_table1[2]
      
      boot_samples2 <- sample(bootSamples, sampleAmount, replace = TRUE, prob = c(probability, 1 - probability))
      boot_table2 <- table(boot_samples2)
      proportion_table2 <- prop.table(boot_table2)
      proportion_val2 <- proportion_table2[2]
      
      boot_stats[i] <- proportion_val1 - proportion_val2
    }
    
  }
  return (boot_stats)
}

#3
survived_died_bootstrap <- get_bootstrap(c("died", "lived"), length(ICUAdmissions$Status), 0.15, FALSE)
hist(survived_died_bootstrap, main = "Bootstrap Distribution - Proportion of Patients that died in ICU")


get_pvalue <- function(sampleStat, bootData, conditional) {

  # get total amount of data set values from the bootstrap distribution (should be 2000 by default)
  total_number_bootstrap_stats <- length(bootData)
  
  # set conditions
  condition <- NULL
  if (conditional == ">") {
    condition <- bootData > sampleStat
  } else if (conditional == "<") {
    condition <- bootData < sampleStat
  }
  
  #get condition
  condition <- length(bootData[condition])
  
  # get p-value
  p_value <- condition / total_number_bootstrap_stats
  
  return (p_value)
}

#4
survived_died_pvalue <- get_pvalue(mean(ICUAdmissions$Status), survived_died_bootstrap, ">")
print(paste("The p-value is:", survived_died_pvalue))


state_conclusion <- function(percentConfidence, pVal) {
  alpha <- 1 - percentConfidence
  if (pVal < alpha) {
    return (paste("Reject the null hypothesis because the p-value of", pVal, "is less than the alpha value of", alpha))
  } else {
    return (paste("Not enough evidence to reject the null hypothesis because the p-value of", pVal, "is greater than the alpha value of", alpha))
  }
}

#5
state_conclusion(0.95, survived_died_pvalue)
# There is enough evidence to suggest that more than 15% of patients die in the ICU.


#6
# due to formatting constraints
# p(d) represents proportion of people treated with Desipramine that relapsed
# p(l) represents proportion of people treated with Lithium that relapsed
# Ho: p(d) - p(l) = 0
# Ha: p(d) - p(l) < 0


#7
# n = 24
# p(d) = 10/24
# p(l) = 18/24
# p(d) - P(l) = 
proportion_des <- 10/24
proportion_lit <- 18/24
diff <- proportion_des - proportion_lit
print(paste("p(d) - P(l) = ", diff))


#8
relapse_bootstrap <- get_bootstrap(c("N", "R"), 24, 20/48, TRUE)
hist(relapse_bootstrap, main = "Bootstrap Distribution - Difference Proportion of relapsed")


#9
relapse_pvalue <- get_pvalue(diff, relapse_bootstrap, "<")
print(paste("The p-value is:", relapse_pvalue))


#10
state_conclusion(0.95, relapse_pvalue)
# There is enough evidence to suggest that Desipramine is better than Lithium at treating cocaine addiction.

