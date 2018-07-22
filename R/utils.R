########################################################################################################
############################################### HELPERS ################################################

library(digest)
vdigest <- Vectorize(digest)

check.var <- function(var.name) {
  if (!exists(var.name)) {
    stop(paste("Variable name", var.name, "must be set."))
  }
}

check.vars <- function(var.names) {
  print.result.to.null.output <- lapply(var.names, check.var)
}

age.bucket.function <- function(age) {
  if (is.na(age)) {
    'Unknown age'
  } else  if (age < 26) {
    '25 or younger'
  } else if (age >= 26 & age < 31) {
    '26 - 30'
  } else if (age >= 31 & age < 36) {
    '31 - 35'
  } else if (age >= 36 & age < 41) {
    '36 - 40'
  } else if (age >= 41 & age < 46) {
    '41 - 45'
  } else if (age >= 46 & age < 50) {
    '46 - 50'
  } else {
    '51 or older'
  }
}

exp.bucket.function <- function(exp) {
  if (is.na(exp)) {
    'Unknown experience'
  } else  if (exp < 6) {
    '00 - 5 yr exp'
  } else if (exp >= 6 & exp < 11) {
    '06 - 10 yr exp'
  } else if (exp >= 11 & exp < 16) {
    '11 - 15 yr exp'
  } else {
    '16+ yr exp'
  }
}

########################################################################################################
############################################# CONSTANTS ################################################

la.zips <- c(
  70112, 70113, 70114, 70115, 70116, 70117, 70118, 70119, 70122,
  70124, 70125, 70126, 70127, 70128, 70129, 70130, 70131, 70139,
  70142, 70143, 70145, 70146, 70148, 70150, 70151, 70152, 70153,
  70154, 70156, 70157, 70158, 70159, 70160, 70161, 70162, 70163,
  70164, 70165, 70166, 70167, 70170, 70172, 70174, 70175, 70176,
  70177, 70178, 70179, 70182, 70184, 70185, 70186, 70187, 70189,
  70190, 70195)

# Months of the year
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Police districts in New Orleans
districts <- c("1st District", "2nd District", "3rd District", "4th District",
               "5th District", "6th District", "7th District", "8th District")

district.nums <- 1:8

district.num.names <- data.frame(
  district.name = districts,
  district.num = district.nums
)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

########################################################################################################
########################################################################################################

# Race info from census
# Provided by Jakob and referenced in 2016 annual report (source 2010 census)
# Race
asian <- "Asian / Pacific Islander"
black <- "Black / African American"
white <- "White"
hispanic <- "Hispanic / Latinx"
native <- "Native / Indigenous"
unknown.race <- "Unknown race"

########################################################################################################
########################################################################################################

district.races <- c(white, black, hispanic, asian, native, unknown.race)
district.total.pops <- c(30330, 60225, 60201, 52785, 30084, 36832, 64310, 9062)

district.1.races <- c(5866, 20711, 2926, 270, 54, 503)
sum(district.1.races) == district.total.pops[1] # check

district.2.races <- c(32977, 21642, 3133, 1217, 144, 1112)
sum(district.2.races) == district.total.pops[2] # check

district.3.races <- c(24516, 30097, 3271, 1099, 132, 1086)
sum(district.3.races) == district.total.pops[3] # check

district.4.races <- c(13090, 34585, 2728, 1438, 151, 793)
sum(district.4.races) == district.total.pops[4] # check

district.5.races <- c(3719, 24755, 992, 100, 91, 427)
sum(district.5.races) == district.total.pops[5] # check

district.6.races <- c(15206, 17925, 2402, 553, 106, 640)
sum(district.6.races) == district.total.pops[6] # check

district.7.races <- c(2401, 54081, 2120, 4898, 96, 714)
sum(district.7.races) == district.total.pops[7] # check

district.8.races <- c(6995, 1070, 479, 308, 53, 157)
sum(district.8.races) == district.total.pops[8] # check

districts.by.race.wide <- data.frame()

districts.by.race.wide <- rbind(districts.by.race.wide,
                                district.1.races, district.2.races, district.3.races, district.4.races,
                                district.5.races, district.6.races, district.7.races, district.8.races)

colnames(districts.by.race.wide) <- district.races
districts.by.race.wide <- districts.by.race.wide %>% mutate(
  district = districts,
  district.total.pop = district.total.pops
)

districts.by.race <- gather(districts.by.race.wide, race, population, 1:6) %>% mutate(
  population = as.integer(population),
  pct.race.in.district = population / district.total.pop * 100
)
districts.by.race
