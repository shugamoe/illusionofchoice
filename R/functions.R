# GP utility funcs
getDataJoined <- function(dataDir = "data/", join = "looser"){
  require(data.table)
  require(lubridate)

  dataCS <- getDataCS()
  dataPB <- getDataPB()

  # Inner join data sets based on email, isbn, and period (strict). Probably don't wanna do.
  suffix <- c(".cs", ".pb")
  setkey(dataCS, email, isbn, period)
  dataJoinedStrict <- merge(dataCS, dataPB,
                              by = c("email", "isbn", "period"),
                              suffixes = suffix)
  setcolorder(dataJoinedStrict, c("cs.id", "pb.id", "email", "period", "crsesec", "course"))
  dataJoinedStrict[,cs.count := .N, by = cs.id]
  dataJoinedStrict[,pb.count := .N, by = pb.id]
  dataJoinedStrict <- dataJoinedStrict[order(pb.count, email, cs.count, cs.id)]

  # Inner join  only on email and period (loose). Probably wanna do.
  dataJoinedLoose <- merge(dataCS, dataPB, by = c("email", "period"),
                             suffixes = suffix)[isbn.cs == isbn.pb,
                                                isbn.match := T][
                                                isbn.cs != isbn.pb,
                                                isbn.match := F][,
                                                email.count := .N, by = email
                                                                 ][,
                                                pb.count := .N, by = pb.id
                                                                   ][,
                                                cs.count := .N, by = cs.id
                                                                 ]

  # Inner join  only on email and period (looser)
  dataJoinedLooser <- merge(dataCS, dataPB, by = c("email"),
                             suffixes = suffix)[isbn.cs == isbn.pb,
                                                isbn.match := T][
                                                isbn.cs != isbn.pb,
                                                isbn.match := F][,
                                                email.count := .N, by = email
                                                                 ][,
                                                pb.count := .N, by = pb.id
                                                                   ][,
                                                cs.count := .N, by = cs.id
                                                                 ]
  # An even looser join allows for clean sample observations to join to price
  # beliefs observations as long as period.cs <= period.pb
  dataJoinedLooser  <- dataJoinedLooser[(period.cs == "fall12" & period.pb %in% c("fall12", "spring13"))
                                          | (period.cs == "spring13" & period.pb == "spring13")]


  setcolorder(dataJoinedLoose, c("cs.id", "pb.id", "email", "period", "crsesec", "course", "isbn.match", "isbn.cs", "isbn.pb", "major.pb", "major.cs", "freshdum.pb", "freshdum.cs"))
  setcolorder(dataJoinedLooser, c("cs.id", "pb.id", "email", "period.cs", "period.pb", "crsesec", "course", "isbn.match", "isbn.cs", "isbn.pb", "major.pb", "major.cs", "freshdum.pb", "freshdum.cs"))

  returnData  <- switch(join, "strict" = dataJoinedStrict, "loose" = dataJoinedLoose, "looser" = dataJoinedLooser)
  return(returnData)
}

getDataCS  <- function(dataDir = "data/", handleMultiClasses = T){
  require(data.table)
  dataCSRaw <- fread(paste0(dataDir, 'cleansample.csv'))

  # ID dataset
  dataCSRaw[,cs.id := .I]

  # Remove blank emails
  dataCS <- dataCSRaw[email != ""]

  # Create a period column for CS
  dataCS[fall12dum == 1, period := "fall12"][
    spring13dum == 1, period := "spring13"
  ]

  # CS: Get fracs of class (crsesec) with offered, and fieldcourse
  dataCS[, sec_fc_count := sum(fieldcourse), by = .(crsesec, period)][,
            sec_of_count := sum(offered), by = .(crsesec, period)][,
            ':=' (sec_fc_frac = sec_fc_count / .N, sec_of_frac = sec_of_count / .N), by = .(crsesec, period)]

  # Added to account for 3/6/2020 drawing in C48.
  if (handleMultiClasses == T){
    dataCS[, emailPeriodCount := .N, by=c("email", "period")]
    dataCS[, offeredSum := sum(offered), by=c("email", "period")]
    dataCS[, fieldCourseSum := sum(fieldcourse), by=c("email", "period")]
  }

  # Remove respondents with more than 10 semesters of college
  dataCS  <- dataCS[semesters <= 10]

  return(dataCS)
}

getDataPB  <- function(dataDir = "data/"){
  require(data.table)
  require(lubridate)

  dataPB <- fread(paste0(dataDir, 'pricebeliefs.csv'))

  # ID dataset
  dataPB[,pb.id := .I]

  # Remove blank emails
  dataPB  <- dataPB[email != ""]

  # Put month and year into PB create a "period" column (fall12 or spring13)
  pbDate <- "%m/%d/%Y %H:%M"
  dataPB[, ':=' (startdate = as.POSIXct(startdate, format = pbDate),
                  enddate = as.POSIXct(enddate, format = pbDate))][,
    ':=' (startmonth = month(startdate), startyear = year(startdate),
          endmonth = month(enddate), endyear = year(enddate))][,
    datedelta := enddate - startdate][
    startyear == 2013 & startmonth == 4, period := "spring13"][
    startyear == 2012 & startmonth %in% c(11, 12), period := "fall12"
    ]

  # Remove respondents with more than 10 semesters of college
  dataPB  <- dataPB[semesters <= 10]

  return(dataPB)
}

getData  <- function(dataType){
  rv <- switch(dataType, "cs" = getDataCS(),
               "pb" = getDataPB(),
               "strict" = getDataJoined(join = dataType),
               "loose" = getDataJoined(join = dataType),
               "looser" = getDataJoined(join = dataType))
  (rv)
}

## http://www.cookbook-r.com/Manipulating_data/Summarizing_data/
## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence
## interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95) {
    require(doBy)
    require(data.table)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # Collapse the data
    formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
    datac <- summaryBy(formula, data=data, FUN=c(length2,mean,sd), na.rm=na.rm)

    # Rename columns
    names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- measurevar
    names(datac)[ names(datac) == paste(measurevar, ".sd",      sep="") ] <- "sd"
    names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- "N"

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval:
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(as.data.table(datac))
}

# Funcs for drawing made in room C35 2/24/2020
# (1) binary treat/no treat
createTreatmentPlot <- function(dataType, data = NULL){ 
  require(ggplot2)
  require(magrittr)
  if (is.null(data)){
    data <- getData(dataType)
  }

  # Treatment and control column, then a compliant/non-compliant treatment grouping
  data[offered != 1, treatOrControl := "control"][
    offered == 1, treatOrControl := "treat"][
    offered == 1 & fieldcourse == 1, treatComplyOrNot := "comply"][
    offered == 1 & fieldcourse != 1, treatComplyOrNot := "nocomply"
    ]

  # Help our labels look nice
  data$treatComplyOrNot  <- factor(data$treatComplyOrNot, levels = c("nocomply", "comply"))

  (ggplot(data, aes(treatOrControl, fill = treatComplyOrNot)) +
    geom_bar() +
    # geom_text(stat='count', aes(label=..count..), vjust=-1) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..))),
             stat="count", vjust = 1.1) +
    labs(title = "Treat/No Treat breakdown", subtitle = paste0("W/ treatment compliance. N = ", nrow(data))) +
    theme_minimal()
  )
}

# (2a) Conditional on treat: # classes in treatment
createTreatmentPlotClasses  <- function(dataType, data = NULL){
  require(ggplot2)
  if (is.null(data)){
    data <- getData(dataType)
  }

  dataUniqueClass  <- switch(dataType,
    "cs" = unique(data[,.(crsesec, period, sec_fc_count, sec_fc_frac, sec_of_count, sec_of_frac)]),
    "loose" = unique(data[,.(crsesec, period, sec_fc_count, sec_fc_frac, sec_of_count, sec_of_frac)]),
    "looser" = unique(data[,.(crsesec, period.cs, sec_fc_count, sec_fc_frac, sec_of_count, sec_of_frac)])
    )

  dataUniqueClass[sec_of_frac == 0, treatOrControl := "control"][
                  sec_of_frac == 1, treatOrControl := "treat"][
                  sec_of_frac == 1 & sec_fc_frac == 1, treatComplyOrNot := "comply"][
                  sec_of_frac == 1 & sec_fc_frac == 0, treatComplyOrNot := "nocomply"]
  # Help our labels look nice
  dataUniqueClass$treatComplyOrNot  <- factor(dataUniqueClass$treatComplyOrNot, levels = c("nocomply", "comply"))

  (ggplot(dataUniqueClass, aes(treatOrControl, fill = treatComplyOrNot)) +
   geom_bar() +
    # geom_text(stat='count', aes(label=..count..), vjust=-1) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..))),
              stat="count", vjust = 1.1) +
   labs(title = "Treat/No Treat breakdown by class", subtitle = paste0("W/ treatment compliance. N (classes) = ", nrow(dataUniqueClass))) +
   theme_minimal()
  )
}

# (2b) Compare online price, bookstore price, their difference, proportion.
createTreatmentPlotClassesPriceDiff  <- function(dataType, data = NULL){
  require(ggplot2)
  require(magrittr)
  if (is.null(data)){
    data <- getData(dataType)
  }

  # Include, 'uncnewp', 'uncusedp', 'onlinenewp', 'onlineusedp'
  dataUniqueClass  <- switch(dataType,
    "cs" = unique(data[,.(crsesec, period, sec_fc_count, sec_fc_frac, sec_of_count, sec_of_frac, uncnewp, uncusedp, onlinenewp, onlineusedp)]),
    "loose" = unique(data[,.(crsesec, period, sec_fc_count, sec_fc_frac, sec_of_count, sec_of_frac, uncnewp, uncusedp, onlinenewp, onlineusedp)]),
    "looser" = unique(data[,.(crsesec, period.cs, sec_fc_count, sec_fc_frac, sec_of_count, sec_of_frac, uncnewp, uncusedp, onlinenewp, onlineusedp)])
  )

  dataUniqueClass[sec_of_frac == 0, treatOrControl := "control"][
                  sec_of_frac == 1, treatOrControl := "treat"][
                  sec_of_frac == 1 & sec_fc_frac == 1, treatComplyOrNot := "comply"][
                  sec_of_frac == 1 & sec_fc_frac == 0, treatComplyOrNot := "nocomply"]
  N  <- nrow(dataUniqueClass)


  uncnewpSummary <- summarySE(dataUniqueClass, measurevar="uncnewp", groupvars=c("treatOrControl", "treatComplyOrNot"))[, price.where := "uncnewp"] %>% setnames("uncnewp", "price")
  uncusedpSummary <- summarySE(dataUniqueClass, measurevar="uncusedp", groupvars=c("treatOrControl", "treatComplyOrNot"), na.rm = TRUE)[, price.where := "uncusedp"] %>% setnames("uncusedp", "price")
  onlinenewpSummary <- summarySE(dataUniqueClass, measurevar="onlinenewp", groupvars=c("treatOrControl", "treatComplyOrNot"))[, price.where := "onlinenewp"] %>% setnames("onlinenewp", "price")
  onlineusedpSummary <- summarySE(dataUniqueClass, measurevar="onlineusedp", groupvars=c("treatOrControl", "treatComplyOrNot"))[, price.where := "onlineusedp"] %>% setnames("onlineusedp", "price")

  priceSummary  <- rbind(uncnewpSummary, uncusedpSummary, onlinenewpSummary, onlineusedpSummary)


  (ggplot(priceSummary, aes(x=price.where, y=price)) +
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=price-ci, ymax=price+ci),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +
    geom_text(aes(label=round(price), vjust = -.5, hjust = -.8)) +
    labs(title = "Class Textbook price means by treatment group and textbook source/condition.", subtitle = paste0("95% CI | N = ", N, "")) +
    facet_wrap(treatOrControl ~ treatComplyOrNot) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  )

}

# 3 Treat/Control in major class
createTreatmentPlotMajorClass  <- function(dataType, data = NULL){
  require(ggplot2)
  if (is.null(data)){
    data <- getData(dataType)
  }

  # Treatment and control column, then a compliant/non-compliant treatment grouping
  data[offered != 1, treatOrControl := "control"][
    offered == 1, treatOrControl := "treat"][
    offered == 1 & fieldcourse == 1, treatComplyOrNot := "comply"][
    offered == 1 & fieldcourse != 1, treatComplyOrNot := "nocomply"
    ]

  # Help labels look nice
  data$treatComplyOrNot  <- factor(data$treatComplyOrNot, levels = c("nocomply", "comply"))


  # Make major.cs column plotting friendly
  dataUniqueCS  <- switch(dataType,
    "cs" = data[major == 0, major.plot := "(0) Course not in major"][major == 1, major.plot := "(1) Course in major"],
    data[major.cs == 0, major.plot := "(0) Course not in major"][major.cs == 1, major.plot := "(1) Course in major"]
    )


  # There are duplicate clean sample (CS) IDs here but unique price belief ids. We want unique CS ids only since the "major" column for CS is binary
  # dataUniqueCS  <- unique(data[,.(cs.id, email, offered, treatOrControl, treatComplyOrNot, major.plot)])

  (ggplot(dataUniqueCS, aes(treatOrControl, fill = treatComplyOrNot)) +
    geom_bar() +
    # geom_text(stat='count', aes(label=..count..), vjust=-1) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..))),
        stat = "count", vjust = 1.1) +
    labs(title = "Treat/No Treat breakdown by whether Clean Sample Class is in Major",
         subtitle = paste0("W/ treatment compliance. N = ", nrow(dataUniqueCS))) +
    facet_wrap(. ~ major.plot) +
    theme_minimal())
}

# 4 Inexperience (freshman) vs experienced
createTreatmentPlotFreshman  <- function(dataType, data = NULL){
  require(ggplot2)
  if (is.null(data)){
    data <- getData(dataType)
  }

  # Treatment and control column, then a compliant/non-compliant treatment grouping
  data[offered != 1, treatOrControl := "control"][ offered == 1, treatOrControl := "treat"][
    offered == 1 & fieldcourse == 1, treatComplyOrNot := "comply"][
    offered == 1 & fieldcourse != 1, treatComplyOrNot := "nocomply"
    ]

  # Make freshdum column plotting friendly
  data  <- switch(dataType,
    "cs" = data[freshdum == 0, fresh.plot := "(0) 2nd, 3rd, 4th year"][freshdum == 1, fresh.plot := "(1) Freshman"],
    data[freshdum.pb == 0, fresh.plot := "(0) 2nd, 3rd, 4th year"][freshdum.pb == 1, fresh.plot := "(1) Freshman"]
    )

  # Helps our labels look nice
  data$treatComplyOrNot  <- factor(data$treatComplyOrNot, levels = c("nocomply", "comply"))

  (ggplot(data, aes(treatOrControl, fill = treatComplyOrNot)) +
    geom_bar() +
    # geom_text(stat='count', aes(label=..count..), vjust=-1) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..))),
              stat="count", vjust = -1) +
    labs(title = "Treat/No Treat breakdown by Freshman status",
         subtitle = paste0("W/ treatment compliance. N = ", nrow(data))) +
    facet_wrap(. ~ fresh.plot) +
    theme_minimal())
}

# Funcs for drawing made in room C48 3/6/2020

# This breakdown all the different levels we care about for the loose joined
# data
createLooseJoinDrilldownPlot <- function(){
  require(ggplot2)
  require(data.table)

  djLoose <- getDataJoined(join="loose")

  djLoose[fieldCourseSum >= 1, treatOrControl := "treat"]
  djLoose[fieldCourseSum == 0, treatOrControl := "control"]
  djLoose[freshdum.cs == 1, freshmanStatus := "freshman"]
  djLoose[freshdum.cs == 0, freshmanStatus := "sophOrHigher"]

  # Helps our labels look nice
  djLoose$freshmanStatus  <- factor(djLoose$freshmanStatus, levels = c("sophOrHigher", "freshman"))

  (ggplot(djLoose, aes(treatOrControl, fill = freshmanStatus)) +
    geom_bar() +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..))),
                  stat="count", vjust = 1.1) +
    facet_wrap(. ~ period) +
    labs(title = "Treatment/Control breakdown by Freshman Status and Semester",
         subtitle = paste0("N = ", nrow(djLoose))) +
    theme_minimal())
}

# Test updates in beliefs from treatment from fall12 to spring13 
createBeliefUpdateGroups <- function(){
  require(data.table)
  returnList = list()

  djLoose <- getDataJoined(join="loose")

  # Initial control group
  initial  <- djLoose[period == "fall12" & fieldCourseSum == 0, ':=' (initial = 1)][initial == 1]
  emails <- unique(initial$email)
  djLoose[period == "spring13" & email %chin% emails, after := 1]
  afterTreatment <- djLoose[after == 1 & fieldCourseSum >= 1]
  afterControl <- djLoose[after == 1 & fieldCourseSum == 0]

  returnList$initial <- initial
  returnList$afterTreatment <- afterTreatment
  returnList$afterControl <- afterControl
  returnList$after <- djLoose[after == 1]
  returnList$fall12 <- djLoose[period == "fall12"]
  returnList$spring13 <- djLoose[period == "spring13"]

  return(returnList)
}



# data.joined.looser  <- getDataJoined()
# data.joined.loose  <- getDataJoined(join = "loose")
# data.joined.strict  <- getDataJoined(join = "strict")
# data.cs <- getDataCS()# fread('data/cleansample.csv')
# data.pb <- getDataPB()# fread('data/pricebeliefs.csv')
# createTreatmentPlot()
# createTreatmentPlotClasses()
# createTreatmentPlotClassesPriceDiff()
# createTreatmentPlotMajorClass()
# createTreatmentPlotFreshman()
