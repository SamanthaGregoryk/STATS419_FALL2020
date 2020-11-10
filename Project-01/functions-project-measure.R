prepareMeasureData <- function(data){
  # create a df to be safe 
  mydata <- data.frame(data)
  
  # create a new column to convert measurements later
  mydata["convert.units"] <- 0 
  
  # reorganize the data
  mydata <- mydata[,c(1:26, 38, 27:37)]
  
  # start to match data
  mydata$data_collector = factor(mydata$data_collector);
  mydata$person_id = factor(mydata$person_id);
  mydata$writing = factor(tolower(mydata$writing));
  mydata$eye = factor(tolower(mydata$eye));
  mydata$`eye_color` = factor(tolower(mydata$`eye_color`));
  mydata$ethnicity = factor(tolower(mydata$ethnicity));
  mydata$swinging = factor(tolower(mydata$swinging));
  mydata$side = factor(tolower(mydata$side));
  
  ## we have variables 'cm', 'in', 'inches', 'Inch'
  # just checking 
  #table(mydata$units)
  
  # for all 'cm' variables in mydata$units, change mydata$convert.units to 0.393701
  mydata$convert.units[mydata$units == 'cm'] <- 1
  mydata$convert.units[mydata$units == 'in'] <- 2.54
  mydata$convert.units[mydata$units == 'Inch'] <- 2.54
  mydata$convert.units[mydata$units == 'inches'] <- 2.54
  
  # multiply columns (4) through (26) by mydata$convert.units to get in unit
  new.data <- mydata$convert.units * mydata[4:26]
  
  # reconstruct data set
  my.data <- mydata[, c(1:3, 27:38)]
  combind <- cbind(my.data, new.data)
  converted.dataset <- combind[, c(1:3, 16:38, 4:15)]
  
  # change mydata$unit variables 'Inch' and 'inches' to 'in'
  converted.dataset$units[converted.dataset$units == 'in'] <- 'cm'
  converted.dataset$units[converted.dataset$units == 'Inch'] <- 'cm'
  converted.dataset$units[converted.dataset$units == 'inches'] <- 'cm'
  
  # change mydata$gender variables to 'Male', 'Female', and 'Non-binary'
  # just checking 
  #table(converted.dataset$gender)
  
  converted.dataset$gender[converted.dataset$gender == 'f'] <- 'Female'
  converted.dataset$gender[converted.dataset$gender == 'F'] <- 'Female'
  converted.dataset$gender[converted.dataset$gender == 'female'] <- 'Female'
  converted.dataset$gender[converted.dataset$gender == 'm'] <- 'Male'
  converted.dataset$gender[converted.dataset$gender == 'M'] <- 'Male'
  converted.dataset$gender[converted.dataset$gender == 'male'] <- 'Male'
  converted.dataset$gender[converted.dataset$gender == 'non-binary'] <- 'No-binary'
  
  # check for duplicate rows and remove NA values
  library(dplyr)
  converted.dataset <- distinct(converted.dataset)
  converted.dataset.no_na <- na.omit(converted.dataset)
  
  converted.dataset.no_na[, c(1:2, 34, 33, 3:26, 28:32, 35:38)]
  
  # need to merge left and right sides 
  ## find the mean: left + right / 2
  ## make new column and delete the other two 
  ## rearrange the columns 
  
  ## hand.length
  converted.dataset.no_na$hand.length <- rowMeans(converted.dataset.no_na[, c("hand.length.left", "hand.length.right")], na.rm=TRUE)
  cd1 <- converted.dataset.no_na[,!names(converted.dataset.no_na) %in% c("hand.length.left", "hand.length.right")]
  
  ## hand.width
  cd1$hand.width <- rowMeans(cd1[, c("hand.width.left", "hand.width.right")], na.rm=TRUE)
  cd2 <- cd1[,!names(cd1) %in% c("hand.width.left", "hand.width.right")]
  
  ## hand.elbow
  cd2$hand.elbow <- rowMeans(cd2[, c("hand.elbow.left", "hand.elbow.right")], na.rm=TRUE)
  cd3 <- cd2[,!names(cd2) %in% c("hand.elbow.left", "hand.elbow.right")]
  
  ## elbow.armpit
  cd3$elbow.armpit <- rowMeans(cd3[, c("elbow.armpit.left", "elbow.armpit.right")], na.rm=TRUE)
  cd4 <- cd3[,!names(cd3) %in% c("elbow.armpit.left", "elbow.armpit.right")]
  
  ## arm.reach
  cd4$arm.reach <- rowMeans(cd4[, c("arm.reach.left", "arm.reach.right")], na.rm=TRUE)
  cd5 <- cd4[,!names(cd4) %in% c("arm.reach.left", "arm.reach.right")]
  
  ## foot.length
  cd5$foot.length <- rowMeans(cd5[, c("foot.length.left", "foot.length.right")], na.rm=TRUE)
  cd6 <- cd5[,!names(cd5) %in% c("foot.length.left", "foot.length.right")]
  
  ## floor.kneepit
  cd6$floor.kneepit <- rowMeans(cd6[, c("floor.kneepit.left", "floor.kneepit.right")], na.rm=TRUE)
  cd7 <- cd6[,!names(cd6) %in% c("floor.kneepit.left", "floor.kneepit.right")]
  
  ## floor.hip
  cd7$floor.hip <- rowMeans(cd7[, c("floor.hip.left", "floor.hip.right")], na.rm=TRUE)
  cd8 <- cd7[,!names(cd7) %in% c("floor.hip.left", "floor.hip.right")]
  
  ## floor.armpit
  cd8$floor.armpit <- rowMeans(cd8[, c("floor.armpit.left", "floor.armpit.right")], na.rm=TRUE)
  cd9 <- cd8[,!names(cd8) %in% c("floor.armpit.left", "floor.armpit.right", "convert.units")]
  
  measure.output <- cd9[, c(1, 2, 15, 14, 3:6, 20:28, 7:13, 16:19)]
}

compareToNBA <- function(data){
  
  # just to be safe
  measure.df <- data.frame(data)
  
  # since we have only males for the NBA draft dataset, we must compare to only males
  male <- split(measure.df, measure.df$gender)[['Male']]
  
  new.df <- male[, -c(5, 7, 8, 11, 12, 14:17, 19, 21:24, 26)]
}

prepareNBAData <- function(data){
  
  # remove and change columns
  NBA.draft <- NBA.draft[-c(2, 23, 33, 52, 69, 76), c(1, 9, 2, 4, 5, 7, 8, 3, 6)]
  
  # change height name
  names(NBA.draft)[names(NBA.draft) == "Height.w.o.Shoes"] <- "Height"
  
  # ft to cm
  NBA.draft$ft.cm <- 30.48
  NBA.draft$in.cm <- 2.54
  
  # ft to cm 
  NBA.draft$height <- NBA.draft$ft.cm * suppressWarnings(as.numeric(NBA.draft$Wingspan))
  NBA.draft$arm.span <- NBA.draft$ft.cm * suppressWarnings(as.numeric(NBA.draft$Wingspan))
  NBA.draft$arm.reach <- NBA.draft$ft.cm * suppressWarnings(as.numeric(NBA.draft$Standing.Reach))
  
  # in to cm
  NBA.draft$hand.length <- NBA.draft$in.cm * suppressWarnings(as.numeric(NBA.draft$Hand.Length))
  NBA.draft$hand.width <- NBA.draft$in.cm * suppressWarnings(as.numeric(NBA.draft$Hand.Width))
  
  # delete rows with NA values 
  NBA.no_na <- na.omit(NBA.draft)
  
  # change from chr to int
  NBA.no_na$age <- as.integer(NBA.no_na$Age)
  
  # remove original columns and move other columns to a better place 
  NBA.1 <- NBA.no_na[, -c(2:9, 10, 11)]
  NBA.2 <- NBA.1[, c(1, 7, 3:6)]
}

outliers.function.height <- function(data){
  
  Q1 <- quantile(data$height.NA, .25)
  Q2 <- quantile(data$height.NA, .75)
  IQR <- IQR(data$height.NA)
  
  # now that we know the outliers, lets subset them 
  deleted.outliers <- subset(data, data$height.NA > (Q1 - 1.5*IQR) & data$height.NA < (Q2 + 1.5*IQR))
}

outliers.function.arm_span <- function(data){
  
  Q1 <- quantile(data$arm.span.NA, .25)
  Q2 <- quantile(data$arm.span.NA, .75)
  IQR <- IQR(data$arm.span.NA)
  
  # now that we know the outliers, lets subset them 
  deleted.outliers <- subset(data, data$arm.span.NA > (Q1 - 1.5*IQR) & data$arm.span.NA < (Q2 + 1.5*IQR))
}

outliers.function.arm_reach <- function(data){
  
  Q1 <- quantile(data$arm.reach, .25)
  Q2 <- quantile(data$arm.reach, .75)
  IQR <- IQR(data$arm.reach)
  
  # now that we know the outliers, lets subset them 
  deleted.outliers <- subset(data, data$arm.reach > (Q1 - 1.5*IQR) & data$arm.reach < (Q2 + 1.5*IQR))
}

