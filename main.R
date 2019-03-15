# Import data into R
data15 <- read.csv("all2015.csv", header = FALSE)
data16 <- read.csv("all2016.csv", header = FALSE)
data17 <- read.csv("all2017.csv", header = FALSE)
data18 <- read.csv("all2018.csv", header = FALSE)
data <- rbind(data15,data16,data17,data18)
# Rename the columns
fields <- read.csv("fields.csv")
names(data) <- fields[, "Header"]

# Add Base-Out State of each play to the data

  # Create function that returns Base-Out State in form "XXX X"
  get.state <- function(runner1, runner2, runner3, outs){
    runners <- paste(runner1, runner2, runner3, sep="")
    paste(runners, outs)                      
  }
  
  # Obtain Base-Out State at the BEGINNING of each play
  RUNNER1 <- ifelse(as.character(data[,"BASE1_RUN_ID"])=="", 0, 1)
  RUNNER2 <- ifelse(as.character(data[,"BASE2_RUN_ID"])=="", 0, 1)
  RUNNER3 <- ifelse(as.character(data[,"BASE3_RUN_ID"])=="", 0, 1)
  data$STATE <- get.state(RUNNER1, RUNNER2, RUNNER3, data$OUTS_CT)
  # Obtain Base-Out State at the END of each play
  NRUNNER1 <- with(data, as.numeric(RUN1_DEST_ID==1 | BAT_DEST_ID==1))
  NRUNNER2 <- with(data, as.numeric(RUN1_DEST_ID==2 | RUN2_DEST_ID==2 | 
                                      BAT_DEST_ID==2))
  NRUNNER3 <- with(data, as.numeric(RUN1_DEST_ID==3 | RUN2_DEST_ID==3 | 
                                      RUN3_DEST_ID==3 | BAT_DEST_ID==3))
  NOUTS <- with(data, OUTS_CT + EVENT_OUTS_CT)
  data$NEW.STATE <- get.state(NRUNNER1, NRUNNER2, NRUNNER3, NOUTS)
  # Add up the number of men who came around to score
  data$RUNS.SCORED <- with(data, (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) + 
                             (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3))
  # Only include plays in which the Base-Out State changes OR runs score on the play
  data <- subset(data, (STATE!=NEW.STATE) | (RUNS.SCORED>0))

# Import Leverage Index List
source("leverage.list.R")

# Exclude extra innings since we do not have leverages of those situations
data <- subset(data, INN_CT < 10)

# Create new variable to define the current Half Inning
library(dplyr) # needed to use mutate() function
data <- mutate(data, half_inning = INN_CT*2-1*(BAT_HOME_ID==0))
# Create new variable to define the Run Differential
data <- mutate(data, run_diff = HOME_SCORE_CT-AWAY_SCORE_CT)
data$DIFFERENTIAL[data$run_diff < -3] <- "-4"
data$DIFFERENTIAL[data$run_diff == -3] <- "-3"
data$DIFFERENTIAL[data$run_diff == -2] <- "-2"
data$DIFFERENTIAL[data$run_diff == -1] <- "-1"
data$DIFFERENTIAL[data$run_diff == 0] <- "00"
data$DIFFERENTIAL[data$run_diff == 1] <- "+1"
data$DIFFERENTIAL[data$run_diff == 2] <- "+2"
data$DIFFERENTIAL[data$run_diff == 3] <- "+3"
data$DIFFERENTIAL[data$run_diff > 3] <- "+4"

# Computes Leverage Index of every play based on inning, base-out state, and run differential
for(i in 1:nrow(data)){
  data$leverage[i] = LI[[data$half_inning[i]]][data$STATE[i],data$DIFFERENTIAL[i]]
}
