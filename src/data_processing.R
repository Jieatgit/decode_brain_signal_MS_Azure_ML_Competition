# this file contains the following data processing functions
# fh_get_events             <- function(stim) 
# fh_templates              <- function(dataset_train) 
# fh_project_2_templates    <- function(dataset1, dataset2) 
# train_test_slipt          <- function(dataset1) 
# train_test_slipt_indecies <- function(dataset1, training_portion=0.75) 
# gaussian_smooth           <- function(x, k = 50) 
# compute_psd               <- function(dataset1) 
# compute_psc               <- function(dataset_psd) 
# compute_psd_proj          <- function(dataset_psd, dataset_psd_mean_psc_rbinded) 
# compute_bb                <- function(dataset1) 
# compute_bb_test           <- function(dataset1, dataset_psd_mean, dataset_psc) 

library("zoo")
library(abind)

# Define function fh_get_events() that will be widely used in the pipeline.
# It outputs events start time, midway from event start to the next event, and event type
# Event type has 3 values: 0 means it is the inter-stimulus interval start time, 
#                          1 means it is the house stimulus onset time
#                          2 means it is the face stimulus onset time
# It takes the stimulus column as the input, which records the stimulus type at each sampling point
fh_get_events <- function(stim) {
	nrows <- nrow(stim)
	if (stim[nrows] != 0 & stim[nrows] != 101) {
		#if the last stimulus type is not 0 or 101, 
		#meaning that the stim column ends at the end of a house or a face stimulus presentation cycle. 
		#Padding a 0 after that, otherwise, the last stimulus onset event will not be recoganized. 
		stim <- rbind(stim, 0)
	}
	tmp <- c(0, stim[1:((length(stim) - 1))])
	b <- which((stim - tmp) != 0) #Get the stimulus type changing time
	c <- floor(diff(b) / 2) #Get the half of the times between two consecutive events
	b <- b[1:length(b) - 1]
	d <- b + c # Get the midway between two consecutive events
	evs <- matrix(data = NA, nrow = length(b), ncol = 3)
	evs[, 1] <- b
	evs[, 2] <- d
	evs[, 3] <- stim[d]
	evs <- evs[which(evs[, 3] != 0),] # If stimulus type is 0, it is not ISI, nor stimulus presentation time. Get rid of these events
	evs[which(evs[, 3] < 51), 3] <- 1 # Stimulus types 1 - 50 means it is house stimulus (stimulus class 1)
	evs[which(evs[, 3] == 101), 3] <- 0 # If original stimulus type is 101, meaning that it is ISI, relabel it as event 0
	evs[which(evs[, 3] > 50), 3] <- 2 # Stimulus types 51 - 100 means it is house stimulus (stimulus class 1)
	rm(b, c, d)
	return(evs)
}

# compute face/house signal templates, ie, the mean of chennals with same response
fh_templates <- function(dataset_train) {
  
  # Step 5.3, create templates for each channel, stimulus type (house or face), and for each patient
  ncols <- ncol(dataset_train) # get the number of columns of the training data. Training data has 67 columns. Col1: PatientID, Cols2-65, 64 channels, Col 66: Stimulus Type, Col 67: Stimulus Presentation Cycle ID
  unique_patients <- unique(dataset_train[,1]) # Get the list of unique patients
  num_patients <- length(unique_patients) # Get the number of unique patients
  templates <- as.data.frame(matrix(NA, nrow=1201*num_patients, ncol=ncols-3)) # every patient will have 1201 rows, where the first row is the standard deviation of signals, which will be used to normalize the signal
  # rows 2-601 will be the house template, and rows 602-1201 will be the face template. A template is defined as the average of the signal between 200 ms before the onset of stimulus,
  # and 399 ms after the onset of stimulus. So totally 600 points for a house template, or a face template. 
  PatientID_Col <- matrix(rep('',1201*num_patients), nrow=1201*num_patients,ncol=1) # A column of patientID, which is going to be cbind with templates
  
  # Start building templates for each patient, channel, and stimulus class (house or face)
  for (j in 1:num_patients){
    patient_id <- unique_patients[j] # Get the current patient ID
    PatientID_Col[((j-1)*1201+1):(j*1201),1] <- patient_id # Assign the same patient ID to the patientID column
    data_j <- dataset_train[dataset_train[,1]==patient_id,] # get the data of this specific patient
    ncols_j <- sum(data_j[1,] != -999999) # Determine how many valid columns this patient has (column -999999 means that this patient does not have that signal channel)
    signal_train <- as.matrix(data_j[,2:(ncols_j-2)]) # get the signal for this patient, excluding those -999999 channels
    signal_train <- apply(signal_train, 2, as.numeric) # convert the signal data to numeric, in case they might be treated as string features
    
    stim_train <- as.matrix(data_j[,ncols-1]) # get the column of stim for this patient
    events_train=fh_get_events(stim_train); # get the event matrix
    events_train= events_train[which(events_train[,3]!=0),] # Only keep the stimulus onset time in the trainign data
    events_train=events_train[,-2] # exclude the midway of events column
    train_t=c(1:nrow(stim_train)); # train_t is the row index of training data
    train_e=events_train; # make a copy of the stimulus onset time data 
    num_chans=ncol(signal_train); # get the number of channels this patient has.
    tlims=c(-200,399);# times to start and end erps, this is the time window we are going to add to the stimulus onset time later. The [stimulus onset time -200, stimulus onset time+399] is the time window we used to construct the templates
    erp_baseline=c(-200,49);# times to calcualte erp based upon(must be within tlims) # We take the [stimulus onset time -200, stimulus onset time+49] as the baseline for each stimulus presentation cycle, assuming that the brain has not reponded to the 
    # visual stimulus within 50 milliseconds after the stimulus onset
    train_chans_sd <- rep(0,num_chans) # initialite a variable to hold the standard deviation of signals for each channel. It will be used to normalize the signals.
    for (k in 1:num_chans){
      train_chans_sd[k] <- sd(signal_train[,k]) # get the standard deviation of each channel in the training data
      signal_train[,k] <- signal_train[,k]/sd(signal_train[,k]);# Normalize the scale of each signal by dividing by its own standard deviation
    }
    
    # This function generates the templates for each signal, and for house and face stimulus separately,
    # It is just the average of each signal in the 600 ms window (-200ms before stimulus, and 399 ms after stimulus for each stimulus type
    # over all stimulus presentation cycles in the training data.
    fh_sta = function(inputdata,events,fh_class,tlims) {
      cls_times= events[which(events[,2]==fh_class),1] # get the stimulus onset time of a specified class (this is the onset time of a stimulus, not the ending time of the previous ISI)
      sta=matrix(data=0, nrow=(tlims[2]-tlims[1]+1),ncol=ncol(inputdata))
      for (k in 1:length(cls_times)){
        sta=sta+inputdata[cls_times[k]+c(tlims[1]:tlims[2]),]; #accumulating the signals after realigning all stimulus presentation cycles along the stimulus onset time
      }
      sta=sta/k; # calculate the average
      return(sta) # output the average as the template
    } 
    
    #get sta templates
    
    sta_h=fh_sta(signal_train,train_e,1,tlims);# templates of house stimulus
    sta_f=fh_sta(signal_train,train_e,2,tlims);# templates of face stimulus
    
    # recenter stas w.r.t. baseline
    for (k in 1:num_chans) {
      sta_h[,k]=sta_h[,k]-mean(sta_h[c(erp_baseline[1]:erp_baseline[2])-tlims[1]+1,k]); #remove the baseline (the average between observation 1 to 250 of the template) from the template. These are the final templates.
      sta_f[,k]=sta_f[,k]-mean(sta_f[c(erp_baseline[1]:erp_baseline[2])-tlims[1]+1,k]);
    }
    
    train_chans_sd <- matrix(train_chans_sd,nrow = 1,ncol = num_chans)
    templates[1201*(j-1)+1,1:num_chans] <- train_chans_sd #indgest the calculated templates into the templates variable
    templates[(1201*(j-1)+2):(1201*(j-1)+601),1:num_chans] <- as.matrix(sta_h)
    templates[(1201*(j-1)+602):(1201*j),1:num_chans] <- as.matrix(sta_f)
    indx <- sapply(templates, is.factor)
    col_indx <- c(1:ncol(templates))[indx]
    templates[, col_indx[-1]] <- lapply(templates[,col_indx[-1]], function(x) as.numeric(as.character(x)))
  }
  
  col_names <- rep('',ncols-2)
  col_names[1] <- 'PatientID'
  for (i in 2:(ncols-2)){
    col_names[i] <- paste('Chanel ', (i-1), sep="")
  }
  templates <- as.matrix(templates)
  templates <- data.frame(PatientID_Col, templates, stringsAsFactors=F)
  colnames(templates) <- col_names #assign column names to the data frame before output
  #outputtemplatefile    <- paste(dataDir, "ecog_train_templates.csv", sep="")
  #write.csv(templates, file = outputtemplatefile, row.names=FALSE) #write the template data to a local csv file
  #We will upload this file to AzureML workspace to build the predictive experiment
  
  return(templates)
}

# Function that projects raw ECoG signals to templates.
# dataset1 is the raw ECoG signals, and dataset2 is the templates
fh_project_2_templates <- function(dataset1, dataset2) {
  ncols1 <- ncol(dataset1)
  ncols2 <- ncol(dataset2)
  unique_patients <- unique(dataset1[,1]) # get the list of unique patients
  num_patients <- length(unique_patients) # get the number of unique patients
  train_data <- NULL
  Patient_IDs <- NULL
  Labels <- NULL
  Stimulus_ID <- NULL
  for (j in 1:num_patients){
    dataset2_j <- dataset2[dataset2[,1]==unique_patients[j],2:ncols2] # get the data for this specific patient
    dataset2_j <- apply(dataset2_j, 2, as.numeric)
    nonna_index <- !is.na(dataset2_j[1,])  # in the template data, if a channel does not have value for a patient, this column is missing. is.na() will be true on this column
    ncols2_j <- sum(nonna_index) #number of columns, not including the patientID column
    dataset1_j <- dataset1[dataset1[,1]==unique_patients[j],2:(ncols2_j+1)] # get the signals of this patient
    dataset1_j_stimulus_id <- dataset1[dataset1[,1]==unique_patients[j],ncols1] # get the stimulus presentation cycle ID of this patient
    train_chan_sd <- dataset2_j[1,1:ncols2_j] # for each patient, the first row in the template data is the standard deviation 
    sta_h <- dataset2_j[2:601,1:ncols2_j] # rows 2 to 601 are the house templates
    sta_f <- dataset2_j[602:1201,1:ncols2_j] # rows 602-1201 are the face templates
    signal_train <- as.matrix(dataset1_j)
    signal_train <- apply(signal_train, 2, as.numeric) # convert the signal to numeric
    stim_train <- as.matrix(dataset1[dataset1[,1]==unique_patients[j],ncols1-1]) # get the stimulus type column of this patient
    events_train <- fh_get_events(stim_train) # get the event onset time
    events_train <- events_train[which(events_train[,3]!=0),]
    events_train <- events_train[,-2]
    
    train_t=c(1:nrow(stim_train));
    train_e=events_train;
    num_stimulus <- nrow(train_e)
    Stimulus_ID_j <- matrix(0,nrow=num_stimulus, ncol=1)
    
    num_chans=ncols2_j;
    
    tlims=c(-200,399);# times to start and end erps
    erp_baseline=c(-200,49);# times to calcualte erp based upon(must be within tlims)
    
    for (k in 1:num_chans){
      signal_train[,k]=signal_train[,k]/train_chan_sd[k]; # Normalize the signals by dividing by the stand deviation in the training data
    }
    
    ## generate train data features
    # Originally, the author of the PLOS paper generates 4 training points between each stimulus for each stimulus presentation cycle. 
    # The purpose of doing so is to select features for machine learning models. 
    # That part is skipped in this sample training experiment. The feature selection part is left 
    # for participants to figure out
    
    f_template_train=matrix(0, nrow=nrow(train_e), ncol=num_chans);
    h_template_train=0*f_template_train;
    
    for (k in 1:nrow(train_e)){ # for each stimulus presentation cycle, calculate the similarity between the signal [stimulus onset time - 200, stimulus onset time + 399] and the templates
      Stimulus_ID_j[k,1] <- dataset1_j_stimulus_id[train_e[k,1]] # get the stimulus presentation cycle ID at the onset time of the stimulus
      # dot products to project the raw signal to templates, as similarity measurement. Very similar as calculating the cosine between two vectors
      for (chan in 1:num_chans){
        dt=signal_train[train_e[k,1]+c(tlims[1]:tlims[2]),chan];# select data for that channel
        dt=dt-mean(dt[c(erp_baseline[1]:erp_baseline[2])-tlims[1]+1]);#normalize by substracting the baseline mean of that signal
        
        f_template_train[k,chan]=sum(sta_f[,chan]*dt);# project to the template of face for that channel
        h_template_train[k,chan]=sum(sta_h[,chan]*dt);# project to the template of house for that channel
      }
    }
    
    traindata_f=f_template_train
    traindata_h=h_template_train
    trainlabels=train_e[,2]
    
    trainingdata=abind(traindata_f[which(trainlabels>0),],traindata_h[which(trainlabels>0),],along=2) #consolidate the face similarities and house similarities together into a single dataset
    training_label= as.matrix(trainlabels[which(trainlabels>0)]);
    nrows_j <- nrow(trainingdata)
    trainingdata_j <- matrix(NA, nrow=nrows_j, ncol=2*(ncols1-3))
    trainingdata_j[,1:ncols2_j] <- trainingdata[,1:ncols2_j]
    trainingdata_j[,(ncols1-3+1):(ncols1-3+ncols2_j)] <- trainingdata[,(ncols2_j+1):(2*ncols2_j)]
    train_data <- rbind(train_data,trainingdata_j)
    Patient_IDs <- rbind(Patient_IDs, matrix(unique_patients[j], nrow=nrows_j, ncol=1))
    Labels <- rbind(Labels, training_label)
    Stimulus_ID <- rbind(Stimulus_ID, Stimulus_ID_j)
  }
  nrows <- nrow(train_data)
  Patient_IDs <- as.matrix(Patient_IDs, nrow=nrows, ncol=1)
  train_data <- as.matrix(train_data)
  data.set <- cbind(Patient_IDs, train_data, Labels, Stimulus_ID)
  col_names <- rep('',(ncols1-3)*2+3)
  col_names[1] <- 'PatientID'
  for (i in 1:(ncols1-3)){
    col_names[i+1] <- paste('Chanel_',i,'_F', sep='') # the first 64 variables will be similarity features with face templates, and the next 64 variables will be with house templates
    col_names[i+1+ncols1-3] <- paste('Chanel_',i,'_H',sep='')
  }
  col_names[2*(ncols1-3)+2] <- 'Stimulus_Type'
  col_names[2*(ncols1-3)+3] <- 'Stimulus_ID'
  
  tmp <- apply(data.set[,2:ncol(data.set)], 2, as.numeric)
  tmp[is.na(tmp)] <- 0
  data.set <- data.frame(data.set[,1], tmp, stringsAsFactors = F)
  #data.set <- as.data.frame(data.set)
  colnames(data.set) <- col_names
  indx <- sapply(data.set, is.factor)
  col_indx <- c(1:length(col_names))[indx]
  data.set[, col_indx[-1]] <- lapply(data.set[,col_indx[-1]], function(x) as.numeric(as.character(x))) # convert numeric variables to numeric, not factors
  
  return(data.set)
}



train_test_slipt <- function(dataset1) {  
  # code downloaded from Miscrosoft Azure ML competition, with modifications
  # Step 5.2, split the data into training and validation. For each patient, we take the first 150 stimulus presentation cycles into training,
  # and the remaining 50 stimulus presentation cycles into validation.
  # We name these two datasets as dataset_train, and dataset_valid
  source(functionfile)
  col_names <- colnames(dataset1)
  training_portion <- 3/4 # Taking the first 75% stimulus presentation cycles as the training data, for each patient
  unique_patients <- unique(dataset1[,1]) # get the list of unique patient ID
  num_patients <- length(unique_patients) # get the number of unique patients
  num_cols <- ncol(dataset1)
  dataset_train <- NULL
  dataset_valid <- NULL
  for (i in 1:num_patients){
    data_i <- dataset1[dataset1[,1]==unique_patients[i],] #get the data of a patient
    nrows_i <- nrow(data_i)
    num_cols_i <- sum(data_i[1,] != -999999) # get the number of columns that are not valued -999999, which indicates that this column does not have signals. 
    signal_i <- data_i[,2:(num_cols_i-2)] # get the signals from valid channels for this patient.
    stim_i <- as.matrix(data_i[,(num_cols-1)]) # get the stimulus column of this patient, and convert it to matrix, which is required by function fh_get_events()
    
    ## create events vector. It returns two columns: col1: stimulus start time, col2: stimulus type 1 for house, 2 for face
    events_i=fh_get_events(stim_i); #get the events for this patient
    events_12_i= events_i[which(events_i[,3]!=0),] # only need the stimulus onset time
    events_12_i=events_12_i[,-2]
    num_stimulus_train <- floor(nrow(events_12_i)*training_portion) #number of events to be put in the training data
    train_last_time <- events_12_i[num_stimulus_train,1] + 399 #the last record in the training data should be the stimulus onset time of the last stimulus presentation cycle + 399 milliseconds
    dataset_train <- rbind(dataset_train, data_i[1:train_last_time,]) #add the training records of this patient to data.set
    dataset_valid <- rbind(dataset_valid, data_i[(train_last_time+1):nrows_i,])
  }
  
  dataset_train <- as.data.frame(dataset_train) #convert it to a data frame
  dataset_valid <- as.data.frame(dataset_valid) #convert it to a data frame
  
  return(list("dataset_train"=dataset_train, "dataset_valid"=dataset_valid))
}


train_test_slipt_indecies <- function(dataset1, training_portion=0.75) {  
  # returns row indecies of training and validation data, instead of datasets
  # train and valid data overlap for 400 rows, ie 400 ms of ISI signal
  #
  # Step 5.2, split the data into training and validation. For each patient, we take the first 150 stimulus presentation cycles into training,
  # and the remaining 50 stimulus presentation cycles into validation.
  # We name these two datasets as dataset_train, and dataset_valid
  unique_patients <- unique(dataset1[,1]) # get the list of unique patient ID
  num_patients <- length(unique_patients) # get the number of unique patients
  num_cols <- ncol(dataset1)
  dataset_train <- NULL
  dataset_test  <- NULL
  for (i in 1:num_patients){
    data_i <- dataset1[dataset1[,1]==unique_patients[i],] #get the data of a patient
    num_rows_i <- nrow(data_i)
    stim_i <- as.matrix(data_i[,(num_cols-1)]) # get the stimulus column of this patient, and convert it to matrix, which is required by function fh_get_events()
    
    ## create events vector. It returns two columns: col1: stimulus start time, col2: stimulus type 1 for house, 2 for face
    events_i=fh_get_events(stim_i); #get the events for this patient
    events_12_i= events_i[which(events_i[,3]!=0),] # only need the stimulus onset time
    events_12_i=events_12_i[,-2]
    num_stimulus_train <- floor(nrow(events_12_i)*training_portion) #number of events to be put in the training data
    train_last_time <- events_12_i[num_stimulus_train,1] + 399 #the last record in the training data should be the stimulus onset time of the last stimulus presentation cycle + 399 milliseconds
    
    dataset_train_i <- matrix(FALSE, num_rows_i, 1)
    dataset_train_i[1:(train_last_time+400)] <- TRUE
    dataset_train <- rbind(dataset_train, dataset_train_i)
    
    dataset_test_i <- matrix(TRUE, num_rows_i, 1)
    dataset_test_i[1:train_last_time] <- FALSE
    dataset_test <- rbind(dataset_test, dataset_test_i)
  }
  # return(dataset_train)
  return(cbind(dataset_train, dataset_test))
  
}


gaussian_smooth <- function(x, k = 50) {
  n=3
  w = dnorm(seq(-n,n,by=2*n/(k-1)))
  
  rollapply(
    x,
    width = k,
    function(z){
      #uncomment if you want to see the structure
      #print(head(z,3))
      return(
        weighted_mean = weighted.mean(z,w)
      )
    },
    by.column = FALSE,
    align = "right"
  )  
}


fh_templates_rollmean <- function(templates){
  k=50
  n_cols = ncol(templates)
  for (i in 1:4){
    n_chan = n_cols-1-sum(is.na(templates[1+1201*(i-1),]))
    for (j in 2:n_chan){
      templates[2:601+1201*(i-1),j]    <- rollmean_keep_size(templates[2:601+1201*(i-1),j],k)
      templates[602:1201+1201*(i-1),j] <- rollmean_keep_size(templates[602:1201+1201*(i-1),j],k)
    }
  }
  return(templates)
}

rollmean_keep_size <- function(x,k, align = "right") {  
  y <- x
  rllmn <- rollmean(x,k)
  l <- length(x)
  if (align == "right") {
    y[k:l] <- rllmn
    for (i in 1:(k-1)) {
      y[i] = mean(x[1:i])
    }
  }
  else {
    y[1:(l-k+1)] <- rllmn
    for (i in (l-k+2):l) {
      y[i] = mean(x[i:l])
    }
  }
  return(y)
}


fh_dataset_rollmean <- function(dataset) {
  k = 50
  unique_patients <- unique(dataset1[,1]) # get the list of unique patient ID
  num_patients <- length(unique_patients) # get the number of unique patients
  num_cols <- ncol(dataset1)
  start_index = 1
  for (i in 1:num_patients){
    nrows_i <- sum(dataset[,1]==unique_patients[i])
    nchan_i <- sum(dataset[start_index,] != -999999) - 3
    for (j in 2:(nchan_i+1)) {
      dataset[start_index:(start_index+nrows_i-1),j] <- rollmean_keep_size(dataset[start_index:(start_index+nrows_i-1),j],k)
    }
    start_index = start_index + nrows_i
  }
  return(dataset)
}



#################
compute_psd <- function(dataset1) {
  unique_patients <- unique(dataset1[,1]) # get the list of unique patient ID
  num_patients <- length(unique_patients) # get the number of unique patients
  num_cols <- ncol(dataset1)
  
  dataset_psd <- NULL
  win_size <- 500
  psd_coef <- win_size/2
  w <- hanning.window(1000)
  w <- w[1:win_size]
  for (i in 1:num_patients){
    data_i <- dataset1[dataset1[,1]==unique_patients[i],] #get the data of a patient
    num_cols_i <- sum(data_i[1,] != -999999) # get the number of columns that are not valued -999999, which indicates that this column does not have signals. 
    stim_i <- as.matrix(data_i[,(num_cols-1)]) # get the stimulus column of this patient, and convert it to matrix, which is required by function fh_get_events()
    
    ## create events vector. It returns two columns: col1: stimulus start time, col2: stimulus type 1 for house, 2 for face
    events_i=fh_get_events(stim_i); #get the events for this patient
    events_12_i= events_i[which(events_i[,3]!=0),] # only need the stimulus onset time
    events_12_i=events_12_i[,-2]
    
    data_i_psd <- data.frame(matrix(0, nrow = psd_coef*nrow(events_12_i), ncol = num_cols))
    colnames(data_i_psd) <- colnames(data_i)
    data_i_psd$PatientID <- rep(unique_patients[i], psd_coef*nrow(events_12_i))
    
    # for (electrode_id in 1:1){#(num_cols_i-3)) {
    for (electrode_id in 1:(num_cols_i-3)) {
      print(c(unique_patients[i], electrode_id))
      
      # compute dataset power spectral density(PSD)
      for (event_id in 1:nrow(events_12_i)) {
        # print(event_id)
        e_start <- events_12_i[event_id,1]
        e       <- data_i[ (e_start-(win_size-400)):(e_start+399 ), electrode_id+1]
        # e_fft   <- fft(w*e)
        na_count = sum(is.na(e))
        if (na_count) {
          e[is.na(e)] = e[1:na_count]
        }      
        e_power <- spectrum(w*e, method = "pgram",plot = FALSE)
        e_power <- e_power$spec[1:psd_coef]
        data_i_psd[((event_id-1)*psd_coef+1):(event_id*psd_coef), electrode_id+1] <- e_power
        data_i_psd$Stimulus_Type[((event_id-1)*psd_coef+1):(event_id*psd_coef)] = events_12_i[event_id,2]
        data_i_psd$Stimulus_ID[((event_id-1)*psd_coef+1):(event_id*psd_coef)] = data_i$Stimulus_ID[e_start]
      }
    }
    dataset_psd <- rbind(dataset_psd, data_i_psd)
  }  
  return(dataset_psd)
}

###########

compute_psc <- function(dataset_psd) {
  win_size <- 500
  psd_coef <- win_size/2
  w <- hanning.window(1000)
  w <- w[1:win_size]
  
  unique_patients <- unique(dataset_psd[,1]) # get the list of unique patient ID
  num_patients    <- length(unique_patients) # get the number of unique patients
  num_cols        <- ncol(dataset_psd)
  
  dataset_psd_mean <- NULL
  dataset_psc <- NULL
  for (i in 1:num_patients){
    data_i_psd <- dataset_psd[dataset_psd[,1]==unique_patients[i],] #get the data of a patient
    num_cols_i <- sum(data_i_psd[1,] != 0)
    num_events_i <- nrow(data_i_psd)/psd_coef
    
    data_i_mean <- data.frame(matrix(0, nrow = psd_coef, ncol = num_cols))
    colnames(data_i_mean) <- colnames(data_i_psd)
    data_i_mean$PatientID <- rep(unique_patients[i], psd_coef)
    
    data_i_psc <- data.frame(matrix(0, nrow = psd_coef, ncol = num_cols))
    colnames(data_i_psc) <- colnames(data_i_psd)
    data_i_psc$PatientID <- rep(unique_patients[i], psd_coef)
    
    for (electrode_id in 1:(num_cols_i-3)) {
      print(c(unique_patients[i], electrode_id))
      
      # Step 2: normalize by overall data mean
      x <- data_i_psd[, electrode_id+1]
      e_mean <- log(rowMeans(matrix(x, nrow = psd_coef, ncol = num_events_i )))
      data_i_psd[, electrode_id+1] <- log(data_i_psd[, electrode_id+1]) - rep(e_mean, num_events_i)
      
      data_i_mean[,electrode_id+1] <- log(rowMeans(matrix(x, nrow = psd_coef, ncol = num_events_i )))
#       for (event_id in 1:num_events_i) {
#         e_start = (event_id-1)*psd_coef+1
#         data_i_psd[e_start:(e_start+psd_coef-1), electrode_id+1] <- log(data_i_psd[e_start:(e_start+psd_coef-1), electrode_id+1]) - data_i_mean[,electrode_id+1]
#       }
      
      # Step 3: compute principle spectral components (PSC)
      M <- matrix(data_i_psd[,electrode_id+1], nrow = psd_coef, ncol = num_events_i)
      C <- t(M)
      C <- cov(C)
      # C <- t(C)%*%C
      E <- eigen(C)
      data_i_psc[,electrode_id+1] <- abs(E$vector[,1])
    }
    
    dataset_psd_mean <- rbind(dataset_psd_mean, data_i_mean)
    dataset_psc      <- rbind(dataset_psc, data_i_psc)
  }  
  return(rbind(dataset_psd_mean, dataset_psc))
}


###########

compute_psd_proj <- function(dataset_psd, dataset_psd_mean_psc_rbinded) {
  win_size <- 500
  psd_coef <- win_size/2
  w <- hanning.window(1000)
  w <- w[1:win_size]
  
  
  unique_patients <- unique(dataset_psd[,1]) # get the list of unique patient ID
  num_patients    <- length(unique_patients) # get the number of unique patients
  num_cols        <- ncol(dataset_psd)
  
  split_idx        <- psd_coef*num_patients
  dataset_psd_mean <- dataset_psd_mean_psc_rbinded[1:split_idx,]
  dataset_psc      <- dataset_psd_mean_psc_rbinded[(split_idx+1):(2*split_idx),]
  
  dataset_psd_proj <- NULL
  
  for (i in 1:num_patients){
    data_i_psd   <- dataset_psd[dataset_psd[,1]==unique_patients[i],]
    num_events_i <- nrow(data_i_psd)/psd_coef
    num_rows_i   <- nrow(data_i_psd)
    num_cols_i   <- sum(data_i_psd[1,] != 0)
    
    data_i_psd_proj               <- data.frame(matrix(0, nrow = num_events_i, ncol = num_cols))
    colnames(data_i_psd_proj)     <- colnames(data_i_psd)
    data_i_psd_proj$PatientID     <- unique_patients[i]
    data_i_psd_proj$Stimulus_ID   <- data_i_psd$Stimulus_ID[seq(1,num_rows_i,by=psd_coef)]
    data_i_psd_proj$Stimulus_Type <- data_i_psd$Stimulus_Type[seq(1,num_rows_i,by=psd_coef)]
    
    for (electrode_id in 1:(num_cols_i-3)) {
      print(c(unique_patients[i], electrode_id))
      # normalize by overall data mean
      e_mean <- dataset_psd_mean[(psd_coef*(i-1)+1):(psd_coef*i), electrode_id+1]
      data_i_psd[, electrode_id+1] <- log(data_i_psd[, electrode_id+1]) - rep(e_mean, num_events_i)
      
      # projection
      e_psc <- dataset_psc[(psd_coef*(i-1)+1):(psd_coef*i), electrode_id+1]
      M <- matrix(data_i_psd[,electrode_id+1], nrow = psd_coef, ncol = num_events_i)
      data_i_psd_proj[,electrode_id+1] <- apply(M,2, function(x){sum(x[1:200]*e_psc[1:200])})
    }
    dataset_psd_proj <- rbind(dataset_psd_proj, data_i_psd_proj)
  }  
  return(dataset_psd_proj)
}


#Computes the scale values for a given number of octaves and voices
scales<-function(numOctave,numVoice){
  scale=vector(length=numOctave*numVoice)
  scale.index=1
  for(octave in 0:(numOctave-1)){
    for(voice in 0:(numVoice-1)){
      scale[scale.index]=2^(octave+(voice/numVoice))
      scale.index=scale.index+1
    }
  }
  scale
}

#Computes the corresponding frequency values for a given vector of scales, center frequency, and sample period
scaleToFreq<-function(scale,center,samplePeriod,plot=T){
  freq=(center)/(scale*(samplePeriod*2));
  if(plot==T){
    windows()
    plot(freq,scale,xlim=c(2,40),xlab="Frequency",ylab="Scale",type="l",main="Scale to Frequency")
  }
  freq
}


compute_bb <- function(dataset1) {
  ## psd, psc, psd_proj
  unique_patients <- unique(dataset1[,1]) # get the list of unique patient ID
  num_patients <- length(unique_patients) # get the number of unique patients
  num_cols <- ncol(dataset1)
  
  # wavelet parameters
  s.freq <- 1000 #sample frequency
  octaves=4 #octaves in scale values
  voices =30 #voices per octave
  center =.4
  omega  =2*pi*center #desired center frequency for Morlet wavelet
  freq = scaleToFreq(scales(octaves,voices),center,1/(s.freq),plot=F)
  psd_coef <- length(freq)
  dataset_bb <- NULL
  dataset_psd_mean <- NULL
  dataset_psc <- NULL
  for (i in 1:num_patients){
    data_i <- dataset1[dataset1[,1]==unique_patients[i],] #get the data of a patient
    num_cols_i <- sum(data_i[1,] != -999999) # get the number of columns that are not valued -999999, which indicates that this column does not have signals. 
    stim_i <- as.matrix(data_i[,(num_cols-1)]) # get the stimulus column of this patient, and convert it to matrix, which is required by function fh_get_events()
    
    ## create events vector. It returns two columns: col1: stimulus start time, col2: stimulus type 1 for house, 2 for face
    events_i=fh_get_events(stim_i); #get the events for this patient
    events_12_i= events_i[which(events_i[,3]!=0),] # only need the stimulus onset time
    events_12_i=events_12_i[,-2]
    
    dataset_bb_i = data_i
    dataset_bb_i[,2:(num_cols-2)] <- 0
    
    data_i_mean <- data.frame(matrix(0, nrow = psd_coef, ncol = num_cols))
    colnames(data_i_mean) <- colnames(data_i)
    data_i_mean$PatientID <- rep(unique_patients[i], psd_coef)
    
    data_i_psc <- data.frame(matrix(0, nrow = psd_coef, ncol = num_cols))
    colnames(data_i_psc) <- colnames(data_i)
    data_i_psc$PatientID <- rep(unique_patients[i], psd_coef)
    idx = NULL
    for (k in 1:nrow(events_12_i)) {
      idx = cbind(idx,c(events_12_i[k,1]-200):(events_12_i[k,1]+399))
    }
    #for (electrode_id in 1:1) {
    for (electrode_id in 1:(num_cols_i-3)) {
      print(c(unique_patients[i], electrode_id))
      
      # wavelet transform
      # compute channal by channal 
      w_coef = matrix(1, nrow(data_i), length(freq))
      for (event_id in 1:nrow(events_12_i)) {
        e_start <- events_12_i[event_id,1]
        e       <- data_i[ (e_start-200):(e_start+399 ), electrode_id+1]
        w_coef_e <- cwt(e,noctave=octaves,nvoice=voices,w0=omega,twoD=TRUE,plot=FALSE) 
        w_coef[ (e_start-200):(e_start+399 ), ] <- w_coef_e
      }
      # compute all at once
      #w_coef = cwt(e,noctave=octaves,nvoice=voices,w0=omega,twoD=TRUE,plot=FALSE) 
      #   
      # normalize
      w_coef = (abs(w_coef))^2
      e_mean = log(colMeans(w_coef[c(events_12_i[,1]+200),]))    
      data_i_mean[,electrode_id+1] <- e_mean
      #     plot(freq, e_mean)
      #     n=length(freq)
      #     w = dnorm((freq-freq[1]/2)/(freq[1]/8) )
      #     par(new=TRUE)
      #     plot(freq,w*24)
      w_coef = log(w_coef)
      w_coef[idx,] = t(t(w_coef[idx,]) - e_mean)
      #     dev.new()
      #     plot(freq,e_mean)
      #     plot(freq,w_coef[events_12_i[1,1],])
      #     f_idx = which(events_12_i[,2] == 1)
      #     h_idx = which(events_12_i[,2] == 2)
      #     
      #     for (n in f_idx[1:10]){
      #       print(paste(n, events_12_i[n,1]+200, sep = ", "))
      #       plot(freq,log(w_coef[events_12_i[n,1]+200,])-e_mean,ylim = c(-7,7))
      #       par(new=TRUE)
      #     }
      #     for (n in h_idx[1:10]){
      #       par(new=TRUE)
      #       plot(freq,log(w_coef[events_12_i[n,1]+200,])-e_mean, col="red",ylim = c(-7,7))
      #     }
      #     plot(freq,log(w_coef[events_12_i[25,2]+200,]))
      
      #     # plot time-freq     
      #     n=10
      #     f = e[(events_12_i[1,1]-400):(events_12_i[n,1]+399)]
      #     coef = log(w_coef[(events_12_i[1,1]-400):(events_12_i[n,1]+399),])
      #     # coef = cwt(f,noctave=octaves,nvoice=voices,w0=omega,twoD=TRUE,plot=FALSE) 
      #     
      #     #create an image plot of the squared values of the coefficients
      #     t.axis=seq(0,(length(f)-1)/s.freq,by=1/s.freq) #a time axis to be used in later plots
      #     scale=1:dim(coef)[2] #a dummy axis to be used to display decreasing frequencies
      #     names(scale)=floor(freq) #the names function was used to avoid problems with decreasing frequency values on the y axis
      #     image(t.axis,scale,abs(coef)^2,yaxt="n",ylab="Frequency",xlab="Time",main="Scalogram",col=rainbow(100))
      #     axis(2,at=seq(1,(octaves*voices)),labels=names(scale))
      #     par(new=TRUE)
      #     plot((0:n)*800, rep(1,n+1), col="black",xlim = c(251,800*n-250),ylim = c(0,1.2))
      
      
      # compute psc
      C <- w_coef[c(events_12_i[,1]+200),] # choose only event center time for psc
      C <- cov(C)
      E <- eigen(C)
      psc <- as.vector(abs(E$vector[,1]))
      data_i_psc[,electrode_id+1] <- psc
      
      # compute broadbrand signal
      l = length(freq)
      d_freq <- freq[2:l] - freq[1:(l-1)]
      # another method: focus on freq around 100
      #w = dnorm((freq-freq[1]/2)/(freq[1]/8) )
      #psc = w
      bb  <- rowSums(t(t(w_coef[,2:l])*psc[2:l]*d_freq))
      # bb <- rowSums(w_coef)
      
      # z-score and smoothing
      pop_sd   <- sd(bb)
      pop_mean <- mean(bb)
      bb <- (bb - pop_mean) / pop_sd
      smooth_width=40
      psd_proj=gaussian_smooth(exp(bb), smooth_width)
      # psd_proj=rollmean_keep_size((bb),100, "left")    
      
      #     # plot broadband vs time
      #     aa = psd_proj
      #     n = 10
      #     ymax = max(aa[(events_12_i[1,1]-400):(events_12_i[n,1]+399)])
      #     plot(1:(800*n),(aa[(events_12_i[1,1]-400):(events_12_i[n,1]+399)]), type = "l",xlim = c(0,800*n), ylim = c(0,ymax*1.1))
      #     for (event_id in 1:n){
      #       c = "black"
      #       if (events_12_i[event_id,2] == 1){
      #         c="red"
      #       }  else if(events_12_i[event_id,2] == 2){
      #         c="blue"
      #       }
      #       par(new=TRUE)
      #       plot(401:800+(event_id-1)*800,(aa[events_12_i[event_id,1]:(events_12_i[event_id,1]+399)]), type = "l",col=c,xlim = c(0,800*n), ylim = c(0,ymax*1.1))
      #     }
      #     par(new=TRUE)
      #     plot((0:n)*800, rep(0.02,n+1), col=c,xlim = c(0,800*n), ylim = c(0,ymax*1.1))
      
      dataset_bb_i[smooth_width:nrow(data_i),electrode_id+1] <- psd_proj
    }
    dataset_bb       <- rbind(dataset_bb, dataset_bb_i)
    dataset_psd_mean <- rbind(dataset_psd_mean, data_i_mean)
    dataset_psc      <- rbind(dataset_psc, data_i_psc)
  }  
  outcome <- rbind(dataset_bb, dataset_psd_mean)
  outcome <- rbind(outcome, dataset_psc)
  return(outcome)
  
}


compute_bb_test <- function(dataset1, dataset_psd_mean, dataset_psc) {
  
  ## psd, psc, psd_proj
  unique_patients <- unique(dataset1[,1]) # get the list of unique patient ID
  num_patients <- length(unique_patients) # get the number of unique patients
  num_cols <- ncol(dataset1)
  
  # wavelet parameters
  s.freq <- 1000 #sample frequency
  octaves<-4 #octaves in scale values
  voices <-30 #voices per octave
  center <-.4
  omega  <-2*pi*center #desired center frequency for Morlet wavelet
  freq <- scaleToFreq(scales(octaves,voices),center,1/(s.freq),plot=F)
  psd_coef <- length(freq)
  
  dataset_bb <- NULL
  
  for (i in 1:num_patients){
    data_i <- dataset1[dataset1[,1]==unique_patients[i],] #get the data of a patient
    num_cols_i <- sum(data_i[1,] != -999999) # get the number of columns that are not valued -999999, which indicates that this column does not have signals. 
    stim_i <- as.matrix(data_i[,(num_cols-1)]) # get the stimulus column of this patient, and convert it to matrix, which is required by function fh_get_events()
    
    ## create events vector. It returns two columns: col1: stimulus start time, col2: stimulus type 1 for house, 2 for face
    events_i=fh_get_events(stim_i); #get the events for this patient
    events_12_i= events_i[which(events_i[,3]!=0),] # only need the stimulus onset time
    events_12_i=events_12_i[,-2]
    
    dataset_bb_i = data_i
    dataset_bb_i[,2:(num_cols-2)] <- 0
    idx = NULL
    for (k in 1:nrow(events_12_i)) {
      idx = cbind(idx,c(events_12_i[k,1]-200):(events_12_i[k,1]+399))
    }
    
    for (electrode_id in 1:1) {
    #for (electrode_id in 1:(num_cols_i-3)) {
      print(c(unique_patients[i], electrode_id))
      
      # wavelet transform
      e = data_i[,electrode_id+1]
      
      # compute channal by channal 
      w_coef = matrix(1, nrow(data_i), length(freq))
      for (event_id in 1:nrow(events_12_i)) {
        e_start <- events_12_i[event_id,1]
        e       <- data_i[ (e_start-200):(e_start+399 ), electrode_id+1]
        w_coef_e <- cwt(e,noctave=octaves,nvoice=voices,w0=omega,twoD=TRUE,plot=FALSE) 
        w_coef[ (e_start-200):(e_start+399 ), ] <- w_coef_e
      }
      # compute all at once
      #w_coef = cwt(e,noctave=octaves,nvoice=voices,w0=omega,twoD=TRUE,plot=FALSE) 
         
      # normalize
      w_coef = (abs(w_coef))^2
      e_mean <- dataset_psd_mean[((i-1)*psd_coef+1):(i*psd_coef),electrode_id+1]
      # e_mean = log(colMeans(w_coef[c(events_12_i[,1]+200),]))    
      # data_i_mean[,electrode_id+1] <- e_mean
      w_coef = log(w_coef)
      #w_coef = t(t(w_coef) - e_mean)
      w_coef[idx,] = t(t(w_coef[idx,]) - e_mean)
      
      # compute psc
      #       C <- w_coef[c(events_12_i[,1]+200),] # choose only event center time for psc
      #       C <- cov(C)
      #       E <- eigen(C)
      #       psc <- as.vector(abs(E$vector[,1]))
      #       data_i_psc[,electrode_id+1] <- psc
      psc <- dataset_psc[((i-1)*psd_coef+1):(i*psd_coef),electrode_id+1]
      
      # compute broadbrand signal
      l = length(freq)
      d_freq <- freq[2:l] - freq[1:(l-1)]
      bb  <- rowSums(t(t(w_coef[,2:l])*psc[2:l]*d_freq))
      
      # z-score and smoothing
      pop_sd   <- sd(bb)
      pop_mean <- mean(bb)
      bb <- (bb - pop_mean) / pop_sd
      smooth_width=40
      psd_proj=gaussian_smooth(exp(bb), smooth_width)
      
      dataset_bb_i[smooth_width:nrow(data_i),electrode_id+1] <- psd_proj
    }
    dataset_bb       <- rbind(dataset_bb, dataset_bb_i)
  }  
  return(dataset_bb)
  
}

