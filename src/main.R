    ### set environment
    srcDir  <- "/Users/Jie/decode_brain/src3/"
    dataDir <- "/Users/Jie/decode_brain/datasets/"
    setwd(srcDir)
    source(paste(srcDir, "models.R", sep=""))
    source(paste(srcDir, "data_processing.R", sep=""))
    
    ### load data, data is downloaded from 
    # http://az754797.vo.msecnd.net/competition/ecog/datasets/ecog_train_with_labels.csv
    datafile <- paste(inDir, "ecog_train_with_labels.csv", sep="")
    dataset1 <- read.csv(datafile, sep=",", header=T, stringsAsFactor=F)
    summary(dataset1)
    
    ### train/test split
    r <- train_test_slipt_indecies(dataset1,0.75)
    dataset_train <- dataset1[r[,1],]
    dataset_valid <- dataset1[r[,2],]
    rm(r)

    ### build features
    # feature type 1: signal similarity
    templates <- fh_templates(dataset_train)
    f1_train  <- fh_project_2_templates(dataset_train, templates)
    f1_valid  <- fh_project_2_templates(dataset_valid, templates)
    
    # feature type 2: broadband frequency projection of each event window
    psd_train <- compute_psd(dataset_train)
    psd_valid <- compute_psd(dataset_valid)
    psd_rbind <- psd_train
    psd_rbind <- rbind(psd_train, psd_valid)
    psd_psc   <- compute_psc(psd_rbind)
    f2_train  <- compute_psd_proj(psd_train, psd_psc)
    f2_valid  <- compute_psd_proj(psd_valid, psd_psc)
    
    # feature type 3: broadband psd similarity in continues time window
    bb = compute_bb(dataset1)
    write.csv(bb,"/Users/Jie/decode_brain/output/model_8.3_bb_mean_psc.csv")
    #bb <- read.csv("/Users/Jie/decode_brain/output/model_8.3_bb_mean_psc.csv", 
    #               sep=",", header=T, stringsAsFactor=F); bb <- bb[,2:68]
    psd_mean = bb[(nrow(bb)-240*4+1):(nrow(bb)-240*2),]  # need to compute test bb
    psc      = bb[(nrow(bb)-240*2+1):nrow(bb),]          # need to compute test bb
    bb       = bb[1:(nrow(bb)-240*4),]
    nrow(bb)-nrow(dataset1) # test sizes, should be of the same size
    # splite train/test
    r <- train_test_slipt_indecies(bb,0.75)
    bb_train <- bb[r[,1],]
    bb_valid <- bb[r[,2],]
    rm(r)
    # projection onto templates
    bb_templates <- fh_templates(bb)
    f3_train  <- fh_project_2_templates(bb_train, bb_templates)
    f3_valid  <- fh_project_2_templates(bb_valid, bb_templates)
    # modify colnames
    ncols1 <- ncol(dataset1)
    col_names <- rep('',(ncols1-3)*2+3)
    col_names[1] <- 'PatientID'
    for (i in 1:(ncols1-3)){
      col_names[i+1] <- paste('bb_Chanel_',i,'_F', sep='') 
      col_names[i+1+ncols1-3] <- paste('bb_Chanel_',i,'_H',sep='')
    }
    col_names[2*(ncols1-3)+2] <- 'Stimulus_Type'
    col_names[2*(ncols1-3)+3] <- 'Stimulus_ID'
    names(f3_train) <- col_names
    names(f3_valid) <- col_names
  
    
