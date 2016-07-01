# Decoding brain signals -- a Microsoft Azure ML Competition project 

This project is for the MS Azure ML competition, see the [competition website](https://gallery.cortanaintelligence.com/Competition/Decoding-Brain-Signals-2) for detail, or [here](doc/competition_description.md).

Based on Dr. Miller's papers [[Miller2009](../doc/Miller2009.pdf), [Miller2010](../doc/Miller2010.pdf), [Miller2014](../doc/Miller2014.pdf), [Miller2015](../doc/Miller2015.pdf)], different types of features are generated. In this project, I generated three type of features. 

* Features of type 1 are computed by the template projection technique, which is simply the similarity of the event signal to the mean of signals of the same responses, i.e., face or house pictures were presented. 
* The type 2 features are generated by the following sequence of operations: (a) compute and normalize the power spectral density (psd) for each event. An event is a signal window of fixed lenght. (b) compute the covariance matrix between frequencies, (c) compute the eigenvectors, also named as the principle spectral component (psc), of the covariance matrix, and (d) project the psd of each event onto the 1st psc to achive the type 2 features.
* The type 3 features are generated using *continus spectral analysis and decomposition*. In short, it's to compute the type 2 features for a time-dependent sliding window and obtain a time-dependent sequence, called broadband signals. The type 3 features are then generated by applying the template projection technique to the broadband signals. In practis, instead of the psd, the time-frequency representation of the signal is approximated by the wavelet transform. Particularly the R library *RWave* is employed to perform wavelet transform.

In the competition, my prediction reaches a ~90% of accurancy (by randomForest/gbm) on a validation dataset on my local machine. Howverever,  the best score on the test data after submission was 81.25%. I suspect the error comes in due to the random shuffling the the event orders in the test data. 
