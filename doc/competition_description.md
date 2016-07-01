
> Started	3/30/2016, 12:00:00 AM (PDT)
> Ended	6/30/2016, 11:59:59 PM (PDT)

## Summary

Each year, millions of people suffer brain-related disorders and injuries and as a result, many face a lifetime of impairment with limited treatment options. This competition is based on one of the greatest challenges in neuroscience today – how to interpret brain signals.

Building on the work of Dr. Kai J. Miller and other neuroscientists, the competition is designed to further our understanding of how our brain interprets electric signals. The medical community and ultimately, patients will benefit from your expertise in machine learning and data science to help decode these signals.

Through this competition, you will play a key role in bringing the next generation of care to patients through advancing neuroscience research. Build the most intelligent model and accurately predict the image shown to a person based on electric signals in the brain. The Grand Prize winner will get $3,000 cash, followed by a 2nd prize of $1,500 cash, and a 3rd prize of $500 cash.

Please see [this keynote video from CVP Joseph Sirosh announcing this competition at the Strata conference.

Check out [this video on Microsoft's Channel 9 introducing the Decoding Brain Signals competition](https://channel9.msdn.com/Series/FWTV-on-9/Decoding-Brainwaves-with-Azure-Machine-Learning)  and [this video with tips and tricks from the Microsoft data scientists who created the competition](https://channel9.msdn.com/Series/FWTV-on-9/Getting-Started-in-the-Decoding-Brain-Signals-Competition).

## Description
j
The link between object perception and brain activity in visual cortical areas is a problem of fundamental importance in neuroscience. This competition asks you to build machine learning models in Microsoft Cortana Intelligence Suite to decode perceptions of human subjects from brain, specifically Electrocorticographic (ECoG) signals. The learning model needs to predict whether the human subject is seeing a house image (stimulus class 1) or a face image (stimulus class 2) from the ECoG signals collected from the subtemporal cortical surfaces of four seizure patients.

300 gray-scale images of houses (labeled as image class 1) or faces (labeled as image class 2), were displayed in a random order on a screen to the patients. ECoG signals were collected from the cortical surfaces of these patients during the experiments. The competition is to decode visual perceptions of these subjects from the ECoG signals, to predict whether the patient is seeing a house image (class 1) or a face image (class 2).

Each image stimulus is displayed to a patient for exactly 400 milliseconds, followed by a 400-millisecond inter-stimulus interval (ISI) where a blank image is displayed. A stimulus presentation cycle consists of the 400-millisecond ISI, followed by the 400-ms image stimulus. The ECoG signals were collected at the frequency of 1000 per second, i.e., every 1 millisecond there was a signal sample. Each patient has exactly 300 stimulus presentation cycles. In this competition, we share the ECoG signals of the first 200 stimulus presentation cycles and their stimulus types (1-50 are different house stimulus (stimulus class 1 in this binary classification task), and 51-100 are different face stimulus (stimulus class 2 in this binary classification task)).

Similar work on another set of 7 patients has been published at PLOS Computational Biology. The data we used in this competition was provided by the author of this paper, Dr. Kai J. Miller. This data was collected from 4 patients in the same experiment as the 7 patients in his paper. These 4 patients do not overlap with the 7 patients. However, reading through Dr. Miller’s paper may be helpful for you to understand the experimental settings, the ECoG signals, and features that were created in the machine learning experiment.


## Data files
[Details of the data description](http://az754797.vo.msecnd.net/competition/ecog/docs/DataDescription.docx)

Download training dataset [here](http://az754797.vo.msecnd.net/competition/ecog/datasets/ecog_train_with_labels.csv).

### Background

The data for this competition was collected from 4 human subjects. All four subjects were epileptic patients at Harborview Hospital in Seattle, WA. Up to 64 electrodes were implanted at the cortical surface of each patient. Electrocorticographic (ECoG) signals were collected during the experiment (described below).

The dataset is a 298MB csv file with headers. If you want to develop your machine learning models on your own platform, download the data, develop your feature engineering codes, train machine learning models, operationalize them as a web service APIs in AzureML, and submit in Cortana Intelligence Competitions Platform to be evaluated on test data. Instructions and examples of how to do so will be available soon.

Please keep in mind that for some patients, some electrodes might have bad signals and therefore were excluded from the data analysis. So, some patients may not have 64 ECoG signal channels in the data in this competition. But the number of channels of each patient in training and testing data is fixed. Both training and testing data have columns for all 64 channels. If a patient does not have some channels, we use -999999 to pad these missing channels.

The n channels that a patient has are all labeled as Electrode_1 to Electrode_n. The same channel ID in two different patients does not mean that it is from electrodes in the same cortical location. Even if the electrodes are implanted at the same cortical location in two different patients, you should not assume that it is recording the activities of the same group of neurons because the anatomical structures of the brains of two different patients might be different.

The experiments that generated the data were performed at the bedside. Visual stimuli were presented with a monitor at the bedside. The ECoG potentials were sampled at 1000 Hz.

Subjects performed a basic face and house stimulus discrimination task. They were presented with grayscale pictures of faces and houses (luminance- and contrast-matched) that were displayed in random order for 400ms each, with 400ms blank screen inter-stimulus interval (ISI) between the pictures. The 10cm-wide pictures were displayed at ~1m from the patients while they were seated at the bedside. There were 3 experimental runs with each patient, with 50 house pictures and 50 face pictures in each run (for a total of 300 stimuli). We called the 400ms ISI and the 400ms picture display time after that a stimulus presentation cycle.

More details about the experiment and the data can be found at the paper Spontaneous Decoding of the Timing and Content of Human Object Perception from Cortical Surface Recordings Reveals Complementary Information in the Event-Related Potential and Broadband Spectral Change by Kai J. Miller et al.

### Training data

For each patient, the data for the first 200 stimulus presentation cycles were put in the training data. The ending time of training data for each patient is exactly the last stimulus onset time plus 399 ms, which is when the 200th stimulus presentation cycle completes. In the training data, the signal of each electrode is continuous.

Here is the training data schema:

Col 1: PatientID. String. Values p1, p2, p3, or p4

Col 2-65: Electrode_1 to Electrode_64. Float. If you see for a patient, one column has all values -999999, it means that this patient does not have this electrode.

Col 66: Stimulus_Type. Integer. Ranged between 0 and 101. 1-50 means house stimulus (labeled 1 in this binary classification task), 51-100 means face stimulus (labeled 2 in this binary classification task). 101 means it is inter-stimulus interval with blank image displayed. 0 means that blank image displayed. But type 0 is not considered as inter-stimulus interval.

Col 67: Stimulus_ID. Integer. Stimulus presentation cycle ID. In training data, each patient has Stimulus_ID from 1 to 200. In training data, rows where Stimulus_Type = 0 also have Stimulus_ID = -1, and vice versa.

Testing Data

The testing data that your submitted web service API is going to be called to predict has the same data schema as the training data. The only difference is that the true labels in column Stimulus_Type in the testing data are replaced with 1s when the original stimulus type is between 1-100. The testing data is fixed, but not shared with participants.

Another difference is that the stimulus presentation cycles in the testing data are shuffled randomly. The data within each stimulus presentation cycle reserves its original sequence. But after shuffling, the signals surrounding the joints between stimulus presentation cycles n and n+1 are not continuously any more.

How was data split into training and testing data?

For each patient, the data till the end of the 200th stimulus presentation cycle are put into the training data. The remainder is put in the testing data. The stimulus presentation cycles in the testing data are randomly reshuffled, as described above.

How was testing data split into public and private testing data?

The testing data was further split into public and private testing data. For each patient, the first 40 stimulus presentation cycles (stimulus ID 201 to 240) are put into public testing data. The remaining 60 stimulus presentation cycles (stimulus ID 241 to 300) are put into private testing data.
