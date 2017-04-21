##  Project: Hospital Quality of Care Scores Association with Clinical Outcomes in MIDAS       
### Author: Georgia Barbayannis; Davit Sargsyan   
### Created: 09/09/2016   

---

### 04/21/2017

* Added 'midas15_dakota_data.R' source file. Based on Jen Wellings' 'HF after AMI' data subsetting.
* Insurance is now combined into just 3 categories (see the code for details). NOTE: update all current projects accordingly.

### 03/25/2017   

* Per Javier's advice, moved all data files outside project folders for faster load of the projects. 
* All data files are now located in 'DATA_HOME = C:/Users/ds752/Documents/git_local/data'

### Notes from 01/06/2017:

1. Analyze each subscore separately.

2. Plot: number of hospitals reporting the scores and number of subscores each year (barplot), starting 2004.

3. New! 2014 scores are online now, update the excel spreadsheet. Also, 2005 scores are now available on the website:
www.???

4. Devide Report Year column into 2: Year Collected and Year Reported; some reports are based on previous years' data.

5. Add PCI and PCI Time Limit columns (e.g. 90, 120, etc, minutes)

6. Download and save all reports; look at the calculations of overall scores. Looks like overall score is NOT the average of components but = # patients reveived care/# patients eligible for care. What does "received care" mean? Should the patient receive all submeasures to count toward the overall score or receiving at least one is enough? If the scores from different years are not compatible, should we analyze the correlation of scores with clinical outcomes for each year separately?

NOTE: given the methodology drift, does delta score make sense?

### Notes from 12/29/2016:

Clinical outcomes: MI readmissions or deaths, death alone (CV and all-cause), MI readmissions alone, readmisson for any reason.

1. Check the clinical outcomes trend, specifically before and after implementation of public score reporting. Investigate 2008 specifically as in both our analysis and Ryan, Nallamothu and Dimick paper it shows a bump (see Exhibit 2). Try to examine possible seasonality effect. 

2. Trend in overall scores - is there improvement from year to year of the scores?

3. Association of process of care scores with clinical outcomes, including adjustment for other risk factors such as year of first MI, age at first MI, gender of patient, teaching/nonteaching hospital , STEMI/NSTEMI, insurance , zip code (income), HF, hypertension, etc.

4. Additional adjustment for hospital to see if the improvement is driven by a handful of very good hospitals (hospital effect)? Need to see if the right lower graph remains significant if you correct for score and correct for hospital. Hospital driven effect – need to see the score effect. 

5. Association of delta scores and patient outcomes, same analysis as (3) and (4) above.

### Summary: 

Did trend of MI change after implementation of guidelines? There were different trends in both scores and outcomes, so how do they relate to each other? Since both change over time and change in different directions, if put them together shows there’s a huge effect so want to adjust to see if this was due to something else. Finally, adjust to see if there are only 1 or 2 hospitals that drive entire analysis and conclusion.