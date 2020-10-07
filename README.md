# Sepsis Prediction using R
Sepsis is a deadly syndrome where a patient has a severe infection that causes organ failure. The sooner septic patients are treated, the more likely they are to survive, but sepsis can be challenging to recognize. It may be possible to use hospital data to develop machine learning models that could flag patients who are likely to be septic. However, before we develop predictive algorithms, we need a reliable method to determine patients who are septic. One component of sepsis is a severe infection.

In this project, we will use two weeks of hospital electronic health record (EHR) data to find out which patients had a severe infection according to four criteria. We will look into the data to see if a doctor ordered a blood test to look for bacteria (a blood culture) and gave the patient a series of intervenous antibiotics.

### Criterias for Suspecting Infection

- The patient receives antibiotics for a sequence of four days, with gaps of one day allowed.
- The sequence must start with a new antibiotic, defined as an antibiotic type that was not given in the previous two days.
- The sequence must start within two days of a blood culture.
- There must be at least one intervenous (I.V.) antibiotic within the +/-2 day window.
