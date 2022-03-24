* Encoding: UTF-8.

DATASET ACTIVATE DataSet1.
*Impute Missing Data Values.
MULTIPLE IMPUTATION age hgt wgt bmi hc gen phb tv reg 
  /IMPUTE METHOD=AUTO NIMPUTATIONS=5 MAXPCTMISSING=NONE 
  /MISSINGSUMMARIES NONE 
  /IMPUTATIONSUMMARIES MODELS 
  /OUTFILE IMPUTATIONS='C:\Users\4216318\Desktop\shinymice\imp.sav' 
    FCSITERATIONS='C:\Users\4216318\Desktop\shinymice\it.sav' .
