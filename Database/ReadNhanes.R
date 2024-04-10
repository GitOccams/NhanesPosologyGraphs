#The 1999-2006 Dual Energy X-ray Absorptiometry (DXA) Multiple Imputation Data Files and Technical Documentation
#dxa data from: https://wwwn.cdc.gov/Nchs/Nhanes/Dxa/Dxa.aspx
#The 1999-2006 CDC NCHS National Health and Nutrition Examination Survey
#bmx and demo data from: https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx

library(haven)
library(data.table)

#Demographics
demo_a<-as.data.table(read_xpt("demo.xpt"))
demo_b<-as.data.table(read_xpt("demo_b.xpt"))
demo_c<-as.data.table(read_xpt("demo_c.xpt"))
demo_d<-as.data.table(read_xpt("demo_d.xpt"))

#Combine the years:
#_a: 1999-2000
#_b: 2001-2002
#_c: 2003-2004
#_d: 2005-2006
demo<-rbind(demo_a,demo_b,demo_c,demo_d)
#RIAGENDR: 1=Male,2=Female
#RIDAGEMN - Age in Months - Recode:  Best age in months at date of screening for individuals under 85 years of age.

#Recode gender to SEX where 0=Male and 1=Female:
demo<-demo[,.(SEQN,SEX=RIAGENDR-1,AGE=RIDAGEMN/12)]


#Body measures
bmx_a<-as.data.table(read_xpt("bmx.xpt"))
bmx_b<-as.data.table(read_xpt("bmx_b.xpt"))
bmx_c<-as.data.table(read_xpt("bmx_c.xpt"))
bmx_d<-as.data.table(read_xpt("bmx_d.xpt"))

#Combine the years (see above)
bmx<-rbind(bmx_a,bmx_b,bmx_c,bmx_d,fill=TRUE)

#BMXRECUM - Recumbent Length (cm): 0 MONTHS - 47 MONTHS
#BMXHT - Standing Height (cm): 2 YEARS - 150 YEARS
#BMXWT - Weight (kg): 0 YEARS - 150 YEARS

bmx<-bmx[,.(SEQN,BMXWT,BMXHT,BMXBMI,BMXRECUM)]

#Replace missing standing heights for recumbent heights for <2years
bmx[,BMXHT:=ifelse(is.na(BMXHT),BMXRECUM,BMXHT)]
#bmx[,BMXBMI2:=BMXWT/(BMXHT*BMXHT/10000)]
#library(lattice)
#xyplot(BMXBMI2~BMXBMI,data=bmx)
#Perfect match for available data
bmx[,BMXBMI:=BMXWT/(BMXHT*BMXHT/10000)]
bmx<-bmx[,.(SEQN,WT=BMXWT,HT=BMXHT,BMI=BMXBMI)]


#dxa
dxa_a<-as.data.table(read_xpt("dxx.xpt"))
dxa_b<-as.data.table(read_xpt("dxx_b.xpt"))
dxa_c<-as.data.table(read_xpt("dxx_c.xpt"))
dxa_d<-as.data.table(read_xpt("dxx_d.xpt"))


dim(dxa_a[DXITOT==0])
#[1] 23355   106
dim(dxa_b[DXITOT==0])
#[1] 32125   106
dim(dxa_c[DXITOT==0])
#[1] 29385   106

#The 2005-2006 dataset does not have the DXITOT indicator but it can be constructed from DXITOTST and DXITOTBN
#dim(dxa_d[DXITOT==0])
# Error in .checkTypos(e, names_x) : 
#   Object 'DXITOT' not found. Perhaps you intended DXITOTST, DXITOTBN, DXDTOTOT
dim(dxa_d[DXITOTST==0])
#[1] 24425   107
dim(dxa_d[DXITOTBN==0])
#[1] 25400   107
dim(dxa_d[DXDTOTOT==0])
#[1]   0 107

dxa_d[,DXITOT:=DXITOTST+DXITOTBN]

dim(dxa_d[DXITOTST==0&DXITOTBN==0])
#[1] 24330   107
dim(dxa_d[DXITOT==0])
#[1] 24330   108


#Combine the years (see above)
dxa<-rbind(dxa_a,dxa_b,dxa_c,dxa_d,fill=TRUE)

#_MULT_ - Imputation Version: values of 1 to 5
#DXITOT: Imputation indicator: 0 = data not imputed
#DXDTOLI - Total Lean body mass incl Bone Mineral Content (g)

#R does not like names starting with "_"
setnames(dxa,"_MULT_","MULT")
#Select records without multiple imputation and then use the first of these records
#Convert LBM to kg
dxa<-dxa[DXITOT==0&MULT==1,.(SEQN,LBM=DXDTOLI/1000)]


all<-merge(demo,bmx,by="SEQN",all=TRUE)
all<-merge(all,dxa,by="SEQN",all=TRUE)
all<-all[!is.na(AGE)]
all<-all[!is.na(WT)]
all<-all[!is.na(SEX)]
all<-all[!is.na(BMI)]

all
#         SEQN SEX        AGE   WT    HT      BMI     LBM
#     1:     1   1  2.4166667 12.5  91.6 14.89769      NA
#     2:     2   0 77.1666667 75.4 174.0 24.90421      NA
#     3:     3   1 10.4166667 32.9 136.6 17.63171      NA
#     4:     4   0  1.8333333 13.3  87.1 17.53135      NA
#     5:     5   0 49.7500000 92.5 178.3 29.09639 64.3922
# ---                                                 
# 37656: 41470   0  0.1666667  7.3  66.5 16.50743      NA
# 37657: 41471   0 12.3333333 43.9 154.4 18.41492 37.0549
# 37658: 41472   0 34.1666667 95.8 191.1 26.23277      NA
# 37659: 41473   0 21.2500000 74.2 168.4 26.16494 53.9599
# 37660: 41474   1 16.6666667 74.9 172.8 25.08386 45.2891

write.table(all,file="NhanesDemo2006.csv",sep=",",col.names=TRUE,quote=FALSE,row.names=FALSE)
