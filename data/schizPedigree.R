library(kinship2)
library(data.table)


# 1 - Create a data.table with the family structure: individual ID, dad ID, mom ID, sex (1=male, 2=female) and avail (if theres DNA available: 1=TRUE, 0=FALSE)
# 2 - Create a data frame with the different phenotypes, if applicable. Otherwise, a single column called <i> affected </i> can be added to the end of the family table.
# 3 - Generate the pedigree based on the family table.
# 4 - Add a secondary ID, in this case the DNA voucher ID
# 5 - Plot the pedigree
# 6 - Plot a pedigree legend

# Family 56-195
fam56195 <- data.table(famid=rep(56195, 11), id=c(90317,90316,19502,10208,10209,10203,10210,10211,19503,19504,19505), dadid=c(0,0,0,90317,90317,19502,19502,19502,19502,19502,19502), momid=c(0,0,0,90316,90316,10208,10208,10208,10208,10208,10208), sex=c(1,2,1,2,2,2,2,1,2,1,2), avail=c(0,0,0,0,1,1,1,1,0,0,0))
aff <- data.table(schizophrenia=c(NA,NA,0,1,1,1,1,1,0,0,0), other_mental_disorder=c(NA,NA,0,0,0,0,0,0,1,0,0))
ped56195 <- pedigree(id=fam56195$id, momid=fam56195$momid, dadid=fam56195$dadid, sex=fam56195$sex, affected=as.matrix(aff))
id56195 <- paste(fam56195$id, c(" "," ", " ", " ", "04C30815", "04C30824", "04C30822","04C30825", " ", " ", " "), sep="\n")
plot(ped56195, col=ifelse(fam56195$avail, 4, 1), id=id56195)

# Family 30-30117
fam30117 <- data.table(famid=rep(30117, 19), id=c(10518, 10519, 99996, 10517, 10088, 99995, 10520, 10521, 10102, 10084, 10091, 10095, 10096, 10155, 10116, 10138, 10176, 10527, 10156), dadid=c(0,0,0,0,10518,0,0,0,99996,10517,10517,10517,10517,10517,10517,10520,99995,10520,10521), momid=c(0,0,0,0,10519,0,0,0,10519,10088,10088,10088,10088,10088,10088,10088,10088,10088,10102), sex=c(1,2,1,1,2,1,1,1,2,1,2,1,2,2,1,1,2,1,2), avail=c(rep(0,8),1,1,0,0,0,1,0,1,0,1,0))
aff30117 <- data.table(schizophrenia=c(0,0,NA,NA,1,NA,0,0,1,1,0,0,0,1,0,1,0,0,0), other_mental_disorder=c(0,0,NA,NA,0,NA,0,0,0,0,1,1,1,0,1,0,0,1,0))
ped30117 <- pedigree(id=fam30117$id, momid=fam30117$momid, dadid=fam30117$dadid, sex=fam30117$sex, affected=as.matrix(aff30117))
id30117 <- paste(fam30117$id, c(rep(" ",8), "90C00789", "90C00691", " ", " ", " ", "90C00866", " ", "90C00990", " ", " ", " "), sep="\n")
plot(ped30117, col=ifelse(fam30117$avail, 4, 1), id=id30117)

# Family 30-30134
fam30134 <- data.table(famid=rep(30134, 14), id=c(11136, 11137, 11144, 11140, 11143, 11502, 11142, 11141, 11139, 11135, 11148, 11149, 11147, 11504), dadid=c(0,0,0,11136,0,0,11136,11136,11136,11144,11144,11144,11143,11502), momid=c(0,0,0,11137,0,0,11137,11137,11137,11140,11140,11140,11140,11142), sex=c(1,2,1,2,1,1,2,2,1,1,2,1,2,1), avail=c(rep(0,6),1,0,0,1,1,0,0,0))
aff30134 <- data.table(schizophrenia=c(NA,NA,NA,0,NA,NA,0,0,0,1,1,0,0,1), schizoaffective_dis=c(NA,NA,NA,0,NA,NA,1,0,0,0,0,0,0,0), other_mental_disorder=c(NA,NA,NA,1,NA,NA,0,0,1,0,0,0,1,0))
ped30134 <- pedigree(id=fam30134$id, momid=fam30134$momid, dadid=fam30134$dadid, sex=fam30134$sex, affected=as.matrix(aff30134))
id30134 <- paste(fam30134$id, c(" "," "," ", " "," "," ", "90C03133"," ", " ", "90C02936", "90C02963", " ", " ", " "), sep="\n")
plot(ped30134, col=ifelse(fam30134$avail, 4, 1), id=id30134)

# Family 30-30135

fam30135 <- data.frame(famid=rep(30135, 16), id=c(11176,11177,11259,11181,11182,11183,11265,11173,11174,11187,11188,11186,11194,11191,11175,11199), dadid=c(0,0,0,0,11176,0,11259,11181,11181,0,0,11181,11183,11183,11187,11188), momid=c(0,0,0,0,11177,0,11177,11182,11182,0,0,11182,11182,11182,11174,11174), sex=c(1,2,1,1,2,1,2,1,2,1,1,2,2,2,1,2), avail=c(rep(0,6),1,1,1,0,0,1,0,0,0,0))
aff30135 <- data.frame(schizophrenia=c(NA,0,0,0,1,0,1,1,1,0,0,1,1,0,0,0), paranoid=c(NA,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0), other_mental_disorder=c(NA,0,1,1,0,0,0,0,0,0,1,0,0,0,0,0))
id30135 <- paste(fam30135$id, c(" "," "," "," "," "," ","90C03142", "90C02970", "90C02971", " ", " ", "90C03009", " ", " "," ", " "), sep="\n")
ped30135 <- pedigree(id=fam30135$id, momid=fam30135$momid, dadid=fam30135$dadid, sex=fam30135$sex, affected=as.matrix(aff30135))
ped30135$censor <- c(1,1,1,1,1,1, rep(0,10))
relate <- matrix(c(11173, 11174, 2), nrow=1, byrow=TRUE)
ped30135 <- pedigree(id=fam30135$id, momid=fam30135$momid, dadid=fam30135$dadid, sex=fam30135$sex, affected=as.matrix(aff30135), status=ped30135$censor, relation=relate)
plot(ped30135, col=ifelse(fam30135$avail, 4, 1), id=id30135)

# Family 32-32213

fam32213 <- data.frame(famid=rep(32213, 14), id=c(10634,10630,10686,10685,10651,10650,10637,10636,10322,10281,10280,10282,10290,10283), dadid=c(0,0,0,0,10634,10686,10634,10686,10651,10637,10322,10322,10322,10322), momid=c(0,0,0,0,10630,10685,10630,10685,10650,10636,10281,10281,10281,10281), sex=c(1,2,1,2,1,2,1,2,1,2,1,1,1,2), avail=c(0,0,0,0,0,0,0,0,0,1,1,1,1,1), status=c(rep(1, 8), rep(0,6)))
aff32213 <- data.frame(schizophrenia=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,1,1,1,1,0), other_mental_disorder=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,0,0,0,0,1))
id32213 <- paste(fam32213$id, c(" ", " ", " ", " ", " ", " ", " ", " ", " ", "90C01298", "90C01297", "90C01304", "90C01308", "90C01319"), sep="\n")
ped32213 <- pedigree(id=fam32213$id, momid=fam32213$momid, dadid=fam32213$dadid, sex=fam32213$sex, affected=as.matrix(aff32213), status=fam32213$status)
plot(ped32213, col=ifelse(fam32213$avail, 4, 1), id=id32213)

# Family 30-30136

fam30136 <- data.frame(famid=rep(30136, 12), id=c(11210,11280,11209,11207,11297,11208,11298,11295,11296,11292,11310,11309), dadid=c(0,0,11210,11210,0,11210,0,11210,11210,11210,11297,11298), momid=c(0,0,11280,11280,0,11280,0,11280,11280,11280,11208,11295), sex=c(1,2,1,2,1,2,1,2,2,1,1,2), avail=c(0,0,0,1,0,0,0,1,1,0,1,0), status=c(0,1,rep(0, 10)))
aff30136 <- data.frame(schizophrenie=c(0,NA,0,1,NA,1,0,1,1,0,1,0), other_mental_disorder=c(rep(0, 11), 1))
id30136 <- paste(fam30136$id, c(" "," ", " ","90C02992"," ", " ", " ", "90C03067", "90C03522", " ", "90C03520", " "), sep="\n")
ped30136 <- pedigree(id=fam30136$id, momid=fam30136$momid, dadid=fam30136$dadid, sex=fam30136$sex, affected=as.matrix(aff30136), status=fam30136$status)
plot(ped30136, col=ifelse(fam30136$avail, 4, 1), id=id30136)

# Family 30-30142

fam30142 <- data.frame(famid=rep(30142, 14), id=c(11577,11578,11579,11581,11582,11583,11590,11574,11586,11588,11587,11573,11593,11594), dadid=c(0,0,0,0,11578,11578,0,11581,11581,11581,11581,11590,11590,11590), momid=c(0,0,0,0,11577,11579,0,11582,11582,11582,11582,11574,11574,11574), sex=c(2,1,2,1,2,2,1,2,1,2,2,2,2,2), avail=c(rep(0,5),1,rep(0,5),1,1,0), status=c(rep(1,4),rep(0,10)))
aff30142 <- data.frame(schizophrenia=c(NA,NA,NA,NA,0,1,0,1,0,0,0,1,1,0), other_mental_disorder=c(NA,NA,NA,NA,0,0,1,0,1,1,1,0,0,1))
id30142 <- paste(fam30142$id, c(" "," "," "," "," ", "90C03540"," "," "," "," "," ","90C03310", "90C03332"," "), sep="\n")
ped30142 <- pedigree(id=fam30142$id, momid=fam30142$momid, dadid=fam30142$dadid, sex=fam30142$sex, affected=as.matrix(aff30142), status=fam30142$status)
plot(ped30142, col=ifelse(fam30142$avail, 4, 1), id=id30142)

# Family 31-31114

fam31114 <- data.frame(famid=rep(31114,18), id=c(10097,10098,10099,10037,10096,10084,10088,10090,10036,10091,10092,10085,10086,10087,10093,10094,10095,10089), dadid=c(0,0,0,10097,0,10097,10097,10097,10099,10099,10099,10096,10096,10096,10096,10096,10096,10096), momid=c(0,0,0,10098,0,10098,10098,10098,10037,10037,10037,10084,10084,10084,10084,10084,10084,10084), sex=c(1,2,1,2,1,2,1,1,1,1,1,1,1,2,2,2,2,1), avail=c(c(rep(0,9),1,1,0,0,1,0,0,1,0)), status=c(1,1,1,rep(0,15)))
aff31114 <- data.frame(schizophrenia=c(0,0,0,1,0,0,0,0,0,1,1,1,0,1,0,0,1,0), other_mental_disorder=c(1,1,1,0,1,1,1,1,0,0,0,0,1,0,1,1,0,1))
id31114 <- paste(fam31114$id, c(" ", " ", " ", " ", " ", " "," "," "," ","90C01805","90C01840"," "," ","90C01798"," "," ","90C01801"," "), sep="\n")
ped31114 <- pedigree(id=fam31114$id, momid=fam31114$momid, dadid=fam31114$dadid, sex=fam31114$sex, affected=as.matrix(aff31114), status=fam31114$status)
plot(ped31114, col=ifelse(fam31114$avail, 4, 1), id=id31114)

# Family 31-31119

fam31119 <- data.frame(famid=rep(31119,28), id=c(10278,10279,10282,10047,10284,10283,10287,10288,10049,10048,10046,10056,10065,10067,10068,10043,10291,10293,10294,10070,10081,10103,10104,10054,10302,10299,10121,10124), dadid=c(0,0,10278,0,0,10278,0,0,0,10282,10282,10282,10282,10282,10282,10282,10287,10284,0,10284,10284,10284,10284,10287,10049,10291,10293,10293), momid=c(0,0,10279,0,0,10279,0,0,0,10047,10047,10047,10047,10047,10047,10047,10288,10283,0,10283,10283,10283,10283,10288,10048,10043,10294,10294), sex=c(1,2,1,2,1,2,1,2,1,2,1,2,2,1,1,2,1,1,2,2,1,2,2,2,1,1,2,2), avail=c(rep(0,10),1,0,1,rep(0,8),1,1,0,1,0,0,0), status=c(1,1,1,0,1,1,1,1,rep(0,8),1,1,rep(0,10)))
aff31119 <- data.frame(schizophrenia=c(NA,NA,0,0,NA,NA,NA,NA,0,0,1,0,1,0,0,1,NA,0,0,0,0,1,1,0,1,0,0,0), other_mental_disorder=c(NA,NA,1,0,NA,NA,NA,NA,0,0,0,1,0,1,0,0,NA,1,1,1,1,0,0,1,0,0,1,1))
id31119 <- paste(fam31119$id, c(rep(" ", 10), "90C01235", " ", "90C01334", rep(" ", 8), " 90C01890", "90C01891", " ", "90C04224", rep(" ", 3)), sep="\n")
ped31119 <- pedigree(id=fam31119$id, momid=fam31119$momid, dadid=fam31119$dadid, sex=fam31119$sex, affected=as.matrix(aff31119), status=fam31119$status)
plot(ped31119, col=ifelse(fam31119$avail, 4, 1), id=id31119)


# Family 53-108

fam53108 <- data.frame(famid=rep(53108, 14), id=c(90027,90028,90020,90019,90029,90030,90031,90032,10046,10031,10032,10034,10036,10035), dadid=c(0,0,0,90027,0,0,90027,0,90027,90020,90020,90029,90030,90032), momid=c(0,0,0,90028,0,0,90028,0,90028,90019,90019,90019,90031,90031), sex=c(1,2,1,2,1,1,2,1,2,1,1,2,2,1), avail=(c(rep(0,8),1,1,1,0,1,1)))
aff53108 <- data.frame(schizophrenia=c(rep(NA,8),1,1,1,0,1,1))
id53108 <- paste(fam53108$id, c(rep(" ", 8), "01C08761", "01C08396", "01C08398", " ", "01C08760", "01C08766"), sep="\n")
ped53108 <- pedigree(id=fam53108$id, momid=fam53108$momid, dadid=fam53108$dadid, sex=fam53108$sex, affected=as.matrix(aff53108))
plot(ped53108, col=ifelse(fam53108$avail, 4, 1), id=id53108)

# Family 70-1120

fam701120 <- data.frame(famid=rep(701120, 52), id=c(62083,62084,62003,11715,11760,11770,11769,62056,11779,11809,11810,11816,11817,62072,62077,12044,62013,62097,11714,62012,62094,62095,11723,62096,62099,11735,62098,11901,11902,11780,11811,11835,12286,12320,12045,11733,11737,12243,11719,11722,62088,11731,11721,11720,11763,11903,11904,11853,12295,62090,62092,62093), dadid=c(0,0,0,62083,62083,0,62083,0,62083,62083,0,62083,62083,0,0,62083,0,0,62003,0,0,0,62003,0,0,62003,0,11770,0,62056,11809,0,11817,11817,62077,62013,62097,62012,62094,62095,0,62096,0,62099,62098,11901,11901,11811,11811,11722,11731,11731), momid=c(0,0,0,62084,62084,0,62084,0,62084,62084,0,62084,62084,0,0,62084,0,0,11715,0,0,0,11715,0,0,11715,0,11769,0,11779,11810,0,62072,62072,12044,11714,11714,11714,11723,11723,0,11723,0,11735,11735,11902,11902,11835,11835,62088,62088,11721), sex=c(1,2,1,2,1,1,2,1,2,1,2,1,1,2,1,2,1,1,2,1,1,1,2,1,1,2,1,1,2,1,1,2,1,2,2,2,1,1,2,1,2,1,2,1,2,1,2,1,1,1,1,1), avail=c(rep(0,4),1,rep(0,13),1,rep(0,22),1,0,0,1,rep(0,7)))
aff701120 <- data.frame(schizophrenia=c(NA,NA,NA,0,1,0,0,NA,0,1,0,0,0,NA,NA,1,NA,NA,1,NA,NA,NA,1,NA,NA,1,NA,0,0,1,0,0,1,0,0,0,0,0,0,0,NA,1,1,0,1,0,0,0,0,NA,NA,NA), other_mental_disorder=c(NA,NA,NA,0,0,0,0,NA,0,0,0,1,0,NA,NA,0,NA,NA,0,NA,NA,NA,0,NA,NA,0,NA,1,0,0,0,1,0,1,1,1,1,1,1,0,NA,0,0,1,0,1,1,0,0,NA,NA,NA))
id701120 <- paste(fam701120$id, c(rep(" ", 4), "03C21012", rep(" ", 13), "03C19764", rep(" ", 22),"03C20094", " ", " ", "03C21155", rep(" ", 7)), sep="\n")
ped701120 <- pedigree(id=fam701120$id, momid=fam701120$momid, dadid=fam701120$dadid, sex=fam701120$sex, affected=as.matrix(aff701120))
plot(ped701120, col=ifelse(fam701120$avail, 4, 1), id=id701120)


# Family 70-1096

fam701096 <- data.frame(famid=rep(701096, 28), id=c(56033,56034,10693,11503,11292,11487,56031,11724,56027,11486,11091,11081,56110,11536,11537,11538,11728,11498,11499,11500,11501,11502,11863,11080,11634,11726,11727,56071), dadid=c(0,0,56033,0,56033,56033,0,0,56033,11503,11487,0,0,11487,11487,11487,0,11724,11724,11724,11724,11724,0,11091,56110,11728,11728,11502), momid=c(0,0,56034,0,56034,56034,0,0,56034,11292,56031,0,0,56031,56031,56031,0,56027,56027,56027,56027,56027,0,11081,11536,11498,11498,11863), sex=c(1,2,1,1,2,1,2,1,2,2,1,2,1,2,2,1,1,2,1,2,1,1,2,2,1,2,2,1), avail=c(0,0,1,0,1,rep(0,19),1,0,0,0))
aff701096 <- data.frame(schizophrenia=c(NA,NA,1,0,1,0,NA,NA,NA,0,0,0,NA,0,0,0,NA,0,rep(NA, 5),0,1,0,0,NA), other_mental_disorder=c(NA,NA,0,0,0,1,NA,NA,NA,1,1,1,NA,1,1,0,NA,1,rep(NA,5),1,0,0,0,NA))
id701096 <- paste(fam701096$id, c(" ", " ", "02C10767", " ", "02C10243", rep(" ", 19), "03C14076", rep(" ", 3)), sep="\n")
ped701120 <- pedigree(id=fam701096$id, momid=fam701096$momid, dadid=fam701096$dadid, sex=fam701096$sex, affected=as.matrix(aff701096))
plot(ped701120, col=ifelse(fam701096$avail, 4, 1), id=id701096)


# Family 71-5077

fam715077 <- data.frame(famid=rep(715077,26), id=c(17,18,99,98,10,11,04,03,22,23,80,83,82,88,01,02,07,44,45,52,46,84,85,86,87,47), dadid=c(0,0,0,0,17,0,17,99,99,0,10,0,10,10,04,04,04,22,22,0,22,83,83,83,83,52), momid=c(0,0,0,0,18,0,18,98,98,0,11,0,11,11,03,03,03,23,23,0,23,82,82,82,82,46), sex=c(1,2,1,2,1,2,1,2,1,2,2,1,2,2,1,1,2,2,1,1,2,1,1,1,1,2), avail=c(rep(0,14),1,1,rep(0,6),1,rep(0,3)))
aff715077 <- data.frame(schizophrenia=c(rep(NA,7),0,NA,NA,0,0,NA,0,1,1,0,0,0,NA,0,0,1,0,0,0), other_mental_disorder=c(rep(NA,7),0,NA,NA,0,0,NA,0,0,0,1,0,0,NA,1,0,0,0,1,0))
id715077 <- paste(fam715077$id, c(rep(" ",14), "03C13990", "03C17802",rep(" ", 6),"03C15391",rep(" ", 3)), sep="\n")
ped715077 <- pedigree(id=fam715077$id, momid=fam715077$momid, dadid=fam715077$dadid, sex=fam715077$sex, affected=as.matrix(aff715077))
plot(ped715077, col=ifelse(fam715077$avail, 4, 1), id=id715077)


# Family 70-1179

fam701179 <- data.frame(famid=rep(701179,28), id=c(70008,70009,12540,70005,12257,70012,70011,70017,70016,12255,70007,12256,12261,12541,12542,70024,70025,70027,12567,12566,12273,12546,12560,12680,12565,12569,12559,12568), dadid=c(0,0,70008,70008,0,0,70008,0,70008,70005,0,70005,70005,70012,0,70012,0,0,70017,0,70007,12541,70024,70027,12567,0,12567,12569), momid=c(0,0,70009,70009,0,0,70009,0,70009,12257,0,12257,12257,70011,0,70011,0,0,70016,0,12256,12542,70025,70025,12566,0,12566,12559), sex=c(1,2,1,1,2,1,2,1,2,1,1,2,2,1,2,1,2,1,1,2,2,2,2,2,1,1,2,1), avail=c(rep(0,9),1,0,1,rep(0,12),1,0,0,0))
aff701179 <- data.frame(schizophrenia=c(rep(NA,4),0,rep(NA,4),1,NA,1,0,0,0,NA,NA,NA,rep(0,5),1,1,0,0,0), other_mental_disorder=c(rep(NA,4),0,rep(NA,4),0,NA,0,1,0,0,NA,NA,NA,1,0,1,1,1,0,0,1,0,0))
id701179 <- paste(fam701179$id, c(rep(" ",9)," 04C37692"," ", "04C37691", rep(" ", 12), "05C50396", rep(" ", 3)), sep="\n")
ped701179 <- pedigree(id=fam701179$id, momid=fam701179$momid, dadid=fam701179$dadid, sex=fam701179$sex, affected=as.matrix(aff701179))
plot(ped701179, col=ifelse(fam701179$avail, 4, 1), id=id701179)

# Family 45-1040

fam451040 <- data.frame(famid=rep(451040,13), id=c(12,13,7,6,9,8,10,11,1,2,3,4,5), dadid=c(0,0,0,12,0,12,12,0,7,7,9,9,10), momid=c(0,0,0,13,0,13,13,0,6,6,8,8,11), sex=c(1,2,1,2,1,2,1,2,1,1,1,1,1), avail=c(rep(0,8),rep(1,5)))
aff451040 <- data.frame(schizophrenia=c(rep(0,8),rep(1,5)))
id451040 <- paste(fam451040$id, c(rep(" ",8),"03C14989", "03C14865", "03C14829", "03C14816", "03C14821"), sep="\n")
ped451040 <- pedigree(id=fam451040$id, momid=fam451040$momid, dadid=fam451040$dadid, sex=fam451040$sex, affected=as.matrix(aff451040))
plot(ped451040, col=ifelse(fam451040$avail, 4, 1), id=id451040)

# Family 44-1053

fam441053 <- data.frame(famid=rep(441053,12), id=c(611,610,011,010,999,511,510,001,002,003,005,004), dadid=c(0,0,0,611,0,0,611,011,011,999,999,511), momid=c(0,0,0,610,0,0,610,010,010,010,010,510), sex=c(1,2,1,2,1,1,2,1,2,2,2,1), avail=c(rep(0,7),1,1,0,0,1), status=c(1,0,1,1,0,1,1,0,0,0,0,0))
aff441053 <- data.frame(schizophrenia=c(NA,NA,0,NA,NA,0,0,1,0,0,NA,1), other_mental_disorder=c(NA,NA,0,NA,NA,0,0,0,1,0,NA,0))
id441053 <- paste(fam441053$id, c(rep(" ", 7)," 01C09383", "01C09388"," ", " ", "02C09539"))
ped441053 <- pedigree(id=fam441053$id, momid=fam441053$momid, dadid=fam441053$dadid, sex=fam441053$sex, affected=as.matrix(aff441053), status=fam441053$status)
plot(ped441053, col=ifelse(fam441053$avail, 4, 1), id=id441053)

# Family 70-1088

fam701088 <- data.frame(famid=rep(701088,35), id=c(55021,55020,55029,55028,55024,55023,55030,11581,55022,55037,55025,55026,11481,11480,55027,11152,11645,11643,11605,11603,11604,11606,11607,12138,12139,11482,11483,11484,11151,11597,11631,11644,11608,12140,12141), dadid=c(0,0,0,0,0,55021,0,55021,55021,0,55021,0,55029,0,55029,55024,0,55024,55030,55022,0,55025,55025,55025,0,11481,11481,11481,55027,11645,11645,11645,11603,12138,12138), momid=c(0,0,0,0,0,55020,0,55020,55020,0,55020,0,55028,0,55028,55023,0,55023,11581,55037,0,55026,55026,55026,0,11480,11480,11480,11152,11643,11643,11643,11604,12139,12139), sex=c(1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,2,1,2,1,1,1,2,1,1,2,1,2,2,2,2,2,2), avail=c(rep(0,21),1,1,rep(0,5),1,rep(0,6)), status=c(rep(0,15),1,rep(0,19)))
aff701088 <- data.frame(schizophrenia=c(rep(NA,7),1,rep(NA,4),0,1,NA,1,NA,0,0,0,0,1,1,0,0,1,0,0,1,0,0,0,NA,0,0), other_mental_disorder=c(rep(NA,7),1,rep(NA,4),0,0,NA,0,NA,0,1,1,1,0,0,0,1,0,1,1,0,1,0,1,NA,1,1))
id701088 <- paste(fam701088$id, c(rep(" ",21),"02C12825","02C12826",rep(" ",5),"02C11048",rep(" ",6)), sep="\n")
ped701088 <- pedigree(id=fam701088$id, momid=fam701088$momid, dadid=fam701088$dadid, sex=fam701088$sex, affected=as.matrix(aff701088), status=fam701088$status)
plot(ped701088, col=ifelse(fam701088$avail, 4, 1), id=id701088)

# Family 49-1002

fam491002 <- data.frame(famid=rep(491002,9), id=c(4,3,1,2,5,27,6,17,18), dadid=c(0,0,4,4,4,0,4,27,27), momid=c(0,0,3,3,3,0,3,6,6), sex=c(1,2,1,2,2,1,2,1,2), avail=c(0,0,0,0,1,0,0,1,1), status=c(1,rep(0,8)))
aff491002 <- data.frame(schizophrenia=c(0,0,1,1,1,0,0,1,1), other_mental_disorder=c(0,0,0,0,0,1,1,0,0))
id491002 <- paste(fam491002$id, c(rep(" ",4),"00C01822"," ", " ","00C01656", "00C01655"), sep="\n")
relate491002 <- matrix(c(2, 5, 2), nrow=1, byrow=TRUE)
ped491002 <- pedigree(id=fam491002$id, momid=fam491002$momid, dadid=fam491002$dadid, sex=fam491002$sex, affected=as.matrix(aff491002), status=fam491002$status, relation=relate491002)
plot(ped491002, col=ifelse(fam491002$avail, 4, 1), id=id491002)


