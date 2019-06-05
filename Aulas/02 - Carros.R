getwd()
setwd('/home/jaapis/Documents/GitHub/RStudio/EDA_Course_Materials/EDA_Course_Materials/lesson2')

# Leitura de CSV
statesInfo <- read.csv('stateData.csv')

# Chama um subconjunto do dataframe com um parâmetro especificado
subset(statesInfo, state.region == 1)

stateSubset <- statesInfo[statesInfo$state.region == 1, ]
head(stateSubset, 2)
dim(stateSubset)

stateSubsetBracket <- statesInfo[statesInfo$state.region ==1, ]
head(stateSubsetBracket)
dim(stateSubsetBracket)

