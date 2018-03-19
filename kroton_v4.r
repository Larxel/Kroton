library(Rcpp)
library(dplyr)
library(data.table)
library(ggplot2)
library(ggthemes)
library(markovchain)
library(rowr)
setwd("~/Documents/Kaggle Ubuntu/Kroton/Case Análise Acadêmica")

# [] Read Data
raw.data = fread("raw_data.csv", encoding = 'Latin-1')
colSums(is.na(raw.data))

# [] Create auxiliar function to get cardinality
cardinality = function(x){
  (length(unique(x)))
}

apply(raw.data, 2,cardinality)


# [] Check Proportions
round(prop.table(table(raw.data$DESEMPENHO_TURMA_PROVA, raw.data$TURNO)),2)




# [] View target variable distribution
pallete = scale_fill_manual(values=c("#DAD2D8","#143642","#0F8B8D","#EC9A29","#A8201A"))
ggplot(raw.data, aes(x=TURNO, y=DESEMPENHO_TURMA_PROVA, fill=TURNO))+
  geom_boxplot(alpha=0.8, width=0.5)+
  ggtitle("Boxplots Desempenho vs Turno")+
  pallete+
  xlab("Turno")+
  ylab("Desempenho na Prova")




# [] Create Auxiliar Features
raw.data$semestre_final    = as.numeric(substr(raw.data$SEMESTRE_FORMATURA,1,4)) + ( as.numeric(substr(raw.data$SEMESTRE_FORMATURA,6,6)) - 1  ) * 0.5
raw.data$semestre_ingresso = raw.data$semestre_final - raw.data$SEMESTRE_ATUAL*0.5
raw.data$percentual_conclusao = raw.data$SEMESTRE_ATUAL / raw.data$N_SEMESTRES


# [] Aggregate all data
all.markov = list()

failsafe = raw.data
raw.data = raw.data %>% filter(!is.na(SEMESTRE_ATUAL))

aggregate.data = raw.data %>% group_by(COD_UNIDADE, DESC_CURSO, TURNO) %>% summarize(count = n()) %>% as.data.frame()
aggregate.data[,2] = gsub("[[:punct:]]", "", aggregate.data[,2])

aggregate.data.alunos = raw.data %>%
  group_by(COD_UNIDADE, DESC_CURSO, TURNO) %>%
  summarize(qtd_alunos = mean(QTD_ALUNOS), nota_media = mean(DESEMPENHO_TURMA_PROVA), max_nota = max(DESEMPENHO_TURMA_PROVA)) %>%
  as.data.frame()



# [] Compute all Markov Chains
for (i in 1:nrow(aggregate.data)){
  cat(paste0(i,"/",nrow(aggregate.data)))
  current.unit = filter(raw.data, COD_UNIDADE == aggregate.data[i,1] & DESC_CURSO == aggregate.data[i,2] & TURNO == aggregate.data[i,3])
  matrix.name  = paste0("Unidade_",aggregate.data[i,1], "_Curso_", aggregate.data[i,2], "_Turno_", aggregate.data[i,3] )
  
  if (nrow(current.unit) == 0){
    next
  }
  
  # [] Yeah boy
  for (k in 1:cardinality(current.unit$SEMESTRE_ATUAL)){
    preliminar.df = filter(current.unit, SEMESTRE_ATUAL == unique(current.unit$SEMESTRE_ATUAL)[k]) %>% select(DESEMPENHO_TURMA_PROVA)
    
    
    if (k == 1){
      current.matrix = preliminar.df
      colnames(current.matrix)[1] = paste0("Semestre_",unique(current.unit$SEMESTRE_ATUAL)[k])
    } else {
      current.matrix = cbind.fill(current.matrix, preliminar.df, fill = NA)
      colnames(current.matrix)[k] = paste0("Semestre_",unique(current.unit$SEMESTRE_ATUAL)[k])
    }
    
  }
  
  
  # [] Save markov chain
  markov = markovchainFit(data = current.matrix)
  all.markov[[i]] = markov
  
  # [] Save plot
  filename = paste0("plot_",matrix.name)
  png(filename, width = 800, height = 800)
  plot(markov$estimate, main=paste0("Markov Chain of ",matrix.name))
  dev.off()
  
  
}

# [] Save output to disk
save(all.markov, file="all_markov.RData")

load("all_markov.RData")

# [] Create function to score transition matrix
ponderate = function(x){
  for (a in 1:nrow(x)){
    for (b in 1:ncol(x)){
      x[a,b] = ifelse((b-a == 0),x[a,b], (b-a) * x[a,b])
    }
  }
  return(x)
}


ponderated.markov = data.frame(Name = NA, UniqueVal = NA)
aggregate.data$score = NA

# [] Apply scoring
for (p in 1:length(all.markov)){
  print(paste0(p,"/",length(all.markov)))
  
  if (all.markov[[p]]$estimate %>% is.null()){
    next
  }
  
  temp = all.markov[[p]]$estimate@transitionMatrix
  temp = ponderate(temp)
  
  aggregate.data$score[p] = sum(colSums(abs(temp)))
}

# [] Join results
final.score = filter(aggregate.data, score > 0 & !is.na(score)) %>% arrange(desc(score))
final.score = left_join(final.score, aggregate.data.alunos, by=c("COD_UNIDADE","DESC_CURSO","TURNO"))
final.score$final_score = final.score$score * final.score$qtd_alunos

final.score = final.score %>% arrange(desc(final_score))
final.score$final_score2 = final.score$final_score ^2

final.score$investment_proportion = (final.score$final_score)/sum((final.score$final_score))
final.score$investment = final.score$investment_proportion * 1000000



glimpse(final.score)
fwrite(final.score,"final_score.csv")


# [] Plot score distribution
ggplot(final.score, aes(x=final_score))+
  geom_density(color="white",fill="deepskyblue3",alpha=0.7)+
  theme_hc(bgcolor="darkunica")+
  ggtitle("Distribuição do Score")

ggplot(final.score, aes(x=final_score))+
  geom_histogram(bins=20,color="white",fill="deepskyblue3",alpha=0.7)+
  theme_hc(bgcolor="darkunica")+
  ggtitle("Distribuição do Score")


# [] ROI Calculation
sum(final.score$qtd_alunos * final.score$nota_media)/sum(final.score$qtd_alunos)

roi_100 = sum(final.score$qtd_alunos * (final.score$nota_media + 1.00*(final.score$max_nota - final.score$nota_media)))/sum(final.score$qtd_alunos)
roi_75  = sum(final.score$qtd_alunos * (final.score$nota_media + 0.75*(final.score$max_nota - final.score$nota_media)))/sum(final.score$qtd_alunos)
roi_50  = sum(final.score$qtd_alunos * (final.score$nota_media + 0.50*(final.score$max_nota - final.score$nota_media)))/sum(final.score$qtd_alunos)
roi_25  = sum(final.score$qtd_alunos * (final.score$nota_media + 0.25*(final.score$max_nota - final.score$nota_media)))/sum(final.score$qtd_alunos)

# [] Organize ROI info
roi.df = data.frame(name = c("ROI 100%", "ROI 75%", "ROI 50%", "ROI 25%"),
                    value = c(roi_100,roi_75,roi_50,roi_25))



final.score$final_nota_100 = final.score$qtd_alunos * (final.score$nota_media + 1.00*(final.score$max_nota - final.score$nota_media))/final.score$qtd_alunos
final.score$final_nota_75  = final.score$qtd_alunos * (final.score$nota_media + 0.75*(final.score$max_nota - final.score$nota_media))/final.score$qtd_alunos
final.score$final_nota_50  = final.score$qtd_alunos * (final.score$nota_media + 0.50*(final.score$max_nota - final.score$nota_media))/final.score$qtd_alunos
final.score$final_nota_25  = final.score$qtd_alunos * (final.score$nota_media + 0.25*(final.score$max_nota - final.score$nota_media))/final.score$qtd_alunos

# [] Create IDs to allow join
roi.aggregate    = aggregate.data.alunos
final.score$id   = paste0(final.score$COD_UNIDADE, final.score$DESC_CURSO, final.score$TURNO)
roi.aggregate$id = paste0(roi.aggregate$COD_UNIDADE, roi.aggregate$DESC_CURSO, roi.aggregate$TURNO)

roi.aggregate = left_join(roi.aggregate, final.score, by="id")


# [] Calculate ROI per percentage
roi.aggregate$roi_100 = NA
roi.aggregate$roi_75  = NA
roi.aggregate$roi_50  = NA
roi.aggregate$roi_25  = NA

for (i in 1:nrow(roi.aggregate)){
  roi.aggregate$roi_100[i] = max(roi.aggregate$final_nota_100[i], roi.aggregate$nota_media.x[i], na.rm=TRUE)
  roi.aggregate$roi_75[i]  = max(roi.aggregate$final_nota_75[i],  roi.aggregate$nota_media.x[i], na.rm=TRUE)
  roi.aggregate$roi_50[i]  = max(roi.aggregate$final_nota_50[i],  roi.aggregate$nota_media.x[i], na.rm=TRUE)
  roi.aggregate$roi_25[i]  = max(roi.aggregate$final_nota_25[i],  roi.aggregate$nota_media.x[i], na.rm=TRUE)
}


# [] Organize ROI info
roi.df = data.frame(name = factor(c("ROI 100%", "ROI 75%", "ROI 50%", "ROI 25%"),levels=c("ROI 100%", "ROI 75%", "ROI 50%", "ROI 25%")),
                    value = c(mean(roi.aggregate$roi_100),mean(roi.aggregate$roi_75),mean(roi.aggregate$roi_50),mean(roi.aggregate$roi_25)))


# [] Plot Roi
ggplot(roi.df, aes(x=name,y=value))+
  geom_bar(stat="identity",width=0.7,fill="springgreen",alpha=0.7)+
  theme_hc(bgcolor="darkunica")+
  geom_text(aes(label=round(value,2)), position=position_dodge(width=0.9), vjust=-0.25,color="white")+
  ggtitle("Cenários de ROI")


fwrite(roi.df,"roi_df.csv")


#top25.df = filter(final.score, score > 9841)
#top25.df$score = sqrt(top25.df$score)
#top25.df$investment_proportion = top25.df$score / sum(top25.df$score)
#top25.df$investment = top25.df$investment_proportion * 1000000
#top100.df = top25.df[1:100,]
#top100.df$investment_proportion = top100.df$score / sum(top100.df$score)
#top100.df$investment = top100.df$investment_proportion * 1000000
#fwrite(top100.df, "top100.csv")






