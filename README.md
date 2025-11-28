Projeto desenvolvido durante o curso Estatística para Geoquímica Ambiental, realizado na 1° Semana Acadêmica da Pós-Graduação em Geociências. O objetivo da prática foi aplicar métodos estatísticos a um conjunto de 30 amostras ambientais, avaliando diferenças entre áreas (Controle vs Impactada) e tipos de local (Rio, Lago e Solo). Durante o curso, foram exploradas técnicas estatísticas aplicadas à geoquímica ambiental, utilizando dados para treinar etapas fundamentais da análise: estatística descritiva, testes de normalidade, comparações entre grupos e correlações entre variáveis.

Este projeto apresenta uma análise estatística de um conjunto de dados geoquímicos composto por 30 amostras de água e solo, distribuídas entre:

- 2 áreas: Controle e Impactada
- 3 tipos de local: Rio, Lago e Solo

Os parâmetros analisados incluem:

- pH
- Condutividade
- Matéria Orgânica (%)
- Metais Pesados (mg/kg)

O objetivo é avaliar padrões ambientais, possíveis impactos e relações entre variáveis, utilizando técnicas estatísticas univariadas e multivariadas.

O projeto responde às seguintes questões:

1. Média, mediana e desvio padrão do pH por grupo de área
2. Detecção de outliers em condutividade e metais pesados
3. Teste de normalidade (Shapiro-Wilk) para todas as variáveis
4. Comparação de pH entre áreas controle e impactada
5. Diferença de matéria orgânica entre rio, lago e solo
6. Correlação entre pH e matéria orgânica
7. Diferença de condutividade entre áreas
8. Diferença de metais pesados entre locais
9. Correlação entre metais pesados e condutividade

Visualizações:

- Gráfico de colunas da média de matéria orgânica por local
- Boxplot de metais pesados por local
- Gráfico de linhas de condutividade por local

🛠️ Ferramentas Utilizadas

- R
- tidyverse
- ggplot2
- dplyr
- stats

As etapas do projeto incluem:

- Limpeza e organização dos dados
 Estatísticas descritivas
- Testes de normalidade
- Shapiro-Wilk
- Testes de hipótese
- t-test
- ANOVA
- Análise de outliers
- Boxplot e valores extremos
- Correlação
- Pearson ou Spearman, dependendo da normalidade
- Construção de gráficos

Interpretação ambiental dos resultados

O pH apresentou valores médios semelhantes entre:
Controle: média = 7.00  
Impactada: média = 6.88  

Testes de normalidade
- pH → Normal
- Condutividade → Não normal
- Matéria Orgânica → Normal
- Metais pesados → Não normal
  
Testes comparativos
- pH (Controle vs Impactada): t-test → sem diferença significativa
- Condutividade (Controle vs Impactada): Wilcoxon → sem diferença significativa
- Matéria Orgânica (entre Rio/Lago/Solo): ANOVA → não significativo
- Metais Pesados (entre Rio/Lago/Solo): Kruskal-Wallis → não significativo

Correlações

- pH × Matéria Orgânica → fraca, não significativa
- Condutividade × Metais Pesados → fraca, não significativa

Conclusão

O conjunto analisado não apresentou diferenças significativas entre grupos ou locais para nenhum dos parâmetros, 
sugerindo que os ambientes avaliados possuem condições semelhantes nos indicadores físico-químicos e geoquímicos considerados.

Autora
Camila Mariana – Graduação em Oceanografia (UERJ)
