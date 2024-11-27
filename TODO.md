---
title: "QUESTION: Is the prevalence of mental disorders in the Brazilian population caused by social inequality?"
output: html_document
---

# TO-DO

-   [ ] Remake table per country, not a time series - REMOVE CONTINENTS
-   [ ] Split the data by decade, treating each decade as a replica per country
    -   [ ] Fazer a média por década - usar a função eta?
    -   [ ] Fazer teste de normalidade nos dados das décadas (gráfico)
-   [ ] Em cada país, a gente vai medir a causalidade entre uma métrica de desigualdade (Gini) e a incidência de transtornos mentais
    -   Usar todos os transtornos ou selecionar alguns?
    -   Se a gente usar todos os transtornos, podemos avaliar também a causalidade entre predisposição genética e transtornos, porque esperamos que populações diferentes tenham prevalência diferente de transtornos mentais (e também esperaremos que populações mais homogêneas tenham uma relação de causalidade mais forte entre predisposição genética e transtornos mentais)
        - Migração afeta a predisposição genética em países menores -> viés
-   [ ] Refazer DAG
-   [ ] DHARMa para avaliar a autocorrelação temporal entre os dados - o DHARMa permite fazer análises de autocorrelação temporal usando os resíduos

# HIPÓTESES

- Esquizofrenia e bipolar disorder tenham uma relação causal mais forte with herdabilidade
- A nível populacional, schizo and bipolar têm efeito causal sobre depressão e ansiedade
- Depressão e ansiedade têm efeito causal sobre transtornos alimentares
- Depressão se retroalimenta ao longo das décadas (não sei se vamos conseguir avaliar)
- Depressão causa ansiedade nas décadas seguintes (não sei se vamos conseguir avaliar)
- Drogadição é uma variável oculta (não está medindo)
    - Esquizofrenia, depressão, transtornos alimentares, transtorno bipolar, ansiedade causam drogadição
    - Drogadição pode causar esquizofrenia nas gerações posteriores (gravidez)
    - Drogadição causa depressão, ansiedade
- Depressão e ansiedade se retroalimentam
- Criminalidade é uma variável oculta que pode causar ansiedade e depressão a nível população

# OLD PROJECT

-   [ ] Look for data for missing years (1990, 1992:1999) - these data don't exist, not even in projections. Should we:
    -   [ ] use populational models considering the fecundity and mortality rates from 1991 to project the population between 1990 and 1999?
    -   [ ] repeat data from 1991 throughout the decade?
-   [ ] Pensar nos modelos que a gente vai usar pra modelar cada uma das doenças
-   [ ] Pensar nas nossas hipóteses para a relação entre o Gini e cada doença
    -   **Hipótese Meari:** esquizofrenia e transtorno bipolar terão uma correlação mais fraca com o índice de Gini, porque estão mais fortemente correlacionadas à herdabilidade:
        -   [Genetic predisposition to schizophrenia: what did we learn and what does the future hold?](https://www.researchgate.net/profile/Karoly-Mirnics/publication/51902468_Vereczkei_A_Mirnics_K_Genetic_predisposition_to_schizophrenia_what_did_we_learn_and_what_does_the_future_hold_Neuropsychopharmacol_Hung_13_205-210/links/0fcfd505893f028661000000/Vereczkei-A-Mirnics-K-Genetic-predisposition-to-schizophrenia-what-did-we-learn-and-what-does-the-future-hold-Neuropsychopharmacol-Hung-13-205-210.pdf)
        -   [The genetics of bipolar disorder](https://www.nature.com/articles/s41380-019-0634-7)
        -   [Genetic risk factors for eating disorders: an update and insights into pathophysiology](https://journals.sagepub.com/doi/full/10.1177/2045125318814734)
        -   [What is the genetic relationship between anxiety and depression?](https://onlinelibrary.wiley.com/doi/full/10.1002/ajmg.c.30171) - I thought this one was kind of representative of the search I did on the topic as in the references are controversial; but I'll keep looking
-   [ ] Pensar nas nossas hipóteses sobre como cada doença pode estar correlacionada
    -   **Hipóteses Meari:**
        -   **Ansiedade x depressão:** muito correlacionadas entre si e também com momentos de grande ansiedade social e principalmente financeira
        -   **Ansiedade x ED:** fortemente correlacionadas (e isso é uma intuição *anedótica* que eu tenho por ter TAG e ter tido anorexia)

# DONE

-   [x] Download demographic data (2022)
-   [x] Download Gini coefficient data
-   [x] Download mental disorder data
-   [x] Combinar os dados baixados em uma única tabela de dados para análise
