# 📊 Análise de Internações por AVC (2018–2019)

Este repositório contém a análise de internações por Acidente Vascular Cerebral (AVC) a partir do banco **Analítico AVC (Excel local, 2018–2019)**.  
O pipeline executa limpeza, anonimização mínima (remoção de PII), padronização de variáveis, extração de comorbidades e geração de estatísticas, gráficos e relatórios automáticos.

---

## ⚙️ Metodologia resumida
1. **Leitura & limpeza:** detecção do cabeçalho no Excel, exclusão de colunas/linhas vazias.  
2. **Anonimização:** remoção de identificadores (`paciente`, `rg_paciente`) e criação de `id_paciente`.  
3. **Datas & filtro:** padronização de datas e restrição da janela **2018–2019**.  
4. **Variáveis clínicas:** normalização de `sexo`, `óbito` e métricas contínuas (`idade`, `NIHSS`, `porta-agulha` etc.).  
5. **Comorbidades:** flags heurísticas (HAS, DM, FA, DPOC, Tabagismo, AVC prévio, Coronariopatia/IAM).  
6. **Análises:** boxplots, correlações (phi & Spearman), série temporal mensal e histogramas.  
7. **Relatórios:** geração automática de **INSIGHTS.md** (texto) e **insights.json** (métricas básicas).  

---

## 📌 Insights principais (2018–2019)

- **Volume:** 407 internações únicas no período.  
- **Série temporal:** tendência estável ao longo de 2018–2019 (variações mensais observadas).  
- **Sexo:** distribuição equilibrada entre Masculino e Feminino.  
- **Idade:** mediana em torno de 65 anos (P25 ≈ 57, P75 ≈ 74).  
- **Gravidade (NIHSS):** mediana próxima de 8 pontos na admissão.  
- **Comorbidades mais prevalentes:** HAS, DM e Tabagismo.  
- **Coocorrências fortes:** HAS–DM e HAS–FA apresentaram associação.  
- **Correlação (Spearman):** maior carga de comorbidades associou-se a maior idade e NIHSS de entrada.  
- **Qualidade de processo (porta-agulha):** mediana ≈ 65 min; ~48% dos casos receberam ≤ 60 min.  

---

## 📂 Estrutura das saídas

- `figs/INSIGHTS.md` → relatório textual completo  
- `figs/insights.json` → resumo estruturado em JSON  
- `figs/*.png` → figuras exportadas automaticamente

### 🔎 Figuras principais
- **Idade por sexo:** `boxplot_idade_sexo.png`  
- **NIHSS por sexo:** `boxplot_nihss_sexo.png`  
- **Porta-agulha × comorbidade prevalente:** `boxplot_porta_agulha_comorb.png`  
- **Correlação entre comorbidades:** `correlacao_comorbidades.png`  
- **Spearman (contínuas vs comorbidades):** `correlacao_spearman_cont.png`  
- **Série temporal mensal:** `serie_mensal_2018_2019.png`  
- **Histogramas operacionais:** `metricas_operacionais_*.png`  

---

## 🚀 Como reproduzir
1. Coloque `Analitico_AVC.xlsx` na pasta `workspace`.  
2. Execute o script em R (`analise_avc_datasus.R`) **ou** a versão em Python (`analise_avc.py`).  
3. As figuras e relatórios serão exportados automaticamente em `/figs`.  
4. Explore os resultados em `INSIGHTS.md` e navegue pelas imagens.

---

✍️ *Autor: Gregório Platero Canton*  
