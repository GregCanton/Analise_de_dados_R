📊 Análise de Internações por AVC (2018–2019)

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
- **Idade por sexo:**
<img width="1080" height="600" alt="boxplot_idade_sexo" src="https://github.com/user-attachments/assets/50b2e1e5-e7c2-45f8-89f7-6830f8b1eac7" />
- **NIHSS por sexo:**
<img width="1080" height="600" alt="boxplot_nihss_sexo" src="https://github.com/user-attachments/assets/55386033-b357-4afb-a928-1436b3d1bdb6" />
- **Porta-agulha × comorbidade prevalente:**
<img width="1080" height="600" alt="boxplot_porta_agulha_comorb" src="https://github.com/user-attachments/assets/fa8ad617-4747-4484-96d5-fcc57ca87db0" />
- **Correlação entre comorbidades:**
 <img width="1080" height="600" alt="correlacao_comorbidades" src="https://github.com/user-attachments/assets/d6d66998-b1d2-4b8f-8e8b-80a2f681238a" />
- **Spearman (contínuas vs comorbidades):**
<img width="1080" height="600" alt="correlacao_spearman_cont" src="https://github.com/user-attachments/assets/2a9b7d74-8bf4-4580-86e0-c99bc9c72dcc" />
- **Série temporal mensal:**
 <img width="1080" height="600" alt="serie_mensal_2018_2019" src="https://github.com/user-attachments/assets/f3d9b596-dc68-4e21-804b-3ce4dbe590af" />
- **Histogramas operacionais:**
 <img width="1080" height="600" alt="metricas_operacionais" src="https://github.com/user-attachments/assets/20f00a40-2195-464e-ad00-2fbec458c1c0" />


---

## 🚀 Como reproduzir
1. Coloque `Analitico_AVC.xlsx` na pasta `workspace`.  
2. Execute o script em R (`analise_avc_datasus.R`) **ou** a versão em Python (`analise_avc.py`).  
3. As figuras e relatórios serão exportados automaticamente em `/figs`.  
4. Explore os resultados em `INSIGHTS.md` e navegue pelas imagens.

---

✍️ *Autor: Gregório Platero Canton*  


