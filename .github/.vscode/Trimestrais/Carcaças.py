
import sidrapy
import pandas as pd
import numpy as np

# Realizando a chamada SIDRA com parâmetros equivalentes
data = sidrapy.get_table(
    table_code="1092",
    territorial_level="1",
    ibge_territorial_code="all",
    variables=["284", "285"],
    classifications={"12716": "115236", "18": "55,56,111734,111735", "12529": "118225"},
    period="all", format="pandas"
)

data.columns = data.iloc[0] # Substitui as colunas pela primeira observação
data = data[1:] # Remove a linha que virou cabeçalho dos dados


# Pipeline com seleção, filtragem, renomeação, transformação e cálculo
df_wide = (
    data[
        ['Trimestre (Código)', 'Trimestre', 'Tipo de rebanho bovino', 'Variável', 'Valor']  # Seleção direta de colunas
    ]
    .query("Variável != 'Número de informantes'")  # Filtrando dados
    .assign(Variável=lambda df: df['Variável'].replace({
        'Animais abatidos': 'Abate',
        'Peso total das carcaças': 'Carcaças'
    }))  # Renomeando valores na coluna 'Variável'
    .rename(columns={
        'Tipo de rebanho bovino': 'Rebanho',
        'Trimestre (Código)': 'Data'
    })  # Renomeando as colunas
    .pivot(index=['Data', 'Trimestre', 'Rebanho'], columns='Variável', values='Valor')  # Transformando para formato largo
    .reset_index()  # Resetando o índice após a pivotagem
    .assign(
        Carcaças=lambda df: pd.to_numeric(df['Carcaças'], errors='coerce'),
        Abate=lambda df: pd.to_numeric(df['Abate'], errors='coerce')
    )  # Convertendo as colunas para valores numéricos
    .assign(Carcasas_por_Abate=lambda df: df['Carcaças'] / df['Abate'])  # Calculando a nova variável
).round(2)

# Exibindo o resultado
df_wide.info()
df_wide['Rebanho'].unique()
df_wide.count()
df_wide.nsmallest(5, 'Carcasas_por_Abate') # retornando as 5 observações com o menor valor.
df_wide.nlargest(5, 'Carcasas_por_Abate') # retornando as 5 observações com o maior valor.
df_wide.isna().any() # retorna as colunas onde contém valores nulos.
df_wide.describe().round(2)

df_wide['Rebanho'].unique()