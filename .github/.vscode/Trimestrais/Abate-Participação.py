import requests
import pandas as pd

# URL da API
url = "https://apisidra.ibge.gov.br/values/t/1092/n1/all/v/284,285/p/all/c12716/115236/c18/55,56,111734,111735/c12529/118225"

# Requisição GET
response = requests.get(url)

# Verificando o status da requisição
if response.status_code == 200:
    # Convertendo para JSON
    data_json = response.json()
    
    # O primeiro item contém os nomes das colunas
    colunas = list(data_json[0].values())

    # Criando lista de dicionários a partir dos dados (ignorando o primeiro item)
    data_list = [dict(zip(colunas, item.values())) for item in data_json[1:]]

    # Criando DataFrame
    df = pd.DataFrame(data_list)

    # Convertendo valores numéricos corretamente (se a coluna existir)
    if "Valor" in df.columns:  # Confirme o nome correto no JSON
        df["Valor"] = pd.to_numeric(df["Valor"], errors="coerce")

    # Exibir as 5 primeiras linhas do DataFrame
    print(df.head())

else:
    print(f"Erro na requisição: {response.status_code}")


# Analise descritiva e visualização inicial
df.info()
df.describe()


df1 = df[[
    "Trimestre (Código)","Trimestre", "Unidade de Medida", "Variável", "Tipo de rebanho bovino", "Valor"]]

df1["Ano"] = df1["Trimestre (Código)"].astype(str).str[:4]
#df1["Ano"].fillna(df1["Trimestre"].str.extract(r'(\d{4})'), inplace=True)


df1["Ano"] = df1["Ano"].astype(int)

abate_anual = (
    df1.groupby(["Ano", "Tipo de rebanho bovino"], as_index=False)["Valor"].sum()
    .merge(
        df1.groupby("Ano", as_index=False)["Valor"].sum()
        .rename(columns={"Valor": "Total_Abate_Ano"}),
        on="Ano"
    )
    .assign(Percentual=lambda x: ((x["Valor"] / x["Total_Abate_Ano"]) * 100).round(2))
)



# gráfico interativo

# Importar bibliotecas
import plotly.express as px
import plotly.graph_objects as go
import seaborn as sns

# Criar gráfico interativo
fig = px.line(
    abate_anual,
    x="Ano",
    y="Percentual",
    color="Tipo de rebanho bovino",
    markers=True,
    title="Percentual de Abate por Tipo de Rebanho ao Longo dos Anos"
)

# Aplicar a paleta inferno
inferno_colors = sns.color_palette("inferno", n_colors=len(abate_anual["Tipo de rebanho bovino"].unique())).as_hex()

# Atualizar cores das linhas
for i, trace in enumerate(fig.data):
    trace.update(line=dict(color=inferno_colors[i % len(inferno_colors)]))

# Personalização do layout
fig.update_layout(
    title=dict(
        text="Percentual de Abate por Tipo de Rebanho ao Longo dos Anos<br><sup>Dados: IBGE | Elaboração: @italo.m.m</sup>",
        font=dict(size=26, family="Josefin Sans", color="#b3b3b3")  # Cor clara para melhor contraste
    ),
    xaxis=dict(
        title="Ano",
        titlefont=dict(size=22, family="Josefin Sans", color="#b3b3b3"),
        tickfont=dict(size=16, family="Josefin Sans", color="#b3b3b3"),
        showgrid=True,  # Exibir grade
        gridcolor="rgba(255,255,255,0.3)",  # Branco com 30% de transparência
        gridwidth=1, 
        griddash="dot", 
        zeroline=True,  # Linha zero
        zerolinewidth=1,
        zerolinecolor="rgba(255,255,255,0.5)"
    ),
    yaxis=dict(
        title="Percentual (%)",
        titlefont=dict(size=22, family="Josefin Sans", color="#b3b3b3"),
        tickfont=dict(size=16, family="Josefin Sans", color="#b3b3b3"),
        showgrid=True,  
        gridcolor="rgba(255,255,255,0.3)",  
        gridwidth=1,
        griddash="dot", 
        zeroline=True,
        zerolinewidth=1,
        zerolinecolor="rgba(255,255,255,0.5)"
    ),
    legend=dict(
        font=dict(size=16, family="Josefin Sans", color="#b3b3b3")
    ),
    plot_bgcolor="#505050",  # Cinza médio para o fundo do gráfico
    paper_bgcolor="#505050"  # Cinza escuro para o fundo geral
)

# Salvar como HTML
fig.write_html(".github/.vscode/Trimestrais/percentual_abate.html")

# Exibir o gráfico
fig.show()
