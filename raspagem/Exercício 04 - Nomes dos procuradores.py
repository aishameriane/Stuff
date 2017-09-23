# Pega o nome de todos os procuradores do Estado de São Paulo

from urllib.request import urlopen
from bs4 import BeautifulSoup

html = urlopen("https://sismpconsultapublica.mpsp.mp.br/ConsultarDistribuicao/ObterFiltrosPorMembro")
bsObj = BeautifulSoup(html.read(), "html.parser")
nomes = bsObj.findAll("option")
nomes = nomes[1:-5]

for nome in nomes:
    print(nome.getText())

