# Pegar os valores previstos para gastos na copa do mundo e somar

from urllib.request import urlopen
from bs4 import BeautifulSoup

xml = urlopen("http://www.portaltransparencia.gov.br/copa2014/api/rest/empreendimento")
bsObj = BeautifulSoup(xml.read(), "xml")
valores = bsObj.findAll("valorTotalPrevisto")

for valor in valores:
    print(valor.getText())

soma = 0
for valor in valores:
    print(valor.get_text())
    soma = soma + float(valor.get_text())

print(soma)
