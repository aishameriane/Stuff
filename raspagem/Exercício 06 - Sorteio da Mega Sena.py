# Exercício 6 - Resultados da Mega-Sena

import requests
from bs4 import BeautifulSoup as bs

url = 'http://www1.caixa.gov.br/loterias/loterias/megasena/megasena_pesquisa_new.asp'

p = requests.get(url)
s = bs(p.content, 'html.parser')
lista = s.find('ul')
numeros = lista.findAll('li')
ordenado = []

for numero in numeros:
    print (numero.getText())
    ordenado.append(int(numero.getText()))

ordenado.sort()
print("Os números sorteados em ordem crescente são: {}".format(ordenado))
