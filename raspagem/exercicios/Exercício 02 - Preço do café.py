# Acesse o site http://beans.itcarlow.ie/prices-loyalty.html
# E imprima na tela o preço do café

from urllib.request import urlopen
from bs4 import BeautifulSoup
#imprimir apenas o h1 e o seu conteúdo líquido
html = urlopen("http://beans.itcarlow.ie/prices-loyalty.html")
bsObj = BeautifulSoup(html.read(), 'html.parser')
print(bsObj.strong)
print("O preço do café para consumidores frequentes é {}.".format(bsObj.strong.getText()))
