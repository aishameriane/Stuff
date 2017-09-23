import requests
from bs4 import BeautifulSoup as bs
import string

url = 'https://www.bayerpet.com.br/Caes/lista-nomes/'

for letra in string.ascii_uppercase:
    for numero in (1,2,3):
        u = url + letra + '/' + str(numero)
        # print(u) # Verifica se funcionou
        p = requests.get(u)
        s = bs(p.content, 'html.parser')
        lista = s.find('ul',
                       class_ = 'list listNames')
        cachorros = lista.findAll('li')
        for cachorro in cachorros:
            print (cachorro.getText())
