# Curso de raspagem de dados - 23092017

## Início

print("Alô mundo”)

dir("abacate”)

help("abacate”.upper)

## Importando bibliotecas

import random

dir random

help(random.choice)

random.choice([3,2,4,10])

x= [3,4,5,6,7,8,10]
x
random.choice(x)

nomes = ["Aisha", "Cássio", "Rafael", "Bruno", "Mateus"]
random.choice(nomes)


nomes.remove("Rafael”)
nomes

scramble = ["Aisha”, 31, "www.oi.com.br”]

nomes.append("Rafael”)
nomes

nomes[5] = "rafael”
nomes


nomes.sort()
nomes


dados = {"nomes”: "aisha, "idade”: 31, "instituição”: "UFSC”}
dados["nomes”]

dados["emails”] = ["aishameriane@gmail.com”, "bruxavoadora@gmail.com”]


Atribuição ( = )
a = 42
b = ‘abacate’
Operadores lógicos:
a > 42
a > 13
a == 13

média_notas = 7
exercícios  = 4
média_notas >= 6 and exercícios >= 6



fruta = "Abacate"
bebida = "Suco de Laranja"
fruta == "Abacate" or bebida == "Cerveja"

fruta == "Mamão”
fruta == "Abacate" or bebida == "Cerveja"

if fruta == "Abacate":
	print("Oba!")


print ('Bem vindo!')
g = input ('Chute um número: ')
chute = int(g)
if chute == 42:
	print ('Você venceu!')
else:
	print ('Você perdeu!')
print ('Fim do jogo!')


# Repetições

valores = [140.23, 199.02, 3.14, 99.12]

soma = 0

for valor in valores:
    soma = soma + valor
    print (soma)

soma

## Repetições com strings

valores = ["140.23", "199.02", "3.14", "99.12"]
soma = 0

for valor in valores:
    soma = soma + float(valor)
    print (soma)

## Ignorando valores de uma lista

lista = [0,1,2,3,4,5,6,7,8,9]
lista[1:-5]

# Caso onde páginas não existem
import requests

# Alfabeto
import string

dir(string)
string.ascii_uppercase

# Biblioteca massa

bokeh

# Expressões Regulares

ver no raspa08
Ou livro do Aurélio Vargas sobre regex (ele tem site também)

