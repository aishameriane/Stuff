print("Hello World!")

# To assign information in variables, we use the equal sign
## print() prints information on the screen
my_var = 2
print(my_var)

# Strings should be inside double or single quotes
## Coding style: it's better to choose a single style for the entire code
my_name = "Aishameriane"
print(my_name)
my_lastname = 'Venes Schmidt'
print(my_lastname)

# Booleans are case sensitive!
## This one returns an error:  m = true
m = True

# Type returns the class of an object
## There are 4 basic types:
### int - integers (numbers)
### float - numbers with decimal representation
### str - (strings) characters inside quotes, without numeric representation
### bool - Booleans (TRUE/FALSE)
type(m)

# Forcing (coersion) variable types
m = int(m)
type(m)
idade = "17.5"
type(idade)
idade = float(idade)
type(idade)

## But int(idade) wouldn´t work

# How to clear the console
def cls(): print ("\n" * 100)

# Doing some math
a =  5
b =  2
a +  b
a *  b
b /  a
b // a # Returns the integer part (quocient)
b %  a # Returns the rest
b ** a # Exponential
(a+b-2*a+a**a)/5


############################
############################

# Logical Operators

# >  # Greater
# <  # Less
# >= # Greater or equal
# <= # Less or equal
# == # Equal
# != # Different

# not 
# and
# or

a = 2
b = 3
a > b
b > a
a == b
a != b
a > b and b > a
a > b or  b > a
not (a != b) == False # a is different from b, so the parenthesis returns True
                      # when negated, became False
                      # which is equal to False


# Exercise
# If a = 2, b = 3, c = 2.0 and d = "2.0", do the following

a = 2
b = 3
c = 2.0
d = "2.0"

a + b
b ** a
a + c
# a + d #This produces an error

a == c
a <= b
a < b and b < c
a < b or b < c
a > c or a >= c
not (a != b and b <= (a ** 2) -1)

# String operations
## Concatenating strings using math operators!

"Aishameriane" + " " + "Venes" + " " + "Schmidt"
"a" + "b"

## It is also possible to multiplicate strings by numbers
"he" * 6 + "ha" * 3 # Awesome

## .upper() transforms everything in CAPSLOCK
"Aishameriane".upper()
("Aishameriane" + " " + "Venes" + " " + "Schmidt").upper()
my_name.upper()

## .lower() makes everything in lower case

"Aishameriane".lower()
("Aishameriane" + " " + "Venes" + " " + "Schmidt").lower()
my_name.lower()

## .capitalize() makes only the first charactere (letter) in caps

"aishameriane".capitalize()
("meu nome é Aishameriane.").capitalize()

## .title() makes the first letter of each word in caps

("aishameriane venes schmidt").title()

## Tests with Strings
### .startswith("something") verifies if the first chatacteres are equal to something
### .endswith("something") verifies if the last chatacteres are equal to something

"Aishameriane".startswith("Aisha")
(("Aishameriane").lower()).startswith("Aisha".lower())
"Aishameriane".endswith("meriane")
"Aishameriane".startswith("Aisha") and "Aishameriane".endswith("meriane")

## Replacing
### .replace("old string", "new string") substitutes something inside a string

"O mundo é horrível.".replace("horrível", "maravilhoso")

## Exercise

my_string = "Workshop Pyladies as 9hrs da manhã :("

my_string.upper()
my_string.lower()
(my_string.replace("hrs","H")).title()
my_string.replace("(", ")")
my_string + ". Mas pelo menos tem café :)"
my_string.replace("Pyladies ", "")
my_string.replace(":(", ":)").replace("9hrs", "10h")

# Creating lists

## Lists in Python can have more than one data type

first  = 1
second = 2
third  = 3
my_list = [first, second, third]
my_list
type(my_list)
my_list * 4

my_other_list = [[1, "Aisha"], True]

a = 0
b = 2
lista = [a < b] # Creates a list with lenght one and the first element equals to True

## Recovering information in lists
my_list = ["a", 2, True, [1], "Ladies", 1, 2]

### The first element is my_list[0]
my_list[0]

### First 3 items

my_list[0:3] # Returns ["a", 2, True]

### Last 3 items

my_list[2:]
my_list[-1] # Last item

### Increments
#### <list>[start:end:increment]

my_list[::2] # From beginning to end with increment equal to 2
my_list[::-2] # From end to beginning with increment equal to 2

my_list[5:0:-2]

### len() returns the size of a list

len(my_list)

## Adding stuff to a list
### .append(stuff) to add stuff OR
### list1 + list2
### The first one changes permanently the list while the second needs to be stored in a variable

my_list.append("oie")
my_list + ["oie"]


## Characters as lists

my_char = "Pyladies"
my_char[0]
my_char[0:5:2]
len(my_char)

# Exercise

string = "Workshop"
lista  = ["Talita", "Alana", "Monalisa"]

len(string)
len(lista)
string.startswith("W")
string.startswith("P")
string[::-1]
lista[::-1]
lista.append("Pessoa")
lista.append(string)

## "sep".join(list) transforms a list of strings into a string

my_list = ["Workshop", "Pyladies", "Floripa"]
"; ".join(my_list)

## Coercing types into strings

### "Something {}".format(1)

"Uma forma de dizer dois: {}".format(2)
"I'm telling something {}!!".format(True)
"{} X {} = {}".format(2,3,2*3)

dir(my_list)

# User declared variables

## variable = input("Message")
## Obs: input() returns a string

user_name = input("Olá! Escreva seu nome: ")
user_age  = int(input("Escreva sua idade (em anos): "))

# Writing functions in Python

def soma(a,b):
    return a+b
print(soma(1,2))
print(soma("Aishameriane ", "Schmidt"))

def multiply(n1, n2):
    return n1 * n2

def pede_num():
    numero_1 = float(input("Informe o primeiro número: "))
    numero_2 = float(input("Informe o segundo número: "))
    lista_num = [numero_1, numero_2]
    return(lista_num)

listinha = pede_num()
multiply(listinha[0], listinha[1])

## Exercise
### Make a function to ask the user two numbers and calculate their ratio
### Returns the exact value of the division
### The rest
### The quocient

def division():
    numero_1 = float(input("Informe o primeiro número: "))
    numero_2 = float(input("Informe o segundo número: "))
    divisao    = numero_1 /  numero_2
    quociente  = numero_1 // numero_2
    resto      = numero_1 %  numero_2
    print("Divisão de {} por {}: {}".format(numero_1, numero_2, divisao))
    print("Quociente de {} por {}: {}".format(numero_1, numero_2, int(inteira)))
    print("Resto da divisão de {} por {}: {}".format(numero_1, numero_2, int(resto)))

###############################################
###############################################


# Conditionals

comida_na_geladeira = input("Tem comida na geladeira? ")
tem_dinheiro = input("Tenho algum trocado? ")

if comida_na_geladeira == "Sim":
    print("Uhul, bora comer!!")
elif tem_dinheiro == "Sim":
    print("Vamos chamar pizza!")
else:
    print("Fico com fome :(")

################################################
################################################

def convite():
    nome    = input("Oi! Qual o seu nome? ")
    convite = input("Oi, {}. Você quer tomar chá, café ou suco?".format(nome))
    if convite.lower() == "chá" or convite.lower() == "cha":
        print("Eu adoro chá!")
    elif convite.lower() == "café" or convite.lower() == "cafe":
        print("Conheço um ótimo café!")
    elif convite.lower() == "suco":
        print("Suco parece ótimo!")
    else:
        print("Tudo bem, respeito sua posição :)")

