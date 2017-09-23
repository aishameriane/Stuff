# Pergunte a velocidade de um carro, supondo um valor inteiro.
# Caso ultrapasse 110km/h, exiba uma mensagem dizendo que o usuário foi multado.
# Neste caso, exiba o valor da multa, cobrando R$ 5,00 por km acima de 110.
print("Bem vindo ou bem vinda!")

velocidade = input ("Insira a velocidade: ")
velocidade = int(velocidade)

limite = 110

if velocidade > limite:
	print ("O carro está acima do limite permitido que é de {} km!".format(limite))
	excedente = (velocidade - limite)
	print ("Seu excedente é de {} km. O valor da multa é de R$ {} reais.".format(excedente, excedente*5))
else:
	print ("Tenha uma boa viagem!")

