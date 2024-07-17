## Sintaxe das funções

**$função(parâmetros)** 
**$nome – O nome da função. Ex.: $LENGTH**
**(parâmetros) – Um ou mais valores a serem passados para a função.**
		`WRITE $LENGTH("Caché")

### $Length
Retorna o número de caracteres ou de delimitadores (+1) em uma string
	WRITE $LENGTH("Teste Confluence") 
	WRITE $LENGTH("DD/MM/AAAA","/") 
### $ZCONVERT
Conversão entre formatos de strings
	`WRITE $ZCONVERT("Confluence","U") 
Exemplo de parâmetros para conversão
- **"U" ou "u" Maiúsculas
- **"L" ou "l" Minúsculas
### $Find
Procura uma substring e retorna um inteiro especificando sua posição final na string. Note que a posição retornada é a posição posterior.
	`WRITE $FIND("Confluence","flu") 
### $Extract
Extrai um ou mais caracteres de acordo com os parâmetros especificados    
	`WRITE $EXTRACT("Treinamento",6) 
	`WRITE $EXTRACT("Treinamento",1,5)

### $Piece
Retorna uma substring com base no delimitador e no(s) pedaço(s) – piece(s) – especificado(s).
	`WRITE $PIECE("01/05/2018","/") 
	`WRITE $PIECE("01/05/2018","/",3) 
	`WRITE $PIECE("01/05/2018","/",1,2) 
###  $Case
Compara expressões e retorna o valor do primeiro caso coincidente, equivalente a um controlador de código (IF/ElseIf/Else)
	`SET x=2
	`SET res=$CASE(x,1:"simples",2:"duplo",3:"triplo",:"erro de entrada")
	`WRITE res 
### $Translate
Executa uma troca dos caracteres do segundo argumento para os caracteres do terceiro. Se não for passado o terceiro argumento, retira todas as ocorrências do mesmo.
	`WRITE $TRANSLATE("Xreiyamenzo","xyz","Tnt")
### $Horolog
Uma string separada por virgula que contém dois números, o primeiro número representa a data e o segundo número a hora, mais especificamente o primeiro número presenta a quantidade de dia desde o inicio da contagem do caché (31/12/1840) e o segundo número representa o número de segundos que se passaram desde a meia-noite.
	`WRITE $HOROLOG 
### $ ZDate
Função para conversão de data em formato $HOROLOG para formatos alternativos.
	`WRITE ZDATE (HOROLOG,4) 
### $Get
Retorna o valor de uma variável especificada. Caso não exista, ao invés de erro, retorna vazio!
Se for passado o segundo parâmetro, caso não exista a variável, retornará o valor especificado.
	`WRITE xyz
	<`UNDEFINED`>
	`WRITE $GET(xyz)
	`WRITE $GET(xyz,"Null")

### $Data
Verifica se uma variável está definida, contém dados e se possui descendentes.
	`SET ^tVar = "Treinamento "
	`SET ^ tVar (1) = "x"
	`SET ^ tVar (1,1) = "12"
	`SET ^ tVar (2,3) =" confluence"
	`WRITE $DATA(^tVar (1))
	`WRITE $DATA(^tVar (2))

### Order
Retorna o primeiro subscrito válido a partir do especificado.
	`Set x(1)=10, x(5)=50, x(9)=30
	`WRITE $ORDER(x(""))
	**Resultado: 10
	`WRITE $ORDER(x(5))
	**Resultado: 50**