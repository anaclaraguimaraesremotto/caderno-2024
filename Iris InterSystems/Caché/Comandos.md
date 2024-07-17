## Atribuindo Valores e Excluindo Varíaveis
### SET
Atribui valores à variáveis
	`SET tVariavel = 123`
	`SET tString = "nome"`


### KILL
Apaga variáveis em memória ou globais
    `KILL tVariavel`
    `KILL ^global`
    `KILL (tTeste,tTemp)`

## Entrada/Saída: (Leitura e Escrita)
### Write (escrita)
Executa uma saída no dispositivo corrente
	`WRITE #,!!!,?6,"Alô",!,"Mundo!"`

  
### Read (leitura)
 Atribui um valor de entrada a uma variável
	`READ Nome`


## Invocar código

### Do
Chama qualquer rotina, procedure, função ou método em COS
	``DO ^Rotina
	`DO ##class(Teste).MinhaFuncao(tParametro1, tParametro2)`

### Goto
Transfere o controle (e não retorna ao ponto de chamada), ou seja, pula para alguma ponto do código.
	`GOTO ^Rotina`

  
### Quit
Termina a execução de um processo iniciado com um comando DO, XECUTE, FOR ou Retorna de um método ou função
       `QUIT result   -> Retornando a variável result de um método ou função.

### Job
Executa um processo em segundo plano, ou seja, não aguarda a execução do código invocado para prosseguir. O Código continua sendo executado em sequência e com o código invocado em paralelo mas em segundo plano.
	`JOB ^ROTINA
	`JOB ##class(Teste).MinhaFuncao(tParametro1, tParametro2)`

### Xecute
Executa comandos especificados dentro de variáveis
`SET a="write Treinamento Confluence" XECUTE a

**Resultado: *Treinamento Confluence***

## Controle de Fluxo
    
### If/ElseIf/Else
Avalia uma expressão, e então seleciona o bloco de código a ser executado, baseado no valor verdadeiro desta expressão.
	`IF var>5 { WRITE "maior" }
	`ELSEIF var<5 { WRITE "menor" }
	`ELSE { WRITE "igual" }`

  
### While
Executa código enquanto a condição for verdadeira (condição aplicada no começo de cada repetição)
	`WHILE tCount < 5
	`{
	  `WRITE !, “Contador do loop tCount: “ _ tCount
	`SET tCount  = tCount  + 1 
	`}`

  
### Do While
Executa código enquanto a condição for verdadeira (condição aplicada no final de cada repetição)
	`DO 
	`{
	`WRITE !, “Contador do loop tCount: “ _ tCount
	`SET tCount  = tCount  + 1
	`} WHILE x<5

### For
Executa um bloco de código repetidamente, testando-o no começo de cada loop
	`FOR var=ValorInicial:Incremento:ValorParada (final)
	`FOR i=1:1:5
	 `{ 
	`WRITE !, “Contador do loop i: “ _ i
	`}

  
## Espera e Termino de Execução
    
### Hang
Suspende a execução por um número de segundos especificado
	`HANG 12
### Halt
Termina a execução do processo corrente
	`HALT
