ObjectScript é uma linguagem sem tipo — você não precisa declarar os tipos de variáveis. Qualquer variável pode ter um valor de cadeia de caracteres, numérico ou objeto. Dito isso, há informações importantes a serem conhecidas ao usar diferentes tipos de dados no ObjectScript, como:
## Strings

Uma cadeia de caracteres é um conjunto de caracteres: letras, dígitos, pontuação e assim por diante delimitados por um conjunto correspondente de aspas ("):

```
 SET string = "This is a string"
 WRITE string
```

Os tópicos sobre cadeias de caracteres incluem:

### Cadeia de caracteres nula / $CHAR(0)

- **SET mystr="":** define uma cadeia de caracteres nula ou vazia. A cadeia de caracteres é definida, tem comprimento zero e não contém dados:
    
```
SET mystr=""
WRITE "defined:",$DATA(mystr),!
WRITE "length: ",$LENGTH(mystr),!
ZZDUMP mystr
```
    
- **SET mystr=$CHAR(0):** define uma cadeia de caracteres para o caractere nulo. A cadeia de caracteres é definida, tem comprimento 1 e contém um único caractere com o valor hexadecimal de 00:
    
    ```
      SET mystr=$CHAR(0)
      WRITE "defined:",$DATA(mystr),!
      WRITE "length: ",$LENGTH(mystr),!
      ZZDUMP mystr
    ```

Observe que esses dois valores não são iguais. No entanto, uma cadeia de caracteres de bits trata esses valores como idênticos.

Observe que o InterSystems SQL tem sua própria interpretação desses valores. Consulte NULL e a cadeia de caracteres vazia no capítulo "Elementos de linguagem" de Usando o Caché SQL.

### Escapando das aspas

Você pode incluir um caractere " (aspas duplas) como um literal dentro de uma cadeia de caracteres, precedendo-o com outro caractere de aspas duplas:

```
 SET string = "This string has ""quotes"" in it."
 WRITE string
```

Não há outras sequências de caracteres de escape dentro de literais de cadeia de caracteres do ObjectScript.

Observe que as aspas literais são especificadas usando outras sequências de escape em outro software InterSystems. Consulte a função *$ZCONVERT* para obter uma tabela dessas sequências de escape.

### Concatenando cadeias de caracteres

Você pode concatenar duas cadeias de caracteres em uma única cadeia de caracteres usando o *operador _ concatenate*
```
 SET a = "Inter"
 SET b = "Systems"
 SET string = a_b
 WRITE string
```

Usando o operador concatenar, você pode incluir caracteres não imprimíveis em uma cadeia de caracteres. A cadeia de caracteres a seguir inclui o caractere linefeed *($CHAR(10)):*

```
 SET lf = $CHAR(10)
 SET string = "This"_lf_"is"_lf_"a string"
 WRITE string
```
 
 **Nota:** A forma como os caracteres não imprimíveis são exibidos é determinada pelo dispositivo de exibição. Por exemplo, o Terminal difere da exibição do navegador do caractere de alimentação de linha e de outros caracteres de posicionamento. Além disso, diferentes navegadores exibem os caracteres de posicionamento *$CHAR(11) e $CHAR(12)* de forma diferente.
 As cadeias de caracteres codificadas pelo Caché — cadeias de caracteres de bits, cadeias de caracteres de estrutura List e cadeias de caracteres JSON — têm limitações no uso do operador concatenado. Para obter mais detalhes, consulte Concatenar cadeias de caracteres codificadas.
 Algumas considerações adicionais se aplicam ao concatenar números. Para obter mais detalhes, consulte "Concatenando números".

### Comparações de cadeias de caracteres

Você pode usar os operadores equals (=) e not equal ('=) para comparar duas cadeias de caracteres. As comparações de igualdade de cadeia de caracteres diferenciam maiúsculas de minúsculas. Tenha cuidado ao usar esses operadores para comparar uma cadeia de caracteres com um número, porque essa comparação é uma comparação de cadeia de caracteres, não uma comparação numérica. Portanto, apenas uma cadeia de caracteres contendo um número em forma canônica é igual ao seu número correspondente. ("-0" não é um número canônico.) Isso é mostrado no exemplo a seguir:

```
  WRITE "Fred" = "Fred",!  // TRUE
  WRITE "Fred" = "FRED",!  // FALSE
  WRITE "-7" = -007.0,!    // TRUE
  WRITE "-007.0" = -7,!    // FALSE
  WRITE "0" = -0,!         // TRUE
  WRITE "-0" = 0,!         // FALSE
  WRITE "-0" = -0,!        // FALSE
```

Os operadores <, >, <=, ou >= não podem ser usados para executar uma comparação de cadeia de caracteres. Esses operadores tratam cadeias de caracteres como números e sempre realizam uma comparação numérica. A qualquer cadeia de caracteres não numérica é atribuído um valor numérico de 0 quando comparado usando esses operadores.

### Comparações de letras e cadeias de caracteres

As comparações de igualdade de cadeia de caracteres diferenciam maiúsculas de minúsculas. Você pode usar a função $ZCONVERT para converter as letras nas cadeias de caracteres a serem comparadas com todas as letras maiúsculas ou minúsculas. Os caracteres que não são letras permanecem inalterados.

Algumas letras têm apenas uma forma de letra minúscula. Por exemplo, o eszett alemão ($CHAR(223)) é definido apenas como uma letra minúscula. Convertê-lo em uma letra maiúscula resulta na mesma letra minúscula. Por esse motivo, ao converter cadeias alfanuméricas em letras maiúsculas e minúsculas, é sempre preferível converter para minúsculas.

### Cordas longas

O Caché suporta duas opções de comprimento máximo de string:
- O comprimento máximo tradicional da cadeia de caracteres de 32.767 caracteres.
- Comprimento máximo da cadeia de caracteres Long Strings de 3.641.144 caracteres.

Cadeias de caracteres longas são habilitadas por padrão. Se cadeias de caracteres longas estiverem habilitadas, o comprimento máximo de uma cadeia de caracteres será de 3.641.144 caracteres. Se as cadeias de caracteres longas estiverem desabilitadas, o comprimento máximo de uma cadeia de caracteres será de 32.767 caracteres.

A tentativa de exceder o comprimento máximo atual da cadeia de caracteres resulta em um erro   <`MAXSTRING`> .

Você pode retornar o comprimento máximo atual da cadeia de caracteres em todo o sistema invocando o MaxLocalLength() método, da seguinte forma:

```
   WRITE $SYSTEM.SYS.MaxLocalLength()
```

Você pode usar qualquer uma das seguintes operações para habilitar ou desabilitar cadeias de caracteres longas em todo o sistema:

- No Portal de Gerenciamento, selecione Sistema, Configuração, Memória e Inicialização. Na página Memória do Sistema e Configurações de Inicialização, marque a caixa de seleção Habilitar Cadeias de Caracteres Longas.
- No arquivo de parâmetros do Caché (arquivo CPF), especifique o valor do parâmetro EnableLongStrings, conforme descrito na seção EnableLongStrings da Referência do Arquivo de Parâmetros do Cacé.
- Na Config.Miscellaneous propriedades de classe, especifique um EnableLongStrings valor booleano. Isso modifica o parâmetro de arquivo CPF correspondente. Por exemplo:

```
ZNSPACE "%SYS"
SET getstat=##class(Config.Miscellaneous).Get(.Properties)
IF getstat '= 1 {WRITE "Get config property error",! QUIT}
SET Properties("EnableLongStrings")=0
SET modstat=##class(Config.Miscellaneous).Modify(.Properties)
IF modstat '= 1 {WRITE "Modify config property error",! QUIT}
```


Quando um processo realmente usa uma cadeia de caracteres longa, a memória para a cadeia de caracteres vem do buffer malloc() do sistema operacional, não do espaço de memória de partição para o processo. Assim, a memória alocada para valores de cadeia de caracteres longos reais não está sujeita ao limite definido pelo parâmetro maximum memory per process (Maximum per Process Memory (KB)) e não afeta o valor $STORAGE do processo.

### Cadeias de caracteres de bits

Uma cadeia de caracteres de bits representa um conjunto lógico de bits numerados com valores booleanos. Os bits em uma cadeia de caracteres são numerados começando com o número de bit 1. Qualquer bit numerado que não tenha sido explicitamente definido como valor booleano 1 é avaliado como 0. Portanto, referenciar qualquer bit numerado além daqueles explicitamente definidos retorna um valor de bit de 0.

- Os valores de bit só podem ser definidos usando as funções de cadeia de caracteres de bits $BIT e $BITLOGIC
- Os valores de bit só podem ser acessados usando as funções de cadeia de caracteres de bits $BIT, $BITLOGIC e $BITCOUNT.

Uma cadeia de caracteres de bits tem um comprimento lógico, que é a posição de bit mais alta definida explicitamente como 0 ou 1. Esse comprimento lógico só é acessível usando a função $BITCOUNT e geralmente não deve ser usado na lógica do aplicativo. Para as funções de cadeia de caracteres de bits, uma variável global ou local indefinida é equivalente a uma cadeia de caracteres de bits com qualquer bit numerado especificado retornando um valor de bit 0 e um valor de $BITCOUNT de 0.

Uma cadeia de caracteres de bits é armazenada como uma cadeia de caracteres normal do Caché com um formato interno. Essa representação de cadeia de caracteres interna não é acessível com as funções de cadeia de caracteres de bits. Devido a esse formato interno, o comprimento da cadeia de caracteres de uma cadeia de caracteres de bits não é significativo para determinar nada sobre o número de bits na cadeia de caracteres.

Devido ao formato interno da cadeia de caracteres de bits, você não pode usar o operador concatenar com cadeias de caracteres de bits. Tentar fazer isso resulta em um erro <`INVALID BIT STRING`>

Duas cadeias de caracteres de bits no mesmo estado (com os mesmos valores booleanos) podem ter representações de cadeia de caracteres internas diferentes e, portanto, as representações de cadeia de caracteres não devem ser inspecionadas ou comparadas na lógica do aplicativo. Para as funções de cadeia de caracteres de bits, as cadeias de caracteres nulas e variáveis globais/locais indefinidas são equivalentes a uma cadeia de caracteres de bits com todos os bits 0 e um comprimento de 0.

Ao contrário de uma cadeia de caracteres comum, uma cadeia de caracteres de bits trata a cadeia de caracteres vazia e o caractere $CHAR(0) para serem equivalentes entre si e para representar um bit 0. Isso ocorre porque $BIT trata qualquer cadeia de caracteres não numérica como 0. Portanto:

```
  SET $BIT(bstr1,1)=""
  SET $BIT(bstr2,1)=$CHAR(0)
  SET $BIT(bstr3,1)=0
  IF $BIT(bstr1,1)=$BIT(bstr2,1) {WRITE "bitstrings are the same"} ELSE {WRITE "bitstrings different"}
  WRITE $BITCOUNT(bstr1),$BITCOUNT(bstr2),$BITCOUNT(bstr3) 
```

Um bit definido em uma variável global durante uma transação será revertido para seu valor anterior após a eversão da transação. No entanto, a reversão não retorna a cadeia de caracteres de bits da variável global para seu comprimento de cadeia de caracteres anterior ou representação de cadeia de caracteres interna anterior. As variáveis locais não são revertidas por uma operação de reversão.

Uma estrutura de bitmap lógico pode ser representada por uma matriz de cadeias de caracteres de bits, onde cada elemento da matriz representa um "bloco" com um número fixo de bits. Como undefined é equivalente a um bloco com todos os 0 bits, a matriz pode ser esparsa, onde os elementos da matriz que representam uma parte de todos os 0 bits não precisam existir. Por esse motivo, e devido ao comportamento de reversão acima, a lógica do aplicativo deve evitar depender do comprimento de uma cadeia de caracteres de bits ou da contagem de bits de valor 0 acessíveis usando $BITCOUNT(str) ou $BITCOUNT(str,0).

Os tópicos relacionados aos números incluem:

- Fundamentos de Números
- Forma Canônica dos Números
- Cadeias de caracteres como números
- Concatenando números
- Números Fracionários
- Notação Científica
- Números extremamente grandes

Os literais numéricos não exigem qualquer pontuação de encerramento. Você pode especificar um número usando quaisquer caracteres numéricos válidos. Caché avalia um número como sintaticamente válido e, em seguida, o converte em forma canônica.

Os requisitos sintáticos para um literal numérico são os seguintes:

- Ele pode conter os números decimais de 0 a 9 e deve conter pelo menos um desses caracteres numéricos. Ele pode conter zeros à esquerda ou à direita. No entanto, quando o InterSystems IRIS converte um número em forma canônica, ele remove automaticamente zeros inteiros à esquerda. Portanto, números para os quais zeros inteiros à esquerda são significativos devem ser inseridos como cadeias de caracteres. Por exemplo, os CEPs postais dos Estados Unidos podem ter um zero inteiro à esquerda, como 02142, e, portanto, devem ser manipulados como cadeias de caracteres, não números.
- Ele pode conter qualquer número de sinais de mais e menos em qualquer sequência. No entanto, um sinal de mais ou menos sinal não pode aparecer após qualquer outro caractere, exceto o caractere de notação científica "E". Em uma expressão numérica, um sinal após um caractere não sinal é avaliado como uma operação de adição ou subtração. Em uma cadeia de caracteres numérica, um sinal após um caractere não assinal é avaliado como um caractere não numérico, encerrando a parte numérica da cadeia de caracteres.
- O Caché usa os valores das propriedades PlusSign e MinusSign para a localidade atual para determinar esses caracteres de sinal ("+" e "-" por padrão); Esses caracteres de sinal dependem da localidade. Para determinar os caracteres PlusSign e MinusSign para sua localidade, invoque o GetFormatItem()

    ```
      WRITE ##class(%SYS.NLS.Format).GetFormatItem("PlusSign"),!
      WRITE ##class(%SYS.NLS.Format).GetFormatItem("MinusSign")
    ```
    
- Ele pode conter no máximo um caractere separador decimal. Em uma expressão numérica, um segundo separador decimal resulta em um erro <`SINTAXE`> de dados. Em uma cadeia de caracteres numérica, um segundo separador decimal é avaliado como o primeiro caractere não numérico, terminando a parte numérica da cadeia de caracteres. O caractere separador decimal pode ser o primeiro caractere ou o último caractere da expressão numérica. A escolha do caractere separador decimal depende da localidade: o formato americano usa um ponto (.) como separador decimal, que é o padrão. O formato europeu usa uma vírgula (,) como separador decimal. Para determinar o caractere DecimalSeparator para sua localidade, chame o GetFormatItem() método:

 ```
 WRITE ##class(%SYS.NLS.Format).GetFormatItem("DecimalSeparator"
 ```

- Pode conter no máximo uma letra "E" (ou "e") para especificar um expoente de base 10 para notação científica. Esse caractere de notação científica ("E" ou "e") deve ser precedido por um número inteiro ou fracionário, e seguido por um inteiro.

Os valores literais numéricos não oferecem suporte ao seguinte:
- Eles não podem conter separadores de grupo numéricos. Estes são dependentes da localidade: o formato americano usa vírgulas, o formato europeu usa pontos. Você pode usar a função $INUMBER para remover separadores de grupo numéricos e a função $FNUMBER para adicionar separadores de grupo numérico.
- Eles não podem conter símbolos de moeda, letras hexadecimais ou outros caracteres não numéricos. Eles não podem conter espaços em branco, exceto antes ou depois dos operadores aritméticos.
- Eles não podem conter sinais de mais ou menos à direita. No entanto, a função $FNUMBER pode exibir um número como uma cadeia de caracteres com um sinal à direita, e a função $NUMBER pode pegar uma cadeia de caracteres nesse formato e convertê-la em um número com um sinal à esquerda.
- Eles não podem especificar parênteses para representar um número como um número negativo (um débito). No entanto, a função $FNUMBER pode exibir um número negativo como uma cadeia de caracteres com parênteses de delimitação, e a função $NUMBER pode pegar uma cadeia de caracteres nesse formato e convertê-la em um número com um sinal negativo à esquerda.

Um número ou expressão numérica pode conter pares de parênteses de delimitação. Esses parênteses não fazem parte do número, mas governam a precedência das operações. Por padrão, o Caché executa todas as operações em estrita ordem da esquerda para a direita.

ObjectScript executa todas as operações numéricas em números em sua forma canônica. Por exemplo, o comprimento do número +007,00 é 1; O comprimento da cadeia de caracteres "+007.00" é 7.

Quando o Caché converte um número em forma canônica, ele executa as seguintes etapas:
1. Os expoentes da notação científica são resolvidos. Por exemplo, 3E4 converte para 30000 e 3E-4 converte para .0003.
2. Os principais sinais são resolvidos. Primeiro, vários sinais são resolvidos para um único sinal (por exemplo, dois sinais de menos resolvem para um sinal de mais). Então, se o sinal principal for um sinal de mais, ele será removido. Você pode usar a função $FNUMBER para acrescentar um sinal de adição a um número canônico positivo do Caché.
	**Nota:**  O ObjectScript resolve qualquer combinação de sinais de mais e menos à esquerda. No SQL, dois sinais de subtração consecutivos são analisados como um indicador de comentário de linha única. Portanto, especificar um número no SQL com dois sinais de subtração à esquerda consecutivos resulta em um erro SQLCODE -12.
3. Todos os zeros à esquerda e à direita são removidos. Isso inclui a remoção de zeros inteiros à esquerda, incluindo o zero inteiro à esquerda de frações menores que 1. Por exemplo, 0,66 torna-se .66.
    - Para acrescentar um zero inteiro a uma fração canônica, use a função $FNUMBER ou $JUSTIFY .66 passa a ser 0,66.
    - Para remover zeros inteiros de uma fração não canônica, use o operador Unary Plus para forçar a conversão de uma cadeia de caracteres numérica em um número canônico. No exemplo a seguir, a parte de segundos fracionários de um carimbo de data/hora, +$PIECE("65798,00000.66",",",2). 00000.66 passa a ser .66.
    - Como parte dessa conversão, as frações zero são simplificadas para 0. Independentemente de como expresso (0.0, .0, .000) todos os valores zero são convertidos em 0.
4. Um separador decimal à direita é removido.
5. -0 é convertido em 0.
6. Operações aritméticas e concatenação numérica são realizadas. O Caché realiza essas operações em estrita ordem da esquerda para a direita. Os números estão em sua forma canônica quando essas operações são executadas. Para obter mais detalhes, consulte Concatenando números abaixo.

Os números de forma canônica do Caché diferem de outros formatos de números canônicos usados no software InterSystems:

- ODBC: Frações de zero inteiro convertidas em ODBC têm um inteiro zero. Portanto, .66 e 000.66 tornam-se ambos 0,66. Você pode usar a função $FNUMBER ou $JUSTIFY para acrescentar um zero inteiro a um número canônico do Caché.
- JSON: Apenas um único sinal de menos à esquerda é permitido; 
	- Um sinal de adição à esquerda ou vários sinais não são permitidos.
	- Expoentes são permitidos, mas não resolvidos. 3E4 é retornado como 3E4.
	- Zeros à esquerda não são permitidos. Os zeros à direita não são removidos.
	- Frações inteiras zero devem ter um inteiro zero. Portanto, .66 e 000.66 não são números JSON válidos, mas 0.66 e 0.660000 são números JSON válidos.
	- Um separador decimal à direita não é permitido.
	- Os valores zero não são convertidos: 0,0, -0 e -0,000 são retornados inalterados como números JSON válidos.

### Cadeias de caracteres como números

A seguir estão as regras gerais para lidar com cadeias de caracteres como números. Para obter mais detalhes, consulte Conversão de cadeia de caracteres para número no capítulo "Operadores e expressões" deste livro.

- Para todas as operações numéricas, uma cadeia de caracteres contendo um número em forma canônica é funcionalmente idêntica ao número correspondente. Por exemplo, "3" = 3, "-2,5" = -2,5. (Observe que -0 não é um número canônico.)
- Para operações aritméticas, uma cadeia de caracteres contendo apenas caracteres numéricos em forma não canônica é funcionalmente idêntica ao número correspondente. Por exemplo, "003" + 3 = 6, "++-2.5000" + -2.5 = -5.
- Para operações maiores que/menores que, uma cadeia de caracteres contendo apenas caracteres numéricos em forma não canônica é funcionalmente idêntica ao número correspondente. Por exemplo, as seguintes afirmações são verdadeiras: "003" > 2, "++-2.5000" >= -2.5.
- Para operações de igualdade (=, '=), uma cadeia de caracteres contendo apenas caracteres numéricos em forma não canônica é tratada como uma cadeia de caracteres, não como um número. Por exemplo, as seguintes afirmações são verdadeiras: "003" = "003", "003" '= 3, "+003" '= "003".

Algumas diretrizes adicionais sobre a análise de cadeias de caracteres como números:

- Uma cadeia numérica mista é uma cadeia de caracteres que começa com caracteres numéricos, seguidos por um ou mais caracteres não numéricos. Por exemplo, "7 anões". As operações numéricas e booleanas do Caché (exceto as operações de igualdade) geralmente analisam uma cadeia numérica mista como um número até encontrar um caractere não numérico. Nesse ponto, o restante da cadeia de caracteres é ignorado. O exemplo a seguir mostra operações aritméticas em cadeias numéricas mistas:

```
WRITE "7dwarves" + 2,!   // returns 9
WRITE "+24/7" + 2,!      // returns 26
WRITE "7,000" + 2,!      // returns 9
WRITE "7.0.99" + 2,!     // returns 9
WRITE "7.5.99" + 2,!     // returns 9.5
```

- Uma cadeia de caracteres não numérica é qualquer cadeia de caracteres na qual um caractere não numérico é encontrado antes de encontrar um caractere numérico. Observe que um espaço em branco é considerado um caractere não numérico. As operações numéricas e booleanas do Caché (exceto as operações de igualdade) geralmente analisam essa cadeia de caracteres como tendo um valor numérico de 0 (zero). O exemplo a seguir mostra operações aritméticas em cadeias de caracteres não numéricas:

```
WRITE "dwarves 7" + 2,!   // returns 2
WRITE "+ 24/7" + 2,!      // returns 2
WRITE "$7000" + 2,!       // returns 2
```

- Você pode prefixar uma cadeia de caracteres com um sinal de adição para forçar sua avaliação como um número para operações de igualdade. Uma cadeia numérica é analisada como um número em forma canônica; Uma cadeia de caracteres não numérica é analisada como 0. (Um prefixo de sinal de menos também força a avaliação de uma cadeia de caracteres como um número para operações de igualdade; o sinal de subtração, é claro, inverte o sinal para um valor diferente de zero.) O exemplo a seguir mostra o sinal de adição forçando a avaliação numérica para operações de igualdade:

```
WRITE +"7" = 7,!            // returns 1 (TRUE)
WRITE +"+007" = 7,!         // returns 1 (TRUE)
WRITE +"7 dwarves" = 7,!    // returns 1 (TRUE)
WRITE +"dwarves" = 0,!      // returns 1 (TRUE)
WRITE +"" = 0,!             // returns 1 (TRUE)
```

Exceções de manipulação de cadeia de caracteres numéricas para comandos e funções individuais são comuns, conforme observado na Referência do Caché ObjectScript.

Um número pode ser concatenado a outro número usando o operador concatenado (_). O Caché primeiro converte cada número em sua forma canônica e, em seguida, realiza uma concatenação de cadeia de caracteres nos resultados. Assim, todos resultam em 1234: 12_34, 12_+34, 12_--34, 12.0_34, 12_0034.0, 12E0_34. A concatenação 12._34 resulta em 1234, mas a concatenação 12_.34 resulta em 12.34. A concatenação 12_-34 resulta na cadeia de caracteres "12-34".

Caché realiza concatenação numérica e operações aritméticas em números após converter esses números para a forma canônica. Ele executa essas operações em ordem estrita da esquerda para a direita, a menos que você especifique parênteses para priorizar uma operação. O exemplo a seguir explica uma consequência disso:

```
  WRITE 7_-6+5 // returns 12
```

Neste exemplo, a concatenação retorna a cadeia de caracteres "7-6". Isso, claro, não é um número canônico. Caché converte essa cadeia em um número canônico truncando no primeiro caractere não numérico (o sinal de menos incorporado). Em seguida, ele executa a próxima operação usando este número canônico 7 + 5 = 12.

### Números Fracionários

O Caché suporta dois tipos numéricos diferentes que podem ser usados para representar números fracionários:

- Ponto flutuante decimal: Por padrão, o Caché representa números fracionários usando seu próprio padrão decimal de ponto flutuante ($DECIMAL números). Este é o formato preferido para a maioria dos usos. Ele fornece o mais alto nível de precisão — 18 dígitos decimais. É consistente em todas as plataformas de sistemas suportadas pelo Caché. O ponto flutuante decimal é preferido para valores de banco de dados. Em particular, um número fracionário como 0,1 pode ser representado exatamente usando notação decimal de ponto flutuante, enquanto o número fracionário 0,1 (bem como a maioria dos números fracionários decimais) só pode ser aproximado por ponto flutuante binário.
    
- Ponto flutuante binário: O padrão de ponto flutuante binário de precisão dupla IEEE é uma maneira padrão da indústria de representar números fracionários. Os números de ponto flutuante IEEE são codificados usando notação binária. A representação de ponto flutuante binário é geralmente preferida ao fazer cálculos de alta velocidade porque a maioria dos computadores inclui hardware de alta velocidade para aritmética de ponto flutuante binário. O ponto flutuante binário de precisão dupla tem uma precisão de 53 bits binários, o que corresponde a 15,95 dígitos decimais de precisão. A representação binária não corresponde exatamente a uma fração decimal porque uma fração como 0,1 não pode ser representada como uma sequência finita de frações binárias. Como a maioria das frações decimais não pode ser representada exatamente nessa notação binária, um número de ponto flutuante IEEE pode diferir ligeiramente do número de ponto flutuante padrão Caché correspondente. Quando um número de ponto flutuante IEEE é exibido como um número fracionário, os bits binários são frequentemente convertidos em um número fracionário com muito mais de 18 dígitos decimais. Isso não significa que os números de ponto flutuante do IEEE sejam mais precisos do que os números fracionários Caché padrão. Os números de ponto flutuante IEEE são capazes de representar números maiores e menores do que os números Caché padrão, e suportam os valores especiais INF (infinito) e NAN (não um número). Para obter mais detalhes, consulte a função $DOUBLE
    

Você pode usar a função $DOUBLE para converter um número de ponto flutuante padrão do Caché em um número de ponto flutuante IEEE. Você pode usar a função $DECIMAL para converter um número de ponto flutuante IEEE em um número de ponto flutuante padrão do Caché.

Por padrão, o Caché converte números fracionários em forma canônica, eliminando todos os zeros à esquerda. Portanto, 0,66 torna-se .66. $FNUMBER e $JUSTIFY (formato de 3 parâmetros) sempre retornam um número fracionário com pelo menos um dígito inteiro; Usando qualquer uma dessas funções, .66 torna-se 0,66.

$FNUMBER e $JUSTIFY podem ser usados para arredondar ou preencher um número numérico para um número especificado de dígitos fracionários. O Caché arredonda 5 ou mais, arredonda 4 ou menos. O preenchimento adiciona zeros como dígitos fracionários, conforme necessário. O caractere separador decimal é removido ao arredondar um número fracionário para um inteiro. O caractere separador decimal é adicionado quando um inteiro é preenchido com zero a um número fracionário.

### Notação Científica

Para especificar a notação científica (exponencial) no ObjectScript, use o seguinte formato:

```
[-]mantissaE[-]exponent 
```

**onde: **

| -        | Opcional — Um ou mais operadores Unary Minus ou Unary Plus. Esses caracteres PlusSign e MinusSign são configuráveis. A conversão para a forma canônica resolve esses operadores depois de resolver a notação científica. |
| -------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| mantissa | Um número inteiro ou fracionário. Pode conter zeros à esquerda e à direita e um caractere separador decimal à direita.                                                                                                   |
| E        | Um operador delimitando o expoente. O "E" maiúsculo é o operador expoente padrão; o "e" minúsculo é um operador expoente configurável, usando o método ScientificNotation() do %SISTEMA. Processo classe.                |
| -        | Opcional — Um único operador Unary Minus ou Unary Plus. Pode ser usado para especificar um expoente negativo. Esses caracteres PlusSign e MinusSign são configuráveis.                                                   |
| expoente | Um inteiro especificando o expoente (a potência de 10). Pode conter zeros à esquerda. Não pode conter um caractere separador decimal.                                                                                    |

Por exemplo, para representar 10, use 1E1. Para representar 2800, use 2.8E3. Para representar .05, use 5E-2.

Não são permitidos espaços entre a mantissa, o E e o expoente. Parênteses, concatenação e outros operadores não são permitidos dentro dessa sintaxe.

Como a resolução da notação científica é o primeiro passo na conversão de um número em forma canônica, algumas operações de conversão não estão disponíveis. A mantissa e o expoente devem ser literais numéricos, não podem ser variáveis ou expressões aritméticas. O expoente deve ser um inteiro com (no máximo) um sinal de mais ou menos.

Veja o Notação Científica() método do %SISTEMA.Processo classe.

### Números extremamente grandes

Os maiores inteiros que podem ser representados exatamente são os inteiros de 19 dígitos -9223372036854775808 e 9223372036854775807. Isso porque esses são os maiores números que podem ser representados com 64 bits assinados. Inteiros maiores que isso são arredondados automaticamente para caber dentro desse limite de 64 bits. Isso é mostrado no exemplo a seguir:

```
  SET x=9223372036854775807
  WRITE x,!
  SET y=x+1
  WRITE y
```

Da mesma forma, expoentes maiores que 128 também podem resultar em arredondamento para permitir a representação dentro de 64 bits assinados. Isso é mostrado no exemplo a seguir:

```
  WRITE 9223372036854775807e-128,!
  WRITE 9223372036854775807e-129
```

Devido a esse arredondamento, as operações aritméticas que resultam em números maiores do que esses inteiros de 19 dígitos têm seus dígitos de ordem baixa substituídos por zeros. Isso pode resultar em situações como as seguintes:

```
  SET longnum=9223372036854775790
  WRITE longnum,!
  SET add17=longnum+17
  SET add21=longnum+21
  SET add24=longnum+24
  WRITE add17,!,add24,!,add21,!
  IF add24=add21 {WRITE "adding 21 same as adding 24"}
```

O maior número de ponto flutuante decimal Caché suportado é 9.223372036854775807E145. O maior valor de $DOUBLE com suporte (supondo que o estouro do IEEE para INFINITY esteja desabilitado) é 1,7976931348623157081E308. O tipo $DOUBLE suporta uma faixa maior de valores do que o tipo decimal Caché, enquanto o tipo decimal Caché suporta mais precisão. O tipo decimal Caché tem uma precisão de aproximadamente 18,96 dígitos decimais (geralmente 19 dígitos, mas às vezes apenas 18 dígitos decimais de precisão), enquanto o tipo $DOUBLE geralmente tem uma precisão em torno de 15,95 dígitos decimais (ou 53 dígitos binários). Por padrão, o Caché representa um literal numérico como um número decimal de ponto flutuante. No entanto, se o literal numérico for maior do que o que pode ser representado no decimal Caché (maior que 9,223372036854775807E145), o Caché converte automaticamente esse valor numérico em representação $DOUBLE.

Um valor numérico maior que 1,7976931348623157081E308 (308 ou 309 dígitos) resulta em um erro <`MAXNUMBER`>

Devido à conversão automática de ponto flutuante decimal para ponto flutuante binário, o comportamento de arredondamento muda em 9,223372036854775807E145 (146 ou 147 dígitos, dependendo do inteiro). Isso é mostrado nos exemplos a seguir:

```
  TRY {
    SET a=1
    FOR i=1:1:310 {SET a=a_1 WRITE i+1," digits = ",+a,! }
  }
  CATCH exp { WRITE "In the CATCH block",!
              IF 1=exp.%IsA("%Exception.SystemException") {
                  WRITE "System exception",!
                  WRITE "Name: ",$ZCVT(exp.Name,"O","HTML"),!
                  WRITE "Location: ",exp.Location,!
                  WRITE "Code: "
                }
              ELSE { WRITE "Some other type of exception",! RETURN }
              WRITE exp.Code,!
              WRITE "Data: ",exp.Data,! 
              RETURN
  }
```

```
  TRY {
    SET a=9
    FOR i=1:1:310 {SET a=a_9 WRITE i+1," digits = ",+a,! }
  }
  CATCH exp { WRITE "In the CATCH block",!
              IF 1=exp.%IsA("%Exception.SystemException") {
                  WRITE "System exception",!
                  WRITE "Name: ",$ZCVT(exp.Name,"O","HTML"),!
                  WRITE "Location: ",exp.Location,!
                  WRITE "Code: "
              }
              ELSE { WRITE "Some other type of exception",! RETURN }
              WRITE exp.Code,!
              WRITE "Data: ",exp.Data,! 
              RETURN
  }
```

Você pode representar um número maior que 309 dígitos como uma cadeia de caracteres numérica. Como esse valor é armazenado como uma cadeia de caracteres em vez de um número, nem o arredondamento nem o erro <`MAXNUMBER`> se aplicam:

```
  SET a="1"
  FOR i=1:1:360 {SET a=a_"1" WRITE i+1," characters = ",a,! }
```

Expoentes que resultariam em um número com mais do que o número máximo permitido de dígitos geram um erro <`MAXNUMBER`> O maior expoente permitido depende do tamanho do número que está recebendo o expoente. Para uma mantissa de um dígito, o expoente máximo é 307 ou 308.

Para obter mais detalhes sobre considerações sobre grandes números ao usar números decimais Caché ou números duplos IEEE, consulte o apêndice "Computação Numérica em Aplicações InterSystems" no Guia de Orientação de Programação do Caché.

## Objetos

Um valor de objeto refere-se a uma instância de um objeto na memória. Você pode atribuir uma referência de objeto (OREF) a qualquer variável local:

```
  ZNSPACE "SAMPLES"
  SET myperson = ##class(Sample.Person).%New()
  WRITE myperson
```
Para fazer referência aos métodos e propriedades de uma instância de objeto, use a sintaxe de ponto:

```
 SET myperson.Name = "El Vez"
```

Para determinar se uma variável contém um objeto, use a função $ISOBJECT

```
 SET str = "A string" 
 SET myperson = ##class(Sample.Person).%New()

 IF $ISOBJECT(myperson) {
     WRITE "myperson is an object.",!
 } ELSE {
     WRITE "myperson is not an object."
 }
 
 IF $ISOBJECT(str) {
      WRITE "str is an object."
 } ELSE {
     WRITE "str is not an object."
 }
```

Não é possível atribuir um valor de objeto a um global. Isso resulta em um erro de tempo de execução.

A atribuição de um valor de objeto a uma variável (ou propriedade de objeto) tem o efeito colateral de incrementar a contagem de referência interna do objeto, conforme mostrado no exemplo a seguir:

```
  ZNSPACE "SAMPLES"
  SET x = ##class(Sample.Person).%New()
  WRITE x,!
  SET y = ##class(Sample.Person).%New()
  WRITE y,!
  SET z = ##class(Sample.Person).%New()
  WRITE z,!
```


Quando o número de referências a um objeto atinge 0, o sistema destrói automaticamente o objeto (invoca seu método de retorno de chamada %OnClose() KEY=GOBJ_callbacks#GOBJ_cb_onclose e o remove da memória).

## Matrizes multidimensionais persistentes (globais)

Um global é uma matriz de banco de dados esparsa e multidimensional. Um global não é diferente de qualquer outro tipo de matriz, com a exceção de que o nome da variável global começa com um acento circunflexo (^). Os dados podem ser armazenados em um global com qualquer número de subscritos; os subscritos no Caché são digitáveis.

Veja a seguir um exemplo de uso de um global. Depois de definir o ^x global, você pode examinar seu valor:

```
 SET ^x = 10
 WRITE "The value of ^x is: ", ^x,!
 SET ^x(2,3,5) = 17
 WRITE "The value of ^x(2,3,5) is: ", ^x(2,3,5)
```

Para obter mais informações sobre globais, consulte o capítulo "Matrizes multidimensionais" neste documento e o documento Usando o Caché Globals

## Valores indefinidos

As variáveis ObjectScript não precisam ser explicitamente declaradas ou definidas. Assim que você atribuir um valor a uma variável, a variável será definida. Até essa primeira atribuição, todas as referências a essa variável são indefinidas. Você pode usar a função $DATA para determinar se uma variável está definida ou indefinida.

$DATA leva um ou dois argumentos. Com um argumento, ele simplesmente testa se uma variável tem um valor:

```
 WRITE "Does ""MyVar"" exist?",! 
 IF $DATA(MyVar) {     
         WRITE "It sure does!"  
 } ELSE {      
         WRITE "It sure doesn't!"
 }   
     
 SET MyVar = 10  
 WRITE !,!,"How about now?",! 
 IF $DATA(MyVar) {
        WRITE "It sure does!" 
 } ELSE {
        WRITE "It sure doesn't!"
 }
```

$DATA retorna um booleano que é True (1) se a variável tem um valor (ou seja, contém dados) e que é False (0) se a variável não tem valor (ou seja, não contém dados). Com dois argumentos, ele executa o teste e define a variável do segundo argumento igual ao valor da variável testada:

```
 IF $DATA(Var1,Var2) {
    WRITE "Var1 has a value of ",Var2,".",!  
 } ELSE {
     WRITE "Var1 is undefined.",!
 }
 
 SET Var1 = 3
 IF $DATA(Var1,Var2) {
    WRITE "Var1 has a value of ",Var2,".",!  
 } ELSE {
     WRITE "Var1 is undefined.",!
 }
```

## Valores Booleanos

Em certos casos, como quando usado com comandos lógicos ou operadores, um valor pode ser interpretado como um valor booleano (verdadeiro ou falso). Nesses casos, uma expressão é interpretada como 1 (verdadeiro) se avaliada como um valor numérico diferente de zero ou 0 (falso) se for avaliada como um valor numérico zero. Uma cadeia numérica é avaliada como seu valor numérico; Uma cadeia de caracteres não numérica é avaliada como 0 (false).

Por exemplo, os seguintes valores são interpretados como true:

```
  IF 1 { WRITE "evaluates as true",! }
       ELSE { WRITE "evaluates as false",! }
  IF 8.5 { WRITE "evaluates as true",! }
       ELSE { WRITE "evaluates as false",! }
  IF "1 banana" { WRITE "evaluates as true",! }
       ELSE { WRITE "evaluates as false",! }
  IF 1+1 { WRITE "evaluates as true",! }
       ELSE { WRITE "evaluates as false",! }
  IF -7 { WRITE "evaluates as true",! }
       ELSE { WRITE "evaluates as false",! }
  IF +"007"=7 { WRITE "evaluates as true",! }
       ELSE { WRITE "evaluates as false",! }
```

Os seguintes valores são interpretados como false:

```
  IF 0 { WRITE "evaluates as true",! }
      ELSE { WRITE "evaluates as false",! }
  IF 3-3 { WRITE "evaluates as true",! }
      ELSE { WRITE "evaluates as false",! }
  IF "one banana" { WRITE "evaluates as true",! }
      ELSE { WRITE "evaluates as false",! }
  IF "" { WRITE "evaluates as true",! }
      ELSE { WRITE "evaluates as false",! }
  IF -0 { WRITE "evaluates as true",! }
      ELSE { WRITE "evaluates as false",! }
  IF "007"=7 { WRITE "evaluates as true",! }
       ELSE { WRITE "evaluates as false",! }
```

Para obter mais detalhes sobre a avaliação de uma cadeia de caracteres como um número, consulte Conversão de cadeia de caracteres para número no capítulo "Operadores e expressões" deste livro.

## Datas

ObjectScript não tem nenhum tipo de data interno; em vez disso, ele inclui várias funções para operar e formatar valores de data representados como cadeias de caracteres. Esses formatos de data incluem:

Formatos de data

| Formato            | Descrição                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| ------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| $HOROLOG           | Esse é o formato retornado pela variável especial $HOROLOG ($H). É uma cadeia de caracteres contendo dois inteiros separados por vírgula: o primeiro é o número de dias desde 31 de dezembro de 1840; o segundo é o número de segundos desde a meia-noite do dia atual. $HOROLOG não oferece suporte a frações de segundos. A função $NOW fornece datas em formato de $HOROLOG com frações de segundos. O Caché disponibiliza uma série de funções para formatação e validação de datas em $HOROLOG formato.                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| Data ODBC          | Este é o formato usado pelo ODBC e muitas outras representações externas. É uma cadeia de caracteres da forma: "AAAA-MM-DD HH:MM:SS". Os valores de data ODBC serão agrupados; ou seja, se você classificar dados por formato de data ODBC, eles serão automaticamente classificados em ordem cronológica.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
| Data da localidade | Esse é o formato usado pela localidade atual. As localidades diferem na forma como formatam datas da seguinte maneira:<br><br>As datas "americanas" são formatadas mm/dd/aaaa (formato de data 1). As datas "europeias" são formatadas dd/mm/aaaa (dateformat 4). Todas as localidades usam dateformat 1, exceto as seguintes — csyw, deuw, engw, espw, eurw, fraw, itaw, mitw, ptbw, rusw, skyw, svnw, turw, ukrw — que usam dateformat 4.<br><br>As datas americanas usam um ponto (.) como separador decimal para frações de segundos. As datas europeias usam uma vírgula (,) como separador decimal para frações de segundos, exceto as seguintes — engw, eurw, skyw — que usam um ponto.<br><br>Todas as localidades usam uma barra (/) como o caractere separador de data, exceto as seguintes, que usam um ponto (.) como o caractere separador de data — tcheco (csyw), russo (rusw), eslovaco (skyw), esloveno (svnw) e ucraniano (ukrw). |
| Tempo do Sistema   | Esse é o formato retornado pela variável especial ZHOROLOG ($ZH). É um número de ponto flutuante que contém o número de segundos (e partes deles) que o sistema está executando. Parar e reiniciar o Caché redefine esse número. Normalmente, esse formato é usado para operações de temporização e teste.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |

O exemplo a seguir mostra como você pode usar os diferentes formatos de data:

```
 SET now = $HOROLOG
 WRITE "Current time and date ($H): ",now,!

 SET odbc = $ZDATETIME(now,3)
 WRITE "Current time and date (ODBC): ",odbc,!

 SET ldate = $ZDATETIME(now,-1)
 WRITE "Current time and date in current locale format: ",ldate,!

 SET time = $ZHOROLOG
 WRITE "Current system time ($ZH): ",time,!
```