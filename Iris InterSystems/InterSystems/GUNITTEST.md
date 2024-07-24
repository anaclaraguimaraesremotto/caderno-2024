# The %UnitTest Framework for InterSystems IRIS

## About the InterSystems IRIS %UnitTest Framework
%UnitTest é a estrutura de teste unitário do InterSystems IRIS. Os desenvolvedores familiarizados com as estruturas xUnit acharão as estruturas contidas no %UnitTest familiares: 
- Crie testes de unidade estendendo a classe %UnitTest.TestCase, adicionando métodos de teste. Consulte Estendendo a classe %UnitTest.TestCase para obter detalhes. 
- Execute tarefas de preparação e limpeza adicionando código a métodos especiais de limpeza e preparação na classe %UnitTest.TestCase. Consulte Métodos de preparação e limpeza da classe %UnitTest.TestCase para obter detalhes. 
- Use o método RunTests() na classe %UnitTest.Manager para executar seus testes. Os resultados gerais aparecem na janela do terminal. Consulte Executando testes de unidade usando os métodos %UnitTest.Manager para obter detalhes. •
- 
- Visualize a página da Web de resultados do teste no Portal de Gerenciamento para obter informações mais detalhadas. Consulte Exibindo relatórios %UnitTest no Portal de Gerenciamento para obter detalhes. 

O pacote %UnitTest inclui as seguintes classes: 
- TestCase — Estenda esta classe para criar sua classe de teste e, em seguida, adicione métodos de classe que contenham seus testes de unidade. 
- Gerenciador — Contém métodos para executar seus testes de unidade. 
- Relatório — Controla a saída do teste, incluindo uma página da Web de resultados de teste.

## Creating Test Cases:The %UnitTest.TestCase Class
Este é o fluxo de trabalho geral para configurar testes de unidade usando a estrutura %UnitTest: 
1. Estenda a classe %UnitTest.TestCase, adicionando um método de teste para cada método a ser testado. Os nomes dos métodos de teste devem começar com a palavra Teste. Consulte Estendendo a classe %UnitTest.TestCase. 
2. Um único método de teste pode conter vários testes. Normalmente, um método de teste conterá um teste para cada aspecto do método a ser testado. Dentro de cada método de teste, crie um ou mais testes usando as macros $AssertX. Normalmente, a macro chamará o método a ser testado, comparando sua saída com algum valor esperado. Se o valor esperado corresponder à saída da macro, o teste será considerado bem-sucedido. Consulte Macros da classe %UnitTest.TestCase. 
3. Adicione código aos métodos de preparação e limpeza para executar as tarefas necessárias. Por exemplo, se um teste procurar excluir um elemento de uma lista, essa lista deverá existir primeiro e deverá conter o elemento a ser excluído. Consulte Métodos de preparação e limpeza da classe %UnitTestTestCase. 

**Observação:** os métodos de preparação e os métodos de limpeza também são chamados de métodos de configuração e métodos de desmontagem
### Extending the %UnitTest.TestCase Class
Crie uma classe que estenda %UnitTest.TestCase para conter os métodos de teste que executam seus testes de unidade. Este processo foi projetado para ser flexível, para acomodar suas necessidades específicas de teste. 


Provavelmente, você adicionará métodos de teste e também poderá adicionar propriedades. Os métodos de teste serão executados pelo método RunTests() da classe %UnitTest.Manager, que procura e executa métodos cujos nomes começam com 'Test'. Você pode adicionar outros métodos auxiliares à sua classe, mas um método será executado como um teste de unidade quando você chamar RunTests() somente se seu nome começar com 'Test'. 

**Nota:** Os métodos de teste são executados em ordem alfabética, portanto, por exemplo, TestAssess() seria executado antes de TestCreate(). 

Dentro de um método de teste, crie um ou mais testes. Use uma macro $AssertX para cada teste. Consulte Macros da classe %UnitTest.TestCase para obter detalhes sobre macros $AssertX. 

Você pode decidir criar um método de teste para cada método de classe que deseja testar. Por exemplo, suponha que sua classe MyPackage.MyClassToBeTested contenha um método Add(), que chama vários testes — talvez você queira criar o método de teste MyTests.TestAdd() para conter o código que executa os testes necessários.

Você também pode testar instâncias de objeto. Nesse caso, você criaria um método como MyTests.TestMyObject(), que poderia conter testes para garantir que as propriedades e a funcionalidade do objeto estejam corretas. 

Além de criar métodos de teste, talvez você queira criar propriedades em sua classe estendida. Isso permite que seus métodos de teste compartilhem informações. Considere os seguintes pontos ao adicionar propriedades: 
- Declare suas propriedades personalizadas na própria classe. 
- Defina as propriedades adicionando código aos métodos de preparação OnBeforeOneTest() e OnBeforeAllTests(), usando .. <`property`> 
- Acesse as propriedades adicionando código aos seus métodos de teste e/ou aos métodos de limpeza OnAfterOneTest() e OnAfterAllTests(), usando .. <`property`> sintaxe. 

**Observação:** por exemplo, se sua propriedade personalizada for chamada de PropertyValue, você a definirá ou acessará usando ```.. PropertyValue. ```
#### Example: Extended %UnitTest.TestCase Class
![[Pasted image 20240724142545.png]]
### Macros of the %UnitTest.TestCase Class

Em cada um dos métodos de teste, use uma das seguintes macros $AssertX para testar cada aspecto testável do método de classe. Por exemplo, se um método de teste for projetado para testar o método Add(), ele poderá conter um teste, usando $AssertEquals, para garantir que ele adicione 2+3 igual a 5, e um segundo teste, usando $AssertNotEquals, para garantir que ele não adicione 3+4 igual a 5. 

Selecione a macro que melhor corresponde ao resultado do teste desejado. Outra maneira de pensar nesse princípio é escrever seu teste a partir da perspectiva de que a afirmação seja bem-sucedida. Se você espera que dois valores sejam iguais, use $AssertEquals; Se você espera que os valores não sejam iguais, use $AssertNotEquals. 

Um teste falhará se a macro $AssertX especificada retornar false; caso contrário, o teste será aprovado. 

As macros $AssertX podem receber os seguintes argumentos: 
- arg1 — Normalmente, a saída do método que está sendo testado ou um valor calculado a partir dessa saída. 
- arg2 — Quando presente, um valor comparado pela macro com arg1. 
- test_description — Uma string que aparece na lista de resultados do teste exibida e descreve o que a macro testou. Isso não tem efeito sobre o resultado do teste. Não se esqueça de que esse argumento pode incluir concatenações, variáveis e métodos. Por exemplo, seu valor pode ser: 
> "Falha ao criar" _ maxObjects _ "objects: " _ $system. Status.GetErrorText(status)


***$AssertEquals (arg1, arg2, test_description)***
	Retorna true se arg1 e arg2 forem iguais. 
	```do $AssertEquals (##class(MyPackage.MyClassToBeTested).Add(2,3), 5, “Test Add(2,3) = 5”)```

***$AssertNotEquals (arg1, arg2, test_description)*** 
	Retorna verdadeiro se arg1 e arg2 não forem iguais. 
	```do $AssertNotEquals (##class(MyPackage.MyClassToBeTested). Add(3,4), 5, "Test Add(3,4) '= 5") ```

***$AssertStatusOK (arg1, test_description)*** 
	Retorna true se o código de status retornado for 1. 
	```do $AsserStatusOK(##class(MyPackage.MyClassToBeTested). SaveContact(valid_contact_ID), "Testar se o contato válido foi salvo") ```

***$AssertStatusNotOK (arg1, test_description)*** 
	Retorna true se o código de status retornado não for 1. 
	```do $AssertStatusNotOK(##class(MyPackage.MyClassToBeTested). SaveContact(invalid_contact_ID), "Testar se o contato inválido não foi salvo") ```

***$AssertTrue (arg1, test_description)*** 
	Retorna true se a expressão for true. 
	```do $AssertStatusTrue(##class(MyPackage.MyClassToBeTested). IsContactValid(valid_contact_ID), "Testar se o contato válido é válido") ```

***$AssertNotTrue (arg1, test_description)*** 
	Retorna true se a expressão não for verdadeira. 
	```do $AssertStatusNotTrue(##class(MyPackage.MyClassToBeTested). IsContactValid(invalid_contact_ID), "Testar se o contato inválido não é válido") ```

***$AssertFilesSame (arg1, arg2, test_description)*** 
	Retorna true se dois arquivos forem idênticos. 
	```do $AssertFilesSame(##class(MyPackage.MyClassToBeTested). FetchFile(URL), control_file, "Testar se o arquivo buscado é idêntico ao arquivo de controle") ```

***$AssertFilesSQLUnorderedSame (arg1, arg2, test_description)*** 
	Retorna true se dois arquivos contendo resultados de consulta SQL contiverem os mesmos resultados não ordenados. 
	```do $AssertFilesSQLUnorderedSame(output.log,reference.log,"Comparando output.log com reference.log") ```

***$AssertSuccess(test_description)***
	Registra incondicionalmente o sucesso. Essa asserção destina-se a substituir a convenção de passar 1 para $AssertTrue 

***$AssertFailure(test_description)*** 
	Falha de log incondicional. Essa afirmação destina-se a substituir a convenção de passar 0 para ```$AssertTrue. ```

***$AssertSkipped(test_description)***
	Registra uma mensagem informando que o teste foi ignorado pelo motivo descrito na test_description. Isso pode ser usado, por exemplo, se as pré-condições para um teste não forem atendidas.

**Observação:** OnBeforeAllTests() não é compatível com essa macro. Chamadas para $AssertSkipped em OnBeforeAllTests() podem resultar em falsos positivos. 

***$LogMessage (mensagem)***
	Grava o valor da mensagem como uma entrada de log, independente de qualquer teste específico. Isso pode, por exemplo, ser muito útil para fornecer contexto e organização em seu log. 
	```do $LogMessage("-- TODOS OS OBJETOS DE TESTE CRIADOS -- ")``` 

**Observação:** para obter a lista mais recente de macros, consulte %UnitTest.TestCase na Referência de classe.
### %UnitTest.TestCase Class Preparation and Cleanup Methods

%UnitTest.TestCase inclui métodos de preparação e limpeza para seus testes. Você pode adicionar código a esses métodos para executar tarefas de preparação, como criar conexões de banco de dados ou inicializar um banco de dados com dados de teste, ou para executar tarefas de limpeza, como fechar conexões de banco de dados ou restaurar o estado do banco de dados.

***OnBeforeOneTest()*** 
	Executa imediatamente antes de cada método de teste na classe de teste. 
***OnBeforeAllTests()*** 
	Executa apenas uma vez, antes de qualquer método de teste na classe de teste. 
***OnAfterOneTest()*** 
	Executa imediatamente após cada método de teste na classe de teste. 
***OnAfterAllTests()***
	Executa apenas uma vez, depois que todos os métodos de teste na classe de teste foram executados
#### Example: Preparation Method
O código neste método será executado uma vez, antes da execução do conjunto de testes. Ele cria um único contato para uso durante o teste. 
Para executar tarefas de preparação várias vezes, uma vez antes de cada teste no pacote, adicione código a OnBeforeOneTest().

![[Pasted image 20240724151126.png]]
#### Example Cleanup Method
O código neste método será executado uma vez, após a execução de todo o conjunto de testes. Ele mata todos os contatos na medida em que o teste é concluído. Para executar tarefas de limpeza várias vezes, uma vez após cada teste no pacote, adicione código a OnAfterOneTest().

![[Pasted image 20240724151209.png]]

## Executing Unit Tests Using the %UnitTest.Manager Methods

Inicie testes usando os métodos incluídos na classe %UnitTest.Manager. 

Este é o fluxo de trabalho geral para executar testes de unidade usando a estrutura %UnitTest: 
1. Informe ao sistema onde encontrar seus testes definindo o ^UnitTestRoot global: USER>set ^UnitTestRoot = "C:UnitTests" 
2. Execute seus testes usando o método RunTests() ou DebugRunTestCase() da classe %UnitTest.Manager: USER>do ##class(%UnitTest.Manager). RunTests("MeusTestes") 
3. Visualize os resultados de seus testes. 

**Nota:** Por padrão, RunTests() carrega todas as classes de teste encontradas no diretório ^UnitTestRoot, compila-as, executa todos os testes que elas contêm e as exclui da memória. 

No entanto, esse pode não ser o paradigma mais eficiente quando você está desenvolvendo seu código. Ou seja, talvez você não queira recarregar e recompilar seus testes toda vez que fizer uma pequena alteração no método que está sendo testado. Como resultado, as classes de teste de unidade geralmente são armazenadas externamente. Você pode usar os argumentos do método RunTests() para controlar explicitamente se os testes devem ser carregados e de onde, se devem ser excluídos e outras considerações. Consulte Argumentos de RunTest() e DebugRunTestCase() para obter detalhes.

### %UnitTest Test Execution Methods
O comportamento padrão ao executar testes de unidade é que os testes sejam carregados no InterSystems IRIS, compilados, executados e excluídos. Isso evita que o código de teste sobrecarregue seu namespace InterSystems IRIS. Para se desviar desse comportamento padrão, você pode usar DebugRunTestCase() ou adicionar sinalizadores ao argumento qualifiers de qualquer um desses métodos. Por exemplo, talvez você queira desenvolver seus casos de teste localmente, dentro do namespace, sem precisar recarregá-los toda vez que fizer uma alteração. Nesse caso, você pode passar o sinalizador /nodelete como parte do argumento dos qualificadores. 

***RunTest ("testSpec", "qualifiers", "userparam")***
	Executa um teste ou conjunto de testes dentro do diretório especificado no diretório global ^UnitTestRoot. Uma vez que os testes são executados, exclui do InterSystems IRIS todos os testes e classes de teste carregados.
	``` USER>Do ##class(%UnitTest.Manager).RunTest("MyTests")```
#### Arguments of RunTest() and DebugRunTestCase()

**testSpec**
	Você pode executar testes de uma ou mais classes de teste em um conjunto de testes. O argumento testSpec determina quais testes executar e onde encontrá-los. Se nenhum valor for passado, o sistema localizará todas as classes de teste em ^UnitTestRoot e seus subdiretórios e executará todos os testes dentro dessas classes. O formato geral é ```testSuite[:testCase[:testMethod][; testcase[:testmethod]]...].``` Observe que os pares testCase:testMethod são separados por ponto-e-vírgula. Por exemplo: 
	```MyTestSuite:MyFirstTestCase:MyFitrstMe hod;MeuSegundoTesteCaso:MeuSegundoMétodo; MyThirdTestCase:MyThirdMethod``` 
	Você pode usar a mesma sintaxe para incluir mais de um teste da mesma classe; Para fazer isso, especifique a classe para cada método. Por exemplo: 
	```MyTestSuite:MyFirstTestCase:OneMethod; MyFirstTestCase:AnotherMethod```
	 Os argumentos são os seguintes: 
	 - testSuite 
		 Um diretório contendo arquivos de classe de teste. O diretório deve ser um subdiretório do diretório ^UnitTestRoot. Por padrão, o gerenciador de testes executa todos os testes em todos os arquivos contidos nesse diretório e em seus subdiretórios. 
	 - testCase 
		 Cada testCase incluído especifica uma única classe contendo métodos de teste a serem executados. A sintaxe é PackageName.ClassName. Quando esse argumento é especificado, o gerenciador de testes executa apenas os testes nas classes nomeadas. 
	 - testMethod 
		 Cada testMethod incluído destaca um método da classe de teste indicada pelo testCase associado a ser executado. 
		 
**qualificadores** 
	Especifica várias opções para executar o teste. A sintaxe geral é /option1/option2/.../optionN. Você pode incluir os seguintes qualificadores: 
	- /load 
		Carregue o teste de um diretório. Use /noload para não carregar nenhum teste e executar os testes já contidos no InterSystems Iris. O padrão é /load. 
	- /run 
		Execute o teste. Use /norun para carregar, mas não executar nenhum teste. O padrão é /run. 
	- /delete 
		Exclua o caso de teste do InterSystems IRIS após a execução. Use /nodelete para salvar a classe. O padrão é /delete. 
	- /recursive 
		Procure testes em subdiretórios do diretório nomeado. Use /norecursive para não executar testes em subdiretórios. O padrão é /recursive. 
	- /debug 
		Com /debug, nenhum teste é executado após a primeira falha de teste. Ao executar a partir do Terminal, o Terminal entrará no modo de depuração após a primeira falha. Para evitar esse comportamento, use /nodebug ou simplesmente não use /debug. O padrão é /nodebug. 
	- /autoload 
		Use /autoload=dir para carregar testes do subdiretório dir do diretório ^UnitTestRoot. O subdiretório padrão é _autoload. 
	- /display 
		Use /display=all para exibir informações estendidas quando o método for executado. Use /display=none para exibir informações limitadas. O padrão é /display=all. 
		
**userparam**
	Um argumento arbitrário passado pelo chamador. O gerenciador reconhece um valor, /log, por padrão. Isso informa ao gerente para minimizar a saída no terminal e gravar os resultados detalhados em um arquivo chamado UNITTEST. LOG no <`install-dir`>diretório /mgr
## Viewing %UnitTest Results
Você pode visualizar os resultados de seus testes de qualquer uma das seguintes maneiras: 
- No console — Os resultados do teste básico são impressos na saída do console. 
- Na tabela %UnitTest.Result.TestAssert — Os resultados do teste são armazenados em forma de tabela em ^UnitTest.Result e podem ser acessados por meio da tabela %UnitTest.Result.TestAssert. 
- No Portal de Gerenciamento — A execução de um teste de unidade gera um relatório de teste que compreende uma série de páginas da Web. Os relatórios de teste são organizados por namespace e podem ser exibidos no Portal de Gerenciamento, na área do Portal UnitTest. Consulte Exibindo relatórios %UnitTest no Portal de Gerenciamento para obter detalhes.

### Viewing %UnitTest Results Programmatically
As declarações de teste, incluindo os resultados do teste, são registradas na tabela %UnitTest.Result.TestAssert para acesso estruturado aos dados. A tabela inclui os seguintes campos: 
***Status***
	O valor de êxito da falha da declaração de teste. Os valores possíveis são os seguintes:
	![[Pasted image 20240724152725.png]]
***action***
	O nome da macro $AssertX usada para executar o teste. Observe que os $ iniciais não estão incluídos na tabela.
***descripton***
	O valor do argumento test_description que você passou para a macro $AssertX. Se test_description não foi passado, esse campo usa como padrão a representação de cadeia de caracteres do primeiro argumento para a macro $AssertX. Consulte Macros da classe %UnitTest.TestCase para obter detalhes sobre $AssertX argumentos de macro.
***location***
	O local na classe de teste da qual a declaração de teste se origina, em ```label[+offset]^[|" ns"|]``` formato doc.ext

#### Troubleshooting Test Assert Locations
Em determinadas circunstâncias, é possível que os locais de declaração de teste não sejam mapeados corretamente de volta para as classes. Por exemplo, se todos os locais estiverem em rotinas INT geradas. Nesses casos, você deve executar seus testes com os qualificadores /keepsource e /generatemap no argumento qualifiers para RunTest(). Isso permite que o gerenciador de teste resolva os locais de rotina de volta para as classes de origem.

### Viewing %UnitTest Reports in the Management Portal

A execução de testes gera um relatório hierárquico, disponível no Portal de Gerenciamento, contendo resultados relacionados a todos os testes executados. 

Se o relatório indicar que um teste foi aprovado, isso significa que a macro $AssertX relevante retornou true: Seu teste produziu o resultado esperado. A falha de teste indica que a macro retornou false: seu teste não produziu o resultado esperado e talvez seja necessário depurar o método que está sendo testado. 

Siga estas etapas para exibir o relatório no Portal de Gerenciamento: 
1. Conceda acesso às classes %UnitTest para acessar o Portal UnitTest no namespace 
	```USER: USER>set $namespace = "%SYS"```
	```%SYS>set ^SYS("Security", "CSP", "AllowPrefix", "/csp/user/", "%UnitTest.") =1``` 
	
	**Observação:** esta etapa deve ser executada uma vez, por motivos de segurança, ou você não poderá navegar até o Portal de Teste de Unidade no Portal de Gerenciamento. 
2. No Portal de Gerenciamento, navegue até System Explorer > Tools > UnitTest Portal e volte para o namespace USER. 
3. Para iniciar o UnitTest Portal e visualizar seu relatório de teste, clique em Ir. Seu relatório é exibido. 
4. Faça uma busca detalhada no relatório seguindo os links no relatório para encontrar informações cada vez mais específicas. 
	- A primeira página fornece um resumo de todos os conjuntos de testes. 
	- A segunda página exibe os resultados de cada conjunto de testes. 
	- A terceira página exibe os resultados de cada caso de teste. 
	- A quarta página exibe os resultados discriminados por método de teste. 
	- A página final exibe os resultados para cada macro $AssertX usada em um método de teste.