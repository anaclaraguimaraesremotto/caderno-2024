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

$AssertEquals (arg1, arg2, test_description) 
Retorna true se arg1 e arg2 forem iguais. 
do $AssertEquals (##class(MyPackage.MyClassToBeTested).Add(2,3), 5, “Test Add(2,3) = 5”)
$AssertNotEquals (arg1, arg2, test_description) Retorna verdadeiro se arg1 e arg2 não forem iguais. do $AssertNotEquals (##class(MyPackage.MyClassToBeTested). Add(3,4), 5, "Test Add(3,4) '= 5") $AssertStatusOK (arg1, test_description) Retorna true se o código de status retornado for 1. do $AsserStatusOK(##class(MyPackage.MyClassToBeTested). SaveContact(valid_contact_ID), "Testar se o contato válido foi salvo") $AssertStatusNotOK (arg1, test_description) Retorna true se o código de status retornado não for 1. do $AssertStatusNotOK(##class(MyPackage.MyClassToBeTested). SaveContact(invalid_contact_ID), "Testar se o contato inválido não foi salvo") $AssertTrue (arg1, test_description) Retorna true se a expressão for true. do $AssertStatusTrue(##class(MyPackage.MyClassToBeTested). IsContactValid(valid_contact_ID), "Testar se o contato válido é válido") $AssertNotTrue (arg1, test_description) Retorna true se a expressão não for verdadeira. do $AssertStatusNotTrue(##class(MyPackage.MyClassToBeTested). IsContactValid(invalid_contact_ID), "Testar se o contato inválido não é válido") $AssertFilesSame (arg1, arg2, test_description) Retorna true se dois arquivos forem idênticos. do $AssertFilesSame(##class(MyPackage.MyClassToBeTested). FetchFile(URL), control_file, "Testar se o arquivo buscado é idêntico ao arquivo de controle") $AssertFilesSQLUnorderedSame (arg1, arg2, test_description) Retorna true se dois arquivos contendo resultados de consulta SQL contiverem os mesmos resultados não ordenados. do $AssertFilesSQLUnorderedSame(output.log,reference.log,"Comparando output.log com reference.log") $AssertSuccess(test_description) Registra incondicionalmente o sucesso. Essa asserção destina-se a substituir a convenção de passar 1 para $AssertTrue $AssertFailure(test_description) Falha de log incondicional. Essa afirmação destina-se a substituir a convenção de passar 0 para $AssertTrue. $AssertSkipped(test_description) Registra uma mensagem informando que o teste foi ignorado pelo motivo descrito na test_description. Isso pode ser usado, por exemplo, se as pré-condições para um teste não forem atendidas.
### %UnitTest.TestCase Class Preparation and Cleanup Methods
#### Example: Preparation Method
#### Example Cleanup Method