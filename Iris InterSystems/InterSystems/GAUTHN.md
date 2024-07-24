## Overview of Authentication Mechanisms

### About Authentication
A autenticação verifica a identidade de qualquer usuário ou outra entidade que tente se conectar ao InterSystems IRIS.® Como se costuma dizer, a autenticação é como você prova que é quem diz ser.

Uma vez autenticado, um usuário estabeleceu comunicações com o InterSystems IRIS, para que seus dados e ferramentas estejam disponíveis. Sem autenticação confiável, a autorização é discutível - um usuário pode se passar por outro e tirar proveito dos privilégios obtidos de forma fraudulenta
### Authentication Mechanisms
Há várias maneiras diferentes de autenticar um usuário; cada um é conhecido como um mecanismo de autenticação. O InterSystems IRIS suporta vários mecanismos de autenticação:

- Kerberos — O protocolo Kerberos foi projetado para fornecer autenticação segura a serviços em uma rede não segura. O Kerberos usa tíquetes para autenticar um usuário e evita a troca de senhas pela rede. 
- Baseado no sistema operacional — A autenticação baseada no sistema operacional usa a identidade do sistema operacional para cada usuário para identificar esse usuário para o InterSystems IRIS. 
- Autenticação de instância — Com a autenticação de instância, o InterSystems IRIS solicita ao usuário uma senha e compara um hash da senha fornecida com um valor armazenado. 
- Lightweight Directory Access Protocol (LDAP) — Com o Lightweight Directory Access Protocol, o InterSystems IRIS autentica o usuário com base nas informações de um repositório central, conhecido como servidor LDAP. 
- Autenticação delegada — A autenticação delegada fornece um meio para criar mecanismos de autenticação personalizados. O desenvolvedor do aplicativo controla totalmente o conteúdo do código de autenticação delegada.

Você também pode permitir que todos os usuários se conectem ao InterSystems IRIS sem realizar qualquer autenticação. Isso é conhecido como acesso não autenticado. A opção de acesso não autenticado é apropriada para organizações com perímetros fortemente protegidos ou nas quais nem o aplicativo nem seus dados são um alvo atraente para invasores.

Geralmente, se você configurar o InterSystems para permitir acesso não autenticado, é recomendável que haja acesso não autenticado exclusivamente. Se houver suporte para um mecanismo de autenticação e, em seguida, acesso não autenticado se a autenticação falhar, isso será chamado de autenticação em cascata, que é descrita em Autenticação em cascata. As circunstâncias para usar mais de um mecanismo de autenticação são descritas em Usar vários mecanismos de autenticação. O InterSystems IRIS é normalmente configurado para usar apenas um deles.
### How Authentication Works

O mecanismo de autenticação é usado pelas chamadas ferramentas de conexão. Eles especificam os meios pelos quais os usuários estabelecem sua conexão com o InterSystems IRIS. Cada ferramenta de conexão (como o Terminal, Java ou web) usa um serviço InterSystems que permite ao administrador especificar o(s) mecanismo(s) de autenticação suportado(s). (Um serviço InterSystems é um gatekeeper para conexão com o InterSystems IRIS; para obter mais informações sobre serviços, consulte Serviços.)

Existem três categorias de ferramentas de conexão, cada uma das quais é conhecida como modo de acesso. Cada modo de acesso tem suas próprias características e tem seus próprios serviços suportados. Os modos de acesso são:

- Local — O usuário interage diretamente com o executável InterSystems IRIS na máquina em que esse executável está sendo executado. 
- Cliente-Servidor — O usuário está operando um executável separado que se conecta ao InterSystems IRIS. 
- Web — O usuário tem um navegador da web e está interagindo com o InterSystems IRIS por meio de um aplicativo baseado na web.

Um usuário final usa uma ferramenta de conexão para interagir com o InterSystems IRIS em um modo de acesso específico usando um mecanismo de autenticação específico. Lembre-se de que os processos descritos neste capítulo não estabelecem acesso autenticado. Em vez disso, eles estabelecem a infraestrutura que um aplicativo usa ao autenticar usuários por meio de um mecanismo específico em um modo de acesso específico.

#### About the Different Access Modes

1. About the Different Access Modes
	Com o acesso local, o usuário final está na mesma máquina que o servidor InterSystems IRIS. Para obter acesso aos dados, o usuário executa uma imagem privada do InterSystems IRIS que está lendo e gravando na memória compartilhada. Se houver vários usuários locais, cada um terá uma cópia individual do executável InterSystems IRIS e todos os executáveis apontarão para a mesma memória compartilhada. Como o usuário e o executável estão na mesma máquina, não há necessidade de proteger ou criptografar as comunicações entre os dois, pois nada está sendo passado de um executável para outro. Como as comunicações entre o usuário e o InterSystems IRIS ocorrem em um único processo, isso também é conhecido como autenticação em processo.
	
	O acesso local está disponível para:
	
	- O terminal —  ```%Service_Console``` no Windows e  ```%Service_Terminal``` em outros sistemas operacionais 
	- Chamando — ```%Service_CallIn
2. Client-Server Access Mode
	Com acesso cliente-servidor, o executável InterSystems IRIS é o servidor e há um executável cliente que pode residir em uma máquina separada. O InterSystems IRIS aceita uma conexão, possivelmente por um fio, do cliente. Essa conexão pode usar qualquer linguagem ou protocolo compatível com o InterSystems IRIS. Esses incluem:
	- ComPort —``` %Service_ComPort``` 
	- Java — ```%Service_Bindings``` 
	- JDBC — ```%Service_Bindings``` 
	- ODBC — ```%Service_Bindings```
	- Telnet — ```%Service_Telnet```
	Todas as ferramentas de conexão oferecem suporte à autenticação por meio de Kerberos ou autenticação de instância, exceto %Service_ComPort, que oferece suporte apenas à autenticação por meio da autenticação de instância.
	
	Em cada caso, o servidor especifica os tipos de autenticação com suporte. Quando o cliente inicia o contato com o servidor, ele deve tentar usar um desses tipos suportados; caso contrário, a tentativa de conexão será rejeitada. Nem todos os tipos de autenticação estão disponíveis para todas as ferramentas de conexão.
	
3. Web Access Mode
	O modo de acesso à web suporta conexões do seguinte formato:
	 ![[Pasted image 20240724090219.png]]
	1. Um usuário solicita conteúdo ou uma ação em um navegador da web. 
	2. O navegador da Web transmite a solicitação ao servidor da Web. 
	3. O servidor web é co-localizado com o gateway web e passa a solicitação para o gateway. 
	4. O gateway passa a solicitação para o servidor InterSystems IRIS.
	
	Quando o servidor InterSystems IRIS fornece conteúdo ou executa uma ação relacionada ao usuário, todo o processo acontece na outra direção. 
	
	Para que o usuário se autentique no InterSystems IRIS, um nome de usuário e senha devem ser passados para baixo da linha. Portanto, esse modo de acesso também é conhecido como modo proxy ou conexão proxy. Uma vez que as informações chegam à máquina InterSystems IRIS, o arranjo entre usuário e servidor é semelhante ao do modo de acesso local. Na verdade, o modo de acesso à Web também usa autenticação em processo.

### Overview of Setting Up Authentication

1. Escolha um mecanismo de autenticação. Sua escolha pode ser baseada em suas necessidades de autorização e modos de acesso. 
2. Configure a autenticação de acordo com as instruções em: 
	- Autenticação Kerberos 
	- Autenticação baseada no sistema operacional 
	- Autenticação de instância 
	- Autenticação LDAP 
	- Autenticação delegada 
3. Opcionalmente, implemente a autenticação de dois fatores.
4. Opcionalmente, implemente a autenticação JSON Web Token (JWT).

Recomenda-se que cada instância do InterSystems IRIS use apenas um mecanismo de autenticação e que você escolha o mecanismo de autenticação da instância antes de instalar o InterSystems IRIS. Uma vez que a instalação tenha ocorrido, você pode começar a configurar o InterSystems IRIS para usar o mecanismo selecionado. Isso envolve várias etapas: 

- Com o Kerberos, certifique-se de que todos os usuários do InterSystems IRIS estejam listados no KDC (Centro de Distribuição de Chaves) Kerberos ou no Controlador de Domínio do Windows. 
- Com a autenticação baseada no sistema operacional, certifique-se de que todos os usuários do InterSystems IRIS apareçam na lista de sistemas operacionais. 
- Para todos os mecanismos de autenticação, configure todos os serviços suportados para usar apenas o mecanismo de autenticação selecionado. 
- Para todos os mecanismos de autenticação, desative todos os serviços não suportados. 
- Para todos os mecanismos de autenticação, configure todos os aplicativos para usar apenas o mecanismo de autenticação selecionado. 

**Nota:** Independentemente do mecanismo de autenticação selecionado, durante a inicialização e o desligamento, a autenticação do sistema operacional é sempre usada

## Kerberos Authentication

### About Kerberos Authentication
#### Kerberos Background
Para conexões extremamente seguras, o InterSystems IRIS suporta o sistema de autenticação Kerberos, que fornece um meio altamente seguro e eficaz de verificar as identidades dos usuários. O Kerberos foi desenvolvido no Instituto de Tecnologia de Massachusetts (MIT) para fornecer autenticação em uma rede não segura e proteger as comunicações que a usam contra ataques sofisticados. O aspecto mais evidente dessa proteção é que a senha de um usuário nunca é transmitida pela rede - mesmo criptografada.

Kerberos é o que é chamado de sistema de terceiros confiáveis: o servidor Kerberos contém todas as informações confidenciais de autenticação (como senhas) e é mantido em um local fisicamente seguro.

Kerberos também é:

- Testado pelo tempo — Kerberos foi originalmente desenvolvido no final dos anos oitenta. Sua arquitetura e design principais têm sido usados por muitos anos em muitos locais; As revisões subsequentes abordaram problemas que foram descobertos ao longo dos anos. 
- Disponível em todas as plataformas InterSystems IRIS suportadas — Originalmente desenvolvido para UNIX®, o Kerberos está disponível em todas as variantes do UNIX® suportadas pelo InterSystems IRIS; A Microsoft integrou o Kerberos ao Windows 2000 e às versões subsequentes do Windows. (Observe que, como o Microsoft .NET Framework não inclui suporte direto a Kerberos, o InterSystems IRIS não oferece suporte a Kerberos para o InterSystems IRIS Managed Provider for .NET.) 
- Configurável de forma flexível — Acomoda redes heterogêneas. 
- Escalável — O protocolo Kerberos minimiza o número de interações com seu Centro de Distribuição de Chaves (KDC); Isso evita que essas interações se tornem um gargalo em sistemas maiores. 
- Rápido — Como um produto de código aberto, o código Kerberos foi examinado e otimizado extensivamente ao longo dos anos. 

A autenticação Kerberos subjacente é o algoritmo de criptografia AES. AES — o Advanced Encryption Standard — é uma cifra de bloco simétrica definida publicamente e isenta de royalties que suporta tamanhos de chave de 128, 192 e 256 bits. Faz parte do Padrão Federal de Processamento de Informações dos EUA (FIPS), escolhido pelo Instituto Nacional de Padrões e Tecnologia dos Estados Unidos (NIST).

Para obter informações sobre o Kerberos, consulte o site do MIT Kerberos e sua lista de documentação disponível.
#### How Kerberos Works
No modelo Kerberos, existem vários atores diferentes. Todos os diferentes programas e pessoas que estão sendo autenticados pelo Kerberos são conhecidos como principais. O sistema Kerberos é administrado por um KDC (Centro de Distribuição de Chaves) Kerberos; no Windows, o Controlador de Domínio do Windows executa as tarefas de um KDC. O KDC emite tíquetes para os usuários para que eles possam interagir com programas, que são representados por entidades de serviço. Depois que um usuário é autenticado e tem um tíquete de serviço, ele pode usar um programa.

Especificamente, a autenticação Kerberos envolve três transações separadas:
1. O cliente recebe o que é chamado de "tíquete de concessão de tíquete" ("TGT") e uma chave de sessão criptografada. 
2. O cliente usa o TGT e a chave de sessão para obter um tíquete de serviço para o InterSystems IRIS, bem como outra chave de sessão criptografada. 
3. O cliente usa o tíquete de serviço e a segunda chave de sessão para se autenticar no InterSystems IRIS e, opcionalmente, estabelecer uma conexão protegida. 
Além de um possível prompt de senha inicial, ele foi projetado para ser invisível para o usuário
#### How InterSystems IRIS Uses Kerberos
Para garantir que o Kerberos proteja adequadamente um ambiente, todos os serviços da InterSystems que o suportam devem ter o Kerberos habilitado e aqueles que não o suportam devem ser desabilitados. A exceção a isso é que os serviços destinados a operar dentro do perímetro de segurança da InterSystems, como o ECP, não suportam Kerberos; Você pode simplesmente ativar ou desativar esses serviços, pois eles são projetados para uso em um ambiente protegido externamente.
### Overview of Configuring Kerberos
Para configurar uma instância do InterSystems IRIS para autenticação Kerberos: 
1. Certifique-se de que o InterSystems IRIS esteja configurado para ser executado como um serviço Kerberos. O procedimento varia, dependendo do sistema operacional do servidor InterSystems IRIS e do tipo de ambiente; consulte Preparando o ambiente de segurança para Kerberos para obter mais informações. 
2. Ative os mecanismos Kerberos relevantes na página Opções de Autenticação/Sessão da Web (Administração do Sistema > Segurança > Segurança do Sistema > Opções de Autenticação/Sessão da Web). 
3. Determine quais serviços serão usados para se conectar ao InterSystems IRIS e desative todos os outros serviços. Para obter uma lista de quais serviços são usados por quais ferramentas de conexão, consulte a tabela Ferramentas de conexão, seus modos de acesso e seus serviços. 
4. Para conexões cliente-servidor, especifique qual nível de segurança de conexão Kerberos o servidor exige. É assim que você determina quais recursos do Kerberos devem fazer parte das conexões que usam o serviço. Consulte Especificar níveis de segurança de conexão para obter mais informações. 
5. Para conexões cliente-servidor, execute a configuração do lado do cliente. Isso garante que o aplicativo tenha acesso às informações necessárias no tempo de execução. Essas informações incluem: 
	- O nome do principal de serviço que representa o InterSystems IRIS. 
	- Os níveis de segurança de conexão permitidos.
	Setting up this information may involve configuring a Windows preferred server or some other configuration mechanism. See Set Up a Client for more information. 
6. Specify how the authentication process obtains user credentials. This is either by checking the user’s Kerberos credentials cache or by providing a Kerberos password prompt for the user. See Obtain User Credentials for more information. 
7. To maximally secure web connections, set up secure channels for the following connections: 
	- Web browser to web server 
	- Web Gateway to InterSystems IRIS server

**Importante:** No Windows, quando conectado usando uma conta de domínio, a autenticação baseada em sistema operacional e Kerberos são as mesmas. Quando conectado localmente, o Kerberos está sujeito a um ataque de falsificação do KDC e, portanto, não é seguro nem recomendado
### About Kerberos and the Access Modes
Cada ferramenta de conexão usa um serviço para estabelecer comunicações com o InterSystems IRIS. Ele também usa um modo de acesso específico. Para garantir a proteção máxima, determine quais serviços você precisa, com base nas ferramentas de conexão que você está usando. Se você não estiver usando um serviço, desative-o.

A seguir está uma lista de ferramentas de conexão, seus modos de acesso e seus serviços:

![[Pasted image 20240724091917.png]]
#### Local
A autenticação Kerberos para um serviço local estabelece que o usuário e o InterSystems IRIS são ambos principais Kerberos válidos. Há apenas uma máquina em uso e apenas um processo nessa máquina; portanto, as páginas de configuração desses serviços no Portal permitem que você especifique se deseja usar a solicitação Kerberos (rotulada simplesmente como Kerberos no Portal de Gerenciamento) ou o cache de credenciais Kerberos.

Nesse cenário, não há conexão entre o usuário e o InterSystems IRIS, pois ambos estão usando o mesmo processo na mesma máquina. Como os dois estão compartilhando um processo, não há informações sendo passadas por um meio não seguro e, portanto, não há necessidade de oferecer proteções especiais para esses dados. (Essa situação é conhecida como autenticação em processo.)
#### Client-Server
Os aplicativos cliente-servidor incluem conexões de Java, JDBC, ODBC e por meio de Telnet. Para um aplicativo cliente-servidor usando autenticação Kerberos, o usuário precisa de credenciais para interagir com o InterSystems IRIS por meio do aplicativo. 

O servidor e o cliente exigem configuração. A configuração do servidor especifica qual tipo de conexão é aceita; A configuração do cliente especifica que tipo de conexão é tentada e também pode especificar como obter as credenciais do usuário. 

Com conexões cliente-servidor, o Kerberos suporta vários níveis de segurança de conexão, que são configurados na máquina do servidor InterSystems IRIS:

- Kerberos — Kerberos gerencia a autenticação inicial entre o usuário e o InterSystems IRIS. As comunicações subsequentes não são protegidas. 
- Kerberos com Integridade de Pacote — Kerberos gerencia a autenticação inicial entre o usuário e o InterSystems IRIS; Cada mensagem subsequente tem um hash que fornece validação de origem e conteúdo. Isso fornece verificação de que cada mensagem em cada direção é realmente de seu suposto remetente; Ele também fornece verificação de que a mensagem não foi alterada em trânsito do remetente para o destinatário. 
- Kerberos com criptografia — Kerberos gerencia a autenticação inicial, garante a integridade de todas as comunicações e também criptografa todas as comunicações. Isso envolve criptografia de ponta a ponta para todas as mensagens em cada direção entre o usuário e o InterSystems IRIS.
### Web
Ao executar uma aplicação web, o usuário não interage diretamente com o servidor InterSystems IRIS. Para proteger todas as informações do monitoramento, você precisa criptografar as conexões entre o usuário e o InterSystems IRIS da seguinte forma: 
- Configure o servidor web para que ele use TLS para proteger as conexões do navegador com ele. 
- Co-localize o servidor web e o gateway da Web, para que não haja necessidade de proteger a conexão entre eles. 
- Configure o Web Gateway para usar a autenticação e criptografia Kerberos. Utilizar a entidade principal Kerberos do Gateway para estabelecer essa ligação
A arquitetura é: 
![[Pasted image 20240724092428.png]]
Qualquer comunicação entre o usuário final e o InterSystems IRIS ocorre por meio de pipes criptografados por TLS ou Kerberos. Para conexões protegidas por Kerberos, isso inclui a autenticação Kerberos do usuário final.
Como o servidor InterSystems IRIS não pode solicitar uma senha ao usuário final, ele invoca uma API que envia conteúdo HTML ao navegador para solicitar. O usuário preenche este formulário que foi enviado; ele viaja de volta para o servidor web, que o entrega ao Web Gateway, que então o entrega ao servidor web (que faz parte do próprio InterSystems IRIS). O servidor web atua como um proxy em nome do usuário no navegador; É por isso que esse tipo de conexão é conhecido como conexão proxy. Ao mesmo tempo, todas as informações relacionadas ao usuário residem na máquina do servidor (como no modo de acesso local); Portanto, uma conexão com a web também é uma forma de autenticação em processo.

### Specify Connection Security Levels
As conexões cliente-servidor com o InterSystems IRIS usam um dos seguintes serviços: 
- %Service_Bindings — Java, JDBC, ODBC 
- %Service_Telnet — Telnet Para qualquer conexão Kerberos usando um desses serviços, você deve especificar os níveis de segurança de conexão que o servidor aceita. 
Para configurar os níveis de segurança de conexão suportados do serviço, o procedimento é:

1. Na página Opções de Autenticação/Sessão Web (Administração do Sistema > Segurança > Segurança do Sistema > Opções de Autenticação/Sessão Web), especifique quais níveis de segurança de conexão habilitar para toda a instância do InterSystems IRIS, onde estes podem ser: 
	- Kerberos — Somente autenticação inicial 
	- Kerberos com Integridade de Pacote — Autenticação inicial e integridade de pacote 
	- Kerberos com Criptografia — Autenticação inicial, integridade de pacote e criptografia de todas as mensagens 
	Para obter mais informações sobre o Opções de autenticação, consulte Opções de autenticação.
2. Na página Serviços (Administração do Sistema > Segurança > Serviços), clique no nome do serviço (na coluna Nome); isso exibe a página Editar Serviço do serviço. 
3. Na página Editar serviço, especifique quais níveis de segurança de conexão devem ser necessários como parte de uma conexão Kerberos. Depois de fazer essa seleção, clique em Salvar.

Se um cliente tentar se conectar ao servidor usando um nível de segurança inferior ao especificado para o servidor, a conexão não será aceita. Se um cliente tentar se conectar ao servidor usando um nível de segurança mais alto do que o especificado para o servidor, a conexão do servidor tentará executar a autenticação usando o nível de segurança especificado.
### Set Up a Client
Ao usar o modo de acesso cliente-servidor, você precisa configurar o cliente. As particularidades deste processo dependem da tecnologia de conexão que está sendo usada.
#### Telnet: Set Up the Preferred Server for Use with Kerberos
Com um cliente Windows, ao estabelecer uma conexão usando o InterSystems IRIS telnet para Windows, o cliente usa informações de configuração que foram armazenadas como parte de um servidor remoto.

**Importante**: O InterSystems IRIS tem seu próprio servidor telnet para Windows. Ao se conectar a uma máquina não-Windows, não há servidor telnet InterSystems IRIS disponível - você simplesmente usa o servidor telnet que vem com o sistema operacional. Depois de estabelecer a conexão com a máquina servidora, você pode iniciar o InterSystems IRIS usando o serviço %Service_Terminal

Para configurar uma conexão de cliente que chega através do telnet, vá para a máquina cliente. Nessa máquina, o procedimento é: 
1. Clique no iniciador InterSystems IRIS e selecione Servidor Preferencial no menu (a opção Servidor Preferido também exibe o nome do servidor preferencial atual). 
2. No submenu exibido, escolha Adicionar/Editar. 
3. Para criar um novo servidor remoto, clique no botão Adicionar; para configurar um servidor já existente, escolha o servidor InterSystems IRIS ao qual você está se conectando e clique no botão Editar. 
4. Isso exibe a caixa de diálogo Adicionar conexão. Na área Método de autenticação dessa caixa de diálogo, clique em Kerberos. Isso expande a caixa de diálogo para exibir vários campos adicionais. 
5. Se você estiver editando os valores de um servidor já existente, não deve haver necessidade de alterar ou adicionar valores para os campos mais gerais nesta caixa de diálogo, pois eles são determinados pelo servidor que você escolheu editar. Se você estiver adicionando um novo servidor, os campos a serem preenchidos serão descritos em Definir uma conexão de servidor remoto. 
6. Nos campos relacionados ao Kerberos da caixa de diálogo, especifique valores para os seguintes campos: 
	- O nível de segurança da conexão, onde as opções são apenas autenticação Kerberos; Autenticação Kerberos com integridade de pacotes; ou autenticação, integridade de pacotes e criptografia Kerberos 
	- O nome da entidade de serviço. Para obter informações sobre como configurar nomes de entidade de serviço, consulte Nomes e convenções de nomenclatura. 
	- Se você estiver configurando uma conexão telnet com uma máquina Windows, marque a caixa especificando que a conexão use o servidor Telnet IRIS do Windows InterSystems. 
7. Clique em OK para salvar os valores especificados e fechar a caixa de diálogo.
#### Set Up an ODBC DSN with Kerberos

O InterSystems IRIS suporta conexões ODBC Kerberizadas de clientes no Windows, UNIX® e Mac para DSNs (Data Source Nodes) em todas as plataformas. As formas de configurar o comportamento do cliente variam de acordo com a plataforma: 
- Em todas as plataformas, a função SQLDriverConnect está disponível, que aceita um conjunto de pares nome-valor. SQLDriverConnect é uma chamada C que faz parte da API ODBC e está documentada no site da Microsoft. Seus pares namevalue são os mesmos do arquivo de inicialização disponível em plataformas não Windows. 
- Em plataformas não Windows, use o arquivo de inicialização ODBC do InterSystems para especificar pares nome-valor que fornecem informações de conexão. Este arquivo é descrito geralmente em Usando o driver ODBC InterSystems. O arquivo tem as seguintes variáveis relacionadas ao Kerberos: 
	-  Método de autenticação — Especifica como o cliente ODBC se autentica no DSN. 0 especifica a autenticação da instância; 1 especifica Kerberos. 
	- Nível de segurança — Para conexões Kerberos, especifica qual funcionalidade é usada para proteger a conexão. 1 especifica que o Kerberos é usado apenas para autenticação; 2 especifica que o Kerberos é usado para autenticação e para garantir a integridade de todos os pacotes passados entre o cliente e o servidor; e 3 especifica que o Kerberos é usado para autenticação, integridade de pacotes e para criptografar todas as mensagens. 
	- Nome da Entidade de Serviço — Especifica o nome do serviço InterSystems que está servindo como DSN. Por exemplo, a entidade de serviço pode ter "iris/localhost.domain.com" como seu nome.
Os nomes dessas variáveis devem ter espaços entre as palavras. Eles não diferenciam maiúsculas de minúsculas. 
- Em um cliente Windows, você pode especificar informações de conexão por meio de uma GUI: a caixa de diálogo de configuração ODBC DSN. O InterSystems IRIS oferece opções na guia DSN do sistema. Esta tela tem ajuda associada que descreve seus campos. O caminho no menu Iniciar do Windows para exibir esta tela varia de acordo com a versão do Windows; pode ser listado em Ferramentas Administrativas. 
**Importante:** No Windows de 64 bits, há duas versões do odbcad32.exe: uma está localizada no diretório C:\Windows\System32 e a outra está localizada no diretório C:\Windows\SysWOW64. Se você estiver executando o Windows de 64 bits, configure os DSNs por meio do em C:\Windows\SysWOW64.
#### Set Up a Java or JDBC Client with Kerberos

O InterSystems IRIS fornece uma classe Java que serve como um utilitário para auxiliar na configuração do cliente Java. Execute-o quando estiver pronto para configurar o cliente. O procedimento é: 
1. Para configurar o cliente para uso com Kerberos, emita o comando Java Configure como: java –classpath '$IRIS_INSTALL_DIRECTORY/dev/java/lib/JDK18/*' com.intersystems.jgss.Configure Isso permite que Configure seja executado de qualquer local na máquina, não apenas de dentro do diretório JDK. Observe que as especificidades desse comando podem variar, dependendo do seu site, como para acomodar estilos de caminho do Windows ou usar JDK11. Este programa usa o Java Generic Security Services (JGSS) para executar as seguintes ações: 
	- Se necessário, modifica o arquivo java.security.
	- Cria ou modifica o arquivo isclogin.conf. 
2. O programa então solicita que você crie e configure o arquivo krb5.conf. Se o arquivo existir, o comando avisará se você deseja usar o krb5.conf existente ou substituí-lo; Se você optar por substituí-lo, ele solicitará as seguintes informações: 
	a. Realm Kerberos — Oferece o domínio local em minúsculas como um valor padrão para o domínio. 
	b. KDC primário — Você só precisa incluir o nome da máquina local, pois o programa acrescenta o nome da região Kerberos ao nome da máquina para você. 
	c. KDC(s) secundário(s) — Você pode especificar os nomes de zero ou mais KDCs para replicar o conteúdo do KDC primário. 
3. Depois de receber essas informações, execute o comando uma segunda vez. (Ele instrui você a fazer isso.) 
4. Quando solicitado a substituir krb5.conf, opte por deixar o arquivo existente. Em seguida, o comando testa a conexão solicitando o nome de usuário e a senha de uma entidade principal no realm Kerberos especificado. 
Se isso for bem-sucedido, a configuração do cliente será concluída
### Obtain User Credentials
Para todos os modos de acesso, você precisa especificar se o aplicativo obtém as credenciais do usuário de um cache de credenciais existente ou solicitando um nome de usuário e senha.
#### Obtain Credentials for Local Access Mode
#### Obtain Credentials for Client-Server Access Mode
1. ODBC and Telnet
2. Java and JDBC
#### Obtain Credentials for Web Access Mode
Para o modo de acesso local, as credenciais do usuário residem na mesma máquina que o InterSystems IRIS. Nessa situação, o aplicativo está usando um serviço para se conectar ao InterSystems IRIS. Isso inclui os seguintes serviços: 
- ```%Service_CallInC```
- ```%Service_Console``` 
- ```%Service_Terminal ```
Para especificar como obter credenciais, o procedimento é: 
1. Na página Serviços (Administração do Sistema > Segurança > Serviços) e selecione o serviço na coluna Nome. Isso exibe a página Editar Serviço do serviço. 
2. Na página Editar serviço, especifique como obter credenciais. Selecione a solicitação (a caixa de seleção Kerberos) ou usando um cache de credenciais (a caixa de seleção Cache de Credenciais Kerberos). Não marque ambos. 
3. Clique em Salvar para usar as configurações. 

**Observação:** se você habilitar a autenticação de cache de credenciais Kerberos (prompting) e Kerberos para o serviço, a autenticação de cache de credenciais terá precedência. Este é o comportamento especificado pelo Kerberos, não pelo InterSystems IRIS. 

No Windows com um Controlador de Domínio (a configuração provável para Windows), o logon estabelece um cache de credenciais Kerberos. No UNIX®, Linux e macOS, a condição padrão típica é não ter credenciais Kerberos, de modo que o InterSystems IRIS seja configurado para usar o prompt Kerberos; nesses sistemas, o usuário pode obter credenciais de uma das seguintes maneiras: 
- Executando o kinit antes de invocar o Terminal 
- Fazendo login em um sistema onde o processo de login executa a autenticação Kerberos para o usuário. 
Nessas situações, o InterSystems IRIS pode ser configurado para usar o cache de credenciais.

### Obtain Credentials for Client-Server Access Mode 
Para o modo de acesso cliente-servidor, as credenciais do usuário residem no computador que hospeda o aplicativo cliente. Nesse caso, a maneira como você especifica como obter credenciais varia de acordo com a forma como o cliente está se conectando: 
- ODBC e Telnet 
- Java e JDBC

1. ODBC and Telnet
	O código IRIS subjacente do InterSystems usado por essas ferramentas de conexão pressupõe que os usuários finais já tenham suas credenciais; nenhuma inspiração é necessária. 
	
	No Windows, cada usuário conectado no domínio tem um cache de credenciais. 
	
	Em outros sistemas operacionais, um usuário terá um cache de credenciais se o sistema operacional tiver executado a autenticação Kerberos para o usuário ou se o usuário tiver executado explicitamente o kinit. Caso contrário, o usuário não terá credenciais no cache e a ferramenta de conexão falhará na autenticação. 
	
	**Nota:** Nem todas as ferramentas de conexão estão disponíveis em todos os sistemas operacionais
	
2. Java and JDBC
	Ao usar Java e JDBC, há duas implementações diferentes de Java disponíveis — Oracle ou IBM. Estes têm vários comportamentos comuns e vários comportamentos diferentes. 
	
	**Nota:** As implementações IBM do Java estão disponíveis apenas até a versão 8; para versões posteriores, a IBM suporta versões de software livre.
	
	 Ambas as implementações armazenam informações sobre uma conexão em propriedades de uma instância da classe java.util.Properties. Essas propriedades são: 
	 - usuário — O nome do usuário que está se conectando ao servidor InterSystems IRIS. Esse valor é definido apenas para determinados comportamentos de conexão. 
	 - password — A senha desse usuário. Esse valor é definido apenas para determinados comportamentos de conexão. 
	 - Nome da entidade de serviço — O nome da entidade principal Kerberos para o servidor InterSystems IRIS. Esse valor é definido para todos os comportamentos de conexão. 
	 - nível de segurança da conexão — O tipo de proteção que o Kerberos fornece para essa conexão. 
		 - 1 especifica que o Kerberos é usado apenas para autenticação; 
		 - 2 especifica que o Kerberos é usado para autenticação e para garantir a integridade de todos os pacotes passados entre o cliente e o servidor; e 
		 - 3 especifica que o Kerberos é usado para autenticação, integridade de pacotes e para criptografar todas as mensagens. Esse valor é definido para todos os comportamentos de conexão. 
	Nas discussões a seguir, a instância da classe java.util.Properties é chamada de objeto connection_properties, em que o valor de cada uma de suas propriedades é definido com uma chamada para o método connection_properties.put, como:
		``` String principalName = "MyServer";``` 
		```connection_properties.put("service principal name",principalName);```
	Para ambas as implementações, o comportamento relacionado a credenciais é determinado pelo valor de um parâmetro no arquivo isclogin.conf (consulte Configurar um cliente Java ou JDBC para uso com Kerberos para obter mais informações sobre esse arquivo). 
	
	Há duas diferenças entre o comportamento das duas implementações Java: 
	- Para especificar o comportamento relacionado a credenciais, o nome do parâmetro a ser definido no arquivo isclogin.conf difere para cada implementação:
		-  Para IBM, é useDefaultCcache. 
		-  Para Oracle, é useTicketCache. 
	- Existem diferentes comportamentos disponíveis em cada implementação. Eles são descritos nas seções a seguir. 
	**Especificando o comportamento em um cliente usando a implementação IBM**
	
	As opções são: 
	- Para usar um cache de credenciais, configure o valor do parâmetro useDefaultCcache como TRUE e não configure os valores das propriedades do usuário ou da senha. Observe que, se nenhum cache de credenciais estiver disponível, uma exceção será lançada. 
	-  Para usar um nome de usuário e senha que são passados programaticamente, defina o valor do parâmetro useDefaultCcache como FALSE e defina os valores das propriedades de usuário e senha. 
	- Para solicitar um nome de usuário e senha, defina o valor do parâmetro useDefaultCcache como FALSE e não defina os valores das propriedades de usuário ou senha. Como essas propriedades não têm valores definidos, as classes das bibliotecas fornecidas com o InterSystems IRIS podem ser usadas para gerar prompts para elas. 
	**Especificando o Comportamento em um Cliente Usando a Implementação do Oracle**
	
	 As opções são:
	- Para usar exclusivamente um nome de usuário e senha que são passados programaticamente, defina o valor do parâmetro useTicketCache como FALSE e defina os valores das propriedades user e password. 
	- Para solicitar exclusivamente um nome de usuário e senha, defina o valor do parâmetro useTicketCache como FALSE e não defina os valores das propriedades de usuário ou senha. Como essas propriedades não têm valores definidos, as classes das bibliotecas fornecidas com o InterSystems IRIS podem ser usadas para gerar prompts para elas. 
	- Para usar exclusivamente um cache de credenciais, defina o valor do parâmetro useTicketCache como TRUE. Para evitar qualquer ação adicional, defina os valores das propriedades de usuário e senha como valores falsos; Isso impede que a solicitação ocorra e garante a falha de qualquer tentativa de autenticação com base nos valores das propriedades. 
	- Para tentar usar um cache de credenciais e, em seguida, usar um nome de usuário e uma senha que são passados programaticamente, defina o valor do parâmetro useTicketCache como TRUE e defina os valores das propriedades user e password. Se não houver cache de credenciais, os valores das propriedades serão usados. 
	- Para tentar usar um cache de credenciais e, em seguida, solicitar um nome de usuário e senha, defina o valor do parâmetro useTicketCache como TRUE e não defina os valores das propriedades de usuário ou senha. Se não houver cache de credenciais, as classes das bibliotecas fornecidas com o InterSystems IRIS podem ser usadas para gerar prompts para elas.

#### Obtain Credentials for Web Access Mode
Com uma conexão baseada na Web que usa Kerberos, sempre há um prompt de nome de usuário e senha. Se isso resultar em autenticação, as credenciais do usuário serão colocadas na memória e descartadas quando não forem mais necessárias.

### Set Up a Secure Channel for a Web Connection
Para proteger ao máximo uma conexão com a web, recomenda-se que as duas pernas de comunicação - tanto entre o navegador quanto o servidor web e depois entre o Web Gateway e o InterSystems IRIS - usem canais seguros. Isso garante que todas as informações, como nomes de usuário e senhas Kerberos, sejam protegidas na transmissão de um ponto para outro. Para proteger cada canal de comunicação, o procedimento é: 
- Entre o navegador da web e o servidor da web 
- Entre o Web Gateway e o InterSystems IRIS
#### Set Up a Kerberized Connection from the Web Gateway to InterSystems IRIS
Para configurar um canal seguro e criptografado entre o Web Gateway e o servidor InterSystems IRIS, você precisa de um principal Kerberos que represente o Gateway. Este princípio estabelece uma conexão criptografada com o InterSystems IRIS, e todas as informações são transmitidas através da conexão. Isso permite que um usuário final se autentique no InterSystems IRIS e evita qualquer espionagem durante esse processo. 
**Nota:** Para obter informações sobre como configurar uma conexão entre o Web Gateway e o servidor InterSystems IRIS protegido por TLS, consulte Configurando o Web Gateway para se conectar ao InterSystems IRIS usando TLS. 
O procedimento é: 
1. Determine ou escolha o nome da entidade principal Kerberos que representa o Gateway
	Para Windows, esse é o nome principal que representa a sessão de serviço de rede do host do Gateway (ou seja, o nome da máquina que hospeda o Gateway com o "$" anexado a ele — machine_name$, , como Athens$). Para outras plataformas, esse é qualquer nome de entidade válido inserido como nome de usuário na tela de configuração do Gateway; Isso identifica a chave apropriada no arquivo da tabela de chaves. 
2. Crie um usuário no InterSystems IRIS com o mesmo nome do principal Kerberos do Gateway. Para fazer isso, siga as instruções em Criar um novo usuário. 
3. Conceda a esse usuário permissões para usar, ler ou gravar quaisquer recursos necessários (também conhecidos como privilégios). Isso é feito associando esses privilégios a uma função e, em seguida, associando o usuário à função. 
4. Configure o serviço %Service_WebGateway. Para fazer isso, preencha os campos descritos em Propriedades do Serviço. 
5. Configure o Gateway para que ele possa entrar em contato com o servidor. O procedimento é: 
	a.Na home page do Portal de Gerenciamento, vá para a página Gerenciamento de Gateway da Web (Administração > Configuração do Sistema > Gerenciamento de Gateway da Web). 
	
	b. Na página de gerenciamento do Web Gateway, há um conjunto de opções à esquerda. Em Configuração, clique em Acesso ao servidor. Isso exibe a página Acesso ao servidor. 
	
	c. Na página Acesso ao servidor, você pode adicionar uma nova configuração ou editar uma existente. Para adicionar uma nova configuração, clique no botão Adicionar Servidor; para editar um existente, selecione-o na lista à esquerda, selecione o botão de opção Editar servidor e clique em Enviar. Isso exibe a página para editar ou configurar os parâmetros de acesso ao servidor. Para além dos parâmetros gerais nesta página (descritos no respetivo ecrã de ajuda), esta página permite-lhe especificar parâmetros relacionados com a segurança para o Gateway. Para conexões Kerberos, são elas: 
	
	 - Nível de segurança da conexão — Escolha o tipo de proteção que você gostaria que o Kerberos tentasse fornecer essa conexão. (Observe que isso deve corresponder ou exceder o tipo de segurança especificado para o serviço Web na etapa anterior.) 
	 - Nome de usuário — O nome da entidade principal Kerberos que representa o Gateway. (Deve ser o mesmo princípio usado na primeira etapa deste processo.) 
	 - Senha — Não especifique um valor para isso. (Este campo é utilizado ao configurar o Gateway para utilização com a autenticação de instâncias.) 
	 - Produto — InterSystems IRIS. 
	 - Nome da entidade de serviço — O nome da entidade principal que representa o servidor InterSystems IRIS. Normalmente, é um nome principal Kerberos padrão, no formato "iris/machine.domain", onde iris é uma string fixa que indica que o serviço é para InterSystems IRIS, machine é o nome da máquina e domain é o nome de domínio, como "intersystems.com". 
	 - Tabela de Chaves — Ao conectar-se a uma instância do InterSystems IRIS no Windows, deixe este campo em branco; para outros sistemas operativos, forneça o nome do ficheiro keytab que contém a chave permanente pertencente ao Gateway Web, incluindo o caminho completo. 
	 
	Depois de inserir todos esses valores, clique no botão Salvar configuração para salvá-los. 

O serviço Web agora está pronto para ser configurado. Isso significa que agora ele pode fornecer a infraestrutura subjacente necessária para dar suporte a um aplicativo Web. 

Ao criar um aplicativo Web seguro, o desenvolvedor do aplicativo precisa: 
1. Escolha um método de autenticação. 
2. Configure as funções para o aplicativo. 
3. Se necessário, certifique-se de que a conexão do navegador com o servidor web use TLS.

## Operating System-Based Authentication
### About OS-Based Authentication
O InterSystems IRIS suporta o que é chamado de autenticação baseada em sistema operacional (ou baseada em sistema operacional). Com a autenticação do sistema operacional, o InterSystems IRIS usa a identidade do usuário do sistema operacional para identificar o usuário do InterSystems IRIS. Quando a autenticação do sistema operacional está habilitada, o usuário se autentica no sistema operacional usando de acordo com os protocolos do sistema operacional. Por exemplo, no UNIX®, este é tradicionalmente um prompt de login em que o sistema operacional compara um hash da senha com o valor armazenado no arquivo /etc/passwd. Quando o usuário tenta se conectar ao InterSystems IRIS pela primeira vez, o InterSystems IRIS adquire a identidade de usuário no nível do sistema operacional do processo. Se essa identidade corresponder a um nome de usuário do InterSystems IRIS, esse usuário será autenticado. 

Esse recurso se aplica apenas a processos do lado do servidor, como aplicativos baseados em terminal (por exemplo, conexão por meio do Terminal) ou processos em lote iniciados a partir do sistema operacional. Não está disponível para um aplicativo que está se conectando ao InterSystems IRIS de outra máquina, como quando uma cópia do Studio em uma máquina está se conectando a um servidor InterSystems IRIS em outra. 

Esse mecanismo é normalmente usado para sistemas UNIX®, além do console do Windows. 

A autenticação baseada em sistema operacional está disponível apenas para processos locais, a saber: 
- Callin (```%Service_Callin```) 
- Console ```(%Service_Console```) 
- Terminal (```%Service_Terminal```)
### Configuring OS-Based Authentication
Para configurar o uso desse tipo de autenticação, o procedimento é: 
1. Na página Opções de Autenticação/Sessão da Web (Administração do Sistema > Segurança > Segurança do Sistema > Opções de Autenticação/Sessão da Web), selecione Permitir autenticação do Sistema Operacional. 
2. Para a página Serviços (Administração do Sistema > Segurança > Serviços) e selecione o serviço na coluna Nome. 
	Isso exibe a página Editar Serviço do serviço. 
3. Na página Editar serviço, escolha baseado no sistema operacional (a caixa de seleção Sistema operacional). 
	Clique em Salvar para usar as configurações.
	
Esse tipo de autenticação não requer outras ações de configuração. 
**Observação:** No Windows, quando conectado usando uma conta de domínio, a autenticação baseada no sistema operacional e Kerberos são as mesmas.
### A Note on %Service_Console
Como o console (%Service_Console) é um serviço baseado no Windows e os logons de domínio do Windows normalmente usam Kerberos, a autenticação baseada no sistema operacional do console fornece autenticação para logons locais.
### A Note on %Service_Callin
Com callin (%Service_Callin), a autenticação baseada em sistema operacional só está disponível em um prompt no nível do sistema operacional. Ao usar o callin programaticamente, a autenticação baseada no sistema operacional não é suportada — apenas o acesso não autenticado está disponível.
## Instance Authentication

### About Instance Authentication
O próprio InterSystems IRIS pode fornecer um mecanismo de login, chamado autenticação de instância. (No Portal de Gerenciamento, ela é chamada de Autorização de Senha.) Especificamente, o InterSystems IRIS mantém um valor de senha para cada conta de usuário e compara esse valor com o fornecido pelo usuário em cada login. Assim como na autenticação tradicional baseada em sistema operacional, o InterSystems IRIS armazena uma versão com hash da senha. Quando o usuário faz login, o valor da senha inserido é hash e as duas versões com hash são comparadas. O gerente do sistema pode configurar determinados critérios de senha, como comprimento mínimo, para garantir o grau desejado de robustez nas senhas selecionadas pelos usuários. Os critérios são descritos em Força da senha e Políticas de senha.

O InterSystems IRIS armazena apenas hashes criptográficos irreversíveis de senhas. Os hashes são calculados usando o algoritmo PBKDF2 com a função pseudoaleatória HMAC-SHA-512, conforme definido no Padrão de Criptografia de Chave Pública #5 v2.1: "Padrão de Criptografia Baseada em Senha". A implementação atual usa 10.000 iterações, 64 bits de sal e gera valores de hash de 64 bytes; para especificar um algoritmo diferente ou aumentar o número de iterações, use os métodos Security.System.PasswordHashAlgorithm e Security.System.PasswordHashWorkFactor, respectivamente. Não há técnicas conhecidas para recuperar senhas originais desses valores de hash. 

Os serviços disponíveis para autenticação com autenticação de instância são: 
- ```%Service_Binding ```
- ```%Service_CallIn ```
- ```%Service_ComPort ```
- ```%Service_Console ```
- ```%Service_Telnet ```
- ```%Service_Terminal ```
- ```%Service_WebGateway```
### Overview of Configuring Instance Authentication
Para que um serviço use a autenticação de instância, você deve configurá-lo da seguinte maneira:
1. Na página Opções de Autenticação/Sessões da Web (Administração do Sistema > Segurança > Segurança do Sistema > Opções de Autenticação/Sessão da Web), habilite a autenticação com autenticação de instância selecionando Permitir autenticação de senha). 
2. Para o serviço específico, vá para a página Serviços (Administração do Sistema > Segurança > Serviços) e selecione esse serviço, como %Service_Bindings, na coluna Nome; isso exibe a página Editar Serviço do serviço. 
3. Nesta página, escolha autenticação de instância, listada simplesmente como Senha na lista de tipos de autenticação. 
4. Clique em Salvar para salvar essa configuração. 
5. Além desse procedimento básico, alguns serviços exigem configuração adicional. Isso é descrito nas seções a seguir:
	- Web
	- ODBC
	- Telnet
### Web

Para acesso à web, você pode opcionalmente exigir que o Web Gateway se autentique no servidor InterSystems IRIS por meio da autenticação de instância. Para realizar essa configuração, o procedimento é: 
1. Na página inicial do Portal de Gerenciamento, vá para a página Gerenciamento de Gateway da Web (Administração > Configuração do Sistema > Gerenciamento de Gateway da Web). 
2. Na página de gerenciamento do Web Gateway, há um conjunto de opções à esquerda. Em Configuração, clique em Acesso ao servidor. Isso exibe a página Acesso ao servidor. 
3. Na página Acesso ao servidor, você pode adicionar uma nova configuração ou editar uma existente. Para adicionar uma nova configuração, clique no botão Adicionar Servidor; para editar um existente, selecione-o na lista à esquerda, selecione o botão de opção Editar servidor e clique em Enviar. Isso exibe a página para editar ou configurar os parâmetros de acesso ao servidor. Além dos parâmetros gerais nesta página (descritos na tela de ajuda), esta página permite que você especifique parâmetros relacionados à segurança para o Gateway. Para conexões de autenticação de instância, são elas: 
- Nível de segurança da conexão — Escolha Senha na lista suspensa para usar a autenticação de instância. 
- Nome de usuário — O nome de usuário sob o qual o serviço Gateway é executado (o processo de instalação cria o usuário CSPSystem para essa finalidade). Este usuário (CSPSystem ou qualquer outro) não deve ter data de validade; ou seja, sua propriedade Data de Expiração deve ter um valor de 0. 
- Senha — A senha associada à conta de usuário recém-inserida. 
- Produto — InterSystems IRIS. 
- Nome da entidade de serviço — Não especifique um valor para isso. (Este campo é utilizado ao configurar o Gateway para utilização com o Kerberos.) 
- Tabela de chaves — Não especifique um valor para isso. (Este campo é utilizado ao configurar o Gateway para utilização com o Kerberos.) 

Depois de inserir todos esses valores, clique no botão Salvar configuração para salvá-los. 

É importante lembrar que os requisitos de autenticação para o Gateway não estão diretamente relacionados com os de uma aplicação que utiliza o Gateway. Por exemplo, pode exigir a autenticação de instância como mecanismo de autenticação para uma aplicação Web, ao configurar o Gateway para utilizar a autenticação Kerberos — ou nenhuma autenticação. Na verdade, a escolha de um mecanismo de autenticação específico para o próprio Gateway não faz nenhum requisito técnico para o aplicativo da web, e vice-versa. Ao mesmo tempo, alguns pares são mais prováveis de ocorrer do que outros. Se um aplicativo Web usar Kerberos autenticação e, em seguida, usar qualquer outra forma de autenticação para o Gateway significa que as informações de autenticação Kerberos fluirá por um canal não criptografado, reduzindo potencialmente sua eficácia.

Com um aplicativo Web que usa autenticação de instância, o nome de usuário e a senha do usuário final são passados do navegador para o servidor Web, que os entrega ao Gateway da Web colocalizado. Como o Gateway tem sua própria conexão para o servidor InterSystems IRIS, ele então passa o nome de usuário e a senha para o servidor InterSystems IRIS. Para estabelecer sua
conexão com o servidor InterSystems IRIS, o Gateway usa a conta CSPSystem, que é uma das
Contas predefinidas do IRIS.

Por padrão, todas essas transações não são criptografadas. Você pode usar o TLS para criptografar mensagens do navegador para a Web servidor. Você pode usar o Kerberos para criptografar mensagens do Gateway para o servidor InterSystems IRIS, conforme descrito em Definir
criar um canal seguro para uma conexão com a web; se você não estiver usando Kerberos, talvez prefira proteger fisicamente a conexão entre as máquinas host, como por exemplo, co-localizando as máquinas do servidor Gateway e InterSystems IRIS em uma área bloqueada com uma conexão física direta entre eles.
### ODBC

O InterSystems IRIS suporta autenticação de instância para conexões ODBC entre todas as suas plataformas suportadas. Isso requer configuração do lado do cliente. As formas de configurar o comportamento do cliente variam de acordo com a plataforma: 
- Em plataformas não Windows, use o arquivo de inicialização ODBC da InterSystems para especificar pares nome-valor que fornecem informações de conexão. Este arquivo é descrito geralmente em Usando o driver ODBC InterSystems. O arquivo tem as seguintes variáveis relevantes para a autenticação da instância: 
	-  Método de autenticação — Especifica como o cliente ODBC se autentica no DSN. 0 especifica a autenticação da instância; 1 especifica Kerberos. 
	- UID — Especifica o nome da conta de usuário padrão para se conectar ao DSN. No tempo de execução, dependendo do comportamento do aplicativo, o usuário final pode ter permissão para substituir esse valor por uma conta de usuário diferente. 
	- Senha — Especifica a senha associada à conta de usuário padrão. Se o usuário final tiver permissão para substituir o valor UID, o aplicativo aceitará um valor para a senha do usuário recém-especificado. 
-  Em um cliente Windows, você pode especificar informações de conexão por meio de uma GUI ou programaticamente: 
	- Por meio de uma GUI, há uma caixa de diálogo de configuração ODBC DSN. O InterSystems IRIS oferece opções na guia DSN do sistema. Esta tela tem ajuda associada que descreve seus campos. O caminho do menu Iniciar do Windows para exibir esta tela varia de acordo com a versão do Windows; ele pode ser listado no Painel de Controle do Windows, em Ferramentas Administrativas, na tela de Fontes de Dados (ODBC). 
	- Programaticamente, está disponível a função SQLDriverConnect, que aceita um conjunto de pares nome-valor. SQLDriverConnect é uma chamada C que faz parte da API ODBC. Seus pares nome-valor são os mesmos do arquivo de inicialização disponível em plataformas não Windows, exceto que a senha é identificada com a palavra-chave PWD
### Telnet
Ao estabelecer uma conexão usando o servidor InterSystems IRIS Telnet para Windows, o cliente usa informações de configuração que foram armazenadas como parte de um servidor remoto InterSystems IRIS. Para configurar um servidor remoto, vá para a máquina cliente. Nessa máquina, o procedimento é:

1. Clique no iniciador InterSystems IRIS e selecione Servidor Preferencial no menu (a opção Servidor Preferido também exibe o nome do servidor preferencial atual). 
2. No submenu exibido, escolha Adicionar/Editar. 
3. Para criar um novo servidor remoto, clique no botão Adicionar; para configurar um servidor já existente, escolha o servidor InterSystems IRIS ao qual você está se conectando e clique no botão Editar. 
4. Isso exibe a caixa de diálogo Adicionar conexão. Na área Método de autenticação dessa caixa de diálogo, clique em Senha para autenticação de instância. 
5. Se você estiver editando os valores de um servidor já existente, não deve haver necessidade de alterar ou adicionar valores para os campos mais gerais nesta caixa de diálogo, pois eles são determinados pelo servidor que você escolheu editar. Se você estiver adicionando um novo servidor, os campos a serem preenchidos serão descritos em Definir uma conexão de servidor remoto. 
6. Clique em OK para salvar os valores especificados e fechar a caixa de diálogo. 
**Importante:** Ao se conectar a uma máquina não-Windows usando telnet, não há servidor telnet InterSystems IRIS disponível - você simplesmente usa o servidor telnet que vem com o sistema operacional. Depois de estabelecer a conexão com a máquina servidora, você pode se conectar ao InterSystems IRIS usando o serviço %Service_Terminal

## Delegated Authentication

### About Delegated Authentication
#### Delegated Authentication Background
O InterSystems IRIS suporta autenticação delegada, que permite implementar mecanismos personalizados para substituir as atividades de autenticação e gerenciamento de funções que fazem parte da segurança da InterSystems, por exemplo, o sistema de autenticação existente de uma empresa. Como desenvolvedor do aplicativo, você controla totalmente o conteúdo do código de autenticação delegada. A autenticação delegada ocorre se uma instância do InterSystems IRIS encontrar uma rotina ZAUTHENTICATE em seu namespace %SYS. Se tal rotina existir, o InterSystems IRIS a usará para autenticar usuários, seja com chamadas para código novo ou existente. O InterSystems IRIS inclui uma rotina, ZAUTHENTICATE.mac, que serve como modelo para criar a rotina ZAUTHENTICATE.

**importante:** Se estiver usando autenticação com HealthShare®, você deve usar a rotina ZAUTHENTICATE fornecida pela InterSystems e não pode escrever sua própria rotina
#### How Delegated Authentication Works
Quando um usuário tenta fazer login e o InterSystems IRIS invoca a autenticação delegada, a sequência de eventos é: 
1. Quando um serviço ou aplicativo usa autenticação delegada, uma tentativa de login resulta automaticamente em uma chamada para a rotina ZAUTHENTICATE. O código de autenticação nessa rotina pode ser qualquer ObjectScript definido pelo usuário, métodos de classe ou $ZF código explicativo. 
2. A próxima etapa depende se a autenticação foi bem-sucedida ou não e se este é ou não o primeiro login usando ZAUTHENTICATE: 
	- Se ZAUTHENTICATE for bem-sucedido e esta for a primeira vez que o usuário foi autenticado por meio deste mecanismo, o usuário será adicionado à lista de usuários do InterSystems IRIS com um tipo de "Usuário delegado". Se ZAUTHENTICATE definir funções ou outras características, elas se tornarão parte das propriedades do usuário. 
	- Se ZAUTHENTICATE for bem-sucedido e este não for o primeiro login, ZAUTHENTICATE atualizará as propriedades do usuário. •
	- Se ZAUTHENTICATE falhar, o usuário receberá um erro de acesso negado e não poderá acessar o sistema. Para determinar por que isso ocorreu: 
		- a. Verifique o campo Motivo da falha ao fazer login no Perfil do usuário. 
		- b. Para obter informações mais detalhadas, verifique o log de auditoria do evento %System/%Login/LoginFailure relevante. Se a auditoria ou o evento LoginFailure não estiverem habilitados, talvez seja necessário habilitar ambos e, em seguida, recriar as circunstâncias da falha de logon. 
3. Se a autenticação de dois fatores estiver habilitada para a instância e os serviços relevantes, haverá uma verificação de que as propriedades PhoneNumber e PhoneProvider do usuário foram definidas. Se essas propriedades forem definidas, a autenticação de dois fatores continuará; Se eles não estiverem definidos, a autenticação de dois fatores não poderá continuar e o usuário não será autenticado. 
4. Um usuário delegado é listado como tal na coluna Tipo da lista de usuários na página Usuários (Administração do Sistema > Segurança > Usuários). As propriedades do usuário são exibidas como somente leitura no Portal de Gerenciamento e não são editáveis de dentro do InterSystems IRIS (uma vez que todas as informações vêm de fora do InterSystems IRIS). 
	**Nota:** Um usuário delegado também não pode ser um usuário de senha do InterSystems IRIS.
### Overview of Configuring Delegated Authentication
Para usar a autenticação delegada, as etapas são: 
1. Crie o código de autenticação definido pelo usuário na rotina ZAUTHENTICATE. Isso pode incluir o uso de autenticação de dois fatores. Essa rotina também pode executar a configuração básica de uma conta de usuário, como especificar funções e outras propriedades do usuário. 
	Se você estiver usando o HealthShare Health Connect, crie uma rotina ZAUTHENTICATE personalizada, conforme descrito neste guia.
	
	Se você estiver usando o HealthShare Unified Care Record, não poderá criar uma versão personalizada do ZAUTHENTICATE para implementar a autenticação delegada porque o Unified Care Record vem com sua própria versão da rotina. Em vez disso, você deve personalizar métodos na classe HS. Local.ZAUTHENTICATE. Para obter mais informações, consulte "Personalizando a autenticação por meio do ZAUTHENTICATE local" no livro Autenticando usuários do HealthShare. 
2. Habilite a autenticação delegada para a instância do InterSystems IRIS na página Opções de autenticação. 
3. Habilite a autenticação delegada para os serviços, aplicativos ou ambos relevantes, conforme necessário. 
4. Opcionalmente, habilite a autenticação de dois fatores para a instância InterSystems IRIS e, se necessário, para aplicativos da web e aplicativos cliente-servidor. 

Por exemplo, para usar a autenticação delegada para o Portal de Gerenciamento de uma instância, as etapas são: 
1. Crie o código de autenticação definido pelo usuário em ZAUTHENTICATE. 
2. Habilite a autenticação delegada para a instância do InterSystems IRIS como um todo. 
3. Habilite a autenticação delegada para o conjunto de aplicativos /csp/sys*
### Create Delegated (User-Defined) Authentication Code
Esta seção descreve vários aspectos da criação de sua própria rotina ZAUTHENTICATE:
- Authentication Code Fundamentals 
- Signature 
- Authentication Code
- Set Values for Roles and Other User Characteristics
- Return Value and Error Messages
#### Authentication Code Fundamentals
A InterSystems fornece uma rotina de amostra, ZAUTHENTICATE.mac, que você pode copiar e modificar. Essa rotina faz parte do exemplo Samples-Security no GitHub (https://github.com/intersystems/Samples-Security). Você pode baixar o exemplo inteiro conforme descrito em Download de exemplos para uso com o InterSystems IRIS , mas pode ser mais conveniente simplesmente abrir a rotina no GitHub e copiar seu conteúdo. 
Para criar seu próprio ZAUTHENTICATE.mac: 
1. Para usar ZAUTHENTICATE.mac como modelo, copie seu conteúdo e salve-o em uma rotina ZAUTHENTICATE.mac no namespace %SYS. 
2. Examine os comentários no exemplo ZAUTHENTICATE.mac. Eles fornecem diretrizes importantes sobre como implementar uma versão personalizada da rotina. 
3. Edite sua rotina adicionando código de autenticação personalizado e qualquer código desejado para definir as características da conta do usuário. 

**CUIDADO:** Como o InterSystems IRIS não impõe restrições ao código de autenticação em ZAUTHENTICATE, o programador de aplicativos é responsável por garantir que esse código seja suficientemente seguro.
#### Signature
A assinatura de ZAUTHENTICATE é:
- ObjectScript
	```
	ZAUTHENTICATE(ServiceName, Namespace, Username, Password, Credentials,
	 Properties) PUBLIC { 
		// authentication code 
		// optional code to specify user account properties and roles 
	}
	```

	onde: 
	- ServiceName — Uma string que representa o nome do serviço através do qual o usuário está se conectando ao InterSystems IRIS, como %Service_Console ou %Service_WebGateway. 
	- Namespace — Uma string que representa o namespace no servidor InterSystems IRIS com o qual uma conexão está sendo estabelecida. Isso é para uso com o serviço %Service_Bindings, como com Studio ou ODBC. 
	- Nome de usuário — Uma string que representa o nome da conta inserida pelo usuário que deve ser validada pelo código da rotina. 
	- Senha — Uma string que representa a senha inserida pelo usuário que deve ser validada. 
	- Credenciais — Aprovado por referência. Não implementado nesta versão do InterSystems IRIS.
	- Propriedades — Passado por referência. Uma matriz de valores retornados que define as características da conta nomeada por Nome de usuário. 
	
	Quando o InterSystems IRIS chama ZAUTHENTICATE, ele tem valores para esses argumentos e os fornece para a rotina. 
	
	**Nota:** Em versões mais antigas dos produtos InterSystems, ZAUTHENTICATE recebeu quatro argumentos. Para compatibilidade com versões anteriores, você ainda pode usar a versão de quatro argumentos. Se você estiver atualizando seu código da versão antiga para a nova, observe que os novos argumentos são o segundo e o quinto: Namespace e Credentials.
#### Authentication Code

O conteúdo do código de autenticação é específico do aplicativo. Se a autenticação for bem-sucedida, a rotina deverá retornar a macro $OK; caso contrário, ele deve retornar um código de erro. Consulte Valor Retornado e Mensagens de Erro para obter mais informações sobre valores retornados. 

**CUIDADO:** Como o InterSystems IRIS não impõe e não pode colocar nenhuma restrição no código de autenticação em ZAUTHENTICATE, o programador do aplicativo é responsável por garantir que esse código seja suficientemente seguro.

1. The GetCredentials Entry Point
	ZAUTHENTICATE inclui um ponto de entrada GetCredentials. Esse ponto de entrada é chamado sempre que a autenticação delegada é habilitada para um serviço e é chamado antes que o usuário seja solicitado a fornecer um nome de usuário e uma senha. Em vez de obter um nome de usuário e senha do usuário, o código na função (criada pelo desenvolvedor do aplicativo) especifica o nome de usuário e a senha. O nome de usuário e a senha retornados são autenticados da maneira normal, como se o usuário os tivesse inserido. Um possível uso desse mecanismo é fornecer um nome de usuário e senha no ponto de entrada e, em seguida, no código de autenticação, 4roles para o processo. 
	
	O nome de usuário e a senha retornados desse ponto de entrada podem ser obtidos por qualquer mecanismo que o desenvolvedor do aplicativo escolher. Eles podem vir de um global, de uma chamada DLL ou LDAP externa ou simplesmente ser definidos dentro da rotina. O desenvolvedor do aplicativo pode até fornecer código para solicitar o nome de usuário e a senha, como em uma conexão de terminal ou com uma página de login. 
	
	Quando há uma chamada para o ponto de entrada GetCredentials, o valor retornado e outros fatores determinam o que acontece a seguir: 
	- Se o código definir os valores de Nome de usuário e Senha e também retornar um status de êxito ($OK), então: 
		- Não há solicitação adicional de nome de usuário/senha. 
		- O processo de autenticação continua. 
		**Importante:** Se o ponto de acesso retornar $OK, seu código deverá definir os valores de Nome de usuário e Senha. Caso contrário, o acesso ao sistema será negado ao usuário e um erro será gravado no log de auditoria. 
		
	- Se o ponto de entrada retornar o status de erro $SYSTEM. Status.Error ($$GetCredentialsFailed), a solicitação normal de nome de usuário/senha continua. 
	- Se o ponto de entrada retornar qualquer outro status de erro, então: 
		-  O usuário tem acesso negado ao sistema. 
		- O erro é registrado no log de auditoria. 
	No exemplo a seguir de um ponto de entrada GetCredentials, o código executa ações diferentes para serviços diferentes: 
	-  Para %Service_Console, ele não solicita nenhuma informação ao usuário e define o nome de usuário e a senha do processo como _SYSTEM e SYS, respectivamente. 
	- Para %Service_Bindings, força o usuário a fornecer um nome de usuário e senha. 
	- Para aplicações web, verifica se a aplicação em uso é a aplicação /csp/ samples; se for esse aplicativo, ele define o nome de usuário e a senha como AdminUser e Test. Para todos os outros aplicativos da web, ele nega o acesso. 
	- Para qualquer outro serviço, nega o acesso.
	
	Por fim, o ponto de entrada Erro executa a limpeza conforme necessário.
	
	O código é: 
	![[Pasted image 20240724111602.png]]
	Para obter mais detalhes, consulte os comentários desse ponto de entrada em ZAUTHENTICATE.mac.
	
1. The SendTwoFactorToken Entry Point
	ZAUTHENTICATE inclui um ponto de entrada SendTwoFactorToken. Esse ponto de entrada é para uso com autenticação de dois fatores. Se estiver definido e a instância do InterSystems IRIS tiver a autenticação de dois fatores habilitada, você poderá substituir a configuração padrão do sistema para o formato da mensagem e do token que a instância envia para o celular do usuário. Isso permite mensagens que podem variar de acordo com o aplicativo, mesmo na mesma instância do InterSystems IRIS. 
	
	Para obter mais detalhes e um exemplo de como usar esse ponto de entrada, consulte esse ponto de entrada no exemplo ZAUTHENTICATE.mac.
### Set Values for Roles and Other User Characteristics
Se a autenticação inicial for bem-sucedida, ZAUTHENTICATE poderá estabelecer as funções e outras características para o usuário autenticado. Para logons subsequentes, ZAUTHENTICATE pode atualizar esses elementos do registro do usuário. 

Para que isso aconteça, o código em ZAUTHENTICATE define os valores da matriz Properties. (Properties é passado por referência a ZAUTHENTICATE.) Normalmente, a origem dos valores que estão sendo definidos é um repositório de informações do usuário que está disponível para ZAUTHENTICATE.

1. User Properties
	Os elementos na matriz Properties são:
	-  Properties("Comment") — Any text 
	- Properties("FullName") — The first and last name of the user 
	- Properties("NameSpace") — The default namespace for a Terminal login 
	- Properties("Roles") — The comma-separated list of roles that the user holds in InterSystems IRIS 
	- Properties("Routine") — The routine that is executed for a Terminal login 
	- Properties("Password") — The user’s password 
	- Properties("Username") — The user’s username
	- Properties("PhoneNumber") — The user’s mobile phone number, for use with two-factor authentication 
	- Properties("PhoneProvider") — The user’s mobile phone’s service provider, for use with two-factor authentication
	
	Cada um desses elementos é descrito com mais detalhes em uma das seções a seguir. 
	
	**Nota:** O valor de cada elemento na matriz properties determina o valor de sua propriedade associada para o usuário que está sendo autenticado. Não é possível usar apenas um subconjunto das propriedades ou manipular seus valores após a autenticação
	
	1. Comment
		Se ZAUTHENTICATE definir o valor de Properties("Comment"), essa string se tornará o valor da propriedade Comment da conta de usuário no InterSystems IRIS. (Essa propriedade é descrita em Propriedades da conta de usuário.) Se nenhum valor for passado de volta para a rotina de chamada, o valor de Comment para a conta de usuário será uma cadeia de caracteres nula e o campo relevante no Portal de Gerenciamento não terá conteúdo
	2. FullName
		Se ZAUTHENTICATE definir o valor de Properties("FullName"), essa string se tornará o valor da propriedade Full name da conta de usuário no InterSystems IRIS. (Essa propriedade é descrita em Propriedades da conta de usuário.) Se nenhum valor for passado de volta para a rotina de chamada, o valor de Nome completo da conta de usuário será uma cadeia de caracteres nula e o campo relevante no Portal de Gerenciamento não terá conteúdo
	3. NameSpace
		Se ZAUTHENTICATE definir o valor de Properties("Namespace"), essa string se tornará o valor da propriedade Startup Namespace da conta de usuário no InterSystems IRIS. (Essa propriedade é descrita em Propriedades da conta de usuário.) Se nenhum valor for passado de volta para a rotina de chamada, o valor do Namespace de Inicialização para a conta de usuário será uma cadeia de caracteres nula e o campo relevante no Portal de Gerenciamento não reterá nenhum conteúdo. 
		
		Uma vez conectado ao InterSystems IRIS, o valor do Namespace de Inicialização (portanto, o de Properties("Namespace")) determina o namespace inicial para qualquer usuário autenticado para acesso local (como para Console, Terminal ou Telnet). Se o Namespace de inicialização não tiver valor (já que Properties("Namespace") não tiver valor), o namespace inicial de qualquer usuário autenticado para acesso local será determinado da seguinte maneira: 
		1. Se o namespace USER existir, esse será o namespace inicial. 
		2. Se o namespace USER não existir, o namespace inicial será o namespace %SYS. 
		
		**Observação**:** se o usuário não tiver os privilégios apropriados para o namespace inicial, o acesso será negado.
	4. Password
		Se ZAUTHENTICATE definir o valor de Properties("Password"), essa string se tornará o valor da propriedade Password da conta de usuário no InterSystems IRIS. (Essa propriedade é descrita em Propriedades da conta de usuário.) Se nenhum valor for passado de volta para a rotina de chamada, o valor de Password para a conta de usuário será uma cadeia de caracteres nula e o campo relevante no Portal de Gerenciamento não terá conteúdo
	5. Roles
		Se ZAUTHENTICATE definir o valor de Properties("Roles"), essa cadeia de caracteres especificará as funções às quais um usuário está atribuído; Esse valor é uma cadeia de caracteres que contém uma lista de funções delimitada por vírgulas. Se nenhum valor for passado de volta para a rotina de chamada, o valor de Funções para a conta de usuário será uma cadeia de caracteres nula e o campo relevante no Portal de Gerenciamento não reterá conteúdo. As informações sobre as funções de um usuário estão disponíveis na guia Funções da página Editar usuário de um usuário. 
		
		Se alguma função retornada em Properties("Roles") não estiver definida, o usuário não será atribuído à função. 
		
		Portanto, o usuário conectado é atribuído a funções da seguinte maneira:
		- Se uma função estiver listada em Propriedades ("Funções") e for definida pela instância do InterSystems IRIS, o usuário será atribuído à função. 
		- Se uma função estiver listada em Propriedades ("Funções") e não for definida pela instância do InterSystems IRIS, o usuário não será atribuído à função. 
		- Um usuário é sempre atribuído às funções associadas ao usuário _PUBLIC. Um usuário também tem acesso a todos os recursos públicos. Para obter informações sobre o usuário _PUBLIC, consulte A conta _PUBLIC; para obter informações sobre recursos públicos, consulte Serviços e seus recursos.
		
	6. Routine
		Se ZAUTHENTICATE definir o valor de Properties("Routine"), essa string se tornará o valor da propriedade Startup Tag^Routine da conta de usuário no InterSystems IRIS. (Essa propriedade é descrita em Propriedades da conta de usuário.) Se nenhum valor for passado de volta para a rotina de chamada, o valor de Marca de Inicialização^Rotina para a conta de usuário será uma cadeia de caracteres nula e o campo relevante no Portal de Gerenciamento não contém conteúdo. 
		
		Se Properties("Routine") tiver um valor, esse valor especificará a rotina a ser executada automaticamente após o login em um serviço do tipo terminal (como para Console, Terminal ou Telnet). Se Properties("Routine") não tiver valor, o login iniciará a sessão do Terminal no modo programador.
	7. Username
		Se ZAUTHENTICATE retornar a propriedade Username, o valor de Username será gravado no banco de dados de segurança após qualquer processamento na função; Isso fornece a chance de modificar o valor que o usuário inseriu no prompt. Se ZAUTHENTICATE não retornar a propriedade Username, o valor da propriedade será gravado no banco de dados de segurança conforme inserido. 
		
		Se ZAUTHENTICATE definir o valor de Properties("Username"), essa string se tornará o valor da propriedade Name da conta de usuário no InterSystems IRIS. (Essa propriedade é descrita em Propriedades da conta de usuário.) Isso fornece ao programador de aplicativos a oportunidade de normalizar o conteúdo fornecido pelo usuário final no prompt de login. 
		
		Se não houver nenhuma chamada explícita que passe o valor de Properties("Username") de volta para a rotina de chamada, não haverá normalização e o valor inserido pelo usuário final no prompt servirá como o valor da propriedade Name da conta de usuário sem qualquer modificação
	8. PhoneNumber and PhoneProvider
		Essas são propriedades associadas à autenticação de dois fatores. 
		
		Se ZAUTHENTICATE definir o valor de Properties("PhoneNumber") e Properties("PhoneProvider"), eles serão gravados no banco de dados InterSystems IRIS para o usuário como o número de telefone celular e o provedor de serviços de telefonia móvel do usuário. Se estes não forem passados de volta para a rotina de chamadas, então o número de telefone e o provedor de serviços gravados no banco de dados InterSystems IRIS são uma string nula. Portanto, para usar a autenticação de dois fatores com autenticação delegada, você deve fornecer ambos
1. The User Information Repository
	ZAUTHENTICATE pode se referir a qualquer tipo de repositório de informações do usuário, como um arquivo global ou externo. Cabe ao código na rotina definir quaisquer propriedades externas na matriz Properties para que o usuário autenticado possa ser criado ou atualizado com essas informações. Por exemplo, enquanto um repositório pode incluir informações como funções e namespaces, o código ZAUTHENTICATE deve disponibilizar essas informações para o InterSystems IRIS. 
	
	Se as informações no repositório forem alteradas, essas informações só serão propagadas de volta para as informações do usuário do InterSystems IRIS se houver código em ZAUTHENTICATE para executar esta ação. Além disso, se houver esse código, as alterações nas funções dos usuários devem ocorrer no repositório; se você alterar as funções de um usuário durante uma sessão, a alteração não entrará em vigor até o próximo login, momento em que as funções do usuário serão redefinidas por ZAUTHENTICATE
#### Return Value and Error Messages

A rotina retorna um dos seguintes valores: 
- Sucesso — $OK. Isso indica que a combinação de nome de usuário/senha foi autenticada com sucesso 
- Falha — $SYSTEM. Status.Error($ERRORMESSAGE). Isso indica que a autenticação falhou. 

ZAUTHENTICATE pode retornar mensagens de erro definidas pelo sistema ou específicas do aplicativo. Todas essas mensagens usam o método Error do %SYSTEM. Classe de status. Esse método é invocado como $SYSTEM. Status.Error e usa um ou dois argumentos, dependendo da condição de erro. 

As mensagens de erro definidas pelo sistema disponíveis são: 
-  $SYSTEM. Status.Error($$AccessDenied) — mensagem de erro de "Acesso negado" 
- $SYSTEM. Status.Error($$InvalidUsernameOrPassword) — Mensagem de erro de "Nome de usuário ou senha inválidos" 
- $SYSTEM. Status.Error($$UserNotAuthorizedOnSystem,Username) — Mensagem de erro "O nome de usuário do usuário não está autorizado" 
- $SYSTEM. Status.Error($$UserAccountIsDisabled,Username) — Mensagem de erro "A conta do nome de usuário está desativada" 
- $SYSTEM. Status.Error($$UserInvalidUsernameOrPassword,Username) — Mensagem de erro de "Nome de usuário nome ou senha inválidos" 
- $SYSTEM. Status.Error($$UserLoginTimeout) — Mensagem de erro de "Tempo limite de login" 
- $SYSTEM. Status.Error($$UserCTRLC) — Mensagem de erro de "Login abortado" 
- $SYSTEM. Status.Error($$UserDoesNotExist,Username) — Mensagem de erro de "O nome de usuário não existe" 
- $SYSTEM. Status.Error($$UserInvalid,Username) — Mensagem de erro de "Nome de usuário O nome de usuário é inválido" 
- $SYSTEM. Status.Error($$PasswordChangeRequired) — Mensagem de erro de "Alteração de senha necessária" 
- $SYSTEM. Status.Error($$UserAccountIsExpired,Username) — Mensagem de erro "A conta do nome de usuário expirou" 
- $SYSTEM. Status.Error($$UserAccountIsInactive,Username) — Mensagem de erro "A conta do nome de usuário está inativa" 
- $SYSTEM. Status.Error($$UserInvalidPassword) — Mensagem de erro de "Senha inválida" 
- $SYSTEM. Status.Error($$ServiceDisabled,ServiceName) — Mensagem de erro "Os logins para o nome do serviço estão desativados" 
- $SYSTEM. Status.Error($$ServiceLoginsDisabled) — Mensagem de erro de "Os logins estão desativados" 
- $SYSTEM. Status.Error($$ServiceNotAuthorized,ServiceName) — Mensagem de erro de "Usuário não autorizado para serviço"

Para gerar uma mensagem personalizada, use o $SYSTEM. Status.Error(), passando a macro $GeneralError e especificando qualquer texto personalizado como o segundo argumento. Por exemplo:

```$SYSTEM.Status.Error($$$GeneralError,"Any text here")```

Observe que, quando uma mensagem de erro é retornada ao chamador, ela é registrada no banco de dados de auditoria (se a auditoria de eventos LoginFailure estiver ativada). No entanto, a única mensagem de erro que o usuário vê é $SYSTEM. Status.Error($$AccessDenied). No entanto, o usuário também vê a mensagem para o erro $PasswordChangeRequired. Retorne esse erro se quiser que o usuário mude da senha atual para uma nova.
### Set Up Delegated Authentication
Depois de criar uma rotina ZAUTHENTICATE para executar a autenticação (e, opcionalmente, tarefas de autorização), a próxima etapa é habilitá-la para os serviços ou aplicativos relevantes da instância. Este procedimento é: 
1. Habilite a autenticação delegada para toda a instância. Na página Opções de Autenticação/Sessão da Web (Administração do Sistema > Segurança > Segurança do Sistema > Opções de Autenticação/Sessão da Web), selecione Permitir autenticação delegada e clique em Salvar. 
	
	Com a autenticação delegada habilitada para a instância, uma caixa de seleção Delegada é exibida na página Editar Serviço para serviços relevantes e na página Editar Aplicativo Web para esses aplicativos. 

2. Habilite a autenticação delegada para serviços e aplicativos, conforme apropriado. Os seguintes serviços oferecem suporte à autenticação delegada: 
- %Service_Bindings 
- %Service_CallIn 
- %Service_ComPort 
- %Service_Console 
- -%Service_Login 
- %Service_Terminal 
- %Service_Telnet 
- %Service_WebGateway

Eles se enquadram em várias categorias de modos de acesso: 

- Acesso local — 
	%Service_CallIn, %Service_ComPort, %Service_Console, %Service_Login, %Service_Terminal, %Service_Telnet
	Para usar a autenticação delegada com conexões locais, habilite-a para o serviço. 
- Acesso cliente-servidor —
	%Service_Bindings 
	Para usar a autenticação delegada com conexões cliente-servidor, habilite-a para o serviço.
- Acesso à Web — 
	%Service_WebGateway 
	Para usar a autenticação delegada com conexões baseadas na Web, habilite-a para o aplicativo Web. Você também pode habilitá-lo para o Web Gateway ativando o serviço %Service_WebGateway

### After Delegated Authentication Succeeds

Depois que o usuário se autentica, dois tópicos importantes são:
-  The State of the System
- Change Passwords
#### The State of the System
#### Change Passwords
## Use LDAP with Delegated Authentication or Other Mechanisms

## Two-Factor Authentication

###
###
###
###
## JSON Web Token (JWT) Authentication

## Services

## Advanced Topics in Authentication