## WebSockets (RFC 6455)
O protocolo WebSockets (RFC 6455) atende ao requisito fundamental de permitir que os servidores enviem mensagens proativamente por push para os clientes, fornecendo um canal de comunicação full-duplex orientado a mensagens entre um cliente e seu servidor. O protocolo é projetado para operar e, portanto, ser protegido sobre o canal TCP padrão já estabelecido entre o cliente e o servidor e usado para suportar o protocolo HTTP entre um navegador da Web e um servidor da Web. 
O protocolo WebSockets e sua API são padronizados pelo W3C e a parte do cliente é incluída no HTML 5. 
Espera-se que os intermediários, como proxies e firewalls, estejam cientes e ofereçam suporte ao protocolo WebSockets.

## WebSockets Protocol

A criação de um WebSocket envolve uma troca ordenada de mensagens entre o cliente e o servidor. Primeiro, o handshake WebSocket deve ocorrer. O handshake é baseado em, e se assemelha, uma troca de mensagens HTTP para que ele possa passar sem problemas através da infraestrutura HTTP existente

- O cliente envia solicitação de handshake para uma conexão WebSocket.
- O servidor envia resposta de handshake (se for capaz)

O servidor Web reconhece a estrutura de cabeçalho HTTP convencional na mensagem de solicitação de handshake e envia uma mensagem de resposta construída de forma semelhante para o cliente, indicando que ele oferece suporte ao protocolo WebSocket - supondo que ele seja capaz. Se ambas as partes concordarem, o canal será alternado de HTTP (http://) para o protocolo WebSockets (ws://)

- Quando o protocolo é comutado com êxito, o canal permite a comunicação full duplex entre o cliente e o servidor
- O enquadramento de dados para mensagens individuais é mínimo

***Mensagem típica de handshake do WebSocket do Cliente***

```
GET /csp/user/MyApp.MyWebSocketServer.cls HTTP/1.1 
Host: localhost 
Upgrade: websocket 
Connection: Upgrade 
Sec-WebSocket-Key: x3JJHMbDL1EzLkh9GBhXDw== 
Sec-WebSocket-Protocol: chat 
Sec-WebSocket-Version: 13 
Origin: http://localhost
```

***Mensagem típica de handshake de WebSocket do servidor***

```
HTTP/1.1 101 Switching Protocols 
Upgrade: websocket
Connection: Upgrade 
Sec-WebSocket-Accept: HSmrc0sMlYUkAGmm5OPpG2HaGWk= 
Sec-WebSocket-Protocol: chat
```

Observe como a mensagem de handshake do cliente solicita que o protocolo seja atualizado de HTTP para WebSocket. Observe também a troca de chaves exclusivas entre o cliente (Sec-WebSocket-Key) e o servidor (Sec-WebSocket-Accept).

## WebSockets Client Code (JavaScript)

No ambiente do navegador, o cliente usa JavaScript. Os livros-texto padrão descrevem o modelo de uso em detalhes. Esta seção descreve brevemente os conceitos básicos.

### Creating a WebSocket
O primeiro parâmetro representa a URL que identifica a extremidade do servidor do aplicativo WebSocket. O segundo parâmetro é opcional e, se presente, especifica o subprotocolo que o servidor deve oferecer suporte para que a conexão WebSocket seja bem-sucedida

```
var ws = new WebSocket(url, [protocol]);
```

Example: 
```
ws = new WebSocket(((window.location.protocol == "https:") 
? "wss:" : "ws:") + "//" 
+ window.location.host 
+ csp/user/MyApp.MyWebSocketServer.cls);
```

Observe como o protocolo é definido como ws ou wss, dependendo se o transporte subjacente é ou não protegido usando SSL/TLS.

O atributo somente leitura ws.readyState define o estado da conexão. Pode ter um dos seguintes valores: 
-  0 A conexão ainda não está estabelecida. 
- 1 A conexão é estabelecida e a comunicação é possível. 
- 2 A conexão está sujeita ao aperto de mão de fechamento. 
- 3 A conexão está fechada ou não pôde ser aberta.

O atributo somente leitura ws.bufferedAmount define o número de bytes de texto UTF-8 que foram enfileirados usando o método send().

### WebSocket Client Events

Os seguintes eventos estão disponíveis. 
- ` ws.onopen`  é acionado quando a conexão do soquete é estabelecida. 
- ` ws.onmessage`  é acionado quando o cliente recebe dados do servidor. Dados recebidos em event.data. 
- ` ws.onerror`  é acionado quando ocorre um erro na comunicação. 
- `ws.onclose` dispara quando a conexão é fechada
### WebSocket Client Methods

Os seguintes métodos estão disponíveis. 
- ` ws.send(data)`  Transmite dados para o cliente. 
- ` ws.close()`  Feche a conexão.

## WebSockets Server Code
A classe IRIS® InterSystems base para implementar WebSocket Servers é %CSP. 
WebSocket Quando o cliente solicita uma conexão WebSocket, a solicitação HTTP inicial (a mensagem de handshake inicial) instrui o mecanismo CSP a inicializar o servidor WebSocket do aplicativo. O servidor WebSocket é a classe nomeada na URL solicitante. Por exemplo, se o servidor WebSocket for chamado MyApp.MyWebSocketServer e for projetado para operar no namespace USER, a URL usada para solicitar a conexão WebSocket será: 

```/csp/user/MyApp.MyWebSocketServer.cls```


### WebSocket Server Events
Para implementar um servidor WebSocket, crie uma subclasse de %CSP. WebSocket e defina retornos de chamada nessa classe conforme necessário. Observe que a sessão da Web é desbloqueada antes de chamar qualquer um desses métodos.

**OnPreServer()**
Implemente esse método para invocar o código que deve ser executado antes que o servidor WebSocket seja estabelecido. As alterações na propriedade SharedConnection devem ser feitas aqui.

**Server() (required)**
Implemente esse método para criar o servidor WebSocket. Esta é a implementação do lado do servidor do aplicativo WebSocket. As mensagens podem ser trocadas com o cliente usando os métodos Read() e Write(). Use o método EndServer() para fechar normalmente o WebSocket da extremidade do servidor

**OnPostServer()**
Implemente esse método para invocar o código que deve ser executado após o servidor WebSocket ter fechado

### WebSocket Server Methods

Você pode invocar os seguintes métodos de dentro desses retornos de chamada:

**Read()**
``` Method Read(ByRef len As %Integer = 32656, ByRef sc As %Status, timeout As %Integer = 86400) As %String ``` 

Esse método lê até caracteres *len* do cliente. Se a chamada for bem-sucedida, o status (*sc*) será retornado como *$$$OK*; caso contrário, um dos seguintes códigos de erro é retornado:

- ```$$$CSPWebSocketTimeout``` O método Read atingiu o tempo limite
- ```$$$CSPWebSocketClosed``` O cliente encerrou o WebSocket.

**Write()** 
```  Method Write(data As %String) As %Status```
<<<<<<< HEAD:Iris InterSystems/InterSystems/AWEBSOCKETS.md

Esse método grava dados no cliente.

**EndServer()**
```Method EndServer() As %Status```

Esse método termina normalmente o servidor WebSocket fechando a conexão com o cliente

**OpenServer()**
```Method OpenServer(WebSocketID As %String = "") As %Status```

Esse método abre um WebSocket Server existente. Somente um WebSocket operando de forma assíncrona (*SharedConnection=1*) pode ser acessado usando esse método.

### WebSocket Server Properties 

Você pode definir ou obter as seguintes propriedades de dentro desses retornos de chamada: 

#### SharedConnection (default: 0)
	Essa propriedade determina se a comunicação entre o cliente e o servidor WebSocket deve ser por meio de uma conexão de Gateway da Web dedicada ou assíncrona por um pool de conexões de Gateway da Web compartilhadas. Essa propriedade deve ser definida no método **OnPreServer()** e pode ser definida da seguinte maneira: 

- *SharedConnection=0* O servidor WebSocket se comunica de forma síncrona com o cliente por meio de uma conexão de gateway da Web dedicada. Neste modo de operação, a conexão de hospedagem é efetivamente 'privada' para o WebSocket Server do aplicativo.
- *SharedConnection=1* O servidor WebSocket se comunica de forma assíncrona com o cliente por meio de um pool de conexões compartilhadas do Web Gateway. Além disso, o soquete expira quando não há atividade para o período de tempo limite da sessão CSP

#### WebSocketID
Essa propriedade representa a identidade exclusiva do WebSocket. 

#### SessionId
Essa propriedade representa a ID de Sessão CSP de hospedagem na qual o WebSocket foi criado. 

#### BinaryData
Essa propriedade instrui o Web Gateway a ignorar a funcionalidade que, de outra forma, interpretaria o fluxo de dados transmitido como texto codificado em UTF-8 e definiria os campos de dados binários apropriados no cabeçalho do quadro WebSocket. 
Isso deve ser definido como 1 antes de gravar um fluxo de dados binários no cliente. 

Por exemplo: 
```SET .. BinaryData = 1```

## WebSockets Server Example
A classe de servidor WebSocket simples a seguir aceita uma conexão de entrada de um cliente e simplesmente ecoa os dados recebidos. 
O tempo limite é definido como 10 segundos e cada vez que o tempo limite do método Read() uma mensagem é gravada no cliente. Isso ilustra um dos principais conceitos subjacentes aos WebSockets: iniciar uma troca de mensagens com o cliente a partir do servidor
Finalmente, o WebSocket fecha normalmente se o cliente (ou seja, usuário) envia a saída da cadeia de caracteres.

```
Method OnPreServer() As %Status 
{ 
	Quit $$$OK 
} 

Method Server() As %Status 
{ 
	Set timeout=10 
	For { 
		Set len=32656 
		Set data=..Read(.len, .status, timeout) 
		If $$$ISERR(status) { 
				If $$$GETERRORCODE(status) = $$$CSPWebSocketClosed { 
					Quit 
				} 
			If $$$GETERRORCODE(status) = $$$CSPWebSocketTimeout { 
				Set status=..Write(“Server timed-out at “_$Horolog) 
			} 
		} 
		else { 
			If data="exit" Quit 
			Set status=..Write(data) 
		} 
	} 
	Set status=..EndServer() 
	Quit $$$OK 
} 

Method OnPostServer() As %Status 
{ 
	Quit $$$OK 
}
```

## WebSockets Server Asynchronous Operation

O exemplo fornecido na seção anterior ilustra um servidor WebSocket operando de forma síncrona com o cliente em uma conexão IRIS InterSystems dedicada. Quando essa conexão é estabelecida, ela é rotulada como *WebSocket* na coluna de status do formulário Status dos Sistemas de Gateways da Web. Com esse modo, o WebSocket está operando dentro do contexto de segurança da sessão da Web de hospedagem e todas as propriedades associadas a essa sessão podem ser facilmente acessadas. 
Com o modo de operação assíncrono (*SharedConnection=1*), a conexão de hospedagem é liberada assim que o objeto WebSocket é criado e o diálogo subsequente com o cliente é sobre o pool de conexões compartilhadas: as mensagens do cliente chegam por meio do pool convencional de conexões de gateway da Web para o IRIS da InterSystems e as mensagens para o cliente são despachadas pelo pool de conexões de servidor que foram estabelecidas entre o gateway da Web e o IRIS da InterSystems. 
No modo assíncrono, o WebSocket Server é desanexado da sessão da Web principal: a propriedade SessionId mantém o valor da ID da Sessão de hospedagem, mas uma instância do objeto de sessão não é criada automaticamente. 
O exemplo fornecido anteriormente pode ser executado de forma assíncrona simplesmente definindo a propriedade SharedConnection no método OnPreServer(). No entanto, não é necessário ter um processo IRIS InterSystems permanentemente associado ao WebSocket. O método **Server()** pode sair (e o processo de hospedagem parar) sem fechar o WebSocket. Desde que o WebSocketID tenha sido mantido, o WebSocket pode ser subsequentemente aberto em um processo IRIS InterSystems diferente e a comunicação com o cliente é retomada. 
No exemplo a seguir, *MYAPP. SAVE()* e *MYAPP. RETRIEVE()* são espaços reservados para código personalizado que você cria para salvar e recuperar uma ID de WebSocket.

Exemplo:
```
Class MyApp.MyWebSocketServer Extends %CSP.WebSocket 
{ 
	Method OnPreServer() As %Status 
	{ 
		MYAPP.SAVE(..WebSocketID) 
		Set ..SharedConnection = 1 
		Quit $$$OK 
	} 
	
	Method Server() As %Status 
	{ 
		Quit $$$OK 
	} 
	
	Method OnPostServer() As %Status 
	{ 
		Quit $$$OK 
	} 
}
```

Observe que o WebSocketID é mantido para uso subsequente no método OnPreServer(). Observe também a configuração da propriedade SharedConnection no método OnPreServer() e que o método Server() simplesmente sai. 
Recuperando subsequentemente o WebSocketID: 
```Set WebSocketID = MYAPP.RETRIEVE()```

Restabelecimento de vínculo com o cliente:
```
Set ws=##class(%CSP.WebSocket).%New() 
Set %status = ws.OpenServer(WebSocketID)
```

Leitura e escrita para o cliente:
``` 
Set %status=ws.Write(message) 
Set data=ws.Read(.len, .%status, timeout)
```

Finalmente, fechando o WebSocket do lado do servidor:
```Set %status=ws.EndServer()```

## See Also

- RFC 6455
- %CSP.WebSocket in the class reference
=======
Esse método grava dados no cliente.
>>>>>>> 2e704dfd49aa28d2aeaf601ed8b7fa1a74a14ab6:InterSystems/WebSockets (RFC 6455).md
