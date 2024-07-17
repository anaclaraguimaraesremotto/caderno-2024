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

Esse método lê até caracteres *len* do cliente. Se a chamada for bem-sucedida, o status (*sc*) será retornado como *$OK*; caso contrário, um dos seguintes códigos de erro é retornado:

- ```$$$CSPWebSocketTimeout``` O método Read atingiu o tempo limite
- ```$$$CSPWebSocketClosed``` O cliente encerrou o WebSocket.

**Write()** 
```  Method Write(data As %String) As %Status```
Esse método grava dados no cliente.