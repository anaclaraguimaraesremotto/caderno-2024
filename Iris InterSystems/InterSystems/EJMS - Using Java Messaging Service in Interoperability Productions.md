## JMS Overview

***Este documento discute uma implementação herdada de mensagens JMS que não deve ser usada para novas implementações. Essa implementação herdada pode ser removida em versões futuras. Em vez disso, use os adaptadores de interoperabilidade JMSPEX (entrada e saída) que a InterSystems implementou usando a estrutura PEX. Como alternativa, use a API de Mensagens JMS.***

O Java Messaging Service (JMS) é uma estrutura de mensagens Java para fornecer comunicação entre dois ou mais sistemas. Nessa estrutura, um provedor JMS gerencia uma fila de mensagens enviadas por clientes JMS. Uma mensagem JMS típica tem o seguinte caminho:

1. Um cliente JMS envia a mensagem para um provedor JMS. 
2. O provedor JMS envia a mensagem para outro cliente JMS

Com produções de interoperabilidade, os produtos InterSystems podem ser um cliente JMS que envia e recebe mensagens JMS. Os clientes JMS da InterSystems usam o host de negócios EnsLib.JMS.Operation para enviar mensagens aos provedores JMS e o host de negócios EnsLib.JMS.Service para receber mensagens dos provedores JMS. Os usuários avançados que estão familiarizados com o ObjectScript podem criar seus próprios hosts de negócios JMS personalizados em vez de usar esses componentes internos. 

Internamente, os hosts de negócios JMS aproveitam um servidor externo InterSystems para se conectar ao Java

### JMS Messages

Na produção de interoperabilidade do cliente JMS, as mensagens JMS são objetos EnsLib.JMS.Message. A propriedade text do objeto de mensagem contém o conteúdo da mensagem. A propriedade type do objeto de mensagem especifica o tipo de mensagem, como TextMessage e BytesMessage. A classe EnsLib.JMS.Message também fornece métodos para definir e recuperar propriedades da mensagem.

## Jar Files

O arquivo jar para o recurso JMS está disponível em: install-dirdevjavalibJDK18intersystems-enslib-jms-3.0.0.jar 
Os seguintes arquivos jar de desenvolvimento de cliente também estão disponíveis: 
- install-dirdevjavajmswljmsclient.jar 
- install-dirdevjavajmswlthint3client.ja

## Configuring and Using JMS Business Services and Operations 

Os produtos InterSystems podem ser configurados para ser um cliente JMS adicionando hosts de negócios internos a uma produção de interoperabilidade. 

Para permitir que um produto InterSystems receba mensagens JMS, adicione um novo serviço comercial à produção de interoperabilidade, definindo a **classe de serviço** desse serviço comercial como EnsLib.JMS.Service. Este serviço empresarial ignora qualquer resposta. 

Para permitir que um produto InterSystems envie mensagens JMS, adicione uma nova operação de negócios à produção de interoperabilidade, definindo a **classe de operação** dessa operação de negócios como EnsLib.JMS.Operation. Essa operação de negócios retorna um objeto EnsLib.JMS.Response de volta ao host de negócios que enviou a mensagem JMS para a operação de negócios. 

Depois de adicionar esses hosts de negócios à produção, defina as seguintes configurações na **guia Configurações: **

- **JMSCredentials** — A credencial definida para o nome de usuário e a senha do servidor JMS. Para obter mais informações sobre como criar uma credencial para o nome de usuário e senha do JMS, consulte Definindo credenciais. 
- **JavaGatewayHost e JavaGatewayPort** — O endereço IP e a porta do servidor externo InterSystems que sua produção está usando para habilitar o suporte a JMS. Um servidor externo também é conhecido como Java Gateway. Se você adicionou o host de negócios EnsLib.JavaGateway.Service à produção, use o endereço IP e a porta em suas Configurações básicas. 
- **Servidor JMS** — URL do servidor JMS. 
- **JMSFactory** — Nome do QueueConnectionFactory. 
- **JMSQueue** — Nome da fila JMS. 
- **JMSClientID** — Nome que aparece na lista de conexões ativas do Servidor JMS.
## Creating Custom JMS Services and Operations Using the Adapter

A criação de serviços de negócios JMS personalizados e operações de negócios requer a gravação de código ObjectScript personalizado e, consequentemente, requer mais recursos de desenvolvimento do que o uso dos serviços e operações JMS internos, mas fornece melhor desempenho, pois você pode acessar o objeto proxy Java Gateway diretamente. 

Para desenvolver um serviço comercial JMS personalizado: 

- Implemente uma classe BusinessService personalizada usando EnsLib.JMS.InboundAdapter como adaptador. 
- Substitua o método OnProcessInput() com a seguinte assinatura: Método OnProcessInput(pMessage As %Net.Remote.Object, Output pOutput As %RegisteredObject) As %Status 
- tMessage é um objeto proxy Gateway de um objeto de mensagem Java da classe com.intersystems.enslib.jms.Message. As propriedades e os métodos do objeto de mensagem Java podem ser acessados usando a interface de proxy do Gateway. O objeto pMessage contém a mensagem recebida do provedor JMS. 

Para desenvolver uma operação de negócios JMS personalizada: 

- Implemente uma classe BusinessOperation personalizada usando EnsLib.JMS.OutboundAdapter como seu adaptador. 
- Substitua o método OnMessage() ou implemente um mapa de mensagens. Consulte Definindo um mapa de mensagens em Desenvolvendo produções para obter informações sobre mapas de mensagens. 
- Chamar.. Adapter.GetNewMessage(tMessage) para obter a mensagem que foi enviada para a operação de negócios por outro host na produção. tMessage tem a classe %Net.Remote.Object. 
- tMessage é um objeto proxy Gateway de um objeto de mensagem Java da classe com.intersystems.enslib.jms.Message. As propriedades e os métodos do objeto de mensagem Java podem ser acessados usando a interface de proxy do Gateway. Acesse tMessage com propriedades e métodos que são implementados na classe Java com.intersystems.enslib.jms.Message. 
- Envie a mensagem para o provedor JMS ligando para .. Adapter.SendMessage(tMessage).

Depois de desenvolver seu serviço de negócios JMS personalizado e a operação de negócios JMS, adicione-os a uma produção da mesma forma que faria com os hosts de negócios JMS integrados.