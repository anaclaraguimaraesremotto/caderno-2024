# Using the Amazon CloudWatch Adapter
O Amazon CloudWatch é um produto da AWS que permite monitorar um aplicativo coletando dados para métricas específicas. Depois de criar uma métrica no CloudWatch, você pode usar uma operação de negócios em uma produção de interoperabilidade da InterSystems para atualizar o CloudWatch com valores para essa métrica. A InterSystems fornece uma operação de negócios integrada que usa o adaptador de saída do CloudWatch para interagir com o CloudWatch. Você também tem a opção de criar uma operação de negócios personalizada que usa esse adaptador. Se você não estiver familiarizado com produções de interoperabilidade, incluindo o uso de operações de negócios e adaptadores de saída, consulte Introdução às produções de interoperabilidade. 

**Importante:** atualmente, apenas o método PutMetricData do adaptador do CloudWatch está totalmente funcional. Não tente usar o método PutMetricAlarm para trabalhar com alarmes do CloudWatch, pois esse método pode mudar em versões futuras

## Outbound Adapter Details

A classe do adaptador de saída do CloudWatch é EnsLib.AmazonCloudWatch.OutboundAdapter. Dentro dessa classe, o método PutMetricData contém a lógica que atualiza o CloudWatch com um valor para uma métrica específica. A assinatura desse método é: 
![[Pasted image 20240724155543.png]]

Where: 
- **namespace** é o namespace do CloudWatch da métrica. 
- **metricName** é o nome da métrica. 
- **metricValue** é o ponto de dados que está sendo enviado ao CloudWatch para a métrica especificada. 
- **metricUnit** é a unidade de medida para o valor da métrica. Esta unidade de medida é necessária. Para obter uma lista de unidades válidas, consulte a referência do Amazon CloudWatch JavaDoc. 
- **dims** é uma matriz JSON com pares de nome/valor que representam as dimensões da métrica. Por exemplo, ``[{"Name":"StorageType","Value":"StandardStorage"},{"Name":"BucketName","Value":"test-bazco}``

## Built-in Business Operation
Em vez de desenvolver uma operação de negócios personalizada que usa o adaptador de saída, você pode economizar tempo e esforço adicionando a operação de negócios EnsLib.AmazonCloudWatch.MetricDataOperation à produção de interoperabilidade. Depois de adicionada, a produção pode enviar uma solicitação pré-criada que contém os dados da métrica para a operação de negócios. A classe dessa solicitação pré-criada é EnsLib.AmazonCloudWatch.PutMetricDataRequest. 

A operação de negócios contém propriedades que correspondem aos parâmetros do adaptador que identificam a métrica do CloudWatch, por exemplo, nome e namespace. Depois de adicionar a operação comercial à produção, você pode definir essas propriedades usando as configurações correspondentes do Portal de Gerenciamento. Para obter instruções sobre como adicionar uma operação de negócios a uma produção, consulte Adicionando hosts de negócios.
## General AWS Settings
O adaptador de saída do CloudWatch estende uma classe de adaptador comum que inclui propriedades gerais da AWS. Quando você adiciona uma operação de negócios que usa o adaptador de saída a uma produção, essas propriedades da AWS podem ser definidas usando as configurações da AWS no Portal de Gerenciamento. 

**CredentialsFile** — Se estiver em branco, a Amazon usa a cadeia de provedores de credenciais padrão para obter as credenciais necessárias para acessar o CloudWatch. Se você preferir usar um arquivo de credencial da AWS, insira seu caminho de arquivo. 

**Região** — identifica a região da AWS que você deseja acessar. Para obter uma lista de regiões do CloudWatch, consulte Regiões da Amazon, zonas de disponibilidade e zonas locais