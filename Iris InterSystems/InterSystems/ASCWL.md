# Secure Custom Web Application Logins
## About Creating a Custom CSP Login Page
Para criar uma página de logon CSP personalizada: 
1. Certifique-se de que o usuário do Web Gateway (CSPSystem) tenha permissões para ler o banco de dados para onde a página de logon personalizada está localizada. Atribua o recurso de banco de dados desejado a uma função apropriada e, em seguida, atribua essa função ao usuário do CSPSystem. 
2. Crie uma subclasse do %CSP. Página de login. 
3. Para personalizar o comportamento do seu aplicativo, substitua os métodos Draw da subclasse, para que a página tenha a aparência desejada. Isso inclui métodos que modificam a aparência da página de logon e métodos que modificam a aparência da página do token de segurança (se você estiver usando a autenticação de dois fatores): 
	- Métodos de página de logon — DrawCSS3STYLE, DrawHEAD, DrawSTYLE, DrawTitle 
	- Métodos de página de token de segurança (ST) — DrawSTHEAD, DrawSTTitle 
	Observe que os métodos DrawTitle e DrawSTTitle chamam o método DrawTitleSection .
4. Invoque a subclasse conforme necessário em seu aplicativo. 

**Importante:** Ao criar uma página de logon personalizada, você deve usar uma subclasse de %CSP. Login. Outras abordagens para criar páginas de logon em aplicativos CSP podem causar problemas de vários tipos. Se você tiver escrito páginas de logon personalizadas que não usam uma subclasse de %CSP. Faça login e aplique quaisquer alterações da InterSystems que atualizem ou protejam sua instância, suas páginas de login podem falhar sem mensagens de erro. Por exemplo, os usuários podem tentar fazer login com nomes de usuário e senhas válidos, mas seus logins falharão sem nenhuma causa visível. Essa situação pode indicar que você precisa alterar seu login personalizado para usar a abordagem necessária

## About Creating a Custom Zen Login Page
Ao criar uma página de login Zen personalizada, use o <``loginForm``> componente de acordo com o procedimento descrito na seção "Controlando o acesso a aplicativos" do capítulo "Zen Security" de Desenvolvendo aplicativos Zen.

**Importante:** Ao criar uma página de login personalizada, você deve usar o <``loginForm``> componente. Outras abordagens para criar páginas de login em aplicativos Zen podem causar problemas de vários tipos. 

Se você tiver criado páginas de login personalizadas que não usam o <``loginForm``> componente e aplicar quaisquer alterações da InterSystems que atualizem ou protejam sua instância, suas páginas de login poderão falhar sem mensagens de erro. Por exemplo, os usuários podem tentar fazer login com nomes de usuário e senhas válidos, mas seus logins falharão sem nenhuma causa visível. Essa situação pode indicar que você precisa alterar seu logon personalizado para usar a abordagem necessária.