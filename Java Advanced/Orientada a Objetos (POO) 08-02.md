## Objeto

Qualquer entidade que tenha **estado** (atributo) e **comportamento** (método). Neles é permitido instanciar objetos da classe para inicializar os atributos e invocar os métodos. 
*Estado = como o objeto se parece ou quais propriedades ele possui*.
*Comportamento = o que o objeto faz*.

Um Objeto também pode ser referenciado como uma **instancia de uma classe**.
*Instanciar classe = criar um objeto.*

==Exemplo:==
Um cachorro é um objeto porque possui estados como *cor, nome, raça, etc.*, além de comportamentos como *abanar a cauda, latir, comer, etc.*

## Classes

As Classes são projetos de um objeto que permite armazenar propriedades e métodos dentro dela. Geralmente representam um substantivo, por exemplo: pessoa, lugar, algo que seja "abstrato".

==Características das classes:==
- Possuem visibilidade (public, private, protected);
- Possuem membros (características e ações);
- visibilidade + palavra reservada + class + nome da classe

``` Java
public class Teste{
//ATRIBUTOS OU PROPRIEDADES
//MÉTODOS
}
```

==Exemplo de Classe com Objeto:==

``` Java
public class Cao{

	// Variáveis de instancia: estado do cão
	String nomeCao;
	int idade;
	String raca:
	String cor;
	
	// Métodos de instancia: comportamento do cão
	void brincar(){
		System.out.println("Brincando");
	}
	
	void andar(){
		System.out.println("Andando");
	}
	
	void comer(){
		System.out.println("Comendo");
	}
	
	void dormir(){
		System.out.println("Dormindo");
	}
}
```

Agora que temos nossa classe Cao definida, vamos atribuir característica aos objetos utilizando uma Classe teste:

``` Java
public class TesteCao {
	public static void main(String[] args) {
	
		// Criando dois objetos
		Cao cachorro1 = new Cao();
		Cao cachorro2 = new Cao();

		// Definindo cão rex
		cachorro1.nomeCao = "Rex";
		cachorro1.idade = 3;
		cachorro1.raca = "SRD";
		cachorro1.cor = "Branco e Preto";

		cachorro1.dormir();

		// Definindo cão bob
		cachorro2.nomeCao = "Bob";
		cachorro2.idade = 7;
		cachorro2.raca = "Golden Retriever";
		cachorro2.cor = "Marrom";

		cachorro2.brincar();
		cachorro1.comer();
	}
}
```

O exemplo acima mostra que a Classe Cao de um modo genérico tem as mesmas características independente do cachorro, sendo a classe sempre um projeto para o objeto cachorro.

## Construtores

O Construtor de um Objeto é um método especial pois inicializa seus atributos *toda vez* que é instanciado (inicializado). A identificação de um construtor é sempre o **mesmo nome da classe**.

Vamos criar um Construtor que recebe um parametro de uma String que será o argumento de entrada na Classe de teste:

``` Java
public class Cao{

	String nomeCao;

	// Criando o Construtor
	public Cao(String nome){
	 nomeCao = nome;
	}

	public String getNome(){
		return nomeCao;
	}	
}
```

``` Java
public class TesteCao {
	public static void main(String[] args) {
	
		Cao cachorro1 = new Cao("Rex");
		Cao cachorro2 = new Cao("Bob");

		System.out.println(cachorro1.getNome());
		System.out.println(cachorro2.getNome());
		
	}
}
```


# Princípios da Programação Orientada a Objetos

## Encapsulamento

O Encapsulamento é uma técnica utilizada para esconder uma ideia, não expor detalhes para o usuário tornando. Se acontecer um problema em um setor, a manutenção será realizada de forma exclusiva sem afetar as demais áreas da aplicação, diminuindo riscos e aumentando a produtividade.

No Encapsulamento, os atributos da classe são do tipo ***private***, sendo necessário a criação dos métodos *setters* e *getters* para acessar os modificadores. Os métodos ==Setters== são utilizados para ***alterar*** a informação de uma propriedade de um Objeto, enquanto os ==Getters== são utilizados para ***retornar*** o valor dessa propriedade.

==Exemplo de Encapsulamento com a Classe Cao:==

``` Java
public class Cao{

	private String nomeCao;
	private int idade;

	public String setNomeCao(String nome){
		this.nome = nome;
	}
	
	public void getNomeCao(){
		return nome;
	}
	
	public int setIdade(){
		this.idade = idade;
	}
	
	public void getIdade(){
		return idade;
	}
}
```

Vamos colocar os métodos Getters dentro do System.out.println para gerar saída de resultado: 

``` Java
public class TesteCao {
	public static void main(String[] args) {
	
		Cao cachorro = new Cao("Rex");

		cachorro.setNomeCao("Rex");		
		cachorro.setIdade(3);

		System.out.println(cachorro.getNomeCao());
		System.out.println(cachorro.getNomeIdade());
		
	}
}
```

## Herança

O significado de Herança na Orientada a Objetos tem o mesmo significado no mundo real onde um filho herda alguma característica do pai.  Na Orientada a Objetos é permitido que uma Classe herde atributos e métodos de outra (tendo restrição para a herança).
Para que os métodos e atributos possam ser herdados, devem ter sua visibilidade como *public*  ou *protected*.

Uma das grandes vantagens de usar esse recurso, é a possibilidade da reutilização do código. Para efetuar essa herança, devemos utilizar a palavra reservada *extends*.

![[Pasted image 20240209041502.png]]

No modelo acima, podemos observar as **subclasses** ==Pessoa Física== e ==Pessoa Jurídica== que HERDAM os atributos *nome* e *endereço* da **superclasse** ==Pessoa==

==Para exemplificar, vamos utilizar um modelo de Empresa:==

Superclasse Funcionário que servirá de base para as subclasses utilizarem seus atributos e métodos:

``` Java
public class Funcionario {
    private String nome;
    private double salario;

    public String getNome() {
        return nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public double getSalario() {
        return salario;
    }

    public void setSalario(double salario) {
        this.salario = salario;
    }

    public double calculaBonificacao(){
        return this.salario * 0.1;
    }

}
```

Subclasse Gerente:

``` Java
public class Gerente extends Funcionario {
    private String usuario;
    private String senha;

    public String getUsuario() {
        return usuario;
    }

    public void setUsuario(String usuario) {
        this.usuario = usuario;
    }

    public String getSenha() {
        return senha;
    }

    public void setSenha(String senha) {
        this.senha = senha;
    }

	// Observe que o método abaixo foi herdado e sobrescrito reutilizando um código da subclasse Funcionario
    public  double calculaBonificacao(){
        return this.getSalario() * 0.6 + 100;
    }

}
```

Subclasse Secretária:

``` Java
public class Secretaria extends Funcionario {
    private int ramal;

    public void setRamal(int ramal) {
        this.ramal = ramal;
    }

    public int getRamal() {
        return ramal;
    }
}
```

Observe que não foi preciso criar em todas as classes os atributos *nome* e *salário*.
Agora vamos utilizar uma nova Classe Teste:

``` Java
public class TesteFuncionario {

    public static void main(String[] args) {

        Gerente gerente = new Gerente();
        gerente.setNome("Carlos Vieira");
        gerente.setSalario(3000.58);
        gerente.setUsuario("carlos.vieira");
        gerente.setSenha("5523");

        Funcionario funcionario = new Funcionario();
        funcionario.setNome("Pedro Castelo");
        funcionario.setSalario(1500);

        Telefonista telefonista = new Telefonista();
        telefonista.setNome("Luana Brana");
        telefonista.setSalario(1300.00);
        telefonista.setEstacaoDeTrabalho(20);

        Secretaria secretaria = new Secretaria();
        secretaria.setNome("Maria Ribeiro");
        secretaria.setSalario(1125.25);
        secretaria.setRamal(5);

        System.out.println("##### Gerente #####");
        System.out.println("Nome.: "+gerente.getNome());
        System.out.println("Salário.: "+gerente.getSalario());
        System.out.println("Usuário.: "+gerente.getUsuario());
        System.out.println("Senha.: "+gerente.getSenha());
        System.out.println("Bonificação.: "+gerente.calculaBonificacao());
        System.out.println();

        System.out.println("##### Funcionário #####");
        System.out.println("Nome.: "+funcionario.getNome());
        System.out.println("Salário.: "+funcionario.getSalario());
        System.out.println("Bonificação.: "+funcionario.calculaBonificacao());
        System.out.println();

        System.out.println("##### Telefonista #####");
        System.out.println("Nome.: "+telefonista.getNome());
        System.out.println("Salário.: "+telefonista.getSalario());
        System.out.println("Estação de Trabalho.: "+telefonista.getEstacaoDeTrabalho());
        System.out.println("Bonificação.: "+telefonista.calculaBonificacao());
        System.out.println();

        System.out.println("##### Secretária #####");
        System.out.println("Nome.: "+secretaria.getNome());
        System.out.println("Salário.: "+secretaria.getSalario());
        System.out.println("Ramal.: "+secretaria.getRamal());
        System.out.println("Bonificação.: "+secretaria.calculaBonificacao());
        System.out.println();
    }

}
```

