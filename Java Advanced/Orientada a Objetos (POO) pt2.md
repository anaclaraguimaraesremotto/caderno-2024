## Polimorfismo

Podemos definir o polimorfismo como um principio capaz de invocar métodos que apesar de apresentar a mesma assinatura, se comportam de maneiras diferentes. Ou seja, os mesmos atributos e objetos podem ser utilizados em objetos distintos mas com lógicas diferentes.

Podemos usar de exemplo um celular e um notebook, os dois são aparelhos eletrônicos, mas a implementação da lógica para o calculo final da venda é diferente. Também podemos levar em consideração que o aparelho celular pode ter um desconto de 10% em cima do valor final, enquanto o desconto do produto notebook pode chegar a 30%.

Outro exemplo que podemos levar em consideração são os mamíferos:

Imagine que temos uma superclasse chamada ==Mamíferos== que contem o método *locomover()*:

```
locomover() {
	// logica desenvolvida para locomoção
} 
```

Agora vamos adicionar 3 subclasses para deixar o exemplo mais claro

==Macaco:==
```
locomover() {
	pulandoDeGalhoEmGalho;
} 
```

==Homem:==
```
locomover() {
	andando;
} 
```

==Baleia:==
``` 
locomover() {
	nadando;
} 
```

Observe que apesar de compartilharem o mesmo método *locomover()*, em cada situação o método é sobrescrito de uma forma diferente se adaptando a natureza de cada espécie, caracterizando o principal conceito do polimorfismo.

Em uma classe teste, a implementação do código ficaria mais ou menos assim:

``` Java
variavelObjeto = new Macaco(); 
variavelObjeto.locomover(); // pulando de galho em galho 

variavelObjeto = new Homem(); 
variavelObjeto.locomover(); // andando 

variavelObjeto = new Baleia(); 
variavelObjeto.locomover(); // nadando
```

Mantendo o código limpo, facilitando a compreensão e agilizando possíveis manutenções.

## Overriding Vs Overloading


A Sobrescrita (Overriding) e a Sobrecarga (Overloading) de métodos são dois recursos muito importantes da programação orientada a objetos, no entanto é preciso saber diferencia-los para que seja feita a aplicação correta dentro de um código.

### Sobrescrita 

A sobrescrita (ou override) esta diretamente relacionada à POO, mais especificamente com a herança. Com ela, podemos especializar nossos métodos herdados da superclasse deixando o seu comportamento mais especifico dentro das subclasses. 

O ato de sobrescrever, ==consiste em criar um novo método na subclasse herdeira contendo a mesma assinatura e o mesmo tipo de retorno do método sobrescrito==. Isso significa que o método deve possuir o mesmo nome, a mesma quantidade e o mesmo tipo de parâmetro utilizado no método sobrescrito

Devemos tomar cuidado em relação ao tipo de retorno, este pode ser um subtipo do tipo de retorno do método sobrescrito. No entanto o oposto não é permitido, gerando um erro de compilação.

==Por exemplo: Se o método da superclasse retornar um List, é permitido que o novo método retorne um ArrayList ou qualquer outro List.==

Implementando o recurso de sobrescrita em um código:

``` Java
public class Funcionario {

    protected String nome;
    protected String cpf;
    protected double salario;

    public double calculaBonus() {
        return salario * 0.10;
    }
}
```


Para garantir a sobrescrita e facilitar a interpretação do código, utilizamos a annotation ``@Override``

``` Java
public class Gerente extends Funcionario {

    String setor;

    @Override
    public double calculaBonus() {
        return this.salario * 0.20;
    }
}
```

Nesse exemplo, temos a superclasse Funcionário que possui suas características e um método para calcular o bônus onde é o salário + 10%, e temos a subclasse Gerente, que possui as mesmas características que um funcionário, com diferença no comportamento.

Observe que a subclasse Gerente está herdando todos os atributos e métodos da Funcionário, porem ela trem uma característica a mais, o setor. Também podemos observar que o método calculaBonus() é sobrescrito de forma que o gerente tenha 20% de bônus e não 10% como no método original. Dessa forma fica fácil observar a mudança de comportamento entre os métodos.

### Sobrecarga

Agora que entendemos o que é Sobrescrita, a diferenciação se tornará mais fácil.
A Sobrecarga de método ocorre em situações em que o ==nome do método na subclasse é o mesmo que o da superclasse, porem há diferença na lista de parâmetros ou no tipo de retorno.== É importante observar que tanto na sobrecarga, quanto na sobrescrita, o nome do método na subclasse é o *mesmo* que o da superclasse.

Em Java, a sobrecarga de um método pode ocorrer na mesma classe, sem a necessidade de herança.

==Exemplo:==

``` Java
public class Calculadora{  
  
	//Somar dois valores (a e b)  
	public int somar(int a, int b){  
		return a + b;  
	}  
	  
	//Desta vez, somamos três valores (a, b e c)  
	public int somar(int a, int b, int c){  
		return a + b + c;  
	}  
}
```

No exemplo acima, a classe Calculadora tem *dois* métodos chamados somar, mas embora tenham o mesmo tipo de retorno, eles se diferem no numero de parâmetros, o que é suficiente para ocorrer a sobrecarga.

Se chamarmos ``somar(3,6)``, o primeiro método será executado, mas se chamarmos ``somar(9, 4, 13)`` o segundo método será executado.

==Agora vamos voltar para o exemplo com herança:==

``` JAVA
public class Animal {  
	public String emitirSom() {  
		return "Som genérico de animal";  
	}  
}  
```

```Java
public class Cao extends Animal {  

	@Override  
	public String emitirSom() {  
		return "Latido";  
	}  
  
	public String emitirSom(int intensidade){  
		return "Latido com intensidade " + intensidade;  
	}  
}
```

Neste exemplo: 
1. Criamos a superclasse Animal com o método ``emitirSom()``;
2. Criamos a subclasse Cao que herda o método ``emitirSom()`` da superclasse Animal;
3. Sobrescrevemos o método ``emitirSom()`` para dizer que esse cachorro esta latindo;
4. Sobrecarregamos o método já sobrescrito, adicionando o parâmetro ``intensidade``.

### Importância de usar a annotation @Override

Agora que sabemos a diferença de Overriding e Overloading, devemos entender a importância de não esquecer de sinalizar a sobrescrita utilizando a annotation @Override.

A annotation é uma forma de garantir que você está de fato executando aquela função, nesse caso, ela garante que você esta sobrescrevendo um método pronto e não criando um método novo.

Vamos supor que você criou uma classe com um método de imprimir:
```java
public class SuperClasse {
    public void imprime() {
        System.out.println("imprime");
    }
}
```

Depois, você fez uma classe que estende essa classe, e você quer mudar o que será impresso quando o método for chamado:

```java
public class MinhaClasse extends SuperClasse {
    public void imprime() {
        System.out.println("imprime diferente");
    }
}
```

Você sobrescreveu corretamente o método `imprime()` e o código acima funcionará sem maiores problemas.

Entretanto um belo dia você resolveu mudar o nome do método na SuperClasse de `imprime()` para `imprimir()`:

```java
public class SuperClasse {
    public void imprimir() {
        System.out.println("imprime");
    }
}
```

Se você não se lembrar da classe que estendeu a SuperClasse você terá um método `imprime()` nela que **não está sobrescrevendo o método na SuperClasse**. Você na verdade está chamando um método novo, que se chama `imprime()`.

Agora vamos ver o que aconteceria se você tivesse usado a annotation @Override

Esse seria seu código a princípio para as duas classes:

```java
public class SuperClasse {
    public void imprime() {
        System.out.println("imprime");
    }
}

public class MinhaClasse extends SuperClasse {
    @Override
    public void imprime() {
        System.out.println("imprime diferente");
    }
}
```

Até o momento, nada de novo. Entretanto quando você alterar o seu método de `imprime()` para `imprimir()` você não mais conseguirá compilar seu código pois o @Override perceberá que você não está sobrescrevendo coisa nenhuma, pois não existe mais nenhum método `imprime()` na SuperClasse.

Você receberá o seguinte erro:

> MinhaClasse must override or implement a supertype method

Em tradução livre:

> MinhaClasse deve sobrescrever ou implementar um método da sua super classe

Toda linguagem orientada a objetos permite a sobrescrita de métodos da superclasse pela classe filha. Entretanto cada linguagem de programação usa seus próprios meios para lidar com essa sobrescrita. O Java optou por usar o annotation @Override para os desenvolvedores que quiserem a segurança citada no decorrer da resposta, entretanto, nada obriga o uso desse annotation.