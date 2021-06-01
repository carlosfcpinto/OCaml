// Função para consultar o número de telefone móvel da classe "Pessoa"

public int consultarTlm ()
{
    int n = 0;
    for (i=0;i<3;i++)
        if(contactos[i].getTipo().equals("telemovel"))
            n=contactos[i].getNumero()
}

public class Pergunta{
    private static int ultimo = 0;
    private int numero;
    private String texto;
    private double valor;
}
//Para a classe Pergunta construa um 

public Pergunta ()
{
    ultimo = ultimo + 1;
    numero = ultimo;
    texto = "";
    valor = 0.0;
}

public static int getUltimo()
{
    return ultimo;
}

public static void setUltimo(int )
{
    Pergunta.ultimo = ultimo;
}

public string gettexto()
{
    return texto;
}
public settext (stringtexto)
{
    This.texto=texto;
}

public string toString()
{
    return("nº da pergunta" + numero + "\ntexto" + texto + "\ncotação" + valor);
}

//pergunta 2

public Frequencia (string s, int n)
{
    disciplina = s;
    data = Localdata.now()
    perguntas = new Pergunta [n];
    for (int i=0; i<n; i++)
        pergunta[i]= new Pergunta();
}

public void setPerguntas(Pergunta[] p)
{
    for (int i<0; i< perguntas.length; i++){
        perguntas[i].setNumero(p[i].getNumero());
        perguntas[i].setText(p[i].getTexto());
        perguntas[i].setValor(p[i].getValor());
    }
}

public String toString()
{
    string s="Nome" + disciplina;
    s+="Data" + data;
    for(int i = 0; i<perguntas.length;i++){
        s+=perguntas[i];
    }
    return (s);
}

public class Jogador {
    String nome;
    private double salario;
    private int golos[] = new int[33];
}

public String toString()
{
    String s= "Nome" + nome;
    s+= "Salário" + String.valueOf(salario);
    for (int i = 0; i < 33; i++){
        s+="Golos na " + i + "ª jornada =" + golos[i];
    }
    return (s);
}

