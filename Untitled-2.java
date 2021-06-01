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

public static void setnome(String)
{
    Jogador.nome = String;
}

public String getnome()
{
    return(nome);
}

public double getsalario()
{
    return(salario);
}

public static void setsalario(double i)
{
    return(i);
}

public static void aumentarsalario(double i)
{
    return(salario * (salario*i));
}

public static void golos(int i, int b)
{
    golos[i-1]=b;
}