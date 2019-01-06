package Client;

import java.util.Scanner;

public class Menu {

    private final String[] options;
    private final Scanner scanner;

    Menu(String[] options ) {
        this.options = options;
        this.scanner = new Scanner( System.in);
    }

    public String toString(String title ){

        StringBuilder sb = new StringBuilder();
        sb.append(" **** ").append(title).append(" ****\n");

        for( int i = 0; i < options.length ; i ++ )
            sb.append(i+1).append(" - ").append(options[i]).append("\n");

        sb.append("Escolha uma opção: " );
        return sb.toString();
    }
    public String toString(){

        StringBuilder sb = new StringBuilder();
        sb.append("****  ****  ****\n");

        for( int i = 0; i < options.length ; i ++ )
            sb.append(i+1).append(" - ").append(options[i]).append("\n");

        sb.append("Escolha uma opção: " );
        return sb.toString();
    }
    public int getOption() {
        int option = -1;

        while( option < 0 || option > options.length )
            option = scanner.nextInt();

        return option-1;
    }

}
