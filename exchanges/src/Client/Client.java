package Client;

import Exchanges.Protos;
import org.zeromq.ZMQ;

import java.io.IOException;
import java.util.Scanner;


public class Client {

    enum State {
        INVESTOR ,
        COMPANY,
        NONE
    }

    private static final Menu menuWithoutLogin = new Menu( new String[] { "Login", "Criar conta", "Subscrever", "Minhas subscrições", "Listar empresas", "Listar leilões"});
    private static final Menu menuInvestors =  new Menu( new String[]   { "Licitar", "Subscrever", "Minhas subscrições", "Listar empresas", "Listar leilões"});
    private static final Menu menuCompanys = new Menu( new String[]     { "Criar leila", "Criar leilao com taxa fixa", "Subscrever", "Minhas subscrições", "Listar empresas", "Listar leilões"});

    private final ZMQ.Socket socketSub ;

    private final Scanner s;

    private State state;

    private Middleware middleware;

    Client( int serverAddress, int  proxyAddress) throws IOException {

        /////////////////// Initiations  ///////////////////

        s = new Scanner( System.in);

        ZMQ.Context context = ZMQ.context(1);
        socketSub = context.socket( ZMQ.SUB );
        //socketSub.connect( "tcp://localhost:+ proxyAddress );

        state = State.NONE;

        this.middleware = new Middleware( serverAddress );

        /////////////////// Start ///////////////////
        boolean flag = true ;

        while( flag ) {

            switch ( state ){
                case NONE:
                    showMenuWithoutLogin();
                    break;
                case COMPANY:
                    showMenuCompanys();
                    break;
                case INVESTOR:
                    showMenuInvestors();
                    break;
            }
        }

        middleware.close();
    }

    private void showMenuInvestors() {
        System.out.println( menuInvestors.toString());

        switch (menuInvestors.getOption() ){
            case 0:
                toAuction();
                break;
            case 1:
                subscribe();
                break;
            case 2:
                listsubscriptions();
                break;
            case 3:
                listCompanies();
                break;
            case 4:
                listCompanies();
                break;
            case 5:
                listAuctions();
                break;
        }
    }

    private void showMenuCompanys() {
        System.out.println( menuCompanys.toString());

        switch ( menuCompanys.getOption() ){
            case 0:
                createAuction();
                break;
            case 1:
                createFixedInterestAuction();
                break;
            case 2:
                subscribe();
                break;
            case 3:
                listsubscriptions();
                break;
            case 4:
                listCompanies();
                break;
            case 5:
                listAuctions();
                break;
        }
    }

    private void showMenuWithoutLogin() throws IOException {

        System.out.println(  menuWithoutLogin.toString() );

        switch (menuWithoutLogin.getOption() ){
            case 0:
                makeLogin();
                break;
            case 1:
                createAccount();
                break;
            case 2:
                subscribe();
                break;
            case 3:
                listsubscriptions();
                break;
            case 4:
                listCompanies();
                break;
            case 5:
                listAuctions();
                break;
        }

    }


    //////////////// Actions /////////////////

    private void createFixedInterestAuction() {

    }

    private void createAuction() {
    }


    private void toAuction() {

    }

    private void listAuctions() {

    }

    private void listCompanies() {

    }

    private void listsubscriptions() {

    }

    private void subscribe() {

    }


    private void createAccount() throws IOException {

        Protos.Authentication.UserType type;

        System.out.print("Pretende criar um investior ( escolha \"1\") ou uma empresa ( outra letra ): ");
        char option = s.nextLine().charAt(0);

        if( option == '1')
            type = Protos.Authentication.UserType.INVESTOR;
        else
            type = Protos.Authentication.UserType.COMPANY;

        System.out.print("Nome de utilizador: ");
        String username = s.nextLine();

        System.out.print("Palavra passe: ");
        String password = s.nextLine();

        middleware.sendAuthentication( type, Protos.Authentication.CredentialsType.REGISTER, username, password);

        Protos.ServerResponse response = middleware.receiveMsg();

        if ( response.hasError() )
            System.out.println("Invalido, tente novamente.");
        else
            System.out.println("Conta criada com sucesso. ");
    }

    private void makeLogin() throws IOException {
        System.out.print("Investidor ( 1 )  Empresa ( 2 ): ");
        char option = s.nextLine().charAt(0);
        Protos.Authentication.UserType type;
        if( option == '1')
            type = Protos.Authentication.UserType.INVESTOR;
        else
            type = Protos.Authentication.UserType.COMPANY;

        System.out.print("Nome de utilizador: ");
        String username = s.nextLine();

        System.out.print("Palavra passe : ");
        String password = s.nextLine();

        middleware.sendAuthentication(type, Protos.Authentication.CredentialsType.LOGIN, username,password);

        System.out.println("Enviado");

        Protos.ServerResponse response = middleware.receiveMsg();

        if( response.hasError() )
            System.out.println("Invalido, tente novamente.");
        else {
            System.out.println("Login aceite, bem vindo. ");

            if( type.equals( Protos.Authentication.UserType.COMPANY ))
                state = State.COMPANY;
            else
                state = State.INVESTOR;
        }
    }

}


