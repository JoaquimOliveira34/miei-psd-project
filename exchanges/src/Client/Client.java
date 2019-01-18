package Client;

import Exchanges.Protos;

import java.io.IOException;
import java.util.List;
import java.util.Scanner;


public class Client {

    enum State {
        INVESTOR ,
        COMPANY,
        NONE
    }

    private static final Menu menuWithoutLogin = new Menu( new String[] { "Login", "Criar conta", "Subscrever", "Minhas subscrições", "Listar empresas", "Listar leilões"});
    private static final Menu menuInvestors =  new Menu( new String[]   { "Licitar", "Subscrever", "Minhas subscrições", "Listar empresas", "Listar leilões", "Ler Mensagens", "Ler Notificações"});
    private static final Menu menuCompanys = new Menu( new String[]     { "Criar leila", "Criar leilao com taxa fixa", "Subscrever", "Minhas subscrições", "Listar empresas", "Listar leilões", "Ler Mensagens", "Ler Notificações"});

    private final Scanner s;

    private State state;

    private Middleware middleware;

    private DirectoryClient directory;

    Client( int serverAddress, int  proxyAddress) throws IOException {

        /////////////////// Initiations  ///////////////////

        s = new Scanner( System.in);

        state = State.NONE;

        this.middleware = new Middleware( serverAddress, proxyAddress );

        // TODO Maybe these parameters should also be customizable?
        this.directory = new DirectoryClient( "localhost", 8080 );

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

    // TODO Question: Should loggout and logging in with a different account do a reset of notifications an messages?
    private void showMenuHeader () {
        int messages = this.middleware.getMailbox().getMessageCount();
        int notifications = this.middleware.getMailbox().getNotificationsCount();

        if ( messages > 0 && notifications > 0 ) {
            System.out.printf( "**** (%d unread messages, %d unread notifications) ****\n", messages, notifications );
        } else if ( messages > 0 ) {
            System.out.printf( "**** (%d unread messages) ****\n", messages );
        } else if ( notifications > 0 ) {
            System.out.printf( "**** (%d unread notifications) ****\n", notifications );
        }
    }

    private void showMenuInvestors() {
        this.showMenuHeader();
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
            case 6:
                listMessages();
                break;
            case 7:
                listNotifications();
        }
    }

    private void showMenuCompanys() throws IOException {
        this.showMenuHeader();
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
            case 6:
                listMessages();
                break;
            case 7:
                listNotifications();
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

    private void createFixedInterestAuction() throws IOException {
        System.out.print("Qual a quantidade de dinheiro a emitir: ");
        int amount = Integer.parseInt( s.nextLine() );

        System.out.print("Qual a taxa de juros do da emissao: ");
        float rate = Float.parseFloat( s.nextLine() );

        this.middleware.sendMsgCompany( Protos.MsgCompany.Type.FIXEDRATE, amount, rate );
    }

    private void createAuction() throws IOException {
        System.out.print("Qual a quantidade de dinheiro a pedir em leilao: ");
        int amount = Integer.parseInt( s.nextLine() );

        System.out.print("Qual a taxa de juros maxima do leilao: ");
        float rate = Float.parseFloat( s.nextLine() );

        this.middleware.sendMsgCompany( Protos.MsgCompany.Type.AUCTION, amount, rate );
    }


    private void toAuction() {

    }

    private void listAuctions() {

    }

    private void listCompanies() {
        try {
            List< DirectoryClient.Company > companies = this.directory.listCompanies();

            System.out.println( "#### Empresas ####" );

            if ( companies.size() == 0 ) {
                System.out.println( "---- Sem empresas registadas... ----" );
            }

            for ( DirectoryClient.Company company : companies ) {
                System.out.printf( "%d: %s, %s\n", company.getId(), company.getName(), company.getZone() );
            }
        } catch ( IOException e ) {
            e.printStackTrace();
        }
    }

    private void listsubscriptions() {

    }

    private void listMessages () {
        System.out.println( "#### Mensagens ####" );

        List<String> messages = this.middleware.getMailbox().readMessages();

        if ( messages.size() == 0 ) {
            System.out.println( "---- Sem mensagens para ler... ----" );
        }

        for ( int i = 0; i < messages.size(); i++ ) {
            System.out.printf( "%d: %s\n", i + 1, messages.get( i ) );
        }
    }

    private void listNotifications () {
        System.out.println( "#### Notificações ####" );

        List<String> notifications = this.middleware.getMailbox().readNotifications();

        if ( notifications.size() == 0 ) {
            System.out.println( "---- Sem notificações para ler... ----" );
        }

        for ( int i = 0; i < notifications.size(); i++ ) {
            System.out.printf( "%d: %s\n", i + 1, notifications.get( i ) );
        }
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

        Protos.ServerResponse response = middleware.getMailbox().readResponse();

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

        Protos.ServerResponse response = middleware.getMailbox().readResponse();

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


