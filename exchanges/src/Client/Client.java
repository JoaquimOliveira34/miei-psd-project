package Client;

import Exchanges.Protos;

import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Scanner;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


public class Client {

    enum State {
        INVESTOR,
        COMPANY,
        NONE
    }

    private static final Menu menuWithoutLogin = new Menu( new String[] { "Login", "Criar conta", "Subscrever", "De-Subscrever Notificaçoes", "Minhas subscrições", "Listar empresas", "Listar leilões" } );
    private static final Menu menuInvestors    = new Menu( new String[] { "Licitar", "Subscrever Emissao Taxa Fixa", "Subscrever Notificaçoes", "De-Subscrever Notificaçoes", "Minhas subscrições", "Listar empresas", "Listar leilões", } );
    private static final Menu menuCompanys     = new Menu( new String[] { "Criar leila", "Criar leilao com taxa fixa", "Subscrever Notificaçoes", "De-Subscrever Notificaçoes", "Minhas subscrições", "Listar empresas", "Listar leilões", } );

    private final Scanner s;

    private State state;

    private Middleware middleware;

    private DirectoryClient directory;

    Client ( int serverAddress, int proxyAddress ) throws IOException {

        /////////////////// Initiations  ///////////////////

        s = new Scanner( System.in );

        state = State.NONE;

        this.middleware = new Middleware( serverAddress, proxyAddress );

        // TODO Maybe these parameters should also be customizable?
        this.directory = new DirectoryClient( "localhost", 8080 );


        this.middleware.getMailbox().setOnReceive( this::onReceiveMessage );

        /////////////////// Start ///////////////////
        boolean flag = true;

        while ( flag ) {

            try {
                switch ( state ) {
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
            } catch ( Exception error ) {
                System.out.println( "[CLIENT ERROR] " + error.getMessage() );
            }
        }

        middleware.close();
    }

    private void showMenuInvestors () throws IOException {
        System.out.println( menuInvestors.toString() );

        switch ( menuInvestors.getOption() ) {
            case 0:
                toAuction();
                break;
            case 1:
                toEmission();
                break;
            case 2:
                subscribe();
                break;
            case 3:
                unsubscribe();
                break;
            case 4:
                listSubscriptions();
                break;
            case 5:
                listCompanies();
                break;
            case 6:
                listCompanies();
                break;
            case 7:
                listAuctions();
                break;
        }
    }

    private void showMenuCompanys () throws IOException {
        System.out.println( menuCompanys.toString() );

        switch ( menuCompanys.getOption() ) {
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
                unsubscribe();
                break;
            case 4:
                listSubscriptions();
                break;
            case 5:
                listCompanies();
                break;
            case 6:
                listAuctions();
                break;
        }
    }

    private void showMenuWithoutLogin () throws IOException {

        System.out.println( menuWithoutLogin.toString() );

        switch ( menuWithoutLogin.getOption() ) {
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
                unsubscribe();
                break;
            case 4:
                listSubscriptions();
                break;
            case 5:
                listCompanies();
                break;
            case 6:
                listAuctions();
                break;
        }

    }

    //////////////// Messages & Notifications /////////////////
    private void onReceiveMessage () {
        for ( String line : this.middleware.getMailbox().readMessages() ) {
            System.out.println( "[msg] " + transformMessage( line ) );
        }

        for ( String line : this.middleware.getMailbox().readNotifications() ) {
            System.out.println( "[sub] " + transformMessage( line ) );
        }
    }

    //////////////// Actions /////////////////

    private void createFixedInterestAuction () throws IOException {
        System.out.print( "Qual a quantidade de dinheiro a emitir: " );
        int amount = Integer.parseInt( s.nextLine() );

        this.middleware.sendMsgCompany( Protos.MsgCompany.Type.FIXEDRATE, amount );
    }

    private void createAuction () throws IOException {
        System.out.print( "Qual a quantidade de dinheiro a pedir em leilao: " );
        int amount = Integer.parseInt( s.nextLine() );

        System.out.print( "Qual a taxa de juros maxima do leilao: " );
        float rate = Float.parseFloat( s.nextLine() );

        this.middleware.sendMsgCompany( Protos.MsgCompany.Type.AUCTION, amount, rate );
    }

    private void toAuction () throws IOException {
        System.out.print( "Qual a empresa a licitar: " );
        int company = this.directory.getCompanyId( s.nextLine() );

        System.out.print( "Qual a quantidade de dinheiro a licitar: " );
        int amount = Integer.parseInt( s.nextLine() );

        System.out.print( "Qual a taxa de juros da licitaçao: " );
        float rate = Float.parseFloat( s.nextLine() );

        this.middleware.sendMsgInvestor( company, amount, rate );
    }

    private void toEmission () throws IOException {
        System.out.print( "Qual a empresa a subscrever emissao taxa fixa: " );
        int company = this.directory.getCompanyId( s.nextLine() );

        System.out.print( "Qual a quantidade de dinheiro a subscrever: " );
        int amount = Integer.parseInt( s.nextLine() );

        this.middleware.sendMsgInvestor( company, amount );
    }

    private void listAuctions () {

    }

    private void listCompanies () {
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

    private void listSubscriptions () {
        System.out.println( "#### Subscriçoes ####" );

        List< String > subscriptions = this.middleware.getMailbox().getSubscriptions();

        if ( subscriptions.size() == 0 ) {
            System.out.println( "---- Sem subscricoes ativas... ----" );
        }

        for ( String sub : subscriptions ) {
            System.out.println( " - " + transformMessage( sub ) );
        }
    }

    private void subscribe () {
        try {
            System.out.println( "Qual a empresa a que quer subscrever? " );
            int company = this.directory.getCompanyId( s.nextLine() );

            this.middleware.getMailbox().subscribe( String.format( "<comp:%d>", company ) );
        } catch ( IOException e ) {
            System.out.println( "A empresa nao foi encontrada." );

            e.printStackTrace();
        }
    }

    private void unsubscribe () {
        try {
            System.out.println( "Qual a empresa a que quer de-subscrever? " );
            int company = this.directory.getCompanyId( s.nextLine() );

            this.middleware.getMailbox().unsubscribe( String.format( "<comp:%d>", company ) );
        } catch ( IOException e ) {
            System.out.println( "A empresa nao foi encontrada." );

            e.printStackTrace();
        }
    }

    private void createAccount () throws IOException {

        Protos.Authentication.UserType type;

        System.out.print( "Pretende criar um investior ( escolha \"1\") ou uma empresa ( outra letra ): " );
        char option = s.nextLine().charAt( 0 );

        if ( option == '1' ) { type = Protos.Authentication.UserType.INVESTOR; } else {
            type = Protos.Authentication.UserType.COMPANY;
        }

        System.out.print( "Nome de utilizador: " );
        String username = s.nextLine();

        System.out.print( "Palavra passe: " );
        String password = s.nextLine();

        middleware.sendAuthentication( type, Protos.Authentication.CredentialsType.REGISTER, username, password );

        Protos.ServerResponse response = middleware.getMailbox().readResponse();

        if ( response.hasError() ) { System.out.println( "Invalido, tente novamente." ); } else {
            System.out.println( "Conta criada com sucesso. " );
        }
    }

    private void makeLogin () throws IOException {
        System.out.print( "Investidor ( 1 )  Empresa ( 2 ): " );
        char                           option = s.nextLine().charAt( 0 );
        Protos.Authentication.UserType type;
        if ( option == '1' ) { type = Protos.Authentication.UserType.INVESTOR; } else {
            type = Protos.Authentication.UserType.COMPANY;
        }

        System.out.print( "Nome de utilizador: " );
        String username = s.nextLine();

        System.out.print( "Palavra passe : " );
        String password = s.nextLine();

        middleware.sendAuthentication( type, Protos.Authentication.CredentialsType.LOGIN, username, password );

        System.out.println( "Enviado" );

        Protos.ServerResponse response = middleware.getMailbox().readResponse();

        if ( response.hasError() ) { System.out.println( "Invalido, tente novamente." ); } else {
            System.out.println( "Login aceite, bem vindo. " );

            if ( type.equals( Protos.Authentication.UserType.COMPANY ) ) { state = State.COMPANY; } else {
                state = State.INVESTOR;
            }

            this.middleware.getMailbox().setWaitingResponse( false );
        }
    }

    //////////////// HELPER FUNCTIONS /////////////////
    public String transformMessage ( String message ) {
        Pattern pattern = Pattern.compile( "<comp:([0-1]+)>" );
        Matcher m       = pattern.matcher( message );

        Set< Integer > ids = new HashSet<>();

        while ( m.find() ) {
            ids.add( Integer.parseInt( m.group( 1 ) ) );
        }

        String transformed = message;

        for ( Integer id : ids ) {
            String name = this.directory.getCompanyName( id );

            transformed = transformed.replaceAll( "<comp:" + id + ">", name );
        }

        return transformed;
    }
}


