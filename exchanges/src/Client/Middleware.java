package Client;

import Exchanges.Protos;
import org.zeromq.ZMQ;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;

public class Middleware {
    private final DataOutputStream cos;
    private final Socket sck;
    private final Mailbox mailbox;

    public Middleware( int ServerPort, int proxyPort ) throws IOException {
        sck = new Socket( InetAddress.getLocalHost(), ServerPort );

        cos = new DataOutputStream(sck.getOutputStream());

        this.mailbox = new Mailbox( ZMQ.context( 1 ), sck, proxyPort );

        this.mailbox.start();
    }

    public Mailbox getMailbox () {
        return mailbox;
    }

    public void sendAuthentication( Protos.Authentication.UserType userType,
                                    Protos.Authentication.CredentialsType credentialsType,
                                    String username, String password ) throws IOException {

        Protos.Authentication auth = Protos.Authentication.newBuilder()
                .setUserType( userType)
                .setCredentialsType( credentialsType)
                .setUsername( username)
                .setPassword( password)
                .build();

        byte[] ba = auth.toByteArray();

        cos.write(ba);
        cos.flush();
    }

    public void sendMsgCompany( Protos.MsgCompany.Type type, int amount, float rate) throws IOException {
        Protos.MsgCompany msgCompany = Protos.MsgCompany.newBuilder()
                .setType( type)
                .setAmount( amount)
                .setRate( rate)
                .build();

        byte[] ba = msgCompany.toByteArray();
        cos.write(ba);
        cos.flush();
    }

    public void sendMsgCompany( Protos.MsgCompany.Type type, int amount) throws IOException {
        Protos.MsgCompany msgCompany = Protos.MsgCompany.newBuilder()
                .setType( type)
                .setAmount( amount)
                .build();

        byte[] ba = msgCompany.toByteArray();
        cos.write(ba);
        cos.flush();
    }

    public void sendMsgInvestor( int companyId, int amount, float rate) throws IOException {
        Protos.MsgInvestor msgInvestor = Protos.MsgInvestor.newBuilder()
                .setCompany( companyId)
                .setAmount( amount)
                .setRate( rate)
                .build();

        byte[] ba = msgInvestor.toByteArray();
        cos.write(ba);
        cos.flush();

    }

    public void sendMsgInvestor( int companyId, int amount) throws IOException {
        Protos.MsgInvestor msgInvestor = Protos.MsgInvestor.newBuilder()
                .setCompany( companyId)
                .setAmount( amount)
                .build();

        byte[] ba = msgInvestor.toByteArray();
        cos.write(ba);
        cos.flush();
    }

    public void close() throws IOException {
        sck.close();
    }
}
