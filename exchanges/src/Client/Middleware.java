package Client;

import Exchanges.Protos;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;

public class Middleware {

    private final DataInputStream cis;
    private final DataOutputStream cos;
    private final Socket sck;
    private final byte[] b1 = new byte[5];

    public Middleware(int ServerPort ) throws IOException {

        sck = new Socket( InetAddress.getLocalHost(), ServerPort);

        cis = new DataInputStream(sck.getInputStream());
        cos = new DataOutputStream(sck.getOutputStream());
    }

    public void sendAuthentication(Protos.Authentication.UserType userType,
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

    public List<Protos.ServerResponse> receiveAllMsg() throws IOException {
        List<Protos.ServerResponse> list = new ArrayList<>();

        while( cis.available() > 0 ){
            list.add( receiveMsg() );
        }
        return list;
    }

    public Protos.ServerResponse receiveMsg() throws IOException {

        // get size of message
        cis.readFully(b1);
        int msgSize = Protos.IntMessage.parseFrom( b1).getValue();

        //get message
        byte[] b2 = new byte[msgSize];
        cis.readFully(b2);

        return  Protos.ServerResponse.parseFrom( b2 );
    }

    public void close() throws IOException {
        sck.close();
    }
}
