package rest.representations;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class Company implements Serializable {
    private int Id;
    private String name;
    private String password;
    private String zone;
    private List<Integer> auctions;

    public Company(){}

    public Company(int id, String name, String password, String zone){
        this.Id = id;
        this.name=name;
        this.password=password;
        this.zone=zone;
        this.auctions = new ArrayList<>();
    }


    public Company(String name,String password, String zone){
        this.Id = -1;
        this.name=name;
        this.password=password;
        this.zone=zone;
        this.auctions = new ArrayList<>();
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public int getId() {
        return Id;
    }

    public void setId(int id) {
        Id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getZone() {
        return zone;
    }

    public void setZone(String zone) {
        this.zone = zone;
    }

    public List<Integer> getAuctions() {
        return auctions;
    }

    public void setAuctions(List<Integer> auctions) {
        this.auctions = auctions;
    }

    public void addAuction(int auctionId){
        this.auctions.add(auctionId);
    }

    public String toString(){
        return "Name " + this.name + " , Zone " + this.zone +"\nAuctions Participated: " + this.auctions.toString();
    }


}
