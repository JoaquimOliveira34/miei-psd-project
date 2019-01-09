package rest.representations;

import java.util.ArrayList;
import java.util.List;

public class Investor {
    private int Id;
    private String name;
    private String zone;
    private List<Integer> auctionsBidded;
    public Investor(){};

    public Investor(int id,String name, String zone){
        this.name=name;
        this.zone=zone;
        this.auctionsBidded = new ArrayList<>();
    }
    public Investor(String name, String zone){
        this.Id = -1;
        this.name=name;
        this.zone=zone;
        this.auctionsBidded = new ArrayList<>();
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

    public List<Integer> getAuctionsBidded() {
        return auctionsBidded;
    }

    public void setAuctionsBidded(List<Integer> auctionsBidded) {
        this.auctionsBidded = auctionsBidded;
    }

    public void addBid(int auctionId){
        this.auctionsBidded.add(auctionId);
    }

    public String toString(){
        return "Name " + this.name + " , Zone " + this.zone +"\nAuctions Participated: " + this.auctionsBidded.toString();
    }
}
