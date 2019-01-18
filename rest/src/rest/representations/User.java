package rest.representations;

public class User {
        private String user;
        private String password;
        private Boolean isInvestor;
        private int id;

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getUser() {
            return user;
        }

        public void setUser(String user) {
            this.user = user;
        }

        public String getPassword() {
            return password;
        }

        public void setPassword(String password) {
            this.password = password;
        }

        public Boolean getIsInvestor() {
            return isInvestor;
        }

        public void setIsInvestor(Boolean isInvestor) {
            this.isInvestor = isInvestor;
        }

        public User(String user, String password, int id, Boolean isInvestor) {
            this.user = user;
            this.password = password;
            this.isInvestor = isInvestor;
            this.id = id;
        }

}
