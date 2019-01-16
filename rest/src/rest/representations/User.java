package rest.representations;

public class User {
        private static String user;
        private static String password;
        private static Boolean isInvestor;
        private static int id;

    public static int getId() {
        return id;
    }

    public static void setId(int id) {
        User.id = id;
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
