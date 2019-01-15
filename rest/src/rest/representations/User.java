package rest.representations;

public class User {
        private static String user;
        private static String password;
        private static Boolean type;

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

        public Boolean getType() {
            return type;
        }

        public void setType(Boolean type) {
            this.type = type;
        }

        public User(String user, String password, Boolean type) {
            this.user = user;
            this.password = password;
            this.type = type;
        }

}
