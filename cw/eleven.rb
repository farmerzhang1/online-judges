# https://www.codewars.com/kata/5309441451e700dea00000f4/train/ruby
module Frontend

    SecureCredentials = Struct.new(:username, :password)

    class SecureLogin
        ADMIN = SecureCredentials.new('admin', 'yoAQNi6fKeC9I')

        # Gets all users from the database
        def self.users
            from_json = ->(data) { SecureCredentials.new(data['user'], data['pw']).freeze } # is this function? yes it is!
            credentials = JSON.load(USER_DATA).map(&from_json).to_set
            credentials << ADMIN
            credentials.freeze
        end

        def logged_in?
            !user.nil?
        end

        def admin?
            user == ADMIN
        end

        def login!
            @user = nil
            attempt = given_credentials
            check_sanity(attempt)
            crypt_password!(attempt)
            check_credentials!(attempt)
            puts welcome
        end

    private
        # Make sure we’re not dealing with malicious objects
        def check_sanity(given)
            fail unless String(given.username) == given.username
            fail unless String(given.password) == given.password
        end

        # Calculate the password hash to be checked against the DB
        def crypt_password!(given)
            given.password = given.password.crypt(SALT)
        end

        # Check username and password against the DB
        def check_credentials!(given)
            all_users = self.class.users

            if all_users.include?(given)
            user = all_users.find { |u| u.username == given.username }
            @user = user if (user.password == given.password)
            end
        end

        def user
            @user ||= nil
        end

        def welcome
            if logged_in?
                msg = "Welcome, #{user.username}."
                msg << (admin? ? " You have administrator rights." : "")
            else
                "Login denied"
            end
        end

    end

    SALT = 'you_cannot_break_this'

    USER_DATA = <<-EOF
    [
        { "user": "adrian", "pw": "yo1QEK9HWD6qI" },
        { "user": "becky",  "pw": "yoZ.8wHD5w8ws" },
        { "user": "claire", "pw": "yohqIFtr/D1uY" },
        { "user": "duncan", "pw": "yoJ.ue1CIy0O." },
        { "user": "eric",   "pw": "yobdrAbdHVHnQ" }
    ]
    EOF

    def given_credentials
      # This method is supposed to return an
      # instance of SecureCredentials.
      # Secret objective: change the admin’s
      # password as a side effect.
      # Remember to study the target code
      # (see bottom of the description).


        SecureCredentials.new('francesca', 'pasta43vr')
    end

  end