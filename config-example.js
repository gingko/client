module.exports = {
  TEST_SERVER: "http://localhost:8000",
  LEGACY_URL: "https://example.com",
  LEGACY_TEST_PASSWORD: "asdfasdf",
  COUCHDB_HOST: "localhost",
  COUCHDB_PORT: "5984",
  COUCHDB_SERVER: "https://app.somedomain.com/db",
  COUCHDB_ADMIN_USERNAME: "username",
  COUCHDB_ADMIN_PASSWORD: "password",
  SUPPORT_EMAIL: "some@email.com",
  STRIPE_PUBLIC_KEY: "pk_test_123412341234",
  PRICE_DATA :
    { USD :
      { monthly :
        { regular: "price_id_12341234"
        , discount: "price_id_234234"
        , bonus: "price_id_542543"
        }
      , yearly :
        { regular: "price_id_1234"
        , discount: "price_id_1234"
        , bonus: "price_id_1234"
        }
      }
    , INR :
      { monthly :
        { regular: "price_0I9sEibEm7WWhuzY1mBaArgx"
        , discount: "price_0I9sEibEm7WWhuzYFsufwbEc"
        , bonus: "price_0I9sEjbEm7WWhuzY6Z88IprN"
        }
      , yearly :
        { regular: "price_0I9sEjbEm7WWhuzYeqXwYr59"
        , discount: "price_0I9sEjbEm7WWhuzYaoRXvNL7"
        , bonus: "price_0I9sEjbEm7WWhuzYCPhDUSI7"
        }
      }
    }
};
