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
        { discount: "price_id_234234"
        , regular: "price_id_12341234"
        , bonus: "price_id_542543"
        }
      , yearly :
        { discount: "price_id_1234"
        , regular: "price_id_1234"
        , bonus: "price_id_1234"
        }
      }
    , INR :
      { monthly :
        { discount: "price_1234"
        , regular: "price_1234"
        , bonus: "price_1234"
        }
      , yearly :
        { discount: "price_1234"
        , regular: "price_1234"
        , bonus: "price_1234"
        }
      }
    , GBP :
      { monthly :
        { discount: "price_1234"
        , regular: "price_1234"
        , bonus: "price_1234"
        }
      , yearly :
        { discount: "price_1234"
        , regular: "price_1234"
        , bonus: "price_1234"
        }
      }
   }
};
