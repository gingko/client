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
    { usd :
        { monthly :
            { regular: "price_id_12341234"
            , discount: "price_id_234234"
            , bonus: "price_id_542543"
            }
        }
    }
};
