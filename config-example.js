module.exports = {
  TEST_SERVER: "http://localhost:8000",
  PRODUCTION_SERVER: "https://app.somedomain.com",
  HOMEPAGE_URL: "https://somedomain.com",
  LEGACY_URL: "https://example.com",
  LEGACY_TEST_PASSWORD: "asdfasdf",
  COUCHDB_HOST: "localhost",
  COUCHDB_PORT: "5984",
  COUCHDB_SERVER: "https://app.somedomain.com/db",
  PUBLIC_KEY: { "keydata": "somedata" },
  SUPPORT_EMAIL: "some@email.com",
  SUPPORT_URGENT_EMAIL: "urgent@email.com",
  FRESHDESK_APPID: 12341234,
  BEAMER_APPID: "myappidforbeamer",
  LOGROCKET_APPID: "asdf/app-name",
  TESTIMONIAL_URL: "https://example.testimonialdomain.com",
  DESKTOP_SERIAL_SALT: 'some salt string',
  DESKTOP_PURCHASE_URL: 'https://somedomain.com/buy',
  DESKTOP_PURCHASE_SUCCESS_URL: 'https://somedomain.com/success',
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
    , etc :
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
