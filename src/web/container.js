const userStore =
  { get: (...args) => {
      var item = localStorage.getItem(...args);
      switch (item) {
        case "true":
          return true;

        case "false":
          return false;

        case null:
          return args[1];

        default:
          return item;
      }
    }
  , set: (...args) => localStorage.setItem(...args)
  };

const getInitialDocState = () => {
  const url = new URL(window.location);
  const treeName = url.searchParams.get("treeId") || "defaultTree";
  var docState =
    { dbPath: [treeName]
    , lastSavedToFile : 0
    , changed: false
    , jsonImportData: false
    };
  return docState;
};

const justLog = (...args) => {
  console.log("sendTo", ...args);
};

export
  { justLog as sendTo
  , justLog as msgWas
  , justLog as answerMain
  , getInitialDocState
  , userStore
  , justLog as openExternal
  , justLog as showMessageBox
  , justLog as exportDocx
  , justLog as exportJson
  , justLog as exportTxt
  };
