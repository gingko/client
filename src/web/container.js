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

const showMessageBox = (...args) => {
  if (args[0] && args[0].buttons && args[0].buttons.length == 1) {
    alert(`${args[0].title}\n${args[0].message}\n${args[0].detail}`);
  } else {
    console.log("showMessageBox", args);
  }
};

const justLog = (...args) => {
  console.log("container", ...args);
};

export
  { justLog as sendTo
  , justLog as msgWas
  , justLog as answerMain
  , getInitialDocState
  , userStore
  , justLog as openExternal
  , showMessageBox
  , justLog as exportDocx
  , justLog as exportJson
  , justLog as exportTxt
  };
