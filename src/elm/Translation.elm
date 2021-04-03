module Translation exposing (Language(..), TranslationId(..), activeLanguages, dateFormat, datetimeFormat, langFromString, langToString, languageDecoder, languageName, timeDistInWords, tr)

import Json.Decode as Json exposing (..)
import Time
import Time.Distance as TimeDistance
import Time.Distance.I18n as I18n
import Time.Format exposing (format)
import Time.Format.Config.Config_de_de
import Time.Format.Config.Config_en_us
import Time.Format.Config.Config_es_es
import Time.Format.Config.Config_fr_fr
import Time.Format.Config.Config_nl_nl
import Time.Format.Config.Config_pt_br
import Time.Format.Config.Config_ru_ru
import Time.Format.Config.Config_sv_se


type TranslationId
    = Cancel
    | HomeBlank
    | HomeImportJSON
    | HomeJSONFrom
    | HomeImportLegacy
    | HomeLegacyFrom
    | RecentDocuments
    | LastUpdated
    | LastOpened
    | OpenOtherDocuments
    | DeleteDocument
    | RemoveFromList
    | NeverSaved
    | UnsavedChanges
    | SavedInternally
    | ChangesSaved
    | ChangesSynced
    | DatabaseError
    | LastSaved
    | LastEdit
    | KeyboardHelp
    | RestoreThisVersion
    | EnterKey
    | EnterAction
    | EditFullscreenAction
    | EditCardTitle
    | ArrowsAction
    | AddChildAction
    | SplitChildAction
    | InsertChildTitle
    | AddBelowAction
    | SplitBelowAction
    | MergeDownAction
    | InsertBelowTitle
    | AddAboveAction
    | SplitUpwardAction
    | MergeUpAction
    | InsertAboveTitle
    | ArrowKeys
    | MoveAction
    | Backspace
    | DeleteAction
    | DeleteCardTitle
    | FormattingGuide
    | ForBold
    | ForItalic
    | ToSaveChanges
    | SaveChangesTitle
    | EscKey
    | AreYouSureCancel
    | ToCancelChanges
    | PressToSearch
    | QuickDocumentSwitcher
    | EmailSupport
    | Logout
    | Language
    | ContributeTranslations
    | Here
    | HeadingFont
    | ContentFont
    | EditingFont
    | WordCountSession Int
    | WordCountTotal Int
    | WordCountCard Int
    | WordCountSubtree Int
    | WordCountGroup Int
    | WordCountColumn Int


type Language
    = En
    | Zh_HANS
    | Zh_HANT
    | Es
    | Fr
    | Ru
    | De
    | Nl
    | Hu
    | Sv
    | Ca
    | Br


languageName : Language -> String
languageName lang =
    case lang of
        En ->
            "English"

        Zh_HANS ->
            "简体中文"

        Zh_HANT ->
            "繁體中文"

        Es ->
            "Español"

        Fr ->
            "Français"

        Ru ->
            "ру́сский"

        De ->
            "Deutsch"

        Nl ->
            "Nederlands"

        Hu ->
            "magyar nyelv"

        Sv ->
            "Svenska"

        Ca ->
            "Català"

        Br ->
            "português"


activeLanguages : List ( Language, String )
activeLanguages =
    [ En, Zh_HANS, Zh_HANT, Es, Fr, Ru, Br, De, Nl, Hu, Sv, Ca ] |> List.map (\l -> ( l, languageName l ))


tr : Language -> TranslationId -> String
tr lang trans =
    let
        numberPlural n sing pl =
            if n == 1 then
                sing |> String.replace "%1" (String.fromInt n)

            else
                pl |> String.replace "%1" (String.fromInt n)

        translationSet =
            case trans of
                Cancel ->
                    { en = "Cancel"
                    , zh_hans = "%zh_hans:Cancel"
                    , zh_hant = "%zh_hant:Cancel"
                    , es = "%es:Cancel"
                    , fr = "%fr:Cancel"
                    , ru = "%ru:Cancel"
                    , de = "%de:Cancel"
                    , nl = "%nl:Cancel"
                    , hu = "%hu:Cancel"
                    , sv = "%sv:Cancel"
                    , ca = "%ca:Cancel"
                    , br = "%br:Cancel"
                    }

                HomeBlank ->
                    { en = "Blank Tree"
                    , zh_hans = "%zh_hans:HomeBlank"
                    , zh_hant = "%zh_hant:HomeBlank"
                    , es = "%es:HomeBlank"
                    , fr = "%fr:HomeBlank"
                    , ru = "%ru:HomeBlank"
                    , de = "%de:HomeBlank"
                    , nl = "%nl:HomeBlank"
                    , hu = "%hu:HomeBlank"
                    , sv = "%sv:HomeBlank"
                    , ca = "%ca:HomeBlank"
                    , br = "%br:HomeBlank"
                    }

                HomeImportJSON ->
                    { en = "Import JSON tree"
                    , zh_hans = "%zh_hans:HomeImportJSON"
                    , zh_hant = "%zh_hant:HomeImportJSON"
                    , es = "%es:HomeImportJSON"
                    , fr = "%fr:HomeImportJSON"
                    , ru = "%ru:HomeImportJSON"
                    , de = "%de:HomeImportJSON"
                    , nl = "%nl:HomeImportJSON"
                    , hu = "%hu:HomeImportJSON"
                    , sv = "%sv:HomeImportJSON"
                    , ca = "%ca:HomeImportJSON"
                    , br = "%br:HomeImportJSON"
                    }

                HomeJSONFrom ->
                    { en = "From Gingko Desktop or Online export file"
                    , zh_hans = "%zh_hans:HomeJSONFrom"
                    , zh_hant = "%zh_hant:HomeJSONFrom"
                    , es = "%es:HomeJSONFrom"
                    , fr = "%fr:HomeJSONFrom"
                    , ru = "%ru:HomeJSONFrom"
                    , de = "%de:HomeJSONFrom"
                    , nl = "%nl:HomeJSONFrom"
                    , hu = "%hu:HomeJSONFrom"
                    , sv = "%sv:HomeJSONFrom"
                    , ca = "%ca:HomeJSONFrom"
                    , br = "%br:HomeJSONFrom"
                    }

                HomeImportLegacy ->
                    { en = "From Old Account"
                    , zh_hans = "%zh_hans:HomeImportLegacy"
                    , zh_hant = "%zh_hant:HomeImportLegacy"
                    , es = "%es:HomeImportLegacy"
                    , fr = "%fr:HomeImportLegacy"
                    , ru = "%ru:HomeImportLegacy"
                    , de = "%de:HomeImportLegacy"
                    , nl = "%nl:HomeImportLegacy"
                    , hu = "%hu:HomeImportLegacy"
                    , sv = "%sv:HomeImportLegacy"
                    , ca = "%ca:HomeImportLegacy"
                    , br = "%br:HomeImportLegacy"
                    }

                HomeLegacyFrom ->
                    { en = "Bulk transfer of trees from your legacy account"
                    , zh_hans = "%zh_hans:HomeLegacyFrom"
                    , zh_hant = "%zh_hant:HomeLegacyFrom"
                    , es = "%es:HomeLegacyFrom"
                    , fr = "%fr:HomeLegacyFrom"
                    , ru = "%ru:HomeLegacyFrom"
                    , de = "%de:HomeLegacyFrom"
                    , nl = "%nl:HomeLegacyFrom"
                    , hu = "%hu:HomeLegacyFrom"
                    , sv = "%sv:HomeLegacyFrom"
                    , ca = "%ca:HomeLegacyFrom"
                    , br = "%br:HomeLegacyFrom"
                    }

                RecentDocuments ->
                    { en = "Recent Documents"
                    , zh_hans = "%zh_hans:RecentDocuments"
                    , zh_hant = "%zh_hant:RecentDocuments"
                    , es = "%es:RecentDocuments"
                    , fr = "%fr:RecentDocuments"
                    , ru = "%ru:RecentDocuments"
                    , de = "%de:RecentDocuments"
                    , nl = "%nl:RecentDocuments"
                    , hu = "%hu:RecentDocuments"
                    , sv = "%sv:RecentDocuments"
                    , ca = "%ca:RecentDocuments"
                    , br = "%br:RecentDocuments"
                    }

                LastUpdated ->
                    { en = "Last Updated"
                    , zh_hans = "%zh_hans:LastUpdated"
                    , zh_hant = "%zh_hant:LastUpdated"
                    , es = "%es:LastUpdated"
                    , fr = "%fr:LastUpdated"
                    , ru = "%ru:LastUpdated"
                    , de = "%de:LastUpdated"
                    , nl = "%nl:LastUpdated"
                    , hu = "%hu:LastUpdated"
                    , sv = "%sv:LastUpdated"
                    , ca = "%ca:LastUpdated"
                    , br = "%br:LastUpdated"
                    }

                LastOpened ->
                    { en = "Last Opened"
                    , zh_hans = "%zh_hans:LastOpened"
                    , zh_hant = "%zh_hant:LastOpened"
                    , es = "%es:LastOpened"
                    , fr = "%fr:LastOpened"
                    , ru = "%ru:LastOpened"
                    , de = "%de:LastOpened"
                    , nl = "%nl:LastOpened"
                    , hu = "%hu:LastOpened"
                    , sv = "%sv:LastOpened"
                    , ca = "%ca:LastOpened"
                    , br = "%br:LastOpened"
                    }

                OpenOtherDocuments ->
                    { en = "Open Other Documents"
                    , zh_hans = "%zh_hans:OpenOtherDocuments"
                    , zh_hant = "%zh_hant:OpenOtherDocuments"
                    , es = "%es:OpenOtherDocuments"
                    , fr = "%fr:OpenOtherDocuments"
                    , ru = "%ru:OpenOtherDocuments"
                    , de = "%de:OpenOtherDocuments"
                    , nl = "%nl:OpenOtherDocuments"
                    , hu = "%hu:OpenOtherDocuments"
                    , sv = "%sv:OpenOtherDocuments"
                    , ca = "%ca:OpenOtherDocuments"
                    , br = "%br:OpenOtherDocuments"
                    }

                DeleteDocument ->
                    { en = "Delete Document"
                    , zh_hans = "%zh_hans:DeleteDocument"
                    , zh_hant = "%zh_hant:DeleteDocument"
                    , es = "%es:DeleteDocument"
                    , fr = "%fr:DeleteDocument"
                    , ru = "%ru:DeleteDocument"
                    , de = "%de:DeleteDocument"
                    , nl = "%nl:DeleteDocument"
                    , hu = "%hu:DeleteDocument"
                    , sv = "%sv:DeleteDocument"
                    , ca = "%ca:DeleteDocument"
                    , br = "%br:DeleteDocument"
                    }

                RemoveFromList ->
                    { en = "Remove From List"
                    , zh_hans = "%zh_hans:RemoveFromList"
                    , zh_hant = "%zh_hant:RemoveFromList"
                    , es = "%es:RemoveFromList"
                    , fr = "%fr:RemoveFromList"
                    , ru = "%ru:RemoveFromList"
                    , de = "%de:RemoveFromList"
                    , nl = "%nl:RemoveFromList"
                    , hu = "%hu:RemoveFromList"
                    , sv = "%sv:RemoveFromList"
                    , ca = "%ca:RemoveFromList"
                    , br = "%br:RemoveFromList"
                    }

                NeverSaved ->
                    { en = "New Document..."
                    , zh_hans = "%zh_hans:NeverSaved"
                    , zh_hant = "%zh_hant:NeverSaved"
                    , es = "%es:NeverSaved"
                    , fr = "%fr:NeverSaved"
                    , ru = "%ru:NeverSaved"
                    , de = "%de:NeverSaved"
                    , nl = "%nl:NeverSaved"
                    , hu = "%hu:NeverSaved"
                    , sv = "%sv:NeverSaved"
                    , ca = "%ca:NeverSaved"
                    , br = "%br:NeverSaved"
                    }

                UnsavedChanges ->
                    { en = "Unsaved Changes..."
                    , zh_hans = "%zh_hans:UnsavedChanges"
                    , zh_hant = "%zh_hant:UnsavedChanges"
                    , es = "%es:UnsavedChanges"
                    , fr = "%fr:UnsavedChanges"
                    , ru = "%ru:UnsavedChanges"
                    , de = "%de:UnsavedChanges"
                    , nl = "%nl:UnsavedChanges"
                    , hu = "%hu:UnsavedChanges"
                    , sv = "%sv:UnsavedChanges"
                    , ca = "%ca:UnsavedChanges"
                    , br = "%br:UnsavedChanges"
                    }

                SavedInternally ->
                    { en = "Saved Offline"
                    , zh_hans = "%zh_hans:SavedInternally"
                    , zh_hant = "%zh_hant:SavedInternally"
                    , es = "%es:SavedInternally"
                    , fr = "%fr:SavedInternally"
                    , ru = "%ru:SavedInternally"
                    , de = "%de:SavedInternally"
                    , nl = "%nl:SavedInternally"
                    , hu = "%hu:SavedInternally"
                    , sv = "%sv:SavedInternally"
                    , ca = "%ca:SavedInternally"
                    , br = "%br:SavedInternally"
                    }

                ChangesSaved ->
                    { en = "Saved"
                    , zh_hans = "%zh_hans:ChangesSaved"
                    , zh_hant = "%zh_hant:ChangesSaved"
                    , es = "%es:ChangesSaved"
                    , fr = "%fr:ChangesSaved"
                    , ru = "%ru:ChangesSaved"
                    , de = "%de:ChangesSaved"
                    , nl = "%nl:ChangesSaved"
                    , hu = "%hu:ChangesSaved"
                    , sv = "%sv:ChangesSaved"
                    , ca = "%ca:ChangesSaved"
                    , br = "%br:ChangesSaved"
                    }

                ChangesSynced ->
                    { en = "Synced"
                    , zh_hans = "%zh_hans:ChangesSynced"
                    , zh_hant = "%zh_hant:ChangesSynced"
                    , es = "%es:ChangesSynced"
                    , fr = "%fr:ChangesSynced"
                    , ru = "%ru:ChangesSynced"
                    , de = "%de:ChangesSynced"
                    , nl = "%nl:ChangesSynced"
                    , hu = "%hu:ChangesSynced"
                    , sv = "%sv:ChangesSynced"
                    , ca = "%ca:ChangesSynced"
                    , br = "%br:ChangesSynced"
                    }

                DatabaseError ->
                    { en = "Database Error..."
                    , zh_hans = "%zh_hans:DatabaseError"
                    , zh_hant = "%zh_hant:DatabaseError"
                    , es = "%es:DatabaseError"
                    , fr = "%fr:DatabaseError"
                    , ru = "%ru:DatabaseError"
                    , de = "%de:DatabaseError"
                    , nl = "%nl:DatabaseError"
                    , hu = "%hu:DatabaseError"
                    , sv = "%sv:DatabaseError"
                    , ca = "%ca:DatabaseError"
                    , br = "%br:DatabaseError"
                    }

                LastSaved ->
                    { en = "Last saved"
                    , zh_hans = "%zh_hans:LastSaved"
                    , zh_hant = "%zh_hant:LastSaved"
                    , es = "%es:LastSaved"
                    , fr = "%fr:LastSaved"
                    , ru = "%ru:LastSaved"
                    , de = "%de:LastSaved"
                    , nl = "%nl:LastSaved"
                    , hu = "%hu:LastSaved"
                    , sv = "%sv:LastSaved"
                    , ca = "%ca:LastSaved"
                    , br = "%br:LastSaved"
                    }

                LastEdit ->
                    { en = "Last edit"
                    , zh_hans = "%zh_hans:LastEdit"
                    , zh_hant = "%zh_hant:LastEdit"
                    , es = "%es:LastEdit"
                    , fr = "%fr:LastEdit"
                    , ru = "%ru:LastEdit"
                    , de = "%de:LastEdit"
                    , nl = "%nl:LastEdit"
                    , hu = "%hu:LastEdit"
                    , sv = "%sv:LastEdit"
                    , ca = "%ca:LastEdit"
                    , br = "%br:LastEdit"
                    }

                KeyboardHelp ->
                    { en = "Keyboard Shortcuts Help"
                    , zh_hans = "%zh_hans:KeyboardHelp"
                    , zh_hant = "%zh_hant:KeyboardHelp"
                    , es = "%es:KeyboardHelp"
                    , fr = "%fr:KeyboardHelp"
                    , ru = "%ru:KeyboardHelp"
                    , de = "%de:KeyboardHelp"
                    , nl = "%nl:KeyboardHelp"
                    , hu = "%hu:KeyboardHelp"
                    , sv = "%sv:KeyboardHelp"
                    , ca = "%ca:KeyboardHelp"
                    , br = "%br:KeyboardHelp"
                    }

                RestoreThisVersion ->
                    { en = "Restore this Version"
                    , zh_hans = "%zh_hans:RestoreThisVersion"
                    , zh_hant = "%zh_hant:RestoreThisVersion"
                    , es = "%es:RestoreThisVersion"
                    , fr = "%fr:RestoreThisVersion"
                    , ru = "%ru:RestoreThisVersion"
                    , de = "%de:RestoreThisVersion"
                    , nl = "%nl:RestoreThisVersion"
                    , hu = "%hu:RestoreThisVersion"
                    , sv = "%sv:RestoreThisVersion"
                    , ca = "%ca:RestoreThisVersion"
                    , br = "%br:RestoreThisVersion"
                    }

                EnterKey ->
                    { en = "Enter"
                    , zh_hans = "%zh_hans:EnterKey"
                    , zh_hant = "%zh_hant:EnterKey"
                    , es = "%es:EnterKey"
                    , fr = "%fr:EnterKey"
                    , ru = "%ru:EnterKey"
                    , de = "%de:EnterKey"
                    , nl = "%nl:EnterKey"
                    , hu = "%hu:EnterKey"
                    , sv = "%sv:EnterKey"
                    , ca = "%ca:EnterKey"
                    , br = "%br:EnterKey"
                    }

                EnterAction ->
                    { en = "to Edit"
                    , zh_hans = "%zh_hans:EnterAction"
                    , zh_hant = "%zh_hant:EnterAction"
                    , es = "%es:EnterAction"
                    , fr = "%fr:EnterAction"
                    , ru = "%ru:EnterAction"
                    , de = "%de:EnterAction"
                    , nl = "%nl:EnterAction"
                    , hu = "%hu:EnterAction"
                    , sv = "%sv:EnterAction"
                    , ca = "%ca:EnterAction"
                    , br = "%br:EnterAction"
                    }

                EditFullscreenAction ->
                    { en = "to Edit in Fullscreen"
                    , zh_hans = "%zh_hans:EditFullscreenAction"
                    , zh_hant = "%zh_hant:EditFullscreenAction"
                    , es = "%es:EditFullscreenAction"
                    , fr = "%fr:EditFullscreenAction"
                    , ru = "%ru:EditFullscreenAction"
                    , de = "%de:EditFullscreenAction"
                    , nl = "%nl:EditFullscreenAction"
                    , hu = "%hu:EditFullscreenAction"
                    , sv = "%sv:EditFullscreenAction"
                    , ca = "%ca:EditFullscreenAction"
                    , br = "%br:EditFullscreenAction"
                    }

                EditCardTitle ->
                    { en = "Edit Card (Enter)"
                    , zh_hans = "%zh_hans:EditCardTitle"
                    , zh_hant = "%zh_hant:EditCardTitle"
                    , es = "%es:EditCardTitle"
                    , fr = "%fr:EditCardTitle"
                    , ru = "%ru:EditCardTitle"
                    , de = "%de:EditCardTitle"
                    , nl = "%nl:EditCardTitle"
                    , hu = "%hu:EditCardTitle"
                    , sv = "%sv:EditCardTitle"
                    , ca = "%ca:EditCardTitle"
                    , br = "%br:EditCardTitle"
                    }

                ArrowsAction ->
                    { en = "to Navigate"
                    , zh_hans = "%zh_hans:ArrowsAction"
                    , zh_hant = "%zh_hant:ArrowsAction"
                    , es = "%es:ArrowsAction"
                    , fr = "%fr:ArrowsAction"
                    , ru = "%ru:ArrowsAction"
                    , de = "%de:ArrowsAction"
                    , nl = "%nl:ArrowsAction"
                    , hu = "%hu:ArrowsAction"
                    , sv = "%sv:ArrowsAction"
                    , ca = "%ca:ArrowsAction"
                    , br = "%br:ArrowsAction"
                    }

                AddChildAction ->
                    { en = "to Add Child"
                    , zh_hans = "%zh_hans:AddChildAction"
                    , zh_hant = "%zh_hant:AddChildAction"
                    , es = "%es:AddChildAction"
                    , fr = "%fr:AddChildAction"
                    , ru = "%ru:AddChildAction"
                    , de = "%de:AddChildAction"
                    , nl = "%nl:AddChildAction"
                    , hu = "%hu:AddChildAction"
                    , sv = "%sv:AddChildAction"
                    , ca = "%ca:AddChildAction"
                    , br = "%br:AddChildAction"
                    }

                SplitChildAction ->
                    { en = "to Split Card to the Right"
                    , zh_hans = "%zh_hans:SplitChildAction"
                    , zh_hant = "%zh_hant:SplitChildAction"
                    , es = "%es:SplitChildAction"
                    , fr = "%fr:SplitChildAction"
                    , ru = "%ru:SplitChildAction"
                    , de = "%de:SplitChildAction"
                    , nl = "%nl:SplitChildAction"
                    , hu = "%hu:SplitChildAction"
                    , sv = "%sv:SplitChildAction"
                    , ca = "%ca:SplitChildAction"
                    , br = "%br:SplitChildAction"
                    }

                InsertChildTitle ->
                    { en = "Insert Child (Ctrl+L)"
                    , zh_hans = "%zh_hans:InsertChildTitle"
                    , zh_hant = "%zh_hant:InsertChildTitle"
                    , es = "%es:InsertChildTitle"
                    , fr = "%fr:InsertChildTitle"
                    , ru = "%ru:InsertChildTitle"
                    , de = "%de:InsertChildTitle"
                    , nl = "%nl:InsertChildTitle"
                    , hu = "%hu:InsertChildTitle"
                    , sv = "%sv:InsertChildTitle"
                    , ca = "%ca:InsertChildTitle"
                    , br = "%br:InsertChildTitle"
                    }

                AddBelowAction ->
                    { en = "to Add Below"
                    , zh_hans = "%zh_hans:AddBelowAction"
                    , zh_hant = "%zh_hant:AddBelowAction"
                    , es = "%es:AddBelowAction"
                    , fr = "%fr:AddBelowAction"
                    , ru = "%ru:AddBelowAction"
                    , de = "%de:AddBelowAction"
                    , nl = "%nl:AddBelowAction"
                    , hu = "%hu:AddBelowAction"
                    , sv = "%sv:AddBelowAction"
                    , ca = "%ca:AddBelowAction"
                    , br = "%br:AddBelowAction"
                    }

                SplitBelowAction ->
                    { en = "to Split Card Down"
                    , zh_hans = "%zh_hans:SplitBelowAction"
                    , zh_hant = "%zh_hant:SplitBelowAction"
                    , es = "%es:SplitBelowAction"
                    , fr = "%fr:SplitBelowAction"
                    , ru = "%ru:SplitBelowAction"
                    , de = "%de:SplitBelowAction"
                    , nl = "%nl:SplitBelowAction"
                    , hu = "%hu:SplitBelowAction"
                    , sv = "%sv:SplitBelowAction"
                    , ca = "%ca:SplitBelowAction"
                    , br = "%br:SplitBelowAction"
                    }

                MergeDownAction ->
                    { en = "to Merge into Next"
                    , zh_hans = "%zh_hans:MergeDownAction"
                    , zh_hant = "%zh_hant:MergeDownAction"
                    , es = "%es:MergeDownAction"
                    , fr = "%fr:MergeDownAction"
                    , ru = "%ru:MergeDownAction"
                    , de = "%de:MergeDownAction"
                    , nl = "%nl:MergeDownAction"
                    , hu = "%hu:MergeDownAction"
                    , sv = "%sv:MergeDownAction"
                    , ca = "%ca:MergeDownAction"
                    , br = "%br:MergeDownAction"
                    }

                InsertBelowTitle ->
                    { en = "Insert Below (Ctrl+J)"
                    , zh_hans = "%zh_hans:InsertBelowTitle"
                    , zh_hant = "%zh_hant:InsertBelowTitle"
                    , es = "%es:InsertBelowTitle"
                    , fr = "%fr:InsertBelowTitle"
                    , ru = "%ru:InsertBelowTitle"
                    , de = "%de:InsertBelowTitle"
                    , nl = "%nl:InsertBelowTitle"
                    , hu = "%hu:InsertBelowTitle"
                    , sv = "%sv:InsertBelowTitle"
                    , ca = "%ca:InsertBelowTitle"
                    , br = "%br:InsertBelowTitle"
                    }

                AddAboveAction ->
                    { en = "to Add Above"
                    , zh_hans = "%zh_hans:AddAboveAction"
                    , zh_hant = "%zh_hant:AddAboveAction"
                    , es = "%es:AddAboveAction"
                    , fr = "%fr:AddAboveAction"
                    , ru = "%ru:AddAboveAction"
                    , de = "%de:AddAboveAction"
                    , nl = "%nl:AddAboveAction"
                    , hu = "%hu:AddAboveAction"
                    , sv = "%sv:AddAboveAction"
                    , ca = "%ca:AddAboveAction"
                    , br = "%br:AddAboveAction"
                    }

                SplitUpwardAction ->
                    { en = "to Split Card Upward"
                    , zh_hans = "%zh_hans:SplitUpwardAction"
                    , zh_hant = "%zh_hant:SplitUpwardAction"
                    , es = "%es:SplitUpwardAction"
                    , fr = "%fr:SplitUpwardAction"
                    , ru = "%ru:SplitUpwardAction"
                    , de = "%de:SplitUpwardAction"
                    , nl = "%nl:SplitUpwardAction"
                    , hu = "%hu:SplitUpwardAction"
                    , sv = "%sv:SplitUpwardAction"
                    , ca = "%ca:SplitUpwardAction"
                    , br = "%br:SplitUpwardAction"
                    }

                MergeUpAction ->
                    { en = "to Merge into Previous"
                    , zh_hans = "%zh_hans:MergeUpAction"
                    , zh_hant = "%zh_hant:MergeUpAction"
                    , es = "%es:MergeUpAction"
                    , fr = "%fr:MergeUpAction"
                    , ru = "%ru:MergeUpAction"
                    , de = "%de:MergeUpAction"
                    , nl = "%nl:MergeUpAction"
                    , hu = "%hu:MergeUpAction"
                    , sv = "%sv:MergeUpAction"
                    , ca = "%ca:MergeUpAction"
                    , br = "%br:MergeUpAction"
                    }

                InsertAboveTitle ->
                    { en = "Insert Above (Ctrl+K)"
                    , zh_hans = "%zh_hans:InsertAboveTitle"
                    , zh_hant = "%zh_hant:InsertAboveTitle"
                    , es = "%es:InsertAboveTitle"
                    , fr = "%fr:InsertAboveTitle"
                    , ru = "%ru:InsertAboveTitle"
                    , de = "%de:InsertAboveTitle"
                    , nl = "%nl:InsertAboveTitle"
                    , hu = "%hu:InsertAboveTitle"
                    , sv = "%sv:InsertAboveTitle"
                    , ca = "%ca:InsertAboveTitle"
                    , br = "%br:InsertAboveTitle"
                    }

                ArrowKeys ->
                    { en = "(arrows)"
                    , zh_hans = "%zh_hans:ArrowKeys"
                    , zh_hant = "%zh_hant:ArrowKeys"
                    , es = "%es:ArrowKeys"
                    , fr = "%fr:ArrowKeys"
                    , ru = "%ru:ArrowKeys"
                    , de = "%de:ArrowKeys"
                    , nl = "%nl:ArrowKeys"
                    , hu = "%hu:ArrowKeys"
                    , sv = "%sv:ArrowKeys"
                    , ca = "%ca:ArrowKeys"
                    , br = "%br:ArrowKeys"
                    }

                MoveAction ->
                    { en = "to Move"
                    , zh_hans = "%zh_hans:MoveAction"
                    , zh_hant = "%zh_hant:MoveAction"
                    , es = "%es:MoveAction"
                    , fr = "%fr:MoveAction"
                    , ru = "%ru:MoveAction"
                    , de = "%de:MoveAction"
                    , nl = "%nl:MoveAction"
                    , hu = "%hu:MoveAction"
                    , sv = "%sv:MoveAction"
                    , ca = "%ca:MoveAction"
                    , br = "%br:MoveAction"
                    }

                Backspace ->
                    { en = "Backspace"
                    , zh_hans = "%zh_hans:Backspace"
                    , zh_hant = "%zh_hant:Backspace"
                    , es = "%es:Backspace"
                    , fr = "%fr:Backspace"
                    , ru = "%ru:Backspace"
                    , de = "%de:Backspace"
                    , nl = "%nl:Backspace"
                    , hu = "%hu:Backspace"
                    , sv = "%sv:Backspace"
                    , ca = "%ca:Backspace"
                    , br = "%br:Backspace"
                    }

                DeleteAction ->
                    { en = "to Delete"
                    , zh_hans = "%zh_hans:DeleteAction"
                    , zh_hant = "%zh_hant:DeleteAction"
                    , es = "%es:DeleteAction"
                    , fr = "%fr:DeleteAction"
                    , ru = "%ru:DeleteAction"
                    , de = "%de:DeleteAction"
                    , nl = "%nl:DeleteAction"
                    , hu = "%hu:DeleteAction"
                    , sv = "%sv:DeleteAction"
                    , ca = "%ca:DeleteAction"
                    , br = "%br:DeleteAction"
                    }

                DeleteCardTitle ->
                    { en = "Delete Card (Ctrl+Backspace)"
                    , zh_hans = "%zh_hans:DeleteCardTitle"
                    , zh_hant = "%zh_hant:DeleteCardTitle"
                    , es = "%es:DeleteCardTitle"
                    , fr = "%fr:DeleteCardTitle"
                    , ru = "%ru:DeleteCardTitle"
                    , de = "%de:DeleteCardTitle"
                    , nl = "%nl:DeleteCardTitle"
                    , hu = "%hu:DeleteCardTitle"
                    , sv = "%sv:DeleteCardTitle"
                    , ca = "%ca:DeleteCardTitle"
                    , br = "%br:DeleteCardTitle"
                    }

                FormattingGuide ->
                    { en = "More Formatting Options..."
                    , zh_hans = "%zh_hans:FormattingGuide"
                    , zh_hant = "%zh_hant:FormattingGuide"
                    , es = "%es:FormattingGuide"
                    , fr = "%fr:FormattingGuide"
                    , ru = "%ru:FormattingGuide"
                    , de = "%de:FormattingGuide"
                    , nl = "%nl:FormattingGuide"
                    , hu = "%hu:FormattingGuide"
                    , sv = "%sv:FormattingGuide"
                    , ca = "%ca:FormattingGuide"
                    , br = "%br:FormattingGuide"
                    }

                ForBold ->
                    { en = "for Bold"
                    , zh_hans = "%zh_hans:ForBold"
                    , zh_hant = "%zh_hant:ForBold"
                    , es = "%es:ForBold"
                    , fr = "%fr:ForBold"
                    , ru = "%ru:ForBold"
                    , de = "%de:ForBold"
                    , nl = "%nl:ForBold"
                    , hu = "%hu:ForBold"
                    , sv = "%sv:ForBold"
                    , ca = "%ca:ForBold"
                    , br = "%br:ForBold"
                    }

                ForItalic ->
                    { en = "for Italic"
                    , zh_hans = "%zh_hans:ForItalic"
                    , zh_hant = "%zh_hant:ForItalic"
                    , es = "%es:ForItalic"
                    , fr = "%fr:ForItalic"
                    , ru = "%ru:ForItalic"
                    , de = "%de:ForItalic"
                    , nl = "%nl:ForItalic"
                    , hu = "%hu:ForItalic"
                    , sv = "%sv:ForItalic"
                    , ca = "%ca:ForItalic"
                    , br = "%br:ForItalic"
                    }

                ToSaveChanges ->
                    { en = "to Save Changes"
                    , zh_hans = "%zh_hans:ToSaveChanges"
                    , zh_hant = "%zh_hant:ToSaveChanges"
                    , es = "%es:ToSaveChanges"
                    , fr = "%fr:ToSaveChanges"
                    , ru = "%ru:ToSaveChanges"
                    , de = "%de:ToSaveChanges"
                    , nl = "%nl:ToSaveChanges"
                    , hu = "%hu:ToSaveChanges"
                    , sv = "%sv:ToSaveChanges"
                    , ca = "%ca:ToSaveChanges"
                    , br = "%br:ToSaveChanges"
                    }

                SaveChangesTitle ->
                    { en = "Save Changes (Ctrl+Enter)"
                    , zh_hans = "%zh_hans:SaveChangesTitle"
                    , zh_hant = "%zh_hant:SaveChangesTitle"
                    , es = "%es:SaveChangesTitle"
                    , fr = "%fr:SaveChangesTitle"
                    , ru = "%ru:SaveChangesTitle"
                    , de = "%de:SaveChangesTitle"
                    , nl = "%nl:SaveChangesTitle"
                    , hu = "%hu:SaveChangesTitle"
                    , sv = "%sv:SaveChangesTitle"
                    , ca = "%ca:SaveChangesTitle"
                    , br = "%br:SaveChangesTitle"
                    }

                EscKey ->
                    { en = "Esc"
                    , zh_hans = "%zh_hans:EscKey"
                    , zh_hant = "%zh_hant:EscKey"
                    , es = "%es:EscKey"
                    , fr = "%fr:EscKey"
                    , ru = "%ru:EscKey"
                    , de = "%de:EscKey"
                    , nl = "%nl:EscKey"
                    , hu = "%hu:EscKey"
                    , sv = "%sv:EscKey"
                    , ca = "%ca:EscKey"
                    , br = "%br:EscKey"
                    }

                AreYouSureCancel ->
                    { en = "Are you sure you want to undo your changes?"
                    , zh_hans = "%zh_hans:AreYouSureCancel"
                    , zh_hant = "%zh_hant:AreYouSureCancel"
                    , es = "%es:AreYouSureCancel"
                    , fr = "%fr:AreYouSureCancel"
                    , ru = "%ru:AreYouSureCancel"
                    , de = "%de:AreYouSureCancel"
                    , nl = "%nl:AreYouSureCancel"
                    , hu = "%hu:AreYouSureCancel"
                    , sv = "%sv:AreYouSureCancel"
                    , ca = "%ca:AreYouSureCancel"
                    , br = "%br:AreYouSureCancel"
                    }

                ToCancelChanges ->
                    { en = "to Cancel Changes"
                    , zh_hans = "%zh_hans:ToCancelChanges"
                    , zh_hant = "%zh_hant:ToCancelChanges"
                    , es = "%es:ToCancelChanges"
                    , fr = "%fr:ToCancelChanges"
                    , ru = "%ru:ToCancelChanges"
                    , de = "%de:ToCancelChanges"
                    , nl = "%nl:ToCancelChanges"
                    , hu = "%hu:ToCancelChanges"
                    , sv = "%sv:ToCancelChanges"
                    , ca = "%ca:ToCancelChanges"
                    , br = "%br:ToCancelChanges"
                    }

                PressToSearch ->
                    { en = "Press '/' to search"
                    , zh_hans = "%zh_hans:PressToSearch"
                    , zh_hant = "%zh_hant:PressToSearch"
                    , es = "%es:PressToSearch"
                    , fr = "%fr:PressToSearch"
                    , ru = "%ru:PressToSearch"
                    , de = "%de:PressToSearch"
                    , nl = "%nl:PressToSearch"
                    , hu = "%hu:PressToSearch"
                    , sv = "%sv:PressToSearch"
                    , ca = "%ca:PressToSearch"
                    , br = "%br:PressToSearch"
                    }

                QuickDocumentSwitcher ->
                    { en = "Quick Document Switcher"
                    , zh_hans = "%zh_hans:QuickDocumentSwitcher"
                    , zh_hant = "%zh_hant:QuickDocumentSwitcher"
                    , es = "%es:QuickDocumentSwitcher"
                    , fr = "%fr:QuickDocumentSwitcher"
                    , ru = "%ru:QuickDocumentSwitcher"
                    , de = "%de:QuickDocumentSwitcher"
                    , nl = "%nl:QuickDocumentSwitcher"
                    , hu = "%hu:QuickDocumentSwitcher"
                    , sv = "%sv:QuickDocumentSwitcher"
                    , ca = "%ca:QuickDocumentSwitcher"
                    , br = "%br:QuickDocumentSwitcher"
                    }

                EmailSupport ->
                    { en = "\u{1F92F} Help!"
                    , zh_hans = "%zh_hans:EmailSupport"
                    , zh_hant = "%zh_hant:EmailSupport"
                    , es = "%es:EmailSupport"
                    , fr = "%fr:EmailSupport"
                    , ru = "%ru:EmailSupport"
                    , de = "%de:EmailSupport"
                    , nl = "%nl:EmailSupport"
                    , hu = "%hu:EmailSupport"
                    , sv = "%sv:EmailSupport"
                    , ca = "%ca:EmailSupport"
                    , br = "%br:EmailSupport"
                    }

                Logout ->
                    { en = "Logout"
                    , zh_hans = "%zh_hans:Logout"
                    , zh_hant = "%zh_hant:Logout"
                    , es = "%es:Logout"
                    , fr = "%fr:Logout"
                    , ru = "%ru:Logout"
                    , de = "%de:Logout"
                    , nl = "%nl:Logout"
                    , hu = "%hu:Logout"
                    , sv = "%sv:Logout"
                    , ca = "%ca:Logout"
                    , br = "%br:Logout"
                    }

                Language ->
                    { en = "Language"
                    , zh_hans = "%zh_hans:Language"
                    , zh_hant = "%zh_hant:Language"
                    , es = "%es:Language"
                    , fr = "%fr:Language"
                    , ru = "%ru:Language"
                    , de = "%de:Language"
                    , nl = "%nl:Language"
                    , hu = "%hu:Language"
                    , sv = "%sv:Language"
                    , ca = "%ca:Language"
                    , br = "%br:Language"
                    }

                ContributeTranslations ->
                    { en = "Contribute translations"
                    , zh_hans = "%zh_hans:ContributeTranslations"
                    , zh_hant = "%zh_hant:ContributeTranslations"
                    , es = "%es:ContributeTranslations"
                    , fr = "%fr:ContributeTranslations"
                    , ru = "%ru:ContributeTranslations"
                    , de = "%de:ContributeTranslations"
                    , nl = "%nl:ContributeTranslations"
                    , hu = "%hu:ContributeTranslations"
                    , sv = "%sv:ContributeTranslations"
                    , ca = "%ca:ContributeTranslations"
                    , br = "%br:ContributeTranslations"
                    }

                Here ->
                    { en = "here"
                    , zh_hans = "%zh_hans:Here"
                    , zh_hant = "%zh_hant:Here"
                    , es = "%es:Here"
                    , fr = "%fr:Here"
                    , ru = "%ru:Here"
                    , de = "%de:Here"
                    , nl = "%nl:Here"
                    , hu = "%hu:Here"
                    , sv = "%sv:Here"
                    , ca = "%ca:Here"
                    , br = "%br:Here"
                    }

                HeadingFont ->
                    { en = "Heading Font"
                    , zh_hans = "%zh_hans:HeadingFont"
                    , zh_hant = "%zh_hant:HeadingFont"
                    , es = "%es:HeadingFont"
                    , fr = "%fr:HeadingFont"
                    , ru = "%ru:HeadingFont"
                    , de = "%de:HeadingFont"
                    , nl = "%nl:HeadingFont"
                    , hu = "%hu:HeadingFont"
                    , sv = "%sv:HeadingFont"
                    , ca = "%ca:HeadingFont"
                    , br = "%br:HeadingFont"
                    }

                ContentFont ->
                    { en = "Content Font"
                    , zh_hans = "%zh_hans:ContentFont"
                    , zh_hant = "%zh_hant:ContentFont"
                    , es = "%es:ContentFont"
                    , fr = "%fr:ContentFont"
                    , ru = "%ru:ContentFont"
                    , de = "%de:ContentFont"
                    , nl = "%nl:ContentFont"
                    , hu = "%hu:ContentFont"
                    , sv = "%sv:ContentFont"
                    , ca = "%ca:ContentFont"
                    , br = "%br:ContentFont"
                    }

                EditingFont ->
                    { en = "Editing/Monospace Font"
                    , zh_hans = "%zh_hans:EditingFont"
                    , zh_hant = "%zh_hant:EditingFont"
                    , es = "%es:EditingFont"
                    , fr = "%fr:EditingFont"
                    , ru = "%ru:EditingFont"
                    , de = "%de:EditingFont"
                    , nl = "%nl:EditingFont"
                    , hu = "%hu:EditingFont"
                    , sv = "%sv:EditingFont"
                    , ca = "%ca:EditingFont"
                    , br = "%br:EditingFont"
                    }

                WordCountSession n ->
                    { en = numberPlural n "Session : %1 word" "Session : %1 words"
                    , zh_hans = numberPlural n "%zh_hans:WordCountSession:0" "%zh_hans:WordCountSession:1"
                    , zh_hant = numberPlural n "%zh_hant:WordCountSession:0" "%zh_hant:WordCountSession:1"
                    , es = numberPlural n "%es:WordCountSession:0" "%es:WordCountSession:1"
                    , fr = numberPlural n "%fr:WordCountSession:0" "%fr:WordCountSession:1"
                    , ru = numberPlural n "%ru:WordCountSession:0" "%ru:WordCountSession:1"
                    , de = numberPlural n "%de:WordCountSession:0" "%de:WordCountSession:1"
                    , nl = numberPlural n "%nl:WordCountSession:0" "%nl:WordCountSession:1"
                    , hu = numberPlural n "%hu:WordCountSession:0" "%hu:WordCountSession:1"
                    , sv = numberPlural n "%sv:WordCountSession:0" "%sv:WordCountSession:1"
                    , ca = numberPlural n "%ca:WordCountSession:0" "%ca:WordCountSession:1"
                    , br = numberPlural n "%br:WordCountSession:0" "%br:WordCountSession:1"
                    }

                WordCountTotal n ->
                    { en = numberPlural n "Total : %1 word" "Total : %1 words"
                    , zh_hans = numberPlural n "%zh_hans:WordCountTotal:0" "%zh_hans:WordCountTotal:1"
                    , zh_hant = numberPlural n "%zh_hant:WordCountTotal:0" "%zh_hant:WordCountTotal:1"
                    , es = numberPlural n "%es:WordCountTotal:0" "%es:WordCountTotal:1"
                    , fr = numberPlural n "%fr:WordCountTotal:0" "%fr:WordCountTotal:1"
                    , ru = numberPlural n "%ru:WordCountTotal:0" "%ru:WordCountTotal:1"
                    , de = numberPlural n "%de:WordCountTotal:0" "%de:WordCountTotal:1"
                    , nl = numberPlural n "%nl:WordCountTotal:0" "%nl:WordCountTotal:1"
                    , hu = numberPlural n "%hu:WordCountTotal:0" "%hu:WordCountTotal:1"
                    , sv = numberPlural n "%sv:WordCountTotal:0" "%sv:WordCountTotal:1"
                    , ca = numberPlural n "%ca:WordCountTotal:0" "%ca:WordCountTotal:1"
                    , br = numberPlural n "%br:WordCountTotal:0" "%br:WordCountTotal:1"
                    }

                WordCountCard n ->
                    { en = numberPlural n "Card : %1 word" "Card : %1 words"
                    , zh_hans = numberPlural n "%zh_hans:WordCountCard:0" "%zh_hans:WordCountCard:1"
                    , zh_hant = numberPlural n "%zh_hant:WordCountCard:0" "%zh_hant:WordCountCard:1"
                    , es = numberPlural n "%es:WordCountCard:0" "%es:WordCountCard:1"
                    , fr = numberPlural n "%fr:WordCountCard:0" "%fr:WordCountCard:1"
                    , ru = numberPlural n "%ru:WordCountCard:0" "%ru:WordCountCard:1"
                    , de = numberPlural n "%de:WordCountCard:0" "%de:WordCountCard:1"
                    , nl = numberPlural n "%nl:WordCountCard:0" "%nl:WordCountCard:1"
                    , hu = numberPlural n "%hu:WordCountCard:0" "%hu:WordCountCard:1"
                    , sv = numberPlural n "%sv:WordCountCard:0" "%sv:WordCountCard:1"
                    , ca = numberPlural n "%ca:WordCountCard:0" "%ca:WordCountCard:1"
                    , br = numberPlural n "%br:WordCountCard:0" "%br:WordCountCard:1"
                    }

                WordCountSubtree n ->
                    { en = numberPlural n "Subtree : %1 word" "Subtree : %1 words"
                    , zh_hans = numberPlural n "%zh_hans:WordCountSubtree:0" "%zh_hans:WordCountSubtree:1"
                    , zh_hant = numberPlural n "%zh_hant:WordCountSubtree:0" "%zh_hant:WordCountSubtree:1"
                    , es = numberPlural n "%es:WordCountSubtree:0" "%es:WordCountSubtree:1"
                    , fr = numberPlural n "%fr:WordCountSubtree:0" "%fr:WordCountSubtree:1"
                    , ru = numberPlural n "%ru:WordCountSubtree:0" "%ru:WordCountSubtree:1"
                    , de = numberPlural n "%de:WordCountSubtree:0" "%de:WordCountSubtree:1"
                    , nl = numberPlural n "%nl:WordCountSubtree:0" "%nl:WordCountSubtree:1"
                    , hu = numberPlural n "%hu:WordCountSubtree:0" "%hu:WordCountSubtree:1"
                    , sv = numberPlural n "%sv:WordCountSubtree:0" "%sv:WordCountSubtree:1"
                    , ca = numberPlural n "%ca:WordCountSubtree:0" "%ca:WordCountSubtree:1"
                    , br = numberPlural n "%br:WordCountSubtree:0" "%br:WordCountSubtree:1"
                    }

                WordCountGroup n ->
                    { en = numberPlural n "Group : %1 word" "Group : %1 words"
                    , zh_hans = numberPlural n "%zh_hans:WordCountGroup:0" "%zh_hans:WordCountGroup:1"
                    , zh_hant = numberPlural n "%zh_hant:WordCountGroup:0" "%zh_hant:WordCountGroup:1"
                    , es = numberPlural n "%es:WordCountGroup:0" "%es:WordCountGroup:1"
                    , fr = numberPlural n "%fr:WordCountGroup:0" "%fr:WordCountGroup:1"
                    , ru = numberPlural n "%ru:WordCountGroup:0" "%ru:WordCountGroup:1"
                    , de = numberPlural n "%de:WordCountGroup:0" "%de:WordCountGroup:1"
                    , nl = numberPlural n "%nl:WordCountGroup:0" "%nl:WordCountGroup:1"
                    , hu = numberPlural n "%hu:WordCountGroup:0" "%hu:WordCountGroup:1"
                    , sv = numberPlural n "%sv:WordCountGroup:0" "%sv:WordCountGroup:1"
                    , ca = numberPlural n "%ca:WordCountGroup:0" "%ca:WordCountGroup:1"
                    , br = numberPlural n "%br:WordCountGroup:0" "%br:WordCountGroup:1"
                    }

                WordCountColumn n ->
                    { en = numberPlural n "Column : %1 word" "Column : %1 words"
                    , zh_hans = numberPlural n "%zh_hans:WordCountColumn:0" "%zh_hans:WordCountColumn:1"
                    , zh_hant = numberPlural n "%zh_hant:WordCountColumn:0" "%zh_hant:WordCountColumn:1"
                    , es = numberPlural n "%es:WordCountColumn:0" "%es:WordCountColumn:1"
                    , fr = numberPlural n "%fr:WordCountColumn:0" "%fr:WordCountColumn:1"
                    , ru = numberPlural n "%ru:WordCountColumn:0" "%ru:WordCountColumn:1"
                    , de = numberPlural n "%de:WordCountColumn:0" "%de:WordCountColumn:1"
                    , nl = numberPlural n "%nl:WordCountColumn:0" "%nl:WordCountColumn:1"
                    , hu = numberPlural n "%hu:WordCountColumn:0" "%hu:WordCountColumn:1"
                    , sv = numberPlural n "%sv:WordCountColumn:0" "%sv:WordCountColumn:1"
                    , ca = numberPlural n "%ca:WordCountColumn:0" "%ca:WordCountColumn:1"
                    , br = numberPlural n "%br:WordCountColumn:0" "%br:WordCountColumn:1"
                    }
    in
    case lang of
        En ->
            .en translationSet

        Zh_HANS ->
            .zh_hans translationSet

        Zh_HANT ->
            .zh_hant translationSet

        Es ->
            .es translationSet

        Fr ->
            .fr translationSet

        Ru ->
            .ru translationSet

        De ->
            .de translationSet

        Nl ->
            .nl translationSet

        Hu ->
            .hu translationSet

        Sv ->
            .sv translationSet

        Ca ->
            .ca translationSet

        Br ->
            .br translationSet


timeDistInWords : Language -> Time.Posix -> Time.Posix -> String
timeDistInWords lang t1 t2 =
    case lang of
        En ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.en t1 t2

        Zh_HANS ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.en t1 t2

        Zh_HANT ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.en t1 t2

        Es ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.es t1 t2

        Fr ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.fr t1 t2

        Ru ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.en t1 t2

        De ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.en t1 t2

        Nl ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.en t1 t2

        Hu ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.en t1 t2

        Sv ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.en t1 t2

        Ca ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.en t1 t2

        Br ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.en t1 t2


dateFormat : Language -> Time.Posix -> String
dateFormat lang time =
    posixToString "%B%e, %Y" lang time


datetimeFormat : Language -> Time.Posix -> String
datetimeFormat lang time =
    posixToString "%B%e, %Y, %-I:%M %p" lang time


posixToString : String -> Language -> Time.Posix -> String
posixToString formatString lang time =
    case lang of
        En ->
            format Time.Format.Config.Config_en_us.config formatString Time.utc time

        Zh_HANS ->
            format Time.Format.Config.Config_en_us.config formatString Time.utc time

        Zh_HANT ->
            format Time.Format.Config.Config_en_us.config formatString Time.utc time

        Es ->
            format Time.Format.Config.Config_es_es.config formatString Time.utc time

        Fr ->
            format Time.Format.Config.Config_fr_fr.config formatString Time.utc time

        Ru ->
            format Time.Format.Config.Config_ru_ru.config formatString Time.utc time

        De ->
            format Time.Format.Config.Config_de_de.config formatString Time.utc time

        Nl ->
            format Time.Format.Config.Config_nl_nl.config formatString Time.utc time

        Hu ->
            format Time.Format.Config.Config_en_us.config formatString Time.utc time

        Sv ->
            format Time.Format.Config.Config_sv_se.config formatString Time.utc time

        Ca ->
            format Time.Format.Config.Config_en_us.config formatString Time.utc time

        Br ->
            format Time.Format.Config.Config_pt_br.config formatString Time.utc time


languageDecoder : Decoder Language
languageDecoder =
    Json.map langFromString string


langFromString : String -> Language
langFromString str =
    case str of
        "en" ->
            En

        "zh" ->
            Zh_HANS

        "zh_HANS" ->
            Zh_HANS

        "zh_HANT" ->
            Zh_HANT

        "es" ->
            Es

        "fr" ->
            Fr

        "ru" ->
            Ru

        "de" ->
            De

        "nl" ->
            Nl

        "hu" ->
            Hu

        "sv" ->
            Sv

        "ca" ->
            Ca

        "br" ->
            Br

        _ ->
            En


langToString : Language -> String
langToString lang =
    case lang of
        En ->
            "en"

        Zh_HANS ->
            "zh_HANS"

        Zh_HANT ->
            "zh_HANT"

        Es ->
            "es"

        Fr ->
            "fr"

        Ru ->
            "ru"

        De ->
            "de"

        Nl ->
            "nl"

        Hu ->
            "hu"

        Sv ->
            "sv"

        Ca ->
            "ca"

        Br ->
            "br"
