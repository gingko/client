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
import Time.Format.Config.Config_ja_jp
import Time.Format.Config.Config_nl_nl
import Time.Format.Config.Config_pt_br
import Time.Format.Config.Config_ru_ru
import Time.Format.Config.Config_sv_se


type TranslationId
    = NoTr String
    | Cancel
      -- Template and Import modal
    | NewDocument
    | ShowDocumentList
    | SortByName
    | SortByLastModified
    | SortByDateCreated
    | TemplatesAndExamples
    | New
    | HomeBlank
    | HomeImportJSON
    | HomeJSONFrom
    | ImportSectionTitle
    | HomeImportLegacy
    | HomeLegacyFrom
    | ImportTextFiles
    | ImportTextFilesDesc
    | ImportOpmlFiles
    | ImportOpmlFilesDesc
    | TimelineTemplate
    | TimelineTemplateDesc
    | AcademicPaperTemplate
    | AcademicPaperTemplateDesc
    | ProjectBrainstormingTemplate
    | ProjectBrainstormingTemplateDesc
    | HerosJourneyTemplate
    | HerosJourneyTemplateDesc
      --
    | RecentDocuments
    | LastUpdated
    | LastOpened
    | OpenOtherDocuments
    | DuplicateDocument
    | DeleteDocument
    | RemoveFromList
    | NeverSaved
    | Loading
    | UnsavedChanges
    | SavedInternally
    | ChangesSaved
    | ChangesSynced
    | DatabaseError
    | LastSaved
    | LastSynced
    | LastEdit
    | ConfirmBannerStrong
    | ConfirmBannerBody
    | Help
    | WhatsNew
    | AccountTooltip
      -- Keyboard Shortcut Help
    | KeyboardShortcuts
    | ViewModeShortcuts
    | CardEditCreateDelete
    | NavigationMovingCards
    | CopyPaste
    | SearchingMerging
    | HelpInfoDocs
    | EditModeShortcuts
    | CardSaveCreate
    | EditCard
    | AddCardBelow
    | AddCardAbove
    | AddCardToRight
    | AddCardBelowSplit
    | AddCardAboveSplit
    | AddCardToRightSplit
    | DeleteCard
    | GoUpDownLeftRight
    | GoToBeginningOfGroup
    | GoToEndOfGroup
    | GoToBeginningOfColumn
    | GoToEndOfColumn
    | MoveCurrentCard
    | PageUp
    | PageDown
    | HomeKey
    | EndKey
    | AnyOfAbove
    | DragCard
    | Search
    | ClearSearch
    | MergeCardUp
    | MergeCardDown
    | WorksAcrossDocuments
    | CopyCurrent
    | PasteBelow
    | PasteAsChild
    | InsertSelected
    | DragSelected
    | WordCounts
    | SwitchDocuments
    | ThisHelpScreen
    | Or
    | EditCardFullscreen
    | EditCards
    | KeyboardHelp
    | RestoreThisVersion
    | EnterKey
    | ShiftKey
    | EnterAction
    | AltKey
    | EditFullscreenAction
    | Navigate
    | EditCardTitle
    | ArrowsAction
    | AddNewCards
    | AddChildAction
    | InsertChildTitle
    | AddBelowAction
    | InsertBelowTitle
    | AddAboveAction
    | AIFeatures
    | ToOpenAIPrompt
    | AIGenerateChildren
    | AIGenerateBelow
    | SplitAtCursor
    | SplitChildAction
    | SplitBelowAction
    | SplitUpwardAction
    | MergeCards
    | MergeDownAction
    | MergeUpAction
    | InsertAboveTitle
    | ArrowKeys
    | MoveAndDelete
    | MoveAction
    | Backspace
    | DeleteAction
    | DeleteCardTitle
    | FormattingGuide
    | ForBold
    | BoldSelection
    | ForItalic
    | ItalicizeSelection
    | ForInsertLink
    | InsertLink
    | SaveChanges
    | SaveChangesAndExit
    | ExitEditMode
    | ToSaveChanges
    | SaveChangesTitle
    | EscKey
    | OtherShortcuts
    | DisplayWordCounts
    | EditMode
    | SaveOrCancelChanges
    | Formatting
    | FormattingTitle
    | SetTitleLevel
    | FormattingList
    | FormattingLink
    | ParenNumber
    | SetHeadingLevel
    | HelpVideos
    | FAQAndDocs
      --
    | AreYouSureCancel
    | ToCancelChanges
    | PressToSearch
    | QuickDocumentSwitcher
    | OpenQuickSwitcher
    | ContactSupport
    | Logout
    | Language
    | ContributeTranslations
    | Here
    | HeadingFont
    | ContentFont
    | EditingFont
    | MigrateTooltip
    | VersionHistory
    | DocumentSettings
    | WordCount
    | WordCountSession Int
    | WordCountTotal Int
    | WordCountCard Int
    | WordCountSubtree Int
    | WordCountGroup Int
    | WordCountColumn Int
    | CharacterCountCard Int
    | CharacterCountSubtree Int
    | CharacterCountGroup Int
    | CharacterCountColumn Int
    | CharacterCountTotal Int
    | WordCountTotalCards Int
    | DocumentTheme
    | ThemeDefault
    | ThemeDarkMode
    | ThemeClassic
    | ThemeGray
    | ThemeGreen
    | ThemeTurquoise
      -- Exporting
    | ExportOrPrint
    | ExportSettingEverything
    | ExportSettingEverythingDesc
    | ExportSettingCurrentSubtree
    | ExportSettingCurrentSubtreeDesc
    | ExportSettingLeavesOnly
    | ExportSettingLeavesOnlyDesc
    | ExportSettingCurrentColumn
    | ExportSettingCurrentColumnDesc
    | ExportSettingWord
    | ExportSettingPlainText
    | ExportSettingJSON
    | ExportSettingOPML
    | CloseExportView
    | DownloadWordFile
    | DownloadTextFile
    | DownloadJSONFile
    | DownloadOPMLFile
    | PrintThis
      -- Upgrade & Subscription
    | Upgrade
    | DaysLeft Int
    | TrialExpired
    | WordOfMouthCTA1
    | WordOfMouthCTA2
    | ManageSubscription


type Language
    = En
    | Zh_HANS
    | Zh_HANT
    | Es
    | Ar
    | Fr
    | Ru
    | De
    | Ja
    | Mr
    | Pes
    | It
    | Ro
    | Hr
    | Nl
    | Hu
    | Sv
    | Ca
    | Br
    | Cs
    | Fa
    | Id
    | Ko
    | Pl
    | Uk
    | Nb


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

        Ar ->
            "العربية"

        Fr ->
            "Français"

        Ru ->
            "ру́сский"

        De ->
            "Deutsch"

        Ja ->
            "日本語"

        Mr ->
            "मराठी"

        Pes ->
            "فارسی"

        It ->
            "italiano"

        Ro ->
            "română"

        Hr ->
            "hrvatski"

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

        Cs ->
            "čeština"

        Fa ->
            "فارسی"

        Id ->
            "Bahasa Indonesia"

        Ko ->
            "한국어"

        Pl ->
            "Polski"

        Uk ->
            "українська"

        Nb ->
            "Norsk Bokmål"


activeLanguages : List ( Language, String )
activeLanguages =
    [ En
    , Zh_HANS
    , Zh_HANT
    , Es
    , Ar
    , Fr
    , Br
    , Uk
    , Ru
    , Id
    , De
    , Ja
    , Mr
    , Pes
    , It
    , Ro
    , Hr
    , Nl
    , Hu
    , Sv
    , Ca
    , Cs
    , Ko
    , Fa
    , Pl
    , Nb
    ]
        |> List.map (\l -> ( l, languageName l ))


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
                NoTr str ->
                    { en = str
                    , zh_hans = str
                    , zh_hant = str
                    , es = str
                    , ar = str
                    , fr = str
                    , ru = str
                    , de = str
                    , ja = str
                    , pes = str
                    , it = str
                    , ro = str
                    , hr = str
                    , nl = str
                    , hu = str
                    , sv = str
                    , ca = str
                    , br = str
                    , mr = str
                    , cs = str
                    , fa = str
                    , id = str
                    , ko = str
                    , pl = str
                    , uk = str
                    , nb = str
                    }

                Cancel ->
                    { en = "Cancel"
                    , zh_hans = "%zh_hans:Cancel%"
                    , zh_hant = "%zh_hant:Cancel%"
                    , es = "%es:Cancel%"
                    , ar = "%ar:Cancel%"
                    , fr = "%fr:Cancel%"
                    , ru = "%ru:Cancel%"
                    , de = "%de:Cancel%"
                    , ja = "%ja:Cancel%"
                    , pes = "%pes:Cancel%"
                    , it = "%it:Cancel%"
                    , ro = "%ro:Cancel%"
                    , hr = "%hr:Cancel%"
                    , nl = "%nl:Cancel%"
                    , hu = "%hu:Cancel%"
                    , sv = "%sv:Cancel%"
                    , ca = "%ca:Cancel%"
                    , br = "%br:Cancel%"
                    , mr = "%mr:Cancel%"
                    , cs = "%cs:Cancel%"
                    , fa = "%fa:Cancel%"
                    , id = "%id:Cancel%"
                    , ko = "%ko:Cancel%"
                    , pl = "%pl:Cancel%"
                    , uk = "%uk:Cancel%"
                    , nb = "%nb:Cancel%"
                    }

                -- Template and Import modal
                NewDocument ->
                    { en = "New Document"
                    , zh_hans = "%zh_hans:NewDocument%"
                    , zh_hant = "%zh_hant:NewDocument%"
                    , es = "%es:NewDocument%"
                    , ar = "%ar:NewDocument%"
                    , fr = "%fr:NewDocument%"
                    , ru = "%ru:NewDocument%"
                    , de = "%de:NewDocument%"
                    , ja = "%ja:NewDocument%"
                    , pes = "%pes:NewDocument%"
                    , it = "%it:NewDocument%"
                    , ro = "%ro:NewDocument%"
                    , hr = "%hr:NewDocument%"
                    , nl = "%nl:NewDocument%"
                    , hu = "%hu:NewDocument%"
                    , sv = "%sv:NewDocument%"
                    , ca = "%ca:NewDocument%"
                    , br = "%br:NewDocument%"
                    , mr = "%mr:NewDocument%"
                    , cs = "%cs:NewDocument%"
                    , fa = "%fa:NewDocument%"
                    , id = "%id:NewDocument%"
                    , ko = "%ko:NewDocument%"
                    , pl = "%pl:NewDocument%"
                    , uk = "%uk:NewDocument%"
                    , nb = "%nb:NewDocument%"
                    }

                ShowDocumentList ->
                    { en = "Show Document List"
                    , zh_hans = "%zh_hans:ShowDocumentList%"
                    , zh_hant = "%zh_hant:ShowDocumentList%"
                    , es = "%es:ShowDocumentList%"
                    , ar = "%ar:ShowDocumentList%"
                    , fr = "%fr:ShowDocumentList%"
                    , ru = "%ru:ShowDocumentList%"
                    , de = "%de:ShowDocumentList%"
                    , ja = "%ja:ShowDocumentList%"
                    , pes = "%pes:ShowDocumentList%"
                    , it = "%it:ShowDocumentList%"
                    , ro = "%ro:ShowDocumentList%"
                    , hr = "%hr:ShowDocumentList%"
                    , nl = "%nl:ShowDocumentList%"
                    , hu = "%hu:ShowDocumentList%"
                    , sv = "%sv:ShowDocumentList%"
                    , ca = "%ca:ShowDocumentList%"
                    , br = "%br:ShowDocumentList%"
                    , mr = "%mr:ShowDocumentList%"
                    , cs = "%cs:ShowDocumentList%"
                    , fa = "%fa:ShowDocumentList%"
                    , id = "%id:ShowDocumentList%"
                    , ko = "%ko:ShowDocumentList%"
                    , pl = "%pl:ShowDocumentList%"
                    , uk = "%uk:ShowDocumentList%"
                    , nb = "%nb:ShowDocumentList%"
                    }

                SortByName ->
                    { en = "Sort by Name"
                    , zh_hans = "%zh_hans:SortByName%"
                    , zh_hant = "%zh_hant:SortByName%"
                    , es = "%es:SortByName%"
                    , ar = "%ar:SortByName%"
                    , fr = "%fr:SortByName%"
                    , ru = "%ru:SortByName%"
                    , de = "%de:SortByName%"
                    , ja = "%ja:SortByName%"
                    , pes = "%pes:SortByName%"
                    , it = "%it:SortByName%"
                    , ro = "%ro:SortByName%"
                    , hr = "%hr:SortByName%"
                    , nl = "%nl:SortByName%"
                    , hu = "%hu:SortByName%"
                    , sv = "%sv:SortByName%"
                    , ca = "%ca:SortByName%"
                    , br = "%br:SortByName%"
                    , mr = "%mr:SortByName%"
                    , cs = "%cs:SortByName%"
                    , fa = "%fa:SortByName%"
                    , id = "%id:SortByName%"
                    , ko = "%ko:SortByName%"
                    , pl = "%pl:SortByName%"
                    , uk = "%uk:SortByName%"
                    , nb = "%nb:SortByName%"
                    }

                SortByLastModified ->
                    { en = "Sort by Last Modified"
                    , zh_hans = "%zh_hans:SortByLastModified%"
                    , zh_hant = "%zh_hant:SortByLastModified%"
                    , es = "%es:SortByLastModified%"
                    , ar = "%ar:SortByLastModified%"
                    , fr = "%fr:SortByLastModified%"
                    , ru = "%ru:SortByLastModified%"
                    , de = "%de:SortByLastModified%"
                    , ja = "%ja:SortByLastModified%"
                    , pes = "%pes:SortByLastModified%"
                    , it = "%it:SortByLastModified%"
                    , ro = "%ro:SortByLastModified%"
                    , hr = "%hr:SortByLastModified%"
                    , nl = "%nl:SortByLastModified%"
                    , hu = "%hu:SortByLastModified%"
                    , sv = "%sv:SortByLastModified%"
                    , ca = "%ca:SortByLastModified%"
                    , br = "%br:SortByLastModified%"
                    , mr = "%mr:SortByLastModified%"
                    , cs = "%cs:SortByLastModified%"
                    , fa = "%fa:SortByLastModified%"
                    , id = "%id:SortByLastModified%"
                    , ko = "%ko:SortByLastModified%"
                    , pl = "%pl:SortByLastModified%"
                    , uk = "%uk:SortByLastModified%"
                    , nb = "%nb:SortByLastModified%"
                    }

                SortByDateCreated ->
                    { en = "Sort by Date Created"
                    , zh_hans = "%zh_hans:SortByDateCreated%"
                    , zh_hant = "%zh_hant:SortByDateCreated%"
                    , es = "%es:SortByDateCreated%"
                    , ar = "%ar:SortByDateCreated%"
                    , fr = "%fr:SortByDateCreated%"
                    , ru = "%ru:SortByDateCreated%"
                    , de = "%de:SortByDateCreated%"
                    , ja = "%ja:SortByDateCreated%"
                    , pes = "%pes:SortByDateCreated%"
                    , it = "%it:SortByDateCreated%"
                    , ro = "%ro:SortByDateCreated%"
                    , hr = "%hr:SortByDateCreated%"
                    , nl = "%nl:SortByDateCreated%"
                    , hu = "%hu:SortByDateCreated%"
                    , sv = "%sv:SortByDateCreated%"
                    , ca = "%ca:SortByDateCreated%"
                    , br = "%br:SortByDateCreated%"
                    , mr = "%mr:SortByDateCreated%"
                    , cs = "%cs:SortByDateCreated%"
                    , fa = "%fa:SortByDateCreated%"
                    , id = "%id:SortByDateCreated%"
                    , ko = "%ko:SortByDateCreated%"
                    , pl = "%pl:SortByDateCreated%"
                    , uk = "%uk:SortByDateCreated%"
                    , nb = "%nb:SortByDateCreated%"
                    }

                TemplatesAndExamples ->
                    { en = "Templates & Examples"
                    , zh_hans = "%zh_hans:TemplatesAndExamples%"
                    , zh_hant = "%zh_hant:TemplatesAndExamples%"
                    , es = "%es:TemplatesAndExamples%"
                    , ar = "%ar:TemplatesAndExamples%"
                    , fr = "%fr:TemplatesAndExamples%"
                    , ru = "%ru:TemplatesAndExamples%"
                    , de = "%de:TemplatesAndExamples%"
                    , ja = "%ja:TemplatesAndExamples%"
                    , pes = "%pes:TemplatesAndExamples%"
                    , it = "%it:TemplatesAndExamples%"
                    , ro = "%ro:TemplatesAndExamples%"
                    , hr = "%hr:TemplatesAndExamples%"
                    , nl = "%nl:TemplatesAndExamples%"
                    , hu = "%hu:TemplatesAndExamples%"
                    , sv = "%sv:TemplatesAndExamples%"
                    , ca = "%ca:TemplatesAndExamples%"
                    , br = "%br:TemplatesAndExamples%"
                    , mr = "%mr:TemplatesAndExamples%"
                    , cs = "%cs:TemplatesAndExamples%"
                    , fa = "%fa:TemplatesAndExamples%"
                    , id = "%id:TemplatesAndExamples%"
                    , ko = "%ko:TemplatesAndExamples%"
                    , pl = "%pl:TemplatesAndExamples%"
                    , uk = "%uk:TemplatesAndExamples%"
                    , nb = "%nb:TemplatesAndExamples%"
                    }

                New ->
                    { en = "New"
                    , zh_hans = "%zh_hans:New%"
                    , zh_hant = "%zh_hant:New%"
                    , es = "%es:New%"
                    , ar = "%ar:New%"
                    , fr = "%fr:New%"
                    , ru = "%ru:New%"
                    , de = "%de:New%"
                    , ja = "%ja:New%"
                    , pes = "%pes:New%"
                    , it = "%it:New%"
                    , ro = "%ro:New%"
                    , hr = "%hr:New%"
                    , nl = "%nl:New%"
                    , hu = "%hu:New%"
                    , sv = "%sv:New%"
                    , ca = "%ca:New%"
                    , br = "%br:New%"
                    , mr = "%mr:New%"
                    , cs = "%cs:New%"
                    , fa = "%fa:New%"
                    , id = "%id:New%"
                    , ko = "%ko:New%"
                    , pl = "%pl:New%"
                    , uk = "%uk:New%"
                    , nb = "%nb:New%"
                    }

                HomeBlank ->
                    { en = "Blank Tree"
                    , zh_hans = "%zh_hans:HomeBlank%"
                    , zh_hant = "%zh_hant:HomeBlank%"
                    , es = "%es:HomeBlank%"
                    , ar = "%ar:HomeBlank%"
                    , fr = "%fr:HomeBlank%"
                    , ru = "%ru:HomeBlank%"
                    , de = "%de:HomeBlank%"
                    , ja = "%ja:HomeBlank%"
                    , pes = "%pes:HomeBlank%"
                    , it = "%it:HomeBlank%"
                    , ro = "%ro:HomeBlank%"
                    , hr = "%hr:HomeBlank%"
                    , nl = "%nl:HomeBlank%"
                    , hu = "%hu:HomeBlank%"
                    , sv = "%sv:HomeBlank%"
                    , ca = "%ca:HomeBlank%"
                    , br = "%br:HomeBlank%"
                    , mr = "%mr:HomeBlank%"
                    , cs = "%cs:HomeBlank%"
                    , fa = "%fa:HomeBlank%"
                    , id = "%id:HomeBlank%"
                    , ko = "%ko:HomeBlank%"
                    , pl = "%pl:HomeBlank%"
                    , uk = "%uk:HomeBlank%"
                    , nb = "%nb:HomeBlank%"
                    }

                HomeImportJSON ->
                    { en = "Import JSON tree"
                    , zh_hans = "%zh_hans:HomeImportJSON%"
                    , zh_hant = "%zh_hant:HomeImportJSON%"
                    , es = "%es:HomeImportJSON%"
                    , ar = "%ar:HomeImportJSON%"
                    , fr = "%fr:HomeImportJSON%"
                    , ru = "%ru:HomeImportJSON%"
                    , de = "%de:HomeImportJSON%"
                    , ja = "%ja:HomeImportJSON%"
                    , pes = "%pes:HomeImportJSON%"
                    , it = "%it:HomeImportJSON%"
                    , ro = "%ro:HomeImportJSON%"
                    , hr = "%hr:HomeImportJSON%"
                    , nl = "%nl:HomeImportJSON%"
                    , hu = "%hu:HomeImportJSON%"
                    , sv = "%sv:HomeImportJSON%"
                    , ca = "%ca:HomeImportJSON%"
                    , br = "%br:HomeImportJSON%"
                    , mr = "%mr:HomeImportJSON%"
                    , cs = "%cs:HomeImportJSON%"
                    , fa = "%fa:HomeImportJSON%"
                    , id = "%id:HomeImportJSON%"
                    , ko = "%ko:HomeImportJSON%"
                    , pl = "%pl:HomeImportJSON%"
                    , uk = "%uk:HomeImportJSON%"
                    , nb = "%nb:HomeImportJSON%"
                    }

                HomeJSONFrom ->
                    { en = "From Gingko Desktop or Online export file"
                    , zh_hans = "%zh_hans:HomeJSONFrom%"
                    , zh_hant = "%zh_hant:HomeJSONFrom%"
                    , es = "%es:HomeJSONFrom%"
                    , ar = "%ar:HomeJSONFrom%"
                    , fr = "%fr:HomeJSONFrom%"
                    , ru = "%ru:HomeJSONFrom%"
                    , de = "%de:HomeJSONFrom%"
                    , ja = "%ja:HomeJSONFrom%"
                    , pes = "%pes:HomeJSONFrom%"
                    , it = "%it:HomeJSONFrom%"
                    , ro = "%ro:HomeJSONFrom%"
                    , hr = "%hr:HomeJSONFrom%"
                    , nl = "%nl:HomeJSONFrom%"
                    , hu = "%hu:HomeJSONFrom%"
                    , sv = "%sv:HomeJSONFrom%"
                    , ca = "%ca:HomeJSONFrom%"
                    , br = "%br:HomeJSONFrom%"
                    , mr = "%mr:HomeJSONFrom%"
                    , cs = "%cs:HomeJSONFrom%"
                    , fa = "%fa:HomeJSONFrom%"
                    , id = "%id:HomeJSONFrom%"
                    , ko = "%ko:HomeJSONFrom%"
                    , pl = "%pl:HomeJSONFrom%"
                    , uk = "%uk:HomeJSONFrom%"
                    , nb = "%nb:HomeJSONFrom%"
                    }

                ImportSectionTitle ->
                    { en = "Import"
                    , zh_hans = "%zh_hans:ImportSectionTitle%"
                    , zh_hant = "%zh_hant:ImportSectionTitle%"
                    , es = "%es:ImportSectionTitle%"
                    , ar = "%ar:ImportSectionTitle%"
                    , fr = "%fr:ImportSectionTitle%"
                    , ru = "%ru:ImportSectionTitle%"
                    , de = "%de:ImportSectionTitle%"
                    , ja = "%ja:ImportSectionTitle%"
                    , pes = "%pes:ImportSectionTitle%"
                    , it = "%it:ImportSectionTitle%"
                    , ro = "%ro:ImportSectionTitle%"
                    , hr = "%hr:ImportSectionTitle%"
                    , nl = "%nl:ImportSectionTitle%"
                    , hu = "%hu:ImportSectionTitle%"
                    , sv = "%sv:ImportSectionTitle%"
                    , ca = "%ca:ImportSectionTitle%"
                    , br = "%br:ImportSectionTitle%"
                    , mr = "%mr:ImportSectionTitle%"
                    , cs = "%cs:ImportSectionTitle%"
                    , fa = "%fa:ImportSectionTitle%"
                    , id = "%id:ImportSectionTitle%"
                    , ko = "%ko:ImportSectionTitle%"
                    , pl = "%pl:ImportSectionTitle%"
                    , uk = "%uk:ImportSectionTitle%"
                    , nb = "%nb:ImportSectionTitle%"
                    }

                HomeImportLegacy ->
                    { en = "From Old Account"
                    , zh_hans = "%zh_hans:HomeImportLegacy%"
                    , zh_hant = "%zh_hant:HomeImportLegacy%"
                    , es = "%es:HomeImportLegacy%"
                    , ar = "%ar:HomeImportLegacy%"
                    , fr = "%fr:HomeImportLegacy%"
                    , ru = "%ru:HomeImportLegacy%"
                    , de = "%de:HomeImportLegacy%"
                    , ja = "%ja:HomeImportLegacy%"
                    , pes = "%pes:HomeImportLegacy%"
                    , it = "%it:HomeImportLegacy%"
                    , ro = "%ro:HomeImportLegacy%"
                    , hr = "%hr:HomeImportLegacy%"
                    , nl = "%nl:HomeImportLegacy%"
                    , hu = "%hu:HomeImportLegacy%"
                    , sv = "%sv:HomeImportLegacy%"
                    , ca = "%ca:HomeImportLegacy%"
                    , br = "%br:HomeImportLegacy%"
                    , mr = "%mr:HomeImportLegacy%"
                    , cs = "%cs:HomeImportLegacy%"
                    , fa = "%fa:HomeImportLegacy%"
                    , id = "%id:HomeImportLegacy%"
                    , ko = "%ko:HomeImportLegacy%"
                    , pl = "%pl:HomeImportLegacy%"
                    , uk = "%uk:HomeImportLegacy%"
                    , nb = "%nb:HomeImportLegacy%"
                    }

                HomeLegacyFrom ->
                    { en = "Bulk transfer of trees from your legacy account"
                    , zh_hans = "%zh_hans:HomeLegacyFrom%"
                    , zh_hant = "%zh_hant:HomeLegacyFrom%"
                    , es = "%es:HomeLegacyFrom%"
                    , ar = "%ar:HomeLegacyFrom%"
                    , fr = "%fr:HomeLegacyFrom%"
                    , ru = "%ru:HomeLegacyFrom%"
                    , de = "%de:HomeLegacyFrom%"
                    , ja = "%ja:HomeLegacyFrom%"
                    , pes = "%pes:HomeLegacyFrom%"
                    , it = "%it:HomeLegacyFrom%"
                    , ro = "%ro:HomeLegacyFrom%"
                    , hr = "%hr:HomeLegacyFrom%"
                    , nl = "%nl:HomeLegacyFrom%"
                    , hu = "%hu:HomeLegacyFrom%"
                    , sv = "%sv:HomeLegacyFrom%"
                    , ca = "%ca:HomeLegacyFrom%"
                    , br = "%br:HomeLegacyFrom%"
                    , mr = "%mr:HomeLegacyFrom%"
                    , cs = "%cs:HomeLegacyFrom%"
                    , fa = "%fa:HomeLegacyFrom%"
                    , id = "%id:HomeLegacyFrom%"
                    , ko = "%ko:HomeLegacyFrom%"
                    , pl = "%pl:HomeLegacyFrom%"
                    , uk = "%uk:HomeLegacyFrom%"
                    , nb = "%nb:HomeLegacyFrom%"
                    }

                ImportTextFiles ->
                    { en = "Import Text Files"
                    , zh_hans = "%zh_hans:ImportTextFiles%"
                    , zh_hant = "%zh_hant:ImportTextFiles%"
                    , es = "%es:ImportTextFiles%"
                    , ar = "%ar:ImportTextFiles%"
                    , fr = "%fr:ImportTextFiles%"
                    , ru = "%ru:ImportTextFiles%"
                    , de = "%de:ImportTextFiles%"
                    , ja = "%ja:ImportTextFiles%"
                    , pes = "%pes:ImportTextFiles%"
                    , it = "%it:ImportTextFiles%"
                    , ro = "%ro:ImportTextFiles%"
                    , hr = "%hr:ImportTextFiles%"
                    , nl = "%nl:ImportTextFiles%"
                    , hu = "%hu:ImportTextFiles%"
                    , sv = "%sv:ImportTextFiles%"
                    , ca = "%ca:ImportTextFiles%"
                    , br = "%br:ImportTextFiles%"
                    , mr = "%mr:ImportTextFiles%"
                    , cs = "%cs:ImportTextFiles%"
                    , fa = "%fa:ImportTextFiles%"
                    , id = "%id:ImportTextFiles%"
                    , ko = "%ko:ImportTextFiles%"
                    , pl = "%pl:ImportTextFiles%"
                    , uk = "%uk:ImportTextFiles%"
                    , nb = "%nb:ImportTextFiles%"
                    }

                ImportTextFilesDesc ->
                    { en = "Import multiple markdown or regular text files."
                    , zh_hans = "%zh_hans:ImportTextFilesDesc%"
                    , zh_hant = "%zh_hant:ImportTextFilesDesc%"
                    , es = "%es:ImportTextFilesDesc%"
                    , ar = "%ar:ImportTextFilesDesc%"
                    , fr = "%fr:ImportTextFilesDesc%"
                    , ru = "%ru:ImportTextFilesDesc%"
                    , de = "%de:ImportTextFilesDesc%"
                    , ja = "%ja:ImportTextFilesDesc%"
                    , pes = "%pes:ImportTextFilesDesc%"
                    , it = "%it:ImportTextFilesDesc%"
                    , ro = "%ro:ImportTextFilesDesc%"
                    , hr = "%hr:ImportTextFilesDesc%"
                    , nl = "%nl:ImportTextFilesDesc%"
                    , hu = "%hu:ImportTextFilesDesc%"
                    , sv = "%sv:ImportTextFilesDesc%"
                    , ca = "%ca:ImportTextFilesDesc%"
                    , br = "%br:ImportTextFilesDesc%"
                    , mr = "%mr:ImportTextFilesDesc%"
                    , cs = "%cs:ImportTextFilesDesc%"
                    , fa = "%fa:ImportTextFilesDesc%"
                    , id = "%id:ImportTextFilesDesc%"
                    , ko = "%ko:ImportTextFilesDesc%"
                    , pl = "%pl:ImportTextFilesDesc%"
                    , uk = "%uk:ImportTextFilesDesc%"
                    , nb = "%nb:ImportTextFilesDesc%"
                    }

                ImportOpmlFiles ->
                    { en = "Import Opml Files"
                    , zh_hans = "%zh_hans:ImportOpmlFiles%"
                    , zh_hant = "%zh_hant:ImportOpmlFiles%"
                    , es = "%es:ImportOpmlFiles%"
                    , ar = "%ar:ImportOpmlFiles%"
                    , fr = "%fr:ImportOpmlFiles%"
                    , ru = "%ru:ImportOpmlFiles%"
                    , de = "%de:ImportOpmlFiles%"
                    , ja = "%ja:ImportOpmlFiles%"
                    , pes = "%pes:ImportOpmlFiles%"
                    , it = "%it:ImportOpmlFiles%"
                    , ro = "%ro:ImportOpmlFiles%"
                    , hr = "%hr:ImportOpmlFiles%"
                    , nl = "%nl:ImportOpmlFiles%"
                    , hu = "%hu:ImportOpmlFiles%"
                    , sv = "%sv:ImportOpmlFiles%"
                    , ca = "%ca:ImportOpmlFiles%"
                    , br = "%br:ImportOpmlFiles%"
                    , mr = "%mr:ImportOpmlFiles%"
                    , cs = "%cs:ImportOpmlFiles%"
                    , fa = "%fa:ImportOpmlFiles%"
                    , id = "%id:ImportOpmlFiles%"
                    , ko = "%ko:ImportOpmlFiles%"
                    , pl = "%pl:ImportOpmlFiles%"
                    , uk = "%uk:ImportOpmlFiles%"
                    , nb = "%nb:ImportOpmlFiles%"
                    }

                ImportOpmlFilesDesc ->
                    { en = "Import from Workflowy or other outliners."
                    , zh_hans = "%zh_hans:ImportOpmlFilesDesc%"
                    , zh_hant = "%zh_hant:ImportOpmlFilesDesc%"
                    , es = "%es:ImportOpmlFilesDesc%"
                    , ar = "%ar:ImportOpmlFilesDesc%"
                    , fr = "%fr:ImportOpmlFilesDesc%"
                    , ru = "%ru:ImportOpmlFilesDesc%"
                    , de = "%de:ImportOpmlFilesDesc%"
                    , ja = "%ja:ImportOpmlFilesDesc%"
                    , pes = "%pes:ImportOpmlFilesDesc%"
                    , it = "%it:ImportOpmlFilesDesc%"
                    , ro = "%ro:ImportOpmlFilesDesc%"
                    , hr = "%hr:ImportOpmlFilesDesc%"
                    , nl = "%nl:ImportOpmlFilesDesc%"
                    , hu = "%hu:ImportOpmlFilesDesc%"
                    , sv = "%sv:ImportOpmlFilesDesc%"
                    , ca = "%ca:ImportOpmlFilesDesc%"
                    , br = "%br:ImportOpmlFilesDesc%"
                    , mr = "%mr:ImportOpmlFilesDesc%"
                    , cs = "%cs:ImportOpmlFilesDesc%"
                    , fa = "%fa:ImportOpmlFilesDesc%"
                    , id = "%id:ImportOpmlFilesDesc%"
                    , ko = "%ko:ImportOpmlFilesDesc%"
                    , pl = "%pl:ImportOpmlFilesDesc%"
                    , uk = "%uk:ImportOpmlFilesDesc%"
                    , nb = "%nb:ImportOpmlFilesDesc%"
                    }

                TimelineTemplate ->
                    { en = "Timeline 2024"
                    , zh_hans = "%zh_hans:TimelineTemplate%"
                    , zh_hant = "%zh_hant:TimelineTemplate%"
                    , es = "%es:TimelineTemplate%"
                    , ar = "%ar:TimelineTemplate%"
                    , fr = "%fr:TimelineTemplate%"
                    , ru = "%ru:TimelineTemplate%"
                    , de = "%de:TimelineTemplate%"
                    , ja = "%ja:TimelineTemplate%"
                    , pes = "%pes:TimelineTemplate%"
                    , it = "%it:TimelineTemplate%"
                    , ro = "%ro:TimelineTemplate%"
                    , hr = "%hr:TimelineTemplate%"
                    , nl = "%nl:TimelineTemplate%"
                    , hu = "%hu:TimelineTemplate%"
                    , sv = "%sv:TimelineTemplate%"
                    , ca = "%ca:TimelineTemplate%"
                    , br = "%br:TimelineTemplate%"
                    , mr = "%mr:TimelineTemplate%"
                    , cs = "%cs:TimelineTemplate%"
                    , fa = "%fa:TimelineTemplate%"
                    , id = "%id:TimelineTemplate%"
                    , ko = "%ko:TimelineTemplate%"
                    , pl = "%pl:TimelineTemplate%"
                    , uk = "%uk:TimelineTemplate%"
                    , nb = "%nb:TimelineTemplate%"
                    }

                TimelineTemplateDesc ->
                    { en = "A tree-based calendar"
                    , zh_hans = "%zh_hans:TimelineTemplateDesc%"
                    , zh_hant = "%zh_hant:TimelineTemplateDesc%"
                    , es = "%es:TimelineTemplateDesc%"
                    , ar = "%ar:TimelineTemplateDesc%"
                    , fr = "%fr:TimelineTemplateDesc%"
                    , ru = "%ru:TimelineTemplateDesc%"
                    , de = "%de:TimelineTemplateDesc%"
                    , ja = "%ja:TimelineTemplateDesc%"
                    , pes = "%pes:TimelineTemplateDesc%"
                    , it = "%it:TimelineTemplateDesc%"
                    , ro = "%ro:TimelineTemplateDesc%"
                    , hr = "%hr:TimelineTemplateDesc%"
                    , nl = "%nl:TimelineTemplateDesc%"
                    , hu = "%hu:TimelineTemplateDesc%"
                    , sv = "%sv:TimelineTemplateDesc%"
                    , ca = "%ca:TimelineTemplateDesc%"
                    , br = "%br:TimelineTemplateDesc%"
                    , mr = "%mr:TimelineTemplateDesc%"
                    , cs = "%cs:TimelineTemplateDesc%"
                    , fa = "%fa:TimelineTemplateDesc%"
                    , id = "%id:TimelineTemplateDesc%"
                    , ko = "%ko:TimelineTemplateDesc%"
                    , pl = "%pl:TimelineTemplateDesc%"
                    , uk = "%uk:TimelineTemplateDesc%"
                    , nb = "%nb:TimelineTemplateDesc%"
                    }

                AcademicPaperTemplate ->
                    { en = "Academic Paper"
                    , zh_hans = "%zh_hans:AcademicPaperTemplate%"
                    , zh_hant = "%zh_hant:AcademicPaperTemplate%"
                    , es = "%es:AcademicPaperTemplate%"
                    , ar = "%ar:AcademicPaperTemplate%"
                    , fr = "%fr:AcademicPaperTemplate%"
                    , ru = "%ru:AcademicPaperTemplate%"
                    , de = "%de:AcademicPaperTemplate%"
                    , ja = "%ja:AcademicPaperTemplate%"
                    , pes = "%pes:AcademicPaperTemplate%"
                    , it = "%it:AcademicPaperTemplate%"
                    , ro = "%ro:AcademicPaperTemplate%"
                    , hr = "%hr:AcademicPaperTemplate%"
                    , nl = "%nl:AcademicPaperTemplate%"
                    , hu = "%hu:AcademicPaperTemplate%"
                    , sv = "%sv:AcademicPaperTemplate%"
                    , ca = "%ca:AcademicPaperTemplate%"
                    , br = "%br:AcademicPaperTemplate%"
                    , mr = "%mr:AcademicPaperTemplate%"
                    , cs = "%cs:AcademicPaperTemplate%"
                    , fa = "%fa:AcademicPaperTemplate%"
                    , id = "%id:AcademicPaperTemplate%"
                    , ko = "%ko:AcademicPaperTemplate%"
                    , pl = "%pl:AcademicPaperTemplate%"
                    , uk = "%uk:AcademicPaperTemplate%"
                    , nb = "%nb:AcademicPaperTemplate%"
                    }

                AcademicPaperTemplateDesc ->
                    { en = "Starting point for journal paper"
                    , zh_hans = "%zh_hans:AcademicPaperTemplateDesc%"
                    , zh_hant = "%zh_hant:AcademicPaperTemplateDesc%"
                    , es = "%es:AcademicPaperTemplateDesc%"
                    , ar = "%ar:AcademicPaperTemplateDesc%"
                    , fr = "%fr:AcademicPaperTemplateDesc%"
                    , ru = "%ru:AcademicPaperTemplateDesc%"
                    , de = "%de:AcademicPaperTemplateDesc%"
                    , ja = "%ja:AcademicPaperTemplateDesc%"
                    , pes = "%pes:AcademicPaperTemplateDesc%"
                    , it = "%it:AcademicPaperTemplateDesc%"
                    , ro = "%ro:AcademicPaperTemplateDesc%"
                    , hr = "%hr:AcademicPaperTemplateDesc%"
                    , nl = "%nl:AcademicPaperTemplateDesc%"
                    , hu = "%hu:AcademicPaperTemplateDesc%"
                    , sv = "%sv:AcademicPaperTemplateDesc%"
                    , ca = "%ca:AcademicPaperTemplateDesc%"
                    , br = "%br:AcademicPaperTemplateDesc%"
                    , mr = "%mr:AcademicPaperTemplateDesc%"
                    , cs = "%cs:AcademicPaperTemplateDesc%"
                    , fa = "%fa:AcademicPaperTemplateDesc%"
                    , id = "%id:AcademicPaperTemplateDesc%"
                    , ko = "%ko:AcademicPaperTemplateDesc%"
                    , pl = "%pl:AcademicPaperTemplateDesc%"
                    , uk = "%uk:AcademicPaperTemplateDesc%"
                    , nb = "%nb:AcademicPaperTemplateDesc%"
                    }

                ProjectBrainstormingTemplate ->
                    { en = "Project Brainstorming"
                    , zh_hans = "%zh_hans:ProjectBrainstormingTemplate%"
                    , zh_hant = "%zh_hant:ProjectBrainstormingTemplate%"
                    , es = "%es:ProjectBrainstormingTemplate%"
                    , ar = "%ar:ProjectBrainstormingTemplate%"
                    , fr = "%fr:ProjectBrainstormingTemplate%"
                    , ru = "%ru:ProjectBrainstormingTemplate%"
                    , de = "%de:ProjectBrainstormingTemplate%"
                    , ja = "%ja:ProjectBrainstormingTemplate%"
                    , pes = "%pes:ProjectBrainstormingTemplate%"
                    , it = "%it:ProjectBrainstormingTemplate%"
                    , ro = "%ro:ProjectBrainstormingTemplate%"
                    , hr = "%hr:ProjectBrainstormingTemplate%"
                    , nl = "%nl:ProjectBrainstormingTemplate%"
                    , hu = "%hu:ProjectBrainstormingTemplate%"
                    , sv = "%sv:ProjectBrainstormingTemplate%"
                    , ca = "%ca:ProjectBrainstormingTemplate%"
                    , br = "%br:ProjectBrainstormingTemplate%"
                    , mr = "%mr:ProjectBrainstormingTemplate%"
                    , cs = "%cs:ProjectBrainstormingTemplate%"
                    , fa = "%fa:ProjectBrainstormingTemplate%"
                    , id = "%id:ProjectBrainstormingTemplate%"
                    , ko = "%ko:ProjectBrainstormingTemplate%"
                    , pl = "%pl:ProjectBrainstormingTemplate%"
                    , uk = "%uk:ProjectBrainstormingTemplate%"
                    , nb = "%nb:ProjectBrainstormingTemplate%"
                    }

                ProjectBrainstormingTemplateDesc ->
                    { en = "Example on clarifying project goals"
                    , zh_hans = "%zh_hans:ProjectBrainstormingTemplateDesc%"
                    , zh_hant = "%zh_hant:ProjectBrainstormingTemplateDesc%"
                    , es = "%es:ProjectBrainstormingTemplateDesc%"
                    , ar = "%ar:ProjectBrainstormingTemplateDesc%"
                    , fr = "%fr:ProjectBrainstormingTemplateDesc%"
                    , ru = "%ru:ProjectBrainstormingTemplateDesc%"
                    , de = "%de:ProjectBrainstormingTemplateDesc%"
                    , ja = "%ja:ProjectBrainstormingTemplateDesc%"
                    , pes = "%pes:ProjectBrainstormingTemplateDesc%"
                    , it = "%it:ProjectBrainstormingTemplateDesc%"
                    , ro = "%ro:ProjectBrainstormingTemplateDesc%"
                    , hr = "%hr:ProjectBrainstormingTemplateDesc%"
                    , nl = "%nl:ProjectBrainstormingTemplateDesc%"
                    , hu = "%hu:ProjectBrainstormingTemplateDesc%"
                    , sv = "%sv:ProjectBrainstormingTemplateDesc%"
                    , ca = "%ca:ProjectBrainstormingTemplateDesc%"
                    , br = "%br:ProjectBrainstormingTemplateDesc%"
                    , mr = "%mr:ProjectBrainstormingTemplateDesc%"
                    , cs = "%cs:ProjectBrainstormingTemplateDesc%"
                    , fa = "%fa:ProjectBrainstormingTemplateDesc%"
                    , id = "%id:ProjectBrainstormingTemplateDesc%"
                    , ko = "%ko:ProjectBrainstormingTemplateDesc%"
                    , pl = "%pl:ProjectBrainstormingTemplateDesc%"
                    , uk = "%uk:ProjectBrainstormingTemplateDesc%"
                    , nb = "%nb:ProjectBrainstormingTemplateDesc%"
                    }

                HerosJourneyTemplate ->
                    { en = "Hero's Journey"
                    , zh_hans = "%zh_hans:HerosJourneyTemplate%"
                    , zh_hant = "%zh_hant:HerosJourneyTemplate%"
                    , es = "%es:HerosJourneyTemplate%"
                    , ar = "%ar:HerosJourneyTemplate%"
                    , fr = "%fr:HerosJourneyTemplate%"
                    , ru = "%ru:HerosJourneyTemplate%"
                    , de = "%de:HerosJourneyTemplate%"
                    , ja = "%ja:HerosJourneyTemplate%"
                    , pes = "%pes:HerosJourneyTemplate%"
                    , it = "%it:HerosJourneyTemplate%"
                    , ro = "%ro:HerosJourneyTemplate%"
                    , hr = "%hr:HerosJourneyTemplate%"
                    , nl = "%nl:HerosJourneyTemplate%"
                    , hu = "%hu:HerosJourneyTemplate%"
                    , sv = "%sv:HerosJourneyTemplate%"
                    , ca = "%ca:HerosJourneyTemplate%"
                    , br = "%br:HerosJourneyTemplate%"
                    , mr = "%mr:HerosJourneyTemplate%"
                    , cs = "%cs:HerosJourneyTemplate%"
                    , fa = "%fa:HerosJourneyTemplate%"
                    , id = "%id:HerosJourneyTemplate%"
                    , ko = "%ko:HerosJourneyTemplate%"
                    , pl = "%pl:HerosJourneyTemplate%"
                    , uk = "%uk:HerosJourneyTemplate%"
                    , nb = "%nb:HerosJourneyTemplate%"
                    }

                HerosJourneyTemplateDesc ->
                    { en = "A framework for fictional stories"
                    , zh_hans = "%zh_hans:HerosJourneyTemplateDesc%"
                    , zh_hant = "%zh_hant:HerosJourneyTemplateDesc%"
                    , es = "%es:HerosJourneyTemplateDesc%"
                    , ar = "%ar:HerosJourneyTemplateDesc%"
                    , fr = "%fr:HerosJourneyTemplateDesc%"
                    , ru = "%ru:HerosJourneyTemplateDesc%"
                    , de = "%de:HerosJourneyTemplateDesc%"
                    , ja = "%ja:HerosJourneyTemplateDesc%"
                    , pes = "%pes:HerosJourneyTemplateDesc%"
                    , it = "%it:HerosJourneyTemplateDesc%"
                    , ro = "%ro:HerosJourneyTemplateDesc%"
                    , hr = "%hr:HerosJourneyTemplateDesc%"
                    , nl = "%nl:HerosJourneyTemplateDesc%"
                    , hu = "%hu:HerosJourneyTemplateDesc%"
                    , sv = "%sv:HerosJourneyTemplateDesc%"
                    , ca = "%ca:HerosJourneyTemplateDesc%"
                    , br = "%br:HerosJourneyTemplateDesc%"
                    , mr = "%mr:HerosJourneyTemplateDesc%"
                    , cs = "%cs:HerosJourneyTemplateDesc%"
                    , fa = "%fa:HerosJourneyTemplateDesc%"
                    , id = "%id:HerosJourneyTemplateDesc%"
                    , ko = "%ko:HerosJourneyTemplateDesc%"
                    , pl = "%pl:HerosJourneyTemplateDesc%"
                    , uk = "%uk:HerosJourneyTemplateDesc%"
                    , nb = "%nb:HerosJourneyTemplateDesc%"
                    }

                --
                RecentDocuments ->
                    { en = "Recent Documents"
                    , zh_hans = "%zh_hans:RecentDocuments%"
                    , zh_hant = "%zh_hant:RecentDocuments%"
                    , es = "%es:RecentDocuments%"
                    , ar = "%ar:RecentDocuments%"
                    , fr = "%fr:RecentDocuments%"
                    , ru = "%ru:RecentDocuments%"
                    , de = "%de:RecentDocuments%"
                    , ja = "%ja:RecentDocuments%"
                    , pes = "%pes:RecentDocuments%"
                    , it = "%it:RecentDocuments%"
                    , ro = "%ro:RecentDocuments%"
                    , hr = "%hr:RecentDocuments%"
                    , nl = "%nl:RecentDocuments%"
                    , hu = "%hu:RecentDocuments%"
                    , sv = "%sv:RecentDocuments%"
                    , ca = "%ca:RecentDocuments%"
                    , br = "%br:RecentDocuments%"
                    , mr = "%mr:RecentDocuments%"
                    , cs = "%cs:RecentDocuments%"
                    , fa = "%fa:RecentDocuments%"
                    , id = "%id:RecentDocuments%"
                    , ko = "%ko:RecentDocuments%"
                    , pl = "%pl:RecentDocuments%"
                    , uk = "%uk:RecentDocuments%"
                    , nb = "%nb:RecentDocuments%"
                    }

                LastUpdated ->
                    { en = "Last Updated"
                    , zh_hans = "%zh_hans:LastUpdated%"
                    , zh_hant = "%zh_hant:LastUpdated%"
                    , es = "%es:LastUpdated%"
                    , ar = "%ar:LastUpdated%"
                    , fr = "%fr:LastUpdated%"
                    , ru = "%ru:LastUpdated%"
                    , de = "%de:LastUpdated%"
                    , ja = "%ja:LastUpdated%"
                    , pes = "%pes:LastUpdated%"
                    , it = "%it:LastUpdated%"
                    , ro = "%ro:LastUpdated%"
                    , hr = "%hr:LastUpdated%"
                    , nl = "%nl:LastUpdated%"
                    , hu = "%hu:LastUpdated%"
                    , sv = "%sv:LastUpdated%"
                    , ca = "%ca:LastUpdated%"
                    , br = "%br:LastUpdated%"
                    , mr = "%mr:LastUpdated%"
                    , cs = "%cs:LastUpdated%"
                    , fa = "%fa:LastUpdated%"
                    , id = "%id:LastUpdated%"
                    , ko = "%ko:LastUpdated%"
                    , pl = "%pl:LastUpdated%"
                    , uk = "%uk:LastUpdated%"
                    , nb = "%nb:LastUpdated%"
                    }

                LastOpened ->
                    { en = "Last Opened"
                    , zh_hans = "%zh_hans:LastOpened%"
                    , zh_hant = "%zh_hant:LastOpened%"
                    , es = "%es:LastOpened%"
                    , ar = "%ar:LastOpened%"
                    , fr = "%fr:LastOpened%"
                    , ru = "%ru:LastOpened%"
                    , de = "%de:LastOpened%"
                    , ja = "%ja:LastOpened%"
                    , pes = "%pes:LastOpened%"
                    , it = "%it:LastOpened%"
                    , ro = "%ro:LastOpened%"
                    , hr = "%hr:LastOpened%"
                    , nl = "%nl:LastOpened%"
                    , hu = "%hu:LastOpened%"
                    , sv = "%sv:LastOpened%"
                    , ca = "%ca:LastOpened%"
                    , br = "%br:LastOpened%"
                    , mr = "%mr:LastOpened%"
                    , cs = "%cs:LastOpened%"
                    , fa = "%fa:LastOpened%"
                    , id = "%id:LastOpened%"
                    , ko = "%ko:LastOpened%"
                    , pl = "%pl:LastOpened%"
                    , uk = "%uk:LastOpened%"
                    , nb = "%nb:LastOpened%"
                    }

                OpenOtherDocuments ->
                    { en = "Open Other Documents"
                    , zh_hans = "%zh_hans:OpenOtherDocuments%"
                    , zh_hant = "%zh_hant:OpenOtherDocuments%"
                    , es = "%es:OpenOtherDocuments%"
                    , ar = "%ar:OpenOtherDocuments%"
                    , fr = "%fr:OpenOtherDocuments%"
                    , ru = "%ru:OpenOtherDocuments%"
                    , de = "%de:OpenOtherDocuments%"
                    , ja = "%ja:OpenOtherDocuments%"
                    , pes = "%pes:OpenOtherDocuments%"
                    , it = "%it:OpenOtherDocuments%"
                    , ro = "%ro:OpenOtherDocuments%"
                    , hr = "%hr:OpenOtherDocuments%"
                    , nl = "%nl:OpenOtherDocuments%"
                    , hu = "%hu:OpenOtherDocuments%"
                    , sv = "%sv:OpenOtherDocuments%"
                    , ca = "%ca:OpenOtherDocuments%"
                    , br = "%br:OpenOtherDocuments%"
                    , mr = "%mr:OpenOtherDocuments%"
                    , cs = "%cs:OpenOtherDocuments%"
                    , fa = "%fa:OpenOtherDocuments%"
                    , id = "%id:OpenOtherDocuments%"
                    , ko = "%ko:OpenOtherDocuments%"
                    , pl = "%pl:OpenOtherDocuments%"
                    , uk = "%uk:OpenOtherDocuments%"
                    , nb = "%nb:OpenOtherDocuments%"
                    }

                DuplicateDocument ->
                    { en = "Duplicate Tree"
                    , zh_hans = "%zh_hans:DuplicateDocument%"
                    , zh_hant = "%zh_hant:DuplicateDocument%"
                    , es = "%es:DuplicateDocument%"
                    , ar = "%ar:DuplicateDocument%"
                    , fr = "%fr:DuplicateDocument%"
                    , ru = "%ru:DuplicateDocument%"
                    , de = "%de:DuplicateDocument%"
                    , ja = "%ja:DuplicateDocument%"
                    , pes = "%pes:DuplicateDocument%"
                    , it = "%it:DuplicateDocument%"
                    , ro = "%ro:DuplicateDocument%"
                    , hr = "%hr:DuplicateDocument%"
                    , nl = "%nl:DuplicateDocument%"
                    , hu = "%hu:DuplicateDocument%"
                    , sv = "%sv:DuplicateDocument%"
                    , ca = "%ca:DuplicateDocument%"
                    , br = "%br:DuplicateDocument%"
                    , mr = "%mr:DuplicateDocument%"
                    , cs = "%cs:DuplicateDocument%"
                    , fa = "%fa:DuplicateDocument%"
                    , id = "%id:DuplicateDocument%"
                    , ko = "%ko:DuplicateDocument%"
                    , pl = "%pl:DuplicateDocument%"
                    , uk = "%uk:DuplicateDocument%"
                    , nb = "%nb:DuplicateDocument%"
                    }

                DeleteDocument ->
                    { en = "Delete Tree"
                    , zh_hans = "%zh_hans:DeleteDocument%"
                    , zh_hant = "%zh_hant:DeleteDocument%"
                    , es = "%es:DeleteDocument%"
                    , ar = "%ar:DeleteDocument%"
                    , fr = "%fr:DeleteDocument%"
                    , ru = "%ru:DeleteDocument%"
                    , de = "%de:DeleteDocument%"
                    , ja = "%ja:DeleteDocument%"
                    , pes = "%pes:DeleteDocument%"
                    , it = "%it:DeleteDocument%"
                    , ro = "%ro:DeleteDocument%"
                    , hr = "%hr:DeleteDocument%"
                    , nl = "%nl:DeleteDocument%"
                    , hu = "%hu:DeleteDocument%"
                    , sv = "%sv:DeleteDocument%"
                    , ca = "%ca:DeleteDocument%"
                    , br = "%br:DeleteDocument%"
                    , mr = "%mr:DeleteDocument%"
                    , cs = "%cs:DeleteDocument%"
                    , fa = "%fa:DeleteDocument%"
                    , id = "%id:DeleteDocument%"
                    , ko = "%ko:DeleteDocument%"
                    , pl = "%pl:DeleteDocument%"
                    , uk = "%uk:DeleteDocument%"
                    , nb = "%nb:DeleteDocument%"
                    }

                RemoveFromList ->
                    { en = "Remove From List"
                    , zh_hans = "%zh_hans:RemoveFromList%"
                    , zh_hant = "%zh_hant:RemoveFromList%"
                    , es = "%es:RemoveFromList%"
                    , ar = "%ar:RemoveFromList%"
                    , fr = "%fr:RemoveFromList%"
                    , ru = "%ru:RemoveFromList%"
                    , de = "%de:RemoveFromList%"
                    , ja = "%ja:RemoveFromList%"
                    , pes = "%pes:RemoveFromList%"
                    , it = "%it:RemoveFromList%"
                    , ro = "%ro:RemoveFromList%"
                    , hr = "%hr:RemoveFromList%"
                    , nl = "%nl:RemoveFromList%"
                    , hu = "%hu:RemoveFromList%"
                    , sv = "%sv:RemoveFromList%"
                    , ca = "%ca:RemoveFromList%"
                    , br = "%br:RemoveFromList%"
                    , mr = "%mr:RemoveFromList%"
                    , cs = "%cs:RemoveFromList%"
                    , fa = "%fa:RemoveFromList%"
                    , id = "%id:RemoveFromList%"
                    , ko = "%ko:RemoveFromList%"
                    , pl = "%pl:RemoveFromList%"
                    , uk = "%uk:RemoveFromList%"
                    , nb = "%nb:RemoveFromList%"
                    }

                NeverSaved ->
                    { en = "New Document..."
                    , zh_hans = "%zh_hans:NeverSaved%"
                    , zh_hant = "%zh_hant:NeverSaved%"
                    , es = "%es:NeverSaved%"
                    , ar = "%ar:NeverSaved%"
                    , fr = "%fr:NeverSaved%"
                    , ru = "%ru:NeverSaved%"
                    , de = "%de:NeverSaved%"
                    , ja = "%ja:NeverSaved%"
                    , pes = "%pes:NeverSaved%"
                    , it = "%it:NeverSaved%"
                    , ro = "%ro:NeverSaved%"
                    , hr = "%hr:NeverSaved%"
                    , nl = "%nl:NeverSaved%"
                    , hu = "%hu:NeverSaved%"
                    , sv = "%sv:NeverSaved%"
                    , ca = "%ca:NeverSaved%"
                    , br = "%br:NeverSaved%"
                    , mr = "%mr:NeverSaved%"
                    , cs = "%cs:NeverSaved%"
                    , fa = "%fa:NeverSaved%"
                    , id = "%id:NeverSaved%"
                    , ko = "%ko:NeverSaved%"
                    , pl = "%pl:NeverSaved%"
                    , uk = "%uk:NeverSaved%"
                    , nb = "%nb:NeverSaved%"
                    }

                Loading ->
                    { en = "Loading..."
                    , zh_hans = "%zh_hans:Loading%"
                    , zh_hant = "%zh_hant:Loading%"
                    , es = "%es:Loading%"
                    , ar = "%ar:Loading%"
                    , fr = "%fr:Loading%"
                    , ru = "%ru:Loading%"
                    , de = "%de:Loading%"
                    , ja = "%ja:Loading%"
                    , pes = "%pes:Loading%"
                    , it = "%it:Loading%"
                    , ro = "%ro:Loading%"
                    , hr = "%hr:Loading%"
                    , nl = "%nl:Loading%"
                    , hu = "%hu:Loading%"
                    , sv = "%sv:Loading%"
                    , ca = "%ca:Loading%"
                    , br = "%br:Loading%"
                    , mr = "%mr:Loading%"
                    , cs = "%cs:Loading%"
                    , fa = "%fa:Loading%"
                    , id = "%id:Loading%"
                    , ko = "%ko:Loading%"
                    , pl = "%pl:Loading%"
                    , uk = "%uk:Loading%"
                    , nb = "%nb:Loading%"
                    }

                UnsavedChanges ->
                    { en = "Unsaved Changes..."
                    , zh_hans = "%zh_hans:UnsavedChanges%"
                    , zh_hant = "%zh_hant:UnsavedChanges%"
                    , es = "%es:UnsavedChanges%"
                    , ar = "%ar:UnsavedChanges%"
                    , fr = "%fr:UnsavedChanges%"
                    , ru = "%ru:UnsavedChanges%"
                    , de = "%de:UnsavedChanges%"
                    , ja = "%ja:UnsavedChanges%"
                    , pes = "%pes:UnsavedChanges%"
                    , it = "%it:UnsavedChanges%"
                    , ro = "%ro:UnsavedChanges%"
                    , hr = "%hr:UnsavedChanges%"
                    , nl = "%nl:UnsavedChanges%"
                    , hu = "%hu:UnsavedChanges%"
                    , sv = "%sv:UnsavedChanges%"
                    , ca = "%ca:UnsavedChanges%"
                    , br = "%br:UnsavedChanges%"
                    , mr = "%mr:UnsavedChanges%"
                    , cs = "%cs:UnsavedChanges%"
                    , fa = "%fa:UnsavedChanges%"
                    , id = "%id:UnsavedChanges%"
                    , ko = "%ko:UnsavedChanges%"
                    , pl = "%pl:UnsavedChanges%"
                    , uk = "%uk:UnsavedChanges%"
                    , nb = "%nb:UnsavedChanges%"
                    }

                SavedInternally ->
                    { en = "Saved Offline"
                    , zh_hans = "%zh_hans:SavedInternally%"
                    , zh_hant = "%zh_hant:SavedInternally%"
                    , es = "%es:SavedInternally%"
                    , ar = "%ar:SavedInternally%"
                    , fr = "%fr:SavedInternally%"
                    , ru = "%ru:SavedInternally%"
                    , de = "%de:SavedInternally%"
                    , ja = "%ja:SavedInternally%"
                    , pes = "%pes:SavedInternally%"
                    , it = "%it:SavedInternally%"
                    , ro = "%ro:SavedInternally%"
                    , hr = "%hr:SavedInternally%"
                    , nl = "%nl:SavedInternally%"
                    , hu = "%hu:SavedInternally%"
                    , sv = "%sv:SavedInternally%"
                    , ca = "%ca:SavedInternally%"
                    , br = "%br:SavedInternally%"
                    , mr = "%mr:SavedInternally%"
                    , cs = "%cs:SavedInternally%"
                    , fa = "%fa:SavedInternally%"
                    , id = "%id:SavedInternally%"
                    , ko = "%ko:SavedInternally%"
                    , pl = "%pl:SavedInternally%"
                    , uk = "%uk:SavedInternally%"
                    , nb = "%nb:SavedInternally%"
                    }

                ChangesSaved ->
                    { en = "Saved"
                    , zh_hans = "%zh_hans:ChangesSaved%"
                    , zh_hant = "%zh_hant:ChangesSaved%"
                    , es = "%es:ChangesSaved%"
                    , ar = "%ar:ChangesSaved%"
                    , fr = "%fr:ChangesSaved%"
                    , ru = "%ru:ChangesSaved%"
                    , de = "%de:ChangesSaved%"
                    , ja = "%ja:ChangesSaved%"
                    , pes = "%pes:ChangesSaved%"
                    , it = "%it:ChangesSaved%"
                    , ro = "%ro:ChangesSaved%"
                    , hr = "%hr:ChangesSaved%"
                    , nl = "%nl:ChangesSaved%"
                    , hu = "%hu:ChangesSaved%"
                    , sv = "%sv:ChangesSaved%"
                    , ca = "%ca:ChangesSaved%"
                    , br = "%br:ChangesSaved%"
                    , mr = "%mr:ChangesSaved%"
                    , cs = "%cs:ChangesSaved%"
                    , fa = "%fa:ChangesSaved%"
                    , id = "%id:ChangesSaved%"
                    , ko = "%ko:ChangesSaved%"
                    , pl = "%pl:ChangesSaved%"
                    , uk = "%uk:ChangesSaved%"
                    , nb = "%nb:ChangesSaved%"
                    }

                ChangesSynced ->
                    { en = "Synced"
                    , zh_hans = "%zh_hans:ChangesSynced%"
                    , zh_hant = "%zh_hant:ChangesSynced%"
                    , es = "%es:ChangesSynced%"
                    , ar = "%ar:ChangesSynced%"
                    , fr = "%fr:ChangesSynced%"
                    , ru = "%ru:ChangesSynced%"
                    , de = "%de:ChangesSynced%"
                    , ja = "%ja:ChangesSynced%"
                    , pes = "%pes:ChangesSynced%"
                    , it = "%it:ChangesSynced%"
                    , ro = "%ro:ChangesSynced%"
                    , hr = "%hr:ChangesSynced%"
                    , nl = "%nl:ChangesSynced%"
                    , hu = "%hu:ChangesSynced%"
                    , sv = "%sv:ChangesSynced%"
                    , ca = "%ca:ChangesSynced%"
                    , br = "%br:ChangesSynced%"
                    , mr = "%mr:ChangesSynced%"
                    , cs = "%cs:ChangesSynced%"
                    , fa = "%fa:ChangesSynced%"
                    , id = "%id:ChangesSynced%"
                    , ko = "%ko:ChangesSynced%"
                    , pl = "%pl:ChangesSynced%"
                    , uk = "%uk:ChangesSynced%"
                    , nb = "%nb:ChangesSynced%"
                    }

                DatabaseError ->
                    { en = "Database Error..."
                    , zh_hans = "%zh_hans:DatabaseError%"
                    , zh_hant = "%zh_hant:DatabaseError%"
                    , es = "%es:DatabaseError%"
                    , ar = "%ar:DatabaseError%"
                    , fr = "%fr:DatabaseError%"
                    , ru = "%ru:DatabaseError%"
                    , de = "%de:DatabaseError%"
                    , ja = "%ja:DatabaseError%"
                    , pes = "%pes:DatabaseError%"
                    , it = "%it:DatabaseError%"
                    , ro = "%ro:DatabaseError%"
                    , hr = "%hr:DatabaseError%"
                    , nl = "%nl:DatabaseError%"
                    , hu = "%hu:DatabaseError%"
                    , sv = "%sv:DatabaseError%"
                    , ca = "%ca:DatabaseError%"
                    , br = "%br:DatabaseError%"
                    , mr = "%mr:DatabaseError%"
                    , cs = "%cs:DatabaseError%"
                    , fa = "%fa:DatabaseError%"
                    , id = "%id:DatabaseError%"
                    , ko = "%ko:DatabaseError%"
                    , pl = "%pl:DatabaseError%"
                    , uk = "%uk:DatabaseError%"
                    , nb = "%nb:DatabaseError%"
                    }

                LastSaved ->
                    { en = "Last saved"
                    , zh_hans = "%zh_hans:LastSaved%"
                    , zh_hant = "%zh_hant:LastSaved%"
                    , es = "%es:LastSaved%"
                    , ar = "%ar:LastSaved%"
                    , fr = "%fr:LastSaved%"
                    , ru = "%ru:LastSaved%"
                    , de = "%de:LastSaved%"
                    , ja = "%ja:LastSaved%"
                    , pes = "%pes:LastSaved%"
                    , it = "%it:LastSaved%"
                    , ro = "%ro:LastSaved%"
                    , hr = "%hr:LastSaved%"
                    , nl = "%nl:LastSaved%"
                    , hu = "%hu:LastSaved%"
                    , sv = "%sv:LastSaved%"
                    , ca = "%ca:LastSaved%"
                    , br = "%br:LastSaved%"
                    , mr = "%mr:LastSaved%"
                    , cs = "%cs:LastSaved%"
                    , fa = "%fa:LastSaved%"
                    , id = "%id:LastSaved%"
                    , ko = "%ko:LastSaved%"
                    , pl = "%pl:LastSaved%"
                    , uk = "%uk:LastSaved%"
                    , nb = "%nb:LastSaved%"
                    }

                LastSynced ->
                    { en = "Last synced"
                    , zh_hans = "%zh_hans:LastSynced%"
                    , zh_hant = "%zh_hant:LastSynced%"
                    , es = "%es:LastSynced%"
                    , ar = "%ar:LastSynced%"
                    , fr = "%fr:LastSynced%"
                    , ru = "%ru:LastSynced%"
                    , de = "%de:LastSynced%"
                    , ja = "%ja:LastSynced%"
                    , pes = "%pes:LastSynced%"
                    , it = "%it:LastSynced%"
                    , ro = "%ro:LastSynced%"
                    , hr = "%hr:LastSynced%"
                    , nl = "%nl:LastSynced%"
                    , hu = "%hu:LastSynced%"
                    , sv = "%sv:LastSynced%"
                    , ca = "%ca:LastSynced%"
                    , br = "%br:LastSynced%"
                    , mr = "%mr:LastSynced%"
                    , cs = "%cs:LastSynced%"
                    , fa = "%fa:LastSynced%"
                    , id = "%id:LastSynced%"
                    , ko = "%ko:LastSynced%"
                    , pl = "%pl:LastSynced%"
                    , uk = "%uk:LastSynced%"
                    , nb = "%nb:LastSynced%"
                    }

                LastEdit ->
                    { en = "Last edit"
                    , zh_hans = "%zh_hans:LastEdit%"
                    , zh_hant = "%zh_hant:LastEdit%"
                    , es = "%es:LastEdit%"
                    , ar = "%ar:LastEdit%"
                    , fr = "%fr:LastEdit%"
                    , ru = "%ru:LastEdit%"
                    , de = "%de:LastEdit%"
                    , ja = "%ja:LastEdit%"
                    , pes = "%pes:LastEdit%"
                    , it = "%it:LastEdit%"
                    , ro = "%ro:LastEdit%"
                    , hr = "%hr:LastEdit%"
                    , nl = "%nl:LastEdit%"
                    , hu = "%hu:LastEdit%"
                    , sv = "%sv:LastEdit%"
                    , ca = "%ca:LastEdit%"
                    , br = "%br:LastEdit%"
                    , mr = "%mr:LastEdit%"
                    , cs = "%cs:LastEdit%"
                    , fa = "%fa:LastEdit%"
                    , id = "%id:LastEdit%"
                    , ko = "%ko:LastEdit%"
                    , pl = "%pl:LastEdit%"
                    , uk = "%uk:LastEdit%"
                    , nb = "%nb:LastEdit%"
                    }

                ConfirmBannerStrong ->
                    { en = "Please confirm your email."
                    , zh_hans = "%zh_hans:ConfirmBannerStrong%"
                    , zh_hant = "%zh_hant:ConfirmBannerStrong%"
                    , es = "%es:ConfirmBannerStrong%"
                    , ar = "%ar:ConfirmBannerStrong%"
                    , fr = "%fr:ConfirmBannerStrong%"
                    , ru = "%ru:ConfirmBannerStrong%"
                    , de = "%de:ConfirmBannerStrong%"
                    , ja = "%ja:ConfirmBannerStrong%"
                    , pes = "%pes:ConfirmBannerStrong%"
                    , it = "%it:ConfirmBannerStrong%"
                    , ro = "%ro:ConfirmBannerStrong%"
                    , hr = "%hr:ConfirmBannerStrong%"
                    , nl = "%nl:ConfirmBannerStrong%"
                    , hu = "%hu:ConfirmBannerStrong%"
                    , sv = "%sv:ConfirmBannerStrong%"
                    , ca = "%ca:ConfirmBannerStrong%"
                    , br = "%br:ConfirmBannerStrong%"
                    , mr = "%mr:ConfirmBannerStrong%"
                    , cs = "%cs:ConfirmBannerStrong%"
                    , fa = "%fa:ConfirmBannerStrong%"
                    , id = "%id:ConfirmBannerStrong%"
                    , ko = "%ko:ConfirmBannerStrong%"
                    , pl = "%pl:ConfirmBannerStrong%"
                    , uk = "%uk:ConfirmBannerStrong%"
                    , nb = "%nb:ConfirmBannerStrong%"
                    }

                ConfirmBannerBody ->
                    { en = "We've sent instructions to "
                    , zh_hans = "%zh_hans:ConfirmBannerBody%"
                    , zh_hant = "%zh_hant:ConfirmBannerBody%"
                    , es = "%es:ConfirmBannerBody%"
                    , ar = "%ar:ConfirmBannerBody%"
                    , fr = "%fr:ConfirmBannerBody%"
                    , ru = "%ru:ConfirmBannerBody%"
                    , de = "%de:ConfirmBannerBody%"
                    , ja = "%ja:ConfirmBannerBody%"
                    , pes = "%pes:ConfirmBannerBody%"
                    , it = "%it:ConfirmBannerBody%"
                    , ro = "%ro:ConfirmBannerBody%"
                    , hr = "%hr:ConfirmBannerBody%"
                    , nl = "%nl:ConfirmBannerBody%"
                    , hu = "%hu:ConfirmBannerBody%"
                    , sv = "%sv:ConfirmBannerBody%"
                    , ca = "%ca:ConfirmBannerBody%"
                    , br = "%br:ConfirmBannerBody%"
                    , mr = "%mr:ConfirmBannerBody%"
                    , cs = "%cs:ConfirmBannerBody%"
                    , fa = "%fa:ConfirmBannerBody%"
                    , id = "%id:ConfirmBannerBody%"
                    , ko = "%ko:ConfirmBannerBody%"
                    , pl = "%pl:ConfirmBannerBody%"
                    , uk = "%uk:ConfirmBannerBody%"
                    , nb = "%nb:ConfirmBannerBody%"
                    }

                Help ->
                    { en = "Help"
                    , zh_hans = "%zh_hans:Help%"
                    , zh_hant = "%zh_hant:Help%"
                    , es = "%es:Help%"
                    , ar = "%ar:Help%"
                    , fr = "%fr:Help%"
                    , ru = "%ru:Help%"
                    , de = "%de:Help%"
                    , ja = "%ja:Help%"
                    , pes = "%pes:Help%"
                    , it = "%it:Help%"
                    , ro = "%ro:Help%"
                    , hr = "%hr:Help%"
                    , nl = "%nl:Help%"
                    , hu = "%hu:Help%"
                    , sv = "%sv:Help%"
                    , ca = "%ca:Help%"
                    , br = "%br:Help%"
                    , mr = "%mr:Help%"
                    , cs = "%cs:Help%"
                    , fa = "%fa:Help%"
                    , id = "%id:Help%"
                    , ko = "%ko:Help%"
                    , pl = "%pl:Help%"
                    , uk = "%uk:Help%"
                    , nb = "%nb:Help%"
                    }

                WhatsNew ->
                    { en = "What's New"
                    , zh_hans = "%zh_hans:WhatsNew%"
                    , zh_hant = "%zh_hant:WhatsNew%"
                    , es = "%es:WhatsNew%"
                    , ar = "%ar:WhatsNew%"
                    , fr = "%fr:WhatsNew%"
                    , ru = "%ru:WhatsNew%"
                    , de = "%de:WhatsNew%"
                    , ja = "%ja:WhatsNew%"
                    , pes = "%pes:WhatsNew%"
                    , it = "%it:WhatsNew%"
                    , ro = "%ro:WhatsNew%"
                    , hr = "%hr:WhatsNew%"
                    , nl = "%nl:WhatsNew%"
                    , hu = "%hu:WhatsNew%"
                    , sv = "%sv:WhatsNew%"
                    , ca = "%ca:WhatsNew%"
                    , br = "%br:WhatsNew%"
                    , mr = "%mr:WhatsNew%"
                    , cs = "%cs:WhatsNew%"
                    , fa = "%fa:WhatsNew%"
                    , id = "%id:WhatsNew%"
                    , ko = "%ko:WhatsNew%"
                    , pl = "%pl:WhatsNew%"
                    , uk = "%uk:WhatsNew%"
                    , nb = "%nb:WhatsNew%"
                    }

                AccountTooltip ->
                    { en = "Account"
                    , zh_hans = "%zh_hans:AccountTooltip%"
                    , zh_hant = "%zh_hant:AccountTooltip%"
                    , es = "%es:AccountTooltip%"
                    , ar = "%ar:AccountTooltip%"
                    , fr = "%fr:AccountTooltip%"
                    , ru = "%ru:AccountTooltip%"
                    , de = "%de:AccountTooltip%"
                    , ja = "%ja:AccountTooltip%"
                    , pes = "%pes:AccountTooltip%"
                    , it = "%it:AccountTooltip%"
                    , ro = "%ro:AccountTooltip%"
                    , hr = "%hr:AccountTooltip%"
                    , nl = "%nl:AccountTooltip%"
                    , hu = "%hu:AccountTooltip%"
                    , sv = "%sv:AccountTooltip%"
                    , ca = "%ca:AccountTooltip%"
                    , br = "%br:AccountTooltip%"
                    , mr = "%mr:AccountTooltip%"
                    , cs = "%cs:AccountTooltip%"
                    , fa = "%fa:AccountTooltip%"
                    , id = "%id:AccountTooltip%"
                    , ko = "%ko:AccountTooltip%"
                    , pl = "%pl:AccountTooltip%"
                    , uk = "%uk:AccountTooltip%"
                    , nb = "%nb:AccountTooltip%"
                    }

                -- Keyboard Shortcut Help
                KeyboardShortcuts ->
                    { en = "Keyboard Shortcuts"
                    , zh_hans = "%zh_hans:KeyboardShortcuts%"
                    , zh_hant = "%zh_hant:KeyboardShortcuts%"
                    , es = "%es:KeyboardShortcuts%"
                    , ar = "%ar:KeyboardShortcuts%"
                    , fr = "%fr:KeyboardShortcuts%"
                    , ru = "%ru:KeyboardShortcuts%"
                    , de = "%de:KeyboardShortcuts%"
                    , ja = "%ja:KeyboardShortcuts%"
                    , pes = "%pes:KeyboardShortcuts%"
                    , it = "%it:KeyboardShortcuts%"
                    , ro = "%ro:KeyboardShortcuts%"
                    , hr = "%hr:KeyboardShortcuts%"
                    , nl = "%nl:KeyboardShortcuts%"
                    , hu = "%hu:KeyboardShortcuts%"
                    , sv = "%sv:KeyboardShortcuts%"
                    , ca = "%ca:KeyboardShortcuts%"
                    , br = "%br:KeyboardShortcuts%"
                    , mr = "%mr:KeyboardShortcuts%"
                    , cs = "%cs:KeyboardShortcuts%"
                    , fa = "%fa:KeyboardShortcuts%"
                    , id = "%id:KeyboardShortcuts%"
                    , ko = "%ko:KeyboardShortcuts%"
                    , pl = "%pl:KeyboardShortcuts%"
                    , uk = "%uk:KeyboardShortcuts%"
                    , nb = "%nb:KeyboardShortcuts%"
                    }

                ViewModeShortcuts ->
                    { en = "View Mode Shortcuts : "
                    , zh_hans = "%zh_hans:ViewModeShortcuts%"
                    , zh_hant = "%zh_hant:ViewModeShortcuts%"
                    , es = "%es:ViewModeShortcuts%"
                    , ar = "%ar:ViewModeShortcuts%"
                    , fr = "%fr:ViewModeShortcuts%"
                    , ru = "%ru:ViewModeShortcuts%"
                    , de = "%de:ViewModeShortcuts%"
                    , ja = "%ja:ViewModeShortcuts%"
                    , pes = "%pes:ViewModeShortcuts%"
                    , it = "%it:ViewModeShortcuts%"
                    , ro = "%ro:ViewModeShortcuts%"
                    , hr = "%hr:ViewModeShortcuts%"
                    , nl = "%nl:ViewModeShortcuts%"
                    , hu = "%hu:ViewModeShortcuts%"
                    , sv = "%sv:ViewModeShortcuts%"
                    , ca = "%ca:ViewModeShortcuts%"
                    , br = "%br:ViewModeShortcuts%"
                    , mr = "%mr:ViewModeShortcuts%"
                    , cs = "%cs:ViewModeShortcuts%"
                    , fa = "%fa:ViewModeShortcuts%"
                    , id = "%id:ViewModeShortcuts%"
                    , ko = "%ko:ViewModeShortcuts%"
                    , pl = "%pl:ViewModeShortcuts%"
                    , uk = "%uk:ViewModeShortcuts%"
                    , nb = "%nb:ViewModeShortcuts%"
                    }

                CardEditCreateDelete ->
                    { en = "Card Edit, Create, Delete"
                    , zh_hans = "%zh_hans:CardEditCreateDelete%"
                    , zh_hant = "%zh_hant:CardEditCreateDelete%"
                    , es = "%es:CardEditCreateDelete%"
                    , ar = "%ar:CardEditCreateDelete%"
                    , fr = "%fr:CardEditCreateDelete%"
                    , ru = "%ru:CardEditCreateDelete%"
                    , de = "%de:CardEditCreateDelete%"
                    , ja = "%ja:CardEditCreateDelete%"
                    , pes = "%pes:CardEditCreateDelete%"
                    , it = "%it:CardEditCreateDelete%"
                    , ro = "%ro:CardEditCreateDelete%"
                    , hr = "%hr:CardEditCreateDelete%"
                    , nl = "%nl:CardEditCreateDelete%"
                    , hu = "%hu:CardEditCreateDelete%"
                    , sv = "%sv:CardEditCreateDelete%"
                    , ca = "%ca:CardEditCreateDelete%"
                    , br = "%br:CardEditCreateDelete%"
                    , mr = "%mr:CardEditCreateDelete%"
                    , cs = "%cs:CardEditCreateDelete%"
                    , fa = "%fa:CardEditCreateDelete%"
                    , id = "%id:CardEditCreateDelete%"
                    , ko = "%ko:CardEditCreateDelete%"
                    , pl = "%pl:CardEditCreateDelete%"
                    , uk = "%uk:CardEditCreateDelete%"
                    , nb = "%nb:CardEditCreateDelete%"
                    }

                NavigationMovingCards ->
                    { en = "Navigation, Moving Cards"
                    , zh_hans = "%zh_hans:NavigationMovingCards%"
                    , zh_hant = "%zh_hant:NavigationMovingCards%"
                    , es = "%es:NavigationMovingCards%"
                    , ar = "%ar:NavigationMovingCards%"
                    , fr = "%fr:NavigationMovingCards%"
                    , ru = "%ru:NavigationMovingCards%"
                    , de = "%de:NavigationMovingCards%"
                    , ja = "%ja:NavigationMovingCards%"
                    , pes = "%pes:NavigationMovingCards%"
                    , it = "%it:NavigationMovingCards%"
                    , ro = "%ro:NavigationMovingCards%"
                    , hr = "%hr:NavigationMovingCards%"
                    , nl = "%nl:NavigationMovingCards%"
                    , hu = "%hu:NavigationMovingCards%"
                    , sv = "%sv:NavigationMovingCards%"
                    , ca = "%ca:NavigationMovingCards%"
                    , br = "%br:NavigationMovingCards%"
                    , mr = "%mr:NavigationMovingCards%"
                    , cs = "%cs:NavigationMovingCards%"
                    , fa = "%fa:NavigationMovingCards%"
                    , id = "%id:NavigationMovingCards%"
                    , ko = "%ko:NavigationMovingCards%"
                    , pl = "%pl:NavigationMovingCards%"
                    , uk = "%uk:NavigationMovingCards%"
                    , nb = "%nb:NavigationMovingCards%"
                    }

                CopyPaste ->
                    { en = "Copy/Paste"
                    , zh_hans = "%zh_hans:CopyPaste%"
                    , zh_hant = "%zh_hant:CopyPaste%"
                    , es = "%es:CopyPaste%"
                    , ar = "%ar:CopyPaste%"
                    , fr = "%fr:CopyPaste%"
                    , ru = "%ru:CopyPaste%"
                    , de = "%de:CopyPaste%"
                    , ja = "%ja:CopyPaste%"
                    , pes = "%pes:CopyPaste%"
                    , it = "%it:CopyPaste%"
                    , ro = "%ro:CopyPaste%"
                    , hr = "%hr:CopyPaste%"
                    , nl = "%nl:CopyPaste%"
                    , hu = "%hu:CopyPaste%"
                    , sv = "%sv:CopyPaste%"
                    , ca = "%ca:CopyPaste%"
                    , br = "%br:CopyPaste%"
                    , mr = "%mr:CopyPaste%"
                    , cs = "%cs:CopyPaste%"
                    , fa = "%fa:CopyPaste%"
                    , id = "%id:CopyPaste%"
                    , ko = "%ko:CopyPaste%"
                    , pl = "%pl:CopyPaste%"
                    , uk = "%uk:CopyPaste%"
                    , nb = "%nb:CopyPaste%"
                    }

                SearchingMerging ->
                    { en = "Searching, Merging Cards"
                    , zh_hans = "%zh_hans:SearchingMerging%"
                    , zh_hant = "%zh_hant:SearchingMerging%"
                    , es = "%es:SearchingMerging%"
                    , ar = "%ar:SearchingMerging%"
                    , fr = "%fr:SearchingMerging%"
                    , ru = "%ru:SearchingMerging%"
                    , de = "%de:SearchingMerging%"
                    , ja = "%ja:SearchingMerging%"
                    , pes = "%pes:SearchingMerging%"
                    , it = "%it:SearchingMerging%"
                    , ro = "%ro:SearchingMerging%"
                    , hr = "%hr:SearchingMerging%"
                    , nl = "%nl:SearchingMerging%"
                    , hu = "%hu:SearchingMerging%"
                    , sv = "%sv:SearchingMerging%"
                    , ca = "%ca:SearchingMerging%"
                    , br = "%br:SearchingMerging%"
                    , mr = "%mr:SearchingMerging%"
                    , cs = "%cs:SearchingMerging%"
                    , fa = "%fa:SearchingMerging%"
                    , id = "%id:SearchingMerging%"
                    , ko = "%ko:SearchingMerging%"
                    , pl = "%pl:SearchingMerging%"
                    , uk = "%uk:SearchingMerging%"
                    , nb = "%nb:SearchingMerging%"
                    }

                HelpInfoDocs ->
                    { en = "Help, Info, Documents"
                    , zh_hans = "%zh_hans:HelpInfoDocs%"
                    , zh_hant = "%zh_hant:HelpInfoDocs%"
                    , es = "%es:HelpInfoDocs%"
                    , ar = "%ar:HelpInfoDocs%"
                    , fr = "%fr:HelpInfoDocs%"
                    , ru = "%ru:HelpInfoDocs%"
                    , de = "%de:HelpInfoDocs%"
                    , ja = "%ja:HelpInfoDocs%"
                    , pes = "%pes:HelpInfoDocs%"
                    , it = "%it:HelpInfoDocs%"
                    , ro = "%ro:HelpInfoDocs%"
                    , hr = "%hr:HelpInfoDocs%"
                    , nl = "%nl:HelpInfoDocs%"
                    , hu = "%hu:HelpInfoDocs%"
                    , sv = "%sv:HelpInfoDocs%"
                    , ca = "%ca:HelpInfoDocs%"
                    , br = "%br:HelpInfoDocs%"
                    , mr = "%mr:HelpInfoDocs%"
                    , cs = "%cs:HelpInfoDocs%"
                    , fa = "%fa:HelpInfoDocs%"
                    , id = "%id:HelpInfoDocs%"
                    , ko = "%ko:HelpInfoDocs%"
                    , pl = "%pl:HelpInfoDocs%"
                    , uk = "%uk:HelpInfoDocs%"
                    , nb = "%nb:HelpInfoDocs%"
                    }

                EditModeShortcuts ->
                    { en = "Edit Mode Shortcuts : "
                    , zh_hans = "%zh_hans:EditModeShortcuts%"
                    , zh_hant = "%zh_hant:EditModeShortcuts%"
                    , es = "%es:EditModeShortcuts%"
                    , ar = "%ar:EditModeShortcuts%"
                    , fr = "%fr:EditModeShortcuts%"
                    , ru = "%ru:EditModeShortcuts%"
                    , de = "%de:EditModeShortcuts%"
                    , ja = "%ja:EditModeShortcuts%"
                    , pes = "%pes:EditModeShortcuts%"
                    , it = "%it:EditModeShortcuts%"
                    , ro = "%ro:EditModeShortcuts%"
                    , hr = "%hr:EditModeShortcuts%"
                    , nl = "%nl:EditModeShortcuts%"
                    , hu = "%hu:EditModeShortcuts%"
                    , sv = "%sv:EditModeShortcuts%"
                    , ca = "%ca:EditModeShortcuts%"
                    , br = "%br:EditModeShortcuts%"
                    , mr = "%mr:EditModeShortcuts%"
                    , cs = "%cs:EditModeShortcuts%"
                    , fa = "%fa:EditModeShortcuts%"
                    , id = "%id:EditModeShortcuts%"
                    , ko = "%ko:EditModeShortcuts%"
                    , pl = "%pl:EditModeShortcuts%"
                    , uk = "%uk:EditModeShortcuts%"
                    , nb = "%nb:EditModeShortcuts%"
                    }

                CardSaveCreate ->
                    { en = "Card Save, Create"
                    , zh_hans = "%zh_hans:CardSaveCreate%"
                    , zh_hant = "%zh_hant:CardSaveCreate%"
                    , es = "%es:CardSaveCreate%"
                    , ar = "%ar:CardSaveCreate%"
                    , fr = "%fr:CardSaveCreate%"
                    , ru = "%ru:CardSaveCreate%"
                    , de = "%de:CardSaveCreate%"
                    , ja = "%ja:CardSaveCreate%"
                    , pes = "%pes:CardSaveCreate%"
                    , it = "%it:CardSaveCreate%"
                    , ro = "%ro:CardSaveCreate%"
                    , hr = "%hr:CardSaveCreate%"
                    , nl = "%nl:CardSaveCreate%"
                    , hu = "%hu:CardSaveCreate%"
                    , sv = "%sv:CardSaveCreate%"
                    , ca = "%ca:CardSaveCreate%"
                    , br = "%br:CardSaveCreate%"
                    , mr = "%mr:CardSaveCreate%"
                    , cs = "%cs:CardSaveCreate%"
                    , fa = "%fa:CardSaveCreate%"
                    , id = "%id:CardSaveCreate%"
                    , ko = "%ko:CardSaveCreate%"
                    , pl = "%pl:CardSaveCreate%"
                    , uk = "%uk:CardSaveCreate%"
                    , nb = "%nb:CardSaveCreate%"
                    }

                EditCard ->
                    { en = "Edit card"
                    , zh_hans = "%zh_hans:EditCard%"
                    , zh_hant = "%zh_hant:EditCard%"
                    , es = "%es:EditCard%"
                    , ar = "%ar:EditCard%"
                    , fr = "%fr:EditCard%"
                    , ru = "%ru:EditCard%"
                    , de = "%de:EditCard%"
                    , ja = "%ja:EditCard%"
                    , pes = "%pes:EditCard%"
                    , it = "%it:EditCard%"
                    , ro = "%ro:EditCard%"
                    , hr = "%hr:EditCard%"
                    , nl = "%nl:EditCard%"
                    , hu = "%hu:EditCard%"
                    , sv = "%sv:EditCard%"
                    , ca = "%ca:EditCard%"
                    , br = "%br:EditCard%"
                    , mr = "%mr:EditCard%"
                    , cs = "%cs:EditCard%"
                    , fa = "%fa:EditCard%"
                    , id = "%id:EditCard%"
                    , ko = "%ko:EditCard%"
                    , pl = "%pl:EditCard%"
                    , uk = "%uk:EditCard%"
                    , nb = "%nb:EditCard%"
                    }

                AddCardBelow ->
                    { en = "Add card below"
                    , zh_hans = "%zh_hans:AddCardBelow%"
                    , zh_hant = "%zh_hant:AddCardBelow%"
                    , es = "%es:AddCardBelow%"
                    , ar = "%ar:AddCardBelow%"
                    , fr = "%fr:AddCardBelow%"
                    , ru = "%ru:AddCardBelow%"
                    , de = "%de:AddCardBelow%"
                    , ja = "%ja:AddCardBelow%"
                    , pes = "%pes:AddCardBelow%"
                    , it = "%it:AddCardBelow%"
                    , ro = "%ro:AddCardBelow%"
                    , hr = "%hr:AddCardBelow%"
                    , nl = "%nl:AddCardBelow%"
                    , hu = "%hu:AddCardBelow%"
                    , sv = "%sv:AddCardBelow%"
                    , ca = "%ca:AddCardBelow%"
                    , br = "%br:AddCardBelow%"
                    , mr = "%mr:AddCardBelow%"
                    , cs = "%cs:AddCardBelow%"
                    , fa = "%fa:AddCardBelow%"
                    , id = "%id:AddCardBelow%"
                    , ko = "%ko:AddCardBelow%"
                    , pl = "%pl:AddCardBelow%"
                    , uk = "%uk:AddCardBelow%"
                    , nb = "%nb:AddCardBelow%"
                    }

                AddCardAbove ->
                    { en = "Add card above"
                    , zh_hans = "%zh_hans:AddCardAbove%"
                    , zh_hant = "%zh_hant:AddCardAbove%"
                    , es = "%es:AddCardAbove%"
                    , ar = "%ar:AddCardAbove%"
                    , fr = "%fr:AddCardAbove%"
                    , ru = "%ru:AddCardAbove%"
                    , de = "%de:AddCardAbove%"
                    , ja = "%ja:AddCardAbove%"
                    , pes = "%pes:AddCardAbove%"
                    , it = "%it:AddCardAbove%"
                    , ro = "%ro:AddCardAbove%"
                    , hr = "%hr:AddCardAbove%"
                    , nl = "%nl:AddCardAbove%"
                    , hu = "%hu:AddCardAbove%"
                    , sv = "%sv:AddCardAbove%"
                    , ca = "%ca:AddCardAbove%"
                    , br = "%br:AddCardAbove%"
                    , mr = "%mr:AddCardAbove%"
                    , cs = "%cs:AddCardAbove%"
                    , fa = "%fa:AddCardAbove%"
                    , id = "%id:AddCardAbove%"
                    , ko = "%ko:AddCardAbove%"
                    , pl = "%pl:AddCardAbove%"
                    , uk = "%uk:AddCardAbove%"
                    , nb = "%nb:AddCardAbove%"
                    }

                AddCardToRight ->
                    { en = "Add card to the right (as child)"
                    , zh_hans = "%zh_hans:AddCardToRight%"
                    , zh_hant = "%zh_hant:AddCardToRight%"
                    , es = "%es:AddCardToRight%"
                    , ar = "%ar:AddCardToRight%"
                    , fr = "%fr:AddCardToRight%"
                    , ru = "%ru:AddCardToRight%"
                    , de = "%de:AddCardToRight%"
                    , ja = "%ja:AddCardToRight%"
                    , pes = "%pes:AddCardToRight%"
                    , it = "%it:AddCardToRight%"
                    , ro = "%ro:AddCardToRight%"
                    , hr = "%hr:AddCardToRight%"
                    , nl = "%nl:AddCardToRight%"
                    , hu = "%hu:AddCardToRight%"
                    , sv = "%sv:AddCardToRight%"
                    , ca = "%ca:AddCardToRight%"
                    , br = "%br:AddCardToRight%"
                    , mr = "%mr:AddCardToRight%"
                    , cs = "%cs:AddCardToRight%"
                    , fa = "%fa:AddCardToRight%"
                    , id = "%id:AddCardToRight%"
                    , ko = "%ko:AddCardToRight%"
                    , pl = "%pl:AddCardToRight%"
                    , uk = "%uk:AddCardToRight%"
                    , nb = "%nb:AddCardToRight%"
                    }

                AddCardBelowSplit ->
                    { en = "Add card below (split at cursor)"
                    , zh_hans = "%zh_hans:AddCardBelowSplit%"
                    , zh_hant = "%zh_hant:AddCardBelowSplit%"
                    , es = "%es:AddCardBelowSplit%"
                    , ar = "%ar:AddCardBelowSplit%"
                    , fr = "%fr:AddCardBelowSplit%"
                    , ru = "%ru:AddCardBelowSplit%"
                    , de = "%de:AddCardBelowSplit%"
                    , ja = "%ja:AddCardBelowSplit%"
                    , pes = "%pes:AddCardBelowSplit%"
                    , it = "%it:AddCardBelowSplit%"
                    , ro = "%ro:AddCardBelowSplit%"
                    , hr = "%hr:AddCardBelowSplit%"
                    , nl = "%nl:AddCardBelowSplit%"
                    , hu = "%hu:AddCardBelowSplit%"
                    , sv = "%sv:AddCardBelowSplit%"
                    , ca = "%ca:AddCardBelowSplit%"
                    , br = "%br:AddCardBelowSplit%"
                    , mr = "%mr:AddCardBelowSplit%"
                    , cs = "%cs:AddCardBelowSplit%"
                    , fa = "%fa:AddCardBelowSplit%"
                    , id = "%id:AddCardBelowSplit%"
                    , ko = "%ko:AddCardBelowSplit%"
                    , pl = "%pl:AddCardBelowSplit%"
                    , uk = "%uk:AddCardBelowSplit%"
                    , nb = "%nb:AddCardBelowSplit%"
                    }

                AddCardAboveSplit ->
                    { en = "Add card above (split at cursor)"
                    , zh_hans = "%zh_hans:AddCardAboveSplit%"
                    , zh_hant = "%zh_hant:AddCardAboveSplit%"
                    , es = "%es:AddCardAboveSplit%"
                    , ar = "%ar:AddCardAboveSplit%"
                    , fr = "%fr:AddCardAboveSplit%"
                    , ru = "%ru:AddCardAboveSplit%"
                    , de = "%de:AddCardAboveSplit%"
                    , ja = "%ja:AddCardAboveSplit%"
                    , pes = "%pes:AddCardAboveSplit%"
                    , it = "%it:AddCardAboveSplit%"
                    , ro = "%ro:AddCardAboveSplit%"
                    , hr = "%hr:AddCardAboveSplit%"
                    , nl = "%nl:AddCardAboveSplit%"
                    , hu = "%hu:AddCardAboveSplit%"
                    , sv = "%sv:AddCardAboveSplit%"
                    , ca = "%ca:AddCardAboveSplit%"
                    , br = "%br:AddCardAboveSplit%"
                    , mr = "%mr:AddCardAboveSplit%"
                    , cs = "%cs:AddCardAboveSplit%"
                    , fa = "%fa:AddCardAboveSplit%"
                    , id = "%id:AddCardAboveSplit%"
                    , ko = "%ko:AddCardAboveSplit%"
                    , pl = "%pl:AddCardAboveSplit%"
                    , uk = "%uk:AddCardAboveSplit%"
                    , nb = "%nb:AddCardAboveSplit%"
                    }

                AddCardToRightSplit ->
                    { en = "Add card to the right (split at cursor)"
                    , zh_hans = "%zh_hans:AddCardToRightSplit%"
                    , zh_hant = "%zh_hant:AddCardToRightSplit%"
                    , es = "%es:AddCardToRightSplit%"
                    , ar = "%ar:AddCardToRightSplit%"
                    , fr = "%fr:AddCardToRightSplit%"
                    , ru = "%ru:AddCardToRightSplit%"
                    , de = "%de:AddCardToRightSplit%"
                    , ja = "%ja:AddCardToRightSplit%"
                    , pes = "%pes:AddCardToRightSplit%"
                    , it = "%it:AddCardToRightSplit%"
                    , ro = "%ro:AddCardToRightSplit%"
                    , hr = "%hr:AddCardToRightSplit%"
                    , nl = "%nl:AddCardToRightSplit%"
                    , hu = "%hu:AddCardToRightSplit%"
                    , sv = "%sv:AddCardToRightSplit%"
                    , ca = "%ca:AddCardToRightSplit%"
                    , br = "%br:AddCardToRightSplit%"
                    , mr = "%mr:AddCardToRightSplit%"
                    , cs = "%cs:AddCardToRightSplit%"
                    , fa = "%fa:AddCardToRightSplit%"
                    , id = "%id:AddCardToRightSplit%"
                    , ko = "%ko:AddCardToRightSplit%"
                    , pl = "%pl:AddCardToRightSplit%"
                    , uk = "%uk:AddCardToRightSplit%"
                    , nb = "%nb:AddCardToRightSplit%"
                    }

                DeleteCard ->
                    { en = "Delete card (and its children)"
                    , zh_hans = "%zh_hans:DeleteCard%"
                    , zh_hant = "%zh_hant:DeleteCard%"
                    , es = "%es:DeleteCard%"
                    , ar = "%ar:DeleteCard%"
                    , fr = "%fr:DeleteCard%"
                    , ru = "%ru:DeleteCard%"
                    , de = "%de:DeleteCard%"
                    , ja = "%ja:DeleteCard%"
                    , pes = "%pes:DeleteCard%"
                    , it = "%it:DeleteCard%"
                    , ro = "%ro:DeleteCard%"
                    , hr = "%hr:DeleteCard%"
                    , nl = "%nl:DeleteCard%"
                    , hu = "%hu:DeleteCard%"
                    , sv = "%sv:DeleteCard%"
                    , ca = "%ca:DeleteCard%"
                    , br = "%br:DeleteCard%"
                    , mr = "%mr:DeleteCard%"
                    , cs = "%cs:DeleteCard%"
                    , fa = "%fa:DeleteCard%"
                    , id = "%id:DeleteCard%"
                    , ko = "%ko:DeleteCard%"
                    , pl = "%pl:DeleteCard%"
                    , uk = "%uk:DeleteCard%"
                    , nb = "%nb:DeleteCard%"
                    }

                GoUpDownLeftRight ->
                    { en = "Go up/down/left/right"
                    , zh_hans = "%zh_hans:GoUpDownLeftRight%"
                    , zh_hant = "%zh_hant:GoUpDownLeftRight%"
                    , es = "%es:GoUpDownLeftRight%"
                    , ar = "%ar:GoUpDownLeftRight%"
                    , fr = "%fr:GoUpDownLeftRight%"
                    , ru = "%ru:GoUpDownLeftRight%"
                    , de = "%de:GoUpDownLeftRight%"
                    , ja = "%ja:GoUpDownLeftRight%"
                    , pes = "%pes:GoUpDownLeftRight%"
                    , it = "%it:GoUpDownLeftRight%"
                    , ro = "%ro:GoUpDownLeftRight%"
                    , hr = "%hr:GoUpDownLeftRight%"
                    , nl = "%nl:GoUpDownLeftRight%"
                    , hu = "%hu:GoUpDownLeftRight%"
                    , sv = "%sv:GoUpDownLeftRight%"
                    , ca = "%ca:GoUpDownLeftRight%"
                    , br = "%br:GoUpDownLeftRight%"
                    , mr = "%mr:GoUpDownLeftRight%"
                    , cs = "%cs:GoUpDownLeftRight%"
                    , fa = "%fa:GoUpDownLeftRight%"
                    , id = "%id:GoUpDownLeftRight%"
                    , ko = "%ko:GoUpDownLeftRight%"
                    , pl = "%pl:GoUpDownLeftRight%"
                    , uk = "%uk:GoUpDownLeftRight%"
                    , nb = "%nb:GoUpDownLeftRight%"
                    }

                GoToBeginningOfGroup ->
                    { en = "Go to beginning of group"
                    , zh_hans = "%zh_hans:GoToBeginningOfGroup%"
                    , zh_hant = "%zh_hant:GoToBeginningOfGroup%"
                    , es = "%es:GoToBeginningOfGroup%"
                    , ar = "%ar:GoToBeginningOfGroup%"
                    , fr = "%fr:GoToBeginningOfGroup%"
                    , ru = "%ru:GoToBeginningOfGroup%"
                    , de = "%de:GoToBeginningOfGroup%"
                    , ja = "%ja:GoToBeginningOfGroup%"
                    , pes = "%pes:GoToBeginningOfGroup%"
                    , it = "%it:GoToBeginningOfGroup%"
                    , ro = "%ro:GoToBeginningOfGroup%"
                    , hr = "%hr:GoToBeginningOfGroup%"
                    , nl = "%nl:GoToBeginningOfGroup%"
                    , hu = "%hu:GoToBeginningOfGroup%"
                    , sv = "%sv:GoToBeginningOfGroup%"
                    , ca = "%ca:GoToBeginningOfGroup%"
                    , br = "%br:GoToBeginningOfGroup%"
                    , mr = "%mr:GoToBeginningOfGroup%"
                    , cs = "%cs:GoToBeginningOfGroup%"
                    , fa = "%fa:GoToBeginningOfGroup%"
                    , id = "%id:GoToBeginningOfGroup%"
                    , ko = "%ko:GoToBeginningOfGroup%"
                    , pl = "%pl:GoToBeginningOfGroup%"
                    , uk = "%uk:GoToBeginningOfGroup%"
                    , nb = "%nb:GoToBeginningOfGroup%"
                    }

                GoToEndOfGroup ->
                    { en = "Go to end of group"
                    , zh_hans = "%zh_hans:GoToEndOfGroup%"
                    , zh_hant = "%zh_hant:GoToEndOfGroup%"
                    , es = "%es:GoToEndOfGroup%"
                    , ar = "%ar:GoToEndOfGroup%"
                    , fr = "%fr:GoToEndOfGroup%"
                    , ru = "%ru:GoToEndOfGroup%"
                    , de = "%de:GoToEndOfGroup%"
                    , ja = "%ja:GoToEndOfGroup%"
                    , pes = "%pes:GoToEndOfGroup%"
                    , it = "%it:GoToEndOfGroup%"
                    , ro = "%ro:GoToEndOfGroup%"
                    , hr = "%hr:GoToEndOfGroup%"
                    , nl = "%nl:GoToEndOfGroup%"
                    , hu = "%hu:GoToEndOfGroup%"
                    , sv = "%sv:GoToEndOfGroup%"
                    , ca = "%ca:GoToEndOfGroup%"
                    , br = "%br:GoToEndOfGroup%"
                    , mr = "%mr:GoToEndOfGroup%"
                    , cs = "%cs:GoToEndOfGroup%"
                    , fa = "%fa:GoToEndOfGroup%"
                    , id = "%id:GoToEndOfGroup%"
                    , ko = "%ko:GoToEndOfGroup%"
                    , pl = "%pl:GoToEndOfGroup%"
                    , uk = "%uk:GoToEndOfGroup%"
                    , nb = "%nb:GoToEndOfGroup%"
                    }

                GoToBeginningOfColumn ->
                    { en = "Go to beginning of column"
                    , zh_hans = "%zh_hans:GoToBeginningOfColumn%"
                    , zh_hant = "%zh_hant:GoToBeginningOfColumn%"
                    , es = "%es:GoToBeginningOfColumn%"
                    , ar = "%ar:GoToBeginningOfColumn%"
                    , fr = "%fr:GoToBeginningOfColumn%"
                    , ru = "%ru:GoToBeginningOfColumn%"
                    , de = "%de:GoToBeginningOfColumn%"
                    , ja = "%ja:GoToBeginningOfColumn%"
                    , pes = "%pes:GoToBeginningOfColumn%"
                    , it = "%it:GoToBeginningOfColumn%"
                    , ro = "%ro:GoToBeginningOfColumn%"
                    , hr = "%hr:GoToBeginningOfColumn%"
                    , nl = "%nl:GoToBeginningOfColumn%"
                    , hu = "%hu:GoToBeginningOfColumn%"
                    , sv = "%sv:GoToBeginningOfColumn%"
                    , ca = "%ca:GoToBeginningOfColumn%"
                    , br = "%br:GoToBeginningOfColumn%"
                    , mr = "%mr:GoToBeginningOfColumn%"
                    , cs = "%cs:GoToBeginningOfColumn%"
                    , fa = "%fa:GoToBeginningOfColumn%"
                    , id = "%id:GoToBeginningOfColumn%"
                    , ko = "%ko:GoToBeginningOfColumn%"
                    , pl = "%pl:GoToBeginningOfColumn%"
                    , uk = "%uk:GoToBeginningOfColumn%"
                    , nb = "%nb:GoToBeginningOfColumn%"
                    }

                GoToEndOfColumn ->
                    { en = "Go to end of column"
                    , zh_hans = "%zh_hans:GoToEndOfColumn%"
                    , zh_hant = "%zh_hant:GoToEndOfColumn%"
                    , es = "%es:GoToEndOfColumn%"
                    , ar = "%ar:GoToEndOfColumn%"
                    , fr = "%fr:GoToEndOfColumn%"
                    , ru = "%ru:GoToEndOfColumn%"
                    , de = "%de:GoToEndOfColumn%"
                    , ja = "%ja:GoToEndOfColumn%"
                    , pes = "%pes:GoToEndOfColumn%"
                    , it = "%it:GoToEndOfColumn%"
                    , ro = "%ro:GoToEndOfColumn%"
                    , hr = "%hr:GoToEndOfColumn%"
                    , nl = "%nl:GoToEndOfColumn%"
                    , hu = "%hu:GoToEndOfColumn%"
                    , sv = "%sv:GoToEndOfColumn%"
                    , ca = "%ca:GoToEndOfColumn%"
                    , br = "%br:GoToEndOfColumn%"
                    , mr = "%mr:GoToEndOfColumn%"
                    , cs = "%cs:GoToEndOfColumn%"
                    , fa = "%fa:GoToEndOfColumn%"
                    , id = "%id:GoToEndOfColumn%"
                    , ko = "%ko:GoToEndOfColumn%"
                    , pl = "%pl:GoToEndOfColumn%"
                    , uk = "%uk:GoToEndOfColumn%"
                    , nb = "%nb:GoToEndOfColumn%"
                    }

                MoveCurrentCard ->
                    { en = "Move current card (and children)"
                    , zh_hans = "%zh_hans:MoveCurrentCard%"
                    , zh_hant = "%zh_hant:MoveCurrentCard%"
                    , es = "%es:MoveCurrentCard%"
                    , ar = "%ar:MoveCurrentCard%"
                    , fr = "%fr:MoveCurrentCard%"
                    , ru = "%ru:MoveCurrentCard%"
                    , de = "%de:MoveCurrentCard%"
                    , ja = "%ja:MoveCurrentCard%"
                    , pes = "%pes:MoveCurrentCard%"
                    , it = "%it:MoveCurrentCard%"
                    , ro = "%ro:MoveCurrentCard%"
                    , hr = "%hr:MoveCurrentCard%"
                    , nl = "%nl:MoveCurrentCard%"
                    , hu = "%hu:MoveCurrentCard%"
                    , sv = "%sv:MoveCurrentCard%"
                    , ca = "%ca:MoveCurrentCard%"
                    , br = "%br:MoveCurrentCard%"
                    , mr = "%mr:MoveCurrentCard%"
                    , cs = "%cs:MoveCurrentCard%"
                    , fa = "%fa:MoveCurrentCard%"
                    , id = "%id:MoveCurrentCard%"
                    , ko = "%ko:MoveCurrentCard%"
                    , pl = "%pl:MoveCurrentCard%"
                    , uk = "%uk:MoveCurrentCard%"
                    , nb = "%nb:MoveCurrentCard%"
                    }

                PageUp ->
                    { en = "PageUp"
                    , zh_hans = "%zh_hans:PageUp%"
                    , zh_hant = "%zh_hant:PageUp%"
                    , es = "%es:PageUp%"
                    , ar = "%ar:PageUp%"
                    , fr = "%fr:PageUp%"
                    , ru = "%ru:PageUp%"
                    , de = "%de:PageUp%"
                    , ja = "%ja:PageUp%"
                    , pes = "%pes:PageUp%"
                    , it = "%it:PageUp%"
                    , ro = "%ro:PageUp%"
                    , hr = "%hr:PageUp%"
                    , nl = "%nl:PageUp%"
                    , hu = "%hu:PageUp%"
                    , sv = "%sv:PageUp%"
                    , ca = "%ca:PageUp%"
                    , br = "%br:PageUp%"
                    , mr = "%mr:PageUp%"
                    , cs = "%cs:PageUp%"
                    , fa = "%fa:PageUp%"
                    , id = "%id:PageUp%"
                    , ko = "%ko:PageUp%"
                    , pl = "%pl:PageUp%"
                    , uk = "%uk:PageUp%"
                    , nb = "%nb:PageUp%"
                    }

                PageDown ->
                    { en = "PageDown"
                    , zh_hans = "%zh_hans:PageDown%"
                    , zh_hant = "%zh_hant:PageDown%"
                    , es = "%es:PageDown%"
                    , ar = "%ar:PageDown%"
                    , fr = "%fr:PageDown%"
                    , ru = "%ru:PageDown%"
                    , de = "%de:PageDown%"
                    , ja = "%ja:PageDown%"
                    , pes = "%pes:PageDown%"
                    , it = "%it:PageDown%"
                    , ro = "%ro:PageDown%"
                    , hr = "%hr:PageDown%"
                    , nl = "%nl:PageDown%"
                    , hu = "%hu:PageDown%"
                    , sv = "%sv:PageDown%"
                    , ca = "%ca:PageDown%"
                    , br = "%br:PageDown%"
                    , mr = "%mr:PageDown%"
                    , cs = "%cs:PageDown%"
                    , fa = "%fa:PageDown%"
                    , id = "%id:PageDown%"
                    , ko = "%ko:PageDown%"
                    , pl = "%pl:PageDown%"
                    , uk = "%uk:PageDown%"
                    , nb = "%nb:PageDown%"
                    }

                HomeKey ->
                    { en = "Home"
                    , zh_hans = "%zh_hans:HomeKey%"
                    , zh_hant = "%zh_hant:HomeKey%"
                    , es = "%es:HomeKey%"
                    , ar = "%ar:HomeKey%"
                    , fr = "%fr:HomeKey%"
                    , ru = "%ru:HomeKey%"
                    , de = "%de:HomeKey%"
                    , ja = "%ja:HomeKey%"
                    , pes = "%pes:HomeKey%"
                    , it = "%it:HomeKey%"
                    , ro = "%ro:HomeKey%"
                    , hr = "%hr:HomeKey%"
                    , nl = "%nl:HomeKey%"
                    , hu = "%hu:HomeKey%"
                    , sv = "%sv:HomeKey%"
                    , ca = "%ca:HomeKey%"
                    , br = "%br:HomeKey%"
                    , mr = "%mr:HomeKey%"
                    , cs = "%cs:HomeKey%"
                    , fa = "%fa:HomeKey%"
                    , id = "%id:HomeKey%"
                    , ko = "%ko:HomeKey%"
                    , pl = "%pl:HomeKey%"
                    , uk = "%uk:HomeKey%"
                    , nb = "%nb:HomeKey%"
                    }

                EndKey ->
                    { en = "End"
                    , zh_hans = "%zh_hans:EndKey%"
                    , zh_hant = "%zh_hant:EndKey%"
                    , es = "%es:EndKey%"
                    , ar = "%ar:EndKey%"
                    , fr = "%fr:EndKey%"
                    , ru = "%ru:EndKey%"
                    , de = "%de:EndKey%"
                    , ja = "%ja:EndKey%"
                    , pes = "%pes:EndKey%"
                    , it = "%it:EndKey%"
                    , ro = "%ro:EndKey%"
                    , hr = "%hr:EndKey%"
                    , nl = "%nl:EndKey%"
                    , hu = "%hu:EndKey%"
                    , sv = "%sv:EndKey%"
                    , ca = "%ca:EndKey%"
                    , br = "%br:EndKey%"
                    , mr = "%mr:EndKey%"
                    , cs = "%cs:EndKey%"
                    , fa = "%fa:EndKey%"
                    , id = "%id:EndKey%"
                    , ko = "%ko:EndKey%"
                    , pl = "%pl:EndKey%"
                    , uk = "%uk:EndKey%"
                    , nb = "%nb:EndKey%"
                    }

                AnyOfAbove ->
                    { en = "(any of above)"
                    , zh_hans = "%zh_hans:AnyOfAbove%"
                    , zh_hant = "%zh_hant:AnyOfAbove%"
                    , es = "%es:AnyOfAbove%"
                    , ar = "%ar:AnyOfAbove%"
                    , fr = "%fr:AnyOfAbove%"
                    , ru = "%ru:AnyOfAbove%"
                    , de = "%de:AnyOfAbove%"
                    , ja = "%ja:AnyOfAbove%"
                    , pes = "%pes:AnyOfAbove%"
                    , it = "%it:AnyOfAbove%"
                    , ro = "%ro:AnyOfAbove%"
                    , hr = "%hr:AnyOfAbove%"
                    , nl = "%nl:AnyOfAbove%"
                    , hu = "%hu:AnyOfAbove%"
                    , sv = "%sv:AnyOfAbove%"
                    , ca = "%ca:AnyOfAbove%"
                    , br = "%br:AnyOfAbove%"
                    , mr = "%mr:AnyOfAbove%"
                    , cs = "%cs:AnyOfAbove%"
                    , fa = "%fa:AnyOfAbove%"
                    , id = "%id:AnyOfAbove%"
                    , ko = "%ko:AnyOfAbove%"
                    , pl = "%pl:AnyOfAbove%"
                    , uk = "%uk:AnyOfAbove%"
                    , nb = "%nb:AnyOfAbove%"
                    }

                DragCard ->
                    { en = "Drag card by left edge"
                    , zh_hans = "%zh_hans:DragCard%"
                    , zh_hant = "%zh_hant:DragCard%"
                    , es = "%es:DragCard%"
                    , ar = "%ar:DragCard%"
                    , fr = "%fr:DragCard%"
                    , ru = "%ru:DragCard%"
                    , de = "%de:DragCard%"
                    , ja = "%ja:DragCard%"
                    , pes = "%pes:DragCard%"
                    , it = "%it:DragCard%"
                    , ro = "%ro:DragCard%"
                    , hr = "%hr:DragCard%"
                    , nl = "%nl:DragCard%"
                    , hu = "%hu:DragCard%"
                    , sv = "%sv:DragCard%"
                    , ca = "%ca:DragCard%"
                    , br = "%br:DragCard%"
                    , mr = "%mr:DragCard%"
                    , cs = "%cs:DragCard%"
                    , fa = "%fa:DragCard%"
                    , id = "%id:DragCard%"
                    , ko = "%ko:DragCard%"
                    , pl = "%pl:DragCard%"
                    , uk = "%uk:DragCard%"
                    , nb = "%nb:DragCard%"
                    }

                Search ->
                    { en = "Search"
                    , zh_hans = "%zh_hans:Search%"
                    , zh_hant = "%zh_hant:Search%"
                    , es = "%es:Search%"
                    , ar = "%ar:Search%"
                    , fr = "%fr:Search%"
                    , ru = "%ru:Search%"
                    , de = "%de:Search%"
                    , ja = "%ja:Search%"
                    , pes = "%pes:Search%"
                    , it = "%it:Search%"
                    , ro = "%ro:Search%"
                    , hr = "%hr:Search%"
                    , nl = "%nl:Search%"
                    , hu = "%hu:Search%"
                    , sv = "%sv:Search%"
                    , ca = "%ca:Search%"
                    , br = "%br:Search%"
                    , mr = "%mr:Search%"
                    , cs = "%cs:Search%"
                    , fa = "%fa:Search%"
                    , id = "%id:Search%"
                    , ko = "%ko:Search%"
                    , pl = "%pl:Search%"
                    , uk = "%uk:Search%"
                    , nb = "%nb:Search%"
                    }

                ClearSearch ->
                    { en = "Clear search, focus current card"
                    , zh_hans = "%zh_hans:ClearSearch%"
                    , zh_hant = "%zh_hant:ClearSearch%"
                    , es = "%es:ClearSearch%"
                    , ar = "%ar:ClearSearch%"
                    , fr = "%fr:ClearSearch%"
                    , ru = "%ru:ClearSearch%"
                    , de = "%de:ClearSearch%"
                    , ja = "%ja:ClearSearch%"
                    , pes = "%pes:ClearSearch%"
                    , it = "%it:ClearSearch%"
                    , ro = "%ro:ClearSearch%"
                    , hr = "%hr:ClearSearch%"
                    , nl = "%nl:ClearSearch%"
                    , hu = "%hu:ClearSearch%"
                    , sv = "%sv:ClearSearch%"
                    , ca = "%ca:ClearSearch%"
                    , br = "%br:ClearSearch%"
                    , mr = "%mr:ClearSearch%"
                    , cs = "%cs:ClearSearch%"
                    , fa = "%fa:ClearSearch%"
                    , id = "%id:ClearSearch%"
                    , ko = "%ko:ClearSearch%"
                    , pl = "%pl:ClearSearch%"
                    , uk = "%uk:ClearSearch%"
                    , nb = "%nb:ClearSearch%"
                    }

                MergeCardUp ->
                    { en = "Merge card up"
                    , zh_hans = "%zh_hans:MergeCardUp%"
                    , zh_hant = "%zh_hant:MergeCardUp%"
                    , es = "%es:MergeCardUp%"
                    , ar = "%ar:MergeCardUp%"
                    , fr = "%fr:MergeCardUp%"
                    , ru = "%ru:MergeCardUp%"
                    , de = "%de:MergeCardUp%"
                    , ja = "%ja:MergeCardUp%"
                    , pes = "%pes:MergeCardUp%"
                    , it = "%it:MergeCardUp%"
                    , ro = "%ro:MergeCardUp%"
                    , hr = "%hr:MergeCardUp%"
                    , nl = "%nl:MergeCardUp%"
                    , hu = "%hu:MergeCardUp%"
                    , sv = "%sv:MergeCardUp%"
                    , ca = "%ca:MergeCardUp%"
                    , br = "%br:MergeCardUp%"
                    , mr = "%mr:MergeCardUp%"
                    , cs = "%cs:MergeCardUp%"
                    , fa = "%fa:MergeCardUp%"
                    , id = "%id:MergeCardUp%"
                    , ko = "%ko:MergeCardUp%"
                    , pl = "%pl:MergeCardUp%"
                    , uk = "%uk:MergeCardUp%"
                    , nb = "%nb:MergeCardUp%"
                    }

                MergeCardDown ->
                    { en = "Merge card down"
                    , zh_hans = "%zh_hans:MergeCardDown%"
                    , zh_hant = "%zh_hant:MergeCardDown%"
                    , es = "%es:MergeCardDown%"
                    , ar = "%ar:MergeCardDown%"
                    , fr = "%fr:MergeCardDown%"
                    , ru = "%ru:MergeCardDown%"
                    , de = "%de:MergeCardDown%"
                    , ja = "%ja:MergeCardDown%"
                    , pes = "%pes:MergeCardDown%"
                    , it = "%it:MergeCardDown%"
                    , ro = "%ro:MergeCardDown%"
                    , hr = "%hr:MergeCardDown%"
                    , nl = "%nl:MergeCardDown%"
                    , hu = "%hu:MergeCardDown%"
                    , sv = "%sv:MergeCardDown%"
                    , ca = "%ca:MergeCardDown%"
                    , br = "%br:MergeCardDown%"
                    , mr = "%mr:MergeCardDown%"
                    , cs = "%cs:MergeCardDown%"
                    , fa = "%fa:MergeCardDown%"
                    , id = "%id:MergeCardDown%"
                    , ko = "%ko:MergeCardDown%"
                    , pl = "%pl:MergeCardDown%"
                    , uk = "%uk:MergeCardDown%"
                    , nb = "%nb:MergeCardDown%"
                    }

                WorksAcrossDocuments ->
                    { en = "(Works across documents)"
                    , zh_hans = "%zh_hans:WorksAcrossDocuments%"
                    , zh_hant = "%zh_hant:WorksAcrossDocuments%"
                    , es = "%es:WorksAcrossDocuments%"
                    , ar = "%ar:WorksAcrossDocuments%"
                    , fr = "%fr:WorksAcrossDocuments%"
                    , ru = "%ru:WorksAcrossDocuments%"
                    , de = "%de:WorksAcrossDocuments%"
                    , ja = "%ja:WorksAcrossDocuments%"
                    , pes = "%pes:WorksAcrossDocuments%"
                    , it = "%it:WorksAcrossDocuments%"
                    , ro = "%ro:WorksAcrossDocuments%"
                    , hr = "%hr:WorksAcrossDocuments%"
                    , nl = "%nl:WorksAcrossDocuments%"
                    , hu = "%hu:WorksAcrossDocuments%"
                    , sv = "%sv:WorksAcrossDocuments%"
                    , ca = "%ca:WorksAcrossDocuments%"
                    , br = "%br:WorksAcrossDocuments%"
                    , mr = "%mr:WorksAcrossDocuments%"
                    , cs = "%cs:WorksAcrossDocuments%"
                    , fa = "%fa:WorksAcrossDocuments%"
                    , id = "%id:WorksAcrossDocuments%"
                    , ko = "%ko:WorksAcrossDocuments%"
                    , pl = "%pl:WorksAcrossDocuments%"
                    , uk = "%uk:WorksAcrossDocuments%"
                    , nb = "%nb:WorksAcrossDocuments%"
                    }

                CopyCurrent ->
                    { en = "Copy current subtree"
                    , zh_hans = "%zh_hans:CopyCurrent%"
                    , zh_hant = "%zh_hant:CopyCurrent%"
                    , es = "%es:CopyCurrent%"
                    , ar = "%ar:CopyCurrent%"
                    , fr = "%fr:CopyCurrent%"
                    , ru = "%ru:CopyCurrent%"
                    , de = "%de:CopyCurrent%"
                    , ja = "%ja:CopyCurrent%"
                    , pes = "%pes:CopyCurrent%"
                    , it = "%it:CopyCurrent%"
                    , ro = "%ro:CopyCurrent%"
                    , hr = "%hr:CopyCurrent%"
                    , nl = "%nl:CopyCurrent%"
                    , hu = "%hu:CopyCurrent%"
                    , sv = "%sv:CopyCurrent%"
                    , ca = "%ca:CopyCurrent%"
                    , br = "%br:CopyCurrent%"
                    , mr = "%mr:CopyCurrent%"
                    , cs = "%cs:CopyCurrent%"
                    , fa = "%fa:CopyCurrent%"
                    , id = "%id:CopyCurrent%"
                    , ko = "%ko:CopyCurrent%"
                    , pl = "%pl:CopyCurrent%"
                    , uk = "%uk:CopyCurrent%"
                    , nb = "%nb:CopyCurrent%"
                    }

                PasteBelow ->
                    { en = "Paste subtree below current card"
                    , zh_hans = "%zh_hans:PasteBelow%"
                    , zh_hant = "%zh_hant:PasteBelow%"
                    , es = "%es:PasteBelow%"
                    , ar = "%ar:PasteBelow%"
                    , fr = "%fr:PasteBelow%"
                    , ru = "%ru:PasteBelow%"
                    , de = "%de:PasteBelow%"
                    , ja = "%ja:PasteBelow%"
                    , pes = "%pes:PasteBelow%"
                    , it = "%it:PasteBelow%"
                    , ro = "%ro:PasteBelow%"
                    , hr = "%hr:PasteBelow%"
                    , nl = "%nl:PasteBelow%"
                    , hu = "%hu:PasteBelow%"
                    , sv = "%sv:PasteBelow%"
                    , ca = "%ca:PasteBelow%"
                    , br = "%br:PasteBelow%"
                    , mr = "%mr:PasteBelow%"
                    , cs = "%cs:PasteBelow%"
                    , fa = "%fa:PasteBelow%"
                    , id = "%id:PasteBelow%"
                    , ko = "%ko:PasteBelow%"
                    , pl = "%pl:PasteBelow%"
                    , uk = "%uk:PasteBelow%"
                    , nb = "%nb:PasteBelow%"
                    }

                PasteAsChild ->
                    { en = "Paste subtree as child of current card"
                    , zh_hans = "%zh_hans:PasteAsChild%"
                    , zh_hant = "%zh_hant:PasteAsChild%"
                    , es = "%es:PasteAsChild%"
                    , ar = "%ar:PasteAsChild%"
                    , fr = "%fr:PasteAsChild%"
                    , ru = "%ru:PasteAsChild%"
                    , de = "%de:PasteAsChild%"
                    , ja = "%ja:PasteAsChild%"
                    , pes = "%pes:PasteAsChild%"
                    , it = "%it:PasteAsChild%"
                    , ro = "%ro:PasteAsChild%"
                    , hr = "%hr:PasteAsChild%"
                    , nl = "%nl:PasteAsChild%"
                    , hu = "%hu:PasteAsChild%"
                    , sv = "%sv:PasteAsChild%"
                    , ca = "%ca:PasteAsChild%"
                    , br = "%br:PasteAsChild%"
                    , mr = "%mr:PasteAsChild%"
                    , cs = "%cs:PasteAsChild%"
                    , fa = "%fa:PasteAsChild%"
                    , id = "%id:PasteAsChild%"
                    , ko = "%ko:PasteAsChild%"
                    , pl = "%pl:PasteAsChild%"
                    , uk = "%uk:PasteAsChild%"
                    , nb = "%nb:PasteAsChild%"
                    }

                InsertSelected ->
                    { en = "Insert selected text as new card"
                    , zh_hans = "%zh_hans:InsertSelected%"
                    , zh_hant = "%zh_hant:InsertSelected%"
                    , es = "%es:InsertSelected%"
                    , ar = "%ar:InsertSelected%"
                    , fr = "%fr:InsertSelected%"
                    , ru = "%ru:InsertSelected%"
                    , de = "%de:InsertSelected%"
                    , ja = "%ja:InsertSelected%"
                    , pes = "%pes:InsertSelected%"
                    , it = "%it:InsertSelected%"
                    , ro = "%ro:InsertSelected%"
                    , hr = "%hr:InsertSelected%"
                    , nl = "%nl:InsertSelected%"
                    , hu = "%hu:InsertSelected%"
                    , sv = "%sv:InsertSelected%"
                    , ca = "%ca:InsertSelected%"
                    , br = "%br:InsertSelected%"
                    , mr = "%mr:InsertSelected%"
                    , cs = "%cs:InsertSelected%"
                    , fa = "%fa:InsertSelected%"
                    , id = "%id:InsertSelected%"
                    , ko = "%ko:InsertSelected%"
                    , pl = "%pl:InsertSelected%"
                    , uk = "%uk:InsertSelected%"
                    , nb = "%nb:InsertSelected%"
                    }

                DragSelected ->
                    { en = "Drag selected text into tree"
                    , zh_hans = "%zh_hans:DragSelected%"
                    , zh_hant = "%zh_hant:DragSelected%"
                    , es = "%es:DragSelected%"
                    , ar = "%ar:DragSelected%"
                    , fr = "%fr:DragSelected%"
                    , ru = "%ru:DragSelected%"
                    , de = "%de:DragSelected%"
                    , ja = "%ja:DragSelected%"
                    , pes = "%pes:DragSelected%"
                    , it = "%it:DragSelected%"
                    , ro = "%ro:DragSelected%"
                    , hr = "%hr:DragSelected%"
                    , nl = "%nl:DragSelected%"
                    , hu = "%hu:DragSelected%"
                    , sv = "%sv:DragSelected%"
                    , ca = "%ca:DragSelected%"
                    , br = "%br:DragSelected%"
                    , mr = "%mr:DragSelected%"
                    , cs = "%cs:DragSelected%"
                    , fa = "%fa:DragSelected%"
                    , id = "%id:DragSelected%"
                    , ko = "%ko:DragSelected%"
                    , pl = "%pl:DragSelected%"
                    , uk = "%uk:DragSelected%"
                    , nb = "%nb:DragSelected%"
                    }

                WordCounts ->
                    { en = "Word counts"
                    , zh_hans = "%zh_hans:WordCounts%"
                    , zh_hant = "%zh_hant:WordCounts%"
                    , es = "%es:WordCounts%"
                    , ar = "%ar:WordCounts%"
                    , fr = "%fr:WordCounts%"
                    , ru = "%ru:WordCounts%"
                    , de = "%de:WordCounts%"
                    , ja = "%ja:WordCounts%"
                    , pes = "%pes:WordCounts%"
                    , it = "%it:WordCounts%"
                    , ro = "%ro:WordCounts%"
                    , hr = "%hr:WordCounts%"
                    , nl = "%nl:WordCounts%"
                    , hu = "%hu:WordCounts%"
                    , sv = "%sv:WordCounts%"
                    , ca = "%ca:WordCounts%"
                    , br = "%br:WordCounts%"
                    , mr = "%mr:WordCounts%"
                    , cs = "%cs:WordCounts%"
                    , fa = "%fa:WordCounts%"
                    , id = "%id:WordCounts%"
                    , ko = "%ko:WordCounts%"
                    , pl = "%pl:WordCounts%"
                    , uk = "%uk:WordCounts%"
                    , nb = "%nb:WordCounts%"
                    }

                SwitchDocuments ->
                    { en = "Switch to different document"
                    , zh_hans = "%zh_hans:SwitchDocuments%"
                    , zh_hant = "%zh_hant:SwitchDocuments%"
                    , es = "%es:SwitchDocuments%"
                    , ar = "%ar:SwitchDocuments%"
                    , fr = "%fr:SwitchDocuments%"
                    , ru = "%ru:SwitchDocuments%"
                    , de = "%de:SwitchDocuments%"
                    , ja = "%ja:SwitchDocuments%"
                    , pes = "%pes:SwitchDocuments%"
                    , it = "%it:SwitchDocuments%"
                    , ro = "%ro:SwitchDocuments%"
                    , hr = "%hr:SwitchDocuments%"
                    , nl = "%nl:SwitchDocuments%"
                    , hu = "%hu:SwitchDocuments%"
                    , sv = "%sv:SwitchDocuments%"
                    , ca = "%ca:SwitchDocuments%"
                    , br = "%br:SwitchDocuments%"
                    , mr = "%mr:SwitchDocuments%"
                    , cs = "%cs:SwitchDocuments%"
                    , fa = "%fa:SwitchDocuments%"
                    , id = "%id:SwitchDocuments%"
                    , ko = "%ko:SwitchDocuments%"
                    , pl = "%pl:SwitchDocuments%"
                    , uk = "%uk:SwitchDocuments%"
                    , nb = "%nb:SwitchDocuments%"
                    }

                ThisHelpScreen ->
                    { en = "This help screen"
                    , zh_hans = "%zh_hans:ThisHelpScreen%"
                    , zh_hant = "%zh_hant:ThisHelpScreen%"
                    , es = "%es:ThisHelpScreen%"
                    , ar = "%ar:ThisHelpScreen%"
                    , fr = "%fr:ThisHelpScreen%"
                    , ru = "%ru:ThisHelpScreen%"
                    , de = "%de:ThisHelpScreen%"
                    , ja = "%ja:ThisHelpScreen%"
                    , pes = "%pes:ThisHelpScreen%"
                    , it = "%it:ThisHelpScreen%"
                    , ro = "%ro:ThisHelpScreen%"
                    , hr = "%hr:ThisHelpScreen%"
                    , nl = "%nl:ThisHelpScreen%"
                    , hu = "%hu:ThisHelpScreen%"
                    , sv = "%sv:ThisHelpScreen%"
                    , ca = "%ca:ThisHelpScreen%"
                    , br = "%br:ThisHelpScreen%"
                    , mr = "%mr:ThisHelpScreen%"
                    , cs = "%cs:ThisHelpScreen%"
                    , fa = "%fa:ThisHelpScreen%"
                    , id = "%id:ThisHelpScreen%"
                    , ko = "%ko:ThisHelpScreen%"
                    , pl = "%pl:ThisHelpScreen%"
                    , uk = "%uk:ThisHelpScreen%"
                    , nb = "%nb:ThisHelpScreen%"
                    }

                Or ->
                    { en = " or "
                    , zh_hans = "%zh_hans:Or%"
                    , zh_hant = "%zh_hant:Or%"
                    , es = "%es:Or%"
                    , ar = "%ar:Or%"
                    , fr = "%fr:Or%"
                    , ru = "%ru:Or%"
                    , de = "%de:Or%"
                    , ja = "%ja:Or%"
                    , pes = "%pes:Or%"
                    , it = "%it:Or%"
                    , ro = "%ro:Or%"
                    , hr = "%hr:Or%"
                    , nl = "%nl:Or%"
                    , hu = "%hu:Or%"
                    , sv = "%sv:Or%"
                    , ca = "%ca:Or%"
                    , br = "%br:Or%"
                    , mr = "%mr:Or%"
                    , cs = "%cs:Or%"
                    , fa = "%fa:Or%"
                    , id = "%id:Or%"
                    , ko = "%ko:Or%"
                    , pl = "%pl:Or%"
                    , uk = "%uk:Or%"
                    , nb = "%nb:Or%"
                    }

                EditCardFullscreen ->
                    { en = "Edit card in fullscreen mode"
                    , zh_hans = "%zh_hans:EditCardFullscreen%"
                    , zh_hant = "%zh_hant:EditCardFullscreen%"
                    , es = "%es:EditCardFullscreen%"
                    , ar = "%ar:EditCardFullscreen%"
                    , fr = "%fr:EditCardFullscreen%"
                    , ru = "%ru:EditCardFullscreen%"
                    , de = "%de:EditCardFullscreen%"
                    , ja = "%ja:EditCardFullscreen%"
                    , pes = "%pes:EditCardFullscreen%"
                    , it = "%it:EditCardFullscreen%"
                    , ro = "%ro:EditCardFullscreen%"
                    , hr = "%hr:EditCardFullscreen%"
                    , nl = "%nl:EditCardFullscreen%"
                    , hu = "%hu:EditCardFullscreen%"
                    , sv = "%sv:EditCardFullscreen%"
                    , ca = "%ca:EditCardFullscreen%"
                    , br = "%br:EditCardFullscreen%"
                    , mr = "%mr:EditCardFullscreen%"
                    , cs = "%cs:EditCardFullscreen%"
                    , fa = "%fa:EditCardFullscreen%"
                    , id = "%id:EditCardFullscreen%"
                    , ko = "%ko:EditCardFullscreen%"
                    , pl = "%pl:EditCardFullscreen%"
                    , uk = "%uk:EditCardFullscreen%"
                    , nb = "%nb:EditCardFullscreen%"
                    }

                EditCards ->
                    { en = "Edit Cards"
                    , zh_hans = "%zh_hans:EditCards%"
                    , zh_hant = "%zh_hant:EditCards%"
                    , es = "%es:EditCards%"
                    , ar = "%ar:EditCards%"
                    , fr = "%fr:EditCards%"
                    , ru = "%ru:EditCards%"
                    , de = "%de:EditCards%"
                    , ja = "%ja:EditCards%"
                    , pes = "%pes:EditCards%"
                    , it = "%it:EditCards%"
                    , ro = "%ro:EditCards%"
                    , hr = "%hr:EditCards%"
                    , nl = "%nl:EditCards%"
                    , hu = "%hu:EditCards%"
                    , sv = "%sv:EditCards%"
                    , ca = "%ca:EditCards%"
                    , br = "%br:EditCards%"
                    , mr = "%mr:EditCards%"
                    , cs = "%cs:EditCards%"
                    , fa = "%fa:EditCards%"
                    , id = "%id:EditCards%"
                    , ko = "%ko:EditCards%"
                    , pl = "%pl:EditCards%"
                    , uk = "%uk:EditCards%"
                    , nb = "%nb:EditCards%"
                    }

                KeyboardHelp ->
                    { en = "Keyboard Shortcuts Help"
                    , zh_hans = "%zh_hans:KeyboardHelp%"
                    , zh_hant = "%zh_hant:KeyboardHelp%"
                    , es = "%es:KeyboardHelp%"
                    , ar = "%ar:KeyboardHelp%"
                    , fr = "%fr:KeyboardHelp%"
                    , ru = "%ru:KeyboardHelp%"
                    , de = "%de:KeyboardHelp%"
                    , ja = "%ja:KeyboardHelp%"
                    , pes = "%pes:KeyboardHelp%"
                    , it = "%it:KeyboardHelp%"
                    , ro = "%ro:KeyboardHelp%"
                    , hr = "%hr:KeyboardHelp%"
                    , nl = "%nl:KeyboardHelp%"
                    , hu = "%hu:KeyboardHelp%"
                    , sv = "%sv:KeyboardHelp%"
                    , ca = "%ca:KeyboardHelp%"
                    , br = "%br:KeyboardHelp%"
                    , mr = "%mr:KeyboardHelp%"
                    , cs = "%cs:KeyboardHelp%"
                    , fa = "%fa:KeyboardHelp%"
                    , id = "%id:KeyboardHelp%"
                    , ko = "%ko:KeyboardHelp%"
                    , pl = "%pl:KeyboardHelp%"
                    , uk = "%uk:KeyboardHelp%"
                    , nb = "%nb:KeyboardHelp%"
                    }

                RestoreThisVersion ->
                    { en = "Restore this Version"
                    , zh_hans = "%zh_hans:RestoreThisVersion%"
                    , zh_hant = "%zh_hant:RestoreThisVersion%"
                    , es = "%es:RestoreThisVersion%"
                    , ar = "%ar:RestoreThisVersion%"
                    , fr = "%fr:RestoreThisVersion%"
                    , ru = "%ru:RestoreThisVersion%"
                    , de = "%de:RestoreThisVersion%"
                    , ja = "%ja:RestoreThisVersion%"
                    , pes = "%pes:RestoreThisVersion%"
                    , it = "%it:RestoreThisVersion%"
                    , ro = "%ro:RestoreThisVersion%"
                    , hr = "%hr:RestoreThisVersion%"
                    , nl = "%nl:RestoreThisVersion%"
                    , hu = "%hu:RestoreThisVersion%"
                    , sv = "%sv:RestoreThisVersion%"
                    , ca = "%ca:RestoreThisVersion%"
                    , br = "%br:RestoreThisVersion%"
                    , mr = "%mr:RestoreThisVersion%"
                    , cs = "%cs:RestoreThisVersion%"
                    , fa = "%fa:RestoreThisVersion%"
                    , id = "%id:RestoreThisVersion%"
                    , ko = "%ko:RestoreThisVersion%"
                    , pl = "%pl:RestoreThisVersion%"
                    , uk = "%uk:RestoreThisVersion%"
                    , nb = "%nb:RestoreThisVersion%"
                    }

                EnterKey ->
                    { en = "Enter"
                    , zh_hans = "%zh_hans:EnterKey%"
                    , zh_hant = "%zh_hant:EnterKey%"
                    , es = "%es:EnterKey%"
                    , ar = "%ar:EnterKey%"
                    , fr = "%fr:EnterKey%"
                    , ru = "%ru:EnterKey%"
                    , de = "%de:EnterKey%"
                    , ja = "%ja:EnterKey%"
                    , pes = "%pes:EnterKey%"
                    , it = "%it:EnterKey%"
                    , ro = "%ro:EnterKey%"
                    , hr = "%hr:EnterKey%"
                    , nl = "%nl:EnterKey%"
                    , hu = "%hu:EnterKey%"
                    , sv = "%sv:EnterKey%"
                    , ca = "%ca:EnterKey%"
                    , br = "%br:EnterKey%"
                    , mr = "%mr:EnterKey%"
                    , cs = "%cs:EnterKey%"
                    , fa = "%fa:EnterKey%"
                    , id = "%id:EnterKey%"
                    , ko = "%ko:EnterKey%"
                    , pl = "%pl:EnterKey%"
                    , uk = "%uk:EnterKey%"
                    , nb = "%nb:EnterKey%"
                    }

                ShiftKey ->
                    { en = "Shift"
                    , zh_hans = "%zh_hans:ShiftKey%"
                    , zh_hant = "%zh_hant:ShiftKey%"
                    , es = "%es:ShiftKey%"
                    , ar = "%ar:ShiftKey%"
                    , fr = "%fr:ShiftKey%"
                    , ru = "%ru:ShiftKey%"
                    , de = "%de:ShiftKey%"
                    , ja = "%ja:ShiftKey%"
                    , pes = "%pes:ShiftKey%"
                    , it = "%it:ShiftKey%"
                    , ro = "%ro:ShiftKey%"
                    , hr = "%hr:ShiftKey%"
                    , nl = "%nl:ShiftKey%"
                    , hu = "%hu:ShiftKey%"
                    , sv = "%sv:ShiftKey%"
                    , ca = "%ca:ShiftKey%"
                    , br = "%br:ShiftKey%"
                    , mr = "%mr:ShiftKey%"
                    , cs = "%cs:ShiftKey%"
                    , fa = "%fa:ShiftKey%"
                    , id = "%id:ShiftKey%"
                    , ko = "%ko:ShiftKey%"
                    , pl = "%pl:ShiftKey%"
                    , uk = "%uk:ShiftKey%"
                    , nb = "%nb:ShiftKey%"
                    }

                EnterAction ->
                    { en = "to Edit"
                    , zh_hans = "%zh_hans:EnterAction%"
                    , zh_hant = "%zh_hant:EnterAction%"
                    , es = "%es:EnterAction%"
                    , ar = "%ar:EnterAction%"
                    , fr = "%fr:EnterAction%"
                    , ru = "%ru:EnterAction%"
                    , de = "%de:EnterAction%"
                    , ja = "%ja:EnterAction%"
                    , pes = "%pes:EnterAction%"
                    , it = "%it:EnterAction%"
                    , ro = "%ro:EnterAction%"
                    , hr = "%hr:EnterAction%"
                    , nl = "%nl:EnterAction%"
                    , hu = "%hu:EnterAction%"
                    , sv = "%sv:EnterAction%"
                    , ca = "%ca:EnterAction%"
                    , br = "%br:EnterAction%"
                    , mr = "%mr:EnterAction%"
                    , cs = "%cs:EnterAction%"
                    , fa = "%fa:EnterAction%"
                    , id = "%id:EnterAction%"
                    , ko = "%ko:EnterAction%"
                    , pl = "%pl:EnterAction%"
                    , uk = "%uk:EnterAction%"
                    , nb = "%nb:EnterAction%"
                    }

                AltKey ->
                    { en = "AltKey"
                    , zh_hans = "%zh_hans:AltKey%"
                    , zh_hant = "%zh_hant:AltKey%"
                    , es = "%es:AltKey%"
                    , ar = "%ar:AltKey%"
                    , fr = "%fr:AltKey%"
                    , ru = "%ru:AltKey%"
                    , de = "%de:AltKey%"
                    , ja = "%ja:AltKey%"
                    , pes = "%pes:AltKey%"
                    , it = "%it:AltKey%"
                    , ro = "%ro:AltKey%"
                    , hr = "%hr:AltKey%"
                    , nl = "%nl:AltKey%"
                    , hu = "%hu:AltKey%"
                    , sv = "%sv:AltKey%"
                    , ca = "%ca:AltKey%"
                    , br = "%br:AltKey%"
                    , mr = "%mr:AltKey%"
                    , cs = "%cs:AltKey%"
                    , fa = "%fa:AltKey%"
                    , id = "%id:AltKey%"
                    , ko = "%ko:AltKey%"
                    , pl = "%pl:AltKey%"
                    , uk = "%uk:AltKey%"
                    , nb = "%nb:AltKey%"
                    }

                EditFullscreenAction ->
                    { en = "to Edit in Fullscreen"
                    , zh_hans = "%zh_hans:EditFullscreenAction%"
                    , zh_hant = "%zh_hant:EditFullscreenAction%"
                    , es = "%es:EditFullscreenAction%"
                    , ar = "%ar:EditFullscreenAction%"
                    , fr = "%fr:EditFullscreenAction%"
                    , ru = "%ru:EditFullscreenAction%"
                    , de = "%de:EditFullscreenAction%"
                    , ja = "%ja:EditFullscreenAction%"
                    , pes = "%pes:EditFullscreenAction%"
                    , it = "%it:EditFullscreenAction%"
                    , ro = "%ro:EditFullscreenAction%"
                    , hr = "%hr:EditFullscreenAction%"
                    , nl = "%nl:EditFullscreenAction%"
                    , hu = "%hu:EditFullscreenAction%"
                    , sv = "%sv:EditFullscreenAction%"
                    , ca = "%ca:EditFullscreenAction%"
                    , br = "%br:EditFullscreenAction%"
                    , mr = "%mr:EditFullscreenAction%"
                    , cs = "%cs:EditFullscreenAction%"
                    , fa = "%fa:EditFullscreenAction%"
                    , id = "%id:EditFullscreenAction%"
                    , ko = "%ko:EditFullscreenAction%"
                    , pl = "%pl:EditFullscreenAction%"
                    , uk = "%uk:EditFullscreenAction%"
                    , nb = "%nb:EditFullscreenAction%"
                    }

                Navigate ->
                    { en = "Navigate"
                    , zh_hans = "%zh_hans:Navigate%"
                    , zh_hant = "%zh_hant:Navigate%"
                    , es = "%es:Navigate%"
                    , ar = "%ar:Navigate%"
                    , fr = "%fr:Navigate%"
                    , ru = "%ru:Navigate%"
                    , de = "%de:Navigate%"
                    , ja = "%ja:Navigate%"
                    , pes = "%pes:Navigate%"
                    , it = "%it:Navigate%"
                    , ro = "%ro:Navigate%"
                    , hr = "%hr:Navigate%"
                    , nl = "%nl:Navigate%"
                    , hu = "%hu:Navigate%"
                    , sv = "%sv:Navigate%"
                    , ca = "%ca:Navigate%"
                    , br = "%br:Navigate%"
                    , mr = "%mr:Navigate%"
                    , cs = "%cs:Navigate%"
                    , fa = "%fa:Navigate%"
                    , id = "%id:Navigate%"
                    , ko = "%ko:Navigate%"
                    , pl = "%pl:Navigate%"
                    , uk = "%uk:Navigate%"
                    , nb = "%nb:Navigate%"
                    }

                EditCardTitle ->
                    { en = "Edit Card (Enter)"
                    , zh_hans = "%zh_hans:EditCardTitle%"
                    , zh_hant = "%zh_hant:EditCardTitle%"
                    , es = "%es:EditCardTitle%"
                    , ar = "%ar:EditCardTitle%"
                    , fr = "%fr:EditCardTitle%"
                    , ru = "%ru:EditCardTitle%"
                    , de = "%de:EditCardTitle%"
                    , ja = "%ja:EditCardTitle%"
                    , pes = "%pes:EditCardTitle%"
                    , it = "%it:EditCardTitle%"
                    , ro = "%ro:EditCardTitle%"
                    , hr = "%hr:EditCardTitle%"
                    , nl = "%nl:EditCardTitle%"
                    , hu = "%hu:EditCardTitle%"
                    , sv = "%sv:EditCardTitle%"
                    , ca = "%ca:EditCardTitle%"
                    , br = "%br:EditCardTitle%"
                    , mr = "%mr:EditCardTitle%"
                    , cs = "%cs:EditCardTitle%"
                    , fa = "%fa:EditCardTitle%"
                    , id = "%id:EditCardTitle%"
                    , ko = "%ko:EditCardTitle%"
                    , pl = "%pl:EditCardTitle%"
                    , uk = "%uk:EditCardTitle%"
                    , nb = "%nb:EditCardTitle%"
                    }

                ArrowsAction ->
                    { en = "to Navigate"
                    , zh_hans = "%zh_hans:ArrowsAction%"
                    , zh_hant = "%zh_hant:ArrowsAction%"
                    , es = "%es:ArrowsAction%"
                    , ar = "%ar:ArrowsAction%"
                    , fr = "%fr:ArrowsAction%"
                    , ru = "%ru:ArrowsAction%"
                    , de = "%de:ArrowsAction%"
                    , ja = "%ja:ArrowsAction%"
                    , pes = "%pes:ArrowsAction%"
                    , it = "%it:ArrowsAction%"
                    , ro = "%ro:ArrowsAction%"
                    , hr = "%hr:ArrowsAction%"
                    , nl = "%nl:ArrowsAction%"
                    , hu = "%hu:ArrowsAction%"
                    , sv = "%sv:ArrowsAction%"
                    , ca = "%ca:ArrowsAction%"
                    , br = "%br:ArrowsAction%"
                    , mr = "%mr:ArrowsAction%"
                    , cs = "%cs:ArrowsAction%"
                    , fa = "%fa:ArrowsAction%"
                    , id = "%id:ArrowsAction%"
                    , ko = "%ko:ArrowsAction%"
                    , pl = "%pl:ArrowsAction%"
                    , uk = "%uk:ArrowsAction%"
                    , nb = "%nb:ArrowsAction%"
                    }

                AddNewCards ->
                    { en = "Add New Cards"
                    , zh_hans = "%zh_hans:AddNewCards%"
                    , zh_hant = "%zh_hant:AddNewCards%"
                    , es = "%es:AddNewCards%"
                    , ar = "%ar:AddNewCards%"
                    , fr = "%fr:AddNewCards%"
                    , ru = "%ru:AddNewCards%"
                    , de = "%de:AddNewCards%"
                    , ja = "%ja:AddNewCards%"
                    , pes = "%pes:AddNewCards%"
                    , it = "%it:AddNewCards%"
                    , ro = "%ro:AddNewCards%"
                    , hr = "%hr:AddNewCards%"
                    , nl = "%nl:AddNewCards%"
                    , hu = "%hu:AddNewCards%"
                    , sv = "%sv:AddNewCards%"
                    , ca = "%ca:AddNewCards%"
                    , br = "%br:AddNewCards%"
                    , mr = "%mr:AddNewCards%"
                    , cs = "%cs:AddNewCards%"
                    , fa = "%fa:AddNewCards%"
                    , id = "%id:AddNewCards%"
                    , ko = "%ko:AddNewCards%"
                    , pl = "%pl:AddNewCards%"
                    , uk = "%uk:AddNewCards%"
                    , nb = "%nb:AddNewCards%"
                    }

                AddChildAction ->
                    { en = "to Add Child"
                    , zh_hans = "%zh_hans:AddChildAction%"
                    , zh_hant = "%zh_hant:AddChildAction%"
                    , es = "%es:AddChildAction%"
                    , ar = "%ar:AddChildAction%"
                    , fr = "%fr:AddChildAction%"
                    , ru = "%ru:AddChildAction%"
                    , de = "%de:AddChildAction%"
                    , ja = "%ja:AddChildAction%"
                    , pes = "%pes:AddChildAction%"
                    , it = "%it:AddChildAction%"
                    , ro = "%ro:AddChildAction%"
                    , hr = "%hr:AddChildAction%"
                    , nl = "%nl:AddChildAction%"
                    , hu = "%hu:AddChildAction%"
                    , sv = "%sv:AddChildAction%"
                    , ca = "%ca:AddChildAction%"
                    , br = "%br:AddChildAction%"
                    , mr = "%mr:AddChildAction%"
                    , cs = "%cs:AddChildAction%"
                    , fa = "%fa:AddChildAction%"
                    , id = "%id:AddChildAction%"
                    , ko = "%ko:AddChildAction%"
                    , pl = "%pl:AddChildAction%"
                    , uk = "%uk:AddChildAction%"
                    , nb = "%nb:AddChildAction%"
                    }

                InsertChildTitle ->
                    { en = "Insert Child (Ctrl+L)"
                    , zh_hans = "%zh_hans:InsertChildTitle%"
                    , zh_hant = "%zh_hant:InsertChildTitle%"
                    , es = "%es:InsertChildTitle%"
                    , ar = "%ar:InsertChildTitle%"
                    , fr = "%fr:InsertChildTitle%"
                    , ru = "%ru:InsertChildTitle%"
                    , de = "%de:InsertChildTitle%"
                    , ja = "%ja:InsertChildTitle%"
                    , pes = "%pes:InsertChildTitle%"
                    , it = "%it:InsertChildTitle%"
                    , ro = "%ro:InsertChildTitle%"
                    , hr = "%hr:InsertChildTitle%"
                    , nl = "%nl:InsertChildTitle%"
                    , hu = "%hu:InsertChildTitle%"
                    , sv = "%sv:InsertChildTitle%"
                    , ca = "%ca:InsertChildTitle%"
                    , br = "%br:InsertChildTitle%"
                    , mr = "%mr:InsertChildTitle%"
                    , cs = "%cs:InsertChildTitle%"
                    , fa = "%fa:InsertChildTitle%"
                    , id = "%id:InsertChildTitle%"
                    , ko = "%ko:InsertChildTitle%"
                    , pl = "%pl:InsertChildTitle%"
                    , uk = "%uk:InsertChildTitle%"
                    , nb = "%nb:InsertChildTitle%"
                    }

                AddBelowAction ->
                    { en = "to Add Below"
                    , zh_hans = "%zh_hans:AddBelowAction%"
                    , zh_hant = "%zh_hant:AddBelowAction%"
                    , es = "%es:AddBelowAction%"
                    , ar = "%ar:AddBelowAction%"
                    , fr = "%fr:AddBelowAction%"
                    , ru = "%ru:AddBelowAction%"
                    , de = "%de:AddBelowAction%"
                    , ja = "%ja:AddBelowAction%"
                    , pes = "%pes:AddBelowAction%"
                    , it = "%it:AddBelowAction%"
                    , ro = "%ro:AddBelowAction%"
                    , hr = "%hr:AddBelowAction%"
                    , nl = "%nl:AddBelowAction%"
                    , hu = "%hu:AddBelowAction%"
                    , sv = "%sv:AddBelowAction%"
                    , ca = "%ca:AddBelowAction%"
                    , br = "%br:AddBelowAction%"
                    , mr = "%mr:AddBelowAction%"
                    , cs = "%cs:AddBelowAction%"
                    , fa = "%fa:AddBelowAction%"
                    , id = "%id:AddBelowAction%"
                    , ko = "%ko:AddBelowAction%"
                    , pl = "%pl:AddBelowAction%"
                    , uk = "%uk:AddBelowAction%"
                    , nb = "%nb:AddBelowAction%"
                    }

                InsertBelowTitle ->
                    { en = "Insert Below (Ctrl+J)"
                    , zh_hans = "%zh_hans:InsertBelowTitle%"
                    , zh_hant = "%zh_hant:InsertBelowTitle%"
                    , es = "%es:InsertBelowTitle%"
                    , ar = "%ar:InsertBelowTitle%"
                    , fr = "%fr:InsertBelowTitle%"
                    , ru = "%ru:InsertBelowTitle%"
                    , de = "%de:InsertBelowTitle%"
                    , ja = "%ja:InsertBelowTitle%"
                    , pes = "%pes:InsertBelowTitle%"
                    , it = "%it:InsertBelowTitle%"
                    , ro = "%ro:InsertBelowTitle%"
                    , hr = "%hr:InsertBelowTitle%"
                    , nl = "%nl:InsertBelowTitle%"
                    , hu = "%hu:InsertBelowTitle%"
                    , sv = "%sv:InsertBelowTitle%"
                    , ca = "%ca:InsertBelowTitle%"
                    , br = "%br:InsertBelowTitle%"
                    , mr = "%mr:InsertBelowTitle%"
                    , cs = "%cs:InsertBelowTitle%"
                    , fa = "%fa:InsertBelowTitle%"
                    , id = "%id:InsertBelowTitle%"
                    , ko = "%ko:InsertBelowTitle%"
                    , pl = "%pl:InsertBelowTitle%"
                    , uk = "%uk:InsertBelowTitle%"
                    , nb = "%nb:InsertBelowTitle%"
                    }

                AddAboveAction ->
                    { en = "to Add Above"
                    , zh_hans = "%zh_hans:AddAboveAction%"
                    , zh_hant = "%zh_hant:AddAboveAction%"
                    , es = "%es:AddAboveAction%"
                    , ar = "%ar:AddAboveAction%"
                    , fr = "%fr:AddAboveAction%"
                    , ru = "%ru:AddAboveAction%"
                    , de = "%de:AddAboveAction%"
                    , ja = "%ja:AddAboveAction%"
                    , pes = "%pes:AddAboveAction%"
                    , it = "%it:AddAboveAction%"
                    , ro = "%ro:AddAboveAction%"
                    , hr = "%hr:AddAboveAction%"
                    , nl = "%nl:AddAboveAction%"
                    , hu = "%hu:AddAboveAction%"
                    , sv = "%sv:AddAboveAction%"
                    , ca = "%ca:AddAboveAction%"
                    , br = "%br:AddAboveAction%"
                    , mr = "%mr:AddAboveAction%"
                    , cs = "%cs:AddAboveAction%"
                    , fa = "%fa:AddAboveAction%"
                    , id = "%id:AddAboveAction%"
                    , ko = "%ko:AddAboveAction%"
                    , pl = "%pl:AddAboveAction%"
                    , uk = "%uk:AddAboveAction%"
                    , nb = "%nb:AddAboveAction%"
                    }

                AIFeatures ->
                    { en = "AI Features (BETA)"
                    , zh_hans = "%zh_hans:AIFeatures%"
                    , zh_hant = "%zh_hant:AIFeatures%"
                    , es = "%es:AIFeatures%"
                    , ar = "%ar:AIFeatures%"
                    , fr = "%fr:AIFeatures%"
                    , ru = "%ru:AIFeatures%"
                    , de = "%de:AIFeatures%"
                    , ja = "%ja:AIFeatures%"
                    , pes = "%pes:AIFeatures%"
                    , it = "%it:AIFeatures%"
                    , ro = "%ro:AIFeatures%"
                    , hr = "%hr:AIFeatures%"
                    , nl = "%nl:AIFeatures%"
                    , hu = "%hu:AIFeatures%"
                    , sv = "%sv:AIFeatures%"
                    , ca = "%ca:AIFeatures%"
                    , br = "%br:AIFeatures%"
                    , mr = "%mr:AIFeatures%"
                    , cs = "%cs:AIFeatures%"
                    , fa = "%fa:AIFeatures%"
                    , id = "%id:AIFeatures%"
                    , ko = "%ko:AIFeatures%"
                    , pl = "%pl:AIFeatures%"
                    , uk = "%uk:AIFeatures%"
                    , nb = "%nb:AIFeatures%"
                    }

                ToOpenAIPrompt ->
                    { en = "to open AI Prompt"
                    , zh_hans = "%zh_hans:ToOpenAIPrompt%"
                    , zh_hant = "%zh_hant:ToOpenAIPrompt%"
                    , es = "%es:ToOpenAIPrompt%"
                    , ar = "%ar:ToOpenAIPrompt%"
                    , fr = "%fr:ToOpenAIPrompt%"
                    , ru = "%ru:ToOpenAIPrompt%"
                    , de = "%de:ToOpenAIPrompt%"
                    , ja = "%ja:ToOpenAIPrompt%"
                    , pes = "%pes:ToOpenAIPrompt%"
                    , it = "%it:ToOpenAIPrompt%"
                    , ro = "%ro:ToOpenAIPrompt%"
                    , hr = "%hr:ToOpenAIPrompt%"
                    , nl = "%nl:ToOpenAIPrompt%"
                    , hu = "%hu:ToOpenAIPrompt%"
                    , sv = "%sv:ToOpenAIPrompt%"
                    , ca = "%ca:ToOpenAIPrompt%"
                    , br = "%br:ToOpenAIPrompt%"
                    , mr = "%mr:ToOpenAIPrompt%"
                    , cs = "%cs:ToOpenAIPrompt%"
                    , fa = "%fa:ToOpenAIPrompt%"
                    , id = "%id:ToOpenAIPrompt%"
                    , ko = "%ko:ToOpenAIPrompt%"
                    , pl = "%pl:ToOpenAIPrompt%"
                    , uk = "%uk:ToOpenAIPrompt%"
                    , nb = "%nb:ToOpenAIPrompt%"
                    }

                AIGenerateChildren ->
                    { en = "Generate as Children of Current"
                    , zh_hans = "%zh_hans:AIGenerateChildren%"
                    , zh_hant = "%zh_hant:AIGenerateChildren%"
                    , es = "%es:AIGenerateChildren%"
                    , ar = "%ar:AIGenerateChildren%"
                    , fr = "%fr:AIGenerateChildren%"
                    , ru = "%ru:AIGenerateChildren%"
                    , de = "%de:AIGenerateChildren%"
                    , ja = "%ja:AIGenerateChildren%"
                    , pes = "%pes:AIGenerateChildren%"
                    , it = "%it:AIGenerateChildren%"
                    , ro = "%ro:AIGenerateChildren%"
                    , hr = "%hr:AIGenerateChildren%"
                    , nl = "%nl:AIGenerateChildren%"
                    , hu = "%hu:AIGenerateChildren%"
                    , sv = "%sv:AIGenerateChildren%"
                    , ca = "%ca:AIGenerateChildren%"
                    , br = "%br:AIGenerateChildren%"
                    , mr = "%mr:AIGenerateChildren%"
                    , cs = "%cs:AIGenerateChildren%"
                    , fa = "%fa:AIGenerateChildren%"
                    , id = "%id:AIGenerateChildren%"
                    , ko = "%ko:AIGenerateChildren%"
                    , pl = "%pl:AIGenerateChildren%"
                    , uk = "%uk:AIGenerateChildren%"
                    , nb = "%nb:AIGenerateChildren%"
                    }

                AIGenerateBelow ->
                    { en = "Generate Below Current"
                    , zh_hans = "%zh_hans:AIGenerateBelow%"
                    , zh_hant = "%zh_hant:AIGenerateBelow%"
                    , es = "%es:AIGenerateBelow%"
                    , ar = "%ar:AIGenerateBelow%"
                    , fr = "%fr:AIGenerateBelow%"
                    , ru = "%ru:AIGenerateBelow%"
                    , de = "%de:AIGenerateBelow%"
                    , ja = "%ja:AIGenerateBelow%"
                    , pes = "%pes:AIGenerateBelow%"
                    , it = "%it:AIGenerateBelow%"
                    , ro = "%ro:AIGenerateBelow%"
                    , hr = "%hr:AIGenerateBelow%"
                    , nl = "%nl:AIGenerateBelow%"
                    , hu = "%hu:AIGenerateBelow%"
                    , sv = "%sv:AIGenerateBelow%"
                    , ca = "%ca:AIGenerateBelow%"
                    , br = "%br:AIGenerateBelow%"
                    , mr = "%mr:AIGenerateBelow%"
                    , cs = "%cs:AIGenerateBelow%"
                    , fa = "%fa:AIGenerateBelow%"
                    , id = "%id:AIGenerateBelow%"
                    , ko = "%ko:AIGenerateBelow%"
                    , pl = "%pl:AIGenerateBelow%"
                    , uk = "%uk:AIGenerateBelow%"
                    , nb = "%nb:AIGenerateBelow%"
                    }

                SplitAtCursor ->
                    { en = "Split At Cursor"
                    , zh_hans = "%zh_hans:SplitAtCursor%"
                    , zh_hant = "%zh_hant:SplitAtCursor%"
                    , es = "%es:SplitAtCursor%"
                    , ar = "%ar:SplitAtCursor%"
                    , fr = "%fr:SplitAtCursor%"
                    , ru = "%ru:SplitAtCursor%"
                    , de = "%de:SplitAtCursor%"
                    , ja = "%ja:SplitAtCursor%"
                    , pes = "%pes:SplitAtCursor%"
                    , it = "%it:SplitAtCursor%"
                    , ro = "%ro:SplitAtCursor%"
                    , hr = "%hr:SplitAtCursor%"
                    , nl = "%nl:SplitAtCursor%"
                    , hu = "%hu:SplitAtCursor%"
                    , sv = "%sv:SplitAtCursor%"
                    , ca = "%ca:SplitAtCursor%"
                    , br = "%br:SplitAtCursor%"
                    , mr = "%mr:SplitAtCursor%"
                    , cs = "%cs:SplitAtCursor%"
                    , fa = "%fa:SplitAtCursor%"
                    , id = "%id:SplitAtCursor%"
                    , ko = "%ko:SplitAtCursor%"
                    , pl = "%pl:SplitAtCursor%"
                    , uk = "%uk:SplitAtCursor%"
                    , nb = "%nb:SplitAtCursor%"
                    }

                SplitChildAction ->
                    { en = "to Split Card to the Right"
                    , zh_hans = "%zh_hans:SplitChildAction%"
                    , zh_hant = "%zh_hant:SplitChildAction%"
                    , es = "%es:SplitChildAction%"
                    , ar = "%ar:SplitChildAction%"
                    , fr = "%fr:SplitChildAction%"
                    , ru = "%ru:SplitChildAction%"
                    , de = "%de:SplitChildAction%"
                    , ja = "%ja:SplitChildAction%"
                    , pes = "%pes:SplitChildAction%"
                    , it = "%it:SplitChildAction%"
                    , ro = "%ro:SplitChildAction%"
                    , hr = "%hr:SplitChildAction%"
                    , nl = "%nl:SplitChildAction%"
                    , hu = "%hu:SplitChildAction%"
                    , sv = "%sv:SplitChildAction%"
                    , ca = "%ca:SplitChildAction%"
                    , br = "%br:SplitChildAction%"
                    , mr = "%mr:SplitChildAction%"
                    , cs = "%cs:SplitChildAction%"
                    , fa = "%fa:SplitChildAction%"
                    , id = "%id:SplitChildAction%"
                    , ko = "%ko:SplitChildAction%"
                    , pl = "%pl:SplitChildAction%"
                    , uk = "%uk:SplitChildAction%"
                    , nb = "%nb:SplitChildAction%"
                    }

                SplitBelowAction ->
                    { en = "to Split Card Down"
                    , zh_hans = "%zh_hans:SplitBelowAction%"
                    , zh_hant = "%zh_hant:SplitBelowAction%"
                    , es = "%es:SplitBelowAction%"
                    , ar = "%ar:SplitBelowAction%"
                    , fr = "%fr:SplitBelowAction%"
                    , ru = "%ru:SplitBelowAction%"
                    , de = "%de:SplitBelowAction%"
                    , ja = "%ja:SplitBelowAction%"
                    , pes = "%pes:SplitBelowAction%"
                    , it = "%it:SplitBelowAction%"
                    , ro = "%ro:SplitBelowAction%"
                    , hr = "%hr:SplitBelowAction%"
                    , nl = "%nl:SplitBelowAction%"
                    , hu = "%hu:SplitBelowAction%"
                    , sv = "%sv:SplitBelowAction%"
                    , ca = "%ca:SplitBelowAction%"
                    , br = "%br:SplitBelowAction%"
                    , mr = "%mr:SplitBelowAction%"
                    , cs = "%cs:SplitBelowAction%"
                    , fa = "%fa:SplitBelowAction%"
                    , id = "%id:SplitBelowAction%"
                    , ko = "%ko:SplitBelowAction%"
                    , pl = "%pl:SplitBelowAction%"
                    , uk = "%uk:SplitBelowAction%"
                    , nb = "%nb:SplitBelowAction%"
                    }

                SplitUpwardAction ->
                    { en = "to Split Card Upward"
                    , zh_hans = "%zh_hans:SplitUpwardAction%"
                    , zh_hant = "%zh_hant:SplitUpwardAction%"
                    , es = "%es:SplitUpwardAction%"
                    , ar = "%ar:SplitUpwardAction%"
                    , fr = "%fr:SplitUpwardAction%"
                    , ru = "%ru:SplitUpwardAction%"
                    , de = "%de:SplitUpwardAction%"
                    , ja = "%ja:SplitUpwardAction%"
                    , pes = "%pes:SplitUpwardAction%"
                    , it = "%it:SplitUpwardAction%"
                    , ro = "%ro:SplitUpwardAction%"
                    , hr = "%hr:SplitUpwardAction%"
                    , nl = "%nl:SplitUpwardAction%"
                    , hu = "%hu:SplitUpwardAction%"
                    , sv = "%sv:SplitUpwardAction%"
                    , ca = "%ca:SplitUpwardAction%"
                    , br = "%br:SplitUpwardAction%"
                    , mr = "%mr:SplitUpwardAction%"
                    , cs = "%cs:SplitUpwardAction%"
                    , fa = "%fa:SplitUpwardAction%"
                    , id = "%id:SplitUpwardAction%"
                    , ko = "%ko:SplitUpwardAction%"
                    , pl = "%pl:SplitUpwardAction%"
                    , uk = "%uk:SplitUpwardAction%"
                    , nb = "%nb:SplitUpwardAction%"
                    }

                MergeCards ->
                    { en = "Merge Cards"
                    , zh_hans = "%zh_hans:MergeCards%"
                    , zh_hant = "%zh_hant:MergeCards%"
                    , es = "%es:MergeCards%"
                    , ar = "%ar:MergeCards%"
                    , fr = "%fr:MergeCards%"
                    , ru = "%ru:MergeCards%"
                    , de = "%de:MergeCards%"
                    , ja = "%ja:MergeCards%"
                    , pes = "%pes:MergeCards%"
                    , it = "%it:MergeCards%"
                    , ro = "%ro:MergeCards%"
                    , hr = "%hr:MergeCards%"
                    , nl = "%nl:MergeCards%"
                    , hu = "%hu:MergeCards%"
                    , sv = "%sv:MergeCards%"
                    , ca = "%ca:MergeCards%"
                    , br = "%br:MergeCards%"
                    , mr = "%mr:MergeCards%"
                    , cs = "%cs:MergeCards%"
                    , fa = "%fa:MergeCards%"
                    , id = "%id:MergeCards%"
                    , ko = "%ko:MergeCards%"
                    , pl = "%pl:MergeCards%"
                    , uk = "%uk:MergeCards%"
                    , nb = "%nb:MergeCards%"
                    }

                MergeDownAction ->
                    { en = "to Merge into Next"
                    , zh_hans = "%zh_hans:MergeDownAction%"
                    , zh_hant = "%zh_hant:MergeDownAction%"
                    , es = "%es:MergeDownAction%"
                    , ar = "%ar:MergeDownAction%"
                    , fr = "%fr:MergeDownAction%"
                    , ru = "%ru:MergeDownAction%"
                    , de = "%de:MergeDownAction%"
                    , ja = "%ja:MergeDownAction%"
                    , pes = "%pes:MergeDownAction%"
                    , it = "%it:MergeDownAction%"
                    , ro = "%ro:MergeDownAction%"
                    , hr = "%hr:MergeDownAction%"
                    , nl = "%nl:MergeDownAction%"
                    , hu = "%hu:MergeDownAction%"
                    , sv = "%sv:MergeDownAction%"
                    , ca = "%ca:MergeDownAction%"
                    , br = "%br:MergeDownAction%"
                    , mr = "%mr:MergeDownAction%"
                    , cs = "%cs:MergeDownAction%"
                    , fa = "%fa:MergeDownAction%"
                    , id = "%id:MergeDownAction%"
                    , ko = "%ko:MergeDownAction%"
                    , pl = "%pl:MergeDownAction%"
                    , uk = "%uk:MergeDownAction%"
                    , nb = "%nb:MergeDownAction%"
                    }

                MergeUpAction ->
                    { en = "to Merge into Previous"
                    , zh_hans = "%zh_hans:MergeUpAction%"
                    , zh_hant = "%zh_hant:MergeUpAction%"
                    , es = "%es:MergeUpAction%"
                    , ar = "%ar:MergeUpAction%"
                    , fr = "%fr:MergeUpAction%"
                    , ru = "%ru:MergeUpAction%"
                    , de = "%de:MergeUpAction%"
                    , ja = "%ja:MergeUpAction%"
                    , pes = "%pes:MergeUpAction%"
                    , it = "%it:MergeUpAction%"
                    , ro = "%ro:MergeUpAction%"
                    , hr = "%hr:MergeUpAction%"
                    , nl = "%nl:MergeUpAction%"
                    , hu = "%hu:MergeUpAction%"
                    , sv = "%sv:MergeUpAction%"
                    , ca = "%ca:MergeUpAction%"
                    , br = "%br:MergeUpAction%"
                    , mr = "%mr:MergeUpAction%"
                    , cs = "%cs:MergeUpAction%"
                    , fa = "%fa:MergeUpAction%"
                    , id = "%id:MergeUpAction%"
                    , ko = "%ko:MergeUpAction%"
                    , pl = "%pl:MergeUpAction%"
                    , uk = "%uk:MergeUpAction%"
                    , nb = "%nb:MergeUpAction%"
                    }

                InsertAboveTitle ->
                    { en = "Insert Above (Ctrl+K)"
                    , zh_hans = "%zh_hans:InsertAboveTitle%"
                    , zh_hant = "%zh_hant:InsertAboveTitle%"
                    , es = "%es:InsertAboveTitle%"
                    , ar = "%ar:InsertAboveTitle%"
                    , fr = "%fr:InsertAboveTitle%"
                    , ru = "%ru:InsertAboveTitle%"
                    , de = "%de:InsertAboveTitle%"
                    , ja = "%ja:InsertAboveTitle%"
                    , pes = "%pes:InsertAboveTitle%"
                    , it = "%it:InsertAboveTitle%"
                    , ro = "%ro:InsertAboveTitle%"
                    , hr = "%hr:InsertAboveTitle%"
                    , nl = "%nl:InsertAboveTitle%"
                    , hu = "%hu:InsertAboveTitle%"
                    , sv = "%sv:InsertAboveTitle%"
                    , ca = "%ca:InsertAboveTitle%"
                    , br = "%br:InsertAboveTitle%"
                    , mr = "%mr:InsertAboveTitle%"
                    , cs = "%cs:InsertAboveTitle%"
                    , fa = "%fa:InsertAboveTitle%"
                    , id = "%id:InsertAboveTitle%"
                    , ko = "%ko:InsertAboveTitle%"
                    , pl = "%pl:InsertAboveTitle%"
                    , uk = "%uk:InsertAboveTitle%"
                    , nb = "%nb:InsertAboveTitle%"
                    }

                ArrowKeys ->
                    { en = "(arrows)"
                    , zh_hans = "%zh_hans:ArrowKeys%"
                    , zh_hant = "%zh_hant:ArrowKeys%"
                    , es = "%es:ArrowKeys%"
                    , ar = "%ar:ArrowKeys%"
                    , fr = "%fr:ArrowKeys%"
                    , ru = "%ru:ArrowKeys%"
                    , de = "%de:ArrowKeys%"
                    , ja = "%ja:ArrowKeys%"
                    , pes = "%pes:ArrowKeys%"
                    , it = "%it:ArrowKeys%"
                    , ro = "%ro:ArrowKeys%"
                    , hr = "%hr:ArrowKeys%"
                    , nl = "%nl:ArrowKeys%"
                    , hu = "%hu:ArrowKeys%"
                    , sv = "%sv:ArrowKeys%"
                    , ca = "%ca:ArrowKeys%"
                    , br = "%br:ArrowKeys%"
                    , mr = "%mr:ArrowKeys%"
                    , cs = "%cs:ArrowKeys%"
                    , fa = "%fa:ArrowKeys%"
                    , id = "%id:ArrowKeys%"
                    , ko = "%ko:ArrowKeys%"
                    , pl = "%pl:ArrowKeys%"
                    , uk = "%uk:ArrowKeys%"
                    , nb = "%nb:ArrowKeys%"
                    }

                MoveAndDelete ->
                    { en = "Move & Delete Cards"
                    , zh_hans = "%zh_hans:MoveAndDelete%"
                    , zh_hant = "%zh_hant:MoveAndDelete%"
                    , es = "%es:MoveAndDelete%"
                    , ar = "%ar:MoveAndDelete%"
                    , fr = "%fr:MoveAndDelete%"
                    , ru = "%ru:MoveAndDelete%"
                    , de = "%de:MoveAndDelete%"
                    , ja = "%ja:MoveAndDelete%"
                    , pes = "%pes:MoveAndDelete%"
                    , it = "%it:MoveAndDelete%"
                    , ro = "%ro:MoveAndDelete%"
                    , hr = "%hr:MoveAndDelete%"
                    , nl = "%nl:MoveAndDelete%"
                    , hu = "%hu:MoveAndDelete%"
                    , sv = "%sv:MoveAndDelete%"
                    , ca = "%ca:MoveAndDelete%"
                    , br = "%br:MoveAndDelete%"
                    , mr = "%mr:MoveAndDelete%"
                    , cs = "%cs:MoveAndDelete%"
                    , fa = "%fa:MoveAndDelete%"
                    , id = "%id:MoveAndDelete%"
                    , ko = "%ko:MoveAndDelete%"
                    , pl = "%pl:MoveAndDelete%"
                    , uk = "%uk:MoveAndDelete%"
                    , nb = "%nb:MoveAndDelete%"
                    }

                MoveAction ->
                    { en = "to Move"
                    , zh_hans = "%zh_hans:MoveAction%"
                    , zh_hant = "%zh_hant:MoveAction%"
                    , es = "%es:MoveAction%"
                    , ar = "%ar:MoveAction%"
                    , fr = "%fr:MoveAction%"
                    , ru = "%ru:MoveAction%"
                    , de = "%de:MoveAction%"
                    , ja = "%ja:MoveAction%"
                    , pes = "%pes:MoveAction%"
                    , it = "%it:MoveAction%"
                    , ro = "%ro:MoveAction%"
                    , hr = "%hr:MoveAction%"
                    , nl = "%nl:MoveAction%"
                    , hu = "%hu:MoveAction%"
                    , sv = "%sv:MoveAction%"
                    , ca = "%ca:MoveAction%"
                    , br = "%br:MoveAction%"
                    , mr = "%mr:MoveAction%"
                    , cs = "%cs:MoveAction%"
                    , fa = "%fa:MoveAction%"
                    , id = "%id:MoveAction%"
                    , ko = "%ko:MoveAction%"
                    , pl = "%pl:MoveAction%"
                    , uk = "%uk:MoveAction%"
                    , nb = "%nb:MoveAction%"
                    }

                Backspace ->
                    { en = "Backspace"
                    , zh_hans = "%zh_hans:Backspace%"
                    , zh_hant = "%zh_hant:Backspace%"
                    , es = "%es:Backspace%"
                    , ar = "%ar:Backspace%"
                    , fr = "%fr:Backspace%"
                    , ru = "%ru:Backspace%"
                    , de = "%de:Backspace%"
                    , ja = "%ja:Backspace%"
                    , pes = "%pes:Backspace%"
                    , it = "%it:Backspace%"
                    , ro = "%ro:Backspace%"
                    , hr = "%hr:Backspace%"
                    , nl = "%nl:Backspace%"
                    , hu = "%hu:Backspace%"
                    , sv = "%sv:Backspace%"
                    , ca = "%ca:Backspace%"
                    , br = "%br:Backspace%"
                    , mr = "%mr:Backspace%"
                    , cs = "%cs:Backspace%"
                    , fa = "%fa:Backspace%"
                    , id = "%id:Backspace%"
                    , ko = "%ko:Backspace%"
                    , pl = "%pl:Backspace%"
                    , uk = "%uk:Backspace%"
                    , nb = "%nb:Backspace%"
                    }

                DeleteAction ->
                    { en = "to Delete"
                    , zh_hans = "%zh_hans:DeleteAction%"
                    , zh_hant = "%zh_hant:DeleteAction%"
                    , es = "%es:DeleteAction%"
                    , ar = "%ar:DeleteAction%"
                    , fr = "%fr:DeleteAction%"
                    , ru = "%ru:DeleteAction%"
                    , de = "%de:DeleteAction%"
                    , ja = "%ja:DeleteAction%"
                    , pes = "%pes:DeleteAction%"
                    , it = "%it:DeleteAction%"
                    , ro = "%ro:DeleteAction%"
                    , hr = "%hr:DeleteAction%"
                    , nl = "%nl:DeleteAction%"
                    , hu = "%hu:DeleteAction%"
                    , sv = "%sv:DeleteAction%"
                    , ca = "%ca:DeleteAction%"
                    , br = "%br:DeleteAction%"
                    , mr = "%mr:DeleteAction%"
                    , cs = "%cs:DeleteAction%"
                    , fa = "%fa:DeleteAction%"
                    , id = "%id:DeleteAction%"
                    , ko = "%ko:DeleteAction%"
                    , pl = "%pl:DeleteAction%"
                    , uk = "%uk:DeleteAction%"
                    , nb = "%nb:DeleteAction%"
                    }

                DeleteCardTitle ->
                    { en = "Delete Card (Ctrl+Backspace)"
                    , zh_hans = "%zh_hans:DeleteCardTitle%"
                    , zh_hant = "%zh_hant:DeleteCardTitle%"
                    , es = "%es:DeleteCardTitle%"
                    , ar = "%ar:DeleteCardTitle%"
                    , fr = "%fr:DeleteCardTitle%"
                    , ru = "%ru:DeleteCardTitle%"
                    , de = "%de:DeleteCardTitle%"
                    , ja = "%ja:DeleteCardTitle%"
                    , pes = "%pes:DeleteCardTitle%"
                    , it = "%it:DeleteCardTitle%"
                    , ro = "%ro:DeleteCardTitle%"
                    , hr = "%hr:DeleteCardTitle%"
                    , nl = "%nl:DeleteCardTitle%"
                    , hu = "%hu:DeleteCardTitle%"
                    , sv = "%sv:DeleteCardTitle%"
                    , ca = "%ca:DeleteCardTitle%"
                    , br = "%br:DeleteCardTitle%"
                    , mr = "%mr:DeleteCardTitle%"
                    , cs = "%cs:DeleteCardTitle%"
                    , fa = "%fa:DeleteCardTitle%"
                    , id = "%id:DeleteCardTitle%"
                    , ko = "%ko:DeleteCardTitle%"
                    , pl = "%pl:DeleteCardTitle%"
                    , uk = "%uk:DeleteCardTitle%"
                    , nb = "%nb:DeleteCardTitle%"
                    }

                FormattingGuide ->
                    { en = "More Formatting Options..."
                    , zh_hans = "%zh_hans:FormattingGuide%"
                    , zh_hant = "%zh_hant:FormattingGuide%"
                    , es = "%es:FormattingGuide%"
                    , ar = "%ar:FormattingGuide%"
                    , fr = "%fr:FormattingGuide%"
                    , ru = "%ru:FormattingGuide%"
                    , de = "%de:FormattingGuide%"
                    , ja = "%ja:FormattingGuide%"
                    , pes = "%pes:FormattingGuide%"
                    , it = "%it:FormattingGuide%"
                    , ro = "%ro:FormattingGuide%"
                    , hr = "%hr:FormattingGuide%"
                    , nl = "%nl:FormattingGuide%"
                    , hu = "%hu:FormattingGuide%"
                    , sv = "%sv:FormattingGuide%"
                    , ca = "%ca:FormattingGuide%"
                    , br = "%br:FormattingGuide%"
                    , mr = "%mr:FormattingGuide%"
                    , cs = "%cs:FormattingGuide%"
                    , fa = "%fa:FormattingGuide%"
                    , id = "%id:FormattingGuide%"
                    , ko = "%ko:FormattingGuide%"
                    , pl = "%pl:FormattingGuide%"
                    , uk = "%uk:FormattingGuide%"
                    , nb = "%nb:FormattingGuide%"
                    }

                ForBold ->
                    { en = "for Bold"
                    , zh_hans = "%zh_hans:ForBold%"
                    , zh_hant = "%zh_hant:ForBold%"
                    , es = "%es:ForBold%"
                    , ar = "%ar:ForBold%"
                    , fr = "%fr:ForBold%"
                    , ru = "%ru:ForBold%"
                    , de = "%de:ForBold%"
                    , ja = "%ja:ForBold%"
                    , pes = "%pes:ForBold%"
                    , it = "%it:ForBold%"
                    , ro = "%ro:ForBold%"
                    , hr = "%hr:ForBold%"
                    , nl = "%nl:ForBold%"
                    , hu = "%hu:ForBold%"
                    , sv = "%sv:ForBold%"
                    , ca = "%ca:ForBold%"
                    , br = "%br:ForBold%"
                    , mr = "%mr:ForBold%"
                    , cs = "%cs:ForBold%"
                    , fa = "%fa:ForBold%"
                    , id = "%id:ForBold%"
                    , ko = "%ko:ForBold%"
                    , pl = "%pl:ForBold%"
                    , uk = "%uk:ForBold%"
                    , nb = "%nb:ForBold%"
                    }

                BoldSelection ->
                    { en = "Bold selection"
                    , zh_hans = "%zh_hans:BoldSelection%"
                    , zh_hant = "%zh_hant:BoldSelection%"
                    , es = "%es:BoldSelection%"
                    , ar = "%ar:BoldSelection%"
                    , fr = "%fr:BoldSelection%"
                    , ru = "%ru:BoldSelection%"
                    , de = "%de:BoldSelection%"
                    , ja = "%ja:BoldSelection%"
                    , pes = "%pes:BoldSelection%"
                    , it = "%it:BoldSelection%"
                    , ro = "%ro:BoldSelection%"
                    , hr = "%hr:BoldSelection%"
                    , nl = "%nl:BoldSelection%"
                    , hu = "%hu:BoldSelection%"
                    , sv = "%sv:BoldSelection%"
                    , ca = "%ca:BoldSelection%"
                    , br = "%br:BoldSelection%"
                    , mr = "%mr:BoldSelection%"
                    , cs = "%cs:BoldSelection%"
                    , fa = "%fa:BoldSelection%"
                    , id = "%id:BoldSelection%"
                    , ko = "%ko:BoldSelection%"
                    , pl = "%pl:BoldSelection%"
                    , uk = "%uk:BoldSelection%"
                    , nb = "%nb:BoldSelection%"
                    }

                ForItalic ->
                    { en = "for Italic"
                    , zh_hans = "%zh_hans:ForItalic%"
                    , zh_hant = "%zh_hant:ForItalic%"
                    , es = "%es:ForItalic%"
                    , ar = "%ar:ForItalic%"
                    , fr = "%fr:ForItalic%"
                    , ru = "%ru:ForItalic%"
                    , de = "%de:ForItalic%"
                    , ja = "%ja:ForItalic%"
                    , pes = "%pes:ForItalic%"
                    , it = "%it:ForItalic%"
                    , ro = "%ro:ForItalic%"
                    , hr = "%hr:ForItalic%"
                    , nl = "%nl:ForItalic%"
                    , hu = "%hu:ForItalic%"
                    , sv = "%sv:ForItalic%"
                    , ca = "%ca:ForItalic%"
                    , br = "%br:ForItalic%"
                    , mr = "%mr:ForItalic%"
                    , cs = "%cs:ForItalic%"
                    , fa = "%fa:ForItalic%"
                    , id = "%id:ForItalic%"
                    , ko = "%ko:ForItalic%"
                    , pl = "%pl:ForItalic%"
                    , uk = "%uk:ForItalic%"
                    , nb = "%nb:ForItalic%"
                    }

                ForInsertLink ->
                    { en = "to Insert Link"
                    , zh_hans = "%zh_hans:ForInsertLink%"
                    , zh_hant = "%zh_hant:ForInsertLink%"
                    , es = "%es:ForInsertLink%"
                    , ar = "%ar:ForInsertLink%"
                    , fr = "%fr:ForInsertLink%"
                    , ru = "%ru:ForInsertLink%"
                    , de = "%de:ForInsertLink%"
                    , ja = "%ja:ForInsertLink%"
                    , pes = "%pes:ForInsertLink%"
                    , it = "%it:ForInsertLink%"
                    , ro = "%ro:ForInsertLink%"
                    , hr = "%hr:ForInsertLink%"
                    , nl = "%nl:ForInsertLink%"
                    , hu = "%hu:ForInsertLink%"
                    , sv = "%sv:ForInsertLink%"
                    , ca = "%ca:ForInsertLink%"
                    , br = "%br:ForInsertLink%"
                    , mr = "%mr:ForInsertLink%"
                    , cs = "%cs:ForInsertLink%"
                    , fa = "%fa:ForInsertLink%"
                    , id = "%id:ForInsertLink%"
                    , ko = "%ko:ForInsertLink%"
                    , pl = "%pl:ForInsertLink%"
                    , uk = "%uk:ForInsertLink%"
                    , nb = "%nb:ForInsertLink%"
                    }

                ItalicizeSelection ->
                    { en = "Italicize selection"
                    , zh_hans = "%zh_hans:ItalicizeSelection%"
                    , zh_hant = "%zh_hant:ItalicizeSelection%"
                    , es = "%es:ItalicizeSelection%"
                    , ar = "%ar:ItalicizeSelection%"
                    , fr = "%fr:ItalicizeSelection%"
                    , ru = "%ru:ItalicizeSelection%"
                    , de = "%de:ItalicizeSelection%"
                    , ja = "%ja:ItalicizeSelection%"
                    , pes = "%pes:ItalicizeSelection%"
                    , it = "%it:ItalicizeSelection%"
                    , ro = "%ro:ItalicizeSelection%"
                    , hr = "%hr:ItalicizeSelection%"
                    , nl = "%nl:ItalicizeSelection%"
                    , hu = "%hu:ItalicizeSelection%"
                    , sv = "%sv:ItalicizeSelection%"
                    , ca = "%ca:ItalicizeSelection%"
                    , br = "%br:ItalicizeSelection%"
                    , mr = "%mr:ItalicizeSelection%"
                    , cs = "%cs:ItalicizeSelection%"
                    , fa = "%fa:ItalicizeSelection%"
                    , id = "%id:ItalicizeSelection%"
                    , ko = "%ko:ItalicizeSelection%"
                    , pl = "%pl:ItalicizeSelection%"
                    , uk = "%uk:ItalicizeSelection%"
                    , nb = "%nb:ItalicizeSelection%"
                    }

                InsertLink ->
                    { en = "Insert Link"
                    , zh_hans = "%zh_hans:InsertLink%"
                    , zh_hant = "%zh_hant:InsertLink%"
                    , es = "%es:InsertLink%"
                    , ar = "%ar:InsertLink%"
                    , fr = "%fr:InsertLink%"
                    , ru = "%ru:InsertLink%"
                    , de = "%de:InsertLink%"
                    , ja = "%ja:InsertLink%"
                    , pes = "%pes:InsertLink%"
                    , it = "%it:InsertLink%"
                    , ro = "%ro:InsertLink%"
                    , hr = "%hr:InsertLink%"
                    , nl = "%nl:InsertLink%"
                    , hu = "%hu:InsertLink%"
                    , sv = "%sv:InsertLink%"
                    , ca = "%ca:InsertLink%"
                    , br = "%br:InsertLink%"
                    , mr = "%mr:InsertLink%"
                    , cs = "%cs:InsertLink%"
                    , fa = "%fa:InsertLink%"
                    , id = "%id:InsertLink%"
                    , ko = "%ko:InsertLink%"
                    , pl = "%pl:InsertLink%"
                    , uk = "%uk:InsertLink%"
                    , nb = "%nb:InsertLink%"
                    }

                SaveChanges ->
                    { en = "Save changes"
                    , zh_hans = "%zh_hans:SaveChanges%"
                    , zh_hant = "%zh_hant:SaveChanges%"
                    , es = "%es:SaveChanges%"
                    , ar = "%ar:SaveChanges%"
                    , fr = "%fr:SaveChanges%"
                    , ru = "%ru:SaveChanges%"
                    , de = "%de:SaveChanges%"
                    , ja = "%ja:SaveChanges%"
                    , pes = "%pes:SaveChanges%"
                    , it = "%it:SaveChanges%"
                    , ro = "%ro:SaveChanges%"
                    , hr = "%hr:SaveChanges%"
                    , nl = "%nl:SaveChanges%"
                    , hu = "%hu:SaveChanges%"
                    , sv = "%sv:SaveChanges%"
                    , ca = "%ca:SaveChanges%"
                    , br = "%br:SaveChanges%"
                    , mr = "%mr:SaveChanges%"
                    , cs = "%cs:SaveChanges%"
                    , fa = "%fa:SaveChanges%"
                    , id = "%id:SaveChanges%"
                    , ko = "%ko:SaveChanges%"
                    , pl = "%pl:SaveChanges%"
                    , uk = "%uk:SaveChanges%"
                    , nb = "%nb:SaveChanges%"
                    }

                SaveChangesAndExit ->
                    { en = "Save changes and exit card"
                    , zh_hans = "%zh_hans:SaveChangesAndExit%"
                    , zh_hant = "%zh_hant:SaveChangesAndExit%"
                    , es = "%es:SaveChangesAndExit%"
                    , ar = "%ar:SaveChangesAndExit%"
                    , fr = "%fr:SaveChangesAndExit%"
                    , ru = "%ru:SaveChangesAndExit%"
                    , de = "%de:SaveChangesAndExit%"
                    , ja = "%ja:SaveChangesAndExit%"
                    , pes = "%pes:SaveChangesAndExit%"
                    , it = "%it:SaveChangesAndExit%"
                    , ro = "%ro:SaveChangesAndExit%"
                    , hr = "%hr:SaveChangesAndExit%"
                    , nl = "%nl:SaveChangesAndExit%"
                    , hu = "%hu:SaveChangesAndExit%"
                    , sv = "%sv:SaveChangesAndExit%"
                    , ca = "%ca:SaveChangesAndExit%"
                    , br = "%br:SaveChangesAndExit%"
                    , mr = "%mr:SaveChangesAndExit%"
                    , cs = "%cs:SaveChangesAndExit%"
                    , fa = "%fa:SaveChangesAndExit%"
                    , id = "%id:SaveChangesAndExit%"
                    , ko = "%ko:SaveChangesAndExit%"
                    , pl = "%pl:SaveChangesAndExit%"
                    , uk = "%uk:SaveChangesAndExit%"
                    , nb = "%nb:SaveChangesAndExit%"
                    }

                ExitEditMode ->
                    { en = "Exit edit mode"
                    , zh_hans = "%zh_hans:ExitEditMode%"
                    , zh_hant = "%zh_hant:ExitEditMode%"
                    , es = "%es:ExitEditMode%"
                    , ar = "%ar:ExitEditMode%"
                    , fr = "%fr:ExitEditMode%"
                    , ru = "%ru:ExitEditMode%"
                    , de = "%de:ExitEditMode%"
                    , ja = "%ja:ExitEditMode%"
                    , pes = "%pes:ExitEditMode%"
                    , it = "%it:ExitEditMode%"
                    , ro = "%ro:ExitEditMode%"
                    , hr = "%hr:ExitEditMode%"
                    , nl = "%nl:ExitEditMode%"
                    , hu = "%hu:ExitEditMode%"
                    , sv = "%sv:ExitEditMode%"
                    , ca = "%ca:ExitEditMode%"
                    , br = "%br:ExitEditMode%"
                    , mr = "%mr:ExitEditMode%"
                    , cs = "%cs:ExitEditMode%"
                    , fa = "%fa:ExitEditMode%"
                    , id = "%id:ExitEditMode%"
                    , ko = "%ko:ExitEditMode%"
                    , pl = "%pl:ExitEditMode%"
                    , uk = "%uk:ExitEditMode%"
                    , nb = "%nb:ExitEditMode%"
                    }

                ToSaveChanges ->
                    { en = "to Save Changes"
                    , zh_hans = "%zh_hans:ToSaveChanges%"
                    , zh_hant = "%zh_hant:ToSaveChanges%"
                    , es = "%es:ToSaveChanges%"
                    , ar = "%ar:ToSaveChanges%"
                    , fr = "%fr:ToSaveChanges%"
                    , ru = "%ru:ToSaveChanges%"
                    , de = "%de:ToSaveChanges%"
                    , ja = "%ja:ToSaveChanges%"
                    , pes = "%pes:ToSaveChanges%"
                    , it = "%it:ToSaveChanges%"
                    , ro = "%ro:ToSaveChanges%"
                    , hr = "%hr:ToSaveChanges%"
                    , nl = "%nl:ToSaveChanges%"
                    , hu = "%hu:ToSaveChanges%"
                    , sv = "%sv:ToSaveChanges%"
                    , ca = "%ca:ToSaveChanges%"
                    , br = "%br:ToSaveChanges%"
                    , mr = "%mr:ToSaveChanges%"
                    , cs = "%cs:ToSaveChanges%"
                    , fa = "%fa:ToSaveChanges%"
                    , id = "%id:ToSaveChanges%"
                    , ko = "%ko:ToSaveChanges%"
                    , pl = "%pl:ToSaveChanges%"
                    , uk = "%uk:ToSaveChanges%"
                    , nb = "%nb:ToSaveChanges%"
                    }

                SaveChangesTitle ->
                    { en = "Save Changes (Ctrl+Enter)"
                    , zh_hans = "%zh_hans:SaveChangesTitle%"
                    , zh_hant = "%zh_hant:SaveChangesTitle%"
                    , es = "%es:SaveChangesTitle%"
                    , ar = "%ar:SaveChangesTitle%"
                    , fr = "%fr:SaveChangesTitle%"
                    , ru = "%ru:SaveChangesTitle%"
                    , de = "%de:SaveChangesTitle%"
                    , ja = "%ja:SaveChangesTitle%"
                    , pes = "%pes:SaveChangesTitle%"
                    , it = "%it:SaveChangesTitle%"
                    , ro = "%ro:SaveChangesTitle%"
                    , hr = "%hr:SaveChangesTitle%"
                    , nl = "%nl:SaveChangesTitle%"
                    , hu = "%hu:SaveChangesTitle%"
                    , sv = "%sv:SaveChangesTitle%"
                    , ca = "%ca:SaveChangesTitle%"
                    , br = "%br:SaveChangesTitle%"
                    , mr = "%mr:SaveChangesTitle%"
                    , cs = "%cs:SaveChangesTitle%"
                    , fa = "%fa:SaveChangesTitle%"
                    , id = "%id:SaveChangesTitle%"
                    , ko = "%ko:SaveChangesTitle%"
                    , pl = "%pl:SaveChangesTitle%"
                    , uk = "%uk:SaveChangesTitle%"
                    , nb = "%nb:SaveChangesTitle%"
                    }

                EscKey ->
                    { en = "Esc"
                    , zh_hans = "%zh_hans:EscKey%"
                    , zh_hant = "%zh_hant:EscKey%"
                    , es = "%es:EscKey%"
                    , ar = "%ar:EscKey%"
                    , fr = "%fr:EscKey%"
                    , ru = "%ru:EscKey%"
                    , de = "%de:EscKey%"
                    , ja = "%ja:EscKey%"
                    , pes = "%pes:EscKey%"
                    , it = "%it:EscKey%"
                    , ro = "%ro:EscKey%"
                    , hr = "%hr:EscKey%"
                    , nl = "%nl:EscKey%"
                    , hu = "%hu:EscKey%"
                    , sv = "%sv:EscKey%"
                    , ca = "%ca:EscKey%"
                    , br = "%br:EscKey%"
                    , mr = "%mr:EscKey%"
                    , cs = "%cs:EscKey%"
                    , fa = "%fa:EscKey%"
                    , id = "%id:EscKey%"
                    , ko = "%ko:EscKey%"
                    , pl = "%pl:EscKey%"
                    , uk = "%uk:EscKey%"
                    , nb = "%nb:EscKey%"
                    }

                OtherShortcuts ->
                    { en = "Other Shortcuts"
                    , zh_hans = "%zh_hans:OtherShortcuts%"
                    , zh_hant = "%zh_hant:OtherShortcuts%"
                    , es = "%es:OtherShortcuts%"
                    , ar = "%ar:OtherShortcuts%"
                    , fr = "%fr:OtherShortcuts%"
                    , ru = "%ru:OtherShortcuts%"
                    , de = "%de:OtherShortcuts%"
                    , ja = "%ja:OtherShortcuts%"
                    , pes = "%pes:OtherShortcuts%"
                    , it = "%it:OtherShortcuts%"
                    , ro = "%ro:OtherShortcuts%"
                    , hr = "%hr:OtherShortcuts%"
                    , nl = "%nl:OtherShortcuts%"
                    , hu = "%hu:OtherShortcuts%"
                    , sv = "%sv:OtherShortcuts%"
                    , ca = "%ca:OtherShortcuts%"
                    , br = "%br:OtherShortcuts%"
                    , mr = "%mr:OtherShortcuts%"
                    , cs = "%cs:OtherShortcuts%"
                    , fa = "%fa:OtherShortcuts%"
                    , id = "%id:OtherShortcuts%"
                    , ko = "%ko:OtherShortcuts%"
                    , pl = "%pl:OtherShortcuts%"
                    , uk = "%uk:OtherShortcuts%"
                    , nb = "%nb:OtherShortcuts%"
                    }

                DisplayWordCounts ->
                    { en = "Display word counts"
                    , zh_hans = "%zh_hans:DisplayWordCounts%"
                    , zh_hant = "%zh_hant:DisplayWordCounts%"
                    , es = "%es:DisplayWordCounts%"
                    , ar = "%ar:DisplayWordCounts%"
                    , fr = "%fr:DisplayWordCounts%"
                    , ru = "%ru:DisplayWordCounts%"
                    , de = "%de:DisplayWordCounts%"
                    , ja = "%ja:DisplayWordCounts%"
                    , pes = "%pes:DisplayWordCounts%"
                    , it = "%it:DisplayWordCounts%"
                    , ro = "%ro:DisplayWordCounts%"
                    , hr = "%hr:DisplayWordCounts%"
                    , nl = "%nl:DisplayWordCounts%"
                    , hu = "%hu:DisplayWordCounts%"
                    , sv = "%sv:DisplayWordCounts%"
                    , ca = "%ca:DisplayWordCounts%"
                    , br = "%br:DisplayWordCounts%"
                    , mr = "%mr:DisplayWordCounts%"
                    , cs = "%cs:DisplayWordCounts%"
                    , fa = "%fa:DisplayWordCounts%"
                    , id = "%id:DisplayWordCounts%"
                    , ko = "%ko:DisplayWordCounts%"
                    , pl = "%pl:DisplayWordCounts%"
                    , uk = "%uk:DisplayWordCounts%"
                    , nb = "%nb:DisplayWordCounts%"
                    }

                EditMode ->
                    { en = "(Edit Mode)"
                    , zh_hans = "%zh_hans:EditMode%"
                    , zh_hant = "%zh_hant:EditMode%"
                    , es = "%es:EditMode%"
                    , ar = "%ar:EditMode%"
                    , fr = "%fr:EditMode%"
                    , ru = "%ru:EditMode%"
                    , de = "%de:EditMode%"
                    , ja = "%ja:EditMode%"
                    , pes = "%pes:EditMode%"
                    , it = "%it:EditMode%"
                    , ro = "%ro:EditMode%"
                    , hr = "%hr:EditMode%"
                    , nl = "%nl:EditMode%"
                    , hu = "%hu:EditMode%"
                    , sv = "%sv:EditMode%"
                    , ca = "%ca:EditMode%"
                    , br = "%br:EditMode%"
                    , mr = "%mr:EditMode%"
                    , cs = "%cs:EditMode%"
                    , fa = "%fa:EditMode%"
                    , id = "%id:EditMode%"
                    , ko = "%ko:EditMode%"
                    , pl = "%pl:EditMode%"
                    , uk = "%uk:EditMode%"
                    , nb = "%nb:EditMode%"
                    }

                SaveOrCancelChanges ->
                    { en = "Save/Cancel Changes"
                    , zh_hans = "%zh_hans:SaveOrCancelChanges%"
                    , zh_hant = "%zh_hant:SaveOrCancelChanges%"
                    , es = "%es:SaveOrCancelChanges%"
                    , ar = "%ar:SaveOrCancelChanges%"
                    , fr = "%fr:SaveOrCancelChanges%"
                    , ru = "%ru:SaveOrCancelChanges%"
                    , de = "%de:SaveOrCancelChanges%"
                    , ja = "%ja:SaveOrCancelChanges%"
                    , pes = "%pes:SaveOrCancelChanges%"
                    , it = "%it:SaveOrCancelChanges%"
                    , ro = "%ro:SaveOrCancelChanges%"
                    , hr = "%hr:SaveOrCancelChanges%"
                    , nl = "%nl:SaveOrCancelChanges%"
                    , hu = "%hu:SaveOrCancelChanges%"
                    , sv = "%sv:SaveOrCancelChanges%"
                    , ca = "%ca:SaveOrCancelChanges%"
                    , br = "%br:SaveOrCancelChanges%"
                    , mr = "%mr:SaveOrCancelChanges%"
                    , cs = "%cs:SaveOrCancelChanges%"
                    , fa = "%fa:SaveOrCancelChanges%"
                    , id = "%id:SaveOrCancelChanges%"
                    , ko = "%ko:SaveOrCancelChanges%"
                    , pl = "%pl:SaveOrCancelChanges%"
                    , uk = "%uk:SaveOrCancelChanges%"
                    , nb = "%nb:SaveOrCancelChanges%"
                    }

                Formatting ->
                    { en = "Formatting"
                    , zh_hans = "%zh_hans:Formatting%"
                    , zh_hant = "%zh_hant:Formatting%"
                    , es = "%es:Formatting%"
                    , ar = "%ar:Formatting%"
                    , fr = "%fr:Formatting%"
                    , ru = "%ru:Formatting%"
                    , de = "%de:Formatting%"
                    , ja = "%ja:Formatting%"
                    , pes = "%pes:Formatting%"
                    , it = "%it:Formatting%"
                    , ro = "%ro:Formatting%"
                    , hr = "%hr:Formatting%"
                    , nl = "%nl:Formatting%"
                    , hu = "%hu:Formatting%"
                    , sv = "%sv:Formatting%"
                    , ca = "%ca:Formatting%"
                    , br = "%br:Formatting%"
                    , mr = "%mr:Formatting%"
                    , cs = "%cs:Formatting%"
                    , fa = "%fa:Formatting%"
                    , id = "%id:Formatting%"
                    , ko = "%ko:Formatting%"
                    , pl = "%pl:Formatting%"
                    , uk = "%uk:Formatting%"
                    , nb = "%nb:Formatting%"
                    }

                FormattingTitle ->
                    { en = "# Title\n## Subtitle"
                    , zh_hans = "%zh_hans:FormattingTitle%"
                    , zh_hant = "%zh_hant:FormattingTitle%"
                    , es = "%es:FormattingTitle%"
                    , ar = "%ar:FormattingTitle%"
                    , fr = "%fr:FormattingTitle%"
                    , ru = "%ru:FormattingTitle%"
                    , de = "%de:FormattingTitle%"
                    , ja = "%ja:FormattingTitle%"
                    , pes = "%pes:FormattingTitle%"
                    , it = "%it:FormattingTitle%"
                    , ro = "%ro:FormattingTitle%"
                    , hr = "%hr:FormattingTitle%"
                    , nl = "%nl:FormattingTitle%"
                    , hu = "%hu:FormattingTitle%"
                    , sv = "%sv:FormattingTitle%"
                    , ca = "%ca:FormattingTitle%"
                    , br = "%br:FormattingTitle%"
                    , mr = "%mr:FormattingTitle%"
                    , cs = "%cs:FormattingTitle%"
                    , fa = "%fa:FormattingTitle%"
                    , id = "%id:FormattingTitle%"
                    , ko = "%ko:FormattingTitle%"
                    , pl = "%pl:FormattingTitle%"
                    , uk = "%uk:FormattingTitle%"
                    , nb = "%nb:FormattingTitle%"
                    }

                SetTitleLevel ->
                    { en = "Set title level (# to #####)"
                    , zh_hans = "%zh_hans:SetTitleLevel%"
                    , zh_hant = "%zh_hant:SetTitleLevel%"
                    , es = "%es:SetTitleLevel%"
                    , ar = "%ar:SetTitleLevel%"
                    , fr = "%fr:SetTitleLevel%"
                    , ru = "%ru:SetTitleLevel%"
                    , de = "%de:SetTitleLevel%"
                    , ja = "%ja:SetTitleLevel%"
                    , pes = "%pes:SetTitleLevel%"
                    , it = "%it:SetTitleLevel%"
                    , ro = "%ro:SetTitleLevel%"
                    , hr = "%hr:SetTitleLevel%"
                    , nl = "%nl:SetTitleLevel%"
                    , hu = "%hu:SetTitleLevel%"
                    , sv = "%sv:SetTitleLevel%"
                    , ca = "%ca:SetTitleLevel%"
                    , br = "%br:SetTitleLevel%"
                    , mr = "%mr:SetTitleLevel%"
                    , cs = "%cs:SetTitleLevel%"
                    , fa = "%fa:SetTitleLevel%"
                    , id = "%id:SetTitleLevel%"
                    , ko = "%ko:SetTitleLevel%"
                    , pl = "%pl:SetTitleLevel%"
                    , uk = "%uk:SetTitleLevel%"
                    , nb = "%nb:SetTitleLevel%"
                    }

                FormattingList ->
                    { en = "- List item\n  - Subitem"
                    , zh_hans = "%zh_hans:FormattingList%"
                    , zh_hant = "%zh_hant:FormattingList%"
                    , es = "%es:FormattingList%"
                    , ar = "%ar:FormattingList%"
                    , fr = "%fr:FormattingList%"
                    , ru = "%ru:FormattingList%"
                    , de = "%de:FormattingList%"
                    , ja = "%ja:FormattingList%"
                    , pes = "%pes:FormattingList%"
                    , it = "%it:FormattingList%"
                    , ro = "%ro:FormattingList%"
                    , hr = "%hr:FormattingList%"
                    , nl = "%nl:FormattingList%"
                    , hu = "%hu:FormattingList%"
                    , sv = "%sv:FormattingList%"
                    , ca = "%ca:FormattingList%"
                    , br = "%br:FormattingList%"
                    , mr = "%mr:FormattingList%"
                    , cs = "%cs:FormattingList%"
                    , fa = "%fa:FormattingList%"
                    , id = "%id:FormattingList%"
                    , ko = "%ko:FormattingList%"
                    , pl = "%pl:FormattingList%"
                    , uk = "%uk:FormattingList%"
                    , nb = "%nb:FormattingList%"
                    }

                FormattingLink ->
                    { en = "[link](http://t.co)"
                    , zh_hans = "%zh_hans:FormattingLink%"
                    , zh_hant = "%zh_hant:FormattingLink%"
                    , es = "%es:FormattingLink%"
                    , ar = "%ar:FormattingLink%"
                    , fr = "%fr:FormattingLink%"
                    , ru = "%ru:FormattingLink%"
                    , de = "%de:FormattingLink%"
                    , ja = "%ja:FormattingLink%"
                    , pes = "%pes:FormattingLink%"
                    , it = "%it:FormattingLink%"
                    , ro = "%ro:FormattingLink%"
                    , hr = "%hr:FormattingLink%"
                    , nl = "%nl:FormattingLink%"
                    , hu = "%hu:FormattingLink%"
                    , sv = "%sv:FormattingLink%"
                    , ca = "%ca:FormattingLink%"
                    , br = "%br:FormattingLink%"
                    , mr = "%mr:FormattingLink%"
                    , cs = "%cs:FormattingLink%"
                    , fa = "%fa:FormattingLink%"
                    , id = "%id:FormattingLink%"
                    , ko = "%ko:FormattingLink%"
                    , pl = "%pl:FormattingLink%"
                    , uk = "%uk:FormattingLink%"
                    , nb = "%nb:FormattingLink%"
                    }

                ParenNumber ->
                    { en = "ParenNumber"
                    , zh_hans = "%zh_hans:ParenNumber%"
                    , zh_hant = "%zh_hant:ParenNumber%"
                    , es = "%es:ParenNumber%"
                    , ar = "%ar:ParenNumber%"
                    , fr = "%fr:ParenNumber%"
                    , ru = "%ru:ParenNumber%"
                    , de = "%de:ParenNumber%"
                    , ja = "%ja:ParenNumber%"
                    , pes = "%pes:ParenNumber%"
                    , it = "%it:ParenNumber%"
                    , ro = "%ro:ParenNumber%"
                    , hr = "%hr:ParenNumber%"
                    , nl = "%nl:ParenNumber%"
                    , hu = "%hu:ParenNumber%"
                    , sv = "%sv:ParenNumber%"
                    , ca = "%ca:ParenNumber%"
                    , br = "%br:ParenNumber%"
                    , mr = "%mr:ParenNumber%"
                    , cs = "%cs:ParenNumber%"
                    , fa = "%fa:ParenNumber%"
                    , id = "%id:ParenNumber%"
                    , ko = "%ko:ParenNumber%"
                    , pl = "%pl:ParenNumber%"
                    , uk = "%uk:ParenNumber%"
                    , nb = "%nb:ParenNumber%"
                    }

                SetHeadingLevel ->
                    { en = "SetHeadingLevel"
                    , zh_hans = "%zh_hans:SetHeadingLevel%"
                    , zh_hant = "%zh_hant:SetHeadingLevel%"
                    , es = "%es:SetHeadingLevel%"
                    , ar = "%ar:SetHeadingLevel%"
                    , fr = "%fr:SetHeadingLevel%"
                    , ru = "%ru:SetHeadingLevel%"
                    , de = "%de:SetHeadingLevel%"
                    , ja = "%ja:SetHeadingLevel%"
                    , pes = "%pes:SetHeadingLevel%"
                    , it = "%it:SetHeadingLevel%"
                    , ro = "%ro:SetHeadingLevel%"
                    , hr = "%hr:SetHeadingLevel%"
                    , nl = "%nl:SetHeadingLevel%"
                    , hu = "%hu:SetHeadingLevel%"
                    , sv = "%sv:SetHeadingLevel%"
                    , ca = "%ca:SetHeadingLevel%"
                    , br = "%br:SetHeadingLevel%"
                    , mr = "%mr:SetHeadingLevel%"
                    , cs = "%cs:SetHeadingLevel%"
                    , fa = "%fa:SetHeadingLevel%"
                    , id = "%id:SetHeadingLevel%"
                    , ko = "%ko:SetHeadingLevel%"
                    , pl = "%pl:SetHeadingLevel%"
                    , uk = "%uk:SetHeadingLevel%"
                    , nb = "%nb:SetHeadingLevel%"
                    }

                HelpVideos ->
                    { en = "Help Videos"
                    , zh_hans = "%zh_hans:HelpVideos%"
                    , zh_hant = "%zh_hant:HelpVideos%"
                    , es = "%es:HelpVideos%"
                    , ar = "%ar:HelpVideos%"
                    , fr = "%fr:HelpVideos%"
                    , ru = "%ru:HelpVideos%"
                    , de = "%de:HelpVideos%"
                    , ja = "%ja:HelpVideos%"
                    , pes = "%pes:HelpVideos%"
                    , it = "%it:HelpVideos%"
                    , ro = "%ro:HelpVideos%"
                    , hr = "%hr:HelpVideos%"
                    , nl = "%nl:HelpVideos%"
                    , hu = "%hu:HelpVideos%"
                    , sv = "%sv:HelpVideos%"
                    , ca = "%ca:HelpVideos%"
                    , br = "%br:HelpVideos%"
                    , mr = "%mr:HelpVideos%"
                    , cs = "%cs:HelpVideos%"
                    , fa = "%fa:HelpVideos%"
                    , id = "%id:HelpVideos%"
                    , ko = "%ko:HelpVideos%"
                    , pl = "%pl:HelpVideos%"
                    , uk = "%uk:HelpVideos%"
                    , nb = "%nb:HelpVideos%"
                    }

                FAQAndDocs ->
                    { en = "FAQ & Documentation"
                    , zh_hans = "%zh_hans:FAQAndDocs%"
                    , zh_hant = "%zh_hant:FAQAndDocs%"
                    , es = "%es:FAQAndDocs%"
                    , ar = "%ar:FAQAndDocs%"
                    , fr = "%fr:FAQAndDocs%"
                    , ru = "%ru:FAQAndDocs%"
                    , de = "%de:FAQAndDocs%"
                    , ja = "%ja:FAQAndDocs%"
                    , pes = "%pes:FAQAndDocs%"
                    , it = "%it:FAQAndDocs%"
                    , ro = "%ro:FAQAndDocs%"
                    , hr = "%hr:FAQAndDocs%"
                    , nl = "%nl:FAQAndDocs%"
                    , hu = "%hu:FAQAndDocs%"
                    , sv = "%sv:FAQAndDocs%"
                    , ca = "%ca:FAQAndDocs%"
                    , br = "%br:FAQAndDocs%"
                    , mr = "%mr:FAQAndDocs%"
                    , cs = "%cs:FAQAndDocs%"
                    , fa = "%fa:FAQAndDocs%"
                    , id = "%id:FAQAndDocs%"
                    , ko = "%ko:FAQAndDocs%"
                    , pl = "%pl:FAQAndDocs%"
                    , uk = "%uk:FAQAndDocs%"
                    , nb = "%nb:FAQAndDocs%"
                    }

                --
                AreYouSureCancel ->
                    { en = "Are you sure you want to undo your changes?"
                    , zh_hans = "%zh_hans:AreYouSureCancel%"
                    , zh_hant = "%zh_hant:AreYouSureCancel%"
                    , es = "%es:AreYouSureCancel%"
                    , ar = "%ar:AreYouSureCancel%"
                    , fr = "%fr:AreYouSureCancel%"
                    , ru = "%ru:AreYouSureCancel%"
                    , de = "%de:AreYouSureCancel%"
                    , ja = "%ja:AreYouSureCancel%"
                    , pes = "%pes:AreYouSureCancel%"
                    , it = "%it:AreYouSureCancel%"
                    , ro = "%ro:AreYouSureCancel%"
                    , hr = "%hr:AreYouSureCancel%"
                    , nl = "%nl:AreYouSureCancel%"
                    , hu = "%hu:AreYouSureCancel%"
                    , sv = "%sv:AreYouSureCancel%"
                    , ca = "%ca:AreYouSureCancel%"
                    , br = "%br:AreYouSureCancel%"
                    , mr = "%mr:AreYouSureCancel%"
                    , cs = "%cs:AreYouSureCancel%"
                    , fa = "%fa:AreYouSureCancel%"
                    , id = "%id:AreYouSureCancel%"
                    , ko = "%ko:AreYouSureCancel%"
                    , pl = "%pl:AreYouSureCancel%"
                    , uk = "%uk:AreYouSureCancel%"
                    , nb = "%nb:AreYouSureCancel%"
                    }

                ToCancelChanges ->
                    { en = "to Cancel Changes"
                    , zh_hans = "%zh_hans:ToCancelChanges%"
                    , zh_hant = "%zh_hant:ToCancelChanges%"
                    , es = "%es:ToCancelChanges%"
                    , ar = "%ar:ToCancelChanges%"
                    , fr = "%fr:ToCancelChanges%"
                    , ru = "%ru:ToCancelChanges%"
                    , de = "%de:ToCancelChanges%"
                    , ja = "%ja:ToCancelChanges%"
                    , pes = "%pes:ToCancelChanges%"
                    , it = "%it:ToCancelChanges%"
                    , ro = "%ro:ToCancelChanges%"
                    , hr = "%hr:ToCancelChanges%"
                    , nl = "%nl:ToCancelChanges%"
                    , hu = "%hu:ToCancelChanges%"
                    , sv = "%sv:ToCancelChanges%"
                    , ca = "%ca:ToCancelChanges%"
                    , br = "%br:ToCancelChanges%"
                    , mr = "%mr:ToCancelChanges%"
                    , cs = "%cs:ToCancelChanges%"
                    , fa = "%fa:ToCancelChanges%"
                    , id = "%id:ToCancelChanges%"
                    , ko = "%ko:ToCancelChanges%"
                    , pl = "%pl:ToCancelChanges%"
                    , uk = "%uk:ToCancelChanges%"
                    , nb = "%nb:ToCancelChanges%"
                    }

                PressToSearch ->
                    { en = "Press '/' to search"
                    , zh_hans = "%zh_hans:PressToSearch%"
                    , zh_hant = "%zh_hant:PressToSearch%"
                    , es = "%es:PressToSearch%"
                    , ar = "%ar:PressToSearch%"
                    , fr = "%fr:PressToSearch%"
                    , ru = "%ru:PressToSearch%"
                    , de = "%de:PressToSearch%"
                    , ja = "%ja:PressToSearch%"
                    , pes = "%pes:PressToSearch%"
                    , it = "%it:PressToSearch%"
                    , ro = "%ro:PressToSearch%"
                    , hr = "%hr:PressToSearch%"
                    , nl = "%nl:PressToSearch%"
                    , hu = "%hu:PressToSearch%"
                    , sv = "%sv:PressToSearch%"
                    , ca = "%ca:PressToSearch%"
                    , br = "%br:PressToSearch%"
                    , mr = "%mr:PressToSearch%"
                    , cs = "%cs:PressToSearch%"
                    , fa = "%fa:PressToSearch%"
                    , id = "%id:PressToSearch%"
                    , ko = "%ko:PressToSearch%"
                    , pl = "%pl:PressToSearch%"
                    , uk = "%uk:PressToSearch%"
                    , nb = "%nb:PressToSearch%"
                    }

                QuickDocumentSwitcher ->
                    { en = "Quick Document Switcher"
                    , zh_hans = "%zh_hans:QuickDocumentSwitcher%"
                    , zh_hant = "%zh_hant:QuickDocumentSwitcher%"
                    , es = "%es:QuickDocumentSwitcher%"
                    , ar = "%ar:QuickDocumentSwitcher%"
                    , fr = "%fr:QuickDocumentSwitcher%"
                    , ru = "%ru:QuickDocumentSwitcher%"
                    , de = "%de:QuickDocumentSwitcher%"
                    , ja = "%ja:QuickDocumentSwitcher%"
                    , pes = "%pes:QuickDocumentSwitcher%"
                    , it = "%it:QuickDocumentSwitcher%"
                    , ro = "%ro:QuickDocumentSwitcher%"
                    , hr = "%hr:QuickDocumentSwitcher%"
                    , nl = "%nl:QuickDocumentSwitcher%"
                    , hu = "%hu:QuickDocumentSwitcher%"
                    , sv = "%sv:QuickDocumentSwitcher%"
                    , ca = "%ca:QuickDocumentSwitcher%"
                    , br = "%br:QuickDocumentSwitcher%"
                    , mr = "%mr:QuickDocumentSwitcher%"
                    , cs = "%cs:QuickDocumentSwitcher%"
                    , fa = "%fa:QuickDocumentSwitcher%"
                    , id = "%id:QuickDocumentSwitcher%"
                    , ko = "%ko:QuickDocumentSwitcher%"
                    , pl = "%pl:QuickDocumentSwitcher%"
                    , uk = "%uk:QuickDocumentSwitcher%"
                    , nb = "%nb:QuickDocumentSwitcher%"
                    }

                OpenQuickSwitcher ->
                    { en = "Open Quick Switcher"
                    , zh_hans = "%zh_hans:OpenQuickSwitcher%"
                    , zh_hant = "%zh_hant:OpenQuickSwitcher%"
                    , es = "%es:OpenQuickSwitcher%"
                    , ar = "%ar:OpenQuickSwitcher%"
                    , fr = "%fr:OpenQuickSwitcher%"
                    , ru = "%ru:OpenQuickSwitcher%"
                    , de = "%de:OpenQuickSwitcher%"
                    , ja = "%ja:OpenQuickSwitcher%"
                    , pes = "%pes:OpenQuickSwitcher%"
                    , it = "%it:OpenQuickSwitcher%"
                    , ro = "%ro:OpenQuickSwitcher%"
                    , hr = "%hr:OpenQuickSwitcher%"
                    , nl = "%nl:OpenQuickSwitcher%"
                    , hu = "%hu:OpenQuickSwitcher%"
                    , sv = "%sv:OpenQuickSwitcher%"
                    , ca = "%ca:OpenQuickSwitcher%"
                    , br = "%br:OpenQuickSwitcher%"
                    , mr = "%mr:OpenQuickSwitcher%"
                    , cs = "%cs:OpenQuickSwitcher%"
                    , fa = "%fa:OpenQuickSwitcher%"
                    , id = "%id:OpenQuickSwitcher%"
                    , ko = "%ko:OpenQuickSwitcher%"
                    , pl = "%pl:OpenQuickSwitcher%"
                    , uk = "%uk:OpenQuickSwitcher%"
                    , nb = "%nb:OpenQuickSwitcher%"
                    }

                ContactSupport ->
                    { en = "Contact Support"
                    , zh_hans = "%zh_hans:EmailSupport%"
                    , zh_hant = "%zh_hant:EmailSupport%"
                    , es = "%es:EmailSupport%"
                    , ar = "%ar:EmailSupport%"
                    , fr = "%fr:EmailSupport%"
                    , ru = "%ru:EmailSupport%"
                    , de = "%de:EmailSupport%"
                    , ja = "%ja:EmailSupport%"
                    , pes = "%pes:EmailSupport%"
                    , it = "%it:EmailSupport%"
                    , ro = "%ro:EmailSupport%"
                    , hr = "%hr:EmailSupport%"
                    , nl = "%nl:EmailSupport%"
                    , hu = "%hu:EmailSupport%"
                    , sv = "%sv:EmailSupport%"
                    , ca = "%ca:EmailSupport%"
                    , br = "%br:EmailSupport%"
                    , mr = "%mr:EmailSupport%"
                    , cs = "%cs:EmailSupport%"
                    , fa = "%fa:EmailSupport%"
                    , id = "%id:EmailSupport%"
                    , ko = "%ko:EmailSupport%"
                    , pl = "%pl:EmailSupport%"
                    , uk = "%uk:EmailSupport%"
                    , nb = "%nb:EmailSupport%"
                    }

                Logout ->
                    { en = "Logout"
                    , zh_hans = "%zh_hans:Logout%"
                    , zh_hant = "%zh_hant:Logout%"
                    , es = "%es:Logout%"
                    , ar = "%ar:Logout%"
                    , fr = "%fr:Logout%"
                    , ru = "%ru:Logout%"
                    , de = "%de:Logout%"
                    , ja = "%ja:Logout%"
                    , pes = "%pes:Logout%"
                    , it = "%it:Logout%"
                    , ro = "%ro:Logout%"
                    , hr = "%hr:Logout%"
                    , nl = "%nl:Logout%"
                    , hu = "%hu:Logout%"
                    , sv = "%sv:Logout%"
                    , ca = "%ca:Logout%"
                    , br = "%br:Logout%"
                    , mr = "%mr:Logout%"
                    , cs = "%cs:Logout%"
                    , fa = "%fa:Logout%"
                    , id = "%id:Logout%"
                    , ko = "%ko:Logout%"
                    , pl = "%pl:Logout%"
                    , uk = "%uk:Logout%"
                    , nb = "%nb:Logout%"
                    }

                Language ->
                    { en = "Language"
                    , zh_hans = "%zh_hans:Language%"
                    , zh_hant = "%zh_hant:Language%"
                    , es = "%es:Language%"
                    , ar = "%ar:Language%"
                    , fr = "%fr:Language%"
                    , ru = "%ru:Language%"
                    , de = "%de:Language%"
                    , ja = "%ja:Language%"
                    , pes = "%pes:Language%"
                    , it = "%it:Language%"
                    , ro = "%ro:Language%"
                    , hr = "%hr:Language%"
                    , nl = "%nl:Language%"
                    , hu = "%hu:Language%"
                    , sv = "%sv:Language%"
                    , ca = "%ca:Language%"
                    , br = "%br:Language%"
                    , mr = "%mr:Language%"
                    , cs = "%cs:Language%"
                    , fa = "%fa:Language%"
                    , id = "%id:Language%"
                    , ko = "%ko:Language%"
                    , pl = "%pl:Language%"
                    , uk = "%uk:Language%"
                    , nb = "%nb:Language%"
                    }

                ContributeTranslations ->
                    { en = "Contribute translations"
                    , zh_hans = "%zh_hans:ContributeTranslations%"
                    , zh_hant = "%zh_hant:ContributeTranslations%"
                    , es = "%es:ContributeTranslations%"
                    , ar = "%ar:ContributeTranslations%"
                    , fr = "%fr:ContributeTranslations%"
                    , ru = "%ru:ContributeTranslations%"
                    , de = "%de:ContributeTranslations%"
                    , ja = "%ja:ContributeTranslations%"
                    , pes = "%pes:ContributeTranslations%"
                    , it = "%it:ContributeTranslations%"
                    , ro = "%ro:ContributeTranslations%"
                    , hr = "%hr:ContributeTranslations%"
                    , nl = "%nl:ContributeTranslations%"
                    , hu = "%hu:ContributeTranslations%"
                    , sv = "%sv:ContributeTranslations%"
                    , ca = "%ca:ContributeTranslations%"
                    , br = "%br:ContributeTranslations%"
                    , mr = "%mr:ContributeTranslations%"
                    , cs = "%cs:ContributeTranslations%"
                    , fa = "%fa:ContributeTranslations%"
                    , id = "%id:ContributeTranslations%"
                    , ko = "%ko:ContributeTranslations%"
                    , pl = "%pl:ContributeTranslations%"
                    , uk = "%uk:ContributeTranslations%"
                    , nb = "%nb:ContributeTranslations%"
                    }

                Here ->
                    { en = "here"
                    , zh_hans = "%zh_hans:Here%"
                    , zh_hant = "%zh_hant:Here%"
                    , es = "%es:Here%"
                    , ar = "%ar:Here%"
                    , fr = "%fr:Here%"
                    , ru = "%ru:Here%"
                    , de = "%de:Here%"
                    , ja = "%ja:Here%"
                    , pes = "%pes:Here%"
                    , it = "%it:Here%"
                    , ro = "%ro:Here%"
                    , hr = "%hr:Here%"
                    , nl = "%nl:Here%"
                    , hu = "%hu:Here%"
                    , sv = "%sv:Here%"
                    , ca = "%ca:Here%"
                    , br = "%br:Here%"
                    , mr = "%mr:Here%"
                    , cs = "%cs:Here%"
                    , fa = "%fa:Here%"
                    , id = "%id:Here%"
                    , ko = "%ko:Here%"
                    , pl = "%pl:Here%"
                    , uk = "%uk:Here%"
                    , nb = "%nb:Here%"
                    }

                HeadingFont ->
                    { en = "Heading Font"
                    , zh_hans = "%zh_hans:HeadingFont%"
                    , zh_hant = "%zh_hant:HeadingFont%"
                    , es = "%es:HeadingFont%"
                    , ar = "%ar:HeadingFont%"
                    , fr = "%fr:HeadingFont%"
                    , ru = "%ru:HeadingFont%"
                    , de = "%de:HeadingFont%"
                    , ja = "%ja:HeadingFont%"
                    , pes = "%pes:HeadingFont%"
                    , it = "%it:HeadingFont%"
                    , ro = "%ro:HeadingFont%"
                    , hr = "%hr:HeadingFont%"
                    , nl = "%nl:HeadingFont%"
                    , hu = "%hu:HeadingFont%"
                    , sv = "%sv:HeadingFont%"
                    , ca = "%ca:HeadingFont%"
                    , br = "%br:HeadingFont%"
                    , mr = "%mr:HeadingFont%"
                    , cs = "%cs:HeadingFont%"
                    , fa = "%fa:HeadingFont%"
                    , id = "%id:HeadingFont%"
                    , ko = "%ko:HeadingFont%"
                    , pl = "%pl:HeadingFont%"
                    , uk = "%uk:HeadingFont%"
                    , nb = "%nb:HeadingFont%"
                    }

                ContentFont ->
                    { en = "Content Font"
                    , zh_hans = "%zh_hans:ContentFont%"
                    , zh_hant = "%zh_hant:ContentFont%"
                    , es = "%es:ContentFont%"
                    , ar = "%ar:ContentFont%"
                    , fr = "%fr:ContentFont%"
                    , ru = "%ru:ContentFont%"
                    , de = "%de:ContentFont%"
                    , ja = "%ja:ContentFont%"
                    , pes = "%pes:ContentFont%"
                    , it = "%it:ContentFont%"
                    , ro = "%ro:ContentFont%"
                    , hr = "%hr:ContentFont%"
                    , nl = "%nl:ContentFont%"
                    , hu = "%hu:ContentFont%"
                    , sv = "%sv:ContentFont%"
                    , ca = "%ca:ContentFont%"
                    , br = "%br:ContentFont%"
                    , mr = "%mr:ContentFont%"
                    , cs = "%cs:ContentFont%"
                    , fa = "%fa:ContentFont%"
                    , id = "%id:ContentFont%"
                    , ko = "%ko:ContentFont%"
                    , pl = "%pl:ContentFont%"
                    , uk = "%uk:ContentFont%"
                    , nb = "%nb:ContentFont%"
                    }

                EditingFont ->
                    { en = "Editing/Monospace Font"
                    , zh_hans = "%zh_hans:EditingFont%"
                    , zh_hant = "%zh_hant:EditingFont%"
                    , es = "%es:EditingFont%"
                    , ar = "%ar:EditingFont%"
                    , fr = "%fr:EditingFont%"
                    , ru = "%ru:EditingFont%"
                    , de = "%de:EditingFont%"
                    , ja = "%ja:EditingFont%"
                    , pes = "%pes:EditingFont%"
                    , it = "%it:EditingFont%"
                    , ro = "%ro:EditingFont%"
                    , hr = "%hr:EditingFont%"
                    , nl = "%nl:EditingFont%"
                    , hu = "%hu:EditingFont%"
                    , sv = "%sv:EditingFont%"
                    , ca = "%ca:EditingFont%"
                    , br = "%br:EditingFont%"
                    , mr = "%mr:EditingFont%"
                    , cs = "%cs:EditingFont%"
                    , fa = "%fa:EditingFont%"
                    , id = "%id:EditingFont%"
                    , ko = "%ko:EditingFont%"
                    , pl = "%pl:EditingFont%"
                    , uk = "%uk:EditingFont%"
                    , nb = "%nb:EditingFont%"
                    }

                MigrateTooltip ->
                    { en = "Upgrade document to new format"
                    , zh_hans = "%zh_hans:MigrateToCardBased%"
                    , zh_hant = "%zh_hant:MigrateToCardBased%"
                    , es = "%es:MigrateToCardBased%"
                    , ar = "%ar:MigrateToCardBased%"
                    , fr = "%fr:MigrateToCardBased%"
                    , ru = "%ru:MigrateToCardBased%"
                    , de = "%de:MigrateToCardBased%"
                    , ja = "%ja:MigrateToCardBased%"
                    , pes = "%pes:MigrateToCardBased%"
                    , it = "%it:MigrateToCardBased%"
                    , ro = "%ro:MigrateToCardBased%"
                    , hr = "%hr:MigrateToCardBased%"
                    , nl = "%nl:MigrateToCardBased%"
                    , hu = "%hu:MigrateToCardBased%"
                    , sv = "%sv:MigrateToCardBased%"
                    , ca = "%ca:MigrateToCardBased%"
                    , br = "%br:MigrateToCardBased%"
                    , mr = "%mr:MigrateToCardBased%"
                    , cs = "%cs:MigrateToCardBased%"
                    , fa = "%fa:MigrateToCardBased%"
                    , id = "%id:MigrateToCardBased%"
                    , ko = "%ko:MigrateToCardBased%"
                    , pl = "%pl:MigrateToCardBased%"
                    , uk = "%uk:MigrateToCardBased%"
                    , nb = "%nb:MigrateToCardBased%"
                    }

                VersionHistory ->
                    { en = "Version History"
                    , zh_hans = "%zh_hans:VersionHistory%"
                    , zh_hant = "%zh_hant:VersionHistory%"
                    , es = "%es:VersionHistory%"
                    , ar = "%ar:VersionHistory%"
                    , fr = "%fr:VersionHistory%"
                    , ru = "%ru:VersionHistory%"
                    , de = "%de:VersionHistory%"
                    , ja = "%ja:VersionHistory%"
                    , pes = "%pes:VersionHistory%"
                    , it = "%it:VersionHistory%"
                    , ro = "%ro:VersionHistory%"
                    , hr = "%hr:VersionHistory%"
                    , nl = "%nl:VersionHistory%"
                    , hu = "%hu:VersionHistory%"
                    , sv = "%sv:VersionHistory%"
                    , ca = "%ca:VersionHistory%"
                    , br = "%br:VersionHistory%"
                    , mr = "%mr:VersionHistory%"
                    , cs = "%cs:VersionHistory%"
                    , fa = "%fa:VersionHistory%"
                    , id = "%id:VersionHistory%"
                    , ko = "%ko:VersionHistory%"
                    , pl = "%pl:VersionHistory%"
                    , uk = "%uk:VersionHistory%"
                    , nb = "%nb:VersionHistory%"
                    }

                DocumentSettings ->
                    { en = "Document Settings"
                    , zh_hans = "%zh_hans:DocumentSettings%"
                    , zh_hant = "%zh_hant:DocumentSettings%"
                    , es = "%es:DocumentSettings%"
                    , ar = "%ar:DocumentSettings%"
                    , fr = "%fr:DocumentSettings%"
                    , ru = "%ru:DocumentSettings%"
                    , de = "%de:DocumentSettings%"
                    , ja = "%ja:DocumentSettings%"
                    , pes = "%pes:DocumentSettings%"
                    , it = "%it:DocumentSettings%"
                    , ro = "%ro:DocumentSettings%"
                    , hr = "%hr:DocumentSettings%"
                    , nl = "%nl:DocumentSettings%"
                    , hu = "%hu:DocumentSettings%"
                    , sv = "%sv:DocumentSettings%"
                    , ca = "%ca:DocumentSettings%"
                    , br = "%br:DocumentSettings%"
                    , mr = "%mr:DocumentSettings%"
                    , cs = "%cs:DocumentSettings%"
                    , fa = "%fa:DocumentSettings%"
                    , id = "%id:DocumentSettings%"
                    , ko = "%ko:DocumentSettings%"
                    , pl = "%pl:DocumentSettings%"
                    , uk = "%uk:DocumentSettings%"
                    , nb = "%nb:DocumentSettings%"
                    }

                WordCount ->
                    { en = "Word count..."
                    , zh_hans = "%zh_hans:WordCount%"
                    , zh_hant = "%zh_hant:WordCount%"
                    , es = "%es:WordCount%"
                    , ar = "%ar:WordCount%"
                    , fr = "%fr:WordCount%"
                    , ru = "%ru:WordCount%"
                    , de = "%de:WordCount%"
                    , ja = "%ja:WordCount%"
                    , pes = "%pes:WordCount%"
                    , it = "%it:WordCount%"
                    , ro = "%ro:WordCount%"
                    , hr = "%hr:WordCount%"
                    , nl = "%nl:WordCount%"
                    , hu = "%hu:WordCount%"
                    , sv = "%sv:WordCount%"
                    , ca = "%ca:WordCount%"
                    , br = "%br:WordCount%"
                    , mr = "%mr:WordCount%"
                    , cs = "%cs:WordCount%"
                    , fa = "%fa:WordCount%"
                    , id = "%id:WordCount%"
                    , ko = "%ko:WordCount%"
                    , pl = "%pl:WordCount%"
                    , uk = "%uk:WordCount%"
                    , nb = "%nb:WordCount%"
                    }

                WordCountSession n ->
                    { en = numberPlural n "Session : %1 word" "Session : %1 words"
                    , zh_hans = numberPlural n "%zh_hans:WordCountSession%:0" "%zh_hans:WordCountSession%:1"
                    , zh_hant = numberPlural n "%zh_hant:WordCountSession%:0" "%zh_hant:WordCountSession%:1"
                    , es = numberPlural n "%es:WordCountSession%:0" "%es:WordCountSession%:1"
                    , ar = numberPlural n "%ar:WordCountSession%:0" "%ar:WordCountSession%:1"
                    , fr = numberPlural n "%fr:WordCountSession%:0" "%fr:WordCountSession%:1"
                    , ru = numberPlural n "%ru:WordCountSession%:0" "%ru:WordCountSession%:1"
                    , de = numberPlural n "%de:WordCountSession%:0" "%de:WordCountSession%:1"
                    , ja = numberPlural n "%ja:WordCountSession%:0" "%ja:WordCountSession%:1"
                    , pes = numberPlural n "%pes:WordCountSession%:0" "%pes:WordCountSession%:1"
                    , it = numberPlural n "%it:WordCountSession%:0" "%it:WordCountSession%:1"
                    , ro = numberPlural n "%ro:WordCountSession%:0" "%ro:WordCountSession%:1"
                    , hr = numberPlural n "%hr:WordCountSession%:0" "%hr:WordCountSession%:1"
                    , nl = numberPlural n "%nl:WordCountSession%:0" "%nl:WordCountSession%:1"
                    , hu = numberPlural n "%hu:WordCountSession%:0" "%hu:WordCountSession%:1"
                    , sv = numberPlural n "%sv:WordCountSession%:0" "%sv:WordCountSession%:1"
                    , ca = numberPlural n "%ca:WordCountSession%:0" "%ca:WordCountSession%:1"
                    , br = numberPlural n "%br:WordCountSession%:0" "%br:WordCountSession%:1"
                    , mr = numberPlural n "%mr:WordCountSession%:0" "%mr:WordCountSession%:1"
                    , cs = numberPlural n "%cs:WordCountSession%:0" "%cs:WordCountSession%:1"
                    , fa = numberPlural n "%fa:WordCountSession%:0" "%fa:WordCountSession%:1"
                    , id = numberPlural n "%id:WordCountSession%:0" "%id:WordCountSession%:1"
                    , ko = numberPlural n "%ko:WordCountSession%:0" "%ko:WordCountSession%:1"
                    , pl = numberPlural n "%pl:WordCountSession%:0" "%pl:WordCountSession%:1"
                    , uk = numberPlural n "%uk:WordCountSession%:0" "%uk:WordCountSession%:1"
                    , nb = numberPlural n "%nb:WordCountSession%:0" "%nb:WordCountSession%:1"
                    }

                WordCountTotal n ->
                    { en = numberPlural n "Total : %1 word" "Total : %1 words"
                    , zh_hans = numberPlural n "%zh_hans:WordCountTotal%:0" "%zh_hans:WordCountTotal%:1"
                    , zh_hant = numberPlural n "%zh_hant:WordCountTotal%:0" "%zh_hant:WordCountTotal%:1"
                    , es = numberPlural n "%es:WordCountTotal%:0" "%es:WordCountTotal%:1"
                    , ar = numberPlural n "%ar:WordCountTotal%:0" "%ar:WordCountTotal%:1"
                    , fr = numberPlural n "%fr:WordCountTotal%:0" "%fr:WordCountTotal%:1"
                    , ru = numberPlural n "%ru:WordCountTotal%:0" "%ru:WordCountTotal%:1"
                    , de = numberPlural n "%de:WordCountTotal%:0" "%de:WordCountTotal%:1"
                    , ja = numberPlural n "%ja:WordCountTotal%:0" "%ja:WordCountTotal%:1"
                    , pes = numberPlural n "%pes:WordCountTotal%:0" "%pes:WordCountTotal%:1"
                    , it = numberPlural n "%it:WordCountTotal%:0" "%it:WordCountTotal%:1"
                    , ro = numberPlural n "%ro:WordCountTotal%:0" "%ro:WordCountTotal%:1"
                    , hr = numberPlural n "%hr:WordCountTotal%:0" "%hr:WordCountTotal%:1"
                    , nl = numberPlural n "%nl:WordCountTotal%:0" "%nl:WordCountTotal%:1"
                    , hu = numberPlural n "%hu:WordCountTotal%:0" "%hu:WordCountTotal%:1"
                    , sv = numberPlural n "%sv:WordCountTotal%:0" "%sv:WordCountTotal%:1"
                    , ca = numberPlural n "%ca:WordCountTotal%:0" "%ca:WordCountTotal%:1"
                    , br = numberPlural n "%br:WordCountTotal%:0" "%br:WordCountTotal%:1"
                    , mr = numberPlural n "%mr:WordCountTotal%:0" "%mr:WordCountTotal%:1"
                    , cs = numberPlural n "%cs:WordCountTotal%:0" "%cs:WordCountTotal%:1"
                    , fa = numberPlural n "%fa:WordCountTotal%:0" "%fa:WordCountTotal%:1"
                    , id = numberPlural n "%id:WordCountTotal%:0" "%id:WordCountTotal%:1"
                    , ko = numberPlural n "%ko:WordCountTotal%:0" "%ko:WordCountTotal%:1"
                    , pl = numberPlural n "%pl:WordCountTotal%:0" "%pl:WordCountTotal%:1"
                    , uk = numberPlural n "%uk:WordCountTotal%:0" "%uk:WordCountTotal%:1"
                    , nb = numberPlural n "%nb:WordCountTotal%:0" "%nb:WordCountTotal%:1"
                    }

                WordCountCard n ->
                    { en = numberPlural n "Card : %1 word" "Card : %1 words"
                    , zh_hans = numberPlural n "%zh_hans:WordCountCard%:0" "%zh_hans:WordCountCard%:1"
                    , zh_hant = numberPlural n "%zh_hant:WordCountCard%:0" "%zh_hant:WordCountCard%:1"
                    , es = numberPlural n "%es:WordCountCard%:0" "%es:WordCountCard%:1"
                    , ar = numberPlural n "%ar:WordCountCard%:0" "%ar:WordCountCard%:1"
                    , fr = numberPlural n "%fr:WordCountCard%:0" "%fr:WordCountCard%:1"
                    , ru = numberPlural n "%ru:WordCountCard%:0" "%ru:WordCountCard%:1"
                    , de = numberPlural n "%de:WordCountCard%:0" "%de:WordCountCard%:1"
                    , ja = numberPlural n "%ja:WordCountCard%:0" "%ja:WordCountCard%:1"
                    , pes = numberPlural n "%pes:WordCountCard%:0" "%pes:WordCountCard%:1"
                    , it = numberPlural n "%it:WordCountCard%:0" "%it:WordCountCard%:1"
                    , ro = numberPlural n "%ro:WordCountCard%:0" "%ro:WordCountCard%:1"
                    , hr = numberPlural n "%hr:WordCountCard%:0" "%hr:WordCountCard%:1"
                    , nl = numberPlural n "%nl:WordCountCard%:0" "%nl:WordCountCard%:1"
                    , hu = numberPlural n "%hu:WordCountCard%:0" "%hu:WordCountCard%:1"
                    , sv = numberPlural n "%sv:WordCountCard%:0" "%sv:WordCountCard%:1"
                    , ca = numberPlural n "%ca:WordCountCard%:0" "%ca:WordCountCard%:1"
                    , br = numberPlural n "%br:WordCountCard%:0" "%br:WordCountCard%:1"
                    , mr = numberPlural n "%mr:WordCountCard%:0" "%mr:WordCountCard%:1"
                    , cs = numberPlural n "%cs:WordCountCard%:0" "%cs:WordCountCard%:1"
                    , fa = numberPlural n "%fa:WordCountCard%:0" "%fa:WordCountCard%:1"
                    , id = numberPlural n "%id:WordCountCard%:0" "%id:WordCountCard%:1"
                    , ko = numberPlural n "%ko:WordCountCard%:0" "%ko:WordCountCard%:1"
                    , pl = numberPlural n "%pl:WordCountCard%:0" "%pl:WordCountCard%:1"
                    , uk = numberPlural n "%uk:WordCountCard%:0" "%uk:WordCountCard%:1"
                    , nb = numberPlural n "%nb:WordCountCard%:0" "%nb:WordCountCard%:1"
                    }

                WordCountSubtree n ->
                    { en = numberPlural n "Subtree : %1 word" "Subtree : %1 words"
                    , zh_hans = numberPlural n "%zh_hans:WordCountSubtree%:0" "%zh_hans:WordCountSubtree%:1"
                    , zh_hant = numberPlural n "%zh_hant:WordCountSubtree%:0" "%zh_hant:WordCountSubtree%:1"
                    , es = numberPlural n "%es:WordCountSubtree%:0" "%es:WordCountSubtree%:1"
                    , ar = numberPlural n "%ar:WordCountSubtree%:0" "%ar:WordCountSubtree%:1"
                    , fr = numberPlural n "%fr:WordCountSubtree%:0" "%fr:WordCountSubtree%:1"
                    , ru = numberPlural n "%ru:WordCountSubtree%:0" "%ru:WordCountSubtree%:1"
                    , de = numberPlural n "%de:WordCountSubtree%:0" "%de:WordCountSubtree%:1"
                    , ja = numberPlural n "%ja:WordCountSubtree%:0" "%ja:WordCountSubtree%:1"
                    , pes = numberPlural n "%pes:WordCountSubtree%:0" "%pes:WordCountSubtree%:1"
                    , it = numberPlural n "%it:WordCountSubtree%:0" "%it:WordCountSubtree%:1"
                    , ro = numberPlural n "%ro:WordCountSubtree%:0" "%ro:WordCountSubtree%:1"
                    , hr = numberPlural n "%hr:WordCountSubtree%:0" "%hr:WordCountSubtree%:1"
                    , nl = numberPlural n "%nl:WordCountSubtree%:0" "%nl:WordCountSubtree%:1"
                    , hu = numberPlural n "%hu:WordCountSubtree%:0" "%hu:WordCountSubtree%:1"
                    , sv = numberPlural n "%sv:WordCountSubtree%:0" "%sv:WordCountSubtree%:1"
                    , ca = numberPlural n "%ca:WordCountSubtree%:0" "%ca:WordCountSubtree%:1"
                    , br = numberPlural n "%br:WordCountSubtree%:0" "%br:WordCountSubtree%:1"
                    , mr = numberPlural n "%mr:WordCountSubtree%:0" "%mr:WordCountSubtree%:1"
                    , cs = numberPlural n "%cs:WordCountSubtree%:0" "%cs:WordCountSubtree%:1"
                    , fa = numberPlural n "%fa:WordCountSubtree%:0" "%fa:WordCountSubtree%:1"
                    , id = numberPlural n "%id:WordCountSubtree%:0" "%id:WordCountSubtree%:1"
                    , ko = numberPlural n "%ko:WordCountSubtree%:0" "%ko:WordCountSubtree%:1"
                    , pl = numberPlural n "%pl:WordCountSubtree%:0" "%pl:WordCountSubtree%:1"
                    , uk = numberPlural n "%uk:WordCountSubtree%:0" "%uk:WordCountSubtree%:1"
                    , nb = numberPlural n "%nb:WordCountSubtree%:0" "%nb:WordCountSubtree%:1"
                    }

                WordCountGroup n ->
                    { en = numberPlural n "Group : %1 word" "Group : %1 words"
                    , zh_hans = numberPlural n "%zh_hans:WordCountGroup%:0" "%zh_hans:WordCountGroup%:1"
                    , zh_hant = numberPlural n "%zh_hant:WordCountGroup%:0" "%zh_hant:WordCountGroup%:1"
                    , es = numberPlural n "%es:WordCountGroup%:0" "%es:WordCountGroup%:1"
                    , ar = numberPlural n "%ar:WordCountGroup%:0" "%ar:WordCountGroup%:1"
                    , fr = numberPlural n "%fr:WordCountGroup%:0" "%fr:WordCountGroup%:1"
                    , ru = numberPlural n "%ru:WordCountGroup%:0" "%ru:WordCountGroup%:1"
                    , de = numberPlural n "%de:WordCountGroup%:0" "%de:WordCountGroup%:1"
                    , ja = numberPlural n "%ja:WordCountGroup%:0" "%ja:WordCountGroup%:1"
                    , pes = numberPlural n "%pes:WordCountGroup%:0" "%pes:WordCountGroup%:1"
                    , it = numberPlural n "%it:WordCountGroup%:0" "%it:WordCountGroup%:1"
                    , ro = numberPlural n "%ro:WordCountGroup%:0" "%ro:WordCountGroup%:1"
                    , hr = numberPlural n "%hr:WordCountGroup%:0" "%hr:WordCountGroup%:1"
                    , nl = numberPlural n "%nl:WordCountGroup%:0" "%nl:WordCountGroup%:1"
                    , hu = numberPlural n "%hu:WordCountGroup%:0" "%hu:WordCountGroup%:1"
                    , sv = numberPlural n "%sv:WordCountGroup%:0" "%sv:WordCountGroup%:1"
                    , ca = numberPlural n "%ca:WordCountGroup%:0" "%ca:WordCountGroup%:1"
                    , br = numberPlural n "%br:WordCountGroup%:0" "%br:WordCountGroup%:1"
                    , mr = numberPlural n "%mr:WordCountGroup%:0" "%mr:WordCountGroup%:1"
                    , cs = numberPlural n "%cs:WordCountGroup%:0" "%cs:WordCountGroup%:1"
                    , fa = numberPlural n "%fa:WordCountGroup%:0" "%fa:WordCountGroup%:1"
                    , id = numberPlural n "%id:WordCountGroup%:0" "%id:WordCountGroup%:1"
                    , ko = numberPlural n "%ko:WordCountGroup%:0" "%ko:WordCountGroup%:1"
                    , pl = numberPlural n "%pl:WordCountGroup%:0" "%pl:WordCountGroup%:1"
                    , uk = numberPlural n "%uk:WordCountGroup%:0" "%uk:WordCountGroup%:1"
                    , nb = numberPlural n "%nb:WordCountGroup%:0" "%nb:WordCountGroup%:1"
                    }

                WordCountColumn n ->
                    { en = numberPlural n "Column : %1 word" "Column : %1 words"
                    , zh_hans = numberPlural n "%zh_hans:WordCountColumn%:0" "%zh_hans:WordCountColumn%:1"
                    , zh_hant = numberPlural n "%zh_hant:WordCountColumn%:0" "%zh_hant:WordCountColumn%:1"
                    , es = numberPlural n "%es:WordCountColumn%:0" "%es:WordCountColumn%:1"
                    , ar = numberPlural n "%ar:WordCountColumn%:0" "%ar:WordCountColumn%:1"
                    , fr = numberPlural n "%fr:WordCountColumn%:0" "%fr:WordCountColumn%:1"
                    , ru = numberPlural n "%ru:WordCountColumn%:0" "%ru:WordCountColumn%:1"
                    , de = numberPlural n "%de:WordCountColumn%:0" "%de:WordCountColumn%:1"
                    , ja = numberPlural n "%ja:WordCountColumn%:0" "%ja:WordCountColumn%:1"
                    , pes = numberPlural n "%pes:WordCountColumn%:0" "%pes:WordCountColumn%:1"
                    , it = numberPlural n "%it:WordCountColumn%:0" "%it:WordCountColumn%:1"
                    , ro = numberPlural n "%ro:WordCountColumn%:0" "%ro:WordCountColumn%:1"
                    , hr = numberPlural n "%hr:WordCountColumn%:0" "%hr:WordCountColumn%:1"
                    , nl = numberPlural n "%nl:WordCountColumn%:0" "%nl:WordCountColumn%:1"
                    , hu = numberPlural n "%hu:WordCountColumn%:0" "%hu:WordCountColumn%:1"
                    , sv = numberPlural n "%sv:WordCountColumn%:0" "%sv:WordCountColumn%:1"
                    , ca = numberPlural n "%ca:WordCountColumn%:0" "%ca:WordCountColumn%:1"
                    , br = numberPlural n "%br:WordCountColumn%:0" "%br:WordCountColumn%:1"
                    , mr = numberPlural n "%mr:WordCountColumn%:0" "%mr:WordCountColumn%:1"
                    , cs = numberPlural n "%cs:WordCountColumn%:0" "%cs:WordCountColumn%:1"
                    , fa = numberPlural n "%fa:WordCountColumn%:0" "%fa:WordCountColumn%:1"
                    , id = numberPlural n "%id:WordCountColumn%:0" "%id:WordCountColumn%:1"
                    , ko = numberPlural n "%ko:WordCountColumn%:0" "%ko:WordCountColumn%:1"
                    , pl = numberPlural n "%pl:WordCountColumn%:0" "%pl:WordCountColumn%:1"
                    , uk = numberPlural n "%uk:WordCountColumn%:0" "%uk:WordCountColumn%:1"
                    , nb = numberPlural n "%nb:WordCountColumn%:0" "%nb:WordCountColumn%:1"
                    }

                CharacterCountCard n ->
                    { en = numberPlural n "Card : %1 character" "Card : %1 characters"
                    , zh_hans = numberPlural n "%zh_hans:CharacterCountCard%:0" "%zh_hans:CharacterCountCard%:1"
                    , zh_hant = numberPlural n "%zh_hant:CharacterCountCard%:0" "%zh_hant:CharacterCountCard%:1"
                    , es = numberPlural n "%es:CharacterCountCard%:0" "%es:CharacterCountCard%:1"
                    , ar = numberPlural n "%ar:CharacterCountCard%:0" "%ar:CharacterCountCard%:1"
                    , fr = numberPlural n "%fr:CharacterCountCard%:0" "%fr:CharacterCountCard%:1"
                    , ru = numberPlural n "%ru:CharacterCountCard%:0" "%ru:CharacterCountCard%:1"
                    , de = numberPlural n "%de:CharacterCountCard%:0" "%de:CharacterCountCard%:1"
                    , ja = numberPlural n "%ja:CharacterCountCard%:0" "%ja:CharacterCountCard%:1"
                    , pes = numberPlural n "%pes:CharacterCountCard%:0" "%pes:CharacterCountCard%:1"
                    , it = numberPlural n "%it:CharacterCountCard%:0" "%it:CharacterCountCard%:1"
                    , ro = numberPlural n "%ro:CharacterCountCard%:0" "%ro:CharacterCountCard%:1"
                    , hr = numberPlural n "%hr:CharacterCountCard%:0" "%hr:CharacterCountCard%:1"
                    , nl = numberPlural n "%nl:CharacterCountCard%:0" "%nl:CharacterCountCard%:1"
                    , hu = numberPlural n "%hu:CharacterCountCard%:0" "%hu:CharacterCountCard%:1"
                    , sv = numberPlural n "%sv:CharacterCountCard%:0" "%sv:CharacterCountCard%:1"
                    , ca = numberPlural n "%ca:CharacterCountCard%:0" "%ca:CharacterCountCard%:1"
                    , br = numberPlural n "%br:CharacterCountCard%:0" "%br:CharacterCountCard%:1"
                    , mr = numberPlural n "%mr:CharacterCountCard%:0" "%mr:CharacterCountCard%:1"
                    , cs = numberPlural n "%cs:CharacterCountCard%:0" "%cs:CharacterCountCard%:1"
                    , fa = numberPlural n "%fa:CharacterCountCard%:0" "%fa:CharacterCountCard%:1"
                    , id = numberPlural n "%id:CharacterCountCard%:0" "%id:CharacterCountCard%:1"
                    , ko = numberPlural n "%ko:CharacterCountCard%:0" "%ko:CharacterCountCard%:1"
                    , pl = numberPlural n "%pl:CharacterCountCard%:0" "%pl:CharacterCountCard%:1"
                    , uk = numberPlural n "%uk:CharacterCountCard%:0" "%uk:CharacterCountCard%:1"
                    , nb = numberPlural n "%nb:CharacterCountCard%:0" "%nb:CharacterCountCard%:1"
                    }

                CharacterCountSubtree n ->
                    { en = numberPlural n "Subtree : %1 character" "Subtree : %1 characters"
                    , zh_hans = numberPlural n "%zh_hans:CharacterCountSubtree%:0" "%zh_hans:CharacterCountSubtree%:1"
                    , zh_hant = numberPlural n "%zh_hant:CharacterCountSubtree%:0" "%zh_hant:CharacterCountSubtree%:1"
                    , es = numberPlural n "%es:CharacterCountSubtree%:0" "%es:CharacterCountSubtree%:1"
                    , ar = numberPlural n "%ar:CharacterCountSubtree%:0" "%ar:CharacterCountSubtree%:1"
                    , fr = numberPlural n "%fr:CharacterCountSubtree%:0" "%fr:CharacterCountSubtree%:1"
                    , ru = numberPlural n "%ru:CharacterCountSubtree%:0" "%ru:CharacterCountSubtree%:1"
                    , de = numberPlural n "%de:CharacterCountSubtree%:0" "%de:CharacterCountSubtree%:1"
                    , ja = numberPlural n "%ja:CharacterCountSubtree%:0" "%ja:CharacterCountSubtree%:1"
                    , pes = numberPlural n "%pes:CharacterCountSubtree%:0" "%pes:CharacterCountSubtree%:1"
                    , it = numberPlural n "%it:CharacterCountSubtree%:0" "%it:CharacterCountSubtree%:1"
                    , ro = numberPlural n "%ro:CharacterCountSubtree%:0" "%ro:CharacterCountSubtree%:1"
                    , hr = numberPlural n "%hr:CharacterCountSubtree%:0" "%hr:CharacterCountSubtree%:1"
                    , nl = numberPlural n "%nl:CharacterCountSubtree%:0" "%nl:CharacterCountSubtree%:1"
                    , hu = numberPlural n "%hu:CharacterCountSubtree%:0" "%hu:CharacterCountSubtree%:1"
                    , sv = numberPlural n "%sv:CharacterCountSubtree%:0" "%sv:CharacterCountSubtree%:1"
                    , ca = numberPlural n "%ca:CharacterCountSubtree%:0" "%ca:CharacterCountSubtree%:1"
                    , br = numberPlural n "%br:CharacterCountSubtree%:0" "%br:CharacterCountSubtree%:1"
                    , mr = numberPlural n "%mr:CharacterCountSubtree%:0" "%mr:CharacterCountSubtree%:1"
                    , cs = numberPlural n "%cs:CharacterCountSubtree%:0" "%cs:CharacterCountSubtree%:1"
                    , fa = numberPlural n "%fa:CharacterCountSubtree%:0" "%fa:CharacterCountSubtree%:1"
                    , id = numberPlural n "%id:CharacterCountSubtree%:0" "%id:CharacterCountSubtree%:1"
                    , ko = numberPlural n "%ko:CharacterCountSubtree%:0" "%ko:CharacterCountSubtree%:1"
                    , pl = numberPlural n "%pl:CharacterCountSubtree%:0" "%pl:CharacterCountSubtree%:1"
                    , uk = numberPlural n "%uk:CharacterCountSubtree%:0" "%uk:CharacterCountSubtree%:1"
                    , nb = numberPlural n "%nb:CharacterCountSubtree%:0" "%nb:CharacterCountSubtree%:1"
                    }

                CharacterCountGroup n ->
                    { en = numberPlural n "Group : %1 character" "Group : %1 characters"
                    , zh_hans = numberPlural n "%zh_hans:CharacterCountGroup%:0" "%zh_hans:CharacterCountGroup%:1"
                    , zh_hant = numberPlural n "%zh_hant:CharacterCountGroup%:0" "%zh_hant:CharacterCountGroup%:1"
                    , es = numberPlural n "%es:CharacterCountGroup%:0" "%es:CharacterCountGroup%:1"
                    , ar = numberPlural n "%ar:CharacterCountGroup%:0" "%ar:CharacterCountGroup%:1"
                    , fr = numberPlural n "%fr:CharacterCountGroup%:0" "%fr:CharacterCountGroup%:1"
                    , ru = numberPlural n "%ru:CharacterCountGroup%:0" "%ru:CharacterCountGroup%:1"
                    , de = numberPlural n "%de:CharacterCountGroup%:0" "%de:CharacterCountGroup%:1"
                    , ja = numberPlural n "%ja:CharacterCountGroup%:0" "%ja:CharacterCountGroup%:1"
                    , pes = numberPlural n "%pes:CharacterCountGroup%:0" "%pes:CharacterCountGroup%:1"
                    , it = numberPlural n "%it:CharacterCountGroup%:0" "%it:CharacterCountGroup%:1"
                    , ro = numberPlural n "%ro:CharacterCountGroup%:0" "%ro:CharacterCountGroup%:1"
                    , hr = numberPlural n "%hr:CharacterCountGroup%:0" "%hr:CharacterCountGroup%:1"
                    , nl = numberPlural n "%nl:CharacterCountGroup%:0" "%nl:CharacterCountGroup%:1"
                    , hu = numberPlural n "%hu:CharacterCountGroup%:0" "%hu:CharacterCountGroup%:1"
                    , sv = numberPlural n "%sv:CharacterCountGroup%:0" "%sv:CharacterCountGroup%:1"
                    , ca = numberPlural n "%ca:CharacterCountGroup%:0" "%ca:CharacterCountGroup%:1"
                    , br = numberPlural n "%br:CharacterCountGroup%:0" "%br:CharacterCountGroup%:1"
                    , mr = numberPlural n "%mr:CharacterCountGroup%:0" "%mr:CharacterCountGroup%:1"
                    , cs = numberPlural n "%cs:CharacterCountGroup%:0" "%cs:CharacterCountGroup%:1"
                    , fa = numberPlural n "%fa:CharacterCountGroup%:0" "%fa:CharacterCountGroup%:1"
                    , id = numberPlural n "%id:CharacterCountGroup%:0" "%id:CharacterCountGroup%:1"
                    , ko = numberPlural n "%ko:CharacterCountGroup%:0" "%ko:CharacterCountGroup%:1"
                    , pl = numberPlural n "%pl:CharacterCountGroup%:0" "%pl:CharacterCountGroup%:1"
                    , uk = numberPlural n "%uk:CharacterCountGroup%:0" "%uk:CharacterCountGroup%:1"
                    , nb = numberPlural n "%nb:CharacterCountGroup%:0" "%nb:CharacterCountGroup%:1"
                    }

                CharacterCountColumn n ->
                    { en = numberPlural n "Column : %1 character" "Column : %1 characters"
                    , zh_hans = numberPlural n "%zh_hans:CharacterCountColumn%:0" "%zh_hans:CharacterCountColumn%:1"
                    , zh_hant = numberPlural n "%zh_hant:CharacterCountColumn%:0" "%zh_hant:CharacterCountColumn%:1"
                    , es = numberPlural n "%es:CharacterCountColumn%:0" "%es:CharacterCountColumn%:1"
                    , ar = numberPlural n "%ar:CharacterCountColumn%:0" "%ar:CharacterCountColumn%:1"
                    , fr = numberPlural n "%fr:CharacterCountColumn%:0" "%fr:CharacterCountColumn%:1"
                    , ru = numberPlural n "%ru:CharacterCountColumn%:0" "%ru:CharacterCountColumn%:1"
                    , de = numberPlural n "%de:CharacterCountColumn%:0" "%de:CharacterCountColumn%:1"
                    , ja = numberPlural n "%ja:CharacterCountColumn%:0" "%ja:CharacterCountColumn%:1"
                    , pes = numberPlural n "%pes:CharacterCountColumn%:0" "%pes:CharacterCountColumn%:1"
                    , it = numberPlural n "%it:CharacterCountColumn%:0" "%it:CharacterCountColumn%:1"
                    , ro = numberPlural n "%ro:CharacterCountColumn%:0" "%ro:CharacterCountColumn%:1"
                    , hr = numberPlural n "%hr:CharacterCountColumn%:0" "%hr:CharacterCountColumn%:1"
                    , nl = numberPlural n "%nl:CharacterCountColumn%:0" "%nl:CharacterCountColumn%:1"
                    , hu = numberPlural n "%hu:CharacterCountColumn%:0" "%hu:CharacterCountColumn%:1"
                    , sv = numberPlural n "%sv:CharacterCountColumn%:0" "%sv:CharacterCountColumn%:1"
                    , ca = numberPlural n "%ca:CharacterCountColumn%:0" "%ca:CharacterCountColumn%:1"
                    , br = numberPlural n "%br:CharacterCountColumn%:0" "%br:CharacterCountColumn%:1"
                    , mr = numberPlural n "%mr:CharacterCountColumn%:0" "%mr:CharacterCountColumn%:1"
                    , cs = numberPlural n "%cs:CharacterCountColumn%:0" "%cs:CharacterCountColumn%:1"
                    , fa = numberPlural n "%fa:CharacterCountColumn%:0" "%fa:CharacterCountColumn%:1"
                    , id = numberPlural n "%id:CharacterCountColumn%:0" "%id:CharacterCountColumn%:1"
                    , ko = numberPlural n "%ko:CharacterCountColumn%:0" "%ko:CharacterCountColumn%:1"
                    , pl = numberPlural n "%pl:CharacterCountColumn%:0" "%pl:CharacterCountColumn%:1"
                    , uk = numberPlural n "%uk:CharacterCountColumn%:0" "%uk:CharacterCountColumn%:1"
                    , nb = numberPlural n "%nb:CharacterCountColumn%:0" "%nb:CharacterCountColumn%:1"
                    }

                CharacterCountTotal n ->
                    { en = numberPlural n "Total : %1 character" "Total : %1 characters"
                    , zh_hans = numberPlural n "%zh_hans:CharacterCountTotal%:0" "%zh_hans:CharacterCountTotal%:1"
                    , zh_hant = numberPlural n "%zh_hant:CharacterCountTotal%:0" "%zh_hant:CharacterCountTotal%:1"
                    , es = numberPlural n "%es:CharacterCountTotal%:0" "%es:CharacterCountTotal%:1"
                    , ar = numberPlural n "%ar:CharacterCountTotal%:0" "%ar:CharacterCountTotal%:1"
                    , fr = numberPlural n "%fr:CharacterCountTotal%:0" "%fr:CharacterCountTotal%:1"
                    , ru = numberPlural n "%ru:CharacterCountTotal%:0" "%ru:CharacterCountTotal%:1"
                    , de = numberPlural n "%de:CharacterCountTotal%:0" "%de:CharacterCountTotal%:1"
                    , ja = numberPlural n "%ja:CharacterCountTotal%:0" "%ja:CharacterCountTotal%:1"
                    , pes = numberPlural n "%pes:CharacterCountTotal%:0" "%pes:CharacterCountTotal%:1"
                    , it = numberPlural n "%it:CharacterCountTotal%:0" "%it:CharacterCountTotal%:1"
                    , ro = numberPlural n "%ro:CharacterCountTotal%:0" "%ro:CharacterCountTotal%:1"
                    , hr = numberPlural n "%hr:CharacterCountTotal%:0" "%hr:CharacterCountTotal%:1"
                    , nl = numberPlural n "%nl:CharacterCountTotal%:0" "%nl:CharacterCountTotal%:1"
                    , hu = numberPlural n "%hu:CharacterCountTotal%:0" "%hu:CharacterCountTotal%:1"
                    , sv = numberPlural n "%sv:CharacterCountTotal%:0" "%sv:CharacterCountTotal%:1"
                    , ca = numberPlural n "%ca:CharacterCountTotal%:0" "%ca:CharacterCountTotal%:1"
                    , br = numberPlural n "%br:CharacterCountTotal%:0" "%br:CharacterCountTotal%:1"
                    , mr = numberPlural n "%mr:CharacterCountTotal%:0" "%mr:CharacterCountTotal%:1"
                    , cs = numberPlural n "%cs:CharacterCountTotal%:0" "%cs:CharacterCountTotal%:1"
                    , fa = numberPlural n "%fa:CharacterCountTotal%:0" "%fa:CharacterCountTotal%:1"
                    , id = numberPlural n "%id:CharacterCountTotal%:0" "%id:CharacterCountTotal%:1"
                    , ko = numberPlural n "%ko:CharacterCountTotal%:0" "%ko:CharacterCountTotal%:1"
                    , pl = numberPlural n "%pl:CharacterCountTotal%:0" "%pl:CharacterCountTotal%:1"
                    , uk = numberPlural n "%uk:CharacterCountTotal%:0" "%uk:CharacterCountTotal%:1"
                    , nb = numberPlural n "%nb:CharacterCountTotal%:0" "%nb:CharacterCountTotal%:1"
                    }

                WordCountTotalCards n ->
                    { en = numberPlural n "Total Cards in Tree : %1" "Total Cards in Tree : %1"
                    , zh_hans = numberPlural n "%zh_hans:WordCountTotalCards%:0" "%zh_hans:WordCountTotalCards%:1"
                    , zh_hant = numberPlural n "%zh_hant:WordCountTotalCards%:0" "%zh_hant:WordCountTotalCards%:1"
                    , es = numberPlural n "%es:WordCountTotalCards%:0" "%es:WordCountTotalCards%:1"
                    , ar = numberPlural n "%ar:WordCountTotalCards%:0" "%ar:WordCountTotalCards%:1"
                    , fr = numberPlural n "%fr:WordCountTotalCards%:0" "%fr:WordCountTotalCards%:1"
                    , ru = numberPlural n "%ru:WordCountTotalCards%:0" "%ru:WordCountTotalCards%:1"
                    , de = numberPlural n "%de:WordCountTotalCards%:0" "%de:WordCountTotalCards%:1"
                    , ja = numberPlural n "%ja:WordCountTotalCards%:0" "%ja:WordCountTotalCards%:1"
                    , pes = numberPlural n "%pes:WordCountTotalCards%:0" "%pes:WordCountTotalCards%:1"
                    , it = numberPlural n "%it:WordCountTotalCards%:0" "%it:WordCountTotalCards%:1"
                    , ro = numberPlural n "%ro:WordCountTotalCards%:0" "%ro:WordCountTotalCards%:1"
                    , hr = numberPlural n "%hr:WordCountTotalCards%:0" "%hr:WordCountTotalCards%:1"
                    , nl = numberPlural n "%nl:WordCountTotalCards%:0" "%nl:WordCountTotalCards%:1"
                    , hu = numberPlural n "%hu:WordCountTotalCards%:0" "%hu:WordCountTotalCards%:1"
                    , sv = numberPlural n "%sv:WordCountTotalCards%:0" "%sv:WordCountTotalCards%:1"
                    , ca = numberPlural n "%ca:WordCountTotalCards%:0" "%ca:WordCountTotalCards%:1"
                    , br = numberPlural n "%br:WordCountTotalCards%:0" "%br:WordCountTotalCards%:1"
                    , mr = numberPlural n "%mr:WordCountTotalCards%:0" "%mr:WordCountTotalCards%:1"
                    , cs = numberPlural n "%cs:WordCountTotalCards%:0" "%cs:WordCountTotalCards%:1"
                    , fa = numberPlural n "%fa:WordCountTotalCards%:0" "%fa:WordCountTotalCards%:1"
                    , id = numberPlural n "%id:WordCountTotalCards%:0" "%id:WordCountTotalCards%:1"
                    , ko = numberPlural n "%ko:WordCountTotalCards%:0" "%ko:WordCountTotalCards%:1"
                    , pl = numberPlural n "%pl:WordCountTotalCards%:0" "%pl:WordCountTotalCards%:1"
                    , uk = numberPlural n "%uk:WordCountTotalCards%:0" "%uk:WordCountTotalCards%:1"
                    , nb = numberPlural n "%nb:WordCountTotalCards%:0" "%nb:WordCountTotalCards%:1"
                    }

                DocumentTheme ->
                    { en = "Document Theme"
                    , zh_hans = "%zh_hans:DocumentTheme%"
                    , zh_hant = "%zh_hant:DocumentTheme%"
                    , es = "%es:DocumentTheme%"
                    , ar = "%ar:DocumentTheme%"
                    , fr = "%fr:DocumentTheme%"
                    , ru = "%ru:DocumentTheme%"
                    , de = "%de:DocumentTheme%"
                    , ja = "%ja:DocumentTheme%"
                    , pes = "%pes:DocumentTheme%"
                    , it = "%it:DocumentTheme%"
                    , ro = "%ro:DocumentTheme%"
                    , hr = "%hr:DocumentTheme%"
                    , nl = "%nl:DocumentTheme%"
                    , hu = "%hu:DocumentTheme%"
                    , sv = "%sv:DocumentTheme%"
                    , ca = "%ca:DocumentTheme%"
                    , br = "%br:DocumentTheme%"
                    , mr = "%mr:DocumentTheme%"
                    , cs = "%cs:DocumentTheme%"
                    , fa = "%fa:DocumentTheme%"
                    , id = "%id:DocumentTheme%"
                    , ko = "%ko:DocumentTheme%"
                    , pl = "%pl:DocumentTheme%"
                    , uk = "%uk:DocumentTheme%"
                    , nb = "%nb:DocumentTheme%"
                    }

                ThemeDefault ->
                    { en = "Default"
                    , zh_hans = "%zh_hans:ThemeDefault%"
                    , zh_hant = "%zh_hant:ThemeDefault%"
                    , es = "%es:ThemeDefault%"
                    , ar = "%ar:ThemeDefault%"
                    , fr = "%fr:ThemeDefault%"
                    , ru = "%ru:ThemeDefault%"
                    , de = "%de:ThemeDefault%"
                    , ja = "%ja:ThemeDefault%"
                    , pes = "%pes:ThemeDefault%"
                    , it = "%it:ThemeDefault%"
                    , ro = "%ro:ThemeDefault%"
                    , hr = "%hr:ThemeDefault%"
                    , nl = "%nl:ThemeDefault%"
                    , hu = "%hu:ThemeDefault%"
                    , sv = "%sv:ThemeDefault%"
                    , ca = "%ca:ThemeDefault%"
                    , br = "%br:ThemeDefault%"
                    , mr = "%mr:ThemeDefault%"
                    , cs = "%cs:ThemeDefault%"
                    , fa = "%fa:ThemeDefault%"
                    , id = "%id:ThemeDefault%"
                    , ko = "%ko:ThemeDefault%"
                    , pl = "%pl:ThemeDefault%"
                    , uk = "%uk:ThemeDefault%"
                    , nb = "%nb:ThemeDefault%"
                    }

                ThemeDarkMode ->
                    { en = "Dark Mode"
                    , zh_hans = "%zh_hans:ThemeDarkMode%"
                    , zh_hant = "%zh_hant:ThemeDarkMode%"
                    , es = "%es:ThemeDarkMode%"
                    , ar = "%ar:ThemeDarkMode%"
                    , fr = "%fr:ThemeDarkMode%"
                    , ru = "%ru:ThemeDarkMode%"
                    , de = "%de:ThemeDarkMode%"
                    , ja = "%ja:ThemeDarkMode%"
                    , pes = "%pes:ThemeDarkMode%"
                    , it = "%it:ThemeDarkMode%"
                    , ro = "%ro:ThemeDarkMode%"
                    , hr = "%hr:ThemeDarkMode%"
                    , nl = "%nl:ThemeDarkMode%"
                    , hu = "%hu:ThemeDarkMode%"
                    , sv = "%sv:ThemeDarkMode%"
                    , ca = "%ca:ThemeDarkMode%"
                    , br = "%br:ThemeDarkMode%"
                    , mr = "%mr:ThemeDarkMode%"
                    , cs = "%cs:ThemeDarkMode%"
                    , fa = "%fa:ThemeDarkMode%"
                    , id = "%id:ThemeDarkMode%"
                    , ko = "%ko:ThemeDarkMode%"
                    , pl = "%pl:ThemeDarkMode%"
                    , uk = "%uk:ThemeDarkMode%"
                    , nb = "%nb:ThemeDarkMode%"
                    }

                ThemeClassic ->
                    { en = "Classic Gingkoapp"
                    , zh_hans = "%zh_hans:ThemeClassic%"
                    , zh_hant = "%zh_hant:ThemeClassic%"
                    , es = "%es:ThemeClassic%"
                    , ar = "%ar:ThemeClassic%"
                    , fr = "%fr:ThemeClassic%"
                    , ru = "%ru:ThemeClassic%"
                    , de = "%de:ThemeClassic%"
                    , ja = "%ja:ThemeClassic%"
                    , pes = "%pes:ThemeClassic%"
                    , it = "%it:ThemeClassic%"
                    , ro = "%ro:ThemeClassic%"
                    , hr = "%hr:ThemeClassic%"
                    , nl = "%nl:ThemeClassic%"
                    , hu = "%hu:ThemeClassic%"
                    , sv = "%sv:ThemeClassic%"
                    , ca = "%ca:ThemeClassic%"
                    , br = "%br:ThemeClassic%"
                    , mr = "%mr:ThemeClassic%"
                    , cs = "%cs:ThemeClassic%"
                    , fa = "%fa:ThemeClassic%"
                    , id = "%id:ThemeClassic%"
                    , ko = "%ko:ThemeClassic%"
                    , pl = "%pl:ThemeClassic%"
                    , uk = "%uk:ThemeClassic%"
                    , nb = "%nb:ThemeClassic%"
                    }

                ThemeGray ->
                    { en = "Gray"
                    , zh_hans = "%zh_hans:ThemeGray%"
                    , zh_hant = "%zh_hant:ThemeGray%"
                    , es = "%es:ThemeGray%"
                    , ar = "%ar:ThemeGray%"
                    , fr = "%fr:ThemeGray%"
                    , ru = "%ru:ThemeGray%"
                    , de = "%de:ThemeGray%"
                    , ja = "%ja:ThemeGray%"
                    , pes = "%pes:ThemeGray%"
                    , it = "%it:ThemeGray%"
                    , ro = "%ro:ThemeGray%"
                    , hr = "%hr:ThemeGray%"
                    , nl = "%nl:ThemeGray%"
                    , hu = "%hu:ThemeGray%"
                    , sv = "%sv:ThemeGray%"
                    , ca = "%ca:ThemeGray%"
                    , br = "%br:ThemeGray%"
                    , mr = "%mr:ThemeGray%"
                    , cs = "%cs:ThemeGray%"
                    , fa = "%fa:ThemeGray%"
                    , id = "%id:ThemeGray%"
                    , ko = "%ko:ThemeGray%"
                    , pl = "%pl:ThemeGray%"
                    , uk = "%uk:ThemeGray%"
                    , nb = "%nb:ThemeGray%"
                    }

                ThemeGreen ->
                    { en = "Green"
                    , zh_hans = "%zh_hans:ThemeGreen%"
                    , zh_hant = "%zh_hant:ThemeGreen%"
                    , es = "%es:ThemeGreen%"
                    , ar = "%ar:ThemeGreen%"
                    , fr = "%fr:ThemeGreen%"
                    , ru = "%ru:ThemeGreen%"
                    , de = "%de:ThemeGreen%"
                    , ja = "%ja:ThemeGreen%"
                    , pes = "%pes:ThemeGreen%"
                    , it = "%it:ThemeGreen%"
                    , ro = "%ro:ThemeGreen%"
                    , hr = "%hr:ThemeGreen%"
                    , nl = "%nl:ThemeGreen%"
                    , hu = "%hu:ThemeGreen%"
                    , sv = "%sv:ThemeGreen%"
                    , ca = "%ca:ThemeGreen%"
                    , br = "%br:ThemeGreen%"
                    , mr = "%mr:ThemeGreen%"
                    , cs = "%cs:ThemeGreen%"
                    , fa = "%fa:ThemeGreen%"
                    , id = "%id:ThemeGreen%"
                    , ko = "%ko:ThemeGreen%"
                    , pl = "%pl:ThemeGreen%"
                    , uk = "%uk:ThemeGreen%"
                    , nb = "%nb:ThemeGreen%"
                    }

                ThemeTurquoise ->
                    { en = "Turquoise"
                    , zh_hans = "%zh_hans:ThemeTurquoise%"
                    , zh_hant = "%zh_hant:ThemeTurquoise%"
                    , es = "%es:ThemeTurquoise%"
                    , ar = "%ar:ThemeTurquoise%"
                    , fr = "%fr:ThemeTurquoise%"
                    , ru = "%ru:ThemeTurquoise%"
                    , de = "%de:ThemeTurquoise%"
                    , ja = "%ja:ThemeTurquoise%"
                    , pes = "%pes:ThemeTurquoise%"
                    , it = "%it:ThemeTurquoise%"
                    , ro = "%ro:ThemeTurquoise%"
                    , hr = "%hr:ThemeTurquoise%"
                    , nl = "%nl:ThemeTurquoise%"
                    , hu = "%hu:ThemeTurquoise%"
                    , sv = "%sv:ThemeTurquoise%"
                    , ca = "%ca:ThemeTurquoise%"
                    , br = "%br:ThemeTurquoise%"
                    , mr = "%mr:ThemeTurquoise%"
                    , cs = "%cs:ThemeTurquoise%"
                    , fa = "%fa:ThemeTurquoise%"
                    , id = "%id:ThemeTurquoise%"
                    , ko = "%ko:ThemeTurquoise%"
                    , pl = "%pl:ThemeTurquoise%"
                    , uk = "%uk:ThemeTurquoise%"
                    , nb = "%nb:ThemeTurquoise%"
                    }

                -- Exporting
                ExportOrPrint ->
                    { en = "Export or Print"
                    , zh_hans = "%zh_hans:ExportOrPrint%"
                    , zh_hant = "%zh_hant:ExportOrPrint%"
                    , es = "%es:ExportOrPrint%"
                    , ar = "%ar:ExportOrPrint%"
                    , fr = "%fr:ExportOrPrint%"
                    , ru = "%ru:ExportOrPrint%"
                    , de = "%de:ExportOrPrint%"
                    , ja = "%ja:ExportOrPrint%"
                    , pes = "%pes:ExportOrPrint%"
                    , it = "%it:ExportOrPrint%"
                    , ro = "%ro:ExportOrPrint%"
                    , hr = "%hr:ExportOrPrint%"
                    , nl = "%nl:ExportOrPrint%"
                    , hu = "%hu:ExportOrPrint%"
                    , sv = "%sv:ExportOrPrint%"
                    , ca = "%ca:ExportOrPrint%"
                    , br = "%br:ExportOrPrint%"
                    , mr = "%mr:ExportOrPrint%"
                    , cs = "%cs:ExportOrPrint%"
                    , fa = "%fa:ExportOrPrint%"
                    , id = "%id:ExportOrPrint%"
                    , ko = "%ko:ExportOrPrint%"
                    , pl = "%pl:ExportOrPrint%"
                    , uk = "%uk:ExportOrPrint%"
                    , nb = "%nb:ExportOrPrint%"
                    }

                ExportSettingEverything ->
                    { en = "Everything"
                    , zh_hans = "%zh_hans:ExportSettingEverything%"
                    , zh_hant = "%zh_hant:ExportSettingEverything%"
                    , es = "%es:ExportSettingEverything%"
                    , ar = "%ar:ExportSettingEverything%"
                    , fr = "%fr:ExportSettingEverything%"
                    , ru = "%ru:ExportSettingEverything%"
                    , de = "%de:ExportSettingEverything%"
                    , ja = "%ja:ExportSettingEverything%"
                    , pes = "%pes:ExportSettingEverything%"
                    , it = "%it:ExportSettingEverything%"
                    , ro = "%ro:ExportSettingEverything%"
                    , hr = "%hr:ExportSettingEverything%"
                    , nl = "%nl:ExportSettingEverything%"
                    , hu = "%hu:ExportSettingEverything%"
                    , sv = "%sv:ExportSettingEverything%"
                    , ca = "%ca:ExportSettingEverything%"
                    , br = "%br:ExportSettingEverything%"
                    , mr = "%mr:ExportSettingEverything%"
                    , cs = "%cs:ExportSettingEverything%"
                    , fa = "%fa:ExportSettingEverything%"
                    , id = "%id:ExportSettingEverything%"
                    , ko = "%ko:ExportSettingEverything%"
                    , pl = "%pl:ExportSettingEverything%"
                    , uk = "%uk:ExportSettingEverything%"
                    , nb = "%nb:ExportSettingEverything%"
                    }

                ExportSettingEverythingDesc ->
                    { en = "All cards in the tree (in depth-first order)"
                    , zh_hans = "%zh_hans:ExportSettingEverythingDesc%"
                    , zh_hant = "%zh_hant:ExportSettingEverythingDesc%"
                    , es = "%es:ExportSettingEverythingDesc%"
                    , ar = "%ar:ExportSettingEverythingDesc%"
                    , fr = "%fr:ExportSettingEverythingDesc%"
                    , ru = "%ru:ExportSettingEverythingDesc%"
                    , de = "%de:ExportSettingEverythingDesc%"
                    , ja = "%ja:ExportSettingEverythingDesc%"
                    , pes = "%pes:ExportSettingEverythingDesc%"
                    , it = "%it:ExportSettingEverythingDesc%"
                    , ro = "%ro:ExportSettingEverythingDesc%"
                    , hr = "%hr:ExportSettingEverythingDesc%"
                    , nl = "%nl:ExportSettingEverythingDesc%"
                    , hu = "%hu:ExportSettingEverythingDesc%"
                    , sv = "%sv:ExportSettingEverythingDesc%"
                    , ca = "%ca:ExportSettingEverythingDesc%"
                    , br = "%br:ExportSettingEverythingDesc%"
                    , mr = "%mr:ExportSettingEverythingDesc%"
                    , cs = "%cs:ExportSettingEverythingDesc%"
                    , fa = "%fa:ExportSettingEverythingDesc%"
                    , id = "%id:ExportSettingEverythingDesc%"
                    , ko = "%ko:ExportSettingEverythingDesc%"
                    , pl = "%pl:ExportSettingEverythingDesc%"
                    , uk = "%uk:ExportSettingEverythingDesc%"
                    , nb = "%nb:ExportSettingEverythingDesc%"
                    }

                ExportSettingCurrentSubtree ->
                    { en = "Current Subtree"
                    , zh_hans = "%zh_hans:ExportSettingCurrentSubtree%"
                    , zh_hant = "%zh_hant:ExportSettingCurrentSubtree%"
                    , es = "%es:ExportSettingCurrentSubtree%"
                    , ar = "%ar:ExportSettingCurrentSubtree%"
                    , fr = "%fr:ExportSettingCurrentSubtree%"
                    , ru = "%ru:ExportSettingCurrentSubtree%"
                    , de = "%de:ExportSettingCurrentSubtree%"
                    , ja = "%ja:ExportSettingCurrentSubtree%"
                    , pes = "%pes:ExportSettingCurrentSubtree%"
                    , it = "%it:ExportSettingCurrentSubtree%"
                    , ro = "%ro:ExportSettingCurrentSubtree%"
                    , hr = "%hr:ExportSettingCurrentSubtree%"
                    , nl = "%nl:ExportSettingCurrentSubtree%"
                    , hu = "%hu:ExportSettingCurrentSubtree%"
                    , sv = "%sv:ExportSettingCurrentSubtree%"
                    , ca = "%ca:ExportSettingCurrentSubtree%"
                    , br = "%br:ExportSettingCurrentSubtree%"
                    , mr = "%mr:ExportSettingCurrentSubtree%"
                    , cs = "%cs:ExportSettingCurrentSubtree%"
                    , fa = "%fa:ExportSettingCurrentSubtree%"
                    , id = "%id:ExportSettingCurrentSubtree%"
                    , ko = "%ko:ExportSettingCurrentSubtree%"
                    , pl = "%pl:ExportSettingCurrentSubtree%"
                    , uk = "%uk:ExportSettingCurrentSubtree%"
                    , nb = "%nb:ExportSettingCurrentSubtree%"
                    }

                ExportSettingCurrentSubtreeDesc ->
                    { en = "Current card and all its children"
                    , zh_hans = "%zh_hans:ExportSettingCurrentSubtreeDesc%"
                    , zh_hant = "%zh_hant:ExportSettingCurrentSubtreeDesc%"
                    , es = "%es:ExportSettingCurrentSubtreeDesc%"
                    , ar = "%ar:ExportSettingCurrentSubtreeDesc%"
                    , fr = "%fr:ExportSettingCurrentSubtreeDesc%"
                    , ru = "%ru:ExportSettingCurrentSubtreeDesc%"
                    , de = "%de:ExportSettingCurrentSubtreeDesc%"
                    , ja = "%ja:ExportSettingCurrentSubtreeDesc%"
                    , pes = "%pes:ExportSettingCurrentSubtreeDesc%"
                    , it = "%it:ExportSettingCurrentSubtreeDesc%"
                    , ro = "%ro:ExportSettingCurrentSubtreeDesc%"
                    , hr = "%hr:ExportSettingCurrentSubtreeDesc%"
                    , nl = "%nl:ExportSettingCurrentSubtreeDesc%"
                    , hu = "%hu:ExportSettingCurrentSubtreeDesc%"
                    , sv = "%sv:ExportSettingCurrentSubtreeDesc%"
                    , ca = "%ca:ExportSettingCurrentSubtreeDesc%"
                    , br = "%br:ExportSettingCurrentSubtreeDesc%"
                    , mr = "%mr:ExportSettingCurrentSubtreeDesc%"
                    , cs = "%cs:ExportSettingCurrentSubtreeDesc%"
                    , fa = "%fa:ExportSettingCurrentSubtreeDesc%"
                    , id = "%id:ExportSettingCurrentSubtreeDesc%"
                    , ko = "%ko:ExportSettingCurrentSubtreeDesc%"
                    , pl = "%pl:ExportSettingCurrentSubtreeDesc%"
                    , uk = "%uk:ExportSettingCurrentSubtreeDesc%"
                    , nb = "%nb:ExportSettingCurrentSubtreeDesc%"
                    }

                ExportSettingLeavesOnly ->
                    { en = "Leaves-only"
                    , zh_hans = "%zh_hans:ExportSettingLeavesOnly%"
                    , zh_hant = "%zh_hant:ExportSettingLeavesOnly%"
                    , es = "%es:ExportSettingLeavesOnly%"
                    , ar = "%ar:ExportSettingLeavesOnly%"
                    , fr = "%fr:ExportSettingLeavesOnly%"
                    , ru = "%ru:ExportSettingLeavesOnly%"
                    , de = "%de:ExportSettingLeavesOnly%"
                    , ja = "%ja:ExportSettingLeavesOnly%"
                    , pes = "%pes:ExportSettingLeavesOnly%"
                    , it = "%it:ExportSettingLeavesOnly%"
                    , ro = "%ro:ExportSettingLeavesOnly%"
                    , hr = "%hr:ExportSettingLeavesOnly%"
                    , nl = "%nl:ExportSettingLeavesOnly%"
                    , hu = "%hu:ExportSettingLeavesOnly%"
                    , sv = "%sv:ExportSettingLeavesOnly%"
                    , ca = "%ca:ExportSettingLeavesOnly%"
                    , br = "%br:ExportSettingLeavesOnly%"
                    , mr = "%mr:ExportSettingLeavesOnly%"
                    , cs = "%cs:ExportSettingLeavesOnly%"
                    , fa = "%fa:ExportSettingLeavesOnly%"
                    , id = "%id:ExportSettingLeavesOnly%"
                    , ko = "%ko:ExportSettingLeavesOnly%"
                    , pl = "%pl:ExportSettingLeavesOnly%"
                    , uk = "%uk:ExportSettingLeavesOnly%"
                    , nb = "%nb:ExportSettingLeavesOnly%"
                    }

                ExportSettingLeavesOnlyDesc ->
                    { en = "Only cards without children"
                    , zh_hans = "%zh_hans:ExportSettingLeavesOnlyDesc%"
                    , zh_hant = "%zh_hant:ExportSettingLeavesOnlyDesc%"
                    , es = "%es:ExportSettingLeavesOnlyDesc%"
                    , ar = "%ar:ExportSettingLeavesOnlyDesc%"
                    , fr = "%fr:ExportSettingLeavesOnlyDesc%"
                    , ru = "%ru:ExportSettingLeavesOnlyDesc%"
                    , de = "%de:ExportSettingLeavesOnlyDesc%"
                    , ja = "%ja:ExportSettingLeavesOnlyDesc%"
                    , pes = "%pes:ExportSettingLeavesOnlyDesc%"
                    , it = "%it:ExportSettingLeavesOnlyDesc%"
                    , ro = "%ro:ExportSettingLeavesOnlyDesc%"
                    , hr = "%hr:ExportSettingLeavesOnlyDesc%"
                    , nl = "%nl:ExportSettingLeavesOnlyDesc%"
                    , hu = "%hu:ExportSettingLeavesOnlyDesc%"
                    , sv = "%sv:ExportSettingLeavesOnlyDesc%"
                    , ca = "%ca:ExportSettingLeavesOnlyDesc%"
                    , br = "%br:ExportSettingLeavesOnlyDesc%"
                    , mr = "%mr:ExportSettingLeavesOnlyDesc%"
                    , cs = "%cs:ExportSettingLeavesOnlyDesc%"
                    , fa = "%fa:ExportSettingLeavesOnlyDesc%"
                    , id = "%id:ExportSettingLeavesOnlyDesc%"
                    , ko = "%ko:ExportSettingLeavesOnlyDesc%"
                    , pl = "%pl:ExportSettingLeavesOnlyDesc%"
                    , uk = "%uk:ExportSettingLeavesOnlyDesc%"
                    , nb = "%nb:ExportSettingLeavesOnlyDesc%"
                    }

                ExportSettingCurrentColumn ->
                    { en = "Current Column"
                    , zh_hans = "%zh_hans:ExportSettingCurrentColumn%"
                    , zh_hant = "%zh_hant:ExportSettingCurrentColumn%"
                    , es = "%es:ExportSettingCurrentColumn%"
                    , ar = "%ar:ExportSettingCurrentColumn%"
                    , fr = "%fr:ExportSettingCurrentColumn%"
                    , ru = "%ru:ExportSettingCurrentColumn%"
                    , de = "%de:ExportSettingCurrentColumn%"
                    , ja = "%ja:ExportSettingCurrentColumn%"
                    , pes = "%pes:ExportSettingCurrentColumn%"
                    , it = "%it:ExportSettingCurrentColumn%"
                    , ro = "%ro:ExportSettingCurrentColumn%"
                    , hr = "%hr:ExportSettingCurrentColumn%"
                    , nl = "%nl:ExportSettingCurrentColumn%"
                    , hu = "%hu:ExportSettingCurrentColumn%"
                    , sv = "%sv:ExportSettingCurrentColumn%"
                    , ca = "%ca:ExportSettingCurrentColumn%"
                    , br = "%br:ExportSettingCurrentColumn%"
                    , mr = "%mr:ExportSettingCurrentColumn%"
                    , cs = "%cs:ExportSettingCurrentColumn%"
                    , fa = "%fa:ExportSettingCurrentColumn%"
                    , id = "%id:ExportSettingCurrentColumn%"
                    , ko = "%ko:ExportSettingCurrentColumn%"
                    , pl = "%pl:ExportSettingCurrentColumn%"
                    , uk = "%uk:ExportSettingCurrentColumn%"
                    , nb = "%nb:ExportSettingCurrentColumn%"
                    }

                ExportSettingCurrentColumnDesc ->
                    { en = "Only cards in the current (vertical) column"
                    , zh_hans = "%zh_hans:ExportSettingCurrentColumnDesc%"
                    , zh_hant = "%zh_hant:ExportSettingCurrentColumnDesc%"
                    , es = "%es:ExportSettingCurrentColumnDesc%"
                    , ar = "%ar:ExportSettingCurrentColumnDesc%"
                    , fr = "%fr:ExportSettingCurrentColumnDesc%"
                    , ru = "%ru:ExportSettingCurrentColumnDesc%"
                    , de = "%de:ExportSettingCurrentColumnDesc%"
                    , ja = "%ja:ExportSettingCurrentColumnDesc%"
                    , pes = "%pes:ExportSettingCurrentColumnDesc%"
                    , it = "%it:ExportSettingCurrentColumnDesc%"
                    , ro = "%ro:ExportSettingCurrentColumnDesc%"
                    , hr = "%hr:ExportSettingCurrentColumnDesc%"
                    , nl = "%nl:ExportSettingCurrentColumnDesc%"
                    , hu = "%hu:ExportSettingCurrentColumnDesc%"
                    , sv = "%sv:ExportSettingCurrentColumnDesc%"
                    , ca = "%ca:ExportSettingCurrentColumnDesc%"
                    , br = "%br:ExportSettingCurrentColumnDesc%"
                    , mr = "%mr:ExportSettingCurrentColumnDesc%"
                    , cs = "%cs:ExportSettingCurrentColumnDesc%"
                    , fa = "%fa:ExportSettingCurrentColumnDesc%"
                    , id = "%id:ExportSettingCurrentColumnDesc%"
                    , ko = "%ko:ExportSettingCurrentColumnDesc%"
                    , pl = "%pl:ExportSettingCurrentColumnDesc%"
                    , uk = "%uk:ExportSettingCurrentColumnDesc%"
                    , nb = "%nb:ExportSettingCurrentColumnDesc%"
                    }

                ExportSettingWord ->
                    { en = "Word"
                    , zh_hans = "%zh_hans:ExportSettingWord%"
                    , zh_hant = "%zh_hant:ExportSettingWord%"
                    , es = "%es:ExportSettingWord%"
                    , ar = "%ar:ExportSettingWord%"
                    , fr = "%fr:ExportSettingWord%"
                    , ru = "%ru:ExportSettingWord%"
                    , de = "%de:ExportSettingWord%"
                    , ja = "%ja:ExportSettingWord%"
                    , pes = "%pes:ExportSettingWord%"
                    , it = "%it:ExportSettingWord%"
                    , ro = "%ro:ExportSettingWord%"
                    , hr = "%hr:ExportSettingWord%"
                    , nl = "%nl:ExportSettingWord%"
                    , hu = "%hu:ExportSettingWord%"
                    , sv = "%sv:ExportSettingWord%"
                    , ca = "%ca:ExportSettingWord%"
                    , br = "%br:ExportSettingWord%"
                    , mr = "%mr:ExportSettingWord%"
                    , cs = "%cs:ExportSettingWord%"
                    , fa = "%fa:ExportSettingWord%"
                    , id = "%id:ExportSettingWord%"
                    , ko = "%ko:ExportSettingWord%"
                    , pl = "%pl:ExportSettingWord%"
                    , uk = "%uk:ExportSettingWord%"
                    , nb = "%nb:ExportSettingWord%"
                    }

                ExportSettingPlainText ->
                    { en = "Plain Text"
                    , zh_hans = "%zh_hans:ExportSettingPlainText%"
                    , zh_hant = "%zh_hant:ExportSettingPlainText%"
                    , es = "%es:ExportSettingPlainText%"
                    , ar = "%ar:ExportSettingPlainText%"
                    , fr = "%fr:ExportSettingPlainText%"
                    , ru = "%ru:ExportSettingPlainText%"
                    , de = "%de:ExportSettingPlainText%"
                    , ja = "%ja:ExportSettingPlainText%"
                    , pes = "%pes:ExportSettingPlainText%"
                    , it = "%it:ExportSettingPlainText%"
                    , ro = "%ro:ExportSettingPlainText%"
                    , hr = "%hr:ExportSettingPlainText%"
                    , nl = "%nl:ExportSettingPlainText%"
                    , hu = "%hu:ExportSettingPlainText%"
                    , sv = "%sv:ExportSettingPlainText%"
                    , ca = "%ca:ExportSettingPlainText%"
                    , br = "%br:ExportSettingPlainText%"
                    , mr = "%mr:ExportSettingPlainText%"
                    , cs = "%cs:ExportSettingPlainText%"
                    , fa = "%fa:ExportSettingPlainText%"
                    , id = "%id:ExportSettingPlainText%"
                    , ko = "%ko:ExportSettingPlainText%"
                    , pl = "%pl:ExportSettingPlainText%"
                    , uk = "%uk:ExportSettingPlainText%"
                    , nb = "%nb:ExportSettingPlainText%"
                    }

                ExportSettingJSON ->
                    { en = "JSON"
                    , zh_hans = "%zh_hans:ExportSettingJSON%"
                    , zh_hant = "%zh_hant:ExportSettingJSON%"
                    , es = "%es:ExportSettingJSON%"
                    , ar = "%ar:ExportSettingJSON%"
                    , fr = "%fr:ExportSettingJSON%"
                    , ru = "%ru:ExportSettingJSON%"
                    , de = "%de:ExportSettingJSON%"
                    , ja = "%ja:ExportSettingJSON%"
                    , pes = "%pes:ExportSettingJSON%"
                    , it = "%it:ExportSettingJSON%"
                    , ro = "%ro:ExportSettingJSON%"
                    , hr = "%hr:ExportSettingJSON%"
                    , nl = "%nl:ExportSettingJSON%"
                    , hu = "%hu:ExportSettingJSON%"
                    , sv = "%sv:ExportSettingJSON%"
                    , ca = "%ca:ExportSettingJSON%"
                    , br = "%br:ExportSettingJSON%"
                    , mr = "%mr:ExportSettingJSON%"
                    , cs = "%cs:ExportSettingJSON%"
                    , fa = "%fa:ExportSettingJSON%"
                    , id = "%id:ExportSettingJSON%"
                    , ko = "%ko:ExportSettingJSON%"
                    , pl = "%pl:ExportSettingJSON%"
                    , uk = "%uk:ExportSettingJSON%"
                    , nb = "%nb:ExportSettingJSON%"
                    }

                ExportSettingOPML ->
                    { en = "OPML"
                    , zh_hans = "%zh_hans:ExportSettingOPML%"
                    , zh_hant = "%zh_hant:ExportSettingOPML%"
                    , es = "%es:ExportSettingOPML%"
                    , ar = "%ar:ExportSettingOPML%"
                    , fr = "%fr:ExportSettingOPML%"
                    , ru = "%ru:ExportSettingOPML%"
                    , de = "%de:ExportSettingOPML%"
                    , ja = "%ja:ExportSettingOPML%"
                    , pes = "%pes:ExportSettingOPML%"
                    , it = "%it:ExportSettingOPML%"
                    , ro = "%ro:ExportSettingOPML%"
                    , hr = "%hr:ExportSettingOPML%"
                    , nl = "%nl:ExportSettingOPML%"
                    , hu = "%hu:ExportSettingOPML%"
                    , sv = "%sv:ExportSettingOPML%"
                    , ca = "%ca:ExportSettingOPML%"
                    , br = "%br:ExportSettingOPML%"
                    , mr = "%mr:ExportSettingOPML%"
                    , cs = "%cs:ExportSettingOPML%"
                    , fa = "%fa:ExportSettingOPML%"
                    , id = "%id:ExportSettingOPML%"
                    , ko = "%ko:ExportSettingOPML%"
                    , pl = "%pl:ExportSettingOPML%"
                    , uk = "%uk:ExportSettingOPML%"
                    , nb = "%nb:ExportSettingOPML%"
                    }

                CloseExportView ->
                    { en = "Close Export View"
                    , zh_hans = "%zh_hans:CloseExportView%"
                    , zh_hant = "%zh_hant:CloseExportView%"
                    , es = "%es:CloseExportView%"
                    , ar = "%ar:CloseExportView%"
                    , fr = "%fr:CloseExportView%"
                    , ru = "%ru:CloseExportView%"
                    , de = "%de:CloseExportView%"
                    , ja = "%ja:CloseExportView%"
                    , pes = "%pes:CloseExportView%"
                    , it = "%it:CloseExportView%"
                    , ro = "%ro:CloseExportView%"
                    , hr = "%hr:CloseExportView%"
                    , nl = "%nl:CloseExportView%"
                    , hu = "%hu:CloseExportView%"
                    , sv = "%sv:CloseExportView%"
                    , ca = "%ca:CloseExportView%"
                    , br = "%br:CloseExportView%"
                    , mr = "%mr:CloseExportView%"
                    , cs = "%cs:CloseExportView%"
                    , fa = "%fa:CloseExportView%"
                    , id = "%id:CloseExportView%"
                    , ko = "%ko:CloseExportView%"
                    , pl = "%pl:CloseExportView%"
                    , uk = "%uk:CloseExportView%"
                    , nb = "%nb:CloseExportView%"
                    }

                DownloadWordFile ->
                    { en = "Download Word File"
                    , zh_hans = "%zh_hans:DownloadWordFile%"
                    , zh_hant = "%zh_hant:DownloadWordFile%"
                    , es = "%es:DownloadWordFile%"
                    , ar = "%ar:DownloadWordFile%"
                    , fr = "%fr:DownloadWordFile%"
                    , ru = "%ru:DownloadWordFile%"
                    , de = "%de:DownloadWordFile%"
                    , ja = "%ja:DownloadWordFile%"
                    , pes = "%pes:DownloadWordFile%"
                    , it = "%it:DownloadWordFile%"
                    , ro = "%ro:DownloadWordFile%"
                    , hr = "%hr:DownloadWordFile%"
                    , nl = "%nl:DownloadWordFile%"
                    , hu = "%hu:DownloadWordFile%"
                    , sv = "%sv:DownloadWordFile%"
                    , ca = "%ca:DownloadWordFile%"
                    , br = "%br:DownloadWordFile%"
                    , mr = "%mr:DownloadWordFile%"
                    , cs = "%cs:DownloadWordFile%"
                    , fa = "%fa:DownloadWordFile%"
                    , id = "%id:DownloadWordFile%"
                    , ko = "%ko:DownloadWordFile%"
                    , pl = "%pl:DownloadWordFile%"
                    , uk = "%uk:DownloadWordFile%"
                    , nb = "%nb:DownloadWordFile%"
                    }

                DownloadTextFile ->
                    { en = "Download Markdown text file"
                    , zh_hans = "%zh_hans:DownloadTextFile%"
                    , zh_hant = "%zh_hant:DownloadTextFile%"
                    , es = "%es:DownloadTextFile%"
                    , ar = "%ar:DownloadTextFile%"
                    , fr = "%fr:DownloadTextFile%"
                    , ru = "%ru:DownloadTextFile%"
                    , de = "%de:DownloadTextFile%"
                    , ja = "%ja:DownloadTextFile%"
                    , pes = "%pes:DownloadTextFile%"
                    , it = "%it:DownloadTextFile%"
                    , ro = "%ro:DownloadTextFile%"
                    , hr = "%hr:DownloadTextFile%"
                    , nl = "%nl:DownloadTextFile%"
                    , hu = "%hu:DownloadTextFile%"
                    , sv = "%sv:DownloadTextFile%"
                    , ca = "%ca:DownloadTextFile%"
                    , br = "%br:DownloadTextFile%"
                    , mr = "%mr:DownloadTextFile%"
                    , cs = "%cs:DownloadTextFile%"
                    , fa = "%fa:DownloadTextFile%"
                    , id = "%id:DownloadTextFile%"
                    , ko = "%ko:DownloadTextFile%"
                    , pl = "%pl:DownloadTextFile%"
                    , uk = "%uk:DownloadTextFile%"
                    , nb = "%nb:DownloadTextFile%"
                    }

                DownloadJSONFile ->
                    { en = "Download JSON file"
                    , zh_hans = "%zh_hans:DownloadJSONFile%"
                    , zh_hant = "%zh_hant:DownloadJSONFile%"
                    , es = "%es:DownloadJSONFile%"
                    , ar = "%ar:DownloadJSONFile%"
                    , fr = "%fr:DownloadJSONFile%"
                    , ru = "%ru:DownloadJSONFile%"
                    , de = "%de:DownloadJSONFile%"
                    , ja = "%ja:DownloadJSONFile%"
                    , pes = "%pes:DownloadJSONFile%"
                    , it = "%it:DownloadJSONFile%"
                    , ro = "%ro:DownloadJSONFile%"
                    , hr = "%hr:DownloadJSONFile%"
                    , nl = "%nl:DownloadJSONFile%"
                    , hu = "%hu:DownloadJSONFile%"
                    , sv = "%sv:DownloadJSONFile%"
                    , ca = "%ca:DownloadJSONFile%"
                    , br = "%br:DownloadJSONFile%"
                    , mr = "%mr:DownloadJSONFile%"
                    , cs = "%cs:DownloadJSONFile%"
                    , fa = "%fa:DownloadJSONFile%"
                    , id = "%id:DownloadJSONFile%"
                    , ko = "%ko:DownloadJSONFile%"
                    , pl = "%pl:DownloadJSONFile%"
                    , uk = "%uk:DownloadJSONFile%"
                    , nb = "%nb:DownloadJSONFile%"
                    }

                DownloadOPMLFile ->
                    { en = "Download OPML file"
                    , zh_hans = "%zh_hans:DownloadOPMLFile%"
                    , zh_hant = "%zh_hant:DownloadOPMLFile%"
                    , es = "%es:DownloadOPMLFile%"
                    , ar = "%ar:DownloadOPMLFile%"
                    , fr = "%fr:DownloadOPMLFile%"
                    , ru = "%ru:DownloadOPMLFile%"
                    , de = "%de:DownloadOPMLFile%"
                    , ja = "%ja:DownloadOPMLFile%"
                    , pes = "%pes:DownloadOPMLFile%"
                    , it = "%it:DownloadOPMLFile%"
                    , ro = "%ro:DownloadOPMLFile%"
                    , hr = "%hr:DownloadOPMLFile%"
                    , nl = "%nl:DownloadOPMLFile%"
                    , hu = "%hu:DownloadOPMLFile%"
                    , sv = "%sv:DownloadOPMLFile%"
                    , ca = "%ca:DownloadOPMLFile%"
                    , br = "%br:DownloadOPMLFile%"
                    , mr = "%mr:DownloadOPMLFile%"
                    , cs = "%cs:DownloadOPMLFile%"
                    , fa = "%fa:DownloadOPMLFile%"
                    , id = "%id:DownloadOPMLFile%"
                    , ko = "%ko:DownloadOPMLFile%"
                    , pl = "%pl:DownloadOPMLFile%"
                    , uk = "%uk:DownloadOPMLFile%"
                    , nb = "%nb:DownloadOPMLFile%"
                    }

                PrintThis ->
                    { en = "Print this"
                    , zh_hans = "%zh_hans:PrintThis%"
                    , zh_hant = "%zh_hant:PrintThis%"
                    , es = "%es:PrintThis%"
                    , ar = "%ar:PrintThis%"
                    , fr = "%fr:PrintThis%"
                    , ru = "%ru:PrintThis%"
                    , de = "%de:PrintThis%"
                    , ja = "%ja:PrintThis%"
                    , pes = "%pes:PrintThis%"
                    , it = "%it:PrintThis%"
                    , ro = "%ro:PrintThis%"
                    , hr = "%hr:PrintThis%"
                    , nl = "%nl:PrintThis%"
                    , hu = "%hu:PrintThis%"
                    , sv = "%sv:PrintThis%"
                    , ca = "%ca:PrintThis%"
                    , br = "%br:PrintThis%"
                    , mr = "%mr:PrintThis%"
                    , cs = "%cs:PrintThis%"
                    , fa = "%fa:PrintThis%"
                    , id = "%id:PrintThis%"
                    , ko = "%ko:PrintThis%"
                    , pl = "%pl:PrintThis%"
                    , uk = "%uk:PrintThis%"
                    , nb = "%nb:PrintThis%"
                    }

                -- Upgrade & Subscription
                Upgrade ->
                    { en = "Upgrade"
                    , zh_hans = "%zh_hans:Upgrade%"
                    , zh_hant = "%zh_hant:Upgrade%"
                    , es = "%es:Upgrade%"
                    , ar = "%ar:Upgrade%"
                    , fr = "%fr:Upgrade%"
                    , ru = "%ru:Upgrade%"
                    , de = "%de:Upgrade%"
                    , ja = "%ja:Upgrade%"
                    , pes = "%pes:Upgrade%"
                    , it = "%it:Upgrade%"
                    , ro = "%ro:Upgrade%"
                    , hr = "%hr:Upgrade%"
                    , nl = "%nl:Upgrade%"
                    , hu = "%hu:Upgrade%"
                    , sv = "%sv:Upgrade%"
                    , ca = "%ca:Upgrade%"
                    , br = "%br:Upgrade%"
                    , mr = "%mr:Upgrade%"
                    , cs = "%cs:Upgrade%"
                    , fa = "%fa:Upgrade%"
                    , id = "%id:Upgrade%"
                    , ko = "%ko:Upgrade%"
                    , pl = "%pl:Upgrade%"
                    , uk = "%uk:Upgrade%"
                    , nb = "%nb:Upgrade%"
                    }

                DaysLeft n ->
                    { en = numberPlural n "%1 day left in trial" "%1 days left in trial"
                    , zh_hans = numberPlural n "%zh_hans:DaysLeft%:0" "%zh_hans:DaysLeft%:1"
                    , zh_hant = numberPlural n "%zh_hant:DaysLeft%:0" "%zh_hant:DaysLeft%:1"
                    , es = numberPlural n "%es:DaysLeft%:0" "%es:DaysLeft%:1"
                    , ar = numberPlural n "%ar:DaysLeft%:0" "%ar:DaysLeft%:1"
                    , fr = numberPlural n "%fr:DaysLeft%:0" "%fr:DaysLeft%:1"
                    , ru = numberPlural n "%ru:DaysLeft%:0" "%ru:DaysLeft%:1"
                    , de = numberPlural n "%de:DaysLeft%:0" "%de:DaysLeft%:1"
                    , ja = numberPlural n "%de:DaysLeft%:0" "%de:DaysLeft%:1"
                    , pes = numberPlural n "%de:DaysLeft%:0" "%de:DaysLeft%:1"
                    , it = numberPlural n "%de:DaysLeft%:0" "%de:DaysLeft%:1"
                    , ro = numberPlural n "%de:DaysLeft%:0" "%de:DaysLeft%:1"
                    , hr = numberPlural n "%de:DaysLeft%:0" "%de:DaysLeft%:1"
                    , nl = numberPlural n "%nl:DaysLeft%:0" "%nl:DaysLeft%:1"
                    , hu = numberPlural n "%hu:DaysLeft%:0" "%hu:DaysLeft%:1"
                    , sv = numberPlural n "%sv:DaysLeft%:0" "%sv:DaysLeft%:1"
                    , ca = numberPlural n "%ca:DaysLeft%:0" "%ca:DaysLeft%:1"
                    , br = numberPlural n "%br:DaysLeft%:0" "%br:DaysLeft%:1"
                    , mr = numberPlural n "%mr:DaysLeft%:0" "%mr:DaysLeft%:1"
                    , cs = numberPlural n "%cs:DaysLeft%:0" "%cs:DaysLeft%:1"
                    , fa = numberPlural n "%fa:DaysLeft%:0" "%fa:DaysLeft%:1"
                    , id = numberPlural n "%id:DaysLeft%:0" "%id:DaysLeft%:1"
                    , ko = numberPlural n "%ko:DaysLeft%:0" "%ko:DaysLeft%:1"
                    , pl = numberPlural n "%pl:DaysLeft%:0" "%pl:DaysLeft%:1"
                    , uk = numberPlural n "%uk:DaysLeft%:0" "%uk:DaysLeft%:1"
                    , nb = numberPlural n "%nb:DaysLeft%:0" "%nb:DaysLeft%:1"
                    }

                TrialExpired ->
                    { en = "Trial Expired"
                    , zh_hans = "%zh_hans:TrialExpired%"
                    , zh_hant = "%zh_hant:TrialExpired%"
                    , es = "%es:TrialExpired%"
                    , ar = "%ar:TrialExpired%"
                    , fr = "%fr:TrialExpired%"
                    , ru = "%ru:TrialExpired%"
                    , de = "%de:TrialExpired%"
                    , ja = "%ja:TrialExpired%"
                    , pes = "%pes:TrialExpired%"
                    , it = "%it:TrialExpired%"
                    , ro = "%ro:TrialExpired%"
                    , hr = "%hr:TrialExpired%"
                    , nl = "%nl:TrialExpired%"
                    , hu = "%hu:TrialExpired%"
                    , sv = "%sv:TrialExpired%"
                    , ca = "%ca:TrialExpired%"
                    , br = "%br:TrialExpired%"
                    , mr = "%mr:TrialExpired%"
                    , cs = "%cs:TrialExpired%"
                    , fa = "%fa:TrialExpired%"
                    , id = "%id:TrialExpired%"
                    , ko = "%ko:TrialExpired%"
                    , pl = "%pl:TrialExpired%"
                    , uk = "%uk:TrialExpired%"
                    , nb = "%nb:TrialExpired%"
                    }

                WordOfMouthCTA1 ->
                    { en = "Love Gingko Writer?"
                    , zh_hans = "%zh_hans:WordOfMouthCTA1%"
                    , zh_hant = "%zh_hant:WordOfMouthCTA1%"
                    , es = "%es:WordOfMouthCTA1%"
                    , ar = "%ar:WordOfMouthCTA1%"
                    , fr = "%fr:WordOfMouthCTA1%"
                    , ru = "%ru:WordOfMouthCTA1%"
                    , de = "%de:WordOfMouthCTA1%"
                    , ja = "%ja:WordOfMouthCTA1%"
                    , pes = "%pes:WordOfMouthCTA1%"
                    , it = "%it:WordOfMouthCTA1%"
                    , ro = "%ro:WordOfMouthCTA1%"
                    , hr = "%hr:WordOfMouthCTA1%"
                    , nl = "%nl:WordOfMouthCTA1%"
                    , hu = "%hu:WordOfMouthCTA1%"
                    , sv = "%sv:WordOfMouthCTA1%"
                    , ca = "%ca:WordOfMouthCTA1%"
                    , br = "%br:WordOfMouthCTA1%"
                    , mr = "%mr:WordOfMouthCTA1%"
                    , cs = "%cs:WordOfMouthCTA1%"
                    , fa = "%fa:WordOfMouthCTA1%"
                    , id = "%id:WordOfMouthCTA1%"
                    , ko = "%ko:WordOfMouthCTA1%"
                    , pl = "%pl:WordOfMouthCTA1%"
                    , uk = "%uk:WordOfMouthCTA1%"
                    , nb = "%nb:WordOfMouthCTA1%"
                    }

                WordOfMouthCTA2 ->
                    { en = "Leave a Testimonial"
                    , zh_hans = "%zh_hans:WordOfMouthCTA2%"
                    , zh_hant = "%zh_hant:WordOfMouthCTA2%"
                    , es = "%es:WordOfMouthCTA2%"
                    , ar = "%ar:WordOfMouthCTA2%"
                    , fr = "%fr:WordOfMouthCTA2%"
                    , ru = "%ru:WordOfMouthCTA2%"
                    , de = "%de:WordOfMouthCTA2%"
                    , ja = "%ja:WordOfMouthCTA2%"
                    , pes = "%pes:WordOfMouthCTA2%"
                    , it = "%it:WordOfMouthCTA2%"
                    , ro = "%ro:WordOfMouthCTA2%"
                    , hr = "%hr:WordOfMouthCTA2%"
                    , nl = "%nl:WordOfMouthCTA2%"
                    , hu = "%hu:WordOfMouthCTA2%"
                    , sv = "%sv:WordOfMouthCTA2%"
                    , ca = "%ca:WordOfMouthCTA2%"
                    , br = "%br:WordOfMouthCTA2%"
                    , mr = "%mr:WordOfMouthCTA2%"
                    , cs = "%cs:WordOfMouthCTA2%"
                    , fa = "%fa:WordOfMouthCTA2%"
                    , id = "%id:WordOfMouthCTA2%"
                    , ko = "%ko:WordOfMouthCTA2%"
                    , pl = "%pl:WordOfMouthCTA2%"
                    , uk = "%uk:WordOfMouthCTA2%"
                    , nb = "%nb:WordOfMouthCTA2%"
                    }

                ManageSubscription ->
                    { en = "Manage Subscription"
                    , zh_hans = "%zh_hans:ManageSubscription%"
                    , zh_hant = "%zh_hant:ManageSubscription%"
                    , es = "%es:ManageSubscription%"
                    , ar = "%ar:ManageSubscription%"
                    , fr = "%fr:ManageSubscription%"
                    , ru = "%ru:ManageSubscription%"
                    , de = "%de:ManageSubscription%"
                    , ja = "%ja:ManageSubscription%"
                    , pes = "%pes:ManageSubscription%"
                    , it = "%it:ManageSubscription%"
                    , ro = "%ro:ManageSubscription%"
                    , hr = "%hr:ManageSubscription%"
                    , nl = "%nl:ManageSubscription%"
                    , hu = "%hu:ManageSubscription%"
                    , sv = "%sv:ManageSubscription%"
                    , ca = "%ca:ManageSubscription%"
                    , br = "%br:ManageSubscription%"
                    , mr = "%mr:ManageSubscription%"
                    , cs = "%cs:ManageSubscription%"
                    , fa = "%fa:ManageSubscription%"
                    , id = "%id:ManageSubscription%"
                    , ko = "%ko:ManageSubscription%"
                    , pl = "%pl:ManageSubscription%"
                    , uk = "%uk:ManageSubscription%"
                    , nb = "%nb:ManageSubscription%"
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

        Ar ->
            .ar translationSet

        Fr ->
            .fr translationSet

        Ru ->
            .ru translationSet

        De ->
            .de translationSet

        Ja ->
            .ja translationSet

        Mr ->
            .mr translationSet

        Pes ->
            .pes translationSet

        It ->
            .it translationSet

        Ro ->
            .ro translationSet

        Hr ->
            .hr translationSet

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

        Cs ->
            .cs translationSet

        Fa ->
            .fa translationSet

        Id ->
            .id translationSet

        Ko ->
            .ko translationSet

        Pl ->
            .pl translationSet

        Uk ->
            .uk translationSet

        Nb ->
            .nb translationSet


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

        Ar ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.en t1 t2

        Fr ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.fr t1 t2

        Ru ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.en t1 t2

        De ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.en t1 t2

        Ja ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.en t1 t2

        Mr ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.en t1 t2

        Pes ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.en t1 t2

        It ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.it t1 t2

        Ro ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.en t1 t2

        Hr ->
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

        _ ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.en t1 t2


dateFormat : Language -> Time.Posix -> String
dateFormat lang time =
    posixToString "%B %-d, %Y" lang time


datetimeFormat : Language -> Time.Posix -> String
datetimeFormat lang time =
    posixToString "%b %-d, %Y, %-I:%M:%S %p" lang time


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

        Ar ->
            format Time.Format.Config.Config_en_us.config formatString Time.utc time

        Fr ->
            format Time.Format.Config.Config_fr_fr.config formatString Time.utc time

        Ru ->
            format Time.Format.Config.Config_ru_ru.config formatString Time.utc time

        De ->
            format Time.Format.Config.Config_de_de.config formatString Time.utc time

        Ja ->
            format Time.Format.Config.Config_ja_jp.config formatString Time.utc time

        Mr ->
            format Time.Format.Config.Config_en_us.config formatString Time.utc time

        Pes ->
            format Time.Format.Config.Config_en_us.config formatString Time.utc time

        It ->
            format Time.Format.Config.Config_en_us.config formatString Time.utc time

        Ro ->
            format Time.Format.Config.Config_en_us.config formatString Time.utc time

        Hr ->
            format Time.Format.Config.Config_en_us.config formatString Time.utc time

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

        _ ->
            format Time.Format.Config.Config_en_us.config formatString Time.utc time


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

        "ar" ->
            Ar

        "fr" ->
            Fr

        "ru" ->
            Ru

        "de" ->
            De

        "ja" ->
            Ja

        "mr" ->
            Mr

        "pes" ->
            Pes

        "it" ->
            It

        "ro" ->
            Ro

        "hr" ->
            Hr

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

        "cs" ->
            Cs

        "fa" ->
            Fa

        "id" ->
            Id

        "ko" ->
            Ko

        "pl" ->
            Pl

        "uk" ->
            Uk

        "nb" ->
            Nb

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

        Ar ->
            "ar"

        Fr ->
            "fr"

        Ru ->
            "ru"

        De ->
            "de"

        Ja ->
            "ja"

        Mr ->
            "ja"

        Pes ->
            "pes"

        It ->
            "it"

        Ro ->
            "ro"

        Hr ->
            "hr"

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

        Cs ->
            "cs"

        Fa ->
            "fa"

        Id ->
            "id"

        Ko ->
            "ko"

        Pl ->
            "pl"

        Uk ->
            "uk"

        Nb ->
            "nb"
