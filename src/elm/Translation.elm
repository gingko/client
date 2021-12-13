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
    | Help
    | WhatsNew
    | AccountTooltip
      -- Keyboard Shortcut Help
    | KeyboardShortcuts
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
    | ForItalic
    | ToSaveChanges
    | SaveChangesTitle
    | EscKey
    | OtherShortcuts
    | DisplayWordCounts
    | EditMode
    | SaveOrCancelChanges
    | Formatting
    | FormattingTitle
    | FormattingList
    | FormattingLink
    | ParenNumber
    | SetHeadingLevel
      --
    | AreYouSureCancel
    | ToCancelChanges
    | PressToSearch
    | QuickDocumentSwitcher
    | OpenQuickSwitcher
    | EmailSupport
    | Logout
    | Language
    | ContributeTranslations
    | Here
    | HeadingFont
    | ContentFont
    | EditingFont
    | VersionHistory
    | DocumentSettings
    | WordCount
    | WordCountSession Int
    | WordCountTotal Int
    | WordCountCard Int
    | WordCountSubtree Int
    | WordCountGroup Int
    | WordCountColumn Int
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
    | DownloadWordFile
    | DownloadTextFile
    | DownloadJSONFile
    | PrintThis
      -- Upgrade & Subscription
    | Upgrade
    | DaysLeft Int
    | TrialExpired
    | ManageSubscription


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
                NoTr str ->
                    { en = str
                    , zh_hans = str
                    , zh_hant = str
                    , es = str
                    , fr = str
                    , ru = str
                    , de = str
                    , nl = str
                    , hu = str
                    , sv = str
                    , ca = str
                    , br = str
                    }

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

                -- Template and Import modal
                NewDocument ->
                    { en = "New Document"
                    , zh_hans = "%zh_hans:NewDocument"
                    , zh_hant = "%zh_hant:NewDocument"
                    , es = "%es:NewDocument"
                    , fr = "%fr:NewDocument"
                    , ru = "%ru:NewDocument"
                    , de = "%de:NewDocument"
                    , nl = "%nl:NewDocument"
                    , hu = "%hu:NewDocument"
                    , sv = "%sv:NewDocument"
                    , ca = "%ca:NewDocument"
                    , br = "%br:NewDocument"
                    }

                ShowDocumentList ->
                    { en = "Show Document List"
                    , zh_hans = "%zh_hans:ShowDocumentList"
                    , zh_hant = "%zh_hant:ShowDocumentList"
                    , es = "%es:ShowDocumentList"
                    , fr = "%fr:ShowDocumentList"
                    , ru = "%ru:ShowDocumentList"
                    , de = "%de:ShowDocumentList"
                    , nl = "%nl:ShowDocumentList"
                    , hu = "%hu:ShowDocumentList"
                    , sv = "%sv:ShowDocumentList"
                    , ca = "%ca:ShowDocumentList"
                    , br = "%br:ShowDocumentList"
                    }

                SortByName ->
                    { en = "Sort by Name"
                    , zh_hans = "%zh_hans:SortByName"
                    , zh_hant = "%zh_hant:SortByName"
                    , es = "%es:SortByName"
                    , fr = "%fr:SortByName"
                    , ru = "%ru:SortByName"
                    , de = "%de:SortByName"
                    , nl = "%nl:SortByName"
                    , hu = "%hu:SortByName"
                    , sv = "%sv:SortByName"
                    , ca = "%ca:SortByName"
                    , br = "%br:SortByName"
                    }

                SortByLastModified ->
                    { en = "Sort by Last Modified"
                    , zh_hans = "%zh_hans:SortByLastModified"
                    , zh_hant = "%zh_hant:SortByLastModified"
                    , es = "%es:SortByLastModified"
                    , fr = "%fr:SortByLastModified"
                    , ru = "%ru:SortByLastModified"
                    , de = "%de:SortByLastModified"
                    , nl = "%nl:SortByLastModified"
                    , hu = "%hu:SortByLastModified"
                    , sv = "%sv:SortByLastModified"
                    , ca = "%ca:SortByLastModified"
                    , br = "%br:SortByLastModified"
                    }

                SortByDateCreated ->
                    { en = "Sort by Date Created"
                    , zh_hans = "%zh_hans:SortByDateCreated"
                    , zh_hant = "%zh_hant:SortByDateCreated"
                    , es = "%es:SortByDateCreated"
                    , fr = "%fr:SortByDateCreated"
                    , ru = "%ru:SortByDateCreated"
                    , de = "%de:SortByDateCreated"
                    , nl = "%nl:SortByDateCreated"
                    , hu = "%hu:SortByDateCreated"
                    , sv = "%sv:SortByDateCreated"
                    , ca = "%ca:SortByDateCreated"
                    , br = "%br:SortByDateCreated"
                    }

                TemplatesAndExamples ->
                    { en = "Templates & Examples"
                    , zh_hans = "%zh_hans:TemplatesAndExamples"
                    , zh_hant = "%zh_hant:TemplatesAndExamples"
                    , es = "%es:TemplatesAndExamples"
                    , fr = "%fr:TemplatesAndExamples"
                    , ru = "%ru:TemplatesAndExamples"
                    , de = "%de:TemplatesAndExamples"
                    , nl = "%nl:TemplatesAndExamples"
                    , hu = "%hu:TemplatesAndExamples"
                    , sv = "%sv:TemplatesAndExamples"
                    , ca = "%ca:TemplatesAndExamples"
                    , br = "%br:TemplatesAndExamples"
                    }

                New ->
                    { en = "New"
                    , zh_hans = "%zh_hans:New"
                    , zh_hant = "%zh_hant:New"
                    , es = "%es:New"
                    , fr = "%fr:New"
                    , ru = "%ru:New"
                    , de = "%de:New"
                    , nl = "%nl:New"
                    , hu = "%hu:New"
                    , sv = "%sv:New"
                    , ca = "%ca:New"
                    , br = "%br:New"
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

                ImportSectionTitle ->
                    { en = "Import"
                    , zh_hans = "%zh_hans:ImportSectionTitle"
                    , zh_hant = "%zh_hant:ImportSectionTitle"
                    , es = "%es:ImportSectionTitle"
                    , fr = "%fr:ImportSectionTitle"
                    , ru = "%ru:ImportSectionTitle"
                    , de = "%de:ImportSectionTitle"
                    , nl = "%nl:ImportSectionTitle"
                    , hu = "%hu:ImportSectionTitle"
                    , sv = "%sv:ImportSectionTitle"
                    , ca = "%ca:ImportSectionTitle"
                    , br = "%br:ImportSectionTitle"
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

                ImportTextFiles ->
                    { en = "Import Text Files"
                    , zh_hans = "%zh_hans:ImportTextFiles"
                    , zh_hant = "%zh_hant:ImportTextFiles"
                    , es = "%es:ImportTextFiles"
                    , fr = "%fr:ImportTextFiles"
                    , ru = "%ru:ImportTextFiles"
                    , de = "%de:ImportTextFiles"
                    , nl = "%nl:ImportTextFiles"
                    , hu = "%hu:ImportTextFiles"
                    , sv = "%sv:ImportTextFiles"
                    , ca = "%ca:ImportTextFiles"
                    , br = "%br:ImportTextFiles"
                    }

                ImportTextFilesDesc ->
                    { en = "Import multiple markdown or regular text files."
                    , zh_hans = "%zh_hans:ImportTextFilesDesc"
                    , zh_hant = "%zh_hant:ImportTextFilesDesc"
                    , es = "%es:ImportTextFilesDesc"
                    , fr = "%fr:ImportTextFilesDesc"
                    , ru = "%ru:ImportTextFilesDesc"
                    , de = "%de:ImportTextFilesDesc"
                    , nl = "%nl:ImportTextFilesDesc"
                    , hu = "%hu:ImportTextFilesDesc"
                    , sv = "%sv:ImportTextFilesDesc"
                    , ca = "%ca:ImportTextFilesDesc"
                    , br = "%br:ImportTextFilesDesc"
                    }

                ImportOpmlFiles ->
                    { en = "Import Opml Files"
                    , zh_hans = "%zh_hans:ImportOpmlFiles"
                    , zh_hant = "%zh_hant:ImportOpmlFiles"
                    , es = "%es:ImportOpmlFiles"
                    , fr = "%fr:ImportOpmlFiles"
                    , ru = "%ru:ImportOpmlFiles"
                    , de = "%de:ImportOpmlFiles"
                    , nl = "%nl:ImportOpmlFiles"
                    , hu = "%hu:ImportOpmlFiles"
                    , sv = "%sv:ImportOpmlFiles"
                    , ca = "%ca:ImportOpmlFiles"
                    , br = "%br:ImportOpmlFiles"
                    }

                ImportOpmlFilesDesc ->
                    { en = "Import from Workflowy or other outliners."
                    , zh_hans = "%zh_hans:ImportOpmlFilesDesc"
                    , zh_hant = "%zh_hant:ImportOpmlFilesDesc"
                    , es = "%es:ImportOpmlFilesDesc"
                    , fr = "%fr:ImportOpmlFilesDesc"
                    , ru = "%ru:ImportOpmlFilesDesc"
                    , de = "%de:ImportOpmlFilesDesc"
                    , nl = "%nl:ImportOpmlFilesDesc"
                    , hu = "%hu:ImportOpmlFilesDesc"
                    , sv = "%sv:ImportOpmlFilesDesc"
                    , ca = "%ca:ImportOpmlFilesDesc"
                    , br = "%br:ImportOpmlFilesDesc"
                    }

                TimelineTemplate ->
                    { en = "Timeline 2022"
                    , zh_hans = "%zh_hans:TimelineTemplate"
                    , zh_hant = "%zh_hant:TimelineTemplate"
                    , es = "%es:TimelineTemplate"
                    , fr = "%fr:TimelineTemplate"
                    , ru = "%ru:TimelineTemplate"
                    , de = "%de:TimelineTemplate"
                    , nl = "%nl:TimelineTemplate"
                    , hu = "%hu:TimelineTemplate"
                    , sv = "%sv:TimelineTemplate"
                    , ca = "%ca:TimelineTemplate"
                    , br = "%br:TimelineTemplate"
                    }

                TimelineTemplateDesc ->
                    { en = "A tree-based calendar"
                    , zh_hans = "%zh_hans:TimelineTemplateDesc"
                    , zh_hant = "%zh_hant:TimelineTemplateDesc"
                    , es = "%es:TimelineTemplateDesc"
                    , fr = "%fr:TimelineTemplateDesc"
                    , ru = "%ru:TimelineTemplateDesc"
                    , de = "%de:TimelineTemplateDesc"
                    , nl = "%nl:TimelineTemplateDesc"
                    , hu = "%hu:TimelineTemplateDesc"
                    , sv = "%sv:TimelineTemplateDesc"
                    , ca = "%ca:TimelineTemplateDesc"
                    , br = "%br:TimelineTemplateDesc"
                    }

                AcademicPaperTemplate ->
                    { en = "Academic Paper"
                    , zh_hans = "%zh_hans:AcademicPaperTemplate"
                    , zh_hant = "%zh_hant:AcademicPaperTemplate"
                    , es = "%es:AcademicPaperTemplate"
                    , fr = "%fr:AcademicPaperTemplate"
                    , ru = "%ru:AcademicPaperTemplate"
                    , de = "%de:AcademicPaperTemplate"
                    , nl = "%nl:AcademicPaperTemplate"
                    , hu = "%hu:AcademicPaperTemplate"
                    , sv = "%sv:AcademicPaperTemplate"
                    , ca = "%ca:AcademicPaperTemplate"
                    , br = "%br:AcademicPaperTemplate"
                    }

                AcademicPaperTemplateDesc ->
                    { en = "Starting point for journal paper"
                    , zh_hans = "%zh_hans:AcademicPaperTemplateDesc"
                    , zh_hant = "%zh_hant:AcademicPaperTemplateDesc"
                    , es = "%es:AcademicPaperTemplateDesc"
                    , fr = "%fr:AcademicPaperTemplateDesc"
                    , ru = "%ru:AcademicPaperTemplateDesc"
                    , de = "%de:AcademicPaperTemplateDesc"
                    , nl = "%nl:AcademicPaperTemplateDesc"
                    , hu = "%hu:AcademicPaperTemplateDesc"
                    , sv = "%sv:AcademicPaperTemplateDesc"
                    , ca = "%ca:AcademicPaperTemplateDesc"
                    , br = "%br:AcademicPaperTemplateDesc"
                    }

                ProjectBrainstormingTemplate ->
                    { en = "Project Brainstorming"
                    , zh_hans = "%zh_hans:ProjectBrainstormingTemplate"
                    , zh_hant = "%zh_hant:ProjectBrainstormingTemplate"
                    , es = "%es:ProjectBrainstormingTemplate"
                    , fr = "%fr:ProjectBrainstormingTemplate"
                    , ru = "%ru:ProjectBrainstormingTemplate"
                    , de = "%de:ProjectBrainstormingTemplate"
                    , nl = "%nl:ProjectBrainstormingTemplate"
                    , hu = "%hu:ProjectBrainstormingTemplate"
                    , sv = "%sv:ProjectBrainstormingTemplate"
                    , ca = "%ca:ProjectBrainstormingTemplate"
                    , br = "%br:ProjectBrainstormingTemplate"
                    }

                ProjectBrainstormingTemplateDesc ->
                    { en = "Example on clarifying project goals"
                    , zh_hans = "%zh_hans:ProjectBrainstormingTemplateDesc"
                    , zh_hant = "%zh_hant:ProjectBrainstormingTemplateDesc"
                    , es = "%es:ProjectBrainstormingTemplateDesc"
                    , fr = "%fr:ProjectBrainstormingTemplateDesc"
                    , ru = "%ru:ProjectBrainstormingTemplateDesc"
                    , de = "%de:ProjectBrainstormingTemplateDesc"
                    , nl = "%nl:ProjectBrainstormingTemplateDesc"
                    , hu = "%hu:ProjectBrainstormingTemplateDesc"
                    , sv = "%sv:ProjectBrainstormingTemplateDesc"
                    , ca = "%ca:ProjectBrainstormingTemplateDesc"
                    , br = "%br:ProjectBrainstormingTemplateDesc"
                    }

                HerosJourneyTemplate ->
                    { en = "Hero's Journey"
                    , zh_hans = "%zh_hans:HerosJourneyTemplate"
                    , zh_hant = "%zh_hant:HerosJourneyTemplate"
                    , es = "%es:HerosJourneyTemplate"
                    , fr = "%fr:HerosJourneyTemplate"
                    , ru = "%ru:HerosJourneyTemplate"
                    , de = "%de:HerosJourneyTemplate"
                    , nl = "%nl:HerosJourneyTemplate"
                    , hu = "%hu:HerosJourneyTemplate"
                    , sv = "%sv:HerosJourneyTemplate"
                    , ca = "%ca:HerosJourneyTemplate"
                    , br = "%br:HerosJourneyTemplate"
                    }

                HerosJourneyTemplateDesc ->
                    { en = "A framework for fictional stories"
                    , zh_hans = "%zh_hans:HerosJourneyTemplateDesc"
                    , zh_hant = "%zh_hant:HerosJourneyTemplateDesc"
                    , es = "%es:HerosJourneyTemplateDesc"
                    , fr = "%fr:HerosJourneyTemplateDesc"
                    , ru = "%ru:HerosJourneyTemplateDesc"
                    , de = "%de:HerosJourneyTemplateDesc"
                    , nl = "%nl:HerosJourneyTemplateDesc"
                    , hu = "%hu:HerosJourneyTemplateDesc"
                    , sv = "%sv:HerosJourneyTemplateDesc"
                    , ca = "%ca:HerosJourneyTemplateDesc"
                    , br = "%br:HerosJourneyTemplateDesc"
                    }

                --
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

                Help ->
                    { en = "Help"
                    , zh_hans = "%zh_hans:Help"
                    , zh_hant = "%zh_hant:Help"
                    , es = "%es:Help"
                    , fr = "%fr:Help"
                    , ru = "%ru:Help"
                    , de = "%de:Help"
                    , nl = "%nl:Help"
                    , hu = "%hu:Help"
                    , sv = "%sv:Help"
                    , ca = "%ca:Help"
                    , br = "%br:Help"
                    }

                WhatsNew ->
                    { en = "What's New"
                    , zh_hans = "%zh_hans:WhatsNew"
                    , zh_hant = "%zh_hant:WhatsNew"
                    , es = "%es:WhatsNew"
                    , fr = "%fr:WhatsNew"
                    , ru = "%ru:WhatsNew"
                    , de = "%de:WhatsNew"
                    , nl = "%nl:WhatsNew"
                    , hu = "%hu:WhatsNew"
                    , sv = "%sv:WhatsNew"
                    , ca = "%ca:WhatsNew"
                    , br = "%br:WhatsNew"
                    }

                AccountTooltip ->
                    { en = "Account"
                    , zh_hans = "%zh_hans:AccountTooltip"
                    , zh_hant = "%zh_hant:AccountTooltip"
                    , es = "%es:AccountTooltip"
                    , fr = "%fr:AccountTooltip"
                    , ru = "%ru:AccountTooltip"
                    , de = "%de:AccountTooltip"
                    , nl = "%nl:AccountTooltip"
                    , hu = "%hu:AccountTooltip"
                    , sv = "%sv:AccountTooltip"
                    , ca = "%ca:AccountTooltip"
                    , br = "%br:AccountTooltip"
                    }

                -- Keyboard Shortcut Help
                KeyboardShortcuts ->
                    { en = "Keyboard Shortcuts"
                    , zh_hans = "%zh_hans:KeyboardShortcuts"
                    , zh_hant = "%zh_hant:KeyboardShortcuts"
                    , es = "%es:KeyboardShortcuts"
                    , fr = "%fr:KeyboardShortcuts"
                    , ru = "%ru:KeyboardShortcuts"
                    , de = "%de:KeyboardShortcuts"
                    , nl = "%nl:KeyboardShortcuts"
                    , hu = "%hu:KeyboardShortcuts"
                    , sv = "%sv:KeyboardShortcuts"
                    , ca = "%ca:KeyboardShortcuts"
                    , br = "%br:KeyboardShortcuts"
                    }

                EditCards ->
                    { en = "Edit Cards"
                    , zh_hans = "%zh_hans:EditCards"
                    , zh_hant = "%zh_hant:EditCards"
                    , es = "%es:EditCards"
                    , fr = "%fr:EditCards"
                    , ru = "%ru:EditCards"
                    , de = "%de:EditCards"
                    , nl = "%nl:EditCards"
                    , hu = "%hu:EditCards"
                    , sv = "%sv:EditCards"
                    , ca = "%ca:EditCards"
                    , br = "%br:EditCards"
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

                ShiftKey ->
                    { en = "Shift"
                    , zh_hans = "%zh_hans:ShiftKey"
                    , zh_hant = "%zh_hant:ShiftKey"
                    , es = "%es:ShiftKey"
                    , fr = "%fr:ShiftKey"
                    , ru = "%ru:ShiftKey"
                    , de = "%de:ShiftKey"
                    , nl = "%nl:ShiftKey"
                    , hu = "%hu:ShiftKey"
                    , sv = "%sv:ShiftKey"
                    , ca = "%ca:ShiftKey"
                    , br = "%br:ShiftKey"
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

                AltKey ->
                    { en = "AltKey"
                    , zh_hans = "%zh_hans:AltKey"
                    , zh_hant = "%zh_hant:AltKey"
                    , es = "%es:AltKey"
                    , fr = "%fr:AltKey"
                    , ru = "%ru:AltKey"
                    , de = "%de:AltKey"
                    , nl = "%nl:AltKey"
                    , hu = "%hu:AltKey"
                    , sv = "%sv:AltKey"
                    , ca = "%ca:AltKey"
                    , br = "%br:AltKey"
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

                Navigate ->
                    { en = "Navigate"
                    , zh_hans = "%zh_hans:Navigate"
                    , zh_hant = "%zh_hant:Navigate"
                    , es = "%es:Navigate"
                    , fr = "%fr:Navigate"
                    , ru = "%ru:Navigate"
                    , de = "%de:Navigate"
                    , nl = "%nl:Navigate"
                    , hu = "%hu:Navigate"
                    , sv = "%sv:Navigate"
                    , ca = "%ca:Navigate"
                    , br = "%br:Navigate"
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

                AddNewCards ->
                    { en = "Add New Cards"
                    , zh_hans = "%zh_hans:AddNewCards"
                    , zh_hant = "%zh_hant:AddNewCards"
                    , es = "%es:AddNewCards"
                    , fr = "%fr:AddNewCards"
                    , ru = "%ru:AddNewCards"
                    , de = "%de:AddNewCards"
                    , nl = "%nl:AddNewCards"
                    , hu = "%hu:AddNewCards"
                    , sv = "%sv:AddNewCards"
                    , ca = "%ca:AddNewCards"
                    , br = "%br:AddNewCards"
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

                SplitAtCursor ->
                    { en = "Split At Cursor"
                    , zh_hans = "%zh_hans:SplitAtCursor"
                    , zh_hant = "%zh_hant:SplitAtCursor"
                    , es = "%es:SplitAtCursor"
                    , fr = "%fr:SplitAtCursor"
                    , ru = "%ru:SplitAtCursor"
                    , de = "%de:SplitAtCursor"
                    , nl = "%nl:SplitAtCursor"
                    , hu = "%hu:SplitAtCursor"
                    , sv = "%sv:SplitAtCursor"
                    , ca = "%ca:SplitAtCursor"
                    , br = "%br:SplitAtCursor"
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

                MergeCards ->
                    { en = "Merge Cards"
                    , zh_hans = "%zh_hans:MergeCards"
                    , zh_hant = "%zh_hant:MergeCards"
                    , es = "%es:MergeCards"
                    , fr = "%fr:MergeCards"
                    , ru = "%ru:MergeCards"
                    , de = "%de:MergeCards"
                    , nl = "%nl:MergeCards"
                    , hu = "%hu:MergeCards"
                    , sv = "%sv:MergeCards"
                    , ca = "%ca:MergeCards"
                    , br = "%br:MergeCards"
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

                MoveAndDelete ->
                    { en = "Move & Delete Cards"
                    , zh_hans = "%zh_hans:MoveAndDelete"
                    , zh_hant = "%zh_hant:MoveAndDelete"
                    , es = "%es:MoveAndDelete"
                    , fr = "%fr:MoveAndDelete"
                    , ru = "%ru:MoveAndDelete"
                    , de = "%de:MoveAndDelete"
                    , nl = "%nl:MoveAndDelete"
                    , hu = "%hu:MoveAndDelete"
                    , sv = "%sv:MoveAndDelete"
                    , ca = "%ca:MoveAndDelete"
                    , br = "%br:MoveAndDelete"
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

                OtherShortcuts ->
                    { en = "Other Shortcuts"
                    , zh_hans = "%zh_hans:OtherShortcuts"
                    , zh_hant = "%zh_hant:OtherShortcuts"
                    , es = "%es:OtherShortcuts"
                    , fr = "%fr:OtherShortcuts"
                    , ru = "%ru:OtherShortcuts"
                    , de = "%de:OtherShortcuts"
                    , nl = "%nl:OtherShortcuts"
                    , hu = "%hu:OtherShortcuts"
                    , sv = "%sv:OtherShortcuts"
                    , ca = "%ca:OtherShortcuts"
                    , br = "%br:OtherShortcuts"
                    }

                DisplayWordCounts ->
                    { en = "Display word counts"
                    , zh_hans = "%zh_hans:DisplayWordCounts"
                    , zh_hant = "%zh_hant:DisplayWordCounts"
                    , es = "%es:DisplayWordCounts"
                    , fr = "%fr:DisplayWordCounts"
                    , ru = "%ru:DisplayWordCounts"
                    , de = "%de:DisplayWordCounts"
                    , nl = "%nl:DisplayWordCounts"
                    , hu = "%hu:DisplayWordCounts"
                    , sv = "%sv:DisplayWordCounts"
                    , ca = "%ca:DisplayWordCounts"
                    , br = "%br:DisplayWordCounts"
                    }

                EditMode ->
                    { en = "(Edit Mode)"
                    , zh_hans = "%zh_hans:EditMode"
                    , zh_hant = "%zh_hant:EditMode"
                    , es = "%es:EditMode"
                    , fr = "%fr:EditMode"
                    , ru = "%ru:EditMode"
                    , de = "%de:EditMode"
                    , nl = "%nl:EditMode"
                    , hu = "%hu:EditMode"
                    , sv = "%sv:EditMode"
                    , ca = "%ca:EditMode"
                    , br = "%br:EditMode"
                    }

                SaveOrCancelChanges ->
                    { en = "Save/Cancel Changes"
                    , zh_hans = "%zh_hans:SaveOrCancelChanges"
                    , zh_hant = "%zh_hant:SaveOrCancelChanges"
                    , es = "%es:SaveOrCancelChanges"
                    , fr = "%fr:SaveOrCancelChanges"
                    , ru = "%ru:SaveOrCancelChanges"
                    , de = "%de:SaveOrCancelChanges"
                    , nl = "%nl:SaveOrCancelChanges"
                    , hu = "%hu:SaveOrCancelChanges"
                    , sv = "%sv:SaveOrCancelChanges"
                    , ca = "%ca:SaveOrCancelChanges"
                    , br = "%br:SaveOrCancelChanges"
                    }

                Formatting ->
                    { en = "Formatting"
                    , zh_hans = "%zh_hans:Formatting"
                    , zh_hant = "%zh_hant:Formatting"
                    , es = "%es:Formatting"
                    , fr = "%fr:Formatting"
                    , ru = "%ru:Formatting"
                    , de = "%de:Formatting"
                    , nl = "%nl:Formatting"
                    , hu = "%hu:Formatting"
                    , sv = "%sv:Formatting"
                    , ca = "%ca:Formatting"
                    , br = "%br:Formatting"
                    }

                FormattingTitle ->
                    { en = "# Title\n## Subtitle"
                    , zh_hans = "%zh_hans:FormattingTitle"
                    , zh_hant = "%zh_hant:FormattingTitle"
                    , es = "%es:FormattingTitle"
                    , fr = "%fr:FormattingTitle"
                    , ru = "%ru:FormattingTitle"
                    , de = "%de:FormattingTitle"
                    , nl = "%nl:FormattingTitle"
                    , hu = "%hu:FormattingTitle"
                    , sv = "%sv:FormattingTitle"
                    , ca = "%ca:FormattingTitle"
                    , br = "%br:FormattingTitle"
                    }

                FormattingList ->
                    { en = "- List item\n  - Subitem"
                    , zh_hans = "%zh_hans:FormattingList"
                    , zh_hant = "%zh_hant:FormattingList"
                    , es = "%es:FormattingList"
                    , fr = "%fr:FormattingList"
                    , ru = "%ru:FormattingList"
                    , de = "%de:FormattingList"
                    , nl = "%nl:FormattingList"
                    , hu = "%hu:FormattingList"
                    , sv = "%sv:FormattingList"
                    , ca = "%ca:FormattingList"
                    , br = "%br:FormattingList"
                    }

                FormattingLink ->
                    { en = "[link](http://t.co)"
                    , zh_hans = "%zh_hans:FormattingLink"
                    , zh_hant = "%zh_hant:FormattingLink"
                    , es = "%es:FormattingLink"
                    , fr = "%fr:FormattingLink"
                    , ru = "%ru:FormattingLink"
                    , de = "%de:FormattingLink"
                    , nl = "%nl:FormattingLink"
                    , hu = "%hu:FormattingLink"
                    , sv = "%sv:FormattingLink"
                    , ca = "%ca:FormattingLink"
                    , br = "%br:FormattingLink"
                    }

                ParenNumber ->
                    { en = "ParenNumber"
                    , zh_hans = "%zh_hans:ParenNumber"
                    , zh_hant = "%zh_hant:ParenNumber"
                    , es = "%es:ParenNumber"
                    , fr = "%fr:ParenNumber"
                    , ru = "%ru:ParenNumber"
                    , de = "%de:ParenNumber"
                    , nl = "%nl:ParenNumber"
                    , hu = "%hu:ParenNumber"
                    , sv = "%sv:ParenNumber"
                    , ca = "%ca:ParenNumber"
                    , br = "%br:ParenNumber"
                    }

                SetHeadingLevel ->
                    { en = "SetHeadingLevel"
                    , zh_hans = "%zh_hans:SetHeadingLevel"
                    , zh_hant = "%zh_hant:SetHeadingLevel"
                    , es = "%es:SetHeadingLevel"
                    , fr = "%fr:SetHeadingLevel"
                    , ru = "%ru:SetHeadingLevel"
                    , de = "%de:SetHeadingLevel"
                    , nl = "%nl:SetHeadingLevel"
                    , hu = "%hu:SetHeadingLevel"
                    , sv = "%sv:SetHeadingLevel"
                    , ca = "%ca:SetHeadingLevel"
                    , br = "%br:SetHeadingLevel"
                    }

                --
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

                OpenQuickSwitcher ->
                    { en = "Open Quick Switcher"
                    , zh_hans = "%zh_hans:OpenQuickSwitcher"
                    , zh_hant = "%zh_hant:OpenQuickSwitcher"
                    , es = "%es:OpenQuickSwitcher"
                    , fr = "%fr:OpenQuickSwitcher"
                    , ru = "%ru:OpenQuickSwitcher"
                    , de = "%de:OpenQuickSwitcher"
                    , nl = "%nl:OpenQuickSwitcher"
                    , hu = "%hu:OpenQuickSwitcher"
                    , sv = "%sv:OpenQuickSwitcher"
                    , ca = "%ca:OpenQuickSwitcher"
                    , br = "%br:OpenQuickSwitcher"
                    }

                EmailSupport ->
                    { en = "Contact Support"
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

                VersionHistory ->
                    { en = "Version History"
                    , zh_hans = "%zh_hans:VersionHistory"
                    , zh_hant = "%zh_hant:VersionHistory"
                    , es = "%es:VersionHistory"
                    , fr = "%fr:VersionHistory"
                    , ru = "%ru:VersionHistory"
                    , de = "%de:VersionHistory"
                    , nl = "%nl:VersionHistory"
                    , hu = "%hu:VersionHistory"
                    , sv = "%sv:VersionHistory"
                    , ca = "%ca:VersionHistory"
                    , br = "%br:VersionHistory"
                    }

                DocumentSettings ->
                    { en = "Document Settings"
                    , zh_hans = "%zh_hans:DocumentSettings"
                    , zh_hant = "%zh_hant:DocumentSettings"
                    , es = "%es:DocumentSettings"
                    , fr = "%fr:DocumentSettings"
                    , ru = "%ru:DocumentSettings"
                    , de = "%de:DocumentSettings"
                    , nl = "%nl:DocumentSettings"
                    , hu = "%hu:DocumentSettings"
                    , sv = "%sv:DocumentSettings"
                    , ca = "%ca:DocumentSettings"
                    , br = "%br:DocumentSettings"
                    }

                WordCount ->
                    { en = "Word count..."
                    , zh_hans = "%zh_hans:WordCount"
                    , zh_hant = "%zh_hant:WordCount"
                    , es = "%es:WordCount"
                    , fr = "%fr:WordCount"
                    , ru = "%ru:WordCount"
                    , de = "%de:WordCount"
                    , nl = "%nl:WordCount"
                    , hu = "%hu:WordCount"
                    , sv = "%sv:WordCount"
                    , ca = "%ca:WordCount"
                    , br = "%br:WordCount"
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

                WordCountTotalCards n ->
                    { en = numberPlural n "Total Cards in Tree : %1" "Total Cards in Tree : %1"
                    , zh_hans = numberPlural n "%zh_hans:WordCountTotalCards:0" "%zh_hans:WordCountTotalCards:1"
                    , zh_hant = numberPlural n "%zh_hant:WordCountTotalCards:0" "%zh_hant:WordCountTotalCards:1"
                    , es = numberPlural n "%es:WordCountTotalCards:0" "%es:WordCountTotalCards:1"
                    , fr = numberPlural n "%fr:WordCountTotalCards:0" "%fr:WordCountTotalCards:1"
                    , ru = numberPlural n "%ru:WordCountTotalCards:0" "%ru:WordCountTotalCards:1"
                    , de = numberPlural n "%de:WordCountTotalCards:0" "%de:WordCountTotalCards:1"
                    , nl = numberPlural n "%nl:WordCountTotalCards:0" "%nl:WordCountTotalCards:1"
                    , hu = numberPlural n "%hu:WordCountTotalCards:0" "%hu:WordCountTotalCards:1"
                    , sv = numberPlural n "%sv:WordCountTotalCards:0" "%sv:WordCountTotalCards:1"
                    , ca = numberPlural n "%ca:WordCountTotalCards:0" "%ca:WordCountTotalCards:1"
                    , br = numberPlural n "%br:WordCountTotalCards:0" "%br:WordCountTotalCards:1"
                    }

                DocumentTheme ->
                    { en = "Document Theme"
                    , zh_hans = "%zh_hans:DocumentTheme"
                    , zh_hant = "%zh_hant:DocumentTheme"
                    , es = "%es:DocumentTheme"
                    , fr = "%fr:DocumentTheme"
                    , ru = "%ru:DocumentTheme"
                    , de = "%de:DocumentTheme"
                    , nl = "%nl:DocumentTheme"
                    , hu = "%hu:DocumentTheme"
                    , sv = "%sv:DocumentTheme"
                    , ca = "%ca:DocumentTheme"
                    , br = "%br:DocumentTheme"
                    }

                ThemeDefault ->
                    { en = "Default"
                    , zh_hans = "%zh_hans:ThemeDefault"
                    , zh_hant = "%zh_hant:ThemeDefault"
                    , es = "%es:ThemeDefault"
                    , fr = "%fr:ThemeDefault"
                    , ru = "%ru:ThemeDefault"
                    , de = "%de:ThemeDefault"
                    , nl = "%nl:ThemeDefault"
                    , hu = "%hu:ThemeDefault"
                    , sv = "%sv:ThemeDefault"
                    , ca = "%ca:ThemeDefault"
                    , br = "%br:ThemeDefault"
                    }

                ThemeDarkMode ->
                    { en = "Dark Mode"
                    , zh_hans = "%zh_hans:ThemeDarkMode"
                    , zh_hant = "%zh_hant:ThemeDarkMode"
                    , es = "%es:ThemeDarkMode"
                    , fr = "%fr:ThemeDarkMode"
                    , ru = "%ru:ThemeDarkMode"
                    , de = "%de:ThemeDarkMode"
                    , nl = "%nl:ThemeDarkMode"
                    , hu = "%hu:ThemeDarkMode"
                    , sv = "%sv:ThemeDarkMode"
                    , ca = "%ca:ThemeDarkMode"
                    , br = "%br:ThemeDarkMode"
                    }

                ThemeClassic ->
                    { en = "Classic Gingkoapp"
                    , zh_hans = "%zh_hans:ThemeClassic"
                    , zh_hant = "%zh_hant:ThemeClassic"
                    , es = "%es:ThemeClassic"
                    , fr = "%fr:ThemeClassic"
                    , ru = "%ru:ThemeClassic"
                    , de = "%de:ThemeClassic"
                    , nl = "%nl:ThemeClassic"
                    , hu = "%hu:ThemeClassic"
                    , sv = "%sv:ThemeClassic"
                    , ca = "%ca:ThemeClassic"
                    , br = "%br:ThemeClassic"
                    }

                ThemeGray ->
                    { en = "Gray"
                    , zh_hans = "%zh_hans:ThemeGray"
                    , zh_hant = "%zh_hant:ThemeGray"
                    , es = "%es:ThemeGray"
                    , fr = "%fr:ThemeGray"
                    , ru = "%ru:ThemeGray"
                    , de = "%de:ThemeGray"
                    , nl = "%nl:ThemeGray"
                    , hu = "%hu:ThemeGray"
                    , sv = "%sv:ThemeGray"
                    , ca = "%ca:ThemeGray"
                    , br = "%br:ThemeGray"
                    }

                ThemeGreen ->
                    { en = "Green"
                    , zh_hans = "%zh_hans:ThemeGreen"
                    , zh_hant = "%zh_hant:ThemeGreen"
                    , es = "%es:ThemeGreen"
                    , fr = "%fr:ThemeGreen"
                    , ru = "%ru:ThemeGreen"
                    , de = "%de:ThemeGreen"
                    , nl = "%nl:ThemeGreen"
                    , hu = "%hu:ThemeGreen"
                    , sv = "%sv:ThemeGreen"
                    , ca = "%ca:ThemeGreen"
                    , br = "%br:ThemeGreen"
                    }

                ThemeTurquoise ->
                    { en = "Turquoise"
                    , zh_hans = "%zh_hans:ThemeTurquoise"
                    , zh_hant = "%zh_hant:ThemeTurquoise"
                    , es = "%es:ThemeTurquoise"
                    , fr = "%fr:ThemeTurquoise"
                    , ru = "%ru:ThemeTurquoise"
                    , de = "%de:ThemeTurquoise"
                    , nl = "%nl:ThemeTurquoise"
                    , hu = "%hu:ThemeTurquoise"
                    , sv = "%sv:ThemeTurquoise"
                    , ca = "%ca:ThemeTurquoise"
                    , br = "%br:ThemeTurquoise"
                    }

                -- Exporting
                ExportOrPrint ->
                    { en = "Export or Print"
                    , zh_hans = "%zh_hans:ExportOrPrint"
                    , zh_hant = "%zh_hant:ExportOrPrint"
                    , es = "%es:ExportOrPrint"
                    , fr = "%fr:ExportOrPrint"
                    , ru = "%ru:ExportOrPrint"
                    , de = "%de:ExportOrPrint"
                    , nl = "%nl:ExportOrPrint"
                    , hu = "%hu:ExportOrPrint"
                    , sv = "%sv:ExportOrPrint"
                    , ca = "%ca:ExportOrPrint"
                    , br = "%br:ExportOrPrint"
                    }

                ExportSettingEverything ->
                    { en = "Everything"
                    , zh_hans = "%zh_hans:ExportSettingEverything"
                    , zh_hant = "%zh_hant:ExportSettingEverything"
                    , es = "%es:ExportSettingEverything"
                    , fr = "%fr:ExportSettingEverything"
                    , ru = "%ru:ExportSettingEverything"
                    , de = "%de:ExportSettingEverything"
                    , nl = "%nl:ExportSettingEverything"
                    , hu = "%hu:ExportSettingEverything"
                    , sv = "%sv:ExportSettingEverything"
                    , ca = "%ca:ExportSettingEverything"
                    , br = "%br:ExportSettingEverything"
                    }

                ExportSettingEverythingDesc ->
                    { en = "All cards in the tree (in depth-first order)"
                    , zh_hans = "%zh_hans:ExportSettingEverythingDesc"
                    , zh_hant = "%zh_hant:ExportSettingEverythingDesc"
                    , es = "%es:ExportSettingEverythingDesc"
                    , fr = "%fr:ExportSettingEverythingDesc"
                    , ru = "%ru:ExportSettingEverythingDesc"
                    , de = "%de:ExportSettingEverythingDesc"
                    , nl = "%nl:ExportSettingEverythingDesc"
                    , hu = "%hu:ExportSettingEverythingDesc"
                    , sv = "%sv:ExportSettingEverythingDesc"
                    , ca = "%ca:ExportSettingEverythingDesc"
                    , br = "%br:ExportSettingEverythingDesc"
                    }

                ExportSettingCurrentSubtree ->
                    { en = "Current Subtree"
                    , zh_hans = "%zh_hans:ExportSettingCurrentSubtree"
                    , zh_hant = "%zh_hant:ExportSettingCurrentSubtree"
                    , es = "%es:ExportSettingCurrentSubtree"
                    , fr = "%fr:ExportSettingCurrentSubtree"
                    , ru = "%ru:ExportSettingCurrentSubtree"
                    , de = "%de:ExportSettingCurrentSubtree"
                    , nl = "%nl:ExportSettingCurrentSubtree"
                    , hu = "%hu:ExportSettingCurrentSubtree"
                    , sv = "%sv:ExportSettingCurrentSubtree"
                    , ca = "%ca:ExportSettingCurrentSubtree"
                    , br = "%br:ExportSettingCurrentSubtree"
                    }

                ExportSettingCurrentSubtreeDesc ->
                    { en = "Current card and all its children"
                    , zh_hans = "%zh_hans:ExportSettingCurrentSubtreeDesc"
                    , zh_hant = "%zh_hant:ExportSettingCurrentSubtreeDesc"
                    , es = "%es:ExportSettingCurrentSubtreeDesc"
                    , fr = "%fr:ExportSettingCurrentSubtreeDesc"
                    , ru = "%ru:ExportSettingCurrentSubtreeDesc"
                    , de = "%de:ExportSettingCurrentSubtreeDesc"
                    , nl = "%nl:ExportSettingCurrentSubtreeDesc"
                    , hu = "%hu:ExportSettingCurrentSubtreeDesc"
                    , sv = "%sv:ExportSettingCurrentSubtreeDesc"
                    , ca = "%ca:ExportSettingCurrentSubtreeDesc"
                    , br = "%br:ExportSettingCurrentSubtreeDesc"
                    }

                ExportSettingLeavesOnly ->
                    { en = "Leaves-only"
                    , zh_hans = "%zh_hans:ExportSettingLeavesOnly"
                    , zh_hant = "%zh_hant:ExportSettingLeavesOnly"
                    , es = "%es:ExportSettingLeavesOnly"
                    , fr = "%fr:ExportSettingLeavesOnly"
                    , ru = "%ru:ExportSettingLeavesOnly"
                    , de = "%de:ExportSettingLeavesOnly"
                    , nl = "%nl:ExportSettingLeavesOnly"
                    , hu = "%hu:ExportSettingLeavesOnly"
                    , sv = "%sv:ExportSettingLeavesOnly"
                    , ca = "%ca:ExportSettingLeavesOnly"
                    , br = "%br:ExportSettingLeavesOnly"
                    }

                ExportSettingLeavesOnlyDesc ->
                    { en = "Only cards without children"
                    , zh_hans = "%zh_hans:ExportSettingLeavesOnlyDesc"
                    , zh_hant = "%zh_hant:ExportSettingLeavesOnlyDesc"
                    , es = "%es:ExportSettingLeavesOnlyDesc"
                    , fr = "%fr:ExportSettingLeavesOnlyDesc"
                    , ru = "%ru:ExportSettingLeavesOnlyDesc"
                    , de = "%de:ExportSettingLeavesOnlyDesc"
                    , nl = "%nl:ExportSettingLeavesOnlyDesc"
                    , hu = "%hu:ExportSettingLeavesOnlyDesc"
                    , sv = "%sv:ExportSettingLeavesOnlyDesc"
                    , ca = "%ca:ExportSettingLeavesOnlyDesc"
                    , br = "%br:ExportSettingLeavesOnlyDesc"
                    }

                ExportSettingCurrentColumn ->
                    { en = "Current Column"
                    , zh_hans = "%zh_hans:ExportSettingCurrentColumn"
                    , zh_hant = "%zh_hant:ExportSettingCurrentColumn"
                    , es = "%es:ExportSettingCurrentColumn"
                    , fr = "%fr:ExportSettingCurrentColumn"
                    , ru = "%ru:ExportSettingCurrentColumn"
                    , de = "%de:ExportSettingCurrentColumn"
                    , nl = "%nl:ExportSettingCurrentColumn"
                    , hu = "%hu:ExportSettingCurrentColumn"
                    , sv = "%sv:ExportSettingCurrentColumn"
                    , ca = "%ca:ExportSettingCurrentColumn"
                    , br = "%br:ExportSettingCurrentColumn"
                    }

                ExportSettingCurrentColumnDesc ->
                    { en = "Only carsd in the current (vertical) column"
                    , zh_hans = "%zh_hans:ExportSettingCurrentColumnDesc"
                    , zh_hant = "%zh_hant:ExportSettingCurrentColumnDesc"
                    , es = "%es:ExportSettingCurrentColumnDesc"
                    , fr = "%fr:ExportSettingCurrentColumnDesc"
                    , ru = "%ru:ExportSettingCurrentColumnDesc"
                    , de = "%de:ExportSettingCurrentColumnDesc"
                    , nl = "%nl:ExportSettingCurrentColumnDesc"
                    , hu = "%hu:ExportSettingCurrentColumnDesc"
                    , sv = "%sv:ExportSettingCurrentColumnDesc"
                    , ca = "%ca:ExportSettingCurrentColumnDesc"
                    , br = "%br:ExportSettingCurrentColumnDesc"
                    }

                ExportSettingWord ->
                    { en = "Word"
                    , zh_hans = "%zh_hans:ExportSettingWord"
                    , zh_hant = "%zh_hant:ExportSettingWord"
                    , es = "%es:ExportSettingWord"
                    , fr = "%fr:ExportSettingWord"
                    , ru = "%ru:ExportSettingWord"
                    , de = "%de:ExportSettingWord"
                    , nl = "%nl:ExportSettingWord"
                    , hu = "%hu:ExportSettingWord"
                    , sv = "%sv:ExportSettingWord"
                    , ca = "%ca:ExportSettingWord"
                    , br = "%br:ExportSettingWord"
                    }

                ExportSettingPlainText ->
                    { en = "Plain Text"
                    , zh_hans = "%zh_hans:ExportSettingPlainText"
                    , zh_hant = "%zh_hant:ExportSettingPlainText"
                    , es = "%es:ExportSettingPlainText"
                    , fr = "%fr:ExportSettingPlainText"
                    , ru = "%ru:ExportSettingPlainText"
                    , de = "%de:ExportSettingPlainText"
                    , nl = "%nl:ExportSettingPlainText"
                    , hu = "%hu:ExportSettingPlainText"
                    , sv = "%sv:ExportSettingPlainText"
                    , ca = "%ca:ExportSettingPlainText"
                    , br = "%br:ExportSettingPlainText"
                    }

                ExportSettingJSON ->
                    { en = "JSON"
                    , zh_hans = "%zh_hans:ExportSettingJSON"
                    , zh_hant = "%zh_hant:ExportSettingJSON"
                    , es = "%es:ExportSettingJSON"
                    , fr = "%fr:ExportSettingJSON"
                    , ru = "%ru:ExportSettingJSON"
                    , de = "%de:ExportSettingJSON"
                    , nl = "%nl:ExportSettingJSON"
                    , hu = "%hu:ExportSettingJSON"
                    , sv = "%sv:ExportSettingJSON"
                    , ca = "%ca:ExportSettingJSON"
                    , br = "%br:ExportSettingJSON"
                    }

                DownloadWordFile ->
                    { en = "Download Word File"
                    , zh_hans = "%zh_hans:DownloadWordFile"
                    , zh_hant = "%zh_hant:DownloadWordFile"
                    , es = "%es:DownloadWordFile"
                    , fr = "%fr:DownloadWordFile"
                    , ru = "%ru:DownloadWordFile"
                    , de = "%de:DownloadWordFile"
                    , nl = "%nl:DownloadWordFile"
                    , hu = "%hu:DownloadWordFile"
                    , sv = "%sv:DownloadWordFile"
                    , ca = "%ca:DownloadWordFile"
                    , br = "%br:DownloadWordFile"
                    }

                DownloadTextFile ->
                    { en = "Download Markdown text file"
                    , zh_hans = "%zh_hans:DownloadTextFile"
                    , zh_hant = "%zh_hant:DownloadTextFile"
                    , es = "%es:DownloadTextFile"
                    , fr = "%fr:DownloadTextFile"
                    , ru = "%ru:DownloadTextFile"
                    , de = "%de:DownloadTextFile"
                    , nl = "%nl:DownloadTextFile"
                    , hu = "%hu:DownloadTextFile"
                    , sv = "%sv:DownloadTextFile"
                    , ca = "%ca:DownloadTextFile"
                    , br = "%br:DownloadTextFile"
                    }

                DownloadJSONFile ->
                    { en = "Download JSON file"
                    , zh_hans = "%zh_hans:DownloadJSONFile"
                    , zh_hant = "%zh_hant:DownloadJSONFile"
                    , es = "%es:DownloadJSONFile"
                    , fr = "%fr:DownloadJSONFile"
                    , ru = "%ru:DownloadJSONFile"
                    , de = "%de:DownloadJSONFile"
                    , nl = "%nl:DownloadJSONFile"
                    , hu = "%hu:DownloadJSONFile"
                    , sv = "%sv:DownloadJSONFile"
                    , ca = "%ca:DownloadJSONFile"
                    , br = "%br:DownloadJSONFile"
                    }

                PrintThis ->
                    { en = "Print this"
                    , zh_hans = "%zh_hans:PrintThis"
                    , zh_hant = "%zh_hant:PrintThis"
                    , es = "%es:PrintThis"
                    , fr = "%fr:PrintThis"
                    , ru = "%ru:PrintThis"
                    , de = "%de:PrintThis"
                    , nl = "%nl:PrintThis"
                    , hu = "%hu:PrintThis"
                    , sv = "%sv:PrintThis"
                    , ca = "%ca:PrintThis"
                    , br = "%br:PrintThis"
                    }

                -- Upgrade & Subscription
                Upgrade ->
                    { en = "Upgrade"
                    , zh_hans = "%zh_hans:Upgrade"
                    , zh_hant = "%zh_hant:Upgrade"
                    , es = "%es:Upgrade"
                    , fr = "%fr:Upgrade"
                    , ru = "%ru:Upgrade"
                    , de = "%de:Upgrade"
                    , nl = "%nl:Upgrade"
                    , hu = "%hu:Upgrade"
                    , sv = "%sv:Upgrade"
                    , ca = "%ca:Upgrade"
                    , br = "%br:Upgrade"
                    }

                DaysLeft n ->
                    { en = numberPlural n "%1 day left in trial" "%1 days left in trial"
                    , zh_hans = numberPlural n "%zh_hans:DaysLeft:0" "%zh_hans:DaysLeft:1"
                    , zh_hant = numberPlural n "%zh_hant:DaysLeft:0" "%zh_hant:DaysLeft:1"
                    , es = numberPlural n "%es:DaysLeft:0" "%es:DaysLeft:1"
                    , fr = numberPlural n "%fr:DaysLeft:0" "%fr:DaysLeft:1"
                    , ru = numberPlural n "%ru:DaysLeft:0" "%ru:DaysLeft:1"
                    , de = numberPlural n "%de:DaysLeft:0" "%de:DaysLeft:1"
                    , nl = numberPlural n "%nl:DaysLeft:0" "%nl:DaysLeft:1"
                    , hu = numberPlural n "%hu:DaysLeft:0" "%hu:DaysLeft:1"
                    , sv = numberPlural n "%sv:DaysLeft:0" "%sv:DaysLeft:1"
                    , ca = numberPlural n "%ca:DaysLeft:0" "%ca:DaysLeft:1"
                    , br = numberPlural n "%br:DaysLeft:0" "%br:DaysLeft:1"
                    }

                TrialExpired ->
                    { en = "Trial Expired"
                    , zh_hans = "%zh_hans:TrialExpired"
                    , zh_hant = "%zh_hant:TrialExpired"
                    , es = "%es:TrialExpired"
                    , fr = "%fr:TrialExpired"
                    , ru = "%ru:TrialExpired"
                    , de = "%de:TrialExpired"
                    , nl = "%nl:TrialExpired"
                    , hu = "%hu:TrialExpired"
                    , sv = "%sv:TrialExpired"
                    , ca = "%ca:TrialExpired"
                    , br = "%br:TrialExpired"
                    }

                ManageSubscription ->
                    { en = "Manage Subscription"
                    , zh_hans = "%zh_hans:ManageSubscription"
                    , zh_hant = "%zh_hant:ManageSubscription"
                    , es = "%es:ManageSubscription"
                    , fr = "%fr:ManageSubscription"
                    , ru = "%ru:ManageSubscription"
                    , de = "%de:ManageSubscription"
                    , nl = "%nl:ManageSubscription"
                    , hu = "%hu:ManageSubscription"
                    , sv = "%sv:ManageSubscription"
                    , ca = "%ca:ManageSubscription"
                    , br = "%br:ManageSubscription"
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
