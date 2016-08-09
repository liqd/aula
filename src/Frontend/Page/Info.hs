{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Werror #-}

module Frontend.Page.Info
where

import Access (publicPage)
import Action (ActionM, query)
import Data.Markdown
import Frontend.Prelude
import Persistent (termsOfUse)

import qualified Data.Text as ST


-- | 14. Info page: Imprint
data PageStaticImprint = PageStaticImprint
  deriving (Eq, Show, Read)

instance Page PageStaticImprint where
    isAuthorized = publicPage


instance ToHtml PageStaticImprint where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p $ imprintMarkdown ^. html

imprintMarkdown :: Document
Right imprintMarkdown = markdown $ ST.unlines
    [ "# Impressum"
    , ""
    , "### Angaben gemäß § 5 TMG:"
    , ""
    , "- politik-digital e. V."
    , "- Alte Schönhauser Straße 23"
    , "- 10119 Berlin"
    , ""
    , "### Vertreten durch:"
    , ""
    , "- Steffen Wenzel (Geschäftsführung)"
    , "- Marina Weisband (Projektleitung)"
    , ""
    , "### Kontakt:"
    , ""
    , "- Email: info@aula.de"
    , ""
    , ""
    , "## Haftungsausschluss (Disclaimer)"
    , ""
    , "### Haftung für Inhalte"
    , ""
    , "Als Diensteanbieter sind wir gemäß § 7 Abs.1 TMG für eigene Inhalte auf diesen Seiten nach den allgemeinen Gesetzen verantwortlich. Nach §§ 8 bis 10 TMG sind wir als Diensteanbieter jedoch nicht verpflichtet, übermittelte oder gespeicherte fremde Informationen zu überwachen oder nach Umständen zu forschen, die auf eine rechtswidrige Tätigkeit hinweisen. Verpflichtungen zur Entfernung oder Sperrung der Nutzung von Informationen nach den allgemeinen Gesetzen bleiben hiervon unberührt. Eine diesbezügliche Haftung ist jedoch erst ab dem Zeitpunkt der Kenntnis einer konkreten Rechtsverletzung möglich. Bei Bekanntwerden von entsprechenden Rechtsverletzungen werden wir diese Inhalte umgehend entfernen."
    , ""
    , "### Haftung für Links"
    , ""
    , "Unser Angebot enthält Links zu externen Webseiten Dritter, auf deren Inhalte wir keinen Einfluss haben. Deshalb können wir für diese fremden Inhalte auch keine Gewähr übernehmen. Für die Inhalte der verlinkten Seiten ist stets der jeweilige Anbieter oder Betreiber der Seiten verantwortlich. Die verlinkten Seiten wurden zum Zeitpunkt der Verlinkung auf mögliche Rechtsverstöße überprüft. Rechtswidrige Inhalte waren zum Zeitpunkt der Verlinkung nicht erkennbar. Eine permanente inhaltliche Kontrolle der verlinkten Seiten ist jedoch ohne konkrete Anhaltspunkte einer Rechtsverletzung nicht zumutbar. Bei Bekanntwerden von Rechtsverletzungen werden wir derartige Links umgehend entfernen."
    , ""
    , "### Urheberrecht"
    , ""
    , "Die durch die Seitenbetreiber erstellten Inhalte und Werke auf diesen Seiten unterliegen dem deutschen Urheberrecht. Die Vervielfältigung, Bearbeitung, Verbreitung und jede Art der Verwertung außerhalb der Grenzen des Urheberrechtes bedürfen der schriftlichen Zustimmung des jeweiligen Autors bzw. Erstellers. Downloads und Kopien dieser Seite sind nur für den privaten, nicht kommerziellen Gebrauch gestattet. Soweit die Inhalte auf dieser Seite nicht vom Betreiber erstellt wurden, werden die Urheberrechte Dritter beachtet. Insbesondere werden Inhalte Dritter als solche gekennzeichnet. Sollten Sie trotzdem auf eine Urheberrechtsverletzung aufmerksam werden, bitten wir um einen entsprechenden Hinweis. Bei Bekanntwerden von Rechtsverletzungen werden wir derartige Inhalte umgehend entfernen."
    , ""
    , ""
    , "## Datenschutzerklärung:"
    , ""
    , "### Datenschutz"
    , ""
    , "Die Betreiber dieser Seiten nehmen den Schutz Ihrer persönlichen Daten sehr ernst. Wir behandeln Ihre personenbezogenen Daten vertraulich und entsprechend der gesetzlichen Datenschutzvorschriften sowie dieser Datenschutzerklärung."
    , ""
    , "Soweit auf unseren Seiten personenbezogene Daten (beispielsweise Name, Anschrift oder E-Mail-Adressen) erhoben werden, erfolgt dies, soweit möglich, stets auf freiwilliger Basis. Diese Daten werden ohne Ihre ausdrückliche Zustimmung nicht an Dritte weitergegeben."
    , ""
    , "Wir weisen darauf hin, dass die Datenübertragung im Internet (z.B. bei der Kommunikation per E-Mail) Sicherheitslücken aufweisen kann. Ein lückenloser Schutz der Daten vor dem Zugriff durch Dritte ist nicht möglich."
    , ""
    , "### Auskunft, Löschung, Sperrung"
    , ""
    , "Sie haben jederzeit das Recht auf unentgeltliche Auskunft über Ihre gespeicherten personenbezogenen Daten, deren Herkunft und Empfänger und den Zweck der Datenverarbeitung sowie ein Recht auf Berichtigung, Sperrung oder Löschung dieser Daten. Hierzu sowie zu weiteren Fragen zum Thema personenbezogene Daten können Sie sich jederzeit unter der im Impressum angegebenen Adresse an uns wenden."
    , ""
    , "### Cookies"
    , ""
    , "Die Internetseiten verwenden teilweise so genannte Cookies. Cookies richten auf Ihrem Rechner keinen Schaden an und enthalten keine Viren. Cookies dienen dazu, unser Angebot nutzerfreundlicher, effektiver und sicherer zu machen. Cookies sind kleine Textdateien, die auf Ihrem Rechner abgelegt werden und die Ihr Browser speichert."
    , ""
    , "Die meisten der von uns verwendeten Cookies sind so genannte „Session-Cookies“. Sie werden nach Ende Ihres Besuchs automatisch gelöscht. Andere Cookies bleiben auf Ihrem Endgerät gespeichert, bis Sie diese löschen. Diese Cookies ermöglichen es uns, Ihren Browser beim nächsten Besuch wiederzuerkennen."
    , ""
    , "Sie können Ihren Browser so einstellen, dass Sie über das Setzen von Cookies informiert werden und Cookies nur im Einzelfall erlauben, die Annahme von Cookies für bestimmte Fälle oder generell ausschließen sowie das automatische Löschen der Cookies beim Schließen des Browser aktivieren. Bei der Deaktivierung von Cookies kann die Funktionalität dieser Website eingeschränkt sein."
    , ""
    , "### Server-Log-Files"
    , ""
    , "Der Provider der Seiten behält sich vor Informationen in sogenannten Server-Log Files automatisch zu erheben und zu speichern. Diese Daten werden von Ihrem Browser automatisch an uns übermittelt. Dies sind:"
    , ""
    , "- Browsertyp/ Browserversion"
    , "- verwendetes Betriebssystem"
    , "- Referrer URL"
    , "- Hostname des zugreifenden Rechners"
    , "- Uhrzeit der Serveranfrage"
    , ""
    , "Diese Daten sind nicht bestimmten Personen zuordenbar. Eine Zusammenführung dieser Daten mit anderen Datenquellen wird nicht vorgenommen. Wir behalten uns vor, diese Daten nachträglich zu prüfen, wenn uns konkrete Anhaltspunkte für eine rechtswidrige Nutzung bekannt werden."
    ]

-- | 15. Info page: Terms of use
data PageTermsOfUse = PageTermsOfUse Document
  deriving (Eq, Show, Read)

instance Page PageTermsOfUse where
    isAuthorized = publicPage


instance ToHtml PageTermsOfUse where
    toHtmlRaw = toHtml
    toHtml p@(PageTermsOfUse terms) = semanticDiv p .
        div_ [class_ "text-markdown"] $
            terms ^. html

termsOfUse :: ActionM m => m PageTermsOfUse
termsOfUse = PageTermsOfUse <$> query Persistent.termsOfUse
