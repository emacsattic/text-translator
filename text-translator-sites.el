;;; text-translator-sites.el --- Text Translator

;; Copyright (C) 2011  khiker

;; Author: khiker <khiker.mail+elisp@gmail.com>
;;         plus   <MLB33828@nifty.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Site data for text-translator.el

;;; Code:


;; Variables:

(defcustom text-translator-site-data-template-alist
  '(;; Google.com
    ("google.com"
     "translate.google.com"
     "/ HTTP/1.1"
     "js=n&prev=_t&hl=ja&ie=UTF-8&text=%s&file=&sl=%o&tl=%t"
     utf-8
     (lambda ()
       (text-translator-extract-tag-exclusion-string
        "<span id=result_box[^>]*>\\(\\(<span [^>]*>\\([^<]\\|<br>\\)*</span>\\)+\\)</span>"))
     (("en" . "ja") ("ja" . "en")
      ("en" . "es") ("es" . "en")
      ("en" . "fr") ("fr" . "en")
      ("en" . "de") ("de" . "en")
      ("en" . "it") ("it" . "en")
      ("en" . "ar") ("ar" . "en")
      ("de" . "fr") ("fr" . "de")
      ("en" . "pt") ("pt" . "en")
      ("en" . "ru") ("ru" . "en")
      ("en" . "ko") ("ko" . "en")
      ("en" . "zh-CN") ("zh-CN" . "en")
      ("en" . "zh-TW") ("zh-TW" . "en")
      ("zh-CN" . "zh-TW") ("zh-TW" . "zh-CN"))
     (("zh-TW" . "tw") ("zh-CN" . "ch")))

    ;; Yahoo.com
    ("yahoo.com"
     "babelfish.yahoo.com"
     "/translate_txt HTTP/1.1"
     "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=%o_%t&btnTrTxt=Translate"
     utf-8-dos
     "    <div id=\"result\"><div style=\"padding:0.6em;\">\\([^<]*\\)</div>"
     (("en" . "ja") ("ja" . "en")
      ("en" . "nl") ("nl" . "en")
      ("en" . "fr") ("fr" . "en")
      ("en" . "de") ("de" . "en")
      ("en" . "el") ("el" . "en")
      ("en" . "it") ("it" . "en")
      ("en" . "ko") ("ko" . "en")
      ("en" . "pt") ("pt" . "en")
      ("en" . "ru") ("ru" . "en")
      ("en" . "es") ("es" . "en")
      ("nl" . "fr")
      ("fr" . "de")
      ("fr" . "el") ("el" . "fr")
      ("fr" . "it") ("it" . "fr")
      ("fr" . "pt") ("pt" . "fr")
      ("fr" . "es") ("es" . "fr")
      ("en" . "zh") ("zh" . "en")
      ("en" . "zt") ("zt" . "en"))
     (("zh" . "ch") ("zt" . "tw")))

    ;; Freetranslation.com
    ("freetranslation.com"
     "ets.freetranslation.com"
     "/ HTTP/1.1"
     "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=%o/%t&srctext=%s"
     utf-8
     "<textarea name=\"dsttext\" cols=\"40\" rows=\"6\" style=\"width:99%;height:142px;\" id=\"resultsBox\">\\([^<]*\\)</textarea>"
     (("English" . "Spanish") ("Spanish" . "English")
      ("English" . "French")  ("French" . "English")
      ("English" . "German")  ("German" . "English")
      ("English" . "Italian") ("Italian" . "English")
      ("English" . "Dutch")   ("Dutch" . "English")
      ("English" . "Portuguese") ("Portuguese" . "English")
      ("English" . "Norwegian"))
     (("English" . "en") ("Spanish" . "es")
      ("French" . "fr") ("German" . "de")
      ("Italian" . "it") ("Dutch" . "nl")
      ("Portuguese" . "pt") ("Norwegian" . "no")))
    ("freetranslation.com"
     "ets6.freetranslation.com"
     "/ HTTP/1.1"
     "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=%o/%t&srctext=%s"
     utf-8
     "<textarea name=\"dsttext\" cols=\"40\" rows=\"6\" style=\"width:99%;height:142px;\" id=\"resultsBox\">\\([^<]*\\)</textarea>"
     (("English" . "Russian") ("Russian" . "English")
      ("English" . "SimplifiedChinese")
      ("English" . "TraditionalChinese"))
     (("English" . "en") ("Russian" . "ru")
      ("SimplifiedChinese" . "ch") ("TraditionalChinese" . "tw")))
    ("freetranslation.com"
     "tets9.freetranslation.com"
     "/ HTTP/1.1"
     "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=%o/%t&srctext=%s"
     utf-8-dos
     "<textarea name=\"dsttext\" cols=\"40\" rows=\"6\" style=\"width:99%;height:142px;\" id=\"resultsBox\">\\([^<]*\\)</textarea>"
     (("English" . "Japanese") ("Japanese" . "English"))
     (("English" . "en") ("Japanese" . "ja")))

    ;; Livedoor.com
    ("livedoor.com"
     "translate.livedoor.com"
     "/ HTTP/1.1"
     "clear_flg=1&src_text=%s&trns_type=%o,%t&sumit=翻訳"
     utf-8-dos
     "<textarea name=\"tar_text\" cols=\"40\" rows=\"10\" wrap=\"physical\">\\([^<]*\\)</textarea>"
     (("1" . "2") ("2" . "1")
      ("2" . "9") ("9" . "2")
      ("2" . "6") ("6" . "2"))
     (("1" . "en") ("2" . "ja")
      ("6" . "ch") ("9" . "ko")))

    ;; Fresheye.com
    ("fresheye.com"
     "mt.fresheye.com"
     "/ft_result.cgi HTTP/1.1"
     "gen_text=%s&e=%o%t"
     utf-8-dos
     "<TEXTAREA class=\"out-form\" name=\"gen_text2\" cols=\"25\" rows=\"15\">\\([^<]*\\)</TEXTAREA>"
     (("E" . "J") ("J" . "E"))
     (("E" . "en") ("J" . "ja")))
    ("fresheye.com"
     "mt.fresheye.com"
     "/ft_cjresult.cgi HTTP/1.1"
     "gen_text=%s&charset=gb2312&cjjc=%o%t"
     utf-8
     "<TEXTAREA class=\"out-form\" name=\"gen_text2\" cols=\"25\" rows=\"15\">\\([^<]*\\)</TEXTAREA>"
     (("c" . "j") ("j" . "c"))
     (("c" . "ch") ("j" . "ja")))
    ("fresheye.com"
     "mt.fresheye.com"
     "/ft_cjresult.cgi HTTP/1.1"
     "gen_text=%s&charset=big5&cjjc=%o%t"
     utf-8
     "<TEXTAREA class=\"out-form\" name=\"gen_text2\" cols=\"25\" rows=\"15\">\\([^<]*\\)</TEXTAREA>"
     (("c" . "j") ("j" . "c"))
     (("c" . "tw") ("j" . "ja")))

    ;; Excite.co.jp
    ("excite.co.jp"
     "www.excite.co.jp"
     "/world/english/ HTTP/1.1"
     "wb_lp=%o%t&before=%s"
     utf-8
     "<textarea name=\"after\" id=\"after\">\\([^<]*\\)</textarea>"
     (("EN" . "JA") ("JA" . "EN"))
     (("EN" . "en") ("JA" . "ja")))
    ("excite.co.jp"
     "www.excite.co.jp"
     "/world/chinese/ HTTP/1.1"
     "wb_lp=%o%t&before=%s"
     utf-8
     "<textarea name=\"after\" id=\"after\">\\([^<]*\\)</textarea>"
     (("JA" . "CH") ("CH" . "JA"))
     (("JA" . "ja") ("CH" . "ch")))
    ("excite.co.jp"
     "www.excite.co.jp"
     "/world/chinese/ HTTP/1.1"
     "wb_lp=%o%t&big5=yes&before=%s"
     utf-8
     "<textarea name=\"after\" id=\"after\">\\([^<]*\\)</textarea>"
     (("JA" . "CH") ("CH" . "JA"))
     (("JA" . "ja") ("CH" . "tw")))
    ("excite.co.jp"
     "www.excite.co.jp"
     "/world/korean/ HTTP/1.1"
     "wb_lp=%o%t&before=%s"
     utf-8
     "<textarea name=\"after\" id=\"after\">\\([^<]*\\)</textarea>"
     (("JA" . "KO") ("KO" . "JA"))
     (("JA" . "ja") ("KO" . "ko")))
    ("excite.co.jp"
     "www.excite.co.jp"
     "/world/french/ HTTP/1.1"
     "wb_lp=%o%t&before=%s"
     utf-8
     "<textarea name=\"after\" id=\"after\">\\([^<]*\\)</textarea>"
     (("JA" . "FR") ("FR" . "JA")
      ("EN" . "FR") ("FR" . "EN"))
     (("JA" . "ja") ("FR" . "fr")
      ("EN" . "en")))
    ("excite.co.jp"
     "www.excite.co.jp"
     "/world/german/ HTTP/1.1"
     "wb_lp=%o%t&before=%s"
     utf-8
     "<textarea name=\"after\" id=\"after\">\\([^<]*\\)</textarea>"
     (("JA" . "DE") ("DE" . "JA")
      ("EN" . "DE") ("DE" . "EN"))
     (("JA" . "ja") ("DE" . "de")
      ("EN" . "en")))
    ("excite.co.jp"
     "www.excite.co.jp"
     "/world/italian/ HTTP/1.1"
     "wb_lp=%o%t&before=%s"
     utf-8
     "<textarea name=\"after\" id=\"after\">\\([^<]*\\)</textarea>"
     (("JA" . "IT") ("IT" . "JA")
      ("EN" . "IT") ("IT" . "EN"))
     (("JA" . "ja") ("IT" . "it")
      ("EN" . "en")))
    ("excite.co.jp"
     "www.excite.co.jp"
     "/world/spanish/ HTTP/1.1"
     "wb_lp=%o%t&before=%s"
     utf-8
     "<textarea name=\"after\" id=\"after\">\\([^<]*\\)</textarea>"
     (("JA" . "ES") ("ES" . "JA")
      ("EN" . "ES") ("ES" . "EN"))
     (("JA" . "ja") ("ES" . "es")
      ("EN" . "en")))
    ("excite.co.jp"
     "www.excite.co.jp"
     "/world/portuguese/ HTTP/1.1"
     "wb_lp=%o%t&before=%s"
     utf-8
     "<textarea name=\"after\" id=\"after\">\\([^<]*\\)</textarea>"
     (("JA" . "PT") ("PT" . "JA")
      ("EN" . "PT") ("PT" . "EN"))
     (("JA" . "ja") ("PT" . "pt")
      ("EN" . "en")))

    ;; Yahoo.co.jp
    ("yahoo.co.jp"
     "honyaku.yahoo.co.jp"
     "/transtext HTTP/1.1"
     "both=TH&text=%s&clearFlg=1&eid=CR-%o%t"
     utf-8
     "<textarea rows=12 cols=30 name=\"trn_text\" id=\"trn_textText\" class=\"smaller\"[ 	]*[^>]*>\\([^<]*\\)</textarea>"
     (("E" . "J") ("J" . "E")
      ("C" . "J") ("J" . "C-CN")
      ("K" . "J") ("J" . "K"))
     (("E" . "en") ("J" . "ja")
      ("C" . "ch") ("C-CN" . "ch")
      ("K" . "ko")))

    ;; Ocn.ne.jp
    ("ocn.ne.jp"
     "cgi01.ocn.ne.jp"
     "/cgi-bin/translation/index.cgi HTTP/1.1"
     "langpair=%o%t&sourceText=%s"
     utf-8
     "<TEXTAREA NAME=\"responseText\" ROWS=\"5\" COLS=\"41\" WRAP=\"virtual\" CLASS=\"in2\">\\([^<]*\\)</TEXTAREA>"
     (("en" . "ja") ("ja" . "en")
      ("ja" . "ko") ("ko" . "ja")
      ("ja" . "zh") ("zh" . "ja"))
     (("zh" . "ch"))))
  "The alist where setting of the site which is used for text translation is
described. To update site-data, evalute `text-translator-site-data-init`."
  :type '(repeat
          (list :tag "Web Site"
                (string :tag "Web site name and translation type")
                (string :tag "Host name")
                (string :tag "POST path and HTTP version")
                (string :tag "POST contents")
                (symbol :tag "Character code")
                (choice (string :tag "regexp") (symbol :tag "function"))
                (list :tag (concat "The correspondence of translation-able "
                                   "name (used by translation sites)"))
                (list :tag (concat "The correspondence of name that used by "
                                   "text-translator and name that used by "
                                   "translation sites"))))
  :group 'text-translator)

(defcustom text-translator-site-data-minimum-alist
  '(;; lou5.jp (Japanese, Lou)
    ("lou5.jp_*normal"
     "lou5.jp"
     "/ HTTP/1.1"
     "v=1&text=%s"
     utf-8
     (lambda ()
       (text-translator-extract-tag-exclusion-string
        "<p class=\"large align-left box\">\\(\\(.\\|\n\\)*?\\)</p>"
        t)))

    ;; tatoeba.org (Furigana, romaji)
    ("tatoeba.org_furigana"
     "tatoeba.org"
     "/eng/tools/romaji_furigana?query=%s&type=furigana HTTP/1.1"
     nil
     utf-8
     "class=\"furigana\">\\(\\(.\\|\n\\)*?\\)</div><form id=\"ToolRomajiFuriganaForm")
    ("tatoeba.org_romaji"
     "tatoeba.org"
     "/eng/tools/romaji_furigana?query=%s&type=romaji HTTP/1.1"
     nil
     utf-8
     "class=\"furigana\">\\(\\(.\\|\n\\)*?\\)</div><form id=\"ToolRomajiFuriganaForm")

    ;; traduku.net (Esperanto, English)
    ("traduku.net_eoen"
     "traduku.net"
     "/cgi-bin/traduku HTTP/1.1"
     "eo_en&t=%s"
     utf-8
     " id=\"rezulto\">\\(\\(.\\|\n\\)*?\\)</div>")
    ("traduku.net_eneo"
     "traduku.net"
     "/cgi-bin/traduku HTTP/1.1"
     "en_eo_trukilo&t=%s"
     utf-8
     " id=\"rezulto\">\\(\\(.\\|\n\\)*?\\)</div>"))

  "*The alist where setting of the site which is used for text translation is
described. To update site-data, evalute `text-translator-site-data-init`."
  :type  '(repeat
           (list :tag "Web Site"
                 (string :tag "Web site name and translation type")
                 (string :tag "Host name")
                 (string :tag "POST path and HTTP version")
                 (string :tag "POST contents")
                 (symbol :tag "Character code")
                 (choice (string :tag "regexp") (symbol :tag "function"))))
  :group 'text-translator)

;; text-translator-site-data-template-alist
;; + text-translator-site-data-minimum-alist
;; = text-translator-site-data-alist
(defvar text-translator-site-data-alist nil
  "The alist where setting of the site which is used for text translation is
described.")



(provide 'text-translator-sites)
;;; text-translator-sites.el ends here
