;;	Jeff Mickey's .emacs.d/init.el file
;;
;;	the awkward part is that I wrote this in vim :/
;;      UPDATE: this was written in emacs. BOOTSTRAPTIME

;; time our .emacs loading
(defvar *emacs-load-start* (current-time))

(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))

;; Use package management!
(require 'package)

(setq package-archives
      (append '(("org"       . "http://orgmode.org/elpa/")
                ("melpa"     . "http://melpa.milkbox.net/packages/")
                ("marmalade" . "http://marmalade-repo.org/packages/"))
              package-archives))

(package-initialize)

; load up the main file
(require 'ob)
(require 'org)
(org-babel-load-file (expand-file-name "boot.org" dotfiles-dir))

;;; init.el ends here


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-include-dirs
   (quote
    ("." "/Users/jmickey/code/Agda-2.3.2.1/std_lib/lib-0.7/src")))
 '(ansi-color-names-vector
   ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(ansi-term-color-vector
   ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"] t)
 '(auto-image-file-mode t)
 '(browse-url-firefox-new-window-is-tab t)
 '(browse-url-firefox-program "firefox")
 '(comint-buffer-maximum-size 20000)
 '(comint-completion-addsuffix t)
 '(comint-get-old-input (lambda nil "") t)
 '(comint-input-ignoredups t)
 '(comint-input-ring-size 5000)
 '(comint-move-point-for-output nil)
 '(comint-prompt-read-only t)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" "4e72cb2841e4801ba202a120c1cffdf88f5512536e557d03b3626d890b52f201" "36a309985a0f9ed1a0c3a69625802f87dee940767c9e200b89cdebdb737e5b29" "bf7ed640479049f1d74319ed004a9821072c1d9331bc1147e01d22748c18ebdf" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(elfeed-feeds
   (quote
    ("http://www.bbc.co.uk/blogs/adamcurtis/atom" "http://boxerhockey.fireball20xl.com/inc/feed.php" "http://dictionary.reference.com/wordoftheday/wotd.rss" "http://www.cleeseblog.com/feed/?COLLCC=1958837040&COLLCC=2147165045&COLLCC=156356006&COLLCC=546151683&COLLCC=4041034544&" "https://lalingvisto.wordpress.com/feed/" "https://www.archlinux.org/feeds/news/" "http://iconicphotos.wordpress.com/feed/" "http://iwdrm.tumblr.com/rss" "http://feeds.mobileread.com/mr/ebooks" "http://feeds.feedburner.com/BuddhistGeeksMagazine" "http://www.dharmaseed.org/feeds/recordings/" "http://hardcorezen.blogspot.com/feeds/posts/default" "http://shoresofzen.com/nozeninthewest/feed/" "http://www.oxherding.com/my_weblog/atom.xml" "http://feeds.feedburner.com/SweepTheDustPushTheDirt" "http://feeds.feedburner.com/ProgressiveBuddhism" "http://theworsthorse.com/feed/" "http://feeds.feedburner.com/WildFoxZen" "http://withoutbounds.wordpress.com/feed/" "http://blogs.transparent.com/esperanto/feed/" "http://feeds2.feedburner.com/EsperantoBlog" "http://esperanto-usa.org/en/node/feed" "http://radioverda.com/programoj/atom.xml" "http://esperanto-usa.org/en/blog/66/feed" "http://cadryskitchen.com/feed/" "http://feeds.feedburner.com/blogspot/hvGu" "http://bapstory.blogspot.com/feeds/posts/default" "http://eatnvegn.blogspot.com/feeds/posts/default" "http://everydaydishtv.blogspot.com/feeds/posts/default" "http://frugalcuisine.blogspot.com/feeds/posts/default" "http://fuckyeahcilantro.tumblr.com/rss" "http://happyveganface.blogspot.com/feeds/posts/default" "http://howtofeedavegan.blogspot.com/feeds/posts/default" "http://judyspages.blogspot.com/feeds/posts/default" "http://feeds.feedburner.com/NotesFromTheVeganFeastKitchen/21stCenturyTable" "http://renegadehealth.com/blog/feed" "http://theveganstoner.blogspot.com/feeds/posts/default" "http://feeds.feedburner.com/vegandad" "http://vforvegan-jillianrenee.blogspot.com/feeds/posts/default" "http://feeds.feedburner.com/vegan-recipes" "http://vegancrunk.blogspot.com/feeds/posts/default" "http://feeds.feedburner.com/ambikaskitchen/DMlT" "http://starbucks4life.livejournal.com/data/rss" "http://machaon2.livejournal.com/data/rss" "http://dbsmilr.blogspot.com/atom.xml" "http://callmecrane.livejournal.com/data/rss" "http://daisuke-takeru.livejournal.com/data/rss" "http://www.xanga.com/rss.aspx?user=andel_marie" "http://www.xanga.com/rss.aspx?user=annakron" "http://www.xanga.com/rssfeed.aspx?u=aviachaya" "http://www.xanga.com/rss.aspx?user=aznbeats" "http://blog.emerick.org/feed/atom/" "http://www.xanga.com/rss.aspx?user=bmay18" "http://www.xanga.com/rss.aspx?user=broadwaybabe05" "http://alkalinegal3.livejournal.com/data/rss" "http://codemac.net/index.atom" "http://www.vox.com/" "http://www.xanga.com/rss.aspx?user=counteract" "http://www.blogs.com/featured-stories.xml" "http://www.xanga.com/rss.aspx?user=dutchie8285" "http://em218aus.livejournal.com/data/rss" "http://epistemiac.blogspot.com/feeds/posts/default" "http://www.xanga.com/rss.aspx?user=fakejojo" "http://www.xanga.com/rss.aspx?user=fuzzychi" "http://rhhokie.livejournal.com/data/rss" "http://moonietron.livejournal.com/data/rss" "http://www.xanga.com/rss.aspx?user=idaho1021" "http://jazfusion.livejournal.com/data/rss" "http://jerfsan.livejournal.com/data/rss" "http://sugarshit-spark.livejournal.com/data/rss" "http://blog.kevincupp.com/index.xml" "http://khelldar.livejournal.com/data/rss" "http://www.xanga.com/rss.aspx?user=kittymaroon" "http://tan512.livejournal.com/data/rss" "http://keeblerelf11.livejournal.com/data/rss" "http://linkita.tumblr.com/rss" "http://www.sigcis.org/blog/100/feed" "http://cinta-del-condu.livejournal.com/data/rss" "http://mouse-aes.livejournal.com/data/rss" "http://musingsfromarootlessbloom.blogspot.com/feeds/posts/default" "http://claireonabike.blogspot.com/feeds/posts/default" "http://tinkerbell1168.livejournal.com/data/rss" "http://kmflute.blogspot.com/feeds/posts/default" "http://www.xanga.com/rss.aspx?user=perpetualbeauty" "http://oh-my-gawd.livejournal.com/data/rss" "http://smallsam.livejournal.com/data/rss" "http://blacknblue08.livejournal.com/data/rss" "http://steveinsenegal.blogspot.com/feeds/posts/default" "http://closing.vox.com/" "http://annehamby.blogspot.com/feeds/posts/default" "http://scarredsoul.livejournal.com/data/atom" "http://la-venus-dille.livejournal.com/data/rss" "http://www.xanga.com/rss.aspx?user=thegogglesofedward" "http://alltom.com/feed.atom" "http://feeds.feedburner.com/tmarkiewicz" "http://www.xanga.com/rss.aspx?user=trainermagz" "http://greeneyedsarah.livejournal.com/data/rss/" "http://www.xanga.com/rss.aspx?user=tygrrlilie" "http://www.xanga.com/rss.aspx?user=veggiegurl22" "http://www.xanga.com/rss.aspx?user=waitn4bux" "http://powderbluelove.livejournal.com/data/rss" "http://www.xanga.com/rss.aspx?user=whiteboatman" "http://www.xanga.com/rss.aspx?user=ximfinity" "http://feeds.feedburner.com/jamesaltucher" "http://gtd.marvelz.com/blog/feed/" "http://feeds.feedburner.com/To-done" "http://feeds.feedburner.com/inoveryourheadblo" "http://www.lifereboot.com/feed/" "http://feeds.feedburner.com/BrazenCareerist" "http://feeds.feedburner.com/PsychologyBlog" "http://kalnel.blogspot.com/feeds/posts/default" "http://feeds.feedburner.com/unclutterer" "http://feeds.feedburner.com/WhatsTheNextAction" "http://zenhabits.net/feed/" "http://feeds.feedburner.com/HackingNetflix" "http://livingromcom.typepad.com/my_weblog/atom.xml" "http://savanttools.com/trailers.asp" "http://torrentfreak.com/category/dvdrip/feed/" "http://feeds.feedburner.com/bunniesoflasvegas/xptf" "http://www.discoworkout.com/?feed=rss2" "http://feeds.feedburner.com/3hive/BFhw" "http://feeds.feedburner.com/Asrestlessasweare" "http://www.arawa.fm/feed/" "http://feeds.feedburner.com/Aurgasm" "http://feeds.feedburner.com/bigstereo" "http://feeds.feedburner.com/DiscoDelicious" "http://feeds.feedburner.com/discodust" "http://masshyperbole.blogspot.com/feeds/posts/default" "http://blog.iso50.com/feed/" "http://killtherhythmblog.blogspot.com/feeds/posts/default" "http://www.blogotheque.net/feed/" "http://lettherebenight.blogspot.com/feeds/posts/default" "http://www.lowendtheoryclub.com/podcast.xml" "http://neongoldrecords.blogspot.com/feeds/posts/default" "http://palmsout.net/feed/" "http://feeds2.feedburner.com/nmecom/rss/newsxml" "http://feeds2.feedburner.com/PitchforkLatestNews" "https://marteydodoo.com/feed/" "http://feeds.feedburner.com/roryphillips" "http://indyweekblogs.com/scan/feed/" "http://www.scissorkick.com/feed/" "http://feeds.feedburner.com/Sophistefunk" "http://feeds.feedburner.com/StereogumMP3Feed" "http://feeds.feedburner.com/swedelife" "http://themixtape.co.uk/rss" "http://feeds2.feedburner.com/TheMusicNinja" "http://www.tiltmag.com/feeds/posts/default?alt=rss" "http://feeds.feedburner.com/TriangleMusic" "http://valeriecherie.blogspot.com/feeds/posts/default" "http://feeds.feedburner.com/blogspot/vzZM" "http://www.wavesatnight.com/feed/" "http://feeds.xlr8r.com/xlr8rmp3" "http://feeds.feedburner.com/nodatatv?format=xml" "http://www.aljazeera.com/Services/Rss/?PostingId=2007731105943979989" "http://feeds.bbci.co.uk/news/rss.xml?edition=int" "http://www.themorningnews.org/index.xml" "http://aintstudyingyou.blogspot.com/feeds/posts/default" "https://git.trikeapps.com/.rss" "http://rss.groups.yahoo.com/group/rationalargumentator/rss" "http://blog.stackoverflow.com/feed/" "http://feeds.feedburner.com/codinghorror/" "https://existentialtype.wordpress.com/feed/" "http://ghcmutterings.wordpress.com/feed/" "http://gitfu.wordpress.com/feed/" "http://cgi.cse.unsw.edu.au/~dons/blog/index.rss" "http://feeds.raganwald.com/raganwald" "http://lambda-the-ultimate.org/rss.xml" "http://feeds.feedburner.com/SoftwareByRob" "http://fexpr.blogspot.com/feeds/posts/default" "http://www.serpentine.com/blog/feed/" "http://axisofeval.blogspot.com/feeds/posts/default?alt=rss" "http://feeds.feedburner.com/goodnightraleigh" "http://feeds2.feedburner.com/raleigh" "http://vegansaurus.com/rss" "http://fulltextrssfeed.com/www.baseballprospectus.com/rss/feed.xml" "http://feeds.feedburner.com/sportsblogs/letsgotribe.xml" "http://fulltextrssfeed.com/www.hardballtimes.com/main/content/rss_2.0" "http://feed.torrentfreak.com/Torrentfreak/" "http://www.democracynow.org/podcast-video.xml")))
 '(fci-rule-color "#383838")
 '(frame-background-mode (quote dark))
 '(haskell-hoogle-command "hoogle")
 '(indent-tabs-mode nil)
 '(org-agenda-files
   (quote
    ("/home/codemac/org/gtd.org" "/home/codemac/org/igneous.org" "/home/codemac/org/_notes/2012.org" "/home/codemac/org/_notes/2013.org" "/home/codemac/org/_notes/2014.org" "/home/codemac/org/_notes/advanced_early_riser.org" "/home/codemac/org/_notes/class2012pgm.org" "/home/codemac/org/_notes/encryption.org" "/home/codemac/org/_notes/fitness.org" "/home/codemac/org/_notes/gifts.org" "/home/codemac/org/_notes/linux_plumbers2013.org" "/home/codemac/org/_notes/nanowrimo2011.org" "/home/codemac/org/_notes/netlink.org" "/home/codemac/org/_notes/notes.org" "/home/codemac/org/_notes/oppression-of-tech.org" "/home/codemac/org/_notes/recipes.org" "/home/codemac/org/_notes/steal.org" "/home/codemac/org/_notes/ubuntu-bootable.org" "/home/codemac/org/_notes/webmac.org" "/home/codemac/org/_notes/whoami.org" "~/work/docs/datapath/TODO-jm.org")))
 '(protect-buffer-bury-p nil)
 '(safe-local-variable-values
   (quote
    ((eval cm/projectile-dirlocals-hook
           (quote mesa))
     (cm/gitty-files . t)
     (eval cm/igneous-product-config)
     (yaml-indent-offset . 8)
     (after-save-hook archive-done-tasks))))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(warning-suppress-types (quote ((undo discard-info))))
 '(yaml-indent-offset 8))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(message "My .emacs loaded in %ds"
         (destructuring-bind (hi lo ms ps) (current-time)
           (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))
