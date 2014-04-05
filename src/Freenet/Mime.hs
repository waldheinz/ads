
{-# LANGUAGE OverloadedStrings #-}

module Freenet.Mime (
  Mime, DefaultMimes, defaultMimes, lookupMime
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Word

type Mime = T.Text

newtype DefaultMimes = DefaultMimes { unDefMime :: Map.Map Word16 Mime }

defaultMimes :: DefaultMimes
defaultMimes = DefaultMimes $ Map.fromList $
    map (\l -> let (mid: mt: _) = T.split (==',') l in (read $ T.unpack mid, mt)) $ T.lines mimeText

lookupMime :: DefaultMimes -> Word16 -> Mime
lookupMime dm mid = case Map.lookup mid (unDefMime dm) of
  Nothing -> "application/octet-stream"
  Just m  -> m

mimeText :: T.Text
mimeText = "0,application/activemessage\n\
\1,application/andrew-inset,ez\n\
\2,application/applefile\n\
\3,application/atomicmail\n\
\4,application/batch-SMTP\n\
\5,application/beep+xml\n\
\6,application/cals-1840\n\
\7,application/commonground\n\
\8,application/cu-seeme,csm cu\n\
\9,application/cybercash\n\
\10,application/dca-rft\n\
\11,application/dec-dx\n\
\12,application/docbook+xml\n\
\13,application/dsptype,tsp\n\
\14,application/dvcs\n\
\15,application/edi-consent\n\
\16,application/edifact\n\
\17,application/edi-x12\n\
\18,application/eshop\n\
\19,application/font-tdpfr\n\
\20,application/futuresplash,spl\n\
\21,application/ghostview\n\
\22,application/hta,hta\n\
\23,application/http\n\
\24,application/hyperstudio\n\
\25,application/iges\n\
\26,application/index\n\
\27,application/index.cmd\n\
\28,application/index.obj\n\
\29,application/index.response\n\
\30,application/index.vnd\n\
\31,application/iotp\n\
\32,application/ipp\n\
\33,application/isup\n\
\34,application/mac-compactpro,cpt\n\
\35,application/marc\n\
\36,application/mac-binhex40,hqx\n\
\37,application/macwriteii\n\
\38,application/mathematica,nb\n\
\39,application/mathematica-old\n\
\40,application/msaccess,mdb\n\
\41,application/msword,doc dot,doc\n\
\42,application/news-message-id\n\
\43,application/news-transmission\n\
\44,application/octet-stream,bin\n\
\45,application/ocsp-request\n\
\46,application/ocsp-response\n\
\47,application/oda,oda\n\
\48,application/ogg,ogg\n\
\49,application/parityfec\n\
\50,application/pics-rules,prf\n\
\51,application/pgp-encrypted\n\
\52,application/pgp-keys,key\n\
\53,application/pdf,pdf\n\
\54,application/pgp-signature,pgp\n\
\55,application/pkcs10\n\
\56,application/pkcs7-mime\n\
\57,application/pkcs7-signature\n\
\58,application/pkix-cert\n\
\59,application/pkixcmp\n\
\60,application/pkix-crl\n\
\61,application/postscript,ps ai eps,ps\n\
\62,application/prs.alvestrand.titrax-sheet\n\
\63,application/prs.cww\n\
\64,application/prs.nprend\n\
\65,application/qsig\n\
\66,application/rar,rar\n\
\67,application/rdf+xml,rdf\n\
\68,application/remote-printing\n\
\69,application/riscos\n\
\70,application/rss+xml,rss\n\
\71,application/rtf,rtf\n\
\72,application/sdp\n\
\73,application/set-payment\n\
\74,application/set-payment-initiation\n\
\75,application/set-registration\n\
\76,application/set-registration-initiation\n\
\77,application/sgml\n\
\78,application/sgml-open-catalog\n\
\79,application/sieve\n\
\80,application/slate\n\
\81,application/smil,smi smil,smil\n\
\82,application/timestamp-query\n\
\83,application/timestamp-reply\n\
\84,application/vemmi\n\
\85,application/whoispp-query\n\
\86,application/whoispp-response\n\
\87,application/wita\n\
\88,application/wordperfect5.1,wp5\n\
\89,application/x400-bp\n\
\90,application/xhtml+xml,xht xhtml,xhtml\n\
\91,application/xml,xml xsl,xml\n\
\92,application/xml-dtd\n\
\93,application/xml-external-parsed-entity\n\
\94,application/zip,zip\n\
\95,application/vnd.3M.Post-it-Notes\n\
\96,application/vnd.accpac.simply.aso\n\
\97,application/vnd.accpac.simply.imp\n\
\98,application/vnd.acucobol\n\
\99,application/vnd.aether.imp\n\
\100,application/vnd.anser-web-certificate-issue-initiation\n\
\101,application/vnd.anser-web-funds-transfer-initiation\n\
\102,application/vnd.audiograph\n\
\103,application/vnd.bmi\n\
\104,application/vnd.businessobjects\n\
\105,application/vnd.canon-cpdl\n\
\106,application/vnd.canon-lips\n\
\107,application/vnd.cinderella,cdy\n\
\108,application/vnd.claymore\n\
\109,application/vnd.commerce-battelle\n\
\110,application/vnd.commonspace\n\
\111,application/vnd.comsocaller\n\
\112,application/vnd.contact.cmsg\n\
\113,application/vnd.cosmocaller\n\
\114,application/vnd.ctc-posml\n\
\115,application/vnd.cups-postscript\n\
\116,application/vnd.cups-raster\n\
\117,application/vnd.cups-raw\n\
\118,application/vnd.cybank\n\
\119,application/vnd.dna\n\
\120,application/vnd.dpgraph\n\
\121,application/vnd.dxr\n\
\122,application/vnd.ecdis-update\n\
\123,application/vnd.ecowin.chart\n\
\124,application/vnd.ecowin.filerequest\n\
\125,application/vnd.ecowin.fileupdate\n\
\126,application/vnd.ecowin.series\n\
\127,application/vnd.ecowin.seriesrequest\n\
\128,application/vnd.ecowin.seriesupdate\n\
\129,application/vnd.enliven\n\
\130,application/vnd.epson.esf\n\
\131,application/vnd.epson.msf\n\
\132,application/vnd.epson.quickanime\n\
\133,application/vnd.epson.salt\n\
\134,application/vnd.epson.ssf\n\
\135,application/vnd.ericsson.quickcall\n\
\136,application/vnd.eudora.data\n\
\137,application/vnd.fdf\n\
\138,application/vnd.ffsns\n\
\139,application/vnd.flographit\n\
\140,application/vnd.framemaker\n\
\141,application/vnd.fsc.weblaunch\n\
\142,application/vnd.fujitsu.oasys\n\
\143,application/vnd.fujitsu.oasys2\n\
\144,application/vnd.fujitsu.oasys3\n\
\145,application/vnd.fujitsu.oasysgp\n\
\146,application/vnd.fujitsu.oasysprs\n\
\147,application/vnd.fujixerox.ddd\n\
\148,application/vnd.fujixerox.docuworks\n\
\149,application/vnd.fujixerox.docuworks.binder\n\
\150,application/vnd.fut-misnet\n\
\151,application/vnd.grafeq\n\
\152,application/vnd.groove-account\n\
\153,application/vnd.groove-identity-message\n\
\154,application/vnd.groove-injector\n\
\155,application/vnd.groove-tool-message\n\
\156,application/vnd.groove-tool-template\n\
\157,application/vnd.groove-vcard\n\
\158,application/vnd.hhe.lesson-player\n\
\159,application/vnd.hp-HPGL\n\
\160,application/vnd.hp-PCL\n\
\161,application/vnd.hp-PCLXL\n\
\162,application/vnd.hp-hpid\n\
\163,application/vnd.hp-hps\n\
\164,application/vnd.httphone\n\
\165,application/vnd.hzn-3d-crossword\n\
\166,application/vnd.ibm.MiniPay\n\
\167,application/vnd.ibm.afplinedata\n\
\168,application/vnd.ibm.modcap\n\
\169,application/vnd.informix-visionary\n\
\170,application/vnd.intercon.formnet\n\
\171,application/vnd.intertrust.digibox\n\
\172,application/vnd.intertrust.nncp\n\
\173,application/vnd.intu.qbo\n\
\174,application/vnd.intu.qfx\n\
\175,application/vnd.irepository.package+xml\n\
\176,application/vnd.is-xpr\n\
\177,application/vnd.japannet-directory-service\n\
\178,application/vnd.japannet-jpnstore-wakeup\n\
\179,application/vnd.japannet-payment-wakeup\n\
\180,application/vnd.japannet-registration\n\
\181,application/vnd.japannet-registration-wakeup\n\
\182,application/vnd.japannet-setstore-wakeup\n\
\183,application/vnd.japannet-verification\n\
\184,application/vnd.japannet-verification-wakeup\n\
\185,application/vnd.koan\n\
\186,application/vnd.lotus-1-2-3\n\
\187,application/vnd.lotus-approach\n\
\188,application/vnd.lotus-freelance\n\
\189,application/vnd.lotus-notes\n\
\190,application/vnd.lotus-organizer\n\
\191,application/vnd.lotus-screencam\n\
\192,application/vnd.lotus-wordpro\n\
\193,application/vnd.mcd\n\
\194,application/vnd.mediastation.cdkey\n\
\195,application/vnd.meridian-slingshot\n\
\196,application/vnd.mif,mif\n\
\197,application/vnd.minisoft-hp3000-save\n\
\198,application/vnd.mitsubishi.misty-guard.trustweb\n\
\199,application/vnd.mobius.daf\n\
\200,application/vnd.mobius.dis\n\
\201,application/vnd.mobius.msl\n\
\202,application/vnd.mobius.plc\n\
\203,application/vnd.mobius.txf\n\
\204,application/vnd.motorola.flexsuite\n\
\205,application/vnd.motorola.flexsuite.adsi\n\
\206,application/vnd.motorola.flexsuite.fis\n\
\207,application/vnd.motorola.flexsuite.gotap\n\
\208,application/vnd.motorola.flexsuite.kmr\n\
\209,application/vnd.motorola.flexsuite.ttc\n\
\210,application/vnd.motorola.flexsuite.wem\n\
\211,application/vnd.mozilla.xul+xml,xul\n\
\212,application/vnd.ms-artgalry\n\
\213,application/vnd.ms-asf\n\
\214,application/vnd.ms-excel,xls xlb xlt,xls\n\
\215,application/vnd.ms-lrm\n\
\216,application/vnd.ms-pki.seccat,cat\n\
\217,application/vnd.ms-pki.stl,stl\n\
\218,application/vnd.ms-powerpoint,ppt pps,pps\n\
\219,application/vnd.ms-project\n\
\220,application/vnd.ms-tnef\n\
\221,application/vnd.ms-works\n\
\222,application/vnd.mseq\n\
\223,application/vnd.msign\n\
\224,application/vnd.music-niff\n\
\225,application/vnd.musician\n\
\226,application/vnd.netfpx\n\
\227,application/vnd.noblenet-directory\n\
\228,application/vnd.noblenet-sealer\n\
\229,application/vnd.noblenet-web\n\
\230,application/vnd.novadigm.EDM\n\
\231,application/vnd.novadigm.EDX\n\
\232,application/vnd.novadigm.EXT\n\
\233,application/vnd.osa.netdeploy\n\
\234,application/vnd.palm\n\
\235,application/vnd.pg.format\n\
\236,application/vnd.pg.osasli\n\
\237,application/vnd.powerbuilder6\n\
\238,application/vnd.powerbuilder6-s\n\
\239,application/vnd.powerbuilder7\n\
\240,application/vnd.powerbuilder7-s\n\
\241,application/vnd.powerbuilder75\n\
\242,application/vnd.powerbuilder75-s\n\
\243,application/vnd.previewsystems.box\n\
\244,application/vnd.publishare-delta-tree\n\
\245,application/vnd.pvi.ptid1\n\
\246,application/vnd.pwg-xhtml-print+xml\n\
\247,application/vnd.rapid\n\
\248,application/vnd.s3sms\n\
\249,application/vnd.seemail\n\
\250,application/vnd.shana.informed.formdata\n\
\251,application/vnd.shana.informed.formtemplate\n\
\252,application/vnd.shana.informed.interchange\n\
\253,application/vnd.shana.informed.package\n\
\254,application/vnd.smaf,mmf\n\
\255,application/vnd.sss-cod\n\
\256,application/vnd.sss-dtf\n\
\257,application/vnd.sss-ntf\n\
\258,application/vnd.stardivision.calc,sdc\n\
\259,application/vnd.stardivision.draw,sda\n\
\260,application/vnd.stardivision.impress,sdd sdp\n\
\261,application/vnd.stardivision.math,smf\n\
\262,application/vnd.stardivision.writer,sdw vor\n\
\263,application/vnd.stardivision.writer-global,sgl\n\
\264,application/vnd.street-stream\n\
\265,application/vnd.sun.xml.calc,sxc\n\
\266,application/vnd.sun.xml.calc.template,stc\n\
\267,application/vnd.sun.xml.draw,sxd\n\
\268,application/vnd.sun.xml.draw.template,std\n\
\269,application/vnd.sun.xml.impress,sxi\n\
\270,application/vnd.sun.xml.impress.template,sti\n\
\271,application/vnd.sun.xml.math,sxm\n\
\272,application/vnd.sun.xml.writer,sxw\n\
\273,application/vnd.sun.xml.writer.global,sxg\n\
\274,application/vnd.sun.xml.writer.template,stw\n\
\275,application/vnd.svd\n\
\276,application/vnd.swiftview-ics\n\
\277,application/vnd.symbian.install,sis\n\
\278,application/vnd.triscape.mxs\n\
\279,application/vnd.trueapp\n\
\280,application/vnd.truedoc\n\
\281,application/vnd.tve-trigger\n\
\282,application/vnd.ufdl\n\
\283,application/vnd.uplanet.alert\n\
\284,application/vnd.uplanet.alert-wbxml\n\
\285,application/vnd.uplanet.bearer-choice\n\
\286,application/vnd.uplanet.bearer-choice-wbxml\n\
\287,application/vnd.uplanet.cacheop\n\
\288,application/vnd.uplanet.cacheop-wbxml\n\
\289,application/vnd.uplanet.channel\n\
\290,application/vnd.uplanet.channel-wbxml\n\
\291,application/vnd.uplanet.list\n\
\292,application/vnd.uplanet.list-wbxml\n\
\293,application/vnd.uplanet.listcmd\n\
\294,application/vnd.uplanet.listcmd-wbxml\n\
\295,application/vnd.uplanet.signal\n\
\296,application/vnd.vcx\n\
\297,application/vnd.vectorworks\n\
\298,application/vnd.vidsoft.vidconference\n\
\299,application/vnd.visio,vsd\n\
\300,application/vnd.vividence.scriptfile\n\
\301,application/vnd.wap.sic\n\
\302,application/vnd.wap.slc\n\
\303,application/vnd.wap.wbxml,wbxml\n\
\304,application/vnd.wap.wmlc,wmlc\n\
\305,application/vnd.wap.wmlscriptc,wmlsc\n\
\306,application/vnd.webturbo\n\
\307,application/vnd.wrq-hp3000-labelled\n\
\308,application/vnd.wt.stf\n\
\309,application/vnd.xara\n\
\310,application/vnd.xfdl\n\
\311,application/vnd.yellowriver-custom-menu\n\
\312,application/x-123,wk\n\
\313,application/x-apple-diskimage,dmg\n\
\314,application/x-bcpio,bcpio\n\
\315,application/x-bittorrent,torrent\n\
\316,application/x-cdf,cdf\n\
\317,application/x-cdlink,vcd\n\
\318,application/x-chess-pgn,pgn\n\
\319,application/x-chm,chm\n\
\320,application/x-core\n\
\321,application/x-cpio,cpio\n\
\322,application/x-csh,csh\n\
\323,application/x-debian-package,deb\n\
\324,application/x-director,dcr dir dxr\n\
\325,application/x-doom,wad\n\
\326,application/x-dms,dms\n\
\327,application/x-dvi,dvi\n\
\328,application/x-executable\n\
\329,application/x-flac,flac\n\
\330,application/x-font,pfa pfb gsf pcf pcf.Z,unknown-font-type\n\
\331,application/x-futuresplash,spl\n\
\332,application/x-gnumeric,gnumeric\n\
\333,application/x-go-sgf,sgf\n\
\334,application/x-graphing-calculator,gcf\n\
\335,application/x-gtar,gtar tgz taz,tgz\n\
\336,application/x-hdf,hdf\n\
\337,application/x-httpd-php,phtml pht php,php\n\
\338,application/x-httpd-php-source,phps\n\
\339,application/x-httpd-php3,php3\n\
\340,application/x-httpd-php3-preprocessed,php3p\n\
\341,application/x-httpd-php4,php4\n\
\342,application/x-ica,ica\n\
\343,application/x-internet-signup,ins isp\n\
\344,application/x-iphone,iii\n\
\345,application/x-java-applet\n\
\346,application/x-java-archive,jar\n\
\347,application/x-java-bean\n\
\348,application/x-java-jnlp-file,jnlp\n\
\349,application/x-java-serialized-object,ser\n\
\350,application/x-java-vm,class\n\
\351,application/x-javascript,js\n\
\352,application/x-kdelnk\n\
\353,application/x-kchart,chrt\n\
\354,application/x-killustrator,kil\n\
\355,application/x-kpresenter,kpr kpt\n\
\356,application/x-koan,skp skd skt skm\n\
\357,application/x-kspread,ksp\n\
\358,application/x-kword,kwd kwt,kwd\n\
\359,application/x-latex,latex\n\
\360,application/x-lha,lha\n\
\361,application/x-lzh,lzh\n\
\362,application/x-lzx,lzx\n\
\363,application/x-maker,frm maker frame fm fb book fbdoc\n\
\364,application/x-mif,mif\n\
\365,application/x-ms-wmz,wmz\n\
\366,application/x-ms-wmd,wmd\n\
\367,application/x-msdos-program,com exe bat dll,exe\n\
\368,application/x-msi,msi\n\
\369,application/x-netcdf,nc\n\
\370,application/x-ns-proxy-autoconfig,pac\n\
\371,application/x-nwc,nwc\n\
\372,application/x-object,o\n\
\373,application/x-oz-application,oza\n\
\374,application/x-pkcs7-certreqresp,p7r\n\
\375,application/x-pkcs7-crl,crl\n\
\376,application/x-python-code,pyc pyo,unknown-pyc-pyo\n\
\377,application/x-quicktimeplayer,qtl\n\
\378,application/x-redhat-package-manager,rpm\n\
\379,application/x-rx\n\
\380,application/x-sh\n\
\381,application/x-shar,shar\n\
\382,application/x-shellscript\n\
\383,application/x-shockwave-flash,swf swfl,swf\n\
\384,application/x-sh,sh\n\
\385,application/x-stuffit,sit\n\
\386,application/x-sv4cpio,sv4cpio\n\
\387,application/x-sv4crc,sv4crc\n\
\388,application/x-tar,tar\n\
\389,application/x-tcl,tcl\n\
\390,application/x-tex-gf,gf\n\
\391,application/x-tex-pk,pk\n\
\392,application/x-texinfo,texinfo texi,texi\n\
\393,application/x-trash,~ % bak old sik\n\
\394,application/x-troff,t tr roff\n\
\395,application/x-troff-man,man\n\
\396,application/x-troff-me,me\n\
\397,application/x-troff-ms,ms\n\
\398,application/x-ustar,ustar\n\
\399,application/x-videolan\n\
\400,application/x-wais-source,src\n\
\401,application/x-wingz,wz\n\
\402,application/x-x509-ca-cert,crt\n\
\403,application/x-xcf,xcf\n\
\404,application/x-xfig,fig\n\
\405,audio/32kadpcm\n\
\406,audio/basic,au snd,au\n\
\407,audio/g.722.1\n\
\408,audio/l16\n\
\409,audio/midi,mid midi kar,mid\n\
\410,audio/mp4a-latm\n\
\411,audio/mpa-robust\n\
\412,audio/mpeg,mpga mpega mp2 mp3 m4a,mp3\n\
\413,audio/mpegurl,m3u\n\
\414,audio/parityfec\n\
\415,audio/prs.sid,sid\n\
\416,audio/telephone-event\n\
\417,audio/tone\n\
\418,audio/vnd.cisco.nse\n\
\419,audio/vnd.cns.anp1\n\
\420,audio/vnd.cns.inf1\n\
\421,audio/vnd.digital-winds\n\
\422,audio/vnd.everad.plj\n\
\423,audio/vnd.lucent.voice\n\
\424,audio/vnd.nortel.vbk\n\
\425,audio/vnd.nuera.ecelp4800\n\
\426,audio/vnd.nuera.ecelp7470\n\
\427,audio/vnd.nuera.ecelp9600\n\
\428,audio/vnd.octel.sbc\n\
\429,audio/vnd.qcelp\n\
\430,audio/vnd.rhetorex.32kadpcm\n\
\431,audio/vnd.vmx.cvsd\n\
\432,audio/x-aiff,aif aiff aifc,aiff\n\
\433,audio/x-gsm,gsm\n\
\434,audio/x-mpegurl,m3u\n\
\435,audio/x-ms-wma,wma\n\
\436,audio/x-ms-wax,wax\n\
\437,audio/x-pn-realaudio-plugin\n\
\438,audio/x-pn-realaudio,ra rm ram,ra\n\
\439,audio/x-realaudio,ra\n\
\440,audio/x-scpls,pls\n\
\441,audio/x-sd2,sd2\n\
\442,audio/x-wav,wav\n\
\443,chemical/x-pdb,pdb\n\
\444,chemical/x-xyz,xyz\n\
\445,image/cgm\n\
\446,image/g3fax\n\
\447,image/gif,gif\n\
\448,image/ief,ief\n\
\449,image/jpeg,jpeg jpg jpe,jpeg\n\
\450,image/naplps\n\
\451,image/pcx,pcx\n\
\452,image/png,png\n\
\453,image/prs.btif\n\
\454,image/prs.pti\n\
\455,image/svg+xml,svg svgz,svg\n\
\456,image/tiff,tiff tif,tiff\n\
\457,image/vnd.cns.inf2\n\
\458,image/vnd.djvu,djvu djv\n\
\459,image/vnd.dwg\n\
\460,image/vnd.dxf\n\
\461,image/vnd.fastbidsheet\n\
\462,image/vnd.fpx\n\
\463,image/vnd.fst\n\
\464,image/vnd.fujixerox.edmics-mmr\n\
\465,image/vnd.fujixerox.edmics-rlc\n\
\466,image/vnd.mix\n\
\467,image/vnd.net-fpx\n\
\468,image/vnd.svf\n\
\469,image/vnd.wap.wbmp,wbmp\n\
\470,image/vnd.xiff\n\
\471,image/x-cmu-raster,ras\n\
\472,image/x-coreldraw,cdr\n\
\473,image/x-coreldrawpattern,pat\n\
\474,image/x-coreldrawtemplate,cdt\n\
\475,image/x-corelphotopaint,cpt\n\
\476,image/x-icon,ico\n\
\477,image/x-jg,art\n\
\478,image/x-jng,jng\n\
\479,image/x-ms-bmp,bmp\n\
\480,image/x-photoshop,psd\n\
\481,image/x-portable-anymap,pnm\n\
\482,image/x-portable-bitmap,pbm\n\
\483,image/x-portable-graymap,pgm\n\
\484,image/x-portable-pixmap,ppm\n\
\485,image/x-rgb,rgb\n\
\486,image/x-xbitmap,xbm\n\
\487,image/x-xpixmap,xpm\n\
\488,image/x-xwindowdump,xwd\n\
\489,inode/chardevice\n\
\490,inode/blockdevice\n\
\491,inode/directory-locked\n\
\492,inode/directory\n\
\493,inode/fifo\n\
\494,inode/socket\n\
\495,message/delivery-status\n\
\496,message/disposition-notification\n\
\497,message/external-body\n\
\498,message/http\n\
\499,message/s-http\n\
\500,message/news\n\
\501,message/partial\n\
\502,message/rfc822\n\
\503,model/iges,igs iges\n\
\504,model/mesh,msh mesh silo\n\
\505,model/vnd.dwf\n\
\506,model/vnd.flatland.3dml\n\
\507,model/vnd.gdl\n\
\508,model/vnd.gs-gdl\n\
\509,model/vnd.gtw\n\
\510,model/vnd.mts\n\
\511,model/vnd.vtu\n\
\512,model/vrml,wrl vrml,vrml\n\
\513,multipart/alternative\n\
\514,multipart/appledouble\n\
\515,multipart/byteranges\n\
\516,multipart/digest\n\
\517,multipart/encrypted\n\
\518,multipart/form-data\n\
\519,multipart/header-set\n\
\520,multipart/mixed\n\
\521,multipart/parallel\n\
\522,multipart/related\n\
\523,multipart/report\n\
\524,multipart/signed\n\
\525,multipart/voice-message\n\
\526,text/calendar,ics icz,ics\n\
\527,text/comma-separated-values,csv\n\
\528,text/css,css\n\
\529,text/directory\n\
\530,text/english\n\
\531,text/enriched\n\
\532,text/h323,323\n\
\533,text/html,htm html shtml,html\n\
\534,text/iuls,uls\n\
\535,text/mathml,mml\n\
\536,text/parityfec\n\
\537,text/plain,asc txt text diff pot,txt\n\
\538,text/prs.lines.tag\n\
\539,text/rfc822-headers\n\
\540,text/richtext,rtx\n\
\541,text/rtf,rtf\n\
\542,text/scriptlet,sct wsc\n\
\543,text/t140\n\
\544,text/texmacs,tm ts\n\
\545,text/tab-separated-values,tsv\n\
\546,text/uri-list\n\
\547,text/vnd.abc\n\
\548,text/vnd.curl\n\
\549,text/vnd.DMClientScript\n\
\550,text/vnd.flatland.3dml\n\
\551,text/vnd.fly\n\
\552,text/vnd.fmi.flexstor\n\
\553,text/vnd.in3d.3dml\n\
\554,text/vnd.in3d.spot\n\
\555,text/vnd.IPTC.NewsML\n\
\556,text/vnd.IPTC.NITF\n\
\557,text/vnd.latex-z\n\
\558,text/vnd.motorola.reflex\n\
\559,text/vnd.ms-mediapackage\n\
\560,text/vnd.sun.j2me.app-descriptor,jad\n\
\561,text/vnd.wap.si\n\
\562,text/vnd.wap.sl\n\
\563,text/vnd.wap.wml,wml\n\
\564,text/vnd.wap.wmlscript,wmls\n\
\565,text/x-c++hdr,h++ hpp hxx hh,hh\n\
\566,text/x-c++src,c++ cpp cxx cc,cc\n\
\567,text/x-chdr,h\n\
\568,text/x-crontab\n\
\569,text/x-csh,csh\n\
\570,text/x-csrc,c\n\
\571,text/x-java,java\n\
\572,text/x-makefile\n\
\573,text/x-moc,moc\n\
\574,text/x-pascal,p pas,pas\n\
\575,text/x-pcs-gcd,gcd\n\
\576,text/x-perl,pl pm,pl\n\
\577,text/x-python,py\n\
\578,text/x-server-parsed-html,shmtl,shtml\n\
\579,text/x-setext,etx\n\
\580,text/x-sh,sh\n\
\581,text/x-tcl,tcl tk,tcl\n\
\582,text/x-tex,tex ltx sty cls,tex\n\
\583,text/x-vcalendar,vcs\n\
\584,text/x-vcard,vcf\n\
\585,video/dl,dl\n\
\586,video/fli,fli\n\
\587,video/gl,gl\n\
\588,video/mpeg,mpeg mpg mpe,mpeg\n\
\589,video/mp4,mp4\n\
\590,video/quicktime,qt mov,mov\n\
\591,video/mp4v-es\n\
\592,video/parityfec\n\
\593,video/pointer\n\
\594,video/vnd.fvt\n\
\595,video/vnd.motorola.video\n\
\596,video/vnd.motorola.videop\n\
\597,video/vnd.mpegurl,mxu\n\
\598,video/vnd.mts\n\
\599,video/vnd.nokia.interleaved-multimedia\n\
\600,video/vnd.vivo\n\
\601,video/x-dv,dif dv\n\
\602,video/x-la-asf,lsf lsx,lsf\n\
\603,video/x-mng,mng\n\
\604,video/x-ms-asf,asf asx,asf\n\
\605,video/x-ms-wm,wm\n\
\606,video/x-ms-wmv,wmv\n\
\607,video/x-ms-wmx,wmx\n\
\608,video/x-ms-wvx,wvx\n\
\609,video/x-msvideo,avi\n\
\610,video/x-sgi-movie,movie\n\
\611,x-conference/x-cooltalk,ice\n\
\612,x-world/x-vrml,vrm vrml wrl,vrml\n\
\613,binary/zip-compressed,zip\n\
\614,video/ogg,ogv\n\
\615,video/matroska,mkv\n\
\616,video/flash,flv\n\
\617,video/ogg-media,ogm\n\
\618,application/x-7z-compressed,7z\n\
\619,audio/speex,spx\n\
\620,audio/ogg,oga\n\
\621,audio/flac,flac"
