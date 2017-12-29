module Data.Chinese.GwoyeuRomatzyh where

-- In the make-shift table below, a final lowercase 'r' or lowercase 'p'
-- identify, respectively, retroflex and palatal initial versions of otherwise
-- ambiguous phonetic symbols.
-- TODO (NG) should be included as the nasal guttural
data Initial =
-- Unaspirated Stops | Aspirated Stops | Nasals | Fricatives | Voiced Continuants |

-- * Labials *
          B          |        P        |   M    |     F      |
-- * Dentals *
          D          |        T        |   N    |                       L         |
-- * Dental silibants *
          TZ         |        TS       |              S      |
-- * Retroflexes *
          Jr         |        CHr      |              SHr    |          R         |
-- * Palatals *
          Jp         |        CHp      |              SHp    |
-- * Gutturals *
          G          |        K        |              H      |          ZeroI

data Final = Final Medial Vowel Ending

data Medial = ZeroM | Im | Um | IUm

data Vowel = ZeroV | Av | Ev | Ov

data Ending = ZeroE | Ie | Ue | Ne | NGe | Le

data Tone = First | Second | Third | Fourth

data Syllable = Syllable Initial Final Tone

instance Show Initial where
    show B = "b"
    show P = "p"
    show M = "m"
    show F = "f"
    show D = "d"
    show T = "t"
    show N = "n"
    show L = "l"
    show TZ = "tz"
    show TS = "ts"
    show S = "s"
    show Jr = "j"
    show CHr = "ch"
    show SHr = "sh"
    show R = "r"
    show Jp = "j"
    show CHp = "ch"
    show SHp = "sh"
    show G = "g"
    show K = "k"
    show H = "h"
    show ZeroI = ""

instance Show Syllable where
    show (Syllable i f t) = show i ++ renderFinalWithTone f t

-- 37 valid finals
renderFinalWithTone :: Final -> Tone -> String
renderFinalWithTone (Final ZeroM ZeroV ZeroE) tone = case tone of
    First  -> "y"
    Second -> "yr"
    Third  -> "yy"
    Fourth -> "yh"
renderFinalWithTone (Final ZeroM Av ZeroE) tone = case tone of
    First  -> "a"
    Second -> "ar"
    Third  -> "aa"
    Fourth -> "ah"
renderFinalWithTone (Final ZeroM Ev ZeroE) tone = case tone of
    First  -> "e"
    Second -> "er"
    Third  -> "ee"
    Fourth -> "eh"


renderFinalWithTone (Final ZeroM Av Ue) tone = case tone of
    First  -> "au"
    Second -> "aur"
    Third  -> "ao"
    Fourth -> "aw"
renderFinalWithTone (Final ZeroM Ov Ue) tone = case tone of
    First  -> "ou"
    Second -> "our"
    Third  -> "oou"
    Fourth -> "ow"


-- TODO ang
-- TODO eng
renderFinalWithTone (Final ZeroM Ov NGe) tone = case tone of
    First  -> "ong"
    Second -> "orng"
    Third  -> "oong"
    Fourth -> "onq"

-- TODO u
renderFinalWithTone (Final Um Av ZeroE) tone = case tone of
    First  -> "ua"
    Second -> "wa"
    Third  -> "oa"
    Fourth -> "uah"
renderFinalWithTone (Final Um Ov ZeroE) tone = case tone of
    First  -> "uo"
    Second -> "wo"
    Third  -> "uoo"
    Fourth -> "uoh"


test :: IO ()
test = print [ Syllable SHr (Final Um Ov ZeroE) First
             , Syllable Jr (Final ZeroM Ov NGe) First
             , Syllable G (Final Um Ov ZeroE) Second
             , Syllable H (Final Um Av ZeroE) Fourth
             ]
