
module FST.Lexer where

import FST.Alex

semicolon (Pn _ l _) _   = TokenSemi l

listEps (Pn _ l _) _   = TokenEps l

hardOpenBracket (Pn _ l _) _   = TokenHOB l

hardClosedBracket (Pn _ l _) _   = TokenHCB l

softOpenBracket (Pn _ l _) _   = TokenSOB l

softClosedBracket (Pn _ l _) _   = TokenSCB l

union (Pn _ l _) _   = TokenUnion l

equal (Pn _ l _) _   = TokenDef l

relation (Pn _ l _) _  = TokenRelation l

star (Pn _ l _) _ = TokenStar l

plus (Pn _ l _) _   = TokenPlus l

repeatSymbol (Pn _ l _) _   = TokenRepeat l

complement (Pn _ l _) _ = TokenComplement l

containment (Pn _ l _) _   = TokenContainment l

minus (Pn _ l _) _ = TokenMinus l

intersect (Pn _ l _) _ = TokenIntersect l

zeroEps (Pn _ l _) _   = TokenEps l

allSymbol (Pn _ l _) _ = TokenAll l

crossproduct (Pn _ l _) _ = TokenCrossproduct l

composition (Pn _ l _) _ = TokenComposition  l

symbol   (Pn _ l _) s = TokenS (l,(init.tail) s)

esymbol (Pn _ l _) s = TokenS (l,tail s)

concatsymbols  (Pn _ l _) s = TokenConcatS (l,(init.tail) s)

mainId (Pn _ l _) _   = TokenMain l

definitions (Pn _ l _) s = let (name:param) = params ((init.tail) s) in TokenFun (l,(name,param))

variable (Pn _ l _) s   = TokenVar   (l,s)

litint (Pn _ l _) s = TokenNum (l,(read s))


semicolon :: Posn -> String -> Token
listEps :: Posn -> String -> Token
hardOpenBracket :: Posn -> String -> Token
hardClosedBracket :: Posn -> String -> Token
softOpenBracket :: Posn -> String -> Token
softClosedBracket :: Posn -> String -> Token
union :: Posn -> String -> Token
equal :: Posn -> String -> Token
relation :: Posn -> String -> Token
star :: Posn -> String -> Token
plus :: Posn -> String -> Token
repeatSymbol :: Posn -> String -> Token
complement :: Posn -> String -> Token
containment :: Posn -> String -> Token
intersect :: Posn -> String -> Token
minus :: Posn -> String -> Token
zeroEps :: Posn -> String -> Token
allSymbol :: Posn -> String -> Token
composition :: Posn -> String -> Token
crossproduct :: Posn -> String -> Token
symbol :: Posn -> String -> Token
concatsymbols :: Posn -> String -> Token
mainId :: Posn -> String -> Token
definitions :: Posn -> String -> Token
variable :: Posn -> String -> Token
litint :: Posn -> String -> Token

params :: String -> [String]
params []       = []
params (',':xs) = let (zs,ys) = span (/= ',') xs
                   in zs:params ys
params xs       = let (zs,ys) = span (/= ',') xs
                   in zs:params ys


type Name   = String

data Token = TokenSemi Int                  | -- ';'
	     TokenHOB  Int                  | -- '['
             TokenHCB  Int                  | -- ']'
             TokenSOB  Int                  | -- '('
             TokenSCB  Int                  | -- ')'
	     TokenConcatS (Int,String)      | -- { s t r i n g }
             TokenStar Int                  | -- '*'
	     TokenComplement Int            | -- '~'
	     TokenContainment Int           | -- '$'
	     TokenMinus Int                 | -- '-'
	     TokenIntersect Int             | -- '&'
             TokenUnion Int                 | -- '|'
	     TokenPlus Int                  | -- '+'
             TokenEps Int                   | -- '0'
             TokenAll Int                   | -- '?'
             TokenS (Int,String)            | -- string
	     TokenRelation Int              | -- ':'
             TokenCrossproduct Int          | -- '.x.'
             TokenComposition Int           | -- '.o.'
             TokenRepeat Int                | -- '^'
             TokenNum  (Int,Int)            | -- num
	     TokenFun  (Int,(Name,[String]))| -- <id,param>
	     TokenMain  Int                 | -- <main>
	     TokenDef  Int                  | -- '::='
	     TokenVar (Int,String)          | -- Variable
	     Err String
	deriving (Eq,Show)

lexer :: String -> [Token]
lexer inp = scan tokens_scan inp

tokens_scan = load_scan (token_acts,stop_act) token_lx
 where stop_act (Pn _ l _) ""  = []
       stop_act (Pn _ l _) inp = [Err ("Parse error at line: "++show l ++
                                       "\nUnable to recognize (tokenize): "
                                       ++ head (lines inp))]


token_acts = [("allSymbol",allSymbol),("complement",complement),("composition",composition),("concatsymbols",concatsymbols),("containment",containment),("crossproduct",crossproduct),("definitions",definitions),("equal",equal),("esymbol",esymbol),("hardClosedBracket",hardClosedBracket),("hardOpenBracket",hardOpenBracket),("intersect",intersect),("listEps",listEps),("litint",litint),("mainId",mainId),("minus",minus),("plus",plus),("relation",relation),("repeatSymbol",repeatSymbol),("semicolon",semicolon),("softClosedBracket",softClosedBracket),("softOpenBracket",softOpenBracket),("star",star),("symbol",symbol),("union",union),("variable",variable),("zeroEps",zeroEps)]

token_lx :: [(Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))]
token_lx = [lx__0_0,lx__1_0,lx__2_0,lx__3_0,lx__4_0,lx__5_0,lx__6_0,lx__7_0,lx__8_0,lx__9_0,lx__10_0,lx__11_0,lx__12_0,lx__13_0,lx__14_0,lx__15_0,lx__16_0,lx__17_0,lx__18_0,lx__19_0,lx__20_0,lx__21_0,lx__22_0,lx__23_0,lx__24_0,lx__25_0,lx__26_0,lx__27_0,lx__28_0,lx__29_0,lx__30_0,lx__31_0,lx__32_0,lx__33_0,lx__34_0,lx__35_0,lx__36_0,lx__37_0,lx__38_0,lx__39_0,lx__40_0,lx__41_0,lx__42_0]
lx__0_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__0_0 = (False,[],-1,(('\t','~'),[('\t',2),('\n',2),('\v',2),('\f',2),('\r',2),(' ',2),('"',27),('#',1),('$',17),('%',29),('&',19),('(',7),(')',8),('*',13),('+',14),('-',18),('.',22),('0',20),('1',42),('2',42),('3',42),('4',42),('5',42),('6',42),('7',42),('8',42),('9',42),(':',10),(';',3),('<',32),('?',21),('A',41),('B',41),('C',41),('D',41),('E',41),('F',41),('G',41),('H',41),('I',41),('J',41),('K',41),('L',41),('M',41),('N',41),('O',41),('P',41),('Q',41),('R',41),('S',41),('T',41),('U',41),('V',41),('W',41),('X',41),('Y',41),('Z',41),('[',4),(']',6),('^',15),('a',41),('b',41),('c',41),('d',41),('e',41),('f',41),('g',41),('h',41),('i',41),('j',41),('k',41),('l',41),('m',41),('n',41),('o',41),('p',41),('q',41),('r',41),('s',41),('t',41),('u',41),('v',41),('w',41),('x',41),('y',41),('z',41),('{',30),('|',9),('~',16)]))
lx__1_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__1_0 = (True,[(0,"",[],Nothing,Nothing)],1,(('\n','\n'),[('\n',-1)]))
lx__2_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__2_0 = (True,[(1,"",[],Nothing,Nothing)],-1,(('\t',' '),[('\t',2),('\n',2),('\v',2),('\f',2),('\r',2),(' ',2)]))
lx__3_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__3_0 = (True,[(2,"semicolon",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__4_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__4_0 = (True,[(4,"hardOpenBracket",[],Nothing,Nothing)],-1,((']',']'),[(']',5)]))
lx__5_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__5_0 = (True,[(3,"listEps",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__6_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__6_0 = (True,[(5,"hardClosedBracket",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__7_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__7_0 = (True,[(6,"softOpenBracket",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__8_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__8_0 = (True,[(7,"softClosedBracket",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__9_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__9_0 = (True,[(8,"union",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__10_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__10_0 = (True,[(10,"relation",[],Nothing,Nothing)],-1,((':',':'),[(':',11)]))
lx__11_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__11_0 = (False,[],-1,(('=','='),[('=',12)]))
lx__12_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__12_0 = (True,[(9,"equal",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__13_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__13_0 = (True,[(11,"star",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__14_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__14_0 = (True,[(12,"plus",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__15_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__15_0 = (True,[(13,"repeatSymbol",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__16_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__16_0 = (True,[(14,"complement",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__17_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__17_0 = (True,[(15,"containment",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__18_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__18_0 = (True,[(16,"minus",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__19_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__19_0 = (True,[(17,"intersect",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__20_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__20_0 = (True,[(18,"zeroEps",[],Nothing,Nothing)],-1,(('0','9'),[('0',42),('1',42),('2',42),('3',42),('4',42),('5',42),('6',42),('7',42),('8',42),('9',42)]))
lx__21_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__21_0 = (True,[(19,"allSymbol",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__22_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__22_0 = (False,[],-1,(('o','x'),[('o',25),('x',23)]))
lx__23_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__23_0 = (False,[],-1,(('.','.'),[('.',24)]))
lx__24_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__24_0 = (True,[(20,"crossproduct",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__25_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__25_0 = (False,[],-1,(('.','.'),[('.',26)]))
lx__26_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__26_0 = (True,[(21,"composition",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__27_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__27_0 = (False,[],27,(('"','"'),[('"',28)]))
lx__28_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__28_0 = (True,[(22,"symbol",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__29_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__29_0 = (True,[(23,"esymbol",[],Nothing,Nothing)],29,(('\t',' '),[('\t',-1),('\n',-1),('\v',-1),('\f',-1),('\r',-1),(' ',-1)]))
lx__30_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__30_0 = (False,[],30,(('}','}'),[('}',31)]))
lx__31_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__31_0 = (True,[(24,"concatsymbols",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__32_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__32_0 = (False,[],-1,(('A','z'),[('A',38),('B',38),('C',38),('D',38),('E',38),('F',38),('G',38),('H',38),('I',38),('J',38),('K',38),('L',38),('M',38),('N',38),('O',38),('P',38),('Q',38),('R',38),('S',38),('T',38),('U',38),('V',38),('W',38),('X',38),('Y',38),('Z',38),('a',38),('b',38),('c',38),('d',38),('e',38),('f',38),('g',38),('h',38),('i',38),('j',38),('k',38),('l',38),('m',33),('n',38),('o',38),('p',38),('q',38),('r',38),('s',38),('t',38),('u',38),('v',38),('w',38),('x',38),('y',38),('z',38)]))
lx__33_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__33_0 = (False,[],39,(('0','z'),[('0',38),('1',38),('2',38),('3',38),('4',38),('5',38),('6',38),('7',38),('8',38),('9',38),('>',40),('A',38),('B',38),('C',38),('D',38),('E',38),('F',38),('G',38),('H',38),('I',38),('J',38),('K',38),('L',38),('M',38),('N',38),('O',38),('P',38),('Q',38),('R',38),('S',38),('T',38),('U',38),('V',38),('W',38),('X',38),('Y',38),('Z',38),('_',38),('a',34),('b',38),('c',38),('d',38),('e',38),('f',38),('g',38),('h',38),('i',38),('j',38),('k',38),('l',38),('m',38),('n',38),('o',38),('p',38),('q',38),('r',38),('s',38),('t',38),('u',38),('v',38),('w',38),('x',38),('y',38),('z',38)]))
lx__34_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__34_0 = (False,[],39,(('0','z'),[('0',38),('1',38),('2',38),('3',38),('4',38),('5',38),('6',38),('7',38),('8',38),('9',38),('>',40),('A',38),('B',38),('C',38),('D',38),('E',38),('F',38),('G',38),('H',38),('I',38),('J',38),('K',38),('L',38),('M',38),('N',38),('O',38),('P',38),('Q',38),('R',38),('S',38),('T',38),('U',38),('V',38),('W',38),('X',38),('Y',38),('Z',38),('_',38),('a',38),('b',38),('c',38),('d',38),('e',38),('f',38),('g',38),('h',38),('i',35),('j',38),('k',38),('l',38),('m',38),('n',38),('o',38),('p',38),('q',38),('r',38),('s',38),('t',38),('u',38),('v',38),('w',38),('x',38),('y',38),('z',38)]))
lx__35_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__35_0 = (False,[],39,(('0','z'),[('0',38),('1',38),('2',38),('3',38),('4',38),('5',38),('6',38),('7',38),('8',38),('9',38),('>',40),('A',38),('B',38),('C',38),('D',38),('E',38),('F',38),('G',38),('H',38),('I',38),('J',38),('K',38),('L',38),('M',38),('N',38),('O',38),('P',38),('Q',38),('R',38),('S',38),('T',38),('U',38),('V',38),('W',38),('X',38),('Y',38),('Z',38),('_',38),('a',38),('b',38),('c',38),('d',38),('e',38),('f',38),('g',38),('h',38),('i',38),('j',38),('k',38),('l',38),('m',38),('n',36),('o',38),('p',38),('q',38),('r',38),('s',38),('t',38),('u',38),('v',38),('w',38),('x',38),('y',38),('z',38)]))
lx__36_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__36_0 = (False,[],39,(('0','z'),[('0',38),('1',38),('2',38),('3',38),('4',38),('5',38),('6',38),('7',38),('8',38),('9',38),('>',37),('A',38),('B',38),('C',38),('D',38),('E',38),('F',38),('G',38),('H',38),('I',38),('J',38),('K',38),('L',38),('M',38),('N',38),('O',38),('P',38),('Q',38),('R',38),('S',38),('T',38),('U',38),('V',38),('W',38),('X',38),('Y',38),('Z',38),('_',38),('a',38),('b',38),('c',38),('d',38),('e',38),('f',38),('g',38),('h',38),('i',38),('j',38),('k',38),('l',38),('m',38),('n',38),('o',38),('p',38),('q',38),('r',38),('s',38),('t',38),('u',38),('v',38),('w',38),('x',38),('y',38),('z',38)]))
lx__37_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__37_0 = (True,[(25,"mainId",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__38_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__38_0 = (False,[],39,(('0','z'),[('0',38),('1',38),('2',38),('3',38),('4',38),('5',38),('6',38),('7',38),('8',38),('9',38),('>',40),('A',38),('B',38),('C',38),('D',38),('E',38),('F',38),('G',38),('H',38),('I',38),('J',38),('K',38),('L',38),('M',38),('N',38),('O',38),('P',38),('Q',38),('R',38),('S',38),('T',38),('U',38),('V',38),('W',38),('X',38),('Y',38),('Z',38),('_',38),('a',38),('b',38),('c',38),('d',38),('e',38),('f',38),('g',38),('h',38),('i',38),('j',38),('k',38),('l',38),('m',38),('n',38),('o',38),('p',38),('q',38),('r',38),('s',38),('t',38),('u',38),('v',38),('w',38),('x',38),('y',38),('z',38)]))
lx__39_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__39_0 = (False,[],39,(('>','>'),[('>',40)]))
lx__40_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__40_0 = (True,[(26,"definitions",[],Nothing,Nothing)],-1,(('0','0'),[]))
lx__41_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__41_0 = (True,[(27,"variable",[],Nothing,Nothing)],-1,(('0','z'),[('0',41),('1',41),('2',41),('3',41),('4',41),('5',41),('6',41),('7',41),('8',41),('9',41),('A',41),('B',41),('C',41),('D',41),('E',41),('F',41),('G',41),('H',41),('I',41),('J',41),('K',41),('L',41),('M',41),('N',41),('O',41),('P',41),('Q',41),('R',41),('S',41),('T',41),('U',41),('V',41),('W',41),('X',41),('Y',41),('Z',41),('_',41),('a',41),('b',41),('c',41),('d',41),('e',41),('f',41),('g',41),('h',41),('i',41),('j',41),('k',41),('l',41),('m',41),('n',41),('o',41),('p',41),('q',41),('r',41),('s',41),('t',41),('u',41),('v',41),('w',41),('x',41),('y',41),('z',41)]))
lx__42_0 :: (Bool, [(Int,String,[Int],Maybe((Char,Char),[(Char,Bool)]),Maybe Int)], Int, ((Char,Char),[(Char,Int)]))
lx__42_0 = (True,[(28,"litint",[],Nothing,Nothing)],-1,(('0','9'),[('0',42),('1',42),('2',42),('3',42),('4',42),('5',42),('6',42),('7',42),('8',42),('9',42)]))

