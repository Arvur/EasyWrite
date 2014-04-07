unit U_Ib2Html;

interface

uses
  SysUtils, RegExpr;

 function Do_Ib2Html(aInput : string) : string;

implementation

uses
  Dialogs;

var
 PrevModifier : Boolean;

//function ExecRegExpr (const ARegExpr, AInputStr : string) : boolean;
//true если строка AInputString совпадает с выражением ARegExpr
//При ошибках в ARegExpr будет генерировать exception!

//function ReplaceRegExpr (const ARegExpr, AInputStr, AReplaceStr : string; AUseSubstitution : boolean = False) : string;
//Возвращает AInputStr в которой все вхождения выражения ARegExpr заменены на AReplaceStr.
//Если AUseSubstitution = true, то AReplaceStr будет восприниматься как шаблон для Substitution:

// ReplaceRegExpr ('({-i}block|var)\s*\(\s*([^ ]*)\s*\)\s*', 'BLOCK( test1)', 'def "$1" value "$2"', True)
//  возвращает:  def 'BLOCK' value 'test1'
// ReplaceRegExpr ('({-i}block|var)\s*\(\s*([^ ]*)\s*\)\s*', 'BLOCK( test1)', 'def "$1" value "$2"')
//  возвращает:  def "$1" value "$2"

function Parse_Code_Plain(aInput : string) : string;
//while(/\[no\](.*)\[\/no\]/.test(tx)){
//var tmp=RegExp.$1
//(*1)while(/&([^a][^m][^p][^;])/.test(tx)){tx=tx.replace(/&([^a][^m][^p][^;])/,'&amp;'+RegExp.$1)} ! выносим наружу !
//while(/\[/.test(tmp)){tmp=tmp.replace(/\[/,'&#91;')}
//tx=tx.replace(/\[no\].*\[\/no\]/,tmp)}
const
 in_PlainCode = '\[no\](.*)\[\/no\]';
var
 tmpRE : TRegExpr;
 tx, tmp : string;
begin
 tx := aInput;
 while ExecRegExpr(in_PlainCode, tx) do begin
  tmpRE := TRegExpr.Create; tmpRE.Expression := in_PlainCode;
  try
   tmpRE.Exec(tx);
   tmp := ReplaceRegExpr('\[', tmpRE.Match[1], '&#91;');
   tx  := StringReplace(tx, tmpRE.Match[0], tmp, []);
  finally
   tmpRE.Free;
  end;
 end;
 Result := tx;
end;

function Parse_Moderation(aInput : string) : string;
//if(!tx.match(/#moderation mode/i)){while(/[<>]/.test(tx)){tx=tx.replace(/</,'&lt;');tx=tx.replace(/>/,'&gt;')}}
var
 tx : string;
begin
 if not ExecRegExpr('#moderation mode', aInput)
  then begin
   tx := ReplaceRegExpr('<', aInput, '&lt;');
   tx := ReplaceRegExpr('>',     tx, '&gt;');
   Result := tx;
  end
  else Result := aInput;
end;

function Parse_Tags(aInput : string) : string;
const
 c1 = '<table cellpadding=3 cellspacing=0 bgcolor=white width=75% border=1 bordercolor=#EEEEEE><tr><td class=lgf>';
var
 tx : string;
begin
 tx := aInput;

 tx := ReplaceRegExpr('\[\*\]', tx, '<li>');
 tx := ReplaceRegExpr('\[hr\]', tx, '<hr width=100% size=1>');

 tx := ReplaceRegExpr('\[\/list\]', tx, '</ul>');
 tx := ReplaceRegExpr(  '\[list\]', tx,  '<ul>');

 tx := ReplaceRegExpr('\[\/b\]', tx, '</b>');
 tx := ReplaceRegExpr(  '\[b\]', tx, '<b>');
 tx := ReplaceRegExpr(  '\[i\]', tx, '<i>');
 tx := ReplaceRegExpr('\[\/i\]', tx, '</i>');
 tx := ReplaceRegExpr(  '\[u\]', tx, '<u>');
 tx := ReplaceRegExpr('\[\/u\]', tx, '</u>');
 tx := ReplaceRegExpr(  '\[s\]', tx, '<small>');
 tx := ReplaceRegExpr('\[\/s\]', tx, '</small>');
 tx := ReplaceRegExpr('\[\/c\]', tx, '</center>');
 tx := ReplaceRegExpr(  '\[c\]', tx, '<center>');

 tx := ReplaceRegExpr('\[\/center\]', tx, '</center>');
 tx := ReplaceRegExpr(  '\[center\]', tx,  '<center>');

 tx := ReplaceRegExpr('\[\/sub\]', tx, '</sub>');
 tx := ReplaceRegExpr(  '\[sub\]', tx,  '<sub>');
 tx := ReplaceRegExpr('\[\/sup\]', tx, '</sup>');
 tx := ReplaceRegExpr(  '\[sup\]', tx,  '<sup>');

 tx := ReplaceRegExpr(  '\[table\]', tx, '<table cellpadding="3" cellspacing="0" bgcolor="#FFFFFF" width="75%" border="1" bordercolor="#EEEEEE"><tr class=lgf><td>');
 while ExecRegExpr('\[tab\] \[tab\]', tx) do begin
  PrevModifier := RegExprModifierG; RegExprModifierG := True; // Greedy
   tx := ReplaceRegExpr(' \[tab\]', tx, '&nbsp;</td><td>');
  RegExprModifierG := PrevModifier;
 end;
 tx := ReplaceRegExpr(    '\[tab\]', tx, '</td><td>');
 tx := ReplaceRegExpr(     '\[tr\]', tx, '</td></tr><tr class=lgf><td>');
 tx := ReplaceRegExpr('\[\/table\]', tx, '</td></tr></table>');

 tx := ReplaceRegExpr('\[url\]([^\[]+)\[\/url\]', tx, '<a target=_blank href="$1">$1</a>', True);
 tx := ReplaceRegExpr('\[email\]([^\[]+)\[\/email\]', tx, '<a target=_blank href="mailto:$1">$1</a>', True);
 tx := ReplaceRegExpr('\[img\]([^\[]+)\[\/img\]', tx, '<img src="$1" border=0>', True);

 tx := ReplaceRegExpr('\[url=([^\]]+)\]', tx, '<a href="$1">', True);
 tx := ReplaceRegExpr('\[email=([^\]]+)\]', tx, '<a href="mailto:$1">', True);
 tx := ReplaceRegExpr('\[\/((url)|(email))\]', tx, '</a>');

 tx := ReplaceRegExpr('\[color=([^\]]+)\]', tx, '<font color="$1">', True);
 tx := ReplaceRegExpr('\[\/((color)|(size)|(font))\]', tx, '</font>');
 tx := ReplaceRegExpr('\[size=([^\]]+)\]', tx, '<font size="$1">', True);
 tx := ReplaceRegExpr('\[font=([^\]]+)\]', tx, '<font face="$1">', True);

 tx := ReplaceRegExpr('\[code\]', tx, '<br> <br><font size=1><b>Код:</b></font>' + c1);
 tx := ReplaceRegExpr('(\[q\])|(\[quote\])', tx, '<br> <br><font size=1><b>Цитата:</b></font>' + c1);
 tx := ReplaceRegExpr('(\[\/q\])|(\[\/quote\])|(\[\/code\])', tx, '</td></tr></table>');

 tx := ReplaceRegExpr('\s\s', tx, ' &nbsp;');

 tx := ReplaceRegExpr(   ':\)', tx, '<img src=http://i.ru-board.com/s/smile.gif>');
 tx := ReplaceRegExpr(   ':\(', tx, '<img src=http://i.ru-board.com/s/sad.gif>');

 tx := ReplaceRegExpr('\[#\](.*)\[\/#\]', tx, '', True);

 Result := tx;
end;

function PreParse(aInput : string) : string;
// все переводы строк менять на \n, обратные слэши делать двойными — \\,
// перед одинарными кавычками ставить обратный слэш, напр.: don\'t. Весь текст должен быть в одну строчку
var
// Counter : Integer;
 tx : string;
begin
// tx := '';
// for Counter := 0 to (aInput.Count - 1) do
//  tx := tx + '\n' + aInput[Counter];
 tx := aInput;
// tx := StringReplace(tx,    '\',  '\\', [rfReplaceAll]);
 tx := StringReplace(tx, #$D#$A,  '\n', [rfReplaceAll]);
// tx := StringReplace(tx,   '''', '\''', [rfReplaceAll]); // Д'Артаньян

 Result := tx;
end;

function Do_Ib2Html(aInput : string) : string;
var
 tx : string;
begin
 tx := aInput;

 tx := ReplaceRegExpr('&([^a][^m][^p][^;])', tx, '&amp;$1', True); //(*1)
 tx := Parse_Moderation(tx);

 PrevModifier := RegExprModifierG; RegExprModifierG := True; // Greedy
  tx := ReplaceRegExpr('(^|\n|[^">=\]])(ht|f)(tp:\/\/[^\s\n]+)', tx, '$1<a target=_blank href="$2$3">$2$3</a>', True);
 RegExprModifierG := PrevModifier;

 tx := PreParse(tx);
 tx := Parse_Code_Plain(tx);
 tx := Parse_Tags(tx);

 tx := StringReplace(tx, '\n',  '<br>', [rfReplaceAll]);
 Result := {'&nbsp;' + }tx;
end;

initialization
 begin
  RegExprModifierG := False; // Greedy OFF
  RegExprModifierI := True;  // Регистро-независимый режим
 end;

end.

// Original
//
//function Ib2HTML(tx){
//while(/\[no\](.*)\[\/no\]/.test(tx)){
//var tmp=RegExp.$1
//(*1)while(/&([^a][^m][^p][^;])/.test(tx)){tx=tx.replace(/&([^a][^m][^p][^;])/,'&amp;'+RegExp.$1)} ! выносим наружу !
//while(/\[/.test(tmp)){tmp=tmp.replace(/\[/,'&#91;')}
//tx=tx.replace(/\[no\].*\[\/no\]/,tmp)}
//if(!tx.match(/#moderation mode/i)){while(/[<>]/.test(tx)){tx=tx.replace(/</,'&lt;');tx=tx.replace(/>/,'&gt;')}}
//
//while(/\[\*\]/.test(tx)){tx=tx.replace(/\[\*\]/,'<li>')}
//while(/\[hr\]/.test(tx)){tx=tx.replace(/\[hr\]/,'<hr width=100% size=1>')}
//
//while(/\[\/list\]/i.test(tx)){tx=tx.replace(/\[\/list\]/i,'</ul>')}
//while(/\[list\]/i.test(tx)){tx=tx.replace(/\[list\]/i,'<ul>')}
//
//while(/\[\/b\]/.test(tx)){tx=tx.replace(/\[\/b\]/,'</b>')}
//while(/\[b\]/.test(tx)){tx=tx.replace(/\[b\]/,'<b>')}
//while(/\[i\]/.test(tx)){tx=tx.replace(/\[i\]/,'<i>')}
//while(/\[\/i\]/.test(tx)){tx=tx.replace(/\[\/i\]/,'</i>')}
//while(/\[u\]/.test(tx)){tx=tx.replace(/\[u\]/,'<u>')}
//while(/\[\/u\]/.test(tx)){tx=tx.replace(/\[\/u\]/,'</u>')}
//while(/\[s\]/.test(tx)){tx=tx.replace(/\[s\]/,'<small>')}
//while(/\[\/s\]/.test(tx)){tx=tx.replace(/\[\/s\]/,'</small>')}
//while(/\[\/c\]/.test(tx)){tx=tx.replace(/\[\/c\]/,'</center>')}
//while(/\[c\]/.test(tx)){tx=tx.replace(/\[c\]/,'<center>')}
//while(/\[\/center\]/.test(tx)){tx=tx.replace(/\[\/center\]/,'</center>')}
//while(/\[center\]/.test(tx)){tx=tx.replace(/\[center\]/,'<center>')}
//
//
//while(/\[table\]/.test(tx)){tx=tx.replace(/\[table\]/,'<table cellpadding="3" cellspacing="0" bgcolor="#FFFFFF" width="75%" border="1" bordercolor="#EEEEEE"><tr class=lgf><td>')}
//while(/\[tab\] \[tab\]/.test(tx)){tx=tx.replace(/ \[tab\]/g,'&nbsp;</td><td>')}
//while(/\[tab\]/.test(tx)){tx=tx.replace(/\[tab\]/,'</td><td>')}
//while(/\[tr\]/.test(tx)){tx=tx.replace(/\[tr\]/,'</td></tr><tr class=lgf><td>')}
//while(/\[\/table\]/.test(tx)){tx=tx.replace(/\[\/table\]/,'</td></tr></table>')}
//
///*while(/\[list\=1\](([^\[][^\/][^l][^i][^s][^t][^\]])+)\[\/list\]/i.test(tx)){tx=tx.replace(/\[list\=1\](([^\[][^\/][^l][^i][^s][^t][^\]])+)\[\/list\]/i,'<ol>'+RegExp.$1+'</ol>')}*/
//
//while(/\[url\]([^\[]+)\[\/url\]/i.test(tx)){tx=tx.replace(/\[url\]([^\[]+)\[\/url\]/,'<a target=_blank href="'+RegExp.$1+'">'+RegExp.$1+'</a>')}
//while(/\[email\]([^\[]+)\[\/email\]/i.test(tx)){tx=tx.replace(/\[email\]([^\[]+)\[\/email\]/,'<a target=_blank href="mailto:'+RegExp.$1+'">'+RegExp.$1+'</a>')}
//while(/\[img\]([^\[]+)\[\/img\]/i.test(tx)){tx=tx.replace(/\[img\]([^\[]+)\[\/img\]/,'<img src="'+RegExp.$1+'">')}
//
//while(/\[url=([^\]]+)\]/.test(tx)){tx=tx.replace(/\[url=([^\]]+)\]/,'<a href="'+RegExp.$1+'">')}
//while(/\[email=([^\]]+)\]/.test(tx)){tx=tx.replace(/\[email=([^\]]+)\]/,'<a href="mailto:'+RegExp.$1+'">')}
//while(/\[\/((url)|(email))\]/.test(tx)){tx=tx.replace(/\[\/((url)|(email))\]/,'</a>')}
//while(/($|\n|[^">=\]])(http|ftp)(:\/\/[^\s\n]+)/i.test(tx)){tx=tx.replace(/($|\n|[^">=\]])(ht|f)(tp:\/\/[^\s\n]+)/,RegExp.$1+'<a target=_blank href="'+RegExp.$2+RegExp.$3+'">'+RegExp.$2+RegExp.$3+'</a>')}
//while(/\[color=([^\]]+)\]/.test(tx)){tx=tx.replace(/\[color=([^\]]+)\]/,'<font color="'+RegExp.$1+'">')}
//while(/\[\/((color)|(size)|(font))\]/.test(tx)){tx=tx.replace(/\[\/((color)|(size)|(font))\]/,'</font>')}
//while(/\[size=([^\]]+)\]/.test(tx)){tx=tx.replace(/\[size=([^\]]+)\]/,'<font size="'+RegExp.$1+'">')}
//while(/\[font=([^\]]+)\]/.test(tx)){tx=tx.replace(/\[font=([^\]]+)\]/,'<font face="'+RegExp.$1+'">')}
//
//var q='<br> <br><font size=1><b>Цитата:</b></font>'
//var c='<br> <br><font size=1><b>Код:</b></font>'
//var c1='<table cellpadding=3 cellspacing=0 bgcolor=white width=75% border=1 bordercolor=#EEEEEE><tr><td class=lgf>'
//var c2='</td></tr></table>'
//while(/\[code\]/.test(tx)){tx=tx.replace(/\[code\]/,c+c1)}
//while(/(\[q\])|(\[quote\])/.test(tx)){tx=tx.replace(/(\[q\])|(\[quote\])/,q+c1)}
//while(/(\[\/q\])|(\[\/quote\])|(\[\/code\])/.test(tx)){tx=tx.replace(/(\[\/q\])|(\[\/quote\])|(\[\/code\])/,c2)}
///*alert(tx)*/
//while(/[\n\r][\n\r]?/.test(tx)){tx=tx.replace(/[\n\r][\n\r]?/,'<br>')}
//while(/\s\s/.test(tx)){tx=tx.replace(/\s\s/,' &nbsp;')}
//
//
//while(/:\)/.test(tx)){tx=tx.replace(/:\)/,'<img src=http://i.ru-board.com/s/smile.gif>')}
//while(/:\(/.test(tx)){tx=tx.replace(/:\(/,'<img src=http://i.ru-board.com/s/sad.gif>')}
//while(/:gigi:/.test(tx)){tx=tx.replace(/:gigi:/,'<img src=http://i.ru-board.com/s/gigi.gif>')}
//
//document.getElementById('PREW').innerHTML='&nbsp;'
//document.getElementById('PREW').innerHTML=tx
//}


