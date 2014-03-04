<!-- R syntax highlighter -->
<script type="text/javascript">
var hljs=new function(){function m(p){return p.replace(/&/gm,"&amp;").replace(/</gm,"&lt;")}function f(r,q,p){return RegExp(q,"m"+(r.cI?"i":"")+(p?"g":""))}function b(r){for(var p=0;p<r.childNodes.length;p++){var q=r.childNodes[p];if(q.nodeName=="CODE"){return q}if(!(q.nodeType==3&&q.nodeValue.match(/\s+/))){break}}}function h(t,s){var p="";for(var r=0;r<t.childNodes.length;r++){if(t.childNodes[r].nodeType==3){var q=t.childNodes[r].nodeValue;if(s){q=q.replace(/\n/g,"")}p+=q}else{if(t.childNodes[r].nodeName=="BR"){p+="\n"}else{p+=h(t.childNodes[r])}}}if(/MSIE [678]/.test(navigator.userAgent)){p=p.replace(/\r/g,"\n")}return p}function a(s){var r=s.className.split(/\s+/);r=r.concat(s.parentNode.className.split(/\s+/));for(var q=0;q<r.length;q++){var p=r[q].replace(/^language-/,"");if(e[p]){return p}}}function c(q){var p=[];(function(s,t){for(var r=0;r<s.childNodes.length;r++){if(s.childNodes[r].nodeType==3){t+=s.childNodes[r].nodeValue.length}else{if(s.childNodes[r].nodeName=="BR"){t+=1}else{if(s.childNodes[r].nodeType==1){p.push({event:"start",offset:t,node:s.childNodes[r]});t=arguments.callee(s.childNodes[r],t);p.push({event:"stop",offset:t,node:s.childNodes[r]})}}}}return t})(q,0);return p}function k(y,w,x){var q=0;var z="";var s=[];function u(){if(y.length&&w.length){if(y[0].offset!=w[0].offset){return(y[0].offset<w[0].offset)?y:w}else{return w[0].event=="start"?y:w}}else{return y.length?y:w}}function t(D){var A="<"+D.nodeName.toLowerCase();for(var B=0;B<D.attributes.length;B++){var C=D.attributes[B];A+=" "+C.nodeName.toLowerCase();if(C.value!==undefined&&C.value!==false&&C.value!==null){A+='="'+m(C.value)+'"'}}return A+">"}while(y.length||w.length){var v=u().splice(0,1)[0];z+=m(x.substr(q,v.offset-q));q=v.offset;if(v.event=="start"){z+=t(v.node);s.push(v.node)}else{if(v.event=="stop"){var p,r=s.length;do{r--;p=s[r];z+=("</"+p.nodeName.toLowerCase()+">")}while(p!=v.node);s.splice(r,1);while(r<s.length){z+=t(s[r]);r++}}}}return z+m(x.substr(q))}function j(){function q(x,y,v){if(x.compiled){return}var u;var s=[];if(x.k){x.lR=f(y,x.l||hljs.IR,true);for(var w in x.k){if(!x.k.hasOwnProperty(w)){continue}if(x.k[w] instanceof Object){u=x.k[w]}else{u=x.k;w="keyword"}for(var r in u){if(!u.hasOwnProperty(r)){continue}x.k[r]=[w,u[r]];s.push(r)}}}if(!v){if(x.bWK){x.b="\\b("+s.join("|")+")\\s"}x.bR=f(y,x.b?x.b:"\\B|\\b");if(!x.e&&!x.eW){x.e="\\B|\\b"}if(x.e){x.eR=f(y,x.e)}}if(x.i){x.iR=f(y,x.i)}if(x.r===undefined){x.r=1}if(!x.c){x.c=[]}x.compiled=true;for(var t=0;t<x.c.length;t++){if(x.c[t]=="self"){x.c[t]=x}q(x.c[t],y,false)}if(x.starts){q(x.starts,y,false)}}for(var p in e){if(!e.hasOwnProperty(p)){continue}q(e[p].dM,e[p],true)}}function d(B,C){if(!j.called){j();j.called=true}function q(r,M){for(var L=0;L<M.c.length;L++){if((M.c[L].bR.exec(r)||[null])[0]==r){return M.c[L]}}}function v(L,r){if(D[L].e&&D[L].eR.test(r)){return 1}if(D[L].eW){var M=v(L-1,r);return M?M+1:0}return 0}function w(r,L){return L.i&&L.iR.test(r)}function K(N,O){var M=[];for(var L=0;L<N.c.length;L++){M.push(N.c[L].b)}var r=D.length-1;do{if(D[r].e){M.push(D[r].e)}r--}while(D[r+1].eW);if(N.i){M.push(N.i)}return f(O,M.join("|"),true)}function p(M,L){var N=D[D.length-1];if(!N.t){N.t=K(N,E)}N.t.lastIndex=L;var r=N.t.exec(M);return r?[M.substr(L,r.index-L),r[0],false]:[M.substr(L),"",true]}function z(N,r){var L=E.cI?r[0].toLowerCase():r[0];var M=N.k[L];if(M&&M instanceof Array){return M}return false}function F(L,P){L=m(L);if(!P.k){return L}var r="";var O=0;P.lR.lastIndex=0;var M=P.lR.exec(L);while(M){r+=L.substr(O,M.index-O);var N=z(P,M);if(N){x+=N[1];r+='<span class="'+N[0]+'">'+M[0]+"</span>"}else{r+=M[0]}O=P.lR.lastIndex;M=P.lR.exec(L)}return r+L.substr(O,L.length-O)}function J(L,M){if(M.sL&&e[M.sL]){var r=d(M.sL,L);x+=r.keyword_count;return r.value}else{return F(L,M)}}function I(M,r){var L=M.cN?'<span class="'+M.cN+'">':"";if(M.rB){y+=L;M.buffer=""}else{if(M.eB){y+=m(r)+L;M.buffer=""}else{y+=L;M.buffer=r}}D.push(M);A+=M.r}function G(N,M,Q){var R=D[D.length-1];if(Q){y+=J(R.buffer+N,R);return false}var P=q(M,R);if(P){y+=J(R.buffer+N,R);I(P,M);return P.rB}var L=v(D.length-1,M);if(L){var O=R.cN?"</span>":"";if(R.rE){y+=J(R.buffer+N,R)+O}else{if(R.eE){y+=J(R.buffer+N,R)+O+m(M)}else{y+=J(R.buffer+N+M,R)+O}}while(L>1){O=D[D.length-2].cN?"</span>":"";y+=O;L--;D.length--}var r=D[D.length-1];D.length--;D[D.length-1].buffer="";if(r.starts){I(r.starts,"")}return R.rE}if(w(M,R)){throw"Illegal"}}var E=e[B];var D=[E.dM];var A=0;var x=0;var y="";try{var s,u=0;E.dM.buffer="";do{s=p(C,u);var t=G(s[0],s[1],s[2]);u+=s[0].length;if(!t){u+=s[1].length}}while(!s[2]);if(D.length>1){throw"Illegal"}return{r:A,keyword_count:x,value:y}}catch(H){if(H=="Illegal"){return{r:0,keyword_count:0,value:m(C)}}else{throw H}}}function g(t){var p={keyword_count:0,r:0,value:m(t)};var r=p;for(var q in e){if(!e.hasOwnProperty(q)){continue}var s=d(q,t);s.language=q;if(s.keyword_count+s.r>r.keyword_count+r.r){r=s}if(s.keyword_count+s.r>p.keyword_count+p.r){r=p;p=s}}if(r.language){p.second_best=r}return p}function i(r,q,p){if(q){r=r.replace(/^((<[^>]+>|\t)+)/gm,function(t,w,v,u){return w.replace(/\t/g,q)})}if(p){r=r.replace(/\n/g,"<br>")}return r}function n(t,w,r){var x=h(t,r);var v=a(t);var y,s;if(v){y=d(v,x)}else{return}var q=c(t);if(q.length){s=document.createElement("pre");s.innerHTML=y.value;y.value=k(q,c(s),x)}y.value=i(y.value,w,r);var u=t.className;if(!u.match("(\\s|^)(language-)?"+v+"(\\s|$)")){u=u?(u+" "+v):v}if(/MSIE [678]/.test(navigator.userAgent)&&t.tagName=="CODE"&&t.parentNode.tagName=="PRE"){s=t.parentNode;var p=document.createElement("div");p.innerHTML="<pre><code>"+y.value+"</code></pre>";t=p.firstChild.firstChild;p.firstChild.cN=s.cN;s.parentNode.replaceChild(p.firstChild,s)}else{t.innerHTML=y.value}t.className=u;t.result={language:v,kw:y.keyword_count,re:y.r};if(y.second_best){t.second_best={language:y.second_best.language,kw:y.second_best.keyword_count,re:y.second_best.r}}}function o(){if(o.called){return}o.called=true;var r=document.getElementsByTagName("pre");for(var p=0;p<r.length;p++){var q=b(r[p]);if(q){n(q,hljs.tabReplace)}}}function l(){if(window.addEventListener){window.addEventListener("DOMContentLoaded",o,false);window.addEventListener("load",o,false)}else{if(window.attachEvent){window.attachEvent("onload",o)}else{window.onload=o}}}var e={};this.LANGUAGES=e;this.highlight=d;this.highlightAuto=g;this.fixMarkup=i;this.highlightBlock=n;this.initHighlighting=o;this.initHighlightingOnLoad=l;this.IR="[a-zA-Z][a-zA-Z0-9_]*";this.UIR="[a-zA-Z_][a-zA-Z0-9_]*";this.NR="\\b\\d+(\\.\\d+)?";this.CNR="\\b(0[xX][a-fA-F0-9]+|(\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?)";this.BNR="\\b(0b[01]+)";this.RSR="!|!=|!==|%|%=|&|&&|&=|\\*|\\*=|\\+|\\+=|,|\\.|-|-=|/|/=|:|;|<|<<|<<=|<=|=|==|===|>|>=|>>|>>=|>>>|>>>=|\\?|\\[|\\{|\\(|\\^|\\^=|\\||\\|=|\\|\\||~";this.ER="(?![\\s\\S])";this.BE={b:"\\\\.",r:0};this.ASM={cN:"string",b:"'",e:"'",i:"\\n",c:[this.BE],r:0};this.QSM={cN:"string",b:'"',e:'"',i:"\\n",c:[this.BE],r:0};this.CLCM={cN:"comment",b:"//",e:"$"};this.CBLCLM={cN:"comment",b:"/\\*",e:"\\*/"};this.HCM={cN:"comment",b:"#",e:"$"};this.NM={cN:"number",b:this.NR,r:0};this.CNM={cN:"number",b:this.CNR,r:0};this.BNM={cN:"number",b:this.BNR,r:0};this.inherit=function(r,s){var p={};for(var q in r){p[q]=r[q]}if(s){for(var q in s){p[q]=s[q]}}return p}}();hljs.LANGUAGES.cpp=function(){var a={keyword:{"false":1,"int":1,"float":1,"while":1,"private":1,"char":1,"catch":1,"export":1,virtual:1,operator:2,sizeof:2,dynamic_cast:2,typedef:2,const_cast:2,"const":1,struct:1,"for":1,static_cast:2,union:1,namespace:1,unsigned:1,"long":1,"throw":1,"volatile":2,"static":1,"protected":1,bool:1,template:1,mutable:1,"if":1,"public":1,friend:2,"do":1,"return":1,"goto":1,auto:1,"void":2,"enum":1,"else":1,"break":1,"new":1,extern:1,using:1,"true":1,"class":1,asm:1,"case":1,typeid:1,"short":1,reinterpret_cast:2,"default":1,"double":1,register:1,explicit:1,signed:1,typename:1,"try":1,"this":1,"switch":1,"continue":1,wchar_t:1,inline:1,"delete":1,alignof:1,char16_t:1,char32_t:1,constexpr:1,decltype:1,noexcept:1,nullptr:1,static_assert:1,thread_local:1,restrict:1,_Bool:1,complex:1},built_in:{std:1,string:1,cin:1,cout:1,cerr:1,clog:1,stringstream:1,istringstream:1,ostringstream:1,auto_ptr:1,deque:1,list:1,queue:1,stack:1,vector:1,map:1,set:1,bitset:1,multiset:1,multimap:1,unordered_set:1,unordered_map:1,unordered_multiset:1,unordered_multimap:1,array:1,shared_ptr:1}};return{dM:{k:a,i:"</",c:[hljs.CLCM,hljs.CBLCLM,hljs.QSM,{cN:"string",b:"'\\\\?.",e:"'",i:"."},{cN:"number",b:"\\b(\\d+(\\.\\d*)?|\\.\\d+)(u|U|l|L|ul|UL|f|F)"},hljs.CNM,{cN:"preprocessor",b:"#",e:"$"},{cN:"stl_container",b:"\\b(deque|list|queue|stack|vector|map|set|bitset|multiset|multimap|unordered_map|unordered_set|unordered_multiset|unordered_multimap|array)\\s*<",e:">",k:a,r:10,c:["self"]}]}}}();hljs.LANGUAGES.r={dM:{c:[hljs.HCM,{cN:"number",b:"\\b0[xX][0-9a-fA-F]+[Li]?\\b",e:hljs.IMMEDIATE_RE,r:0},{cN:"number",b:"\\b\\d+(?:[eE][+\\-]?\\d*)?L\\b",e:hljs.IMMEDIATE_RE,r:0},{cN:"number",b:"\\b\\d+\\.(?!\\d)(?:i\\b)?",e:hljs.IMMEDIATE_RE,r:1},{cN:"number",b:"\\b\\d+(?:\\.\\d*)?(?:[eE][+\\-]?\\d*)?i?\\b",e:hljs.IMMEDIATE_RE,r:0},{cN:"number",b:"\\.\\d+(?:[eE][+\\-]?\\d*)?i?\\b",e:hljs.IMMEDIATE_RE,r:1},{cN:"keyword",b:"(?:tryCatch|library|setGeneric|setGroupGeneric)\\b",e:hljs.IMMEDIATE_RE,r:10},{cN:"keyword",b:"\\.\\.\\.",e:hljs.IMMEDIATE_RE,r:10},{cN:"keyword",b:"\\.\\.\\d+(?![\\w.])",e:hljs.IMMEDIATE_RE,r:10},{cN:"keyword",b:"\\b(?:function)",e:hljs.IMMEDIATE_RE,r:2},{cN:"keyword",b:"(?:if|in|break|next|repeat|else|for|return|switch|while|try|stop|warning|require|attach|detach|source|setMethod|setClass)\\b",e:hljs.IMMEDIATE_RE,r:1},{cN:"literal",b:"(?:NA|NA_integer_|NA_real_|NA_character_|NA_complex_)\\b",e:hljs.IMMEDIATE_RE,r:10},{cN:"literal",b:"(?:NULL|TRUE|FALSE|T|F|Inf|NaN)\\b",e:hljs.IMMEDIATE_RE,r:1},{cN:"identifier",b:"[a-zA-Z.][a-zA-Z0-9._]*\\b",e:hljs.IMMEDIATE_RE,r:0},{cN:"operator",b:"<\\-(?!\\s*\\d)",e:hljs.IMMEDIATE_RE,r:2},{cN:"operator",b:"\\->|<\\-",e:hljs.IMMEDIATE_RE,r:1},{cN:"operator",b:"%%|~",e:hljs.IMMEDIATE_RE},{cN:"operator",b:">=|<=|==|!=|\\|\\||&&|=|\\+|\\-|\\*|/|\\^|>|<|!|&|\\||\\$|:",e:hljs.IMMEDIATE_RE,r:0},{cN:"operator",b:"%",e:"%",i:"\\n",r:1},{cN:"identifier",b:"`",e:"`",r:0},{cN:"string",b:'"',e:'"',c:[hljs.BE],r:0},{cN:"string",b:"'",e:"'",c:[hljs.BE],r:0},{cN:"paren",b:"[[({\\])}]",e:hljs.IMMEDIATE_RE,r:0}]}};
hljs.initHighlightingOnLoad();
</script>

Rfacebook: Access to Facebook API via R
---------

This package provides a series of functions that allow R users to access Facebook's API to get information about users and posts, and collect public status updates that mention specific keywords.

Current CRAN release is 0.3. To install most updated version (0.3.3) from GitHub, type:

```
library(devtools)
install_github("Rfacebook", "pablobarbera", subdir="Rfacebook")
```

Click <a href="http://github.com/pablobarbera/Rfacebook/blob/master/Rfacebook-manual.pdf?raw=true">here</a> to read the documentation and <a href="http://pablobarbera.com/blog/archives/3.html">here</a> to read the vignette.

<h3>Installation and authentication</h3>

<p>Rfacebook can be installed directly from CRAN, but the most updated version <a href="https://github.com/pablobarbera/Rfacebook">will always be on GitHub</a>. The code below shows how to install from both sources.</p>

<pre><code class="r">install.packages(&quot;Rfacebook&quot;)  # from CRAN
library(devtools)
install_github(&quot;Rfacebook&quot;, &quot;pablobarbera&quot;, subdir = &quot;Rfacebook&quot;)  # from GitHub
</code></pre>

<p>Most API requests require the use of an access token. There are two ways of making authenticated requests with Rfacebook. One option is to generate a temporary token on the <a href="https://developers.facebook.com/tools/explorer/">Graph API Explorer</a>. Then just copy and paste the code into the R console and save it as a string vector to be passed as an argument to any Rfacebook function, as I show below. However, note that this access token will only be valid for two hours. It is possible to generate a &#39;long-lived&#39; token (valid for two months) using the <code>fbOAuth</code> function, but the process is a bit more complicated. For a step-by-step tutorial, check this <a href="http://thinktostart.wordpress.com/2013/11/19/analyzing-facebook-with-r/">fantastic blog post</a> by <a href="https://twitter.com/JulianHi">JulianHi</a>.</p>

<pre><code class="r">library(Rfacebook)
# token generated here: https://developers.facebook.com/tools/explorer 
token &lt;- &quot;XXXXXXXXXXXXXX&quot;
me &lt;- getUsers(&quot;pablobarbera&quot;, token, private_info = TRUE)
me$name # my name
</code></pre>

<pre><code>## [1] &quot;Pablo Barberá&quot;
</code></pre>

<pre><code class="r">me$hometown # my hometown
</code></pre>

<pre><code>## [1] &quot;Cáceres, Spain&quot;
</code></pre>

<p>The example above shows how to retrieve information about a Facebook user. Note that this can be applied to any user (or vector of users), and that both user screen names or IDs can be used as arguments. Private information will be, of course, returned only for friends and if the token has been given permission to access such data.</p>

<h3>Analyzing your network of friends</h3>

<p>The function <code>getFriends</code> allows the user to capture information about his/her Facebook friends. Since user IDs are assigned in consecutive order, it's possible to find out which of our friends was the first one to open a Facebook account. (In my case, one of my friends is user ~35,000 &ndash; he was studying in Harvard around the time Facebook was created.)</p>

<pre><code class="r">my_friends &lt;- getFriends(token, simplify = TRUE)
head(my_friends$id, n = 1) # get lowest user ID
</code></pre>

<pre><code>## [1] &quot;35XXX&quot;
</code></pre>

<p>To access additional information about a list of friends (or any other user), you can use the <code>getUsers</code> function, which will return a data frame with users&#39; Facebook data. Some of the variables that are available for all users are: gender, language, and country. It is also possible to obtain relationship status, hometown, birthday, and location for our friends if we set <code>private_info=TRUE</code>.</p>

<p>A quick analysis of my Facebook friends (see below) indicates that 65% of them are male, more than half of them live in the US and use Facebook in English, and only 2 indicate that their relationship status &ldquo;is complicated&rdquo;. (Note that language and country are extracted from the <code>locale</code> <a href="https://developers.facebook.com/docs/internationalization/">codes</a>.)</p>

<pre><code class="r">my_friends_info &lt;- getUsers(my_friends$id, token, private_info = TRUE)
table(my_friends_info$gender)  # gender
</code></pre>

<pre><code>## female   male 
##    155    292
</code></pre>

<pre><code class="r">table(substr(my_friends_info$locale, 1, 2))  # language
</code></pre>

<pre><code>##  ca  da  de  en  es  fi  fr  gl  it  nb  nl  pl  pt  ru  tr 
##  38   2  10 302  82   1   6   1  11   1   1   1   2   1   2
</code></pre>

<pre><code class="r">table(substr(my_friends_info$locale, 4, 5))  # country
</code></pre>

<pre><code>##  BR  DE  DK  ES  FI  FR  GB  IT  LA  NL  NO  PL  PT  RU  TR  US 
##   1  10   2  82   1   6  79  11  39   1   1   1   1   1   2 223
</code></pre>

<pre><code class="r">table(my_friends_info$relationship_status)[&quot;It&#39;s complicated&quot;]  # relationship status
</code></pre>

<pre><code>## It&#39;s complicated 
##                2
</code></pre>

<p>Finally, the function <code>getNetwork</code> extracts a list of all the mutual friendships among the user friends, which can be then used to analyze and visualize a Facebook ego network. The first step is to use the <code>getNetwork</code> function. If the <code>format</code> option is set equal to <code>edgelist</code>, it will return a list of all the edges of that network. If <code>format=adj.matrix</code>, then it will return an adjacency matrix of dimensions (n x n), with n being the number of friends, and 0 or 1 indicating whether the user in row 'i' is also friends with user in column 'j'.</p>

<pre><code class="r">mat &lt;- getNetwork(token, format = &quot;adj.matrix&quot;)
</code></pre>

<pre><code> |=================================================================| 100%
</code></pre>

<pre><code class="r">dim(mat)
</code></pre>

<pre><code>## [1] 462 462
</code></pre>

<p>This adjacency matrix can then be converted into an igraph object, which facilitates the task of computing measures of centrality, discovering communities, or visualizing the structure of the network. As an illustration, the plot below displays my Facebook ego network, where the colors represent clusters discovered with a community detection algorithm, which clearly overlap with membership in offline communities. This was one of the examples from my <a href='https://github.com/pablobarbera/Rdataviz'>workshop on data visualization with R and ggplot2</a>. The code to replicate it with your own Facebook data is available <a href='https://github.com/pablobarbera/Rdataviz/blob/master/code/05_networks.R'>here</a>. David Smith has also <a href="http://blog.revolutionanalytics.com/2013/11/how-to-analyze-you-facebook-friends-network-with-r.html">posted code to generate a similar network plot</a>.</p>

<center><img src="images/network_plot_final.png" alt="Facebook ego network" style="width: 750px;"/></center>


<h3>Searching public Facebook posts</h3>

<p>Rfacebook can also be used to collect public Facebook posts that mention a given keyword using the <code>searchFacebook</code> function. As shown in the example below, it is possible to search within specific a time range with the <code>until</code> and <code>since</code> options. If they are left empty, the most recent public status updates will be returned. The <code>n</code> argument specifies the maximum number of posts to capture, as long as there are enough posts that contain the keyword. Due to the limitations of the API, it is only possible to search for a single keyword, and the results will not include messages that are more than around two weeks old.</p>

<pre><code class="r">posts &lt;- searchFacebook(string = &quot;upworthy&quot;, token, n = 500, 
    since = &quot;25 november 2013 00:00&quot;, until = &quot;25 november 2013 23:59&quot;)
</code></pre>

<pre><code>## 148 posts
</code></pre>

<pre><code class="r">posts[which.max(posts$likes_count), ]
</code></pre>

<pre><code>##            from_id from_name                                message
## 87 354522044588660  Upworthy Is this real life?!?!? - Adam Mordecai
##                created_time type                link
## 87 2013-11-25T17:10:47+0000 link http://u.pw/1gbVzsV
##                                 id likes_count comments_count shares_count
## 87 354522044588660_669076639799864        8174            559         2701
</code></pre>

<p>The code above shows which public Facebook post mentioning the website &ldquo;upworthy&rdquo; and published on November 25th received the highest number of likes. It also illustrates the type of information that is returned for each post: the name and user ID of who posted it, the text of the status update (&ldquo;message&rdquo;), a timestamp, the type of post (link, status, photo or video), the URL of the link, the ID of the post, and the counts of likes, comments, and shares. Going to facebook.com and pasting the ID of the post after the slash shows that the most popular public Facebook post was <a href="http://www.facebook.com/354522044588660_669076639799864">this one</a>.</p>

<h3>Analyzing data from a Facebook page</h3>

<p>Facebook pages are probably the best source of information about how individuals use this social networking site, since all posts, likes, and comments can be collected combining the <code>getPage</code> and <code>getPost</code> functions. For example, assume that we&#39;re interested in learning about how the Facebook page <a href="https://www.facebook.com/humansofnewyork">Humans of New York</a> has become popular, and what type of audience it has. The first step would be to retrieve a data frame with information about all of its posts using the code below. To make sure I collect every single post, I set <code>n</code> to a very high number, and the function will stop automatically when it reaches the total number of available posts (3,674).</p>

<pre><code class="r">page &lt;- getPage(&quot;humansofnewyork&quot;, token, n = 5000)
</code></pre>

<pre><code>## 100 posts (...) 3674 posts
</code></pre>

<pre><code class="r">page[which.max(page$likes_count), ]
</code></pre>

<pre><code>##              from_id          from_name
## 1915 102099916530784 Humans of New York
               message
## 1915 Today I met an NYU student named Stella.  I took a photo of her.  (...)
##                  created_time  type
## 1915 2012-10-19T00:27:36+0000 photo
##                                                                                                                            link
## 1915 https://www.facebook.com/photo.php?fbid=375691212504985&amp;set=a.102107073196735.4429.102099916530784&amp;type=1&amp;relevant_count=1
##                                   id likes_count comments_count
## 1915 102099916530784_375691225838317      894583         117337
##      shares_count
## 1915        60528
</code></pre>

<p>The <a href="https://www.facebook.com/photo.php?fbid=375691212504985&amp;set=a.102107073196735.4429.102099916530784&amp;type=1&amp;relevant_count=1">most popular post ever</a> received almost 900,000 likes and 120,000 comments, and was shared over 60,000 times. As we can see, the variables returned for each post are the same as when we search for Facebook posts: information about the content of the post, its author, and its popularity and reach. Using this data frame, it is relatively straightforward to visualize how the popularity of Humans of New York has grown exponentially over time. The code below illustrates how to aggregate the metrics by month in order to compute the median count of likes/comments/shares per post: for example, in November 2013 the average post received around 40,000 likes.</p>

<pre><code class="r">## convert Facebook date format to R date format
format.facebook.date &lt;- function(datestring) {
    date &lt;- as.POSIXct(datestring, format = &quot;%Y-%m-%dT%H:%M:%S+0000&quot;, tz = &quot;GMT&quot;)
}
## aggregate metric counts over month
aggregate.metric &lt;- function(metric) {
    m &lt;- aggregate(page[[paste0(metric, &quot;_count&quot;)]], list(month = page$month), 
        mean)
    m$month &lt;- as.Date(paste0(m$month, &quot;-15&quot;))
    m$metric &lt;- metric
    return(m)
}
# create data frame with average metric counts per month
page$datetime &lt;- format.facebook.date(page$created_time)
page$month &lt;- format(page$datetime, &quot;%Y-%m&quot;)
df.list &lt;- lapply(c(&quot;likes&quot;, &quot;comments&quot;, &quot;shares&quot;), aggregate.metric)
df &lt;- do.call(rbind, df.list)
# visualize evolution in metric
library(ggplot2)
library(scales)
ggplot(df, aes(x = month, y = x, group = metric)) + geom_line(aes(color = metric)) + 
    scale_x_date(breaks = &quot;years&quot;, labels = date_format(&quot;%Y&quot;)) + scale_y_log10(&quot;Average count per post&quot;, 
    breaks = c(10, 100, 1000, 10000, 50000)) + theme_bw() + theme(axis.title.x = element_blank())
</code></pre>

<center><img src="images/humans.png" style="width: 700px;"/></center>

<p>To retrieve more information about each individual post, you can use the <code>getPost</code> function, which will return the same variables as above, as well as a list of comments and likes. Continuing with my example, the code below shows how to collect a list of 1,000 users who liked the most recent post, for which we will also gather information in order to analyze the audience of this page in terms of gender, language, and country.</p>

<pre><code class="r">post_id &lt;- head(page$id, n = 1)  ## ID of most recent post
post &lt;- getPost(post_id, token, n = 1000, likes = TRUE, comments = FALSE)
users &lt;- getUsers(post$likes$from_id, token)
</code></pre>

<pre><code>## 500 users -- 1000 users --
</code></pre>

<pre><code class="r">table(users$gender)  # gender
</code></pre>

<pre><code>## female   male 
##    784    209
</code></pre>

<pre><code class="r">table(substr(users$locale, 4, 5))  # country
</code></pre>

<pre><code>##  AZ  BE  BG  BR  CA  CZ  DE  DK  EE  ES  FR  GB  GR  HR  HU  IL  IR  IT 
##   1   1   1   8   1   2   8   8   1   4   5 141   1   3   1   1   1   9 
##  LA  LT  NL  PI  PL  PT  RO  RS  RU  SE  SI  SK  TH  US  VA 
##   8   3   5   5   5   2   1   1   8   2   1   1   1 758   1
</code></pre>

<pre><code class="r">table(substr(users$locale, 1, 2))  # language
</code></pre>

<pre><code>##  az  bg  cs  da  de  el  en  es  et  fa  fr  he  hr  hu  it  la  lt  nl 
##   1   1   2   8   8   1 904  12   1   1   6   1   3   1   9   1   3   6 
##  pl  pt  ro  ru  sk  sl  sr  sv  th 
##   5  10   1   8   1   1   1   2   1
</code></pre>

<h3>Updating your Facebook status from R</h3>

<p>Finally, yes:</p>

<pre><code class="r">updateStatus(&quot;You can also update your Facebook status from R&quot;, token)
</code></pre>

<pre><code>## Success! Link to status update:
## http://www.facebook.com/557698085_10152090718708086
</code></pre>

<p>However, to make this work you will need to get a <a href="http://thinktostart.wordpress.com/2013/11/19/analyzing-facebook-with-r/">long-lived OAuth token</a> first, setting <code>private_info=TRUE</code> in the <code>fbOAUth</code> function.</p>

<h3>Concluding...</h3>

<p>I hope this package is useful for R users interested in working with social media data. Future releases of the package will include additional function to cover other aspects of the API. My plan is to keep the <a href='https://github.com/pablobarbera/Rfacebook'>GitHub version</a> up to date fixing any possible bugs, and release only major versions to CRAN.</p>

<p>You can contact me at pablo.barbera[at]nyu.edu or via twitter (<a href="http://www.twitter.com/p_barbera">@p_barbera</a>) for any question or suggestion you might have, or to report any bugs in the code.</p>


<script type="text/javascript">

  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', 'UA-1191254-10']);
  _gaq.push(['_trackPageview']);

  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();

</script>

