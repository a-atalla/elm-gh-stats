parcelRequire=function(e,r,t,n){var i,o="function"==typeof parcelRequire&&parcelRequire,u="function"==typeof require&&require;function f(t,n){if(!r[t]){if(!e[t]){var i="function"==typeof parcelRequire&&parcelRequire;if(!n&&i)return i(t,!0);if(o)return o(t,!0);if(u&&"string"==typeof t)return u(t);var c=new Error("Cannot find module '"+t+"'");throw c.code="MODULE_NOT_FOUND",c}p.resolve=function(r){return e[t][1][r]||r},p.cache={};var l=r[t]=new f.Module(t);e[t][0].call(l.exports,p,l,l.exports,this)}return r[t].exports;function p(e){return f(p.resolve(e))}}f.isParcelRequire=!0,f.Module=function(e){this.id=e,this.bundle=f,this.exports={}},f.modules=e,f.cache=r,f.parent=o,f.register=function(r,t){e[r]=[function(e,r){r.exports=t},{}]};for(var c=0;c<t.length;c++)try{f(t[c])}catch(e){i||(i=e)}if(t.length){var l=f(t[t.length-1]);"object"==typeof exports&&"undefined"!=typeof module?module.exports=l:"function"==typeof define&&define.amd?define(function(){return l}):n&&(this[n]=l)}if(parcelRequire=f,i)throw i;return f}({"XucH":[function(require,module,exports) {

},{"./files/cairo-latin-200.woff2":[["cairo-latin-200.401cedb1.woff2","nYol"],"nYol"],"./files/cairo-latin-200.woff":[["cairo-latin-200.8b7cb22f.woff","kcDW"],"kcDW"],"./files/cairo-latin-300.woff2":[["cairo-latin-300.3d89733b.woff2","TS9u"],"TS9u"],"./files/cairo-latin-300.woff":[["cairo-latin-300.de128c68.woff","L2EN"],"L2EN"],"./files/cairo-latin-400.woff2":[["cairo-latin-400.abda8d67.woff2","LpKb"],"LpKb"],"./files/cairo-latin-400.woff":[["cairo-latin-400.dec1bdc5.woff","f0uB"],"f0uB"],"./files/cairo-latin-600.woff2":[["cairo-latin-600.d1adae8b.woff2","C3lI"],"C3lI"],"./files/cairo-latin-600.woff":[["cairo-latin-600.968c26ae.woff","IsUz"],"IsUz"],"./files/cairo-latin-700.woff2":[["cairo-latin-700.056e6796.woff2","oh76"],"oh76"],"./files/cairo-latin-700.woff":[["cairo-latin-700.6b07cdf7.woff","uDrQ"],"uDrQ"],"./files/cairo-latin-900.woff2":[["cairo-latin-900.f5ed1d70.woff2","xybp"],"xybp"],"./files/cairo-latin-900.woff":[["cairo-latin-900.85b8fa69.woff","TU1a"],"TU1a"]}],"G7yG":[function(require,module,exports) {

},{}],"asWa":[function(require,module,exports) {
!function(n){"use strict";function r(n,r,t){return t.a=n,t.f=r,t}function t(n){return r(2,n,function(r){return function(t){return n(r,t)}})}function e(n){return r(3,n,function(r){return function(t){return function(e){return n(r,t,e)}}})}function u(n){return r(4,n,function(r){return function(t){return function(e){return function(u){return n(r,t,e,u)}}}})}function a(n){return r(5,n,function(r){return function(t){return function(e){return function(u){return function(a){return n(r,t,e,u,a)}}}}})}function i(n){return r(7,n,function(r){return function(t){return function(e){return function(u){return function(a){return function(i){return function(o){return n(r,t,e,u,a,i,o)}}}}}}})}function o(n,r,t){return 2===n.a?n.f(r,t):n(r)(t)}function f(n,r,t,e){return 3===n.a?n.f(r,t,e):n(r)(t)(e)}function c(n,r,t,e,u){return 4===n.a?n.f(r,t,e,u):n(r)(t)(e)(u)}function v(n,r,t,e,u,a){return 5===n.a?n.f(r,t,e,u,a):n(r)(t)(e)(u)(a)}function s(n,r,t,e,u,a,i,o){return 7===n.a?n.f(r,t,e,u,a,i,o):n(r)(t)(e)(u)(a)(i)(o)}function d(n,r){for(var t,e=[],u=b(n,r,0,e);u&&(t=e.pop());u=b(t.a,t.b,0,e));return u}function b(n,r,t,e){if(n===r)return!0;if("object"!=typeof n||null===n||null===r)return"function"==typeof n&&N(5),!1;if(t>100)return e.push($(n,r)),!0;for(var u in 0>n.$&&(n=mr(n),r=mr(r)),n)if(!b(n[u],r[u],t+1,e))return!1;return!0}function l(n,r,t){if("object"!=typeof n)return n===r?0:r>n?-1:1;if(void 0===n.$)return(t=l(n.a,r.a))?t:(t=l(n.b,r.b))?t:l(n.c,r.c);for(;n.b&&r.b&&!(t=l(n.a,r.a));n=n.b,r=r.b);return t||(n.b?1:r.b?-1:0)}var h=t(function(n,r){var t=l(n,r);return 0>t?$r:t?hr:lr});function $(n,r){return{a:n,b:r}}function g(n){return n}function p(n,r){var t={};for(var e in n)t[e]=n[e];for(var e in r)t[e]=r[e];return t}function m(n,r){if("string"==typeof n)return n+r;if(!n.b)return r;var t=y(n.a,r);n=n.b;for(var e=t;n.b;n=n.b)e=e.b=y(n.a,r);return t}var w={$:0};function y(n,r){return{$:1,a:n,b:r}}var A=t(y);function j(n){for(var r=w,t=n.length;t--;)r=y(n[t],r);return r}var C=e(function(n,r,t){for(var e=[];r.b&&t.b;r=r.b,t=t.b)e.push(o(n,r.a,t.a));return j(e)}),_=e(function(n,r,t){for(var e=[],u=0;n>u;u++)e[u]=t(r+u);return e}),k=t(function(n,r){for(var t=[],e=0;n>e&&r.b;e++)t[e]=r.a,r=r.b;return t.length=e,$(t,r)});function N(n){throw Error("https://github.com/elm/core/blob/1.0.0/hints/"+n+".md")}var E=Math.ceil,O=Math.floor,J=Math.log,L=isNaN,F=t(function(n,r){return n+r}),T=e(function(n,r,t){for(var e=t.length;e--;){var u=t[e],a=t.charCodeAt(e);56320>a||a>57343||(u=t[--e]+u),r=o(n,g(u),r)}return r}),R=t(function(n,r){return r.split(n)}),S=t(function(n,r){return r.join(n)}),q=e(function(n,r,t){return t.slice(n,r)}),x=t(function(n,r){for(var t=r.length;t--;){var e=r[t],u=r.charCodeAt(t);if(56320>u||u>57343||(e=r[--t]+e),!n(g(e)))return!1}return!0}),H=t(function(n,r){return 0===r.indexOf(n)});function z(n){return n+""}function I(n){return{$:2,b:n}}var M=I(function(n){return"number"!=typeof n?nn("an INT",n):n>-2147483647&&2147483647>n&&(0|n)===n?Cr(n):!isFinite(n)||n%1?nn("an INT",n):Cr(n)}),P=(I(function(n){return"boolean"==typeof n?Cr(n):nn("a BOOL",n)}),I(function(n){return"number"==typeof n?Cr(n):nn("a FLOAT",n)})),Y=(I(function(n){return Cr(un(n))}),I(function(n){return"string"==typeof n?Cr(n):n instanceof String?Cr(n+""):nn("a STRING",n)})),B=t(function(n,r){return{$:6,d:n,b:r}});function Z(n,r){return{$:9,f:n,g:r}}var D=t(function(n,r){return Z(n,[r])}),G=e(function(n,r,t){return Z(n,[r,t])}),U=t(function(n,r){try{return X(n,JSON.parse(r))}catch(n){return wr(o(yr,"This is not valid JSON! "+n.message,un(r)))}}),V=t(function(n,r){return X(n,an(r))});function X(n,r){switch(n.$){case 2:return n.b(r);case 5:return null===r?Cr(n.c):nn("null",r);case 3:return K(r)?W(n.b,r,j):nn("a LIST",r);case 4:return K(r)?W(n.b,r,Q):nn("an ARRAY",r);case 6:var t=n.d;if("object"!=typeof r||null===r||!(t in r))return nn("an OBJECT with a field named `"+t+"`",r);var e=X(n.b,r[t]);return bt(e)?e:wr(o(Ar,t,e.a));case 7:var u=n.e;return K(r)?r.length>u?(e=X(n.b,r[u]),bt(e)?e:wr(o(jr,u,e.a))):nn("a LONGER array. Need index "+u+" but only see "+r.length+" entries",r):nn("an ARRAY",r);case 8:if("object"!=typeof r||null===r||K(r))return nn("an OBJECT",r);var a=w;for(var i in r)if(r.hasOwnProperty(i)){if(e=X(n.b,r[i]),!bt(e))return wr(o(Ar,i,e.a));a=y($(i,e.a),a)}return Cr(Zr(a));case 9:for(var f=n.f,c=n.g,v=0;c.length>v;v++){if(e=X(c[v],r),!bt(e))return e;f=f(e.a)}return Cr(f);case 10:return e=X(n.b,r),bt(e)?X(n.h(e.a),r):e;case 11:for(var s=w,d=n.g;d.b;d=d.b){if(e=X(d.a,r),bt(e))return e;s=y(e.a,s)}return wr(_r(Zr(s)));case 1:return wr(o(yr,n.a,un(r)));case 0:return Cr(n.a)}}function W(n,r,t){for(var e=r.length,u=[],a=0;e>a;a++){var i=X(n,r[a]);if(!bt(i))return wr(o(jr,a,i.a));u[a]=i.a}return Cr(t(u))}function K(n){return Array.isArray(n)||"undefined"!=typeof FileList&&n instanceof FileList}function Q(n){return o(dt,n.length,function(r){return n[r]})}function nn(n,r){return wr(o(yr,"Expecting "+n,un(r)))}function rn(n,r){if(n===r)return!0;if(n.$!==r.$)return!1;switch(n.$){case 0:case 1:return n.a===r.a;case 2:return n.b===r.b;case 5:return n.c===r.c;case 3:case 4:case 8:return rn(n.b,r.b);case 6:return n.d===r.d&&rn(n.b,r.b);case 7:return n.e===r.e&&rn(n.b,r.b);case 9:return n.f===r.f&&tn(n.g,r.g);case 10:return n.h===r.h&&rn(n.b,r.b);case 11:return tn(n.g,r.g)}}function tn(n,r){var t=n.length;if(t!==r.length)return!1;for(var e=0;t>e;e++)if(!rn(n[e],r[e]))return!1;return!0}var en=t(function(n,r){return JSON.stringify(an(r),null,n)+""});function un(n){return n}function an(n){return n}function on(n){return{$:0,a:n}}function fn(n){return{$:2,b:n,c:null}}un(null);var cn=t(function(n,r){return{$:3,b:n,d:r}}),vn=0;function sn(n){var r={$:0,e:vn++,f:n,g:null,h:[]};return gn(r),r}function dn(n){return fn(function(r){r(on(sn(n)))})}function bn(n,r){n.h.push(r),gn(n)}var ln=t(function(n,r){return fn(function(t){bn(n,r),t(on(0))})}),hn=!1,$n=[];function gn(n){if($n.push(n),!hn){for(hn=!0;n=$n.shift();)pn(n);hn=!1}}function pn(n){for(;n.f;){var r=n.f.$;if(0===r||1===r){for(;n.g&&n.g.$!==r;)n.g=n.g.i;if(!n.g)return;n.f=n.g.b(n.f.a),n.g=n.g.i}else{if(2===r)return void(n.f.c=n.f.b(function(r){n.f=r,gn(n)}));if(5===r){if(0===n.h.length)return;n.f=n.f.b(n.h.shift())}else n.g={$:3===r?0:1,b:n.f.b,i:n.g},n.f=n.f.d}}}var mn={};function wn(n,r,t,e,u){return{b:n,c:r,d:t,e:e,f:u}}function yn(n,r){var t={g:r,h:void 0},e=n.c,u=n.d,a=n.e,i=n.f;function v(n){return o(cn,v,{$:5,b:function(r){var o=r.a;return 0===r.$?f(u,t,o,n):a&&i?c(e,t,o.i,o.j,n):f(e,t,a?o.i:o.j,n)}})}return t.h=sn(o(cn,v,n.b))}var An=t(function(n,r){return fn(function(t){n.g(r),t(on(0))})}),jn=t(function(n,r){return o(ln,n.h,{$:0,a:r})});function Cn(n){return function(r){return{$:1,k:n,l:r}}}function _n(n){return{$:2,m:n}}var kn,Nn=[],En=!1;function On(n,r,t){if(Nn.push({p:n,q:r,r:t}),!En){En=!0;for(var e;e=Nn.shift();)Jn(e.p,e.q,e.r);En=!1}}function Jn(n,r,t){var e={};for(var u in Ln(!0,r,e,null),Ln(!1,t,e,null),n)bn(n[u],{$:"fx",a:e[u]||{i:w,j:w}})}function Ln(n,r,t,e){switch(r.$){case 1:var u=r.k,a=function(n,t,e){function u(n){for(var r=e;r;r=r.t)n=r.s(n);return n}return o(n?mn[t].e:mn[t].f,u,r.l)}(n,u,e);return void(t[u]=function(n,r,t){return t=t||{i:w,j:w},n?t.i=y(r,t.i):t.j=y(r,t.j),t}(n,a,t[u]));case 2:for(var i=r.m;i.b;i=i.b)Ln(n,i.a,t,e);return;case 3:return void Ln(n,r.o,t,{s:r.n,t:e})}}var Fn="undefined"!=typeof document?document:{};function Tn(n,r){n.appendChild(r)}function Rn(n){return{$:0,a:n}}var Sn=t(function(n,r){return t(function(t,e){for(var u=[],a=0;e.b;e=e.b){var i=e.a;a+=i.b||0,u.push(i)}return a+=u.length,{$:1,c:r,d:In(t),e:u,f:n,b:a}})})(void 0);t(function(n,r){return t(function(t,e){for(var u=[],a=0;e.b;e=e.b){var i=e.a;a+=i.b.b||0,u.push(i)}return a+=u.length,{$:2,c:r,d:In(t),e:u,f:n,b:a}})})(void 0);var qn,xn=t(function(n,r){return{$:"a0",n:n,o:r}}),Hn=t(function(n,r){return{$:"a2",n:n,o:r}}),zn=t(function(n,r){return{$:"a3",n:n,o:r}});function In(n){for(var r={};n.b;n=n.b){var t=n.a,e=t.$,u=t.n,a=t.o;if("a2"!==e){var i=r[e]||(r[e]={});"a3"===e&&"class"===u?Mn(i,u,a):i[u]=a}else"className"===u?Mn(r,u,an(a)):r[u]=an(a)}return r}function Mn(n,r,t){var e=n[r];n[r]=e?e+" "+t:t}function Pn(n,r){var t=n.$;if(5===t)return Pn(n.k||(n.k=n.m()),r);if(0===t)return Fn.createTextNode(n.a);if(4===t){for(var e=n.k,u=n.j;4===e.$;)"object"!=typeof u?u=[u,e.j]:u.push(e.j),e=e.k;var a={j:u,p:r};return(i=Pn(e,a)).elm_event_node_ref=a,i}if(3===t)return Yn(i=n.h(n.g),r,n.d),i;var i=n.f?Fn.createElementNS(n.f,n.c):Fn.createElement(n.c);kn&&"a"==n.c&&i.addEventListener("click",kn(i)),Yn(i,r,n.d);for(var o=n.e,f=0;o.length>f;f++)Tn(i,Pn(1===t?o[f]:o[f].b,r));return i}function Yn(n,r,t){for(var e in t){var u=t[e];"a1"===e?Bn(n,u):"a0"===e?Gn(n,r,u):"a3"===e?Zn(n,u):"a4"===e?Dn(n,u):("value"!==e&&"checked"!==e||n[e]!==u)&&(n[e]=u)}}function Bn(n,r){var t=n.style;for(var e in r)t[e]=r[e]}function Zn(n,r){for(var t in r){var e=r[t];void 0!==e?n.setAttribute(t,e):n.removeAttribute(t)}}function Dn(n,r){for(var t in r){var e=r[t],u=e.f,a=e.o;void 0!==a?n.setAttributeNS(u,t,a):n.removeAttributeNS(u,t)}}function Gn(n,r,t){var e=n.elmFs||(n.elmFs={});for(var u in t){var a=t[u],i=e[u];if(a){if(i){if(i.q.$===a.$){i.q=a;continue}n.removeEventListener(u,i)}i=Un(r,a),n.addEventListener(u,i,qn&&{passive:2>gt(a)}),e[u]=i}else n.removeEventListener(u,i),e[u]=void 0}}try{window.addEventListener("t",null,Object.defineProperty({},"passive",{get:function(){qn=!0}}))}catch(n){}function Un(n,r){function t(r){var e=t.q,u=X(e.a,r);if(bt(u)){for(var a,i=gt(e),o=u.a,f=i?3>i?o.a:o.o:o,c=1==i?o.b:3==i&&o.P,v=(c&&r.stopPropagation(),(2==i?o.b:3==i&&o.M)&&r.preventDefault(),n);a=v.j;){if("function"==typeof a)f=a(f);else for(var s=a.length;s--;)f=a[s](f);v=v.p}v(f,c)}}return t.q=r,t}function Vn(n,r){return n.$==r.$&&rn(n.a,r.a)}function Xn(n,r,t,e){var u={$:r,r:t,s:e,t:void 0,u:void 0};return n.push(u),u}function Wn(n,r,t,e){if(n!==r){var u=n.$,a=r.$;if(u!==a){if(1!==u||2!==a)return void Xn(t,0,e,r);r=function(n){for(var r=n.e,t=r.length,e=[],u=0;t>u;u++)e[u]=r[u].b;return{$:1,c:n.c,d:n.d,e:e,f:n.f,b:n.b}}(r),a=1}switch(a){case 5:for(var i=n.l,o=r.l,f=i.length,c=f===o.length;c&&f--;)c=i[f]===o[f];if(c)return void(r.k=n.k);r.k=r.m();var v=[];return Wn(n.k,r.k,v,0),void(v.length>0&&Xn(t,1,e,v));case 4:for(var s=n.j,d=r.j,b=!1,l=n.k;4===l.$;)b=!0,"object"!=typeof s?s=[s,l.j]:s.push(l.j),l=l.k;for(var h=r.k;4===h.$;)b=!0,"object"!=typeof d?d=[d,h.j]:d.push(h.j),h=h.k;return b&&s.length!==d.length?void Xn(t,0,e,r):((b?function(n,r){for(var t=0;n.length>t;t++)if(n[t]!==r[t])return!1;return!0}(s,d):s===d)||Xn(t,2,e,d),void Wn(l,h,t,e+1));case 0:return void(n.a!==r.a&&Xn(t,3,e,r.a));case 1:return void Kn(n,r,t,e,nr);case 2:return void Kn(n,r,t,e,rr);case 3:if(n.h!==r.h)return void Xn(t,0,e,r);var $=Qn(n.d,r.d);$&&Xn(t,4,e,$);var g=r.i(n.g,r.g);return void(g&&Xn(t,5,e,g))}}}function Kn(n,r,t,e,u){if(n.c===r.c&&n.f===r.f){var a=Qn(n.d,r.d);a&&Xn(t,4,e,a),u(n,r,t,e)}else Xn(t,0,e,r)}function Qn(n,r,t){var e;for(var u in n)if("a1"!==u&&"a0"!==u&&"a3"!==u&&"a4"!==u)if(u in r){var a=n[u],i=r[u];a===i&&"value"!==u&&"checked"!==u||"a0"===t&&Vn(a,i)||((e=e||{})[u]=i)}else(e=e||{})[u]=t?"a1"===t?"":"a0"===t||"a3"===t?void 0:{f:n[u].f,o:void 0}:"string"==typeof n[u]?"":null;else{var o=Qn(n[u],r[u]||{},u);o&&((e=e||{})[u]=o)}for(var f in r)f in n||((e=e||{})[f]=r[f]);return e}function nr(n,r,t,e){var u=n.e,a=r.e,i=u.length,o=a.length;i>o?Xn(t,6,e,{v:o,i:i-o}):o>i&&Xn(t,7,e,{v:i,e:a});for(var f=o>i?i:o,c=0;f>c;c++){var v=u[c];Wn(v,a[c],t,++e),e+=v.b||0}}function rr(n,r,t,e){for(var u=[],a={},i=[],o=n.e,f=r.e,c=o.length,v=f.length,s=0,d=0,b=e;c>s&&v>d;){var l=(k=o[s]).a,h=(N=f[d]).a,$=k.b,g=N.b,p=void 0,m=void 0;if(l!==h){var w=o[s+1],y=f[d+1];if(w){var A=w.a,j=w.b;m=h===A}if(y){var C=y.a,_=y.b;p=l===C}if(p&&m)Wn($,_,u,++b),er(a,u,l,g,d,i),b+=$.b||0,ur(a,u,l,j,++b),b+=j.b||0,s+=2,d+=2;else if(p)b++,er(a,u,h,g,d,i),Wn($,_,u,b),b+=$.b||0,s+=1,d+=2;else if(m)ur(a,u,l,$,++b),b+=$.b||0,Wn(j,g,u,++b),b+=j.b||0,s+=2,d+=1;else{if(!w||A!==C)break;ur(a,u,l,$,++b),er(a,u,h,g,d,i),b+=$.b||0,Wn(j,_,u,++b),b+=j.b||0,s+=2,d+=2}}else Wn($,g,u,++b),b+=$.b||0,s++,d++}for(;c>s;){var k;ur(a,u,(k=o[s]).a,$=k.b,++b),b+=$.b||0,s++}for(;v>d;){var N,E=E||[];er(a,u,(N=f[d]).a,N.b,void 0,E),d++}(u.length>0||i.length>0||E)&&Xn(t,8,e,{w:u,x:i,y:E})}var tr="_elmW6BL";function er(n,r,t,e,u,a){var i=n[t];if(!i)return a.push({r:u,A:i={c:0,z:e,r:u,s:void 0}}),void(n[t]=i);if(1===i.c){a.push({r:u,A:i}),i.c=2;var o=[];return Wn(i.z,e,o,i.r),i.r=u,void(i.s.s={w:o,A:i})}er(n,r,t+tr,e,u,a)}function ur(n,r,t,e,u){var a=n[t];if(a){if(0===a.c){a.c=2;var i=[];return Wn(e,a.z,i,u),void Xn(r,9,u,{w:i,A:a})}ur(n,r,t+tr,e,u)}else{var o=Xn(r,9,u,void 0);n[t]={c:1,z:e,r:u,s:o}}}function ar(n,r,t,e){return 0===t.length?n:(function n(r,t,e,u){!function r(t,e,u,a,i,o,f){for(var c=u[a],v=c.r;v===i;){var s=c.$;if(1===s)n(t,e.k,c.s,f);else if(8===s)c.t=t,c.u=f,(d=c.s.w).length>0&&r(t,e,d,0,i,o,f);else if(9===s){c.t=t,c.u=f;var d,b=c.s;b&&(b.A.s=t,(d=b.w).length>0&&r(t,e,d,0,i,o,f))}else c.t=t,c.u=f;if(!(c=u[++a])||(v=c.r)>o)return a}var l=e.$;if(4===l){for(var h=e.k;4===h.$;)h=h.k;return r(t,h,u,a,i+1,o,t.elm_event_node_ref)}for(var $=e.e,g=t.childNodes,p=0;$.length>p;p++){var m=1===l?$[p]:$[p].b,w=++i+(m.b||0);if(!(i>v||v>w||(c=u[a=r(g[p],m,u,a,i,w,f)])&&(v=c.r)<=o))return a;i=w}return a}(r,t,e,0,0,t.b,u)}(n,r,t,e),ir(n,t))}function ir(n,r){for(var t=0;r.length>t;t++){var e=r[t],u=e.t,a=or(u,e);u===n&&(n=a)}return n}function or(n,r){switch(r.$){case 0:return function(n){var t=n.parentNode,e=Pn(r.s,r.u);return e.elm_event_node_ref||(e.elm_event_node_ref=n.elm_event_node_ref),t&&e!==n&&t.replaceChild(e,n),e}(n);case 4:return Yn(n,r.u,r.s),n;case 3:return n.replaceData(0,n.length,r.s),n;case 1:return ir(n,r.s);case 2:return n.elm_event_node_ref?n.elm_event_node_ref.j=r.s:n.elm_event_node_ref={j:r.s,p:r.u},n;case 6:for(var t=r.s,e=0;t.i>e;e++)n.removeChild(n.childNodes[t.v]);return n;case 7:for(var u=(t=r.s).e,a=n.childNodes[e=t.v];u.length>e;e++)n.insertBefore(Pn(u[e],r.u),a);return n;case 9:if(!(t=r.s))return n.parentNode.removeChild(n),n;var i=t.A;return void 0!==i.r&&n.parentNode.removeChild(n),i.s=ir(n,t.w),n;case 8:return function(n,r){var t=r.s,e=function(n,r){if(n){for(var t=Fn.createDocumentFragment(),e=0;n.length>e;e++){var u=n[e].A;Tn(t,2===u.c?u.s:Pn(u.z,r.u))}return t}}(t.y,r);n=ir(n,t.w);for(var u=t.x,a=0;u.length>a;a++){var i=u[a],o=i.A,f=2===o.c?o.s:Pn(o.z,r.u);n.insertBefore(f,n.childNodes[i.r])}return e&&Tn(n,e),n}(n,r);case 5:return r.s(n);default:N(10)}}var fr=u(function(n,r,t,e){return function(n,r,t,e,u,a){var i=o(V,n,un(r?r.flags:void 0));bt(i)||N(2);var f={},c=t(i.a),v=c.a,s=a(b,v),d=function(n,r){var t;for(var e in mn){var u=mn[e];u.a&&((t=t||{})[e]=u.a(e,r)),n[e]=yn(u,r)}return t}(f,b);function b(n,r){var t=o(e,n,v);s(v=t.a,r),On(f,t.b,u(v))}return On(f,c.b,u(v)),d?{ports:d}:{}}(r,e,n.aN,n.a_,n.aX,function(r,t){var u=n.a$,a=e.node,i=function n(r){if(3===r.nodeType)return Rn(r.textContent);if(1!==r.nodeType)return Rn("");for(var t=w,e=r.attributes,u=e.length;u--;){var a=e[u];t=y(o(zn,a.name,a.value),t)}var i=r.tagName.toLowerCase(),c=w,v=r.childNodes;for(u=v.length;u--;)c=y(n(v[u]),c);return f(Sn,i,t,c)}(a);return function(n,r){r(n);var t=0;function e(){t=1===t?0:(cr(e),r(n),1)}return function(u,a){n=u,a?(r(n),2===t&&(t=1)):(0===t&&cr(e),t=2)}}(t,function(n){var t=u(n),e=function(n,r){var t=[];return Wn(n,r,t,0),t}(i,t);a=ar(a,i,e,r),i=t})})}),cr=("undefined"!=typeof cancelAnimationFrame&&cancelAnimationFrame,"undefined"!=typeof requestAnimationFrame?requestAnimationFrame:function(n){return setTimeout(n,1e3/60)});"undefined"!=typeof document&&document,"undefined"!=typeof window&&window;var vr=e(function(n,r,t){return fn(function(e){function u(n){e(r(t.aJ.a(n)))}var a=new XMLHttpRequest;a.addEventListener("error",function(){u(Yt)}),a.addEventListener("timeout",function(){u(Dt)}),a.addEventListener("load",function(){u(function(n,r){return o(r.status>=200&&300>r.status?Pt:It,function(n){return{aw:n.responseURL,aV:n.status,aW:n.statusText,Z:sr(n.getAllResponseHeaders())}}(r),n(r.response))}(t.aJ.b,a))}),Vt(t.av)&&function(n,r,t){r.upload.addEventListener("progress",function(e){r.c||sn(o(Xt,n,$(t,Zt({aU:e.loaded,as:e.total}))))}),r.addEventListener("progress",function(e){r.c||sn(o(Xt,n,$(t,Bt({aS:e.loaded,as:e.lengthComputable?kr(e.total):Nr}))))})}(n,a,t.av.a);try{a.open(t.aO,t.aw,!0)}catch(n){return u(Mt(t.aw))}return function(n,r){for(var t=r.Z;t.b;t=t.b)n.setRequestHeader(t.a.a,t.a.b);n.timeout=r.aY.a||0,n.responseType=r.aJ.d,n.withCredentials=r.aC}(a,t),t.aF.a&&a.setRequestHeader("Content-Type",t.aF.a),a.send(t.aF.b),function(){a.c=!0,a.abort()}})});function sr(n){if(!n)return Ut;for(var r=Ut,t=n.split("\r\n"),e=t.length;e--;){var u=t[e],a=u.indexOf(": ");if(a>0){var i=u.substring(0,a),o=u.substring(a+2);r=f(ve,i,function(n){return kr(Vt(n)?o+", "+n.a:o)},r)}}return r}var dr=e(function(n,r,t){return{$:0,d:n,b:r,a:t}}),br=t(function(n,r){return{$:0,d:r.d,b:r.b,a:function(t){return n(r.a(t))}}}),lr=1,hr=2,$r=0,gr=A,pr=e(function(n,r,t){for(;;){if(-2===t.$)return r;var e=t.d,u=n,a=f(n,t.b,t.c,f(pr,n,r,t.e));n=u,r=a,t=e}}),mr=function(n){return f(pr,e(function(n,r,t){return o(gr,$(n,r),t)}),w,n)},wr=function(n){return{$:1,a:n}},yr=t(function(n,r){return{$:3,a:n,b:r}}),Ar=t(function(n,r){return{$:0,a:n,b:r}}),jr=t(function(n,r){return{$:1,a:n,b:r}}),Cr=function(n){return{$:0,a:n}},_r=function(n){return{$:2,a:n}},kr=function(n){return{$:0,a:n}},Nr={$:1},Er=x,Or=en,Jr=z,Lr=t(function(n,r){return o(S,n,function(n){for(var r=[];n.b;n=n.b)r.push(n.a);return r}(r))}),Fr=t(function(n,r){return j(o(R,n,r))}),Tr=function(n){return o(Lr,"\n    ",o(Fr,"\n",n))},Rr=e(function(n,r,t){for(;;){if(!t.b)return r;var e=t.b,u=n,a=o(n,t.a,r);n=u,r=a,t=e}}),Sr=function(n){return f(Rr,t(function(n,r){return r+1}),0,n)},qr=C,xr=e(function(n,r,t){for(;;){if(l(n,r)>=1)return t;var e=n,u=r-1,a=o(gr,r,t);n=e,r=u,t=a}}),Hr=t(function(n,r){return f(xr,n,r,w)}),zr=t(function(n,r){return f(qr,n,o(Hr,0,Sr(r)-1),r)}),Ir=function(n){var r=n.charCodeAt(0);return 55296>r||r>56319?r:1024*(r-55296)+n.charCodeAt(1)-56320+65536},Mr=function(n){var r=Ir(n);return r>=97&&122>=r},Pr=function(n){var r=Ir(n);return 90>=r&&r>=65},Yr=function(n){return Mr(n)||Pr(n)},Br=function(n){return Mr(n)||Pr(n)||function(n){var r=Ir(n);return 57>=r&&r>=48}(n)},Zr=function(n){return f(Rr,gr,w,n)},Dr=function(n){var r=n.charCodeAt(0);return isNaN(r)?Nr:kr(55296>r||r>56319?$(g(n[0]),n.slice(1)):$(g(n[0]+n[1]),n.slice(2)))},Gr=t(function(n,r){return"\n\n("+Jr(n+1)+") "+Tr(Ur(r))}),Ur=function(n){return o(Vr,n,w)},Vr=t(function(n,r){n:for(;;)switch(n.$){case 0:var t=n.a,e=n.b,u=function(){var n=Dr(t);if(1===n.$)return!1;var r=n.a,e=r.b;return Yr(r.a)&&o(Er,Br,e)}();n=e,r=o(gr,u?"."+t:"['"+t+"']",r);continue n;case 1:e=n.b;var a="["+Jr(n.a)+"]";n=e,r=o(gr,a,r);continue n;case 2:var i=n.a;if(i.b){if(i.b.b){var f=(r.b?"The Json.Decode.oneOf at json"+o(Lr,"",Zr(r)):"Json.Decode.oneOf")+" failed in the following "+Jr(Sr(i))+" ways:";return o(Lr,"\n\n",o(gr,f,o(zr,Gr,i)))}n=e=i.a,r=r;continue n}return"Ran into a Json.Decode.oneOf with no possibilities"+(r.b?" at json"+o(Lr,"",Zr(r)):"!");default:var c=n.a,v=n.b;return(f=r.b?"Problem with the value at json"+o(Lr,"",Zr(r))+":\n\n    ":"Problem with the given value:\n\n")+Tr(o(Or,4,v))+"\n\n"+c}}),Xr=u(function(n,r,t,e){return{$:0,a:n,b:r,c:t,d:e}}),Wr=[],Kr=E,Qr=t(function(n,r){return J(r)/J(n)}),nt=Kr(o(Qr,2,32)),rt=c(Xr,0,nt,Wr,Wr),tt=_,et=t(function(n,r){return r(n)}),ut=O,at=function(n){return n.length},it=t(function(n,r){return l(n,r)>0?n:r}),ot=k,ft=t(function(n,r){for(;;){var t=o(ot,32,n),e=t.b,u=o(gr,{$:0,a:t.a},r);if(!e.b)return Zr(u);n=e,r=u}}),ct=t(function(n,r){for(;;){var t=Kr(r/32);if(1===t)return o(ot,32,n).a;n=o(ft,n,w),r=t}}),vt=t(function(n,r){if(r.a){var t=32*r.a,e=ut(o(Qr,32,t-1)),u=n?Zr(r.d):r.d,a=o(ct,u,r.a);return c(Xr,at(r.c)+t,o(it,5,e*nt),a,r.c)}return c(Xr,at(r.c),nt,Wr,r.c)}),st=a(function(n,r,t,e,u){for(;;){if(0>r)return o(vt,!1,{d:e,a:t/32|0,c:u});var a={$:1,a:f(tt,32,r,n)};n=n,r-=32,t=t,e=o(gr,a,e),u=u}}),dt=t(function(n,r){if(n>0){var t=n%32;return v(st,r,n-t-32,n,w,f(tt,t,n-t,r))}return rt}),bt=function(n){return!n.$},lt=D,ht=G,$t=function(n){return{$:0,a:n}},gt=function(n){switch(n.$){case 0:return 0;case 1:return 1;case 2:return 2;default:return 3}},pt=function(n){return n},mt=function(n){return n.length},wt=q,yt=t(function(n,r){return 1>n?r:f(wt,n,mt(r),r)}),At=H,jt=on,Ct=jt(0),_t=u(function(n,r,t,e){if(e.b){var u=e.a,a=e.b;if(a.b){var i=a.a,v=a.b;if(v.b){var s=v.a,d=v.b;if(d.b){var b=d.b;return o(n,u,o(n,i,o(n,s,o(n,d.a,t>500?f(Rr,n,r,Zr(b)):c(_t,n,r,t+1,b)))))}return o(n,u,o(n,i,o(n,s,r)))}return o(n,u,o(n,i,r))}return o(n,u,r)}return r}),kt=e(function(n,r,t){return c(_t,n,r,0,t)}),Nt=t(function(n,r){return f(kt,t(function(r,t){return o(gr,n(r),t)}),w,r)}),Et=cn,Ot=t(function(n,r){return o(Et,function(r){return jt(n(r))},r)}),Jt=e(function(n,r,t){return o(Et,function(r){return o(Et,function(t){return jt(o(n,r,t))},t)},r)}),Lt=function(n){return f(kt,Jt(gr),jt(w),n)},Ft=An,Tt=t(function(n,r){var t=r;return dn(o(Et,Ft(n),t))});mn.Task=wn(Ct,e(function(n,r){return o(Ot,function(){return 0},Lt(o(Nt,Tt(n),r)))}),e(function(){return jt(0)}),t(function(n,r){return o(Ot,n,r)})),Cn("Task");var Rt=fr,St=_n(w),qt=_n(w),xt=function(n){switch(n.$){case 0:return n.a;case 1:return"Server is taking too long to respond. Please try again later.";case 2:return"Unable to reach server";case 3:return"Request Failed with status code: "+Jr(n.a);default:return n.a}},Ht=function(n){return{$:1,a:n}},zt=U,It=t(function(n,r){return{$:3,a:n,b:r}}),Mt=function(n){return{$:0,a:n}},Pt=t(function(n,r){return{$:4,a:n,b:r}}),Yt={$:2},Bt=function(n){return{$:1,a:n}},Zt=function(n){return{$:0,a:n}},Dt={$:1},Gt={$:-2},Ut=Gt,Vt=function(n){return!n.$},Xt=jn,Wt=h,Kt=t(function(n,r){n:for(;;){if(-2===r.$)return Nr;var t=r.c,e=r.d,u=r.e;switch(o(Wt,n,r.b)){case 0:n=n,r=e;continue n;case 1:return kr(t);default:n=n,r=u;continue n}}}),Qt=a(function(n,r,t,e,u){return{$:-1,a:n,b:r,c:t,d:e,e:u}}),ne=a(function(n,r,t,e,u){if(-1!==u.$||u.a){if(-1!==e.$||e.a||-1!==e.d.$||e.d.a)return v(Qt,n,r,t,e,u);var a=e.d;return i=e.e,v(Qt,0,e.b,e.c,v(Qt,1,a.b,a.c,a.d,a.e),v(Qt,1,r,t,i,u))}var i,o=u.b,f=u.c,c=u.d,s=u.e;return-1!==e.$||e.a?v(Qt,n,o,f,v(Qt,0,r,t,e,c),s):v(Qt,0,r,t,v(Qt,1,e.b,e.c,e.d,i=e.e),v(Qt,1,o,f,c,s))}),re=e(function(n,r,t){if(-2===t.$)return v(Qt,0,n,r,Gt,Gt);var e=t.a,u=t.b,a=t.c,i=t.d,c=t.e;switch(o(Wt,n,u)){case 0:return v(ne,e,u,a,f(re,n,r,i),c);case 1:return v(Qt,e,u,r,i,c);default:return v(ne,e,u,a,i,f(re,n,r,c))}}),te=e(function(n,r,t){var e=f(re,n,r,t);return-1!==e.$||e.a?e:v(Qt,1,e.b,e.c,e.d,e.e)}),ee=function(n){if(-1===n.$&&-1===n.d.$&&-1===n.e.$){if(-1!==n.e.d.$||n.e.d.a){var r=n.d,t=n.e;return i=t.b,o=t.c,e=t.d,s=t.e,v(Qt,1,n.b,n.c,v(Qt,0,r.b,r.c,r.d,r.e),v(Qt,0,i,o,e,s))}var e,u=n.d,a=n.e,i=a.b,o=a.c,f=(e=a.d).d,c=e.e,s=a.e;return v(Qt,0,e.b,e.c,v(Qt,1,n.b,n.c,v(Qt,0,u.b,u.c,u.d,u.e),f),v(Qt,1,i,o,c,s))}return n},ue=function(n){if(-1===n.$&&-1===n.d.$&&-1===n.e.$){if(-1!==n.d.d.$||n.d.d.a){var r=n.d,t=n.e;return c=t.b,s=t.c,d=t.d,b=t.e,v(Qt,1,e=n.b,u=n.c,v(Qt,0,r.b,r.c,r.d,o=r.e),v(Qt,0,c,s,d,b))}var e=n.b,u=n.c,a=n.d,i=a.d,o=a.e,f=n.e,c=f.b,s=f.c,d=f.d,b=f.e;return v(Qt,0,a.b,a.c,v(Qt,1,i.b,i.c,i.d,i.e),v(Qt,1,e,u,o,v(Qt,0,c,s,d,b)))}return n},ae=i(function(n,r,t,e,u,a,i){if(-1!==a.$||a.a){n:for(;;){if(-1===i.$&&1===i.a){if(-1===i.d.$){if(1===i.d.a)return ue(r);break n}return ue(r)}break n}return r}return v(Qt,t,a.b,a.c,a.d,v(Qt,0,e,u,a.e,i))}),ie=function(n){if(-1===n.$&&-1===n.d.$){var r=n.a,t=n.b,e=n.c,u=n.d,a=u.d,i=n.e;if(1===u.a){if(-1!==a.$||a.a){var o=ee(n);if(-1===o.$){var f=o.e;return v(ne,o.a,o.b,o.c,ie(o.d),f)}return Gt}return v(Qt,r,t,e,ie(u),i)}return v(Qt,r,t,e,ie(u),i)}return Gt},oe=t(function(n,r){if(-2===r.$)return Gt;var t=r.a,e=r.b,u=r.c,a=r.d,i=r.e;if(0>l(n,e)){if(-1===a.$&&1===a.a){var f=a.d;if(-1!==f.$||f.a){var c=ee(r);if(-1===c.$){var d=c.e;return v(ne,c.a,c.b,c.c,o(oe,n,c.d),d)}return Gt}return v(Qt,t,e,u,o(oe,n,a),i)}return v(Qt,t,e,u,o(oe,n,a),i)}return o(fe,n,s(ae,n,r,t,e,u,a,i))}),fe=t(function(n,r){if(-1===r.$){var t=r.a,e=r.b,u=r.c,a=r.d,i=r.e;if(d(n,e)){var f=function(n){for(;;){if(-1!==n.$||-1!==n.d.$)return n;n=n.d}}(i);return-1===f.$?v(ne,t,f.b,f.c,a,ie(i)):Gt}return v(ne,t,e,u,a,o(oe,n,i))}return Gt}),ce=t(function(n,r){var t=o(oe,n,r);return-1!==t.$||t.a?t:v(Qt,1,t.b,t.c,t.d,t.e)}),ve=e(function(n,r,t){var e=r(o(Kt,n,t));return e.$?o(ce,n,t):f(te,n,e.a,t)}),se=e(function(n,r,t){return r(n(t))}),de=t(function(n,r){return f(dr,"",pt,o(se,r,n))}),be=t(function(n,r){return r.$?wr(n(r.a)):Cr(r.a)}),le=function(n){return{$:4,a:n}},he={$:2},$e={$:1},ge=t(function(n,r){switch(r.$){case 0:return wr({$:0,a:r.a});case 1:return wr($e);case 2:return wr(he);case 3:return wr({$:3,a:r.a.aV});default:return o(be,le,n(r.b))}}),pe=t(function(n,r){return o(de,n,ge(function(n){return o(be,Ur,o(zt,r,n))}))}),me={$:0},we=function(n){return{$:1,a:n}},ye=t(function(n,r){return{ao:n,at:r}}),Ae=jt(o(ye,Ut,w)),je=function(n){return fn(function(r){var t=n.f;2===t.$&&t.c&&t.c(),n.f=null,r(on(0))})},Ce=dn,_e=e(function(n,r,t){n:for(;;){if(r.b){var e=r.a,u=r.b;if(e.$){var a=e.a;return o(Et,function(r){var e=a.av;return f(_e,n,u,1===e.$?t:f(te,e.a,r,t))},Ce(f(vr,n,Ft(n),a)))}var i=e.a,c=o(Kt,i,t);if(1===c.$){n=n,r=u,t=t;continue n}return o(Et,function(){return f(_e,n,u,o(ce,i,t))},je(c.a))}return jt(t)}}),ke=u(function(n,r,t,e){return o(Et,function(n){return jt(o(ye,n,t))},f(_e,n,r,e.ao))}),Ne=e(function(n,r,t){var e=n(r);return e.$?t:o(gr,e.a,t)}),Ee=t(function(n,r){return f(kt,Ne(n),w,r)}),Oe=u(function(n,r,t,e){var u=e.b;return d(r,e.a)?kr(o(Ft,n,u(t))):Nr}),Je=e(function(n,r,t){return o(Et,function(){return jt(t)},Lt(o(Ee,f(Oe,n,r.a,r.b),t.at)))}),Le=t(function(n,r){if(r.$){var t=r.a;return we({aC:t.aC,aF:t.aF,aJ:o(br,n,t.aJ),Z:t.Z,aO:t.aO,aY:t.aY,av:t.av,aw:t.aw})}return{$:0,a:r.a}}),Fe=t(function(n,r){return{$:0,a:n,b:r}});mn.Http=wn(Ae,ke,Je,Le,t(function(n,r){return o(Fe,r.a,o(se,r.b,n))}));var Te,Re=Cn("Http"),Se=(Cn("Http"),function(n){return function(n){return Re(we({aC:!1,aF:n.aF,aJ:n.aJ,Z:n.Z,aO:n.aO,aY:n.aY,av:n.av,aw:n.aw}))}({aF:me,aJ:n.aJ,Z:w,aO:"GET",aY:Nr,av:Nr,aw:n.aw})}),qe=function(n){return{$:3,b:n}},xe=a(function(n,r,t,e,u){return{R:u,S:e,ab:n,I:r,aw:t}}),He=u(function(n,r,t,e){return{V:t,ab:n,I:r,as:e}}),ze=P,Ie=M,Me=ht(et),Pe=B,Ye=e(function(n,r,t){return o(Me,o(Pe,n,r),t)}),Be=Y,Ze=f(Ye,"size",ze,f(Ye,"download_count",Ie,f(Ye,"name",Be,f(Ye,"id",Ie,$t(He))))),De=f(Ye,"avatar_url",Be,f(Ye,"login",Be,$t(t(function(n,r){return{aE:r,ad:n}})))),Ge=f(Ye,"assets",qe(Ze),f(Ye,"author",De,f(Ye,"url",Be,f(Ye,"name",Be,f(Ye,"id",Ie,$t(xe)))))),Ue=t(function(n,r){return Se({aJ:o(pe,Ht,qe(Ge)),aw:(t=j(["https://api.github.com/repos/",n,"/",r,"/releases"]),o(Lr,"",t))});var t}),Ve=t(function(n,r){switch(n.$){case 0:return $(p(r,{A:Nr,w:!0}),o(Ue,r.H,r.C));case 1:return $(p(r,n.a.$?{A:kr(xt(n.a.a)),w:!1}:{w:!1,J:n.a.a}),St);case 2:return $(p(r,{H:n.a}),St);default:return $(p(r,{C:n.a}),St)}}),Xe=function(n){return{$:3,a:n}},We={$:0},Ke=function(n){return{$:2,a:n}},Qe=Sn("button"),nu=un,ru=t(function(n,r){return o(Hn,n,nu(r))}),tu=ru("className"),eu=Sn("div"),uu=Sn("h1"),au=Sn("input"),iu=xn,ou=t(function(n,r){return o(iu,n,{$:0,a:r})}),fu=function(n){return $(n,!0)},cu=t(function(n,r){return o(iu,n,{$:1,a:r})}),vu=o(t(function(n,r){return f(kt,Pe,r,n)}),j(["target","value"]),Be),su=function(n){return o(cu,"input",o(lt,fu,o(lt,n,vu)))},du=ru("placeholder"),bu=Rn,lu=ru("value"),hu=Sn("p"),$u=Sn("h3"),gu=Sn("ul"),pu=Sn("h2"),mu=Sn("i"),wu=function(n){return 0>n?-n:n},yu=t(function(n,r){for(;;){if(!r.b)return!1;var t=r.b;if(n(r.a))return!0;n=n,r=t}}),Au=T,ju=t(function(n,r){return m(n&&o(yu,function(n){return"0"!==n&&"."!==n},f(Au,gr,w,r))?"-":"",r)}),Cu=z,_u=F,ku=function(n){var r=n.a,t=n.b;if("9"===r){var e=Dr(t);return 1===e.$?"01":o(_u,"0",ku(e.a))}var u,a=Ir(r);return a>=48&&57>a?o(_u,g(0>(u=a+1)||u>1114111?"�":u>65535?String.fromCharCode(Math.floor((u-=65536)/1024)+55296,u%1024+56320):String.fromCharCode(u)),t):"0"},Nu=L,Eu=t(function(n,r){return r.$?Nr:kr(n(r.a))}),Ou=function(n){return o(_u,n,"")},Ju=e(function(n,r,t){return n>0?f(Ju,n>>1,m(r,r),1&n?m(t,r):t):t}),Lu=t(function(n,r){return f(Ju,n,r,"")}),Fu=e(function(n,r,t){return m(t,o(Lu,n-mt(t),Ou(r)))}),Tu=function(n){for(var r=n.length,t=[],e=0;r>e;){var u=n.charCodeAt(e);55296>u||u>56319?(t[r-e]=n[e],e++):(t[r-e]=n[e+1],t[r-++e]=n[e-1],e++)}return t.join("")},Ru=function(n){var r=o(Fr,".",n);return r.b?$(r.a,r.b.b?r.b.a:"0"):$("0","0")},Su=t(function(n,r){var t=r.b;return $(n(r.a),t)}),qu=t(function(n,r){return r.$?n:r.a}),xu=e(function(n,r,t){if((e=t)===1/0||e===-1/0||Nu(t))return Cu(t);var e,u=0>t,a=Ru(function(n){var r=o(Fr,"e",Cu(wu(n)));if(r.b){if(r.b.b){var t=r.a,e=r.b.a,u=o(qu,0,function(n){for(var r=0,t=n.charCodeAt(0),e=43==t||45==t?1:0,u=e;n.length>u;++u){var a=n.charCodeAt(u);if(48>a||a>57)return Nr;r=10*r+a-48}return u==e?Nr:kr(45==t?-r:r)}(o(At,"+",e)?o(yt,1,e):e)),a=Ru(t),i=m(a.a,a.b),c=0>u?o(qu,"0",o(Eu,function(n){return n.a+"."+n.b},o(Eu,Su(Ou),Dr(m(o(Lu,wu(u),"0"),i))))):f(Fu,u+1,"0",i);return m(0>n?"-":"",c)}return m(0>n?"-":"",t=r.a)}return""}(wu(t))),i=a.a,c=a.b,v=mt(i)+r,s=m(o(Lu,1-v,"0"),f(Fu,v,"0",m(i,c))),d=mt(s),b=o(it,1,v),h=o(n,u,f(wt,b,d,s)),$=f(wt,0,b,s),g=h?Tu(o(qu,"1",o(Eu,ku,Dr(Tu($))))):$,p=mt(g),w="0"===g?g:r>0?0>l(r,mt(c))?f(wt,0,p-r,g)+"."+f(wt,p-r,p,g):m(i+".",f(Fu,r,"0",c)):m(g,o(Lu,wu(r),"0"));return o(ju,u,w)})(t(function(n,r){var t,e=Dr(r);return 1!==e.$&&("5"===e.a.a?""!==e.a.b||!n:(t=Ir(e.a.a))>53&&n||t>=53&&!n)})),Hu=Sn("li"),zu=function(n){return o(Hu,j([tu("asset")]),j([o(eu,j([tu("asset-name")]),j([bu(n.I)])),o(eu,j([tu("asset-info")]),j([o(eu,j([tu("icon-text size")]),j([o(mu,j([tu("material-icons icon")]),j([bu("network_check")])),bu((r=n.as,o(xu,2,r/1e6)+" MB"))])),o(eu,j([tu("icon-text")]),j([o(mu,j([tu("material-icons icon")]),j([bu("cloud_download")])),bu(Jr(n.V))]))]))]));var r},Iu=function(n){return o(eu,j([tu("result-item")]),j([o(eu,j([tu("top")]),j([o(pu,j([tu("release-name")]),j([bu(n.I)])),o($u,j([tu("user")]),j([o(mu,j([tu("material-icons icon")]),j([bu("person")])),bu(n.S.ad)]))])),(r=n.R,o(gu,w,o(Nt,zu,r)))]));var r},Mu=function(n){if(n.w)return o(hu,w,j([bu("Loading ...")]));var r,t=n.A;return t.$?(r=n.J,o(gu,j([tu("result-list")]),o(Nt,Iu,r))):function(n){return o(eu,j([tu("error-container")]),j([o($u,j([tu("error-message")]),j([bu(n)]))]))}(t.a)};Te={Main:{init:Rt({aN:function(){return $({A:Nr,w:!1,C:"",J:w,H:""},St)},aX:function(){return qt},a_:Ve,a$:function(n){return o(eu,w,j([o(eu,j([tu("container")]),j([o(uu,j([tu("title")]),j([bu("GitHub Release Stats")])),o(eu,j([tu("form")]),j([o(au,j([du("Username"),tu("input username-input"),lu(n.H),su(Ke)]),w),o(au,j([du("Project Name"),tu("input project-name-input"),lu(n.C),su(Xe)]),w),o(Qe,j([(r=We,o(ou,"click",$t(r))),tu("btn")]),j([bu("Get")]))]))])),o(eu,j([tu("result-container")]),j([Mu(n)]))]));var r}})($t(0))(0)}},n.Elm?function n(r,t){for(var e in t)e in r?"init"==e?N(6):n(r[e],t[e]):r[e]=t[e]}(n.Elm,Te):n.Elm=Te}(this);
},{}],"Focm":[function(require,module,exports) {
"use strict";require("typeface-cairo"),require("./style/default.scss"),require("./style/desktop.scss");var e=require("./Main.elm");e.Elm.Main.init({node:document.getElementById("elm-app")});
},{"typeface-cairo":"XucH","./style/default.scss":"G7yG","./style/desktop.scss":"G7yG","./Main.elm":"asWa"}]},{},["Focm"], null)
//# sourceMappingURL=/elm-gh-stats/src.a9f56937.js.map