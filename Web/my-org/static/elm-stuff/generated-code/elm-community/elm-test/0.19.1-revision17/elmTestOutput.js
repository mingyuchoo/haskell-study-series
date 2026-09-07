// Apply Node polyfills as necessary.
var window = {
  Date: Date,
  addEventListener: function () {},
  removeEventListener: function () {},
};

var location = {
  href: '',
  host: '',
  hostname: '',
  protocol: '',
  origin: '',
  port: '',
  pathname: '',
  search: '',
  hash: '',
  username: '',
  password: '',
};
var document = { body: {}, createTextNode: function () {}, location: location };

if (typeof FileList === 'undefined') {
  FileList = function () {};
}

if (typeof File === 'undefined') {
  File = function () {};
}

if (typeof XMLHttpRequest === 'undefined') {
  XMLHttpRequest = function () {
    return {
      addEventListener: function () {},
      open: function () {},
      send: function () {},
    };
  };

  var oldConsoleWarn = console.warn;
  console.warn = function () {
    if (
      arguments.length === 1 &&
      arguments[0].indexOf('Compiled in DEV mode') === 0
    )
      return;
    return oldConsoleWarn.apply(console, arguments);
  };
}

if (typeof FormData === 'undefined') {
  FormData = function () {
    this._data = [];
  };
  FormData.prototype.append = function () {
    this._data.push(Array.prototype.slice.call(arguments));
  };
}

var Elm = (function(module) {
var __elmTestSymbol = Symbol("elmTestSymbol");
(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});



function _Test_runThunk(thunk)
{
  try {
    // Attempt to run the thunk as normal.
    return $elm$core$Result$Ok(thunk(_Utils_Tuple0));
  } catch (err) {
    // If it throws, return an error instead of crashing.
    return $elm$core$Result$Err(err.toString());
  }
}



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}


function _Url_percentEncode(string)
{
	return encodeURIComponent(string);
}

function _Url_percentDecode(string)
{
	try
	{
		return $elm$core$Maybe$Just(decodeURIComponent(string));
	}
	catch (e)
	{
		return $elm$core$Maybe$Nothing;
	}
}


// NOTE: this is duplicating constants also defined in Test.Internal.KernelConstants
//       so if you make any changes here, be sure to synchronize them there!
var virtualDomKernelConstants =
  {
    nodeTypeTagger: 4,
    nodeTypeThunk: 5,
    kids: "e",
    refs: "l",
    thunk: "m",
    node: "k",
    value: "a"
  }

function forceThunks(vNode) {
  if (typeof vNode !== "undefined" && vNode.$ === "#2") {
    // This is a tuple (the kids : List (String, Html) field of a Keyed node); recurse into the right side of the tuple
    vNode.b = forceThunks(vNode.b);
  }
  if (typeof vNode !== 'undefined' && vNode.$ === virtualDomKernelConstants.nodeTypeThunk && !vNode[virtualDomKernelConstants.node]) {
    // This is a lazy node; evaluate it
    var args = vNode[virtualDomKernelConstants.thunk];
    vNode[virtualDomKernelConstants.node] = vNode[virtualDomKernelConstants.thunk].apply(args);
    // And then recurse into the evaluated node
    vNode[virtualDomKernelConstants.node] = forceThunks(vNode[virtualDomKernelConstants.node]);
  }
  if (typeof vNode !== 'undefined' && vNode.$ === virtualDomKernelConstants.nodeTypeTagger) {
    // This is an Html.map; recurse into the node it is wrapping
    vNode[virtualDomKernelConstants.node] = forceThunks(vNode[virtualDomKernelConstants.node]);
  }
  if (typeof vNode !== 'undefined' && typeof vNode[virtualDomKernelConstants.kids] !== 'undefined') {
    // This is something with children (either a node with kids : List Html, or keyed with kids : List (String, Html));
    // recurse into the children
    vNode[virtualDomKernelConstants.kids] = vNode[virtualDomKernelConstants.kids].map(forceThunks);
  }
  return vNode;
}

function _HtmlAsJson_toJson(html)
{
  return _Json_wrap(forceThunks(html));
}

function _HtmlAsJson_eventHandler(event)
{
  return event[virtualDomKernelConstants.value];
}

function _HtmlAsJson_taggerFunction(tagger)
{
  return tagger.a;
}

function _HtmlAsJson_attributeToJson(attribute)
{
  return _Json_wrap(attribute);
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS
//
// For some reason, tabs can appear in href protocols and it still works.
// So '\tjava\tSCRIPT:alert("!!!")' and 'javascript:alert("!!!")' are the same
// in practice. That is why _VirtualDom_RE_js and _VirtualDom_RE_js_html look
// so freaky.
//
// Pulling the regular expressions out to the top level gives a slight speed
// boost in small benchmarks (4-10%) but hoisting values to reduce allocation
// can be unpredictable in large programs where JIT may have a harder time with
// functions are not fully self-contained. The benefit is more that the js and
// js_html ones are so weird that I prefer to see them near each other.


var _VirtualDom_RE_script = /^script$/i;
var _VirtualDom_RE_on_formAction = /^(on|formAction$)/i;
var _VirtualDom_RE_js = /^\s*j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:/i;
var _VirtualDom_RE_js_html = /^\s*(j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:|d\s*a\s*t\s*a\s*:\s*t\s*e\s*x\s*t\s*\/\s*h\s*t\s*m\s*l\s*(,|;))/i;


function _VirtualDom_noScript(tag)
{
	return _VirtualDom_RE_script.test(tag) ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return _VirtualDom_RE_on_formAction.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return _VirtualDom_RE_js.test(value)
		? /**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return _VirtualDom_RE_js_html.test(value)
		? /**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlJson(value)
{
	return (typeof _Json_unwrap(value) === 'string' && _VirtualDom_RE_js_html.test(_Json_unwrap(value)))
		? _Json_wrap(
			/**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		) : value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}
var $elm$core$List$cons = _List_cons;
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $author$project$Test$Reporter$Reporter$ConsoleReport = function (a) {
	return {$: 'ConsoleReport', a: a};
};
var $author$project$Console$Text$Monochrome = {$: 'Monochrome'};
var $elm$core$Debug$todo = _Debug_todo;
var $author$project$Test$Runner$Node$checkHelperReplaceMe___ = function (_v0) {
	return _Debug_todo(
		'Test.Runner.Node',
		{
			start: {line: 362, column: 5},
			end: {line: 362, column: 15}
		})('The regex for replacing this Debug.todo with some real code must have failed since you see this message!\n\nPlease report this bug: https://github.com/rtfeldman/node-test-runner/issues/new\n');
};
var $author$project$Test$Runner$Node$check = value => value && value.__elmTestSymbol === __elmTestSymbol ? $elm$core$Maybe$Just(value) : $elm$core$Maybe$Nothing;
var $author$project$GraphFixture$edge = F3(
	function (kind, from, to) {
		return {from: from, kind: kind, to: to};
	});
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $author$project$Domain$NoData = {$: 'NoData'};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $author$project$GraphFixture$goal = {
	active: false,
	analysis: {coverage: 0.5, possibleCause: '권한 부족'},
	evaluation: {latestValue: $elm$core$Maybe$Nothing, progress: 0, status: $author$project$Domain$NoData},
	goal: {
		baseline: 0,
		deadline: '2026-12-31',
		description: '고객 성장',
		id: 'g',
		metric: {direction: 'HigherIsBetter', id: 'm', name: '신규 고객 수', unit: '명'},
		requiredBudget: 100,
		requiredPermissions: _List_fromArray(
			['Hiring']),
		target: 100
	},
	owner: $elm$core$Maybe$Just('p'),
	results: _List_Nil,
	strategies: _List_Nil
};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $author$project$AppFixture$mapPage = F2(
	function (f, model) {
		return _Utils_update(
			model,
			{
				pageState: f(model.pageState)
			});
	});
var $author$project$AppFixture$mapSession = F2(
	function (f, model) {
		return _Utils_update(
			model,
			{
				session: f(model.session)
			});
	});
var $author$project$GraphFixture$node = F2(
	function (tag, id) {
		return {contents: id, tag: tag};
	});
var $author$project$Remote$Loaded = function (a) {
	return {$: 'Loaded', a: a};
};
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $author$project$App$Agents$init = {drafts: $elm$core$Dict$empty, errors: $elm$core$Dict$empty, loading: false, snapshots: $elm$core$Dict$empty};
var $author$project$App$Discovery$init = {documents: $elm$core$Dict$empty, drafts: $elm$core$Dict$empty, errors: $elm$core$Dict$empty, loading: false};
var $author$project$App$Drafts$init = {deletion: $elm$core$Maybe$Nothing, drafts: $elm$core$Dict$empty, goalDrafts: $elm$core$Dict$empty, goalSerial: $elm$core$Dict$empty, reviewDrafts: $elm$core$Dict$empty, serial: 0};
var $author$project$Page$Organizations = {$: 'Organizations'};
var $author$project$Ui$Activity$init = {from: '', kind: '', query: '', review: $elm$core$Maybe$Nothing, until: ''};
var $author$project$Ui$ResponsibilityGraph$init = {diagram: true, query: '', selected: $elm$core$Maybe$Nothing, showDependencies: false, showResources: false, zoom: 1};
var $author$project$App$PageState$init = {activity: $author$project$Ui$Activity$init, expandedGoal: $elm$core$Maybe$Nothing, graph: $author$project$Ui$ResponsibilityGraph$init, guideOpen: false, listModes: $elm$core$Dict$empty, page: $author$project$Page$Organizations, peopleQuery: '', peopleStatus: 'active', selectedPerson: $elm$core$Maybe$Nothing};
var $author$project$App$Session$Idle = {$: 'Idle'};
var $author$project$Remote$Loading = {$: 'Loading'};
var $author$project$App$Session$init = {fresh: false, org: $elm$core$Maybe$Nothing, organizations: $author$project$Remote$Loading, request: 0, saving: $author$project$App$Session$Idle, syncing: true, workspace: $author$project$Remote$Loading};
var $author$project$App$Model$init = function (flags) {
	return {agents: $author$project$App$Agents$init, discovery: $author$project$App$Discovery$init, error: false, flags: flags, forms: $author$project$App$Drafts$init, notice: '', pageState: $author$project$App$PageState$init, session: $author$project$App$Session$init};
};
var $author$project$App$Effect$LoadAgents = F2(
	function (a, b) {
		return {$: 'LoadAgents', a: a, b: b};
	});
var $author$project$App$Effect$LoadDiscovery = F2(
	function (a, b) {
		return {$: 'LoadDiscovery', a: a, b: b};
	});
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $author$project$App$Effect$LoadOrganizations = function (a) {
	return {$: 'LoadOrganizations', a: a};
};
var $author$project$App$Effect$LoadWorkspace = F2(
	function (a, b) {
		return {$: 'LoadWorkspace', a: a, b: b};
	});
var $author$project$App$Session$refresh = function (state) {
	var token = state.request + 1;
	var next = _Utils_update(
		state,
		{fresh: false, request: token, syncing: true});
	return _Utils_Tuple2(
		next,
		_List_fromArray(
			[
				function () {
				var _v0 = state.org;
				if (_v0.$ === 'Nothing') {
					return $author$project$App$Effect$LoadOrganizations(token);
				} else {
					var org = _v0.a;
					return A2($author$project$App$Effect$LoadWorkspace, token, org);
				}
			}()
			]));
};
var $author$project$App$Update$setAgentsLoading = F2(
	function (loading, state) {
		return _Utils_update(
			state,
			{loading: loading});
	});
var $author$project$App$Update$setDiscoveryLoading = F2(
	function (loading, state) {
		return _Utils_update(
			state,
			{loading: loading});
	});
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$App$Update$refresh = function (model) {
	var forms = model.forms;
	var _v0 = $author$project$App$Session$refresh(model.session);
	var session = _v0.a;
	var effects = _v0.b;
	return _Utils_Tuple2(
		_Utils_update(
			model,
			{
				agents: A2(
					$author$project$App$Update$setAgentsLoading,
					!_Utils_eq(session.org, $elm$core$Maybe$Nothing),
					model.agents),
				discovery: A2(
					$author$project$App$Update$setDiscoveryLoading,
					!_Utils_eq(session.org, $elm$core$Maybe$Nothing),
					model.discovery),
				forms: _Utils_update(
					forms,
					{deletion: $elm$core$Maybe$Nothing}),
				session: session
			}),
		_Utils_ap(
			effects,
			A2(
				$elm$core$Maybe$withDefault,
				_List_Nil,
				A2(
					$elm$core$Maybe$map,
					function (org) {
						return _List_fromArray(
							[
								A2($author$project$App$Effect$LoadDiscovery, session.request, org),
								A2($author$project$App$Effect$LoadAgents, session.request, org)
							]);
					},
					session.org))));
};
var $author$project$App$Update$init = function (flags) {
	return $author$project$App$Update$refresh(
		$author$project$App$Model$init(flags));
};
var $author$project$ListViewTest$workspace = {
	authorities: _List_Nil,
	compiler: {diagnostics: _List_Nil, errors: 0, warnings: 0},
	decisionShare: $elm$core$Dict$empty,
	demo: false,
	edges: _List_Nil,
	events: _List_Nil,
	goals: _List_Nil,
	organization: {createdAt: '2026-01-01', id: 'org-a', name: 'Alpha'},
	people: _List_Nil,
	reviewWarnings: _List_Nil,
	reviews: _List_Nil,
	version: 1
};
var $author$project$ListViewTest$ready = function () {
	var initial = $author$project$App$Update$init(
		{deadline: '2026-12-31', seed: 'test', today: '2026-01-01'}).a;
	return A2(
		$author$project$AppFixture$mapSession,
		function (s) {
			return _Utils_update(
				s,
				{
					fresh: true,
					org: $elm$core$Maybe$Just('org-a'),
					syncing: false,
					workspace: $author$project$Remote$Loaded($author$project$ListViewTest$workspace)
				});
		},
		initial);
}();
var $author$project$Test$Runner$Node$Receive = function (a) {
	return {$: 'Receive', a: a};
};
var $elm_explorations$test$Test$Runner$Failure$DuplicatedName = {$: 'DuplicatedName'};
var $elm_explorations$test$Test$Internal$ElmTestVariant__Batch = function (a) {
	return {__elmTestSymbol: __elmTestSymbol, $: 'ElmTestVariant__Batch', a: a};
};
var $elm_explorations$test$Test$Runner$Failure$EmptyList = {$: 'EmptyList'};
var $elm_explorations$test$Test$Runner$Failure$Invalid = function (a) {
	return {$: 'Invalid', a: a};
};
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$core$Set$Set_elm_builtin = function (a) {
	return {$: 'Set_elm_builtin', a: a};
};
var $elm$core$Set$empty = $elm$core$Set$Set_elm_builtin($elm$core$Dict$empty);
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return $elm$core$Set$Set_elm_builtin(
			A3($elm$core$Dict$insert, key, _Utils_Tuple0, dict));
	});
var $elm$core$Dict$isEmpty = function (dict) {
	if (dict.$ === 'RBEmpty_elm_builtin') {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Set$isEmpty = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$isEmpty(dict);
};
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (_v0.$ === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var $elm$core$Set$member = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return A2($elm$core$Dict$member, key, dict);
	});
var $elm_explorations$test$Test$Internal$duplicatedName = function (tests) {
	var names = function (test) {
		names:
		while (true) {
			switch (test.$) {
				case 'ElmTestVariant__Labeled':
					var str = test.a;
					return _List_fromArray(
						[str]);
				case 'ElmTestVariant__Batch':
					var subtests = test.a;
					return A2($elm$core$List$concatMap, names, subtests);
				case 'ElmTestVariant__UnitTest':
					return _List_Nil;
				case 'ElmTestVariant__FuzzTest':
					return _List_Nil;
				case 'ElmTestVariant__Skipped':
					var subTest = test.a;
					var $temp$test = subTest;
					test = $temp$test;
					continue names;
				default:
					var subTest = test.a;
					var $temp$test = subTest;
					test = $temp$test;
					continue names;
			}
		}
	};
	var accumDuplicates = F2(
		function (newName, _v2) {
			var dups = _v2.a;
			var uniques = _v2.b;
			return A2($elm$core$Set$member, newName, uniques) ? _Utils_Tuple2(
				A2($elm$core$Set$insert, newName, dups),
				uniques) : _Utils_Tuple2(
				dups,
				A2($elm$core$Set$insert, newName, uniques));
		});
	var _v1 = A3(
		$elm$core$List$foldl,
		accumDuplicates,
		_Utils_Tuple2($elm$core$Set$empty, $elm$core$Set$empty),
		A2($elm$core$List$concatMap, names, tests));
	var dupsAccum = _v1.a;
	var uniquesAccum = _v1.b;
	return $elm$core$Set$isEmpty(dupsAccum) ? $elm$core$Result$Ok(uniquesAccum) : $elm$core$Result$Err(dupsAccum);
};
var $elm_explorations$test$Test$Internal$ElmTestVariant__UnitTest = function (a) {
	return {__elmTestSymbol: __elmTestSymbol, $: 'ElmTestVariant__UnitTest', a: a};
};
var $elm_explorations$test$Test$Expectation$Fail = function (a) {
	return {$: 'Fail', a: a};
};
var $elm_explorations$test$Test$Distribution$NoDistribution = {$: 'NoDistribution'};
var $elm_explorations$test$Test$Expectation$fail = function (_v0) {
	var description = _v0.description;
	var reason = _v0.reason;
	return $elm_explorations$test$Test$Expectation$Fail(
		{description: description, distributionReport: $elm_explorations$test$Test$Distribution$NoDistribution, given: $elm$core$Maybe$Nothing, reason: reason});
};
var $elm_explorations$test$Test$Internal$failNow = function (record) {
	return $elm_explorations$test$Test$Internal$ElmTestVariant__UnitTest(
		function (_v0) {
			return _List_fromArray(
				[
					$elm_explorations$test$Test$Expectation$fail(record)
				]);
		});
};
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $elm_explorations$test$Test$concat = function (tests) {
	if ($elm$core$List$isEmpty(tests)) {
		return $elm_explorations$test$Test$Internal$failNow(
			{
				description: 'This `concat` has no tests in it. Let\'s give it some!',
				reason: $elm_explorations$test$Test$Runner$Failure$Invalid($elm_explorations$test$Test$Runner$Failure$EmptyList)
			});
	} else {
		var _v0 = $elm_explorations$test$Test$Internal$duplicatedName(tests);
		if (_v0.$ === 'Err') {
			var dups = _v0.a;
			var dupDescription = function (duped) {
				return 'A test group contains multiple tests named \'' + (duped + '\'. Do some renaming so that tests have unique names.');
			};
			return $elm_explorations$test$Test$Internal$failNow(
				{
					description: A2(
						$elm$core$String$join,
						'\n',
						A2(
							$elm$core$List$map,
							dupDescription,
							$elm$core$Set$toList(dups))),
					reason: $elm_explorations$test$Test$Runner$Failure$Invalid($elm_explorations$test$Test$Runner$Failure$DuplicatedName)
				});
		} else {
			return $elm_explorations$test$Test$Internal$ElmTestVariant__Batch(tests);
		}
	}
};
var $elm_explorations$test$Test$Runner$Failure$BadDescription = {$: 'BadDescription'};
var $elm_explorations$test$Test$Internal$ElmTestVariant__Labeled = F2(
	function (a, b) {
		return {__elmTestSymbol: __elmTestSymbol, $: 'ElmTestVariant__Labeled', a: a, b: b};
	});
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$trim = _String_trim;
var $elm_explorations$test$Test$describe = F2(
	function (untrimmedDesc, tests) {
		var desc = $elm$core$String$trim(untrimmedDesc);
		if ($elm$core$String$isEmpty(desc)) {
			return $elm_explorations$test$Test$Internal$failNow(
				{
					description: 'This `describe` has a blank description. Let\'s give it a useful one!',
					reason: $elm_explorations$test$Test$Runner$Failure$Invalid($elm_explorations$test$Test$Runner$Failure$BadDescription)
				});
		} else {
			if ($elm$core$List$isEmpty(tests)) {
				return $elm_explorations$test$Test$Internal$failNow(
					{
						description: 'This `describe ' + (desc + '` has no tests in it. Let\'s give it some!'),
						reason: $elm_explorations$test$Test$Runner$Failure$Invalid($elm_explorations$test$Test$Runner$Failure$EmptyList)
					});
			} else {
				var _v0 = $elm_explorations$test$Test$Internal$duplicatedName(tests);
				if (_v0.$ === 'Err') {
					var dups = _v0.a;
					var dupDescription = function (duped) {
						return 'Contains multiple tests named \'' + (duped + '\'. Let\'s rename them so we know which is which.');
					};
					return A2(
						$elm_explorations$test$Test$Internal$ElmTestVariant__Labeled,
						desc,
						$elm_explorations$test$Test$Internal$failNow(
							{
								description: A2(
									$elm$core$String$join,
									'\n',
									A2(
										$elm$core$List$map,
										dupDescription,
										$elm$core$Set$toList(dups))),
								reason: $elm_explorations$test$Test$Runner$Failure$Invalid($elm_explorations$test$Test$Runner$Failure$DuplicatedName)
							}));
				} else {
					var childrenNames = _v0.a;
					return A2($elm$core$Set$member, desc, childrenNames) ? A2(
						$elm_explorations$test$Test$Internal$ElmTestVariant__Labeled,
						desc,
						$elm_explorations$test$Test$Internal$failNow(
							{
								description: 'The test \'' + (desc + '\' contains a child test of the same name. Let\'s rename them so we know which is which.'),
								reason: $elm_explorations$test$Test$Runner$Failure$Invalid($elm_explorations$test$Test$Runner$Failure$DuplicatedName)
							})) : A2(
						$elm_explorations$test$Test$Internal$ElmTestVariant__Labeled,
						desc,
						$elm_explorations$test$Test$Internal$ElmTestVariant__Batch(tests));
				}
			}
		}
	});
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $author$project$Test$Runner$Node$elmTestPort__receive = _Platform_incomingPort('elmTestPort__receive', $elm$json$Json$Decode$value);
var $author$project$Test$Reporter$Reporter$TestReporter = F4(
	function (format, reportBegin, reportComplete, reportSummary) {
		return {format: format, reportBegin: reportBegin, reportComplete: reportComplete, reportSummary: reportSummary};
	});
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var $author$project$Console$Text$Default = {$: 'Default'};
var $author$project$Console$Text$Normal = {$: 'Normal'};
var $author$project$Console$Text$Text = F2(
	function (a, b) {
		return {$: 'Text', a: a, b: b};
	});
var $author$project$Console$Text$plain = $author$project$Console$Text$Text(
	{background: $author$project$Console$Text$Default, foreground: $author$project$Console$Text$Default, modifiers: _List_Nil, style: $author$project$Console$Text$Normal});
var $author$project$Test$Reporter$Console$pluralize = F3(
	function (singular, plural, count) {
		var suffix = (count === 1) ? singular : plural;
		return A2(
			$elm$core$String$join,
			' ',
			_List_fromArray(
				[
					$elm$core$String$fromInt(count),
					suffix
				]));
	});
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$Test$Runner$Node$Vendor$Console$colorsInverted = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[7m', str, '\u001B[27m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$dark = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[2m', str, '\u001B[22m']));
};
var $author$project$Console$Text$applyModifiersHelp = F2(
	function (modifier, str) {
		if (modifier.$ === 'Inverted') {
			return $author$project$Test$Runner$Node$Vendor$Console$colorsInverted(str);
		} else {
			return $author$project$Test$Runner$Node$Vendor$Console$dark(str);
		}
	});
var $author$project$Console$Text$applyModifiers = F2(
	function (modifiers, str) {
		return A3($elm$core$List$foldl, $author$project$Console$Text$applyModifiersHelp, str, modifiers);
	});
var $author$project$Test$Runner$Node$Vendor$Console$bold = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[1m', str, '\u001B[22m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$underline = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[4m', str, '\u001B[24m']));
};
var $author$project$Console$Text$applyStyle = F2(
	function (style, str) {
		switch (style.$) {
			case 'Normal':
				return str;
			case 'Bold':
				return $author$project$Test$Runner$Node$Vendor$Console$bold(str);
			default:
				return $author$project$Test$Runner$Node$Vendor$Console$underline(str);
		}
	});
var $author$project$Test$Runner$Node$Vendor$Console$bgBlack = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[40m', str, '\u001B[49m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$bgBlue = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[44m', str, '\u001B[49m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$bgCyan = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[46m', str, '\u001B[49m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$bgGreen = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[42m', str, '\u001B[49m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$bgMagenta = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[45m', str, '\u001B[49m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$bgRed = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[41m', str, '\u001B[49m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$bgWhite = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[47m', str, '\u001B[49m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$bgYellow = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[43m', str, '\u001B[49m']));
};
var $author$project$Console$Text$colorizeBackground = F2(
	function (color, str) {
		switch (color.$) {
			case 'Default':
				return str;
			case 'Red':
				return $author$project$Test$Runner$Node$Vendor$Console$bgRed(str);
			case 'Green':
				return $author$project$Test$Runner$Node$Vendor$Console$bgGreen(str);
			case 'Yellow':
				return $author$project$Test$Runner$Node$Vendor$Console$bgYellow(str);
			case 'Black':
				return $author$project$Test$Runner$Node$Vendor$Console$bgBlack(str);
			case 'Blue':
				return $author$project$Test$Runner$Node$Vendor$Console$bgBlue(str);
			case 'Magenta':
				return $author$project$Test$Runner$Node$Vendor$Console$bgMagenta(str);
			case 'Cyan':
				return $author$project$Test$Runner$Node$Vendor$Console$bgCyan(str);
			default:
				return $author$project$Test$Runner$Node$Vendor$Console$bgWhite(str);
		}
	});
var $author$project$Test$Runner$Node$Vendor$Console$black = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[30m', str, '\u001B[39m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$blue = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[34m', str, '\u001B[39m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$cyan = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[36m', str, '\u001B[39m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$green = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[32m', str, '\u001B[39m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$magenta = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[35m', str, '\u001B[39m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$red = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[31m', str, '\u001B[39m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$white = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[37m', str, '\u001B[39m']));
};
var $author$project$Test$Runner$Node$Vendor$Console$yellow = function (str) {
	return A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			['\u001B[33m', str, '\u001B[39m']));
};
var $author$project$Console$Text$colorizeForeground = F2(
	function (color, str) {
		switch (color.$) {
			case 'Default':
				return str;
			case 'Red':
				return $author$project$Test$Runner$Node$Vendor$Console$red(str);
			case 'Green':
				return $author$project$Test$Runner$Node$Vendor$Console$green(str);
			case 'Yellow':
				return $author$project$Test$Runner$Node$Vendor$Console$yellow(str);
			case 'Black':
				return $author$project$Test$Runner$Node$Vendor$Console$black(str);
			case 'Blue':
				return $author$project$Test$Runner$Node$Vendor$Console$blue(str);
			case 'Magenta':
				return $author$project$Test$Runner$Node$Vendor$Console$magenta(str);
			case 'Cyan':
				return $author$project$Test$Runner$Node$Vendor$Console$cyan(str);
			default:
				return $author$project$Test$Runner$Node$Vendor$Console$white(str);
		}
	});
var $author$project$Console$Text$render = F2(
	function (useColor, txt) {
		if (txt.$ === 'Text') {
			var attrs = txt.a;
			var str = txt.b;
			if (useColor.$ === 'UseColor') {
				return A2(
					$author$project$Console$Text$applyStyle,
					attrs.style,
					A2(
						$author$project$Console$Text$applyModifiers,
						attrs.modifiers,
						A2(
							$author$project$Console$Text$colorizeForeground,
							attrs.foreground,
							A2($author$project$Console$Text$colorizeBackground, attrs.background, str))));
			} else {
				return str;
			}
		} else {
			var texts = txt.a;
			return A2(
				$elm$core$String$join,
				'',
				A2(
					$elm$core$List$map,
					$author$project$Console$Text$render(useColor),
					texts));
		}
	});
var $author$project$Test$Reporter$Console$textToValue = F2(
	function (useColor, txt) {
		return $elm$json$Json$Encode$string(
			A2($author$project$Console$Text$render, useColor, txt));
	});
var $author$project$Test$Reporter$Console$reportBegin = F2(
	function (useColor, _v0) {
		var globs = _v0.globs;
		var fuzzRuns = _v0.fuzzRuns;
		var testCount = _v0.testCount;
		var initialSeed = _v0.initialSeed;
		var prefix = 'Running ' + (A3($author$project$Test$Reporter$Console$pluralize, 'test', 'tests', testCount) + ('. To reproduce these results, run: elm-test --fuzz ' + ($elm$core$String$fromInt(fuzzRuns) + (' --seed ' + $elm$core$String$fromInt(initialSeed)))));
		return $elm$core$Maybe$Just(
			$elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'type',
						$elm$json$Json$Encode$string('begin')),
						_Utils_Tuple2(
						'output',
						A2(
							$author$project$Test$Reporter$Console$textToValue,
							useColor,
							$author$project$Console$Text$plain(
								A2(
									$elm$core$String$join,
									' ',
									A2($elm$core$List$cons, prefix, globs)) + '\n')))
					])));
	});
var $author$project$Test$Reporter$JUnit$reportBegin = function (_v0) {
	return $elm$core$Maybe$Nothing;
};
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(_Utils_Tuple0),
				entries));
	});
var $author$project$Test$Reporter$Json$reportBegin = function (_v0) {
	var globs = _v0.globs;
	var paths = _v0.paths;
	var fuzzRuns = _v0.fuzzRuns;
	var testCount = _v0.testCount;
	var initialSeed = _v0.initialSeed;
	return $elm$core$Maybe$Just(
		$elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'event',
					$elm$json$Json$Encode$string('runStart')),
					_Utils_Tuple2(
					'testCount',
					$elm$json$Json$Encode$string(
						$elm$core$String$fromInt(testCount))),
					_Utils_Tuple2(
					'fuzzRuns',
					$elm$json$Json$Encode$string(
						$elm$core$String$fromInt(fuzzRuns))),
					_Utils_Tuple2(
					'globs',
					A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, globs)),
					_Utils_Tuple2(
					'paths',
					A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, paths)),
					_Utils_Tuple2(
					'initialSeed',
					$elm$json$Json$Encode$string(
						$elm$core$String$fromInt(initialSeed)))
				])));
};
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm_explorations$test$AsciiTable$AlignLeft = {$: 'AlignLeft'};
var $elm_explorations$test$AsciiTable$AlignRight = {$: 'AlignRight'};
var $elm_explorations$test$Test$Runner$Distribution$bars = 30;
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $elm$core$String$length = _String_length;
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			$elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var $elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3($elm$core$String$repeatHelp, n, chunk, '');
	});
var $elm$core$String$padRight = F3(
	function (n, _char, string) {
		return _Utils_ap(
			string,
			A2(
				$elm$core$String$repeat,
				n - $elm$core$String$length(string),
				$elm$core$String$fromChar(_char)));
	});
var $elm$core$Basics$round = _Basics_round;
var $elm_explorations$test$Test$Runner$Distribution$barView = function (_v0) {
	var count = _v0.count;
	var runsElapsed = _v0.runsElapsed;
	var percentage = count / runsElapsed;
	var barsForPercentage = percentage * $elm_explorations$test$Test$Runner$Distribution$bars;
	var fullBars = $elm$core$Basics$round(barsForPercentage);
	return A3(
		$elm$core$String$padRight,
		$elm_explorations$test$Test$Runner$Distribution$bars,
		_Utils_chr('░'),
		A2($elm$core$String$repeat, fullBars, '█'));
};
var $elm$core$String$fromFloat = _String_fromNumber;
var $elm$core$List$map3 = _List_map3;
var $elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$max, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$String$padLeft = F3(
	function (n, _char, string) {
		return _Utils_ap(
			A2(
				$elm$core$String$repeat,
				n - $elm$core$String$length(string),
				$elm$core$String$fromChar(_char)),
			string);
	});
var $elm$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (n <= 0) {
				return result;
			} else {
				var $temp$result = A2($elm$core$List$cons, value, result),
					$temp$n = n - 1,
					$temp$value = value;
				result = $temp$result;
				n = $temp$n;
				value = $temp$value;
				continue repeatHelp;
			}
		}
	});
var $elm$core$List$repeat = F2(
	function (n, value) {
		return A3($elm$core$List$repeatHelp, _List_Nil, n, value);
	});
var $elm_explorations$test$MicroListExtra$rowsLength = function (listOfLists) {
	if (!listOfLists.b) {
		return 0;
	} else {
		var x = listOfLists.a;
		return $elm$core$List$length(x);
	}
};
var $elm_explorations$test$MicroListExtra$transpose = function (listOfLists) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$List$map2($elm$core$List$cons),
		A2(
			$elm$core$List$repeat,
			$elm_explorations$test$MicroListExtra$rowsLength(listOfLists),
			_List_Nil),
		listOfLists);
};
var $elm_explorations$test$AsciiTable$view = F2(
	function (columns, items) {
		var padFn = F3(
			function (length, align, string) {
				if (align.$ === 'AlignLeft') {
					return A3(
						$elm$core$String$padRight,
						length,
						_Utils_chr(' '),
						string);
				} else {
					return A3(
						$elm$core$String$padLeft,
						length,
						_Utils_chr(' '),
						string);
				}
			});
		var columnData = A2(
			$elm$core$List$map,
			function (col) {
				return A2($elm$core$List$map, col.toString, items);
			},
			columns);
		var columnLengths = A2(
			$elm$core$List$map,
			function (colRows) {
				return A2(
					$elm$core$Maybe$withDefault,
					0,
					$elm$core$List$maximum(
						A2($elm$core$List$map, $elm$core$String$length, colRows)));
			},
			columnData);
		var paddedColumnData = A4(
			$elm$core$List$map3,
			F3(
				function (col, colLength, colStrings) {
					return A2(
						$elm$core$List$map,
						A2(padFn, colLength, col.align),
						colStrings);
				}),
			columns,
			columnLengths,
			columnData);
		return A3(
			$elm$core$List$map2,
			F2(
				function (item, rowCells) {
					return {
						item: item,
						renderedRow: A2($elm$core$String$join, '  ', rowCells)
					};
				}),
			items,
			$elm_explorations$test$MicroListExtra$transpose(paddedColumnData));
	});
var $elm_explorations$test$Test$Runner$Distribution$viewLabels = function (labels) {
	return $elm$core$List$isEmpty(labels) ? '<uncategorized>' : A2($elm$core$String$join, ', ', labels);
};
var $elm_explorations$test$Test$Runner$Distribution$formatAsciiTable = F2(
	function (runsElapsed, items) {
		return A2(
			$elm_explorations$test$AsciiTable$view,
			_List_fromArray(
				[
					{
					align: $elm_explorations$test$AsciiTable$AlignLeft,
					toString: function (_v0) {
						var labels = _v0.a;
						return '  ' + ($elm_explorations$test$Test$Runner$Distribution$viewLabels(labels) + ':');
					}
				},
					{
					align: $elm_explorations$test$AsciiTable$AlignRight,
					toString: function (_v1) {
						var percentage = _v1.c;
						return $elm$core$String$fromFloat(percentage) + '%';
					}
				},
					{
					align: $elm_explorations$test$AsciiTable$AlignRight,
					toString: function (_v2) {
						var count = _v2.b;
						return '(' + ($elm$core$String$fromInt(count) + 'x)');
					}
				},
					{
					align: $elm_explorations$test$AsciiTable$AlignLeft,
					toString: function (_v3) {
						var count = _v3.b;
						return $elm_explorations$test$Test$Runner$Distribution$barView(
							{count: count, runsElapsed: runsElapsed});
					}
				}
				]),
			items);
	});
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === 'RBNode_elm_builtin') {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === 'RBNode_elm_builtin') {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === 'RBNode_elm_builtin') {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$diff = F2(
	function (t1, t2) {
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (k, v, t) {
					return A2($elm$core$Dict$remove, k, t);
				}),
			t1,
			t2);
	});
var $elm$core$Set$diff = F2(
	function (_v0, _v1) {
		var dict1 = _v0.a;
		var dict2 = _v1.a;
		return $elm$core$Set$Set_elm_builtin(
			A2($elm$core$Dict$diff, dict1, dict2));
	});
var $elm$core$Set$fromList = function (list) {
	return A3($elm$core$List$foldl, $elm$core$Set$insert, $elm$core$Set$empty, list);
};
var $elm$core$Basics$not = _Basics_not;
var $elm_explorations$test$Test$Runner$Distribution$isStrictSubset = F2(
	function (all, combination) {
		var combinationSet = $elm$core$Set$fromList(combination);
		var containsCombinationFully = function (set) {
			return (!$elm$core$Set$isEmpty(
				A2($elm$core$Set$diff, set, combinationSet))) && $elm$core$Set$isEmpty(
				A2($elm$core$Set$diff, combinationSet, set));
		};
		var allSets = A2(
			$elm$core$List$map,
			A2($elm$core$Basics$composeR, $elm$core$Tuple$first, $elm$core$Set$fromList),
			all);
		return A2($elm$core$List$any, containsCombinationFully, allSets);
	});
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$List$partition = F2(
	function (pred, list) {
		var step = F2(
			function (x, _v0) {
				var trues = _v0.a;
				var falses = _v0.b;
				return pred(x) ? _Utils_Tuple2(
					A2($elm$core$List$cons, x, trues),
					falses) : _Utils_Tuple2(
					trues,
					A2($elm$core$List$cons, x, falses));
			});
		return A3(
			$elm$core$List$foldr,
			step,
			_Utils_Tuple2(_List_Nil, _List_Nil),
			list);
	});
var $elm$core$String$replace = F3(
	function (before, after, string) {
		return A2(
			$elm$core$String$join,
			after,
			A2($elm$core$String$split, before, string));
	});
var $elm$core$List$sortBy = _List_sortBy;
var $elm_explorations$test$MicroListExtra$findIndexHelp = F3(
	function (index, predicate, list) {
		findIndexHelp:
		while (true) {
			if (!list.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var x = list.a;
				var xs = list.b;
				if (predicate(x)) {
					return $elm$core$Maybe$Just(index);
				} else {
					var $temp$index = index + 1,
						$temp$predicate = predicate,
						$temp$list = xs;
					index = $temp$index;
					predicate = $temp$predicate;
					list = $temp$list;
					continue findIndexHelp;
				}
			}
		}
	});
var $elm_explorations$test$MicroListExtra$findIndex = $elm_explorations$test$MicroListExtra$findIndexHelp(0);
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $elm_explorations$test$MicroListExtra$splitAt = F2(
	function (n, xs) {
		return _Utils_Tuple2(
			A2($elm$core$List$take, n, xs),
			A2($elm$core$List$drop, n, xs));
	});
var $elm_explorations$test$MicroListExtra$splitWhen = F2(
	function (predicate, list) {
		return A2(
			$elm$core$Maybe$map,
			function (i) {
				return A2($elm_explorations$test$MicroListExtra$splitAt, i, list);
			},
			A2($elm_explorations$test$MicroListExtra$findIndex, predicate, list));
	});
var $elm_explorations$test$Test$Runner$Distribution$formatTable = function (_v0) {
	var runsElapsed = _v0.runsElapsed;
	var distributionCount = _v0.distributionCount;
	var runsElapsed_ = runsElapsed;
	var distributionList = $elm$core$Dict$toList(distributionCount);
	var distribution = A2(
		$elm$core$List$map,
		function (_v8) {
			var labels = _v8.a;
			var count = _v8.b;
			var percentage = $elm$core$Basics$round((count / runsElapsed_) * 1000) / 10;
			return _Utils_Tuple3(labels, count, percentage);
		},
		A2(
			$elm$core$List$filter,
			function (_v7) {
				var labels = _v7.a;
				var count = _v7.b;
				return !(($elm$core$List$length(labels) === 1) && ((!count) && A2($elm_explorations$test$Test$Runner$Distribution$isStrictSubset, distributionList, labels)));
			},
			distributionList));
	var _v1 = A2(
		$elm$core$List$partition,
		function (_v3) {
			var labels = _v3.a;
			return $elm$core$List$length(labels) <= 1;
		},
		A2(
			$elm$core$List$sortBy,
			function (_v2) {
				var count = _v2.b;
				return -count;
			},
			distribution));
	var baseRows = _v1.a;
	var combinationsRows = _v1.b;
	var reorderedTable = _Utils_ap(baseRows, combinationsRows);
	var rawTable = A2($elm_explorations$test$Test$Runner$Distribution$formatAsciiTable, runsElapsed_, reorderedTable);
	var _v4 = A2(
		$elm$core$Maybe$withDefault,
		_Utils_Tuple2(rawTable, _List_Nil),
		A2(
			$elm_explorations$test$MicroListExtra$splitWhen,
			function (_v5) {
				var item = _v5.item;
				var _v6 = item;
				var labels = _v6.a;
				return $elm$core$List$length(labels) > 1;
			},
			rawTable));
	var base = _v4.a;
	var combinations = _v4.b;
	var baseString = A2(
		$elm$core$String$join,
		'\n',
		A2(
			$elm$core$List$map,
			function ($) {
				return $.renderedRow;
			},
			base));
	var combinationsString_ = $elm$core$List$isEmpty(combinations) ? '' : A3(
		$elm$core$String$replace,
		'{COMBINATIONS}',
		A2(
			$elm$core$String$join,
			'\n',
			A2(
				$elm$core$List$map,
				function ($) {
					return $.renderedRow;
				},
				combinations)),
		'\n\nCombinations (included in the above base counts):\n{COMBINATIONS}');
	var table = _Utils_ap(baseString, combinationsString_);
	return A3($elm$core$String$replace, '{CATEGORIES}', table, 'Distribution report:\n====================\n{CATEGORIES}');
};
var $elm_explorations$test$Test$Distribution$distributionReportTable = function (r) {
	return $elm_explorations$test$Test$Runner$Distribution$formatTable(r);
};
var $author$project$Test$Reporter$Console$distributionReportToString = function (distributionReport) {
	switch (distributionReport.$) {
		case 'NoDistribution':
			return $elm$core$Maybe$Nothing;
		case 'DistributionToReport':
			var r = distributionReport.a;
			return $elm$core$Maybe$Just(
				$elm_explorations$test$Test$Distribution$distributionReportTable(r));
		case 'DistributionCheckSucceeded':
			return $elm$core$Maybe$Nothing;
		default:
			var r = distributionReport.a;
			return $elm$core$Maybe$Just(
				$elm_explorations$test$Test$Distribution$distributionReportTable(r));
	}
};
var $author$project$Console$Text$Texts = function (a) {
	return {$: 'Texts', a: a};
};
var $author$project$Console$Text$concat = $author$project$Console$Text$Texts;
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $author$project$Console$Text$Dark = {$: 'Dark'};
var $author$project$Console$Text$dark = function (txt) {
	if (txt.$ === 'Text') {
		var styles = txt.a;
		var str = txt.b;
		return A2(
			$author$project$Console$Text$Text,
			_Utils_update(
				styles,
				{
					modifiers: A2($elm$core$List$cons, $author$project$Console$Text$Dark, styles.modifiers)
				}),
			str);
	} else {
		var texts = txt.a;
		return $author$project$Console$Text$Texts(
			A2($elm$core$List$map, $author$project$Console$Text$dark, texts));
	}
};
var $elm_explorations$test$Test$Runner$formatLabels = F3(
	function (formatDescription, formatTest, labels) {
		var _v0 = A2(
			$elm$core$List$filter,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, $elm$core$String$isEmpty),
			labels);
		if (!_v0.b) {
			return _List_Nil;
		} else {
			var test = _v0.a;
			var descriptions = _v0.b;
			return $elm$core$List$reverse(
				A2(
					$elm$core$List$cons,
					formatTest(test),
					A2($elm$core$List$map, formatDescription, descriptions)));
		}
	});
var $author$project$Console$Text$Red = {$: 'Red'};
var $author$project$Console$Text$red = $author$project$Console$Text$Text(
	{background: $author$project$Console$Text$Default, foreground: $author$project$Console$Text$Red, modifiers: _List_Nil, style: $author$project$Console$Text$Normal});
var $author$project$Test$Reporter$Console$withChar = F2(
	function (icon, str) {
		return $elm$core$String$fromChar(icon) + (' ' + (str + '\n'));
	});
var $author$project$Test$Reporter$Console$failureLabelsToText = A2(
	$elm$core$Basics$composeR,
	A2(
		$elm_explorations$test$Test$Runner$formatLabels,
		A2(
			$elm$core$Basics$composeL,
			A2($elm$core$Basics$composeL, $author$project$Console$Text$dark, $author$project$Console$Text$plain),
			$author$project$Test$Reporter$Console$withChar(
				_Utils_chr('↓'))),
		A2(
			$elm$core$Basics$composeL,
			$author$project$Console$Text$red,
			$author$project$Test$Reporter$Console$withChar(
				_Utils_chr('✗')))),
	$author$project$Console$Text$concat);
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm$core$Array$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, list);
			var jsArray = _v0.a;
			var remainingItems = _v0.b;
			if (_Utils_cmp(
				$elm$core$Elm$JsArray$length(jsArray),
				$elm$core$Array$branchFactor) < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					true,
					{nodeList: nodeList, nodeListSize: nodeListSize, tail: jsArray});
			} else {
				var $temp$list = remainingItems,
					$temp$nodeList = A2(
					$elm$core$List$cons,
					$elm$core$Array$Leaf(jsArray),
					nodeList),
					$temp$nodeListSize = nodeListSize + 1;
				list = $temp$list;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue fromListHelp;
			}
		}
	});
var $elm$core$Array$fromList = function (list) {
	if (!list.b) {
		return $elm$core$Array$empty;
	} else {
		return A3($elm$core$Array$fromListHelp, list, _List_Nil, 0);
	}
};
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm$core$Array$bitMask = 4294967295 >>> (32 - $elm$core$Array$shiftStep);
var $elm$core$Basics$ge = _Utils_ge;
var $elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
var $elm$core$Array$getHelp = F3(
	function (shift, index, tree) {
		getHelp:
		while (true) {
			var pos = $elm$core$Array$bitMask & (index >>> shift);
			var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (_v0.$ === 'SubTree') {
				var subTree = _v0.a;
				var $temp$shift = shift - $elm$core$Array$shiftStep,
					$temp$index = index,
					$temp$tree = subTree;
				shift = $temp$shift;
				index = $temp$index;
				tree = $temp$tree;
				continue getHelp;
			} else {
				var values = _v0.a;
				return A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, values);
			}
		}
	});
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $elm$core$Array$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var $elm$core$Array$get = F2(
	function (index, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? $elm$core$Maybe$Nothing : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? $elm$core$Maybe$Just(
			A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, tail)) : $elm$core$Maybe$Just(
			A3($elm$core$Array$getHelp, startShift, index, tree)));
	});
var $elm$core$Array$length = function (_v0) {
	var len = _v0.a;
	return len;
};
var $author$project$Test$Runner$Node$Vendor$Diff$Added = function (a) {
	return {$: 'Added', a: a};
};
var $author$project$Test$Runner$Node$Vendor$Diff$CannotGetA = function (a) {
	return {$: 'CannotGetA', a: a};
};
var $author$project$Test$Runner$Node$Vendor$Diff$CannotGetB = function (a) {
	return {$: 'CannotGetB', a: a};
};
var $author$project$Test$Runner$Node$Vendor$Diff$NoChange = function (a) {
	return {$: 'NoChange', a: a};
};
var $author$project$Test$Runner$Node$Vendor$Diff$Removed = function (a) {
	return {$: 'Removed', a: a};
};
var $author$project$Test$Runner$Node$Vendor$Diff$UnexpectedPath = F2(
	function (a, b) {
		return {$: 'UnexpectedPath', a: a, b: b};
	});
var $author$project$Test$Runner$Node$Vendor$Diff$makeChangesHelp = F5(
	function (changes, getA, getB, _v0, path) {
		makeChangesHelp:
		while (true) {
			var x = _v0.a;
			var y = _v0.b;
			if (!path.b) {
				return $elm$core$Result$Ok(changes);
			} else {
				var _v2 = path.a;
				var prevX = _v2.a;
				var prevY = _v2.b;
				var tail = path.b;
				var change = function () {
					if (_Utils_eq(x - 1, prevX) && _Utils_eq(y - 1, prevY)) {
						var _v4 = getA(x);
						if (_v4.$ === 'Just') {
							var a = _v4.a;
							return $elm$core$Result$Ok(
								$author$project$Test$Runner$Node$Vendor$Diff$NoChange(a));
						} else {
							return $elm$core$Result$Err(
								$author$project$Test$Runner$Node$Vendor$Diff$CannotGetA(x));
						}
					} else {
						if (_Utils_eq(x, prevX)) {
							var _v5 = getB(y);
							if (_v5.$ === 'Just') {
								var b = _v5.a;
								return $elm$core$Result$Ok(
									$author$project$Test$Runner$Node$Vendor$Diff$Added(b));
							} else {
								return $elm$core$Result$Err(
									$author$project$Test$Runner$Node$Vendor$Diff$CannotGetB(y));
							}
						} else {
							if (_Utils_eq(y, prevY)) {
								var _v6 = getA(x);
								if (_v6.$ === 'Just') {
									var a = _v6.a;
									return $elm$core$Result$Ok(
										$author$project$Test$Runner$Node$Vendor$Diff$Removed(a));
								} else {
									return $elm$core$Result$Err(
										$author$project$Test$Runner$Node$Vendor$Diff$CannotGetA(x));
								}
							} else {
								return $elm$core$Result$Err(
									A2(
										$author$project$Test$Runner$Node$Vendor$Diff$UnexpectedPath,
										_Utils_Tuple2(x, y),
										path));
							}
						}
					}
				}();
				if (change.$ === 'Err') {
					var err = change.a;
					return $elm$core$Result$Err(err);
				} else {
					var c = change.a;
					var $temp$changes = A2($elm$core$List$cons, c, changes),
						$temp$getA = getA,
						$temp$getB = getB,
						$temp$_v0 = _Utils_Tuple2(prevX, prevY),
						$temp$path = tail;
					changes = $temp$changes;
					getA = $temp$getA;
					getB = $temp$getB;
					_v0 = $temp$_v0;
					path = $temp$path;
					continue makeChangesHelp;
				}
			}
		}
	});
var $author$project$Test$Runner$Node$Vendor$Diff$makeChanges = F3(
	function (getA, getB, path) {
		if (!path.b) {
			return $elm$core$Result$Ok(_List_Nil);
		} else {
			var latest = path.a;
			var tail = path.b;
			return A5($author$project$Test$Runner$Node$Vendor$Diff$makeChangesHelp, _List_Nil, getA, getB, latest, tail);
		}
	});
var $author$project$Test$Runner$Node$Vendor$Diff$Continue = function (a) {
	return {$: 'Continue', a: a};
};
var $author$project$Test$Runner$Node$Vendor$Diff$Found = function (a) {
	return {$: 'Found', a: a};
};
var $elm$core$Elm$JsArray$unsafeSet = _JsArray_unsafeSet;
var $elm$core$Array$setHelp = F4(
	function (shift, index, value, tree) {
		var pos = $elm$core$Array$bitMask & (index >>> shift);
		var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
		if (_v0.$ === 'SubTree') {
			var subTree = _v0.a;
			var newSub = A4($elm$core$Array$setHelp, shift - $elm$core$Array$shiftStep, index, value, subTree);
			return A3(
				$elm$core$Elm$JsArray$unsafeSet,
				pos,
				$elm$core$Array$SubTree(newSub),
				tree);
		} else {
			var values = _v0.a;
			var newLeaf = A3($elm$core$Elm$JsArray$unsafeSet, $elm$core$Array$bitMask & index, value, values);
			return A3(
				$elm$core$Elm$JsArray$unsafeSet,
				pos,
				$elm$core$Array$Leaf(newLeaf),
				tree);
		}
	});
var $elm$core$Array$set = F3(
	function (index, value, array) {
		var len = array.a;
		var startShift = array.b;
		var tree = array.c;
		var tail = array.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? array : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			tree,
			A3($elm$core$Elm$JsArray$unsafeSet, $elm$core$Array$bitMask & index, value, tail)) : A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			A4($elm$core$Array$setHelp, startShift, index, value, tree),
			tail));
	});
var $author$project$Test$Runner$Node$Vendor$Diff$step = F4(
	function (snake_, offset, k, v) {
		var fromTop = A2(
			$elm$core$Maybe$withDefault,
			_List_Nil,
			A2($elm$core$Array$get, (k + 1) + offset, v));
		var fromLeft = A2(
			$elm$core$Maybe$withDefault,
			_List_Nil,
			A2($elm$core$Array$get, (k - 1) + offset, v));
		var _v0 = function () {
			var _v2 = _Utils_Tuple2(fromLeft, fromTop);
			if (!_v2.a.b) {
				if (!_v2.b.b) {
					return _Utils_Tuple2(
						_List_Nil,
						_Utils_Tuple2(0, 0));
				} else {
					var _v3 = _v2.b;
					var _v4 = _v3.a;
					var topX = _v4.a;
					var topY = _v4.b;
					return _Utils_Tuple2(
						fromTop,
						_Utils_Tuple2(topX + 1, topY));
				}
			} else {
				if (!_v2.b.b) {
					var _v5 = _v2.a;
					var _v6 = _v5.a;
					var leftX = _v6.a;
					var leftY = _v6.b;
					return _Utils_Tuple2(
						fromLeft,
						_Utils_Tuple2(leftX, leftY + 1));
				} else {
					var _v7 = _v2.a;
					var _v8 = _v7.a;
					var leftX = _v8.a;
					var leftY = _v8.b;
					var _v9 = _v2.b;
					var _v10 = _v9.a;
					var topX = _v10.a;
					var topY = _v10.b;
					return (_Utils_cmp(leftY + 1, topY) > -1) ? _Utils_Tuple2(
						fromLeft,
						_Utils_Tuple2(leftX, leftY + 1)) : _Utils_Tuple2(
						fromTop,
						_Utils_Tuple2(topX + 1, topY));
				}
			}
		}();
		var path = _v0.a;
		var _v1 = _v0.b;
		var x = _v1.a;
		var y = _v1.b;
		var _v11 = A3(
			snake_,
			x + 1,
			y + 1,
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(x, y),
				path));
		var newPath = _v11.a;
		var goal = _v11.b;
		return goal ? $author$project$Test$Runner$Node$Vendor$Diff$Found(newPath) : $author$project$Test$Runner$Node$Vendor$Diff$Continue(
			A3($elm$core$Array$set, k + offset, newPath, v));
	});
var $author$project$Test$Runner$Node$Vendor$Diff$onpLoopK = F4(
	function (snake_, offset, ks, v) {
		onpLoopK:
		while (true) {
			if (!ks.b) {
				return $author$project$Test$Runner$Node$Vendor$Diff$Continue(v);
			} else {
				var k = ks.a;
				var ks_ = ks.b;
				var _v1 = A4($author$project$Test$Runner$Node$Vendor$Diff$step, snake_, offset, k, v);
				if (_v1.$ === 'Found') {
					var path = _v1.a;
					return $author$project$Test$Runner$Node$Vendor$Diff$Found(path);
				} else {
					var v_ = _v1.a;
					var $temp$snake_ = snake_,
						$temp$offset = offset,
						$temp$ks = ks_,
						$temp$v = v_;
					snake_ = $temp$snake_;
					offset = $temp$offset;
					ks = $temp$ks;
					v = $temp$v;
					continue onpLoopK;
				}
			}
		}
	});
var $author$project$Test$Runner$Node$Vendor$Diff$onpLoopP = F5(
	function (snake_, delta, offset, p, v) {
		onpLoopP:
		while (true) {
			var ks = (delta > 0) ? _Utils_ap(
				$elm$core$List$reverse(
					A2($elm$core$List$range, delta + 1, delta + p)),
				A2($elm$core$List$range, -p, delta)) : _Utils_ap(
				$elm$core$List$reverse(
					A2($elm$core$List$range, delta + 1, p)),
				A2($elm$core$List$range, (-p) + delta, delta));
			var _v0 = A4($author$project$Test$Runner$Node$Vendor$Diff$onpLoopK, snake_, offset, ks, v);
			if (_v0.$ === 'Found') {
				var path = _v0.a;
				return path;
			} else {
				var v_ = _v0.a;
				var $temp$snake_ = snake_,
					$temp$delta = delta,
					$temp$offset = offset,
					$temp$p = p + 1,
					$temp$v = v_;
				snake_ = $temp$snake_;
				delta = $temp$delta;
				offset = $temp$offset;
				p = $temp$p;
				v = $temp$v;
				continue onpLoopP;
			}
		}
	});
var $author$project$Test$Runner$Node$Vendor$Diff$snake = F5(
	function (getA, getB, nextX, nextY, path) {
		snake:
		while (true) {
			var _v0 = _Utils_Tuple2(
				getA(nextX),
				getB(nextY));
			_v0$2:
			while (true) {
				if (_v0.a.$ === 'Just') {
					if (_v0.b.$ === 'Just') {
						var a = _v0.a.a;
						var b = _v0.b.a;
						if (_Utils_eq(a, b)) {
							var $temp$getA = getA,
								$temp$getB = getB,
								$temp$nextX = nextX + 1,
								$temp$nextY = nextY + 1,
								$temp$path = A2(
								$elm$core$List$cons,
								_Utils_Tuple2(nextX, nextY),
								path);
							getA = $temp$getA;
							getB = $temp$getB;
							nextX = $temp$nextX;
							nextY = $temp$nextY;
							path = $temp$path;
							continue snake;
						} else {
							return _Utils_Tuple2(path, false);
						}
					} else {
						break _v0$2;
					}
				} else {
					if (_v0.b.$ === 'Nothing') {
						var _v1 = _v0.a;
						var _v2 = _v0.b;
						return _Utils_Tuple2(path, true);
					} else {
						break _v0$2;
					}
				}
			}
			return _Utils_Tuple2(path, false);
		}
	});
var $author$project$Test$Runner$Node$Vendor$Diff$onp = F4(
	function (getA, getB, m, n) {
		var v = A2(
			$elm$core$Array$initialize,
			(m + n) + 1,
			$elm$core$Basics$always(_List_Nil));
		var delta = n - m;
		return A5(
			$author$project$Test$Runner$Node$Vendor$Diff$onpLoopP,
			A2($author$project$Test$Runner$Node$Vendor$Diff$snake, getA, getB),
			delta,
			m,
			0,
			v);
	});
var $author$project$Test$Runner$Node$Vendor$Diff$testDiff = F2(
	function (a, b) {
		var arrB = $elm$core$Array$fromList(b);
		var getB = function (y) {
			return A2($elm$core$Array$get, y - 1, arrB);
		};
		var n = $elm$core$Array$length(arrB);
		var arrA = $elm$core$Array$fromList(a);
		var getA = function (x) {
			return A2($elm$core$Array$get, x - 1, arrA);
		};
		var m = $elm$core$Array$length(arrA);
		var path = A4($author$project$Test$Runner$Node$Vendor$Diff$onp, getA, getB, m, n);
		return A3($author$project$Test$Runner$Node$Vendor$Diff$makeChanges, getA, getB, path);
	});
var $author$project$Test$Runner$Node$Vendor$Diff$diff = F2(
	function (a, b) {
		var _v0 = A2($author$project$Test$Runner$Node$Vendor$Diff$testDiff, a, b);
		if (_v0.$ === 'Ok') {
			var changes = _v0.a;
			return changes;
		} else {
			return _List_Nil;
		}
	});
var $author$project$Test$Reporter$Highlightable$Highlighted = function (a) {
	return {$: 'Highlighted', a: a};
};
var $author$project$Test$Reporter$Highlightable$Plain = function (a) {
	return {$: 'Plain', a: a};
};
var $author$project$Test$Reporter$Highlightable$fromDiff = function (diff) {
	switch (diff.$) {
		case 'Added':
			return _List_Nil;
		case 'Removed':
			var _char = diff.a;
			return _List_fromArray(
				[
					$author$project$Test$Reporter$Highlightable$Highlighted(_char)
				]);
		default:
			var _char = diff.a;
			return _List_fromArray(
				[
					$author$project$Test$Reporter$Highlightable$Plain(_char)
				]);
	}
};
var $author$project$Test$Reporter$Highlightable$diffLists = F2(
	function (expected, actual) {
		return A2(
			$elm$core$List$concatMap,
			$author$project$Test$Reporter$Highlightable$fromDiff,
			A2($author$project$Test$Runner$Node$Vendor$Diff$diff, expected, actual));
	});
var $elm$core$String$toFloat = _String_toFloat;
var $author$project$Test$Reporter$Console$Format$isFloat = function (str) {
	var _v0 = $elm$core$String$toFloat(str);
	if (_v0.$ === 'Just') {
		return true;
	} else {
		return false;
	}
};
var $author$project$Test$Reporter$Highlightable$map = F2(
	function (transform, highlightable) {
		if (highlightable.$ === 'Highlighted') {
			var val = highlightable.a;
			return $author$project$Test$Reporter$Highlightable$Highlighted(
				transform(val));
		} else {
			var val = highlightable.a;
			return $author$project$Test$Reporter$Highlightable$Plain(
				transform(val));
		}
	});
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $author$project$Test$Reporter$Highlightable$resolve = F2(
	function (_v0, highlightable) {
		var fromHighlighted = _v0.fromHighlighted;
		var fromPlain = _v0.fromPlain;
		if (highlightable.$ === 'Highlighted') {
			var val = highlightable.a;
			return fromHighlighted(val);
		} else {
			var val = highlightable.a;
			return fromPlain(val);
		}
	});
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $author$project$Test$Reporter$Console$Format$highlightEqual = F2(
	function (expected, actual) {
		if ((expected === '\"\"') || (actual === '\"\"')) {
			return $elm$core$Maybe$Nothing;
		} else {
			if ($author$project$Test$Reporter$Console$Format$isFloat(expected) && $author$project$Test$Reporter$Console$Format$isFloat(actual)) {
				return $elm$core$Maybe$Nothing;
			} else {
				var isHighlighted = $author$project$Test$Reporter$Highlightable$resolve(
					{
						fromHighlighted: $elm$core$Basics$always(true),
						fromPlain: $elm$core$Basics$always(false)
					});
				var expectedChars = $elm$core$String$toList(expected);
				var edgeCount = function (highlightedString) {
					var highlights = A2($elm$core$List$map, isHighlighted, highlightedString);
					return $elm$core$List$length(
						A2(
							$elm$core$List$filter,
							function (_v0) {
								var lhs = _v0.a;
								var rhs = _v0.b;
								return !_Utils_eq(lhs, rhs);
							},
							A3(
								$elm$core$List$map2,
								$elm$core$Tuple$pair,
								A2($elm$core$List$drop, 1, highlights),
								highlights)));
				};
				var actualChars = $elm$core$String$toList(actual);
				var highlightedActual = A2(
					$elm$core$List$map,
					$author$project$Test$Reporter$Highlightable$map($elm$core$String$fromChar),
					A2($author$project$Test$Reporter$Highlightable$diffLists, actualChars, expectedChars));
				var highlightedExpected = A2(
					$elm$core$List$map,
					$author$project$Test$Reporter$Highlightable$map($elm$core$String$fromChar),
					A2($author$project$Test$Reporter$Highlightable$diffLists, expectedChars, actualChars));
				var plainCharCount = $elm$core$List$length(
					A2(
						$elm$core$List$filter,
						A2($elm$core$Basics$composeL, $elm$core$Basics$not, isHighlighted),
						highlightedExpected));
				return ((_Utils_cmp(
					edgeCount(highlightedActual),
					plainCharCount) > 0) || (_Utils_cmp(
					edgeCount(highlightedExpected),
					plainCharCount) > 0)) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
					_Utils_Tuple2(highlightedExpected, highlightedActual));
			}
		}
	});
var $author$project$Test$Reporter$Console$Format$verticalBar = F3(
	function (comparison, expected, actual) {
		return A2(
			$elm$core$String$join,
			'\n',
			_List_fromArray(
				[actual, '╷', '│ ' + comparison, '╵', expected]));
	});
var $author$project$Test$Reporter$Console$Format$listDiffToString = F4(
	function (index, description, _v0, originals) {
		listDiffToString:
		while (true) {
			var expected = _v0.expected;
			var actual = _v0.actual;
			var _v1 = _Utils_Tuple2(expected, actual);
			if (!_v1.a.b) {
				if (!_v1.b.b) {
					return A2(
						$elm$core$String$join,
						'',
						_List_fromArray(
							[
								'Two lists were unequal previously, yet ended up equal later.',
								'This should never happen!',
								'Please report this bug to https://github.com/elm-community/elm-test/issues - and include these lists: ',
								'\n',
								A2($elm$core$String$join, ', ', originals.originalExpected),
								'\n',
								A2($elm$core$String$join, ', ', originals.originalActual)
							]));
				} else {
					var _v3 = _v1.b;
					return A3(
						$author$project$Test$Reporter$Console$Format$verticalBar,
						description + ' was longer than',
						A2($elm$core$String$join, ', ', originals.originalExpected),
						A2($elm$core$String$join, ', ', originals.originalActual));
				}
			} else {
				if (!_v1.b.b) {
					var _v2 = _v1.a;
					return A3(
						$author$project$Test$Reporter$Console$Format$verticalBar,
						description + ' was shorter than',
						A2($elm$core$String$join, ', ', originals.originalExpected),
						A2($elm$core$String$join, ', ', originals.originalActual));
				} else {
					var _v4 = _v1.a;
					var firstExpected = _v4.a;
					var restExpected = _v4.b;
					var _v5 = _v1.b;
					var firstActual = _v5.a;
					var restActual = _v5.b;
					if (_Utils_eq(firstExpected, firstActual)) {
						var $temp$index = index + 1,
							$temp$description = description,
							$temp$_v0 = {actual: restActual, expected: restExpected},
							$temp$originals = originals;
						index = $temp$index;
						description = $temp$description;
						_v0 = $temp$_v0;
						originals = $temp$originals;
						continue listDiffToString;
					} else {
						return A2(
							$elm$core$String$join,
							'',
							_List_fromArray(
								[
									A3(
									$author$project$Test$Reporter$Console$Format$verticalBar,
									description,
									A2($elm$core$String$join, ', ', originals.originalExpected),
									A2($elm$core$String$join, ', ', originals.originalActual)),
									'\n\nThe first diff is at index ',
									$elm$core$String$fromInt(index),
									': it was `',
									firstActual,
									'`, but `',
									firstExpected,
									'` was expected.'
								]));
					}
				}
			}
		}
	});
var $author$project$Test$Reporter$Console$Format$format = F3(
	function (formatEquality, description, reason) {
		switch (reason.$) {
			case 'Custom':
				return description;
			case 'Equality':
				var expected = reason.a;
				var actual = reason.b;
				var _v1 = A2($author$project$Test$Reporter$Console$Format$highlightEqual, expected, actual);
				if (_v1.$ === 'Nothing') {
					return A3($author$project$Test$Reporter$Console$Format$verticalBar, description, expected, actual);
				} else {
					var _v2 = _v1.a;
					var highlightedExpected = _v2.a;
					var highlightedActual = _v2.b;
					var _v3 = A2(formatEquality, highlightedExpected, highlightedActual);
					var formattedExpected = _v3.a;
					var formattedActual = _v3.b;
					return A3($author$project$Test$Reporter$Console$Format$verticalBar, description, formattedExpected, formattedActual);
				}
			case 'Comparison':
				var first = reason.a;
				var second = reason.b;
				return A3($author$project$Test$Reporter$Console$Format$verticalBar, description, first, second);
			case 'TODO':
				return description;
			case 'Invalid':
				if (reason.a.$ === 'BadDescription') {
					var _v4 = reason.a;
					return (description === '') ? 'The empty string is not a valid test description.' : ('This is an invalid test description: ' + description);
				} else {
					return description;
				}
			case 'ListDiff':
				var expected = reason.a;
				var actual = reason.b;
				return A4(
					$author$project$Test$Reporter$Console$Format$listDiffToString,
					0,
					description,
					{actual: actual, expected: expected},
					{originalActual: actual, originalExpected: expected});
			default:
				var expected = reason.a.expected;
				var actual = reason.a.actual;
				var extra = reason.a.extra;
				var missing = reason.a.missing;
				var missingStr = $elm$core$List$isEmpty(missing) ? '' : ('\nThese keys are missing: ' + function (d) {
					return '[ ' + (d + ' ]');
				}(
					A2($elm$core$String$join, ', ', missing)));
				var extraStr = $elm$core$List$isEmpty(extra) ? '' : ('\nThese keys are extra: ' + function (d) {
					return '[ ' + (d + ' ]');
				}(
					A2($elm$core$String$join, ', ', extra)));
				return A2(
					$elm$core$String$join,
					'',
					_List_fromArray(
						[
							A3($author$project$Test$Reporter$Console$Format$verticalBar, description, expected, actual),
							'\n',
							extraStr,
							missingStr
						]));
		}
	});
var $author$project$Test$Reporter$Console$Format$Color$fromHighlightable = $author$project$Test$Reporter$Highlightable$resolve(
	{fromHighlighted: $author$project$Test$Runner$Node$Vendor$Console$colorsInverted, fromPlain: $elm$core$Basics$identity});
var $author$project$Test$Reporter$Console$Format$Color$formatEquality = F2(
	function (highlightedExpected, highlightedActual) {
		var formattedExpected = A2(
			$elm$core$String$join,
			'',
			A2($elm$core$List$map, $author$project$Test$Reporter$Console$Format$Color$fromHighlightable, highlightedExpected));
		var formattedActual = A2(
			$elm$core$String$join,
			'',
			A2($elm$core$List$map, $author$project$Test$Reporter$Console$Format$Color$fromHighlightable, highlightedActual));
		return _Utils_Tuple2(formattedExpected, formattedActual);
	});
var $author$project$Test$Reporter$Console$Format$Monochrome$fromHighlightable = function (indicator) {
	return $author$project$Test$Reporter$Highlightable$resolve(
		{
			fromHighlighted: function (_char) {
				return _Utils_Tuple2(_char, indicator);
			},
			fromPlain: function (_char) {
				return _Utils_Tuple2(_char, ' ');
			}
		});
};
var $elm$core$List$unzip = function (pairs) {
	var step = F2(
		function (_v0, _v1) {
			var x = _v0.a;
			var y = _v0.b;
			var xs = _v1.a;
			var ys = _v1.b;
			return _Utils_Tuple2(
				A2($elm$core$List$cons, x, xs),
				A2($elm$core$List$cons, y, ys));
		});
	return A3(
		$elm$core$List$foldr,
		step,
		_Utils_Tuple2(_List_Nil, _List_Nil),
		pairs);
};
var $author$project$Test$Reporter$Console$Format$Monochrome$formatEquality = F2(
	function (highlightedExpected, highlightedActual) {
		var _v0 = $elm$core$List$unzip(
			A2(
				$elm$core$List$map,
				$author$project$Test$Reporter$Console$Format$Monochrome$fromHighlightable('▲'),
				highlightedExpected));
		var formattedExpected = _v0.a;
		var expectedIndicators = _v0.b;
		var combinedExpected = A2(
			$elm$core$String$join,
			'\n',
			_List_fromArray(
				[
					A2($elm$core$String$join, '', formattedExpected),
					A2($elm$core$String$join, '', expectedIndicators)
				]));
		var _v1 = $elm$core$List$unzip(
			A2(
				$elm$core$List$map,
				$author$project$Test$Reporter$Console$Format$Monochrome$fromHighlightable('▼'),
				highlightedActual));
		var formattedActual = _v1.a;
		var actualIndicators = _v1.b;
		var combinedActual = A2(
			$elm$core$String$join,
			'\n',
			_List_fromArray(
				[
					A2($elm$core$String$join, '', actualIndicators),
					A2($elm$core$String$join, '', formattedActual)
				]));
		return _Utils_Tuple2(combinedExpected, combinedActual);
	});
var $author$project$Test$Reporter$Console$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n',
		A2(
			$elm$core$List$map,
			$elm$core$Basics$append('    '),
			A2($elm$core$String$split, '\n', str)));
};
var $author$project$Test$Reporter$Console$failureToText = F2(
	function (useColor, _v0) {
		var given = _v0.a.given;
		var description = _v0.a.description;
		var reason = _v0.a.reason;
		var distributionReport = _v0.b;
		var givenText = A2(
			$elm$core$Maybe$map,
			function (str) {
				return $author$project$Console$Text$dark(
					$author$project$Console$Text$plain('\nGiven ' + (str + '\n')));
			},
			given);
		var formatEquality = function () {
			if (useColor.$ === 'Monochrome') {
				return $author$project$Test$Reporter$Console$Format$Monochrome$formatEquality;
			} else {
				return $author$project$Test$Reporter$Console$Format$Color$formatEquality;
			}
		}();
		var messageText = $author$project$Console$Text$plain(
			'\n' + ($author$project$Test$Reporter$Console$indent(
				A3($author$project$Test$Reporter$Console$Format$format, formatEquality, description, reason)) + '\n\n'));
		var distributionText = A2(
			$elm$core$Maybe$map,
			function (str) {
				return $author$project$Console$Text$dark(
					$author$project$Console$Text$plain(
						'\n' + ($author$project$Test$Reporter$Console$indent(str) + '\n')));
			},
			$author$project$Test$Reporter$Console$distributionReportToString(distributionReport));
		return $author$project$Console$Text$concat(
			A2(
				$elm$core$List$filterMap,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						distributionText,
						givenText,
						$elm$core$Maybe$Just(messageText)
					])));
	});
var $author$project$Test$Reporter$Console$failuresToText = F3(
	function (useColor, labels, failures) {
		return $author$project$Console$Text$concat(
			A2(
				$elm$core$List$cons,
				$author$project$Test$Reporter$Console$failureLabelsToText(labels),
				A2(
					$elm$core$List$map,
					$author$project$Test$Reporter$Console$failureToText(useColor),
					failures)));
	});
var $author$project$Test$Reporter$Console$getStatus = function (outcome) {
	switch (outcome.$) {
		case 'Failed':
			return 'fail';
		case 'Todo':
			return 'todo';
		default:
			return 'pass';
	}
};
var $author$project$Console$Text$Green = {$: 'Green'};
var $author$project$Console$Text$green = $author$project$Console$Text$Text(
	{background: $author$project$Console$Text$Default, foreground: $author$project$Console$Text$Green, modifiers: _List_Nil, style: $author$project$Console$Text$Normal});
var $author$project$Test$Reporter$Console$passedLabelsToText = A2(
	$elm$core$Basics$composeR,
	A2(
		$elm_explorations$test$Test$Runner$formatLabels,
		A2(
			$elm$core$Basics$composeL,
			A2($elm$core$Basics$composeL, $author$project$Console$Text$dark, $author$project$Console$Text$plain),
			$author$project$Test$Reporter$Console$withChar(
				_Utils_chr('↓'))),
		A2(
			$elm$core$Basics$composeL,
			$author$project$Console$Text$green,
			$author$project$Test$Reporter$Console$withChar(
				_Utils_chr('✓')))),
	$author$project$Console$Text$concat);
var $author$project$Test$Reporter$Console$passedToText = F2(
	function (labels, distributionReport) {
		return $author$project$Console$Text$concat(
			_List_fromArray(
				[
					$author$project$Test$Reporter$Console$passedLabelsToText(labels),
					$author$project$Console$Text$dark(
					$author$project$Console$Text$plain(
						'\n' + ($author$project$Test$Reporter$Console$indent(distributionReport) + '\n\n')))
				]));
	});
var $author$project$Test$Reporter$Console$reportComplete = F2(
	function (useColor, _v0) {
		var labels = _v0.labels;
		var outcome = _v0.outcome;
		return $elm$json$Json$Encode$object(
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string('complete')),
				A2(
					$elm$core$List$cons,
					_Utils_Tuple2(
						'status',
						$elm$json$Json$Encode$string(
							$author$project$Test$Reporter$Console$getStatus(outcome))),
					function () {
						switch (outcome.$) {
							case 'Passed':
								var distributionReport = outcome.a;
								var _v2 = $author$project$Test$Reporter$Console$distributionReportToString(distributionReport);
								if (_v2.$ === 'Nothing') {
									return _List_Nil;
								} else {
									var report = _v2.a;
									return _List_fromArray(
										[
											_Utils_Tuple2(
											'distributionReport',
											A2(
												$author$project$Test$Reporter$Console$textToValue,
												useColor,
												A2($author$project$Test$Reporter$Console$passedToText, labels, report)))
										]);
								}
							case 'Failed':
								var failures = outcome.a;
								return _List_fromArray(
									[
										_Utils_Tuple2(
										'failure',
										A2(
											$author$project$Test$Reporter$Console$textToValue,
											useColor,
											A3($author$project$Test$Reporter$Console$failuresToText, useColor, labels, failures)))
									]);
							default:
								var str = outcome.a;
								return _List_fromArray(
									[
										_Utils_Tuple2(
										'todo',
										$elm$json$Json$Encode$string(str)),
										_Utils_Tuple2(
										'labels',
										A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, labels))
									]);
						}
					}())));
	});
var $author$project$Test$Reporter$JUnit$encodeDuration = function (time) {
	return $elm$json$Json$Encode$string(
		$elm$core$String$fromFloat(time / 1000));
};
var $author$project$Test$Reporter$JUnit$distributionReportToString = function (distributionReport) {
	switch (distributionReport.$) {
		case 'NoDistribution':
			return $elm$core$Maybe$Nothing;
		case 'DistributionToReport':
			var r = distributionReport.a;
			return $elm$core$Maybe$Just(
				$elm_explorations$test$Test$Distribution$distributionReportTable(r));
		case 'DistributionCheckSucceeded':
			return $elm$core$Maybe$Nothing;
		default:
			var r = distributionReport.a;
			return $elm$core$Maybe$Just(
				$elm_explorations$test$Test$Distribution$distributionReportTable(r));
	}
};
var $author$project$Test$Reporter$JUnit$encodeDistributionReport = function (reportText) {
	return _Utils_Tuple2(
		'system-out',
		$elm$json$Json$Encode$string(reportText));
};
var $author$project$Test$Reporter$JUnit$encodeFailureTuple = function (message) {
	return _Utils_Tuple2(
		'failure',
		$elm$json$Json$Encode$string(message));
};
var $author$project$Test$Reporter$JUnit$reasonToString = F2(
	function (description, reason) {
		switch (reason.$) {
			case 'Custom':
				return description;
			case 'Equality':
				var expected = reason.a;
				var actual = reason.b;
				return expected + ('\n\nwas not equal to\n\n' + actual);
			case 'Comparison':
				var first = reason.a;
				var second = reason.b;
				return first + ('\n\nfailed when compared with ' + (description + (' on\n\n' + second)));
			case 'TODO':
				return 'TODO: ' + description;
			case 'Invalid':
				if (reason.a.$ === 'BadDescription') {
					var _v1 = reason.a;
					var explanation = (description === '') ? 'The empty string is not a valid test description.' : ('This is an invalid test description: ' + description);
					return 'Invalid test: ' + explanation;
				} else {
					return 'Invalid test: ' + description;
				}
			case 'ListDiff':
				var expected = reason.a;
				var actual = reason.b;
				return A2($elm$core$String$join, ', ', expected) + ('\n\nhad different elements than\n\n' + A2($elm$core$String$join, ', ', actual));
			default:
				var expected = reason.a.expected;
				var actual = reason.a.actual;
				var extra = reason.a.extra;
				var missing = reason.a.missing;
				return expected + ('\n\nhad different contents than\n\n' + (actual + ('\n\nthese were extra:\n\n' + (A2($elm$core$String$join, '\n', extra) + ('\n\nthese were missing:\n\n' + A2($elm$core$String$join, '\n', missing))))));
		}
	});
var $author$project$Test$Reporter$JUnit$formatFailure = function (_v0) {
	var given = _v0.given;
	var description = _v0.description;
	var reason = _v0.reason;
	var message = A2($author$project$Test$Reporter$JUnit$reasonToString, description, reason);
	if (given.$ === 'Just') {
		var str = given.a;
		return 'Given ' + (str + ('\n\n' + message));
	} else {
		return message;
	}
};
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $elm$core$List$singleton = function (value) {
	return _List_fromArray(
		[value]);
};
var $author$project$Test$Reporter$JUnit$encodeOutcome = function (outcome) {
	switch (outcome.$) {
		case 'Passed':
			var distributionReport = outcome.a;
			return A2(
				$elm$core$Maybe$withDefault,
				_List_Nil,
				A2(
					$elm$core$Maybe$map,
					A2($elm$core$Basics$composeR, $author$project$Test$Reporter$JUnit$encodeDistributionReport, $elm$core$List$singleton),
					$author$project$Test$Reporter$JUnit$distributionReportToString(distributionReport)));
		case 'Failed':
			var failures = outcome.a;
			var message = A2(
				$elm$core$String$join,
				'\n\n\n',
				A2(
					$elm$core$List$map,
					A2($elm$core$Basics$composeR, $elm$core$Tuple$first, $author$project$Test$Reporter$JUnit$formatFailure),
					failures));
			var distributionReports = A2(
				$elm$core$String$join,
				'\n\n\n',
				A2(
					$elm$core$List$filterMap,
					A2($elm$core$Basics$composeR, $elm$core$Tuple$second, $author$project$Test$Reporter$JUnit$distributionReportToString),
					failures));
			var nonemptyDistributionReports = $elm$core$String$isEmpty(distributionReports) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(distributionReports);
			return A2(
				$elm$core$List$filterMap,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						$elm$core$Maybe$Just(
						$author$project$Test$Reporter$JUnit$encodeFailureTuple(message)),
						A2($elm$core$Maybe$map, $author$project$Test$Reporter$JUnit$encodeDistributionReport, nonemptyDistributionReports)
					]));
		default:
			var message = outcome.a;
			return _List_fromArray(
				[
					$author$project$Test$Reporter$JUnit$encodeFailureTuple('TODO: ' + message)
				]);
	}
};
var $author$project$Test$Reporter$JUnit$formatClassAndName = function (labels) {
	if (labels.b) {
		var head = labels.a;
		var rest = labels.b;
		return _Utils_Tuple2(
			A2(
				$elm$core$String$join,
				' ',
				$elm$core$List$reverse(rest)),
			head);
	} else {
		return _Utils_Tuple2('', '');
	}
};
var $author$project$Test$Reporter$JUnit$reportComplete = function (_v0) {
	var labels = _v0.labels;
	var duration = _v0.duration;
	var outcome = _v0.outcome;
	var _v1 = $author$project$Test$Reporter$JUnit$formatClassAndName(labels);
	var classname = _v1.a;
	var name = _v1.b;
	return $elm$json$Json$Encode$object(
		_Utils_ap(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'@classname',
					$elm$json$Json$Encode$string(classname)),
					_Utils_Tuple2(
					'@name',
					$elm$json$Json$Encode$string(name)),
					_Utils_Tuple2(
					'@time',
					$author$project$Test$Reporter$JUnit$encodeDuration(duration))
				]),
			$author$project$Test$Reporter$JUnit$encodeOutcome(outcome)));
};
var $elm$json$Json$Encode$int = _Json_wrap;
var $author$project$Test$Reporter$Json$encodeDistributionCount = function (dict) {
	return A2(
		$elm$json$Json$Encode$list,
		function (_v0) {
			var labels = _v0.a;
			var count = _v0.b;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'labels',
						A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, labels)),
						_Utils_Tuple2(
						'count',
						$elm$json$Json$Encode$int(count))
					]));
		},
		$elm$core$Dict$toList(dict));
};
var $author$project$Test$Reporter$Json$encodeSumType = F2(
	function (sumType, data) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string(sumType)),
					_Utils_Tuple2('data', data)
				]));
	});
var $elm$json$Json$Encode$float = _Json_wrap;
var $elm$json$Json$Encode$null = _Json_encodeNull;
var $author$project$Test$Reporter$Json$encodeDistributionReport = function (distributionReport) {
	switch (distributionReport.$) {
		case 'NoDistribution':
			return A2($author$project$Test$Reporter$Json$encodeSumType, 'NoDistribution', $elm$json$Json$Encode$null);
		case 'DistributionToReport':
			var r = distributionReport.a;
			return A2(
				$author$project$Test$Reporter$Json$encodeSumType,
				'DistributionToReport',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'distributionCount',
							$author$project$Test$Reporter$Json$encodeDistributionCount(r.distributionCount)),
							_Utils_Tuple2(
							'runsElapsed',
							$elm$json$Json$Encode$int(r.runsElapsed))
						])));
		case 'DistributionCheckSucceeded':
			var r = distributionReport.a;
			return A2(
				$author$project$Test$Reporter$Json$encodeSumType,
				'DistributionCheckSucceeded',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'distributionCount',
							$author$project$Test$Reporter$Json$encodeDistributionCount(r.distributionCount)),
							_Utils_Tuple2(
							'runsElapsed',
							$elm$json$Json$Encode$int(r.runsElapsed))
						])));
		default:
			var r = distributionReport.a;
			return A2(
				$author$project$Test$Reporter$Json$encodeSumType,
				'DistributionCheckFailed',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'distributionCount',
							$author$project$Test$Reporter$Json$encodeDistributionCount(r.distributionCount)),
							_Utils_Tuple2(
							'runsElapsed',
							$elm$json$Json$Encode$int(r.runsElapsed)),
							_Utils_Tuple2(
							'badLabel',
							$elm$json$Json$Encode$string(r.badLabel)),
							_Utils_Tuple2(
							'badLabelPercentage',
							$elm$json$Json$Encode$float(r.badLabelPercentage)),
							_Utils_Tuple2(
							'expectedDistribution',
							$elm$json$Json$Encode$string(r.expectedDistribution))
						])));
	}
};
var $author$project$Test$Reporter$Json$encodeDistributionReports = function (outcome) {
	switch (outcome.$) {
		case 'Failed':
			var failures = outcome.a;
			return A2(
				$elm$core$List$map,
				A2($elm$core$Basics$composeR, $elm$core$Tuple$second, $author$project$Test$Reporter$Json$encodeDistributionReport),
				failures);
		case 'Todo':
			return _List_Nil;
		default:
			var distributionReport = outcome.a;
			return _List_fromArray(
				[
					$author$project$Test$Reporter$Json$encodeDistributionReport(distributionReport)
				]);
	}
};
var $author$project$Test$Reporter$Json$encodeReason = F2(
	function (description, reason) {
		switch (reason.$) {
			case 'Custom':
				return A2(
					$author$project$Test$Reporter$Json$encodeSumType,
					'Custom',
					$elm$json$Json$Encode$string(description));
			case 'Equality':
				var expected = reason.a;
				var actual = reason.b;
				return A2(
					$author$project$Test$Reporter$Json$encodeSumType,
					'Equality',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'expected',
								$elm$json$Json$Encode$string(expected)),
								_Utils_Tuple2(
								'actual',
								$elm$json$Json$Encode$string(actual)),
								_Utils_Tuple2(
								'comparison',
								$elm$json$Json$Encode$string(description))
							])));
			case 'Comparison':
				var first = reason.a;
				var second = reason.b;
				return A2(
					$author$project$Test$Reporter$Json$encodeSumType,
					'Comparison',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'first',
								$elm$json$Json$Encode$string(first)),
								_Utils_Tuple2(
								'second',
								$elm$json$Json$Encode$string(second)),
								_Utils_Tuple2(
								'comparison',
								$elm$json$Json$Encode$string(description))
							])));
			case 'TODO':
				return A2(
					$author$project$Test$Reporter$Json$encodeSumType,
					'TODO',
					$elm$json$Json$Encode$string(description));
			case 'Invalid':
				if (reason.a.$ === 'BadDescription') {
					var _v1 = reason.a;
					var explanation = (description === '') ? 'The empty string is not a valid test description.' : ('This is an invalid test description: ' + description);
					return A2(
						$author$project$Test$Reporter$Json$encodeSumType,
						'Invalid',
						$elm$json$Json$Encode$string(explanation));
				} else {
					return A2(
						$author$project$Test$Reporter$Json$encodeSumType,
						'Invalid',
						$elm$json$Json$Encode$string(description));
				}
			case 'ListDiff':
				var expected = reason.a;
				var actual = reason.b;
				return A2(
					$author$project$Test$Reporter$Json$encodeSumType,
					'ListDiff',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'expected',
								A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, expected)),
								_Utils_Tuple2(
								'actual',
								A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, actual))
							])));
			default:
				var expected = reason.a.expected;
				var actual = reason.a.actual;
				var extra = reason.a.extra;
				var missing = reason.a.missing;
				return A2(
					$author$project$Test$Reporter$Json$encodeSumType,
					'CollectionDiff',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'expected',
								$elm$json$Json$Encode$string(expected)),
								_Utils_Tuple2(
								'actual',
								$elm$json$Json$Encode$string(actual)),
								_Utils_Tuple2(
								'extra',
								A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, extra)),
								_Utils_Tuple2(
								'missing',
								A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, missing))
							])));
		}
	});
var $author$project$Test$Reporter$Json$encodeFailure = function (_v0) {
	var given = _v0.given;
	var description = _v0.description;
	var reason = _v0.reason;
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'given',
				A2(
					$elm$core$Maybe$withDefault,
					$elm$json$Json$Encode$null,
					A2($elm$core$Maybe$map, $elm$json$Json$Encode$string, given))),
				_Utils_Tuple2(
				'message',
				$elm$json$Json$Encode$string(description)),
				_Utils_Tuple2(
				'reason',
				A2($author$project$Test$Reporter$Json$encodeReason, description, reason))
			]));
};
var $author$project$Test$Reporter$Json$encodeFailures = function (outcome) {
	switch (outcome.$) {
		case 'Failed':
			var failures = outcome.a;
			return A2(
				$elm$core$List$map,
				A2($elm$core$Basics$composeR, $elm$core$Tuple$first, $author$project$Test$Reporter$Json$encodeFailure),
				failures);
		case 'Todo':
			var str = outcome.a;
			return _List_fromArray(
				[
					$elm$json$Json$Encode$string(str)
				]);
		default:
			return _List_Nil;
	}
};
var $author$project$Test$Reporter$Json$encodeLabels = function (labels) {
	return A2(
		$elm$json$Json$Encode$list,
		$elm$json$Json$Encode$string,
		$elm$core$List$reverse(labels));
};
var $author$project$Test$Reporter$Json$getStatus = function (outcome) {
	switch (outcome.$) {
		case 'Failed':
			return 'fail';
		case 'Todo':
			return 'todo';
		default:
			return 'pass';
	}
};
var $author$project$Test$Reporter$Json$reportComplete = function (_v0) {
	var duration = _v0.duration;
	var labels = _v0.labels;
	var outcome = _v0.outcome;
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'event',
				$elm$json$Json$Encode$string('testCompleted')),
				_Utils_Tuple2(
				'status',
				$elm$json$Json$Encode$string(
					$author$project$Test$Reporter$Json$getStatus(outcome))),
				_Utils_Tuple2(
				'labels',
				$author$project$Test$Reporter$Json$encodeLabels(labels)),
				_Utils_Tuple2(
				'failures',
				A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					$author$project$Test$Reporter$Json$encodeFailures(outcome))),
				_Utils_Tuple2(
				'distributionReports',
				A2(
					$elm$json$Json$Encode$list,
					$elm$core$Basics$identity,
					$author$project$Test$Reporter$Json$encodeDistributionReports(outcome))),
				_Utils_Tuple2(
				'duration',
				$elm$json$Json$Encode$string(
					$elm$core$String$fromInt(duration)))
			]));
};
var $author$project$Test$Reporter$Console$formatDuration = function (time) {
	return $elm$core$String$fromFloat(time) + ' ms';
};
var $author$project$Test$Reporter$Console$stat = F2(
	function (label, value) {
		return $author$project$Console$Text$concat(
			_List_fromArray(
				[
					$author$project$Console$Text$dark(
					$author$project$Console$Text$plain(label)),
					$author$project$Console$Text$plain(value + '\n')
				]));
	});
var $author$project$Test$Reporter$Console$todoLabelsToText = A2(
	$elm$core$Basics$composeR,
	A2(
		$elm_explorations$test$Test$Runner$formatLabels,
		A2(
			$elm$core$Basics$composeL,
			A2($elm$core$Basics$composeL, $author$project$Console$Text$dark, $author$project$Console$Text$plain),
			$author$project$Test$Reporter$Console$withChar(
				_Utils_chr('↓'))),
		A2(
			$elm$core$Basics$composeL,
			A2($elm$core$Basics$composeL, $author$project$Console$Text$dark, $author$project$Console$Text$plain),
			$author$project$Test$Reporter$Console$withChar(
				_Utils_chr('↓')))),
	$author$project$Console$Text$concat);
var $author$project$Test$Reporter$Console$todoToChalk = function (message) {
	return $author$project$Console$Text$plain('◦ TODO: ' + (message + '\n\n'));
};
var $author$project$Test$Reporter$Console$todosToText = function (_v0) {
	var labels = _v0.a;
	var failure = _v0.b;
	return $author$project$Console$Text$concat(
		_List_fromArray(
			[
				$author$project$Test$Reporter$Console$todoLabelsToText(labels),
				$author$project$Test$Reporter$Console$todoToChalk(failure)
			]));
};
var $author$project$Test$Reporter$Console$summarizeTodos = A2(
	$elm$core$Basics$composeR,
	$elm$core$List$map($author$project$Test$Reporter$Console$todosToText),
	$author$project$Console$Text$concat);
var $author$project$Console$Text$Underline = {$: 'Underline'};
var $author$project$Console$Text$underline = function (txt) {
	if (txt.$ === 'Text') {
		var styles = txt.a;
		var str = txt.b;
		return A2(
			$author$project$Console$Text$Text,
			_Utils_update(
				styles,
				{style: $author$project$Console$Text$Underline}),
			str);
	} else {
		var texts = txt.a;
		return $author$project$Console$Text$Texts(
			A2($elm$core$List$map, $author$project$Console$Text$dark, texts));
	}
};
var $author$project$Console$Text$Yellow = {$: 'Yellow'};
var $author$project$Console$Text$yellow = $author$project$Console$Text$Text(
	{background: $author$project$Console$Text$Default, foreground: $author$project$Console$Text$Yellow, modifiers: _List_Nil, style: $author$project$Console$Text$Normal});
var $author$project$Test$Reporter$Console$reportSummary = F3(
	function (useColor, _v0, autoFail) {
		var todos = _v0.todos;
		var passed = _v0.passed;
		var failed = _v0.failed;
		var duration = _v0.duration;
		var todoStats = function () {
			var _v7 = $elm$core$List$length(todos);
			if (!_v7) {
				return $author$project$Console$Text$plain('');
			} else {
				var numTodos = _v7;
				return A2(
					$author$project$Test$Reporter$Console$stat,
					'Todo:     ',
					$elm$core$String$fromInt(numTodos));
			}
		}();
		var individualTodos = (failed > 0) ? $author$project$Console$Text$plain('') : $author$project$Test$Reporter$Console$summarizeTodos(
			$elm$core$List$reverse(todos));
		var headlineResult = function () {
			var _v3 = _Utils_Tuple3(
				autoFail,
				failed,
				$elm$core$List$length(todos));
			_v3$4:
			while (true) {
				if (_v3.a.$ === 'Nothing') {
					if (!_v3.b) {
						switch (_v3.c) {
							case 0:
								var _v4 = _v3.a;
								return $elm$core$Result$Ok('TEST RUN PASSED');
							case 1:
								var _v5 = _v3.a;
								return $elm$core$Result$Err(
									_Utils_Tuple3($author$project$Console$Text$yellow, 'TEST RUN INCOMPLETE', ' because there is 1 TODO remaining'));
							default:
								var _v6 = _v3.a;
								var numTodos = _v3.c;
								return $elm$core$Result$Err(
									_Utils_Tuple3(
										$author$project$Console$Text$yellow,
										'TEST RUN INCOMPLETE',
										' because there are ' + ($elm$core$String$fromInt(numTodos) + ' TODOs remaining')));
						}
					} else {
						break _v3$4;
					}
				} else {
					if (!_v3.b) {
						var failure = _v3.a.a;
						return $elm$core$Result$Err(
							_Utils_Tuple3($author$project$Console$Text$yellow, 'TEST RUN INCOMPLETE', ' because ' + failure));
					} else {
						break _v3$4;
					}
				}
			}
			return $elm$core$Result$Err(
				_Utils_Tuple3($author$project$Console$Text$red, 'TEST RUN FAILED', ''));
		}();
		var headline = function () {
			if (headlineResult.$ === 'Ok') {
				var str = headlineResult.a;
				return $author$project$Console$Text$underline(
					$author$project$Console$Text$green('\n' + (str + '\n\n')));
			} else {
				var _v2 = headlineResult.a;
				var colorize = _v2.a;
				var str = _v2.b;
				var suffix = _v2.c;
				return $author$project$Console$Text$concat(
					_List_fromArray(
						[
							$author$project$Console$Text$underline(
							colorize('\n' + str)),
							colorize(suffix + '\n\n')
						]));
			}
		}();
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string('summary')),
					_Utils_Tuple2(
					'summary',
					$elm$json$Json$Encode$string(
						A2(
							$author$project$Console$Text$render,
							useColor,
							$author$project$Console$Text$concat(
								_List_fromArray(
									[
										headline,
										A2(
										$author$project$Test$Reporter$Console$stat,
										'Duration: ',
										$author$project$Test$Reporter$Console$formatDuration(duration)),
										A2(
										$author$project$Test$Reporter$Console$stat,
										'Passed:   ',
										$elm$core$String$fromInt(passed)),
										A2(
										$author$project$Test$Reporter$Console$stat,
										'Failed:   ',
										$elm$core$String$fromInt(failed)),
										todoStats,
										individualTodos
									])))))
				]));
	});
var $author$project$Test$Reporter$TestResults$Failed = function (a) {
	return {$: 'Failed', a: a};
};
var $author$project$Test$Reporter$JUnit$encodeExtraFailure = function (_v0) {
	return $author$project$Test$Reporter$JUnit$reportComplete(
		{
			duration: 0,
			labels: _List_Nil,
			outcome: $author$project$Test$Reporter$TestResults$Failed(_List_Nil)
		});
};
var $author$project$Test$Reporter$JUnit$reportSummary = F2(
	function (_v0, autoFail) {
		var testCount = _v0.testCount;
		var duration = _v0.duration;
		var failed = _v0.failed;
		var extraFailures = function () {
			var _v1 = _Utils_Tuple2(failed, autoFail);
			if ((!_v1.a) && (_v1.b.$ === 'Just')) {
				var failure = _v1.b.a;
				return _List_fromArray(
					[
						$author$project$Test$Reporter$JUnit$encodeExtraFailure(failure)
					]);
			} else {
				return _List_Nil;
			}
		}();
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'testsuite',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'@name',
								$elm$json$Json$Encode$string('elm-test')),
								_Utils_Tuple2(
								'@package',
								$elm$json$Json$Encode$string('elm-test')),
								_Utils_Tuple2(
								'@tests',
								$elm$json$Json$Encode$int(testCount)),
								_Utils_Tuple2(
								'@failures',
								$elm$json$Json$Encode$int(failed)),
								_Utils_Tuple2(
								'@errors',
								$elm$json$Json$Encode$int(0)),
								_Utils_Tuple2(
								'@time',
								$elm$json$Json$Encode$float(duration)),
								_Utils_Tuple2(
								'testcase',
								A2($elm$json$Json$Encode$list, $elm$core$Basics$identity, extraFailures))
							])))
				]));
	});
var $author$project$Test$Reporter$Json$reportSummary = F2(
	function (_v0, autoFail) {
		var duration = _v0.duration;
		var passed = _v0.passed;
		var failed = _v0.failed;
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'event',
					$elm$json$Json$Encode$string('runComplete')),
					_Utils_Tuple2(
					'passed',
					$elm$json$Json$Encode$string(
						$elm$core$String$fromInt(passed))),
					_Utils_Tuple2(
					'failed',
					$elm$json$Json$Encode$string(
						$elm$core$String$fromInt(failed))),
					_Utils_Tuple2(
					'duration',
					$elm$json$Json$Encode$string(
						$elm$core$String$fromFloat(duration))),
					_Utils_Tuple2(
					'autoFail',
					A2(
						$elm$core$Maybe$withDefault,
						$elm$json$Json$Encode$null,
						A2($elm$core$Maybe$map, $elm$json$Json$Encode$string, autoFail)))
				]));
	});
var $author$project$Test$Reporter$Reporter$createReporter = function (report) {
	switch (report.$) {
		case 'JsonReport':
			return A4($author$project$Test$Reporter$Reporter$TestReporter, 'JSON', $author$project$Test$Reporter$Json$reportBegin, $author$project$Test$Reporter$Json$reportComplete, $author$project$Test$Reporter$Json$reportSummary);
		case 'ConsoleReport':
			var useColor = report.a;
			return A4(
				$author$project$Test$Reporter$Reporter$TestReporter,
				'CHALK',
				$author$project$Test$Reporter$Console$reportBegin(useColor),
				$author$project$Test$Reporter$Console$reportComplete(useColor),
				$author$project$Test$Reporter$Console$reportSummary(useColor));
		default:
			return A4($author$project$Test$Reporter$Reporter$TestReporter, 'JUNIT', $author$project$Test$Reporter$JUnit$reportBegin, $author$project$Test$Reporter$JUnit$reportComplete, $author$project$Test$Reporter$JUnit$reportSummary);
	}
};
var $author$project$Test$Runner$Node$elmTestPort__send = _Platform_outgoingPort('elmTestPort__send', $elm$json$Json$Encode$string);
var $author$project$Test$Runner$Node$failInit = F3(
	function (message, report, _v0) {
		var model = {
			autoFail: $elm$core$Maybe$Nothing,
			available: $elm$core$Dict$empty,
			nextTestToRun: 0,
			processes: 0,
			results: _List_Nil,
			runInfo: {fuzzRuns: 0, globs: _List_Nil, initialSeed: 0, paths: _List_Nil, testCount: 0},
			testReporter: $author$project$Test$Reporter$Reporter$createReporter(report)
		};
		var cmd = $author$project$Test$Runner$Node$elmTestPort__send(
			A2(
				$elm$json$Json$Encode$encode,
				0,
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'type',
							$elm$json$Json$Encode$string('SUMMARY')),
							_Utils_Tuple2(
							'exitCode',
							$elm$json$Json$Encode$int(1)),
							_Utils_Tuple2(
							'message',
							$elm$json$Json$Encode$string(message))
						]))));
		return _Utils_Tuple2(model, cmd);
	});
var $elm_explorations$test$Test$Runner$Invalid = function (a) {
	return {$: 'Invalid', a: a};
};
var $elm_explorations$test$Test$Runner$Only = function (a) {
	return {$: 'Only', a: a};
};
var $elm_explorations$test$Test$Runner$Plain = function (a) {
	return {$: 'Plain', a: a};
};
var $elm_explorations$test$Test$Runner$Skipping = function (a) {
	return {$: 'Skipping', a: a};
};
var $elm_explorations$test$Test$Runner$countRunnables = function (runnable) {
	countRunnables:
	while (true) {
		if (runnable.$ === 'Runnable') {
			return 1;
		} else {
			var runner = runnable.b;
			var $temp$runnable = runner;
			runnable = $temp$runnable;
			continue countRunnables;
		}
	}
};
var $elm_explorations$test$Test$Runner$countAllRunnables = A2(
	$elm$core$List$foldl,
	A2($elm$core$Basics$composeR, $elm_explorations$test$Test$Runner$countRunnables, $elm$core$Basics$add),
	0);
var $elm_explorations$test$Test$Runner$Labeled = F2(
	function (a, b) {
		return {$: 'Labeled', a: a, b: b};
	});
var $elm_explorations$test$Test$Runner$Runnable = function (a) {
	return {$: 'Runnable', a: a};
};
var $elm_explorations$test$Test$Runner$Thunk = function (a) {
	return {$: 'Thunk', a: a};
};
var $elm_explorations$test$Test$Runner$emptyDistribution = function (seed) {
	return {all: _List_Nil, only: _List_Nil, seed: seed, skipped: _List_Nil};
};
var $elm$core$Bitwise$xor = _Bitwise_xor;
var $elm_explorations$test$Test$Runner$fnvHash = F2(
	function (a, b) {
		return ((a ^ b) * 16777619) >>> 0;
	});
var $elm_explorations$test$Test$Runner$fnvHashString = F2(
	function (hash, str) {
		return A3(
			$elm$core$List$foldl,
			$elm_explorations$test$Test$Runner$fnvHash,
			hash,
			A2(
				$elm$core$List$map,
				$elm$core$Char$toCode,
				$elm$core$String$toList(str)));
	});
var $elm_explorations$test$Test$Runner$fnvInit = 2166136261;
var $elm$random$Random$Generator = function (a) {
	return {$: 'Generator', a: a};
};
var $elm$random$Random$Seed = F2(
	function (a, b) {
		return {$: 'Seed', a: a, b: b};
	});
var $elm$random$Random$next = function (_v0) {
	var state0 = _v0.a;
	var incr = _v0.b;
	return A2($elm$random$Random$Seed, ((state0 * 1664525) + incr) >>> 0, incr);
};
var $elm$random$Random$peel = function (_v0) {
	var state = _v0.a;
	var word = (state ^ (state >>> ((state >>> 28) + 4))) * 277803737;
	return ((word >>> 22) ^ word) >>> 0;
};
var $elm$random$Random$int = F2(
	function (a, b) {
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v0 = (_Utils_cmp(a, b) < 0) ? _Utils_Tuple2(a, b) : _Utils_Tuple2(b, a);
				var lo = _v0.a;
				var hi = _v0.b;
				var range = (hi - lo) + 1;
				if (!((range - 1) & range)) {
					return _Utils_Tuple2(
						(((range - 1) & $elm$random$Random$peel(seed0)) >>> 0) + lo,
						$elm$random$Random$next(seed0));
				} else {
					var threshhold = (((-range) >>> 0) % range) >>> 0;
					var accountForBias = function (seed) {
						accountForBias:
						while (true) {
							var x = $elm$random$Random$peel(seed);
							var seedN = $elm$random$Random$next(seed);
							if (_Utils_cmp(x, threshhold) < 0) {
								var $temp$seed = seedN;
								seed = $temp$seed;
								continue accountForBias;
							} else {
								return _Utils_Tuple2((x % range) + lo, seedN);
							}
						}
					};
					return accountForBias(seed0);
				}
			});
	});
var $elm$random$Random$map3 = F4(
	function (func, _v0, _v1, _v2) {
		var genA = _v0.a;
		var genB = _v1.a;
		var genC = _v2.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v3 = genA(seed0);
				var a = _v3.a;
				var seed1 = _v3.b;
				var _v4 = genB(seed1);
				var b = _v4.a;
				var seed2 = _v4.b;
				var _v5 = genC(seed2);
				var c = _v5.a;
				var seed3 = _v5.b;
				return _Utils_Tuple2(
					A3(func, a, b, c),
					seed3);
			});
	});
var $elm$core$Bitwise$or = _Bitwise_or;
var $elm$random$Random$step = F2(
	function (_v0, seed) {
		var generator = _v0.a;
		return generator(seed);
	});
var $elm$random$Random$independentSeed = $elm$random$Random$Generator(
	function (seed0) {
		var makeIndependentSeed = F3(
			function (state, b, c) {
				return $elm$random$Random$next(
					A2($elm$random$Random$Seed, state, (1 | (b ^ c)) >>> 0));
			});
		var gen = A2($elm$random$Random$int, 0, 4294967295);
		return A2(
			$elm$random$Random$step,
			A4($elm$random$Random$map3, makeIndependentSeed, gen, gen, gen),
			seed0);
	});
var $elm$random$Random$initialSeed = function (x) {
	var _v0 = $elm$random$Random$next(
		A2($elm$random$Random$Seed, 0, 1013904223));
	var state1 = _v0.a;
	var incr = _v0.b;
	var state2 = (state1 + x) >>> 0;
	return $elm$random$Random$next(
		A2($elm$random$Random$Seed, state2, incr));
};
var $elm$random$Random$maxInt = 2147483647;
var $elm_explorations$test$Test$Runner$batchDistribute = F4(
	function (hashed, runs, test, prev) {
		var next = A4($elm_explorations$test$Test$Runner$distributeSeedsHelp, hashed, runs, prev.seed, test);
		return {
			all: _Utils_ap(prev.all, next.all),
			only: _Utils_ap(prev.only, next.only),
			seed: next.seed,
			skipped: _Utils_ap(prev.skipped, next.skipped)
		};
	});
var $elm_explorations$test$Test$Runner$distributeSeedsHelp = F4(
	function (hashed, runs, seed, test) {
		switch (test.$) {
			case 'ElmTestVariant__UnitTest':
				var aRun = test.a;
				return {
					all: _List_fromArray(
						[
							$elm_explorations$test$Test$Runner$Runnable(
							$elm_explorations$test$Test$Runner$Thunk(
								function (_v1) {
									return aRun(_Utils_Tuple0);
								}))
						]),
					only: _List_Nil,
					seed: seed,
					skipped: _List_Nil
				};
			case 'ElmTestVariant__FuzzTest':
				var aRun = test.a;
				var _v2 = A2($elm$random$Random$step, $elm$random$Random$independentSeed, seed);
				var firstSeed = _v2.a;
				var nextSeed = _v2.b;
				return {
					all: _List_fromArray(
						[
							$elm_explorations$test$Test$Runner$Runnable(
							$elm_explorations$test$Test$Runner$Thunk(
								function (_v3) {
									return A2(aRun, firstSeed, runs);
								}))
						]),
					only: _List_Nil,
					seed: nextSeed,
					skipped: _List_Nil
				};
			case 'ElmTestVariant__Labeled':
				var description = test.a;
				var subTest = test.b;
				if (hashed) {
					var next = A4($elm_explorations$test$Test$Runner$distributeSeedsHelp, true, runs, seed, subTest);
					return {
						all: A2(
							$elm$core$List$map,
							$elm_explorations$test$Test$Runner$Labeled(description),
							next.all),
						only: A2(
							$elm$core$List$map,
							$elm_explorations$test$Test$Runner$Labeled(description),
							next.only),
						seed: next.seed,
						skipped: A2(
							$elm$core$List$map,
							$elm_explorations$test$Test$Runner$Labeled(description),
							next.skipped)
					};
				} else {
					var intFromSeed = A2(
						$elm$random$Random$step,
						A2($elm$random$Random$int, 0, $elm$random$Random$maxInt),
						seed).a;
					var hashedSeed = $elm$random$Random$initialSeed(
						A2(
							$elm_explorations$test$Test$Runner$fnvHash,
							intFromSeed,
							A2($elm_explorations$test$Test$Runner$fnvHashString, $elm_explorations$test$Test$Runner$fnvInit, description)));
					var next = A4($elm_explorations$test$Test$Runner$distributeSeedsHelp, true, runs, hashedSeed, subTest);
					return {
						all: A2(
							$elm$core$List$map,
							$elm_explorations$test$Test$Runner$Labeled(description),
							next.all),
						only: A2(
							$elm$core$List$map,
							$elm_explorations$test$Test$Runner$Labeled(description),
							next.only),
						seed: seed,
						skipped: A2(
							$elm$core$List$map,
							$elm_explorations$test$Test$Runner$Labeled(description),
							next.skipped)
					};
				}
			case 'ElmTestVariant__Skipped':
				var subTest = test.a;
				var next = A4($elm_explorations$test$Test$Runner$distributeSeedsHelp, hashed, runs, seed, subTest);
				return {all: _List_Nil, only: _List_Nil, seed: next.seed, skipped: next.all};
			case 'ElmTestVariant__Only':
				var subTest = test.a;
				var next = A4($elm_explorations$test$Test$Runner$distributeSeedsHelp, hashed, runs, seed, subTest);
				return _Utils_update(
					next,
					{only: next.all});
			default:
				var tests = test.a;
				return A3(
					$elm$core$List$foldl,
					A2($elm_explorations$test$Test$Runner$batchDistribute, hashed, runs),
					$elm_explorations$test$Test$Runner$emptyDistribution(seed),
					tests);
		}
	});
var $elm_explorations$test$Test$Runner$distributeSeeds = $elm_explorations$test$Test$Runner$distributeSeedsHelp(false);
var $elm_explorations$test$Test$Runner$Failure$Custom = {$: 'Custom'};
var $elm_explorations$test$Expect$fail = function (str) {
	return $elm_explorations$test$Test$Expectation$fail(
		{description: str, reason: $elm_explorations$test$Test$Runner$Failure$Custom});
};
var $elm_explorations$test$Test$Runner$runThunk = _Test_runThunk;
var $elm_explorations$test$Test$Runner$run = function (_v0) {
	var fn = _v0.a;
	var _v1 = $elm_explorations$test$Test$Runner$runThunk(fn);
	if (_v1.$ === 'Ok') {
		var test = _v1.a;
		return test;
	} else {
		var message = _v1.a;
		return _List_fromArray(
			[
				$elm_explorations$test$Expect$fail('This test failed because it threw an exception: \"' + (message + '\"'))
			]);
	}
};
var $elm_explorations$test$Test$Runner$fromRunnableTreeHelp = F2(
	function (labels, runner) {
		fromRunnableTreeHelp:
		while (true) {
			if (runner.$ === 'Runnable') {
				var runnable = runner.a;
				return _List_fromArray(
					[
						{
						labels: labels,
						run: function (_v1) {
							return $elm_explorations$test$Test$Runner$run(runnable);
						}
					}
					]);
			} else {
				var label = runner.a;
				var subRunner = runner.b;
				var $temp$labels = A2($elm$core$List$cons, label, labels),
					$temp$runner = subRunner;
				labels = $temp$labels;
				runner = $temp$runner;
				continue fromRunnableTreeHelp;
			}
		}
	});
var $elm_explorations$test$Test$Runner$fromRunnableTree = $elm_explorations$test$Test$Runner$fromRunnableTreeHelp(_List_Nil);
var $elm_explorations$test$Test$Runner$fromTest = F3(
	function (runs, seed, test) {
		if (runs < 1) {
			return $elm_explorations$test$Test$Runner$Invalid(
				'Test runner run count must be at least 1, not ' + $elm$core$String$fromInt(runs));
		} else {
			var distribution = A3($elm_explorations$test$Test$Runner$distributeSeeds, runs, seed, test);
			return $elm$core$List$isEmpty(distribution.only) ? ((!$elm_explorations$test$Test$Runner$countAllRunnables(distribution.skipped)) ? $elm_explorations$test$Test$Runner$Plain(
				A2($elm$core$List$concatMap, $elm_explorations$test$Test$Runner$fromRunnableTree, distribution.all)) : $elm_explorations$test$Test$Runner$Skipping(
				A2($elm$core$List$concatMap, $elm_explorations$test$Test$Runner$fromRunnableTree, distribution.all))) : $elm_explorations$test$Test$Runner$Only(
				A2($elm$core$List$concatMap, $elm_explorations$test$Test$Runner$fromRunnableTree, distribution.only));
		}
	});
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Test$Runner$Node$init = F2(
	function (_v0, _v1) {
		var processes = _v0.processes;
		var globs = _v0.globs;
		var paths = _v0.paths;
		var fuzzRuns = _v0.fuzzRuns;
		var initialSeed = _v0.initialSeed;
		var report = _v0.report;
		var runners = _v0.runners;
		var testReporter = $author$project$Test$Reporter$Reporter$createReporter(report);
		var _v2 = function () {
			switch (runners.$) {
				case 'Plain':
					var runnerList = runners.a;
					return {
						autoFail: $elm$core$Maybe$Nothing,
						indexedRunners: A2(
							$elm$core$List$indexedMap,
							F2(
								function (a, b) {
									return _Utils_Tuple2(a, b);
								}),
							runnerList)
					};
				case 'Only':
					var runnerList = runners.a;
					return {
						autoFail: $elm$core$Maybe$Just('Test.only was used'),
						indexedRunners: A2(
							$elm$core$List$indexedMap,
							F2(
								function (a, b) {
									return _Utils_Tuple2(a, b);
								}),
							runnerList)
					};
				case 'Skipping':
					var runnerList = runners.a;
					return {
						autoFail: $elm$core$Maybe$Just('Test.skip was used'),
						indexedRunners: A2(
							$elm$core$List$indexedMap,
							F2(
								function (a, b) {
									return _Utils_Tuple2(a, b);
								}),
							runnerList)
					};
				default:
					var str = runners.a;
					return {
						autoFail: $elm$core$Maybe$Just(str),
						indexedRunners: _List_Nil
					};
			}
		}();
		var indexedRunners = _v2.indexedRunners;
		var autoFail = _v2.autoFail;
		var testCount = $elm$core$List$length(indexedRunners);
		var model = {
			autoFail: autoFail,
			available: $elm$core$Dict$fromList(indexedRunners),
			nextTestToRun: 0,
			processes: processes,
			results: _List_Nil,
			runInfo: {fuzzRuns: fuzzRuns, globs: globs, initialSeed: initialSeed, paths: paths, testCount: testCount},
			testReporter: testReporter
		};
		return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
	});
var $author$project$Test$Runner$Node$noTestsFoundError = function (globs) {
	return $elm$core$List$isEmpty(globs) ? $elm$core$String$trim('\nNo exposed values of type Test found in the tests/ directory.\n\nAre there tests in any .elm file in the tests/ directory?\nIf not – add some!\nIf there are – are they exposed?\n        ') : A3(
		$elm$core$String$replace,
		'%globs',
		A2($elm$core$String$join, '\n', globs),
		$elm$core$String$trim('\nNo exposed values of type Test found in files matching:\n\n%globs\n\nAre the above patterns correct? Maybe try running elm-test with no arguments?\n\nAre there tests in any of the matched files?\nIf not – add some!\nIf there are – are they exposed?\n        '));
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $author$project$Test$Runner$Node$Dispatch = function (a) {
	return {$: 'Dispatch', a: a};
};
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $author$project$Test$Runner$JsMessage$Summary = F3(
	function (a, b, c) {
		return {$: 'Summary', a: a, b: b, c: c};
	});
var $author$project$Test$Runner$JsMessage$Test = function (a) {
	return {$: 'Test', a: a};
};
var $elm$json$Json$Decode$fail = _Json_fail;
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $elm$json$Json$Decode$list = _Json_decodeList;
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map3 = _Json_map3;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Test$Runner$JsMessage$todoDecoder = A3(
	$elm$json$Json$Decode$map2,
	F2(
		function (a, b) {
			return _Utils_Tuple2(a, b);
		}),
	A2(
		$elm$json$Json$Decode$field,
		'labels',
		$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
	A2($elm$json$Json$Decode$field, 'todo', $elm$json$Json$Decode$string));
var $author$project$Test$Runner$JsMessage$decodeMessageFromType = function (messageType) {
	switch (messageType) {
		case 'TEST':
			return A2(
				$elm$json$Json$Decode$map,
				$author$project$Test$Runner$JsMessage$Test,
				A2($elm$json$Json$Decode$field, 'index', $elm$json$Json$Decode$int));
		case 'SUMMARY':
			return A4(
				$elm$json$Json$Decode$map3,
				$author$project$Test$Runner$JsMessage$Summary,
				A2($elm$json$Json$Decode$field, 'duration', $elm$json$Json$Decode$float),
				A2($elm$json$Json$Decode$field, 'failures', $elm$json$Json$Decode$int),
				A2(
					$elm$json$Json$Decode$field,
					'todos',
					$elm$json$Json$Decode$list($author$project$Test$Runner$JsMessage$todoDecoder)));
		default:
			return $elm$json$Json$Decode$fail('Unrecognized message type: ' + messageType);
	}
};
var $author$project$Test$Runner$JsMessage$decoder = A2(
	$elm$json$Json$Decode$andThen,
	$author$project$Test$Runner$JsMessage$decodeMessageFromType,
	A2($elm$json$Json$Decode$field, 'type', $elm$json$Json$Decode$string));
var $author$project$Test$Runner$Node$Complete = F4(
	function (a, b, c, d) {
		return {$: 'Complete', a: a, b: b, c: c, d: d};
	});
var $elm$time$Time$Name = function (a) {
	return {$: 'Name', a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 'Offset', a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $author$project$Test$Reporter$TestResults$Passed = function (a) {
	return {$: 'Passed', a: a};
};
var $author$project$Test$Reporter$TestResults$Todo = function (a) {
	return {$: 'Todo', a: a};
};
var $elm_explorations$test$Test$Runner$getDistributionReport = function (expectation) {
	if (expectation.$ === 'Pass') {
		var distributionReport = expectation.a.distributionReport;
		return distributionReport;
	} else {
		var distributionReport = expectation.a.distributionReport;
		return distributionReport;
	}
};
var $elm_explorations$test$Test$Runner$getFailureReason = function (expectation) {
	if (expectation.$ === 'Pass') {
		return $elm$core$Maybe$Nothing;
	} else {
		var record = expectation.a;
		return $elm$core$Maybe$Just(
			{description: record.description, given: record.given, reason: record.reason});
	}
};
var $elm_explorations$test$Test$Runner$Failure$TODO = {$: 'TODO'};
var $elm_explorations$test$Test$Runner$isTodo = function (expectation) {
	if (expectation.$ === 'Pass') {
		return false;
	} else {
		var reason = expectation.a.reason;
		return _Utils_eq(reason, $elm_explorations$test$Test$Runner$Failure$TODO);
	}
};
var $author$project$Test$Reporter$TestResults$outcomesFromExpectationsHelp = F2(
	function (expectation, builder) {
		var _v0 = $elm_explorations$test$Test$Runner$getFailureReason(expectation);
		if (_v0.$ === 'Just') {
			var failure = _v0.a;
			return $elm_explorations$test$Test$Runner$isTodo(expectation) ? _Utils_update(
				builder,
				{
					todos: A2($elm$core$List$cons, failure.description, builder.todos)
				}) : _Utils_update(
				builder,
				{
					failures: A2(
						$elm$core$List$cons,
						_Utils_Tuple2(
							failure,
							$elm_explorations$test$Test$Runner$getDistributionReport(expectation)),
						builder.failures)
				});
		} else {
			return _Utils_update(
				builder,
				{
					passes: A2(
						$elm$core$List$cons,
						$elm_explorations$test$Test$Runner$getDistributionReport(expectation),
						builder.passes)
				});
		}
	});
var $author$project$Test$Reporter$TestResults$outcomesFromExpectations = function (expectations) {
	if (expectations.b) {
		if (!expectations.b.b) {
			var expectation = expectations.a;
			var _v1 = $elm_explorations$test$Test$Runner$getFailureReason(expectation);
			if (_v1.$ === 'Nothing') {
				return _List_fromArray(
					[
						$author$project$Test$Reporter$TestResults$Passed(
						$elm_explorations$test$Test$Runner$getDistributionReport(expectation))
					]);
			} else {
				var failure = _v1.a;
				return $elm_explorations$test$Test$Runner$isTodo(expectation) ? _List_fromArray(
					[
						$author$project$Test$Reporter$TestResults$Todo(failure.description)
					]) : _List_fromArray(
					[
						$author$project$Test$Reporter$TestResults$Failed(
						_List_fromArray(
							[
								_Utils_Tuple2(
								failure,
								$elm_explorations$test$Test$Runner$getDistributionReport(expectation))
							]))
					]);
			}
		} else {
			var builder = A3(
				$elm$core$List$foldl,
				$author$project$Test$Reporter$TestResults$outcomesFromExpectationsHelp,
				{failures: _List_Nil, passes: _List_Nil, todos: _List_Nil},
				expectations);
			var failuresList = function () {
				var _v2 = builder.failures;
				if (!_v2.b) {
					return _List_Nil;
				} else {
					var failures = _v2;
					return _List_fromArray(
						[
							$author$project$Test$Reporter$TestResults$Failed(failures)
						]);
				}
			}();
			return $elm$core$List$concat(
				_List_fromArray(
					[
						A2($elm$core$List$map, $author$project$Test$Reporter$TestResults$Passed, builder.passes),
						A2($elm$core$List$map, $author$project$Test$Reporter$TestResults$Todo, builder.todos),
						failuresList
					]));
		}
	} else {
		return _List_Nil;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $author$project$Test$Runner$Node$sendResults = F3(
	function (isFinished, testReporter, results) {
		var typeStr = isFinished ? 'FINISHED' : 'RESULTS';
		var addToKeyValues = F2(
			function (_v0, list) {
				var testId = _v0.a;
				var result = _v0.b;
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(
						$elm$core$String$fromInt(testId),
						testReporter.reportComplete(result)),
					list);
			});
		return $author$project$Test$Runner$Node$elmTestPort__send(
			A2(
				$elm$json$Json$Encode$encode,
				0,
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'type',
							$elm$json$Json$Encode$string(typeStr)),
							_Utils_Tuple2(
							'results',
							$elm$json$Json$Encode$object(
								A3($elm$core$List$foldl, addToKeyValues, _List_Nil, results)))
						]))));
	});
var $author$project$Test$Runner$Node$dispatch = F2(
	function (model, startTime) {
		var _v0 = A2($elm$core$Dict$get, model.nextTestToRun, model.available);
		if (_v0.$ === 'Nothing') {
			return A3($author$project$Test$Runner$Node$sendResults, true, model.testReporter, model.results);
		} else {
			var config = _v0.a;
			var outcomes = $author$project$Test$Reporter$TestResults$outcomesFromExpectations(
				config.run(_Utils_Tuple0));
			return A2(
				$elm$core$Task$perform,
				A3($author$project$Test$Runner$Node$Complete, config.labels, outcomes, startTime),
				$elm$time$Time$now);
		}
	});
var $author$project$Test$Reporter$TestResults$isFailure = function (outcome) {
	if (outcome.$ === 'Failed') {
		return true;
	} else {
		return false;
	}
};
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0.a;
	return millis;
};
var $author$project$Test$Runner$Node$sendBegin = function (model) {
	var extraFields = function () {
		var _v0 = model.testReporter.reportBegin(model.runInfo);
		if (_v0.$ === 'Just') {
			var report = _v0.a;
			return _List_fromArray(
				[
					_Utils_Tuple2('message', report)
				]);
		} else {
			return _List_Nil;
		}
	}();
	var baseFields = _List_fromArray(
		[
			_Utils_Tuple2(
			'type',
			$elm$json$Json$Encode$string('BEGIN')),
			_Utils_Tuple2(
			'testCount',
			$elm$json$Json$Encode$int(model.runInfo.testCount))
		]);
	return $author$project$Test$Runner$Node$elmTestPort__send(
		A2(
			$elm$json$Json$Encode$encode,
			0,
			$elm$json$Json$Encode$object(
				_Utils_ap(baseFields, extraFields))));
};
var $author$project$Test$Runner$Node$update = F2(
	function (msg, model) {
		var testReporter = model.testReporter;
		switch (msg.$) {
			case 'Receive':
				var val = msg.a;
				var _v1 = A2($elm$json$Json$Decode$decodeValue, $author$project$Test$Runner$JsMessage$decoder, val);
				if (_v1.$ === 'Ok') {
					if (_v1.a.$ === 'Summary') {
						var _v2 = _v1.a;
						var duration = _v2.a;
						var failed = _v2.b;
						var todos = _v2.c;
						var testCount = model.runInfo.testCount;
						var summaryInfo = {
							duration: duration,
							failed: failed,
							passed: (testCount - failed) - $elm$core$List$length(todos),
							testCount: testCount,
							todos: todos
						};
						var summary = A2(testReporter.reportSummary, summaryInfo, model.autoFail);
						var exitCode = (failed > 0) ? 2 : ((_Utils_eq(model.autoFail, $elm$core$Maybe$Nothing) && $elm$core$List$isEmpty(todos)) ? 0 : 3);
						var cmd = $author$project$Test$Runner$Node$elmTestPort__send(
							A2(
								$elm$json$Json$Encode$encode,
								0,
								$elm$json$Json$Encode$object(
									_List_fromArray(
										[
											_Utils_Tuple2(
											'type',
											$elm$json$Json$Encode$string('SUMMARY')),
											_Utils_Tuple2(
											'exitCode',
											$elm$json$Json$Encode$int(exitCode)),
											_Utils_Tuple2('message', summary)
										]))));
						return _Utils_Tuple2(model, cmd);
					} else {
						var index = _v1.a.a;
						var cmd = A2($elm$core$Task$perform, $author$project$Test$Runner$Node$Dispatch, $elm$time$Time$now);
						return _Utils_eq(index, -1) ? _Utils_Tuple2(
							_Utils_update(
								model,
								{nextTestToRun: index + model.processes}),
							$elm$core$Platform$Cmd$batch(
								_List_fromArray(
									[
										cmd,
										$author$project$Test$Runner$Node$sendBegin(model)
									]))) : _Utils_Tuple2(
							_Utils_update(
								model,
								{nextTestToRun: index}),
							cmd);
					}
				} else {
					var err = _v1.a;
					var cmd = $author$project$Test$Runner$Node$elmTestPort__send(
						A2(
							$elm$json$Json$Encode$encode,
							0,
							$elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'type',
										$elm$json$Json$Encode$string('ERROR')),
										_Utils_Tuple2(
										'message',
										$elm$json$Json$Encode$string(
											$elm$json$Json$Decode$errorToString(err)))
									]))));
					return _Utils_Tuple2(model, cmd);
				}
			case 'Dispatch':
				var startTime = msg.a;
				return _Utils_Tuple2(
					model,
					A2($author$project$Test$Runner$Node$dispatch, model, startTime));
			default:
				var labels = msg.a;
				var outcomes = msg.b;
				var startTime = msg.c;
				var endTime = msg.d;
				var nextTestToRun = model.nextTestToRun + model.processes;
				var isFinished = _Utils_cmp(nextTestToRun, model.runInfo.testCount) > -1;
				var duration = $elm$time$Time$posixToMillis(endTime) - $elm$time$Time$posixToMillis(startTime);
				var prependOutcome = F2(
					function (outcome, rest) {
						return A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								model.nextTestToRun,
								{duration: duration, labels: labels, outcome: outcome}),
							rest);
					});
				var results = A3($elm$core$List$foldl, prependOutcome, model.results, outcomes);
				if (isFinished || A2($elm$core$List$any, $author$project$Test$Reporter$TestResults$isFailure, outcomes)) {
					var cmd = A3($author$project$Test$Runner$Node$sendResults, isFinished, testReporter, results);
					return isFinished ? _Utils_Tuple2(model, cmd) : _Utils_Tuple2(
						_Utils_update(
							model,
							{nextTestToRun: nextTestToRun, results: _List_Nil}),
						$elm$core$Platform$Cmd$batch(
							_List_fromArray(
								[
									cmd,
									A2($elm$core$Task$perform, $author$project$Test$Runner$Node$Dispatch, $elm$time$Time$now)
								])));
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{nextTestToRun: nextTestToRun, results: results}),
						A2($elm$core$Task$perform, $author$project$Test$Runner$Node$Dispatch, $elm$time$Time$now));
				}
		}
	});
var $elm$core$Platform$worker = _Platform_worker;
var $author$project$Test$Runner$Node$run = F2(
	function (_v0, possiblyTests) {
		var runs = _v0.runs;
		var seed = _v0.seed;
		var report = _v0.report;
		var globs = _v0.globs;
		var paths = _v0.paths;
		var processes = _v0.processes;
		var tests = A2(
			$elm$core$List$filterMap,
			function (_v4) {
				var moduleName = _v4.a;
				var maybeModuleTests = _v4.b;
				var moduleTests = A2($elm$core$List$filterMap, $elm$core$Basics$identity, maybeModuleTests);
				return $elm$core$List$isEmpty(moduleTests) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
					A2($elm_explorations$test$Test$describe, moduleName, moduleTests));
			},
			possiblyTests);
		if ($elm$core$List$isEmpty(tests)) {
			return $elm$core$Platform$worker(
				{
					init: A2(
						$author$project$Test$Runner$Node$failInit,
						$author$project$Test$Runner$Node$noTestsFoundError(globs),
						report),
					subscriptions: function (_v1) {
						return $elm$core$Platform$Sub$none;
					},
					update: F2(
						function (_v2, model) {
							return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
						})
				});
		} else {
			var runners = A3(
				$elm_explorations$test$Test$Runner$fromTest,
				runs,
				$elm$random$Random$initialSeed(seed),
				$elm_explorations$test$Test$concat(tests));
			var wrappedInit = $author$project$Test$Runner$Node$init(
				{fuzzRuns: runs, globs: globs, initialSeed: seed, paths: paths, processes: processes, report: report, runners: runners});
			return $elm$core$Platform$worker(
				{
					init: wrappedInit,
					subscriptions: function (_v3) {
						return $author$project$Test$Runner$Node$elmTestPort__receive($author$project$Test$Runner$Node$Receive);
					},
					update: $author$project$Test$Runner$Node$update
				});
		}
	});
var $author$project$GraphFixture$workspace = {
	authorities: _List_Nil,
	compiler: {diagnostics: _List_Nil, errors: 0, warnings: 0},
	decisionShare: $elm$core$Dict$empty,
	demo: false,
	edges: _List_Nil,
	events: _List_Nil,
	goals: _List_Nil,
	organization: {createdAt: '2026-01-01', id: 'org-a', name: '검증 조직'},
	people: _List_Nil,
	reviewWarnings: _List_Nil,
	reviews: _List_Nil,
	version: 1
};
var $author$project$GraphFixture$sample = function () {
	var person = function (id) {
		return {active: true, department: $elm$core$Maybe$Nothing, email: $elm$core$Maybe$Nothing, id: id, name: '같은 이름', reportsTo: $elm$core$Maybe$Nothing, role: '운영'};
	};
	var base = $author$project$GraphFixture$goal.goal;
	var isolated = _Utils_update(
		$author$project$GraphFixture$goal,
		{
			goal: _Utils_update(
				base,
				{description: '연결 없는 목표', id: 'isolated'}),
			owner: $elm$core$Maybe$Nothing
		});
	return _Utils_update(
		$author$project$GraphFixture$workspace,
		{
			authorities: _List_fromArray(
				[
					{budgetLimit: 50, canApprove: _List_Nil, canChangePrice: true, canHire: false, owner: 'p'}
				]),
			edges: _List_fromArray(
				[
					A3(
					$author$project$GraphFixture$edge,
					'Owns',
					A2($author$project$GraphFixture$node, 'PersonNode', 'p'),
					A2($author$project$GraphFixture$node, 'GoalNode', 'g')),
					A3(
					$author$project$GraphFixture$edge,
					'Owns',
					A2($author$project$GraphFixture$node, 'PersonNode', 'p'),
					A2($author$project$GraphFixture$node, 'GoalNode', 'g')),
					A3(
					$author$project$GraphFixture$edge,
					'Measures',
					A2($author$project$GraphFixture$node, 'GoalNode', 'g'),
					A2($author$project$GraphFixture$node, 'MetricNode', 'm')),
					A3(
					$author$project$GraphFixture$edge,
					'DependsOn',
					A2($author$project$GraphFixture$node, 'GoalNode', 'g'),
					A2($author$project$GraphFixture$node, 'GoalNode', 'isolated')),
					A3(
					$author$project$GraphFixture$edge,
					'Controls',
					A2($author$project$GraphFixture$node, 'PersonNode', 'p'),
					A2($author$project$GraphFixture$node, 'ResourceNode', 'Budget')),
					A3(
					$author$project$GraphFixture$edge,
					'Controls',
					A2($author$project$GraphFixture$node, 'PersonNode', 'p'),
					A2($author$project$GraphFixture$node, 'ResourceNode', 'Pricing'))
				]),
			goals: _List_fromArray(
				[$author$project$GraphFixture$goal, isolated]),
			people: _List_fromArray(
				[
					person('p'),
					person('p2')
				])
		});
}();
var $author$project$ListViewTest$goal = {
	active: false,
	analysis: {coverage: 1, possibleCause: '측정 필요'},
	evaluation: {latestValue: $elm$core$Maybe$Nothing, progress: 0, status: $author$project$Domain$NoData},
	goal: {
		baseline: 0,
		deadline: '2026-12-31',
		description: '매출 개선',
		id: 'g',
		metric: {direction: 'HigherIsBetter', id: 'm', name: '매출', unit: '원'},
		requiredBudget: 0,
		requiredPermissions: _List_Nil,
		target: 100
	},
	owner: $elm$core$Maybe$Just('p'),
	results: _List_Nil,
	strategies: _List_Nil
};
var $author$project$ListViewTest$person = {
	active: true,
	department: $elm$core$Maybe$Just('개발'),
	email: $elm$core$Maybe$Just('p@example.com'),
	id: 'p',
	name: '김직원',
	reportsTo: $elm$core$Maybe$Nothing,
	role: 'Engineer'
};
var $author$project$ListViewTest$sample = _Utils_update(
	$author$project$ListViewTest$workspace,
	{
		events: _List_fromArray(
			[
				{
				activity: {detail: '', personId: $elm$core$Maybe$Nothing, raw: 'null', reviewId: $elm$core$Maybe$Nothing, tag: '', targetId: '', targetKind: ''},
				actor: $elm$core$Maybe$Just('p'),
				at: '2026-09-07',
				description: '목표 등록',
				evaluatedGoal: $elm$core$Maybe$Nothing,
				evaluatedStatus: $elm$core$Maybe$Nothing,
				seq: 1
			}
			]),
		goals: _List_fromArray(
			[$author$project$ListViewTest$goal]),
		people: _List_fromArray(
			[$author$project$ListViewTest$person]),
		reviews: _List_fromArray(
			[
				{
				decisions: _List_fromArray(
					[
						{
						deadline: $elm$core$Maybe$Just('2026-12-31'),
						owner: 'p',
						text: '개선 진행'
					}
					]),
				evaluation: $author$project$ListViewTest$goal.evaluation,
				goal: 'g',
				heldAt: '2026-09-07',
				id: 'r',
				learnings: _List_fromArray(
					['고객 의견']),
				note: '첫 회고'
			}
			])
	});
var $author$project$Page$ActivityLog = {$: 'ActivityLog'};
var $author$project$Domain$Discovery$AddObservation = function (a) {
	return {$: 'AddObservation', a: a};
};
var $author$project$Domain$Discovery$AddWorkflow = function (a) {
	return {$: 'AddWorkflow', a: a};
};
var $author$project$Page$Authorities = {$: 'Authorities'};
var $author$project$Form$Goal$Direction = {$: 'Direction'};
var $author$project$App$Update$EditAgents = function (a) {
	return {$: 'EditAgents', a: a};
};
var $author$project$App$Update$EditDiscovery = function (a) {
	return {$: 'EditDiscovery', a: a};
};
var $author$project$App$Update$EditGoal = F2(
	function (a, b) {
		return {$: 'EditGoal', a: a, b: b};
	});
var $author$project$App$Update$EditReview = F2(
	function (a, b) {
		return {$: 'EditReview', a: a, b: b};
	});
var $author$project$App$Effect$FocusElement = function (a) {
	return {$: 'FocusElement', a: a};
};
var $author$project$App$Update$Guide = F2(
	function (a, b) {
		return {$: 'Guide', a: a, b: b};
	});
var $author$project$Domain$Agent$Import = function (a) {
	return {$: 'Import', a: a};
};
var $author$project$Form$Goal$MetricId = {$: 'MetricId'};
var $author$project$Form$Goal$MetricName = {$: 'MetricName'};
var $author$project$App$Update$OpenPerson = function (a) {
	return {$: 'OpenPerson', a: a};
};
var $author$project$Page$People = {$: 'People'};
var $author$project$Page$Responsibility = {$: 'Responsibility'};
var $author$project$Page$Reviews = {$: 'Reviews'};
var $author$project$Form$Goal$Unit = {$: 'Unit'};
var $author$project$App$Drafts$advanceSerial = function (state) {
	return _Utils_update(
		state,
		{serial: state.serial + 1});
};
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$App$Session$busy = function (state) {
	return !_Utils_eq(state.saving, $author$project$App$Session$Idle);
};
var $author$project$App$Update$busy = function (model) {
	return $author$project$App$Session$busy(model.session);
};
var $author$project$App$Agents$clearDraft = F2(
	function (org, state) {
		return _Utils_update(
			state,
			{
				drafts: A2($elm$core$Dict$remove, org, state.drafts)
			});
	});
var $author$project$App$Discovery$clearDraft = F2(
	function (org, state) {
		return _Utils_update(
			state,
			{
				drafts: A2($elm$core$Dict$remove, org, state.drafts)
			});
	});
var $author$project$App$Drafts$closeDelete = function (state) {
	return _Utils_update(
		state,
		{deletion: $elm$core$Maybe$Nothing});
};
var $author$project$App$Drafts$confirmDelete = F2(
	function (name, state) {
		return _Utils_update(
			state,
			{
				deletion: A2(
					$elm$core$Maybe$map,
					function (snapshot) {
						return _Utils_update(
							snapshot,
							{confirmation: name});
					},
					state.deletion)
			});
	});
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $author$project$Domain$Agent$mapRole = F2(
	function (ident, f) {
		return $elm$core$List$map(
			function (role) {
				return _Utils_eq(role.id, ident) ? f(role) : role;
			});
	});
var $author$project$Domain$Agent$Permission = function (a) {
	return {$: 'Permission', a: a};
};
var $author$project$Domain$Agent$Person = function (a) {
	return {$: 'Person', a: a};
};
var $elm$core$String$startsWith = _String_startsWith;
var $author$project$Domain$Agent$parseApproval = function (key) {
	return A2($elm$core$String$startsWith, 'person:', key) ? $elm$core$Maybe$Just(
		$author$project$Domain$Agent$Person(
			A2($elm$core$String$dropLeft, 7, key))) : (A2($elm$core$String$startsWith, 'permission:', key) ? $elm$core$Maybe$Just(
		$author$project$Domain$Agent$Permission(
			A2($elm$core$String$dropLeft, 11, key))) : $elm$core$Maybe$Nothing);
};
var $author$project$Domain$Agent$splitTools = A2(
	$elm$core$Basics$composeR,
	$elm$core$String$split(','),
	A2(
		$elm$core$Basics$composeR,
		$elm$core$List$concatMap(
			$elm$core$String$split('\n')),
		A2(
			$elm$core$Basics$composeR,
			$elm$core$List$map($elm$core$String$trim),
			$elm$core$List$filter(
				A2($elm$core$Basics$composeL, $elm$core$Basics$not, $elm$core$String$isEmpty)))));
var $author$project$Domain$Agent$apply = F2(
	function (change, roles) {
		switch (change.$) {
			case 'Import':
				var drafts = change.a;
				return drafts;
			case 'Name':
				var ident = change.a;
				var value = change.b;
				return A3(
					$author$project$Domain$Agent$mapRole,
					ident,
					function (r) {
						return _Utils_update(
							r,
							{name: value});
					},
					roles);
			case 'Inputs':
				var ident = change.a;
				var value = change.b;
				return A3(
					$author$project$Domain$Agent$mapRole,
					ident,
					function (r) {
						return _Utils_update(
							r,
							{inputs: value});
					},
					roles);
			case 'Outputs':
				var ident = change.a;
				var value = change.b;
				return A3(
					$author$project$Domain$Agent$mapRole,
					ident,
					function (r) {
						return _Utils_update(
							r,
							{outputs: value});
					},
					roles);
			case 'Tools':
				var ident = change.a;
				var value = change.b;
				return A3(
					$author$project$Domain$Agent$mapRole,
					ident,
					function (r) {
						return _Utils_update(
							r,
							{
								tools: $author$project$Domain$Agent$splitTools(value)
							});
					},
					roles);
			case 'Level':
				var ident = change.a;
				var value = change.b;
				return A3(
					$author$project$Domain$Agent$mapRole,
					ident,
					function (r) {
						return _Utils_update(
							r,
							{level: value});
					},
					roles);
			case 'SetApproval':
				var ident = change.a;
				var value = change.b;
				return A3(
					$author$project$Domain$Agent$mapRole,
					ident,
					function (r) {
						return _Utils_update(
							r,
							{
								approval: $author$project$Domain$Agent$parseApproval(value)
							});
					},
					roles);
			case 'Handoff':
				var ident = change.a;
				var target = change.b;
				var selected = change.c;
				return A3(
					$author$project$Domain$Agent$mapRole,
					ident,
					function (r) {
						var without = A2(
							$elm$core$List$filter,
							$elm$core$Basics$neq(target),
							r.handoffTo);
						return _Utils_update(
							r,
							{
								handoffTo: (selected && (!_Utils_eq(target, ident))) ? _Utils_ap(
									without,
									_List_fromArray(
										[target])) : without
							});
					},
					roles);
			case 'Status':
				var ident = change.a;
				var value = change.b;
				return A3(
					$author$project$Domain$Agent$mapRole,
					ident,
					function (r) {
						return _Utils_update(
							r,
							{status: value});
					},
					roles);
			case 'Evidence':
				var ident = change.a;
				var value = change.b;
				return A3(
					$author$project$Domain$Agent$mapRole,
					ident,
					function (r) {
						return _Utils_update(
							r,
							{evidence: value});
					},
					roles);
			default:
				var ident = change.a;
				return A2(
					$elm$core$List$map,
					function (r) {
						return _Utils_update(
							r,
							{
								handoffTo: A2(
									$elm$core$List$filter,
									$elm$core$Basics$neq(ident),
									r.handoffTo)
							});
					},
					A2(
						$elm$core$List$filter,
						function (r) {
							return !_Utils_eq(r.id, ident);
						},
						roles));
		}
	});
var $author$project$App$Agents$saved = F2(
	function (org, state) {
		return A2($elm$core$Dict$get, org, state.snapshots);
	});
var $author$project$App$Agents$current = F2(
	function (org, state) {
		var _v0 = A2($elm$core$Dict$get, org, state.drafts);
		if (_v0.$ === 'Just') {
			var draft = _v0.a;
			return $elm$core$Maybe$Just(draft);
		} else {
			return A2(
				$elm$core$Maybe$map,
				function (snapshot) {
					return {agents: snapshot.agents, version: snapshot.version};
				},
				A2($author$project$App$Agents$saved, org, state));
		}
	});
var $author$project$App$Agents$edit = F3(
	function (org, change, state) {
		var _v0 = A2($author$project$App$Agents$current, org, state);
		if (_v0.$ === 'Nothing') {
			return state;
		} else {
			var design = _v0.a;
			return _Utils_update(
				state,
				{
					drafts: A3(
						$elm$core$Dict$insert,
						org,
						_Utils_update(
							design,
							{
								agents: A2($author$project$Domain$Agent$apply, change, design.agents)
							}),
						state.drafts)
				});
		}
	});
var $author$project$Domain$Discovery$editObservation = F3(
	function (key, value, o) {
		switch (key) {
			case 'subject':
				return _Utils_update(
					o,
					{subject: value});
			case 'detail':
				return _Utils_update(
					o,
					{detail: value});
			case 'status':
				return _Utils_update(
					o,
					{status: value});
			case 'evidence':
				return _Utils_update(
					o,
					{evidence: value});
			default:
				return o;
		}
	});
var $author$project$Domain$Discovery$editWorkflow = F3(
	function (key, value, w) {
		switch (key) {
			case 'name':
				return _Utils_update(
					w,
					{name: value});
			case 'role':
				return _Utils_update(
					w,
					{role: value});
			case 'trigger':
				return _Utils_update(
					w,
					{trigger: value});
			case 'inputs':
				return _Utils_update(
					w,
					{inputs: value});
			case 'tools':
				return _Utils_update(
					w,
					{tools: value});
			case 'outputs':
				return _Utils_update(
					w,
					{outputs: value});
			case 'handoff':
				return _Utils_update(
					w,
					{handoff: value});
			case 'approval':
				return _Utils_update(
					w,
					{approval: value});
			case 'status':
				return _Utils_update(
					w,
					{status: value});
			case 'evidence':
				return _Utils_update(
					w,
					{evidence: value});
			default:
				return w;
		}
	});
var $author$project$Domain$Discovery$emptyWorkflow = function (ident) {
	return {approval: '', approvalPermission: $elm$core$Maybe$Nothing, approvalPerson: $elm$core$Maybe$Nothing, evidence: '', handoff: '', handoffWorkflows: _List_Nil, id: ident, inputs: '', name: '', outputs: '', role: '', rolePerson: $elm$core$Maybe$Nothing, status: 'unknown', tools: '', trigger: ''};
};
var $author$project$Domain$Discovery$mapWorkflow = F3(
	function (ident, f, doc) {
		return _Utils_update(
			doc,
			{
				workflows: A2(
					$elm$core$List$map,
					function (w) {
						return _Utils_eq(w.id, ident) ? f(w) : w;
					},
					doc.workflows)
			});
	});
var $author$project$Domain$Discovery$optional = function (value) {
	return ($elm$core$String$trim(value) === '') ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(value);
};
var $author$project$Domain$Discovery$apply = F2(
	function (change, doc) {
		var updated = function () {
			switch (change.$) {
				case 'Scope':
					var value = change.a;
					return _Utils_update(
						doc,
						{scope: value});
				case 'AsOf':
					var value = change.a;
					return _Utils_update(
						doc,
						{asOf: value});
				case 'AddObservation':
					var ident = change.a;
					return _Utils_update(
						doc,
						{
							observations: _Utils_ap(
								doc.observations,
								_List_fromArray(
									[
										{detail: '', evidence: '', id: ident, status: 'unknown', subject: ''}
									]))
						});
				case 'ObservationField':
					var ident = change.a;
					var key = change.b;
					var value = change.c;
					return _Utils_update(
						doc,
						{
							observations: A2(
								$elm$core$List$map,
								function (o) {
									return _Utils_eq(o.id, ident) ? A3($author$project$Domain$Discovery$editObservation, key, value, o) : o;
								},
								doc.observations)
						});
				case 'AddWorkflow':
					var ident = change.a;
					return _Utils_update(
						doc,
						{
							workflows: _Utils_ap(
								doc.workflows,
								_List_fromArray(
									[
										$author$project$Domain$Discovery$emptyWorkflow(ident)
									]))
						});
				case 'WorkflowField':
					var ident = change.a;
					var key = change.b;
					var value = change.c;
					return A3(
						$author$project$Domain$Discovery$mapWorkflow,
						ident,
						A2($author$project$Domain$Discovery$editWorkflow, key, value),
						doc);
				case 'WorkflowRolePerson':
					var ident = change.a;
					var value = change.b;
					return A3(
						$author$project$Domain$Discovery$mapWorkflow,
						ident,
						function (w) {
							return _Utils_update(
								w,
								{
									rolePerson: $author$project$Domain$Discovery$optional(value)
								});
						},
						doc);
				case 'WorkflowApprovalPerson':
					var ident = change.a;
					var value = change.b;
					return A3(
						$author$project$Domain$Discovery$mapWorkflow,
						ident,
						function (w) {
							return _Utils_update(
								w,
								{
									approvalPerson: $author$project$Domain$Discovery$optional(value)
								});
						},
						doc);
				case 'WorkflowApprovalPermission':
					var ident = change.a;
					var value = change.b;
					return A3(
						$author$project$Domain$Discovery$mapWorkflow,
						ident,
						function (w) {
							return _Utils_update(
								w,
								{
									approvalPermission: $author$project$Domain$Discovery$optional(value)
								});
						},
						doc);
				case 'WorkflowHandoff':
					var ident = change.a;
					var target = change.b;
					var selected = change.c;
					return A3(
						$author$project$Domain$Discovery$mapWorkflow,
						ident,
						function (w) {
							var without = A2(
								$elm$core$List$filter,
								$elm$core$Basics$neq(target),
								w.handoffWorkflows);
							return _Utils_update(
								w,
								{
									handoffWorkflows: (selected && (!_Utils_eq(target, ident))) ? _Utils_ap(
										without,
										_List_fromArray(
											[target])) : without
								});
						},
						doc);
				case 'RemoveObservation':
					var ident = change.a;
					return _Utils_update(
						doc,
						{
							observations: A2(
								$elm$core$List$filter,
								function (o) {
									return !_Utils_eq(o.id, ident);
								},
								doc.observations)
						});
				case 'RemoveWorkflow':
					var ident = change.a;
					return _Utils_update(
						doc,
						{
							workflows: A2(
								$elm$core$List$map,
								function (w) {
									return _Utils_update(
										w,
										{
											handoffWorkflows: A2(
												$elm$core$List$filter,
												$elm$core$Basics$neq(ident),
												w.handoffWorkflows)
										});
								},
								A2(
									$elm$core$List$filter,
									function (w) {
										return !_Utils_eq(w.id, ident);
									},
									doc.workflows))
						});
				case 'ReviewNote':
					var value = change.a;
					return _Utils_update(
						doc,
						{
							review: {note: value, status: doc.review.status}
						});
				default:
					var value = change.a;
					return _Utils_update(
						doc,
						{
							review: {note: doc.review.note, status: value}
						});
			}
		}();
		switch (change.$) {
			case 'ReviewNote':
				return updated;
			case 'ReviewStatus':
				return updated;
			default:
				return _Utils_update(
					updated,
					{
						review: {note: updated.review.note, status: 'pending'}
					});
		}
	});
var $author$project$App$Discovery$saved = F2(
	function (org, state) {
		return A2($elm$core$Dict$get, org, state.documents);
	});
var $author$project$App$Discovery$current = F2(
	function (org, state) {
		var _v0 = A2($elm$core$Dict$get, org, state.drafts);
		if (_v0.$ === 'Just') {
			var draft = _v0.a;
			return $elm$core$Maybe$Just(draft);
		} else {
			return A2($author$project$App$Discovery$saved, org, state);
		}
	});
var $author$project$App$Discovery$edit = F3(
	function (org, change, state) {
		var _v0 = A2($author$project$App$Discovery$current, org, state);
		if (_v0.$ === 'Nothing') {
			return state;
		} else {
			var snapshot = _v0.a;
			return _Utils_update(
				state,
				{
					drafts: A3(
						$elm$core$Dict$insert,
						org,
						_Utils_update(
							snapshot,
							{
								discovery: A2($author$project$Domain$Discovery$apply, change, snapshot.discovery)
							}),
						state.drafts)
				});
		}
	});
var $author$project$App$Drafts$defaultContext = function (model) {
	return {
		deadline: model.flags.deadline,
		goalIndex: A2(
			$elm$core$Maybe$withDefault,
			0,
			A2(
				$elm$core$Dict$get,
				A2($elm$core$Maybe$withDefault, '', model.session.org),
				model.forms.goalSerial)),
		seed: model.flags.seed,
		today: model.flags.today,
		workspace: function () {
			var _v0 = model.session.workspace;
			if (_v0.$ === 'Loaded') {
				var data = _v0.a;
				return $elm$core$Maybe$Just(data);
			} else {
				return $elm$core$Maybe$Nothing;
			}
		}()
	};
};
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $author$project$Form$Defaults$defaultValue = F3(
	function (model, action, name) {
		var w = model.workspace;
		var owner = function (key) {
			return A2(
				$elm$core$Maybe$withDefault,
				'',
				A2(
					$elm$core$Maybe$andThen,
					function (data) {
						return A2(
							$elm$core$Maybe$andThen,
							function ($) {
								return $.owner;
							},
							$elm$core$List$head(
								A2(
									$elm$core$List$filter,
									A2(
										$elm$core$Basics$composeR,
										function ($) {
											return $.goal;
										},
										A2(
											$elm$core$Basics$composeR,
											function ($) {
												return $.id;
											},
											$elm$core$Basics$eq(key))),
									data.goals)));
					},
					w));
		};
		switch (action.$) {
			case 'Rename':
				return (name === 'name') ? A2(
					$elm$core$Maybe$withDefault,
					'',
					A2(
						$elm$core$Maybe$map,
						A2(
							$elm$core$Basics$composeR,
							function ($) {
								return $.organization;
							},
							function ($) {
								return $.name;
							}),
						w)) : '';
			case 'UpdatePerson':
				var key = action.a;
				var person = A2(
					$elm$core$Maybe$andThen,
					function (data) {
						return $elm$core$List$head(
							A2(
								$elm$core$List$filter,
								A2(
									$elm$core$Basics$composeR,
									function ($) {
										return $.id;
									},
									$elm$core$Basics$eq(key)),
								data.people));
					},
					w);
				return A2(
					$elm$core$Maybe$withDefault,
					'',
					A2(
						$elm$core$Maybe$map,
						function (p) {
							switch (name) {
								case 'name':
									return p.name;
								case 'role':
									return p.role;
								case 'department':
									return A2($elm$core$Maybe$withDefault, '', p.department);
								case 'email':
									return A2($elm$core$Maybe$withDefault, '', p.email);
								case 'reportsTo':
									return A2($elm$core$Maybe$withDefault, '', p.reportsTo);
								default:
									return '';
							}
						},
						person));
			case 'AddGoal':
				switch (name) {
					case 'baseline':
						return '0';
					case 'target':
						return '100';
					case 'budget':
						return '0';
					case 'metricId':
						return 'metric-' + (model.seed + ('-' + $elm$core$String$fromInt(model.goalIndex)));
					case 'direction':
						return 'HigherIsBetter';
					case 'startsAt':
						return model.today;
					case 'deadline':
						return model.deadline;
					default:
						return '';
				}
			case 'Assign':
				var key = action.a;
				return (name === 'owner') ? owner(key) : '';
			case 'Report':
				var key = action.a;
				return (name === 'reportedBy') ? owner(key) : '';
			case 'Grant':
				var key = action.a;
				var authority = A2(
					$elm$core$Maybe$andThen,
					function (data) {
						return $elm$core$List$head(
							A2(
								$elm$core$List$filter,
								A2(
									$elm$core$Basics$composeR,
									function ($) {
										return $.owner;
									},
									$elm$core$Basics$eq(key)),
								data.authorities));
					},
					w);
				return (name === 'budget') ? A2(
					$elm$core$Maybe$withDefault,
					'0',
					A2(
						$elm$core$Maybe$map,
						A2(
							$elm$core$Basics$composeR,
							function ($) {
								return $.budgetLimit;
							},
							$elm$core$String$fromFloat),
						authority)) : (A2(
					$elm$core$Maybe$withDefault,
					false,
					A2(
						$elm$core$Maybe$map,
						function (a) {
							return A2($elm$core$List$member, name, a.canApprove) || (((name === 'Hiring') && a.canHire) || ((name === 'Pricing') && a.canChangePrice));
						},
						authority)) ? 'true' : 'false');
			default:
				return '';
		}
	});
var $author$project$Domain$Permission$permissionKeys = _List_fromArray(
	['Pricing', 'Hiring', 'BudgetApproval', 'Contracting', 'Marketing', 'Infrastructure', 'ProductLaunch']);
var $author$project$Form$Defaults$draftDefaults = F2(
	function (model, action) {
		return $elm$core$Dict$fromList(
			A2(
				$elm$core$List$map,
				function (key) {
					return _Utils_Tuple2(
						key,
						A3($author$project$Form$Defaults$defaultValue, model, action, key));
				},
				_Utils_ap(
					_List_fromArray(
						['name', 'role', 'department', 'email', 'reportsTo', 'successor', 'budget', 'owner', 'reportedBy', 'note', 'value']),
					$author$project$Domain$Permission$permissionKeys)));
	});
var $author$project$App$Drafts$draftDefaults = function (model) {
	return $author$project$Form$Defaults$draftDefaults(
		$author$project$App$Drafts$defaultContext(model));
};
var $author$project$Form$Action$actionKey = function (action) {
	switch (action.$) {
		case 'CreateOrg':
			return 'organization';
		case 'ImportDemo':
			return 'demo';
		case 'Rename':
			return 'rename';
		case 'AddPerson':
			return 'person';
		case 'UpdatePerson':
			var key = action.a;
			return 'person-edit-' + key;
		case 'DeactivatePerson':
			var key = action.a;
			return 'person-deactivate-' + key;
		case 'AddGoal':
			return 'goal';
		case 'Assign':
			var key = action.a;
			return 'owner-' + key;
		case 'Grant':
			var key = action.a;
			return 'authority-' + key;
		case 'Report':
			var key = action.a;
			return 'result-' + key;
		case 'Strategy':
			var key = action.a;
			return 'strategy-' + key;
		case 'AddReview':
			return 'review';
		case 'Activate':
			var key = action.a;
			return 'activate-' + key;
		case 'Evaluate':
			var key = action.a;
			return 'evaluate-' + key;
		default:
			return 'delete';
	}
};
var $author$project$App$Drafts$formKey = F2(
	function (model, action) {
		return A2($elm$core$Maybe$withDefault, 'list', model.session.org) + ('/' + $author$project$Form$Action$actionKey(action));
	});
var $author$project$App$Drafts$workspaceVersion = function (model) {
	var _v0 = model.session.workspace;
	if (_v0.$ === 'Loaded') {
		var w = _v0.a;
		return w.version;
	} else {
		return 0;
	}
};
var $author$project$App$Drafts$edit = F4(
	function (action, key, value, model) {
		var state = model.forms;
		var draftKey = A2($author$project$App$Drafts$formKey, model, action);
		var current = A2(
			$elm$core$Maybe$withDefault,
			A2($author$project$App$Drafts$draftDefaults, model, action),
			A2($elm$core$Dict$get, draftKey, state.drafts));
		var version = function () {
			switch (action.$) {
				case 'UpdatePerson':
					return A2(
						$elm$core$Maybe$withDefault,
						$elm$core$String$fromInt(
							$author$project$App$Drafts$workspaceVersion(model)),
						A2($elm$core$Dict$get, '__version', current));
				case 'DeactivatePerson':
					return A2(
						$elm$core$Maybe$withDefault,
						$elm$core$String$fromInt(
							$author$project$App$Drafts$workspaceVersion(model)),
						A2($elm$core$Dict$get, '__version', current));
				default:
					return $elm$core$String$fromInt(
						$author$project$App$Drafts$workspaceVersion(model));
			}
		}();
		return _Utils_update(
			state,
			{
				drafts: A3(
					$elm$core$Dict$insert,
					draftKey,
					A3(
						$elm$core$Dict$insert,
						'__version',
						version,
						A3($elm$core$Dict$insert, key, value, current)),
					state.drafts)
			});
	});
var $author$project$Form$Action$AddGoal = {$: 'AddGoal'};
var $author$project$Form$Goal$edit = F3(
	function (key, content, draft) {
		switch (key.$) {
			case 'Description':
				return _Utils_update(
					draft,
					{description: content});
			case 'MetricName':
				return _Utils_update(
					draft,
					{metricName: content});
			case 'Unit':
				return _Utils_update(
					draft,
					{unit: content});
			case 'MetricId':
				return _Utils_update(
					draft,
					{metricId: content});
			case 'Direction':
				return _Utils_update(
					draft,
					{direction: content});
			case 'Baseline':
				return _Utils_update(
					draft,
					{baseline: content});
			case 'Target':
				return _Utils_update(
					draft,
					{target: content});
			case 'StartsAt':
				return _Utils_update(
					draft,
					{startsAt: content});
			case 'Deadline':
				return _Utils_update(
					draft,
					{deadline: content});
			case 'Budget':
				return _Utils_update(
					draft,
					{budget: content});
			case 'Parent':
				return _Utils_update(
					draft,
					{parent: content});
			default:
				var permission = key.a;
				return _Utils_update(
					draft,
					{
						permissions: (content === 'true') ? A2(
							$elm$core$List$cons,
							permission,
							A2(
								$elm$core$List$filter,
								$elm$core$Basics$neq(permission),
								draft.permissions)) : A2(
							$elm$core$List$filter,
							$elm$core$Basics$neq(permission),
							draft.permissions)
					});
		}
	});
var $author$project$App$Drafts$defaultValue = function (model) {
	return $author$project$Form$Defaults$defaultValue(
		$author$project$App$Drafts$defaultContext(model));
};
var $author$project$Form$Goal$fromValues = function (get) {
	return {
		baseline: get('baseline'),
		budget: get('budget'),
		deadline: get('deadline'),
		description: get('description'),
		direction: get('direction'),
		metricId: get('metricId'),
		metricName: get('metricName'),
		parent: get('parent'),
		permissions: A2(
			$elm$core$List$filter,
			function (key) {
				return get(key) === 'true';
			},
			$author$project$Domain$Permission$permissionKeys),
		startsAt: get('startsAt'),
		target: get('target'),
		unit: get('unit')
	};
};
var $author$project$App$Drafts$goalDraft = function (model) {
	return A2(
		$elm$core$Maybe$withDefault,
		$author$project$Form$Goal$fromValues(
			A2($author$project$App$Drafts$defaultValue, model, $author$project$Form$Action$AddGoal)),
		A2(
			$elm$core$Dict$get,
			A2($author$project$App$Drafts$formKey, model, $author$project$Form$Action$AddGoal),
			model.forms.goalDrafts));
};
var $author$project$App$Drafts$editGoal = F3(
	function (field, value, model) {
		var state = model.forms;
		return _Utils_update(
			state,
			{
				goalDrafts: A3(
					$elm$core$Dict$insert,
					A2($author$project$App$Drafts$formKey, model, $author$project$Form$Action$AddGoal),
					A3(
						$author$project$Form$Goal$edit,
						field,
						value,
						$author$project$App$Drafts$goalDraft(model)),
					state.goalDrafts)
			});
	});
var $author$project$Form$Action$AddReview = {$: 'AddReview'};
var $author$project$Form$Review$edit = F3(
	function (key, content, draft) {
		switch (key.$) {
			case 'Goal':
				return _Utils_update(
					draft,
					{goal: content});
			case 'Note':
				return _Utils_update(
					draft,
					{note: content});
			case 'Learning':
				return _Utils_update(
					draft,
					{learning: content});
			case 'Decision':
				return _Utils_update(
					draft,
					{decision: content});
			case 'DecisionOwner':
				return _Utils_update(
					draft,
					{decisionOwner: content});
			default:
				return _Utils_update(
					draft,
					{decisionDeadline: content});
		}
	});
var $author$project$Form$Review$fromValues = function (get) {
	return {
		decision: get('decision'),
		decisionDeadline: get('decisionDeadline'),
		decisionOwner: get('decisionOwner'),
		goal: get('goal'),
		learning: get('learning'),
		note: get('note')
	};
};
var $author$project$App$Drafts$reviewDraft = function (model) {
	return A2(
		$elm$core$Maybe$withDefault,
		$author$project$Form$Review$fromValues(
			A2($author$project$App$Drafts$defaultValue, model, $author$project$Form$Action$AddReview)),
		A2(
			$elm$core$Dict$get,
			A2($author$project$App$Drafts$formKey, model, $author$project$Form$Action$AddReview),
			model.forms.reviewDrafts));
};
var $author$project$App$Drafts$editReview = F3(
	function (field, value, model) {
		var state = model.forms;
		return _Utils_update(
			state,
			{
				reviewDrafts: A3(
					$elm$core$Dict$insert,
					A2($author$project$App$Drafts$formKey, model, $author$project$Form$Action$AddReview),
					A3(
						$author$project$Form$Review$edit,
						field,
						value,
						$author$project$App$Drafts$reviewDraft(model)),
					state.reviewDrafts)
			});
	});
var $author$project$App$PageState$filterPeople = F2(
	function (status, state) {
		return _Utils_update(
			state,
			{peopleStatus: status});
	});
var $author$project$App$Session$finishSave = function (state) {
	return _Utils_update(
		state,
		{saving: $author$project$App$Session$Idle});
};
var $author$project$Form$Goal$Baseline = {$: 'Baseline'};
var $author$project$Form$Goal$Budget = {$: 'Budget'};
var $author$project$Form$Goal$Deadline = {$: 'Deadline'};
var $author$project$Form$Goal$Description = {$: 'Description'};
var $author$project$Form$Goal$Parent = {$: 'Parent'};
var $author$project$Form$Goal$Permission = function (a) {
	return {$: 'Permission', a: a};
};
var $author$project$Form$Goal$StartsAt = {$: 'StartsAt'};
var $author$project$Form$Goal$Target = {$: 'Target'};
var $author$project$Form$Goal$fromKey = function (key) {
	switch (key) {
		case 'description':
			return $elm$core$Maybe$Just($author$project$Form$Goal$Description);
		case 'metricName':
			return $elm$core$Maybe$Just($author$project$Form$Goal$MetricName);
		case 'unit':
			return $elm$core$Maybe$Just($author$project$Form$Goal$Unit);
		case 'metricId':
			return $elm$core$Maybe$Just($author$project$Form$Goal$MetricId);
		case 'direction':
			return $elm$core$Maybe$Just($author$project$Form$Goal$Direction);
		case 'baseline':
			return $elm$core$Maybe$Just($author$project$Form$Goal$Baseline);
		case 'target':
			return $elm$core$Maybe$Just($author$project$Form$Goal$Target);
		case 'startsAt':
			return $elm$core$Maybe$Just($author$project$Form$Goal$StartsAt);
		case 'deadline':
			return $elm$core$Maybe$Just($author$project$Form$Goal$Deadline);
		case 'budget':
			return $elm$core$Maybe$Just($author$project$Form$Goal$Budget);
		case 'parent':
			return $elm$core$Maybe$Just($author$project$Form$Goal$Parent);
		default:
			return A2($elm$core$List$member, key, $author$project$Domain$Permission$permissionKeys) ? $elm$core$Maybe$Just(
				$author$project$Form$Goal$Permission(key)) : $elm$core$Maybe$Nothing;
	}
};
var $author$project$Form$Review$Decision = {$: 'Decision'};
var $author$project$Form$Review$DecisionDeadline = {$: 'DecisionDeadline'};
var $author$project$Form$Review$DecisionOwner = {$: 'DecisionOwner'};
var $author$project$Form$Review$Goal = {$: 'Goal'};
var $author$project$Form$Review$Learning = {$: 'Learning'};
var $author$project$Form$Review$Note = {$: 'Note'};
var $author$project$Form$Review$fromKey = function (key) {
	switch (key) {
		case 'goal':
			return $elm$core$Maybe$Just($author$project$Form$Review$Goal);
		case 'note':
			return $elm$core$Maybe$Just($author$project$Form$Review$Note);
		case 'learning':
			return $elm$core$Maybe$Just($author$project$Form$Review$Learning);
		case 'decision':
			return $elm$core$Maybe$Just($author$project$Form$Review$Decision);
		case 'decisionOwner':
			return $elm$core$Maybe$Just($author$project$Form$Review$DecisionOwner);
		case 'decisionDeadline':
			return $elm$core$Maybe$Just($author$project$Form$Review$DecisionDeadline);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $author$project$App$PageState$guide = F3(
	function (page, target, state) {
		return _Utils_update(
			state,
			{
				activity: _Utils_eq(page, $author$project$Page$ActivityLog) ? $author$project$Ui$Activity$init : state.activity,
				expandedGoal: A2($elm$core$String$startsWith, 'goal-', target) ? $elm$core$Maybe$Just(
					A2($elm$core$String$dropLeft, 5, target)) : state.expandedGoal,
				page: page
			});
	});
var $elm$core$Tuple$mapFirst = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			func(x),
			y);
	});
var $author$project$App$PageState$navigate = F2(
	function (page, state) {
		return _Utils_update(
			state,
			{activity: $author$project$Ui$Activity$init, graph: $author$project$Ui$ResponsibilityGraph$init, page: page, peopleQuery: '', peopleStatus: 'active', selectedPerson: $elm$core$Maybe$Nothing});
	});
var $author$project$App$Drafts$openDelete = F2(
	function (snapshot, state) {
		return _Utils_update(
			state,
			{
				deletion: $elm$core$Maybe$Just(snapshot)
			});
	});
var $author$project$App$PageState$openPerson = F2(
	function (key, state) {
		return _Utils_update(
			state,
			{
				selectedPerson: $elm$core$Maybe$Just(key)
			});
	});
var $author$project$Form$Goal$value = F2(
	function (draft, key) {
		switch (key.$) {
			case 'Description':
				return draft.description;
			case 'MetricName':
				return draft.metricName;
			case 'Unit':
				return draft.unit;
			case 'MetricId':
				return draft.metricId;
			case 'Direction':
				return draft.direction;
			case 'Baseline':
				return draft.baseline;
			case 'Target':
				return draft.target;
			case 'StartsAt':
				return draft.startsAt;
			case 'Deadline':
				return draft.deadline;
			case 'Budget':
				return draft.budget;
			case 'Parent':
				return draft.parent;
			default:
				var permission = key.a;
				return A2($elm$core$List$member, permission, draft.permissions) ? 'true' : 'false';
		}
	});
var $author$project$Form$Review$value = F2(
	function (draft, key) {
		switch (key.$) {
			case 'Goal':
				return draft.goal;
			case 'Note':
				return draft.note;
			case 'Learning':
				return draft.learning;
			case 'Decision':
				return draft.decision;
			case 'DecisionOwner':
				return draft.decisionOwner;
			default:
				return draft.decisionDeadline;
		}
	});
var $author$project$App$Drafts$get = F3(
	function (model, action, name) {
		switch (action.$) {
			case 'AddGoal':
				return A2(
					$elm$core$Maybe$withDefault,
					'',
					A2(
						$elm$core$Maybe$map,
						$author$project$Form$Goal$value(
							$author$project$App$Drafts$goalDraft(model)),
						$author$project$Form$Goal$fromKey(name)));
			case 'AddReview':
				return A2(
					$elm$core$Maybe$withDefault,
					'',
					A2(
						$elm$core$Maybe$map,
						$author$project$Form$Review$value(
							$author$project$App$Drafts$reviewDraft(model)),
						$author$project$Form$Review$fromKey(name)));
			default:
				return A2(
					$elm$core$Maybe$withDefault,
					A3($author$project$App$Drafts$defaultValue, model, action, name),
					A2(
						$elm$core$Maybe$andThen,
						$elm$core$Dict$get(name),
						A2(
							$elm$core$Dict$get,
							A2($author$project$App$Drafts$formKey, model, action),
							model.forms.drafts)));
		}
	});
var $author$project$App$Drafts$prepareReview = function (model) {
	return (A3($author$project$App$Drafts$get, model, $author$project$Form$Action$AddReview, 'goal') === '') ? A3($author$project$App$Drafts$editReview, $author$project$Form$Review$Goal, 'demo-revenue', model) : model.forms;
};
var $author$project$App$Agents$rebase = F2(
	function (org, state) {
		var _v0 = _Utils_Tuple2(
			A2($elm$core$Dict$get, org, state.drafts),
			A2($author$project$App$Agents$saved, org, state));
		if ((_v0.a.$ === 'Just') && (_v0.b.$ === 'Just')) {
			var draft = _v0.a.a;
			var latest = _v0.b.a;
			return _Utils_update(
				state,
				{
					drafts: A3(
						$elm$core$Dict$insert,
						org,
						_Utils_update(
							draft,
							{version: latest.version}),
						state.drafts)
				});
		} else {
			return state;
		}
	});
var $author$project$App$Discovery$rebase = F2(
	function (org, state) {
		var _v0 = _Utils_Tuple2(
			A2($elm$core$Dict$get, org, state.drafts),
			A2($author$project$App$Discovery$saved, org, state));
		if ((_v0.a.$ === 'Just') && (_v0.b.$ === 'Just')) {
			var draft = _v0.a.a;
			var latest = _v0.b.a;
			var doc = draft.discovery;
			return _Utils_update(
				state,
				{
					drafts: A3(
						$elm$core$Dict$insert,
						org,
						_Utils_update(
							draft,
							{
								discovery: _Utils_update(
									doc,
									{
										review: {note: doc.review.note, status: 'pending'}
									}),
								version: latest.version
							}),
						state.drafts)
				});
		} else {
			return state;
		}
	});
var $author$project$App$Agents$receive = F3(
	function (org, result, state) {
		if (result.$ === 'Ok') {
			var snapshot = result.a;
			return _Utils_update(
				state,
				{
					errors: A2($elm$core$Dict$remove, org, state.errors),
					loading: false,
					snapshots: A3($elm$core$Dict$insert, org, snapshot, state.snapshots)
				});
		} else {
			var error = result.a;
			return _Utils_update(
				state,
				{
					errors: A3($elm$core$Dict$insert, org, error, state.errors),
					loading: false
				});
		}
	});
var $author$project$App$Discovery$receive = F3(
	function (org, result, state) {
		if (result.$ === 'Ok') {
			var snapshot = result.a;
			return _Utils_update(
				state,
				{
					documents: A3($elm$core$Dict$insert, org, snapshot, state.documents),
					errors: A2($elm$core$Dict$remove, org, state.errors),
					loading: false
				});
		} else {
			var error = result.a;
			return _Utils_update(
				state,
				{
					errors: A3($elm$core$Dict$insert, org, error, state.errors),
					loading: false
				});
		}
	});
var $author$project$App$Update$receive = F4(
	function (token, result, session, model) {
		if (!_Utils_eq(token, model.session.request)) {
			return _Utils_Tuple2(model, _List_Nil);
		} else {
			if (result.$ === 'Ok') {
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{session: session}),
					_List_Nil);
			} else {
				var message = result.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{error: true, notice: message, session: session}),
					_List_Nil);
			}
		}
	});
var $author$project$Remote$Failed = function (a) {
	return {$: 'Failed', a: a};
};
var $author$project$App$Session$response = function (result) {
	if (result.$ === 'Ok') {
		var value = result.a;
		return $author$project$Remote$Loaded(value);
	} else {
		var message = result.a;
		return $author$project$Remote$Failed(message);
	}
};
var $author$project$App$Session$succeeded = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $author$project$App$Session$receiveOrganizations = F2(
	function (result, state) {
		return _Utils_update(
			state,
			{
				fresh: $author$project$App$Session$succeeded(result),
				organizations: $author$project$App$Session$response(result),
				syncing: false
			});
	});
var $author$project$App$Session$receiveWorkspace = F2(
	function (result, state) {
		return _Utils_update(
			state,
			{
				fresh: $author$project$App$Session$succeeded(result),
				syncing: false,
				workspace: $author$project$App$Session$response(result)
			});
	});
var $author$project$Form$Action$DeactivatePerson = function (a) {
	return {$: 'DeactivatePerson', a: a};
};
var $author$project$Form$Action$UpdatePerson = function (a) {
	return {$: 'UpdatePerson', a: a};
};
var $author$project$App$Drafts$resetPerson = F2(
	function (key, model) {
		var state = model.forms;
		return _Utils_update(
			state,
			{
				drafts: A2(
					$elm$core$Dict$remove,
					A2(
						$author$project$App$Drafts$formKey,
						model,
						$author$project$Form$Action$DeactivatePerson(key)),
					A2(
						$elm$core$Dict$remove,
						A2(
							$author$project$App$Drafts$formKey,
							model,
							$author$project$Form$Action$UpdatePerson(key)),
						state.drafts))
			});
	});
var $author$project$Form$Action$DeleteOrg = {$: 'DeleteOrg'};
var $author$project$App$Session$organizationDeleted = function (state) {
	return _Utils_update(
		state,
		{org: $elm$core$Maybe$Nothing, organizations: $author$project$Remote$Loading, workspace: $author$project$Remote$Loading});
};
var $author$project$App$Agents$remove = F2(
	function (org, state) {
		return _Utils_update(
			state,
			{
				drafts: A2($elm$core$Dict$remove, org, state.drafts),
				errors: A2($elm$core$Dict$remove, org, state.errors),
				snapshots: A2($elm$core$Dict$remove, org, state.snapshots)
			});
	});
var $author$project$App$Discovery$remove = F2(
	function (org, state) {
		return _Utils_update(
			state,
			{
				documents: A2($elm$core$Dict$remove, org, state.documents),
				drafts: A2($elm$core$Dict$remove, org, state.drafts),
				errors: A2($elm$core$Dict$remove, org, state.errors)
			});
	});
var $elm$core$Dict$filter = F2(
	function (isGood, dict) {
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (k, v, d) {
					return A2(isGood, k, v) ? A3($elm$core$Dict$insert, k, v, d) : d;
				}),
			$elm$core$Dict$empty,
			dict);
	});
var $author$project$App$Drafts$removeOrganization = F2(
	function (org, state) {
		var keep = F2(
			function (key, _v0) {
				return !A2(
					$elm$core$String$startsWith,
					A2($elm$core$Maybe$withDefault, '', org) + '/',
					key);
			});
		return _Utils_update(
			state,
			{
				drafts: A2($elm$core$Dict$filter, keep, state.drafts),
				goalDrafts: A2($elm$core$Dict$filter, keep, state.goalDrafts),
				reviewDrafts: A2($elm$core$Dict$filter, keep, state.reviewDrafts)
			});
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (_v0.$ === 'Just') {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $author$project$App$Drafts$saved = F2(
	function (action, model) {
		var state = model.forms;
		var key = A2($author$project$App$Drafts$formKey, model, action);
		return _Utils_update(
			state,
			{
				deletion: $elm$core$Maybe$Nothing,
				drafts: A2($elm$core$Dict$remove, key, state.drafts),
				goalDrafts: _Utils_eq(action, $author$project$Form$Action$AddGoal) ? A2($elm$core$Dict$remove, key, state.goalDrafts) : state.goalDrafts,
				goalSerial: _Utils_eq(action, $author$project$Form$Action$AddGoal) ? A3(
					$elm$core$Dict$update,
					A2($elm$core$Maybe$withDefault, '', model.session.org),
					A2(
						$elm$core$Basics$composeR,
						$elm$core$Maybe$withDefault(0),
						A2(
							$elm$core$Basics$composeR,
							$elm$core$Basics$add(1),
							$elm$core$Maybe$Just)),
					state.goalSerial) : state.goalSerial,
				reviewDrafts: _Utils_eq(action, $author$project$Form$Action$AddReview) ? A2($elm$core$Dict$remove, key, state.reviewDrafts) : state.reviewDrafts
			});
	});
var $author$project$App$PageState$setPage = F2(
	function (page, state) {
		return _Utils_update(
			state,
			{page: page});
	});
var $author$project$App$Update$saved = F3(
	function (action, response, model) {
		if (response.$ === 'Err') {
			var message = response.a;
			return $author$project$App$Update$refresh(
				_Utils_update(
					model,
					{
						error: true,
						forms: $author$project$App$Drafts$closeDelete(model.forms),
						notice: message + ' 자동 재시도하지 않았습니다. 최신 상태를 확인한 뒤 다시 저장하세요. 입력 내용은 보존됩니다.',
						session: $author$project$App$Session$finishSave(model.session)
					}));
		} else {
			var next = _Utils_update(
				model,
				{
					error: false,
					forms: A2($author$project$App$Drafts$saved, action, model),
					notice: '저장했습니다. 최신 조직 상태와 감사 기록을 확인하세요.',
					session: $author$project$App$Session$finishSave(model.session)
				});
			return _Utils_eq(action, $author$project$Form$Action$DeleteOrg) ? $author$project$App$Update$refresh(
				_Utils_update(
					next,
					{
						agents: A2(
							$elm$core$Maybe$withDefault,
							next.agents,
							A2(
								$elm$core$Maybe$map,
								function (org) {
									return A2($author$project$App$Agents$remove, org, next.agents);
								},
								model.session.org)),
						discovery: A2(
							$elm$core$Maybe$withDefault,
							next.discovery,
							A2(
								$elm$core$Maybe$map,
								function (org) {
									return A2($author$project$App$Discovery$remove, org, next.discovery);
								},
								model.session.org)),
						forms: A2($author$project$App$Drafts$removeOrganization, model.session.org, next.forms),
						notice: '조직을 논리 삭제했습니다. 원본 감사 기록과 다른 조직은 보존됩니다.',
						pageState: A2($author$project$App$PageState$setPage, $author$project$Page$Organizations, next.pageState),
						session: $author$project$App$Session$organizationDeleted(next.session)
					})) : $author$project$App$Update$refresh(next);
		}
	});
var $author$project$App$PageState$searchPeople = F2(
	function (query, state) {
		return _Utils_update(
			state,
			{peopleQuery: query});
	});
var $author$project$App$Session$selectOrganization = F2(
	function (org, state) {
		return _Utils_update(
			state,
			{org: org, workspace: $author$project$Remote$Loading});
	});
var $author$project$App$PageState$setActivity = F2(
	function (activity, state) {
		return _Utils_update(
			state,
			{activity: activity});
	});
var $author$project$Page$pageName = function (page) {
	switch (page.$) {
		case 'Organizations':
			return '조직 목록';
		case 'Discovery':
			return '조직 진단';
		case 'Workflows':
			return '업무 흐름';
		case 'AgentDrafts':
			return '에이전트 초안';
		case 'AgentGraph':
			return '에이전트 구조';
		case 'People':
			return '구성원';
		case 'Dashboard':
			return '목표';
		case 'Responsibility':
			return '책임';
		case 'Authorities':
			return '권한';
		case 'Results':
			return '결과';
		case 'Reviews':
			return '학습';
		case 'ActivityLog':
			return '활동 기록';
		default:
			return '조직 설정';
	}
};
var $author$project$App$PageState$setListMode = F3(
	function (page, mode, state) {
		return _Utils_update(
			state,
			{
				listModes: A3(
					$elm$core$Dict$insert,
					$author$project$Page$pageName(page),
					mode,
					state.listModes)
			});
	});
var $author$project$App$Effect$SaveCommand = F5(
	function (a, b, c, d, e) {
		return {$: 'SaveCommand', a: a, b: b, c: c, d: d, e: e};
	});
var $author$project$App$Session$Saving = function (a) {
	return {$: 'Saving', a: a};
};
var $author$project$App$Session$beginSave = F2(
	function (key, state) {
		return _Utils_update(
			state,
			{
				saving: $author$project$App$Session$Saving(key)
			});
	});
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$core$Result$map = F2(
	function (func, ra) {
		if (ra.$ === 'Ok') {
			var a = ra.a;
			return $elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return $elm$core$Result$Err(e);
		}
	});
var $elm$url$Url$percentEncode = _Url_percentEncode;
var $author$project$Api$Path$orgPath = F2(
	function (org, tail) {
		return '/api/organizations/' + ($elm$url$Url$percentEncode(org) + ((tail === '') ? '' : ('/' + tail)));
	});
var $elm$core$String$toInt = _String_toInt;
var $author$project$Form$Goal$validate = function (draft) {
	if (A2(
		$elm$core$List$any,
		A2(
			$elm$core$Basics$composeR,
			$elm$core$String$trim,
			$elm$core$Basics$eq('')),
		_List_fromArray(
			[draft.description, draft.metricId, draft.metricName, draft.unit, draft.startsAt, draft.deadline]))) {
		return $elm$core$Result$Err('필수 항목을 모두 입력하세요.');
	} else {
		var _v0 = _Utils_Tuple3(
			$elm$core$String$toFloat(draft.baseline),
			$elm$core$String$toFloat(draft.target),
			$elm$core$String$toFloat(draft.budget));
		if (((_v0.a.$ === 'Just') && (_v0.b.$ === 'Just')) && (_v0.c.$ === 'Just')) {
			var baseline = _v0.a.a;
			var target = _v0.b.a;
			var budget = _v0.c.a;
			return (_Utils_cmp(draft.deadline, draft.startsAt) < 0) ? $elm$core$Result$Err('마감일은 시작일 이후여야 합니다.') : $elm$core$Result$Ok(
				{baseline: baseline, budget: budget, deadline: draft.deadline, description: draft.description, direction: draft.direction, metricId: draft.metricId, metricName: draft.metricName, parent: draft.parent, permissions: draft.permissions, startsAt: draft.startsAt, target: target, unit: draft.unit});
		} else {
			return $elm$core$Result$Err('숫자 항목을 올바르게 입력하세요.');
		}
	}
};
var $author$project$Form$Review$validate = function (draft) {
	return A2(
		$elm$core$List$any,
		A2(
			$elm$core$Basics$composeR,
			$elm$core$String$trim,
			$elm$core$Basics$eq('')),
		_List_fromArray(
			[draft.goal, draft.note])) ? $elm$core$Result$Err('필수 항목을 모두 입력하세요.') : ((($elm$core$String$trim(draft.decision) !== '') && (draft.decisionOwner === '')) ? $elm$core$Result$Err('다음 결정의 담당자를 선택하세요.') : $elm$core$Result$Ok(draft));
};
var $author$project$Api$Command$payload = F2(
	function (model, action) {
		var version = A2(
			$elm$core$Maybe$withDefault,
			model.version,
			$elm$core$String$toInt(
				A2(model.value, action, '__version')));
		var val = model.value(action);
		var uid = function (prefix) {
			return $elm$json$Json$Encode$string(
				prefix + ('-' + (model.seed + ('-' + $elm$core$String$fromInt(model.serial)))));
		};
		var str = function (key) {
			return _Utils_Tuple2(
				key,
				$elm$json$Json$Encode$string(
					val(key)));
		};
		var ps = A2(
			$elm$json$Json$Encode$list,
			$elm$json$Json$Encode$string,
			A2(
				$elm$core$List$filter,
				function (key) {
					return val(key) === 'true';
				},
				$author$project$Domain$Permission$permissionKeys));
		var post = F2(
			function (route, fields) {
				return $elm$core$Result$Ok(
					_Utils_Tuple3(
						'POST',
						route,
						$elm$json$Json$Encode$object(fields)));
			});
		var path = function (tail) {
			return A2(
				$author$project$Api$Path$orgPath,
				A2($elm$core$Maybe$withDefault, '', model.org),
				tail);
		};
		var num = function (key) {
			return $elm$json$Json$Encode$float(
				A2(
					$elm$core$Maybe$withDefault,
					0,
					$elm$core$String$toFloat(
						val(key))));
		};
		var nullable = function (value_) {
			return (value_ === '') ? $elm$json$Json$Encode$null : $elm$json$Json$Encode$string(value_);
		};
		var profile = _List_fromArray(
			[
				str('name'),
				str('role'),
				_Utils_Tuple2(
				'department',
				nullable(
					$elm$core$String$trim(
						val('department')))),
				_Utils_Tuple2(
				'email',
				nullable(
					$elm$core$String$trim(
						val('email')))),
				_Utils_Tuple2(
				'reportsTo',
				nullable(
					val('reportsTo')))
			]);
		var current = function (result) {
			return (!_Utils_eq(version, model.version)) ? $elm$core$Result$Err('작성 중 조직이 변경되었습니다. ‘최신 정보로 다시 불러오기’를 눌러 변경 내용을 확인한 뒤 다시 작성해 주세요.') : result;
		};
		var blank = function (keys) {
			return A2(
				$elm$core$List$any,
				function (key) {
					return $elm$core$String$trim(
						val(key)) === '';
				},
				keys);
		};
		var badNumber = function (keys) {
			return A2(
				$elm$core$List$any,
				function (key) {
					return _Utils_eq(
						$elm$core$String$toFloat(
							val(key)),
						$elm$core$Maybe$Nothing);
				},
				keys);
		};
		var validate = F3(
			function (keys, nums, result) {
				return blank(keys) ? $elm$core$Result$Err('필수 항목을 모두 입력하세요.') : (badNumber(nums) ? $elm$core$Result$Err('숫자 항목을 올바르게 입력하세요.') : result);
			});
		switch (action.$) {
			case 'CreateOrg':
				return A3(
					validate,
					_List_fromArray(
						['name']),
					_List_Nil,
					A2(
						post,
						'/api/organizations',
						_List_fromArray(
							[
								_Utils_Tuple2(
								'id',
								uid('org')),
								str('name')
							])));
			case 'ImportDemo':
				return A2(post, '/api/demo', _List_Nil);
			case 'Rename':
				return A3(
					validate,
					_List_fromArray(
						['name']),
					_List_Nil,
					(!_Utils_eq(version, model.version)) ? $elm$core$Result$Err('작성 중 조직이 변경되었습니다. 최신 조직 이름을 확인하고 수정 입력을 다시 해 주세요.') : $elm$core$Result$Ok(
						_Utils_Tuple3(
							'PATCH',
							path(''),
							$elm$json$Json$Encode$object(
								_List_fromArray(
									[
										str('name'),
										_Utils_Tuple2(
										'expectedVersion',
										$elm$json$Json$Encode$int(version))
									])))));
			case 'AddPerson':
				return A3(
					validate,
					_List_fromArray(
						['name', 'role']),
					_List_Nil,
					A2(
						post,
						path('people'),
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								'id',
								uid('person')),
							profile)));
			case 'UpdatePerson':
				var key = action.a;
				return A3(
					validate,
					_List_fromArray(
						['name', 'role']),
					_List_Nil,
					current(
						$elm$core$Result$Ok(
							_Utils_Tuple3(
								'PATCH',
								path(
									'people/' + $elm$url$Url$percentEncode(key)),
								$elm$json$Json$Encode$object(
									A2(
										$elm$core$List$cons,
										_Utils_Tuple2(
											'expectedVersion',
											$elm$json$Json$Encode$int(version)),
										profile))))));
			case 'DeactivatePerson':
				var key = action.a;
				return current(
					A2(
						post,
						path(
							'people/' + ($elm$url$Url$percentEncode(key) + '/deactivate')),
						_List_fromArray(
							[
								_Utils_Tuple2(
								'successor',
								nullable(
									val('successor'))),
								_Utils_Tuple2(
								'expectedVersion',
								$elm$json$Json$Encode$int(version))
							])));
			case 'AddGoal':
				return A2(
					$elm$core$Result$map,
					function (goal) {
						return _Utils_Tuple3(
							'POST',
							path('goals'),
							$elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'id',
										uid('goal')),
										_Utils_Tuple2(
										'organization',
										$elm$json$Json$Encode$string(
											A2($elm$core$Maybe$withDefault, '', model.org))),
										_Utils_Tuple2(
										'description',
										$elm$json$Json$Encode$string(goal.description)),
										_Utils_Tuple2(
										'metric',
										$elm$json$Json$Encode$object(
											_List_fromArray(
												[
													_Utils_Tuple2(
													'id',
													$elm$json$Json$Encode$string(goal.metricId)),
													_Utils_Tuple2(
													'name',
													$elm$json$Json$Encode$string(goal.metricName)),
													_Utils_Tuple2(
													'unit',
													$elm$json$Json$Encode$string(goal.unit)),
													_Utils_Tuple2(
													'direction',
													$elm$json$Json$Encode$string(goal.direction))
												]))),
										_Utils_Tuple2(
										'baseline',
										$elm$json$Json$Encode$float(goal.baseline)),
										_Utils_Tuple2(
										'target',
										$elm$json$Json$Encode$float(goal.target)),
										_Utils_Tuple2(
										'startsAt',
										$elm$json$Json$Encode$string(goal.startsAt + 'T00:00:00Z')),
										_Utils_Tuple2(
										'deadline',
										$elm$json$Json$Encode$string(goal.deadline + 'T00:00:00Z')),
										_Utils_Tuple2(
										'parent',
										nullable(goal.parent)),
										_Utils_Tuple2(
										'requiredPermissions',
										A2(
											$elm$json$Json$Encode$list,
											$elm$json$Json$Encode$string,
											A2(
												$elm$core$List$filter,
												function (key) {
													return A2($elm$core$List$member, key, goal.permissions);
												},
												$author$project$Domain$Permission$permissionKeys))),
										_Utils_Tuple2(
										'requiredBudget',
										$elm$json$Json$Encode$float(goal.budget))
									])));
					},
					$author$project$Form$Goal$validate(model.goal));
			case 'Assign':
				var key = action.a;
				return A3(
					validate,
					_List_fromArray(
						['owner']),
					_List_Nil,
					A2(
						post,
						path(
							'goals/' + ($elm$url$Url$percentEncode(key) + '/owner')),
						_List_fromArray(
							[
								str('owner')
							])));
			case 'Grant':
				var key = action.a;
				return A3(
					validate,
					_List_Nil,
					_List_fromArray(
						['budget']),
					A2(
						post,
						path(
							'people/' + ($elm$url$Url$percentEncode(key) + '/authority')),
						_List_fromArray(
							[
								_Utils_Tuple2(
								'owner',
								$elm$json$Json$Encode$string(key)),
								_Utils_Tuple2(
								'budgetLimit',
								num('budget')),
								_Utils_Tuple2(
								'canHire',
								$elm$json$Json$Encode$bool(false)),
								_Utils_Tuple2(
								'canChangePrice',
								$elm$json$Json$Encode$bool(false)),
								_Utils_Tuple2('canApprove', ps)
							])));
			case 'Report':
				var key = action.a;
				return A3(
					validate,
					_List_fromArray(
						['reportedBy', 'note']),
					_List_fromArray(
						['value']),
					A2(
						post,
						path(
							'goals/' + ($elm$url$Url$percentEncode(key) + '/results')),
						_List_fromArray(
							[
								_Utils_Tuple2(
								'value',
								num('value')),
								str('reportedBy'),
								str('note'),
								_Utils_Tuple2(
								'actor',
								$elm$json$Json$Encode$string(
									val('reportedBy')))
							])));
			case 'Strategy':
				var key = action.a;
				return A3(
					validate,
					_List_fromArray(
						['note']),
					_List_Nil,
					A2(
						post,
						path(
							'goals/' + ($elm$url$Url$percentEncode(key) + '/strategy')),
						_List_fromArray(
							[
								str('note')
							])));
			case 'Activate':
				var key = action.a;
				return A2(
					post,
					path(
						'goals/' + ($elm$url$Url$percentEncode(key) + '/activate')),
					_List_Nil);
			case 'Evaluate':
				var key = action.a;
				return A2(
					post,
					path('evaluations'),
					_List_fromArray(
						[
							_Utils_Tuple2(
							'goal',
							$elm$json$Json$Encode$string(key))
						]));
			case 'AddReview':
				return A2(
					$elm$core$Result$map,
					function (review) {
						return _Utils_Tuple3(
							'POST',
							path('reviews'),
							$elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'id',
										uid('review')),
										_Utils_Tuple2(
										'goal',
										$elm$json$Json$Encode$string(review.goal)),
										_Utils_Tuple2(
										'note',
										$elm$json$Json$Encode$string(review.note)),
										_Utils_Tuple2(
										'learnings',
										A2(
											$elm$json$Json$Encode$list,
											$elm$core$Basics$identity,
											($elm$core$String$trim(review.learning) === '') ? _List_Nil : _List_fromArray(
												[
													$elm$json$Json$Encode$object(
													_List_fromArray(
														[
															_Utils_Tuple2(
															'text',
															$elm$json$Json$Encode$string(review.learning))
														]))
												]))),
										_Utils_Tuple2(
										'decisions',
										A2(
											$elm$json$Json$Encode$list,
											$elm$core$Basics$identity,
											($elm$core$String$trim(review.decision) === '') ? _List_Nil : _List_fromArray(
												[
													$elm$json$Json$Encode$object(
													_List_fromArray(
														[
															_Utils_Tuple2(
															'text',
															$elm$json$Json$Encode$string(review.decision)),
															_Utils_Tuple2(
															'owner',
															$elm$json$Json$Encode$string(review.decisionOwner)),
															_Utils_Tuple2(
															'deadline',
															(review.decisionDeadline === '') ? $elm$json$Json$Encode$null : $elm$json$Json$Encode$string(review.decisionDeadline + 'T23:59:59Z'))
														]))
												])))
									])));
					},
					$author$project$Form$Review$validate(model.review));
			default:
				var _v1 = model.deletion;
				if (_v1.$ === 'Just') {
					var snapshot = _v1.a;
					return (_Utils_eq(snapshot.confirmation, snapshot.name) && _Utils_eq(
						model.org,
						$elm$core$Maybe$Just(snapshot.id))) ? $elm$core$Result$Ok(
						_Utils_Tuple3(
							'DELETE',
							A2($author$project$Api$Path$orgPath, snapshot.id, ''),
							$elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'confirmName',
										$elm$json$Json$Encode$string(snapshot.confirmation)),
										_Utils_Tuple2(
										'expectedVersion',
										$elm$json$Json$Encode$int(snapshot.version))
									])))) : $elm$core$Result$Err('조직 이름을 정확히 입력하세요.');
				} else {
					return $elm$core$Result$Err('삭제 확인을 먼저 열어 주세요.');
				}
		}
	});
var $author$project$App$Drafts$payload = F2(
	function (model, action) {
		return A2(
			$author$project$Api$Command$payload,
			{
				deletion: model.forms.deletion,
				goal: $author$project$App$Drafts$goalDraft(model),
				org: model.session.org,
				review: $author$project$App$Drafts$reviewDraft(model),
				seed: model.flags.seed,
				serial: model.forms.serial,
				value: $author$project$App$Drafts$get(model),
				version: $author$project$App$Drafts$workspaceVersion(model)
			},
			action);
	});
var $author$project$App$Update$payload = $author$project$App$Drafts$payload;
var $author$project$App$Update$submit = F2(
	function (action, model) {
		if ($author$project$App$Update$busy(model)) {
			return _Utils_Tuple2(model, _List_Nil);
		} else {
			if (!model.session.fresh) {
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{error: true, notice: '최신 상태를 먼저 불러와 주세요. 입력 내용은 보존됩니다.'}),
					_List_Nil);
			} else {
				var _v0 = A2($author$project$App$Update$payload, model, action);
				if (_v0.$ === 'Err') {
					var message = _v0.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{error: true, notice: message}),
						_List_Nil);
				} else {
					var _v1 = _v0.a;
					var method = _v1.a;
					var path = _v1.b;
					var body = _v1.c;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								error: false,
								forms: $author$project$App$Drafts$advanceSerial(model.forms),
								notice: '저장 중입니다…',
								session: A2(
									$author$project$App$Session$beginSave,
									$author$project$Form$Action$actionKey(action),
									model.session)
							}),
						_List_fromArray(
							[
								A5($author$project$App$Effect$SaveCommand, model.session.request, action, method, path, body)
							]));
				}
			}
		}
	});
var $author$project$App$Effect$SaveAgents = F4(
	function (a, b, c, d) {
		return {$: 'SaveAgents', a: a, b: b, c: c, d: d};
	});
var $author$project$App$Agents$conflicted = F2(
	function (org, state) {
		var _v0 = _Utils_Tuple2(
			A2($elm$core$Dict$get, org, state.drafts),
			A2($author$project$App$Agents$saved, org, state));
		if ((_v0.a.$ === 'Just') && (_v0.b.$ === 'Just')) {
			var draft = _v0.a.a;
			var latest = _v0.b.a;
			return !_Utils_eq(draft.version, latest.version);
		} else {
			return false;
		}
	});
var $author$project$Domain$Agent$problems = function (roles) {
	return A2(
		$elm$core$List$concatMap,
		function (role) {
			return _Utils_ap(
				($elm$core$String$trim(role.name) === '') ? _List_fromArray(
					[role.id + ': 역할 이름을 입력하세요.']) : _List_Nil,
				((role.status === 'confirmed') && ($elm$core$String$trim(role.evidence) === '')) ? _List_fromArray(
					[role.name + ': 확인된 사실에는 근거가 필요합니다.']) : _List_Nil);
		},
		roles);
};
var $author$project$App$Update$submitAgents = function (model) {
	var _v0 = model.session.org;
	if (_v0.$ === 'Nothing') {
		return _Utils_Tuple2(model, _List_Nil);
	} else {
		var org = _v0.a;
		var _v1 = A2($author$project$App$Agents$current, org, model.agents);
		if (_v1.$ === 'Nothing') {
			return _Utils_Tuple2(model, _List_Nil);
		} else {
			var design = _v1.a;
			return ($author$project$App$Update$busy(model) || (model.agents.loading || ((!model.session.fresh) || A2($elm$core$Dict$member, org, model.agents.errors)))) ? _Utils_Tuple2(
				_Utils_update(
					model,
					{error: true, notice: '최신 설계를 불러온 뒤 저장하세요. 입력은 보존됩니다.'}),
				_List_Nil) : (A2($author$project$App$Agents$conflicted, org, model.agents) ? _Utils_Tuple2(
				_Utils_update(
					model,
					{error: true, notice: '입력 중 저장된 조직이 변경되었습니다. 최신 저장 내용과 비교한 뒤 다시 적용하세요.'}),
				_List_Nil) : ((!$elm$core$List$isEmpty(
				$author$project$Domain$Agent$problems(design.agents))) ? _Utils_Tuple2(
				_Utils_update(
					model,
					{
						error: true,
						notice: A2(
							$elm$core$String$join,
							' ',
							$author$project$Domain$Agent$problems(design.agents))
					}),
				_List_Nil) : _Utils_Tuple2(
				_Utils_update(
					model,
					{
						error: false,
						notice: '설계안 저장 중…',
						session: A2($author$project$App$Session$beginSave, 'agents', model.session)
					}),
				_List_fromArray(
					[
						A4($author$project$App$Effect$SaveAgents, model.session.request, org, design.version, design.agents)
					]))));
		}
	}
};
var $author$project$App$Effect$SaveDiscovery = F3(
	function (a, b, c) {
		return {$: 'SaveDiscovery', a: a, b: b, c: c};
	});
var $author$project$App$Discovery$conflicted = F2(
	function (org, state) {
		var _v0 = _Utils_Tuple2(
			A2($author$project$App$Discovery$current, org, state),
			A2($author$project$App$Discovery$saved, org, state));
		if ((_v0.a.$ === 'Just') && (_v0.b.$ === 'Just')) {
			var draft = _v0.a.a;
			var latest = _v0.b.a;
			return !_Utils_eq(draft.version, latest.version);
		} else {
			return false;
		}
	});
var $author$project$Domain$Discovery$problems = function (doc) {
	var blank = A2($elm$core$Basics$composeR, $elm$core$String$trim, $elm$core$String$isEmpty);
	var observation = function (o) {
		return _Utils_ap(
			blank(o.subject) ? _List_fromArray(
				['현황 항목의 제목을 입력하세요.']) : _List_Nil,
			((o.status === 'confirmed') && blank(o.evidence)) ? _List_fromArray(
				[o.subject + ': 확인된 사실에는 근거가 필요합니다.']) : _List_Nil);
	};
	var workflow = function (w) {
		return _Utils_ap(
			blank(w.name) ? _List_fromArray(
				['업무 이름을 입력하세요.']) : _List_Nil,
			((w.status === 'confirmed') && blank(w.evidence)) ? _List_fromArray(
				[w.name + ': 확인된 사실에는 근거가 필요합니다.']) : _List_Nil);
	};
	return _Utils_ap(
		A2($elm$core$List$concatMap, observation, doc.observations),
		A2($elm$core$List$concatMap, workflow, doc.workflows));
};
var $author$project$App$Update$submitDiscovery = function (model) {
	var _v0 = model.session.org;
	if (_v0.$ === 'Nothing') {
		return _Utils_Tuple2(model, _List_Nil);
	} else {
		var org = _v0.a;
		var _v1 = A2($author$project$App$Discovery$current, org, model.discovery);
		if (_v1.$ === 'Nothing') {
			return _Utils_Tuple2(model, _List_Nil);
		} else {
			var snapshot = _v1.a;
			return ($author$project$App$Update$busy(model) || (model.discovery.loading || ((!model.session.fresh) || A2($elm$core$Dict$member, org, model.discovery.errors)))) ? _Utils_Tuple2(
				_Utils_update(
					model,
					{error: true, notice: '최신 현황을 불러온 뒤 저장하세요. 입력은 보존됩니다.'}),
				_List_Nil) : (A2($author$project$App$Discovery$conflicted, org, model.discovery) ? _Utils_Tuple2(
				_Utils_update(
					model,
					{error: true, notice: '입력 중 저장된 조직이 변경되었습니다. 최신 저장 내용과 비교한 뒤 다시 적용하세요.'}),
				_List_Nil) : ((!$elm$core$List$isEmpty(
				$author$project$Domain$Discovery$problems(snapshot.discovery))) ? _Utils_Tuple2(
				_Utils_update(
					model,
					{
						error: true,
						notice: A2(
							$elm$core$String$join,
							' ',
							$author$project$Domain$Discovery$problems(snapshot.discovery))
					}),
				_List_Nil) : _Utils_Tuple2(
				_Utils_update(
					model,
					{
						error: false,
						notice: '현황 저장 중…',
						session: A2($author$project$App$Session$beginSave, 'discovery', model.session)
					}),
				_List_fromArray(
					[
						A3($author$project$App$Effect$SaveDiscovery, model.session.request, org, snapshot)
					]))));
		}
	}
};
var $author$project$App$PageState$toggleGuide = function (state) {
	return _Utils_update(
		state,
		{guideOpen: !state.guideOpen});
};
var $elm$core$Basics$clamp = F3(
	function (low, high, number) {
		return (_Utils_cmp(number, low) < 0) ? low : ((_Utils_cmp(number, high) > 0) ? high : number);
	});
var $author$project$Ui$ResponsibilityGraph$update = F2(
	function (msg, state) {
		switch (msg.$) {
			case 'SetDiagram':
				var value = msg.a;
				return _Utils_update(
					state,
					{diagram: value});
			case 'Search':
				var value = msg.a;
				return _Utils_update(
					state,
					{query: value, selected: $elm$core$Maybe$Nothing});
			case 'Dependencies':
				var value = msg.a;
				return _Utils_update(
					state,
					{showDependencies: value});
			case 'Resources':
				var value = msg.a;
				return _Utils_update(
					state,
					{
						selected: ((!value) && A2(
							$elm$core$Maybe$withDefault,
							false,
							A2(
								$elm$core$Maybe$map,
								$elm$core$String$startsWith('ResourceNode:'),
								state.selected))) ? $elm$core$Maybe$Nothing : state.selected,
						showResources: value
					});
			case 'Select':
				var key = msg.a;
				return _Utils_update(
					state,
					{
						selected: $elm$core$Maybe$Just(key)
					});
			case 'ClearSelection':
				return _Utils_update(
					state,
					{selected: $elm$core$Maybe$Nothing});
			case 'Zoom':
				var delta = msg.a;
				return _Utils_update(
					state,
					{
						zoom: A3($elm$core$Basics$clamp, 1, 3, state.zoom + delta)
					});
			default:
				return _Utils_update(
					state,
					{zoom: 1});
		}
	});
var $author$project$App$PageState$updateGraph = F2(
	function (message, state) {
		return _Utils_update(
			state,
			{
				graph: A2($author$project$Ui$ResponsibilityGraph$update, message, state.graph)
			});
	});
var $author$project$App$Update$update = F2(
	function (msg, model) {
		update:
		while (true) {
			switch (msg.$) {
				case 'GotDiscovery':
					var token = msg.a;
					var org = msg.b;
					var response = msg.c;
					return ((!_Utils_eq(token, model.session.request)) || (!_Utils_eq(
						$elm$core$Maybe$Just(org),
						model.session.org))) ? _Utils_Tuple2(model, _List_Nil) : _Utils_Tuple2(
						_Utils_update(
							model,
							{
								discovery: A3($author$project$App$Discovery$receive, org, response, model.discovery)
							}),
						_List_Nil);
				case 'EditDiscovery':
					var change = msg.a;
					return ($author$project$App$Update$busy(model) || (model.discovery.loading || A2(
						$elm$core$Maybe$withDefault,
						false,
						A2(
							$elm$core$Maybe$map,
							function (org) {
								return A2($elm$core$Dict$member, org, model.discovery.errors);
							},
							model.session.org)))) ? _Utils_Tuple2(model, _List_Nil) : _Utils_Tuple2(
						_Utils_update(
							model,
							{
								discovery: A2(
									$elm$core$Maybe$withDefault,
									model.discovery,
									A2(
										$elm$core$Maybe$map,
										function (org) {
											return A3($author$project$App$Discovery$edit, org, change, model.discovery);
										},
										model.session.org))
							}),
						_List_Nil);
				case 'AddObservation':
					var $temp$msg = $author$project$App$Update$EditDiscovery(
						$author$project$Domain$Discovery$AddObservation(
							'observation-' + (model.flags.seed + ('-' + $elm$core$String$fromInt(model.forms.serial))))),
						$temp$model = _Utils_update(
						model,
						{
							forms: $author$project$App$Drafts$advanceSerial(model.forms)
						});
					msg = $temp$msg;
					model = $temp$model;
					continue update;
				case 'AddWorkflow':
					var $temp$msg = $author$project$App$Update$EditDiscovery(
						$author$project$Domain$Discovery$AddWorkflow(
							'workflow-' + (model.flags.seed + ('-' + $elm$core$String$fromInt(model.forms.serial))))),
						$temp$model = _Utils_update(
						model,
						{
							forms: $author$project$App$Drafts$advanceSerial(model.forms)
						});
					msg = $temp$msg;
					model = $temp$model;
					continue update;
				case 'ResetDiscovery':
					return $author$project$App$Update$busy(model) ? _Utils_Tuple2(model, _List_Nil) : _Utils_Tuple2(
						_Utils_update(
							model,
							{
								discovery: A2(
									$elm$core$Maybe$withDefault,
									model.discovery,
									A2(
										$elm$core$Maybe$map,
										function (org) {
											return A2($author$project$App$Discovery$clearDraft, org, model.discovery);
										},
										model.session.org))
							}),
						_List_Nil);
				case 'RebaseDiscovery':
					return $author$project$App$Update$busy(model) ? _Utils_Tuple2(model, _List_Nil) : _Utils_Tuple2(
						_Utils_update(
							model,
							{
								discovery: A2(
									$elm$core$Maybe$withDefault,
									model.discovery,
									A2(
										$elm$core$Maybe$map,
										function (org) {
											return A2($author$project$App$Discovery$rebase, org, model.discovery);
										},
										model.session.org)),
								notice: '최신 버전에 입력을 다시 적용했습니다. 내용을 검토한 뒤 저장하세요.'
							}),
						_List_Nil);
				case 'SubmitDiscovery':
					return $author$project$App$Update$submitDiscovery(model);
				case 'SavedDiscovery':
					var token = msg.a;
					var org = msg.b;
					var response = msg.c;
					if ((!_Utils_eq(token, model.session.request)) || (!_Utils_eq(
						$elm$core$Maybe$Just(org),
						model.session.org))) {
						return _Utils_Tuple2(model, _List_Nil);
					} else {
						if (response.$ === 'Ok') {
							return $author$project$App$Update$refresh(
								_Utils_update(
									model,
									{
										discovery: A2($author$project$App$Discovery$clearDraft, org, model.discovery),
										error: false,
										notice: '현황을 저장했습니다. 저장된 근거로 에이전트 초안을 다시 확인하세요.',
										session: $author$project$App$Session$finishSave(model.session)
									}));
						} else {
							var message = response.a;
							return $author$project$App$Update$refresh(
								_Utils_update(
									model,
									{
										error: true,
										notice: message + ' 입력은 보존했습니다. 최신 저장 내용과 비교한 뒤 다시 적용하세요.',
										session: $author$project$App$Session$finishSave(model.session)
									}));
						}
					}
				case 'GotAgents':
					var token = msg.a;
					var org = msg.b;
					var response = msg.c;
					return ((!_Utils_eq(token, model.session.request)) || (!_Utils_eq(
						$elm$core$Maybe$Just(org),
						model.session.org))) ? _Utils_Tuple2(model, _List_Nil) : _Utils_Tuple2(
						_Utils_update(
							model,
							{
								agents: A3($author$project$App$Agents$receive, org, response, model.agents)
							}),
						_List_Nil);
				case 'EditAgents':
					var change = msg.a;
					return ($author$project$App$Update$busy(model) || (model.agents.loading || A2(
						$elm$core$Maybe$withDefault,
						false,
						A2(
							$elm$core$Maybe$map,
							function (org) {
								return A2($elm$core$Dict$member, org, model.agents.errors);
							},
							model.session.org)))) ? _Utils_Tuple2(model, _List_Nil) : _Utils_Tuple2(
						_Utils_update(
							model,
							{
								agents: A2(
									$elm$core$Maybe$withDefault,
									model.agents,
									A2(
										$elm$core$Maybe$map,
										function (org) {
											return A3($author$project$App$Agents$edit, org, change, model.agents);
										},
										model.session.org))
							}),
						_List_Nil);
				case 'ImportAgentDrafts':
					var _v2 = A2(
						$elm$core$Maybe$andThen,
						function (org) {
							return A2($author$project$App$Agents$saved, org, model.agents);
						},
						model.session.org);
					if (_v2.$ === 'Just') {
						var snapshot = _v2.a;
						var $temp$msg = $author$project$App$Update$EditAgents(
							$author$project$Domain$Agent$Import(snapshot.drafts)),
							$temp$model = _Utils_update(
							model,
							{error: false, notice: '규칙 기반 초안을 설계안으로 가져왔습니다. 등급, 승인 주체, 인계 대상을 검토한 뒤 저장하세요.'});
						msg = $temp$msg;
						model = $temp$model;
						continue update;
					} else {
						return _Utils_Tuple2(model, _List_Nil);
					}
				case 'ResetAgents':
					return $author$project$App$Update$busy(model) ? _Utils_Tuple2(model, _List_Nil) : _Utils_Tuple2(
						_Utils_update(
							model,
							{
								agents: A2(
									$elm$core$Maybe$withDefault,
									model.agents,
									A2(
										$elm$core$Maybe$map,
										function (org) {
											return A2($author$project$App$Agents$clearDraft, org, model.agents);
										},
										model.session.org))
							}),
						_List_Nil);
				case 'RebaseAgents':
					return $author$project$App$Update$busy(model) ? _Utils_Tuple2(model, _List_Nil) : _Utils_Tuple2(
						_Utils_update(
							model,
							{
								agents: A2(
									$elm$core$Maybe$withDefault,
									model.agents,
									A2(
										$elm$core$Maybe$map,
										function (org) {
											return A2($author$project$App$Agents$rebase, org, model.agents);
										},
										model.session.org)),
								notice: '최신 버전에 설계안 입력을 다시 적용했습니다. 내용을 검토한 뒤 저장하세요.'
							}),
						_List_Nil);
				case 'SubmitAgents':
					return $author$project$App$Update$submitAgents(model);
				case 'SavedAgents':
					var token = msg.a;
					var org = msg.b;
					var response = msg.c;
					if ((!_Utils_eq(token, model.session.request)) || (!_Utils_eq(
						$elm$core$Maybe$Just(org),
						model.session.org))) {
						return _Utils_Tuple2(model, _List_Nil);
					} else {
						if (response.$ === 'Ok') {
							return $author$project$App$Update$refresh(
								_Utils_update(
									model,
									{
										agents: A2($author$project$App$Agents$clearDraft, org, model.agents),
										error: false,
										notice: '에이전트 설계안을 저장했습니다. 저장된 설계의 진단과 구조 화면을 확인하세요.',
										session: $author$project$App$Session$finishSave(model.session)
									}));
						} else {
							var message = response.a;
							return $author$project$App$Update$refresh(
								_Utils_update(
									model,
									{
										error: true,
										notice: message + ' 설계안 입력은 보존했습니다. 최신 저장 내용과 비교한 뒤 다시 적용하세요.',
										session: $author$project$App$Session$finishSave(model.session)
									}));
						}
					}
				case 'ActivityChange':
					var state = msg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								pageState: A2($author$project$App$PageState$setActivity, state, model.pageState)
							}),
						_List_Nil);
				case 'OpenReviewActivity':
					var review = msg.a;
					return $author$project$App$Update$busy(model) ? _Utils_Tuple2(model, _List_Nil) : A2(
						$elm$core$Tuple$mapFirst,
						function (next) {
							return _Utils_update(
								next,
								{
									pageState: A2(
										$author$project$App$PageState$setActivity,
										{
											from: '',
											kind: '',
											query: '',
											review: $elm$core$Maybe$Just(review),
											until: ''
										},
										next.pageState)
								});
						},
						A2(
							$author$project$App$Update$update,
							A2($author$project$App$Update$Guide, $author$project$Page$ActivityLog, 'audit-history'),
							model));
				case 'GraphGo':
					var target = msg.a;
					if ($author$project$App$Update$busy(model)) {
						return _Utils_Tuple2(model, _List_Nil);
					} else {
						if (A2($elm$core$String$startsWith, 'person:', target)) {
							var $temp$msg = $author$project$App$Update$OpenPerson(
								A2($elm$core$String$dropLeft, 7, target)),
								$temp$model = _Utils_update(
								model,
								{
									pageState: A2($author$project$App$PageState$setPage, $author$project$Page$People, model.pageState)
								});
							msg = $temp$msg;
							model = $temp$model;
							continue update;
						} else {
							if (A2($elm$core$String$startsWith, 'authority-', target)) {
								var $temp$msg = A2($author$project$App$Update$Guide, $author$project$Page$Authorities, target),
									$temp$model = model;
								msg = $temp$msg;
								model = $temp$model;
								continue update;
							} else {
								var $temp$msg = A2($author$project$App$Update$Guide, $author$project$Page$Responsibility, target),
									$temp$model = model;
								msg = $temp$msg;
								model = $temp$model;
								continue update;
							}
						}
					}
				case 'GraphMsg':
					var graphMsg = msg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								pageState: A2($author$project$App$PageState$updateGraph, graphMsg, model.pageState)
							}),
						_List_Nil);
				case 'SetListMode':
					var page = msg.a;
					var mode = msg.b;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								pageState: A3($author$project$App$PageState$setListMode, page, mode, model.pageState)
							}),
						_List_Nil);
				case 'Navigate':
					var page = msg.a;
					var org = msg.b;
					return $author$project$App$Update$busy(model) ? _Utils_Tuple2(model, _List_Nil) : ((_Utils_eq(org, model.session.org) && (!_Utils_eq(org, $elm$core$Maybe$Nothing))) ? _Utils_Tuple2(
						_Utils_update(
							model,
							{
								forms: $author$project$App$Drafts$closeDelete(model.forms),
								pageState: A2($author$project$App$PageState$setPage, page, model.pageState)
							}),
						_List_Nil) : $author$project$App$Update$refresh(
						_Utils_update(
							model,
							{
								error: false,
								forms: $author$project$App$Drafts$closeDelete(model.forms),
								notice: '',
								pageState: A2($author$project$App$PageState$navigate, page, model.pageState),
								session: A2($author$project$App$Session$selectOrganization, org, model.session)
							})));
				case 'Refresh':
					return $author$project$App$Update$busy(model) ? _Utils_Tuple2(model, _List_Nil) : $author$project$App$Update$refresh(model);
				case 'GotOrganizations':
					var token = msg.a;
					var response = msg.b;
					return A4(
						$author$project$App$Update$receive,
						token,
						response,
						A2($author$project$App$Session$receiveOrganizations, response, model.session),
						model);
				case 'GotWorkspace':
					var token = msg.a;
					var response = msg.b;
					return A4(
						$author$project$App$Update$receive,
						token,
						response,
						A2($author$project$App$Session$receiveWorkspace, response, model.session),
						model);
				case 'Edit':
					var action = msg.a;
					var key = msg.b;
					var val = msg.c;
					if ($author$project$App$Update$busy(model)) {
						return _Utils_Tuple2(model, _List_Nil);
					} else {
						switch (action.$) {
							case 'AddGoal':
								return A2(
									$elm$core$Maybe$withDefault,
									_Utils_Tuple2(model, _List_Nil),
									A2(
										$elm$core$Maybe$map,
										function (field) {
											return A2(
												$author$project$App$Update$update,
												A2($author$project$App$Update$EditGoal, field, val),
												model);
										},
										$author$project$Form$Goal$fromKey(key)));
							case 'AddReview':
								return A2(
									$elm$core$Maybe$withDefault,
									_Utils_Tuple2(model, _List_Nil),
									A2(
										$elm$core$Maybe$map,
										function (field) {
											return A2(
												$author$project$App$Update$update,
												A2($author$project$App$Update$EditReview, field, val),
												model);
										},
										$author$project$Form$Review$fromKey(key)));
							default:
								return _Utils_Tuple2(
									_Utils_update(
										model,
										{
											forms: A4($author$project$App$Drafts$edit, action, key, val, model)
										}),
									_List_Nil);
						}
					}
				case 'EditGoal':
					var field = msg.a;
					var val = msg.b;
					if ($author$project$App$Update$busy(model)) {
						return _Utils_Tuple2(model, _List_Nil);
					} else {
						var metric = function () {
							var _v8 = model.session.workspace;
							if (_v8.$ === 'Loaded') {
								var workspace = _v8.a;
								return A2(
									$elm$core$Dict$get,
									val,
									$elm$core$Dict$fromList(
										A2(
											$elm$core$List$map,
											function (metric_) {
												return _Utils_Tuple2(metric_.id, metric_);
											},
											A2(
												$elm$core$List$map,
												A2(
													$elm$core$Basics$composeR,
													function ($) {
														return $.goal;
													},
													function ($) {
														return $.metric;
													}),
												workspace.goals))));
							} else {
								return $elm$core$Maybe$Nothing;
							}
						}();
						var next = function () {
							if (_Utils_eq(field, $author$project$Form$Goal$MetricId)) {
								if (metric.$ === 'Just') {
									var selected = metric.a;
									return A3(
										$elm$core$List$foldl,
										F2(
											function (_v6, current) {
												var key = _v6.a;
												var content = _v6.b;
												return _Utils_update(
													current,
													{
														forms: A3($author$project$App$Drafts$editGoal, key, content, current)
													});
											}),
										model,
										_List_fromArray(
											[
												_Utils_Tuple2($author$project$Form$Goal$MetricId, selected.id),
												_Utils_Tuple2($author$project$Form$Goal$MetricName, selected.name),
												_Utils_Tuple2($author$project$Form$Goal$Unit, selected.unit),
												_Utils_Tuple2($author$project$Form$Goal$Direction, selected.direction)
											]));
								} else {
									return A3(
										$elm$core$List$foldl,
										F2(
											function (_v7, current) {
												var key = _v7.a;
												var content = _v7.b;
												return _Utils_update(
													current,
													{
														forms: A3($author$project$App$Drafts$editGoal, key, content, current)
													});
											}),
										_Utils_update(
											model,
											{
												forms: $author$project$App$Drafts$advanceSerial(model.forms)
											}),
										_List_fromArray(
											[
												_Utils_Tuple2(
												$author$project$Form$Goal$MetricId,
												'metric-' + (model.flags.seed + ('-new-' + $elm$core$String$fromInt(model.forms.serial)))),
												_Utils_Tuple2($author$project$Form$Goal$MetricName, ''),
												_Utils_Tuple2($author$project$Form$Goal$Unit, ''),
												_Utils_Tuple2($author$project$Form$Goal$Direction, 'HigherIsBetter')
											]));
								}
							} else {
								return _Utils_update(
									model,
									{
										forms: A3($author$project$App$Drafts$editGoal, field, val, model)
									});
							}
						}();
						return _Utils_Tuple2(next, _List_Nil);
					}
				case 'EditReview':
					var field = msg.a;
					var val = msg.b;
					return $author$project$App$Update$busy(model) ? _Utils_Tuple2(model, _List_Nil) : _Utils_Tuple2(
						_Utils_update(
							model,
							{
								forms: A3($author$project$App$Drafts$editReview, field, val, model)
							}),
						_List_Nil);
				case 'Submit':
					var action = msg.a;
					return A2($author$project$App$Update$submit, action, model);
				case 'Saved':
					var token = msg.a;
					var action = msg.b;
					var response = msg.c;
					return (!_Utils_eq(token, model.session.request)) ? _Utils_Tuple2(model, _List_Nil) : A3($author$project$App$Update$saved, action, response, model);
				case 'OpenDelete':
					var _v9 = model.session.workspace;
					if (_v9.$ === 'Loaded') {
						var workspace = _v9.a;
						return (model.session.fresh && (!$author$project$App$Update$busy(model))) ? _Utils_Tuple2(
							_Utils_update(
								model,
								{
									forms: A2(
										$author$project$App$Drafts$openDelete,
										{confirmation: '', id: workspace.organization.id, name: workspace.organization.name, version: workspace.version},
										model.forms)
								}),
							_List_fromArray(
								[
									$author$project$App$Effect$FocusElement('delete-confirm')
								])) : _Utils_Tuple2(
							_Utils_update(
								model,
								{error: true, notice: '최신 조직 정보를 불러온 뒤 다시 확인하세요.'}),
							_List_Nil);
					} else {
						return _Utils_Tuple2(model, _List_Nil);
					}
				case 'ConfirmDelete':
					var name = msg.a;
					return $author$project$App$Update$busy(model) ? _Utils_Tuple2(model, _List_Nil) : _Utils_Tuple2(
						_Utils_update(
							model,
							{
								forms: A2($author$project$App$Drafts$confirmDelete, name, model.forms)
							}),
						_List_Nil);
				case 'CloseDelete':
					return $author$project$App$Update$busy(model) ? _Utils_Tuple2(model, _List_Nil) : _Utils_Tuple2(
						_Utils_update(
							model,
							{
								forms: $author$project$App$Drafts$closeDelete(model.forms)
							}),
						_List_Nil);
				case 'ToggleGuide':
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								pageState: $author$project$App$PageState$toggleGuide(model.pageState)
							}),
						_List_Nil);
				case 'Guide':
					var page = msg.a;
					var target = msg.b;
					return $author$project$App$Update$busy(model) ? _Utils_Tuple2(model, _List_Nil) : _Utils_Tuple2(
						_Utils_update(
							model,
							{
								forms: (_Utils_eq(page, $author$project$Page$Reviews) && (target === 'review-form')) ? $author$project$App$Drafts$prepareReview(model) : model.forms,
								pageState: A3($author$project$App$PageState$guide, page, target, model.pageState)
							}),
						_List_fromArray(
							[
								$author$project$App$Effect$FocusElement(target)
							]));
				case 'SearchPeople':
					var query = msg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								pageState: A2($author$project$App$PageState$searchPeople, query, model.pageState)
							}),
						_List_Nil);
				case 'FilterPeople':
					var status = msg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								pageState: A2($author$project$App$PageState$filterPeople, status, model.pageState)
							}),
						_List_Nil);
				case 'ResetPerson':
					var key = msg.a;
					return $author$project$App$Update$busy(model) ? _Utils_Tuple2(model, _List_Nil) : $author$project$App$Update$refresh(
						_Utils_update(
							model,
							{
								error: false,
								forms: A2($author$project$App$Drafts$resetPerson, key, model),
								notice: '구성원 수정·인계 입력을 초기화하고 최신 정보를 불러옵니다. 확인한 뒤 다시 작성하세요.'
							}));
				case 'OpenPerson':
					var key = msg.a;
					return $author$project$App$Update$busy(model) ? _Utils_Tuple2(model, _List_Nil) : _Utils_Tuple2(
						_Utils_update(
							model,
							{
								pageState: A2($author$project$App$PageState$openPerson, key, model.pageState)
							}),
						_List_fromArray(
							[
								$author$project$App$Effect$FocusElement('person-detail')
							]));
				default:
					return _Utils_Tuple2(model, _List_Nil);
			}
		}
	});
var $author$project$ListViewTest$step = F2(
	function (msg, model) {
		return A2($author$project$App$Update$update, msg, model).a;
	});
var $author$project$App$Update$ActivityChange = function (a) {
	return {$: 'ActivityChange', a: a};
};
var $author$project$Page$Dashboard = {$: 'Dashboard'};
var $author$project$App$Update$Navigate = F2(
	function (a, b) {
		return {$: 'Navigate', a: a, b: b};
	});
var $author$project$App$Update$OpenReviewActivity = function (a) {
	return {$: 'OpenReviewActivity', a: a};
};
var $author$project$Ui$ListView$Table = {$: 'Table'};
var $author$project$Ui$Label$personName = F2(
	function (w, key) {
		return A2(
			$elm$core$Maybe$withDefault,
			key,
			A2(
				$elm$core$Maybe$map,
				function (p) {
					return _Utils_ap(
						p.name,
						p.active ? '' : ' (비활성)');
				},
				$elm$core$List$head(
					A2(
						$elm$core$List$filter,
						A2(
							$elm$core$Basics$composeR,
							function ($) {
								return $.id;
							},
							$elm$core$Basics$eq(key)),
						w.people))));
	});
var $author$project$Ui$Activity$actorName = F2(
	function (w, event) {
		return A2(
			$elm$core$Maybe$withDefault,
			'로컬 운영자 (미인증)',
			A2(
				$elm$core$Maybe$map,
				function (ident) {
					return A2($author$project$Ui$Label$personName, w, ident) + ' (미인증)';
				},
				event.actor));
	});
var $author$project$Ui$Label$goalName = F2(
	function (w, key) {
		return A2(
			$elm$core$Maybe$withDefault,
			key,
			A2(
				$elm$core$Maybe$map,
				A2(
					$elm$core$Basics$composeR,
					function ($) {
						return $.goal;
					},
					function ($) {
						return $.description;
					}),
				$elm$core$List$head(
					A2(
						$elm$core$List$filter,
						A2(
							$elm$core$Basics$composeR,
							function ($) {
								return $.goal;
							},
							A2(
								$elm$core$Basics$composeR,
								function ($) {
									return $.id;
								},
								$elm$core$Basics$eq(key))),
						w.goals))));
	});
var $author$project$Ui$Activity$targetName = F2(
	function (w, event) {
		var _v0 = event.activity.targetKind;
		switch (_v0) {
			case 'person':
				return A2($author$project$Ui$Label$personName, w, event.activity.targetId);
			case 'goal':
				return A2($author$project$Ui$Label$goalName, w, event.activity.targetId);
			case 'organization':
				return _Utils_eq(event.activity.targetId, w.organization.id) ? w.organization.name : event.activity.targetId;
			case 'survey':
				return '조직 진단';
			case 'agents':
				return '에이전트 설계';
			default:
				return '—';
		}
	});
var $author$project$Ui$Activity$description = F2(
	function (w, event) {
		var target = A2($author$project$Ui$Activity$targetName, w, event);
		var withDetail = function (label) {
			return target + (' · ' + (label + ((event.activity.detail === '') ? '' : (' · ' + event.activity.detail))));
		};
		var person = A2(
			$elm$core$Maybe$withDefault,
			'',
			A2(
				$elm$core$Maybe$map,
				$author$project$Ui$Label$personName(w),
				event.activity.personId));
		var _v0 = event.activity.tag;
		switch (_v0) {
			case 'OwnerAssigned':
				return target + ('의 책임자를 ' + (person + '으로 지정'));
			case 'OrganizationCreated':
				return withDetail('조직 생성');
			case 'OrganizationRenamed':
				return withDetail('조직 이름 변경');
			case 'OrganizationDeleted':
				return withDetail('조직 삭제');
			case 'DemoSeeded':
				return withDetail('체험 데이터 생성');
			case 'PersonAdded':
				return withDetail('구성원 추가');
			case 'EmployeeAdded':
				return withDetail('구성원 추가');
			case 'PersonUpdated':
				return withDetail('구성원 정보 수정');
			case 'PersonDeactivated':
				return _Utils_ap(
					withDetail('구성원 비활성화'),
					(person === '') ? '' : (' · 후임 ' + person));
			case 'GoalCreated':
				return withDetail('목표 생성');
			case 'GoalActivated':
				return withDetail('목표 활성화');
			case 'AuthorityGranted':
				return withDetail('권한 부여');
			case 'AuthorityRevoked':
				return withDetail('권한 회수');
			case 'ResultReported':
				return _Utils_ap(
					withDetail('결과 보고'),
					(person === '') ? '' : (' · 보고자 ' + person));
			case 'GoalEvaluated':
				return withDetail('목표 평가');
			case 'ReviewHeld':
				return withDetail('회고 기록');
			case 'StrategyChanged':
				return withDetail('전략 변경');
			default:
				return event.description;
		}
	});
var $elm_explorations$test$Test$Runner$Failure$Equality = F2(
	function (a, b) {
		return {$: 'Equality', a: a, b: b};
	});
var $elm$core$String$contains = _String_contains;
var $elm_explorations$test$Test$Expectation$Pass = function (a) {
	return {$: 'Pass', a: a};
};
var $elm_explorations$test$Expect$pass = $elm_explorations$test$Test$Expectation$Pass(
	{distributionReport: $elm_explorations$test$Test$Distribution$NoDistribution});
var $elm_explorations$test$Test$Internal$toString = _Debug_toString;
var $elm_explorations$test$Expect$testWith = F5(
	function (makeReason, label, runTest, expected, actual) {
		return A2(runTest, actual, expected) ? $elm_explorations$test$Expect$pass : $elm_explorations$test$Test$Expectation$fail(
			{
				description: label,
				reason: A2(
					makeReason,
					$elm_explorations$test$Test$Internal$toString(expected),
					$elm_explorations$test$Test$Internal$toString(actual))
			});
	});
var $elm_explorations$test$Expect$equateWith = F4(
	function (reason, comparison, b, a) {
		var isJust = function (x) {
			if (x.$ === 'Just') {
				return true;
			} else {
				return false;
			}
		};
		var isFloat = function (x) {
			return isJust(
				$elm$core$String$toFloat(x)) && (!isJust(
				$elm$core$String$toInt(x)));
		};
		var usesFloats = isFloat(
			$elm_explorations$test$Test$Internal$toString(a)) || isFloat(
			$elm_explorations$test$Test$Internal$toString(b));
		var floatError = A2($elm$core$String$contains, reason, 'not') ? 'Do not use Expect.notEqual with floats. Use Expect.notWithin instead.' : 'Do not use Expect.equal with floats. Use Expect.within instead.';
		return usesFloats ? $elm_explorations$test$Expect$fail(floatError) : A5($elm_explorations$test$Expect$testWith, $elm_explorations$test$Test$Runner$Failure$Equality, reason, comparison, b, a);
	});
var $elm_explorations$test$Expect$equal = A2($elm_explorations$test$Expect$equateWith, 'Expect.equal', $elm$core$Basics$eq);
var $author$project$ActivityTest$event = {
	activity: {
		detail: '',
		personId: $elm$core$Maybe$Just('p'),
		raw: '{\"tag\":\"OwnerAssigned\"}',
		reviewId: $elm$core$Maybe$Nothing,
		tag: 'OwnerAssigned',
		targetId: 'g',
		targetKind: 'goal'
	},
	actor: $elm$core$Maybe$Just('p'),
	at: '2026-09-07T23:59:59.123456Z',
	description: 'raw legacy g p',
	evaluatedGoal: $elm$core$Maybe$Nothing,
	evaluatedStatus: $elm$core$Maybe$Nothing,
	seq: 4
};
var $author$project$Ui$Activity$category = function (event) {
	var _v0 = event.activity.tag;
	switch (_v0) {
		case 'OwnerAssigned':
			return '책임';
		case 'AuthorityGranted':
			return '권한';
		case 'AuthorityRevoked':
			return '권한';
		case 'ResultReported':
			return '결과';
		case 'GoalEvaluated':
			return '결과';
		case 'ReviewHeld':
			return '학습';
		case 'StrategyChanged':
			return '학습';
		case 'DiscoverySaved':
			return '진단';
		case 'AgentRolesSaved':
			return '에이전트';
		default:
			var _v1 = event.activity.targetKind;
			switch (_v1) {
				case 'person':
					return '구성원';
				case 'goal':
					return '목표';
				case 'organization':
					return '조직';
				default:
					return '기타';
			}
	}
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toLower = _String_toLower;
var $author$project$Ui$Activity$filtered = F2(
	function (state, w) {
		return A2(
			$elm$core$List$sortBy,
			A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.seq;
				},
				$elm$core$Basics$negate),
			A2(
				$elm$core$List$filter,
				function (event) {
					return ((state.kind === '') || _Utils_eq(
						$author$project$Ui$Activity$category(event),
						state.kind)) && (((state.from === '') || (_Utils_cmp(
						A2($elm$core$String$left, 10, event.at),
						state.from) > -1)) && (((state.until === '') || (_Utils_cmp(
						A2($elm$core$String$left, 10, event.at),
						state.until) < 1)) && ((_Utils_eq(state.review, $elm$core$Maybe$Nothing) || _Utils_eq(event.activity.reviewId, state.review)) && A2(
						$elm$core$String$contains,
						$elm$core$String$toLower(
							$elm$core$String$trim(state.query)),
						$elm$core$String$toLower(
							A2(
								$elm$core$String$join,
								' ',
								_List_fromArray(
									[
										A2($author$project$Ui$Activity$targetName, w, event),
										A2($author$project$Ui$Activity$actorName, w, event),
										A2($author$project$Ui$Activity$description, w, event),
										event.activity.targetId,
										A2($elm$core$Maybe$withDefault, '', event.activity.personId),
										A2($elm$core$Maybe$withDefault, '', event.activity.reviewId),
										A2($elm$core$Maybe$withDefault, '', event.actor),
										$author$project$Ui$Activity$category(event)
									])))))));
				},
				w.events));
	});
var $elm_explorations$test$Test$Html$Query$Internal$InternalError = function (a) {
	return {$: 'InternalError', a: a};
};
var $elm_explorations$test$Test$Html$Query$Internal$Query = F2(
	function (a, b) {
		return {$: 'Query', a: a, b: b};
	});
var $elm_explorations$test$Test$Html$Query$Internal$Single = F2(
	function (a, b) {
		return {$: 'Single', a: a, b: b};
	});
var $elm_explorations$test$Test$Html$Query$Internal$ValidationErrors = function (a) {
	return {$: 'ValidationErrors', a: a};
};
var $elm_explorations$test$Test$Html$Internal$Inert$DecodeError = function (a) {
	return {$: 'DecodeError', a: a};
};
var $elm_explorations$test$Test$Html$Internal$Inert$Node = function (a) {
	return {$: 'Node', a: a};
};
var $elm_explorations$test$Test$Html$Internal$Inert$ValidationErrors = function (a) {
	return {$: 'ValidationErrors', a: a};
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$HtmlContext = F2(
	function (a, b) {
		return {$: 'HtmlContext', a: a, b: b};
	});
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$NodeEntry = function (a) {
	return {$: 'NodeEntry', a: a};
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$NodeRecord = F4(
	function (tag, children, facts, descendantsCount) {
		return {children: children, descendantsCount: descendantsCount, facts: facts, tag: tag};
	});
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$TextTag = function (a) {
	return {$: 'TextTag', a: a};
};
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$CustomNode = function (a) {
	return {$: 'CustomNode', a: a};
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$MarkdownNode = function (a) {
	return {$: 'MarkdownNode', a: a};
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$CustomNodeRecord = F2(
	function (facts, model) {
		return {facts: facts, model: model};
	});
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$ClassVsClassNameValidation = {$: 'ClassVsClassNameValidation'};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$Facts = F5(
	function (styles, events, attributeNamespace, stringAttributes, boolAttributes) {
		return {attributeNamespace: attributeNamespace, boolAttributes: boolAttributes, events: events, stringAttributes: stringAttributes, styles: styles};
	});
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Constants$attributeNamespaceKey = 'a4';
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $elm$json$Json$Decode$keyValuePairs = _Json_decodeKeyValuePairs;
var $elm$json$Json$Decode$dict = function (decoder) {
	return A2(
		$elm$json$Json$Decode$map,
		$elm$core$Dict$fromList,
		$elm$json$Json$Decode$keyValuePairs(decoder));
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Constants$eventKey = 'a0';
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeEvents = function (taggedEventDecoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$json$Json$Decode$field,
				$elm_explorations$test$Test$Html$Internal$ElmHtml$Constants$eventKey,
				$elm$json$Json$Decode$dict(
					A2($elm$json$Json$Decode$map, taggedEventDecoder, $elm$json$Json$Decode$value))),
				$elm$json$Json$Decode$succeed($elm$core$Dict$empty)
			]));
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$classVsClassNameValidationMessage = 'Found the `class` attribute and the `className` property used in the same HTML node. This would result in unspecified behaviour, and elm-test wouldn\'t be able to reliably query for classnames. Please only use one of the two.';
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Constants$attributeKey = 'a3';
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeDictFilterMap = function (decoder) {
	return A2(
		$elm$json$Json$Decode$map,
		A2(
			$elm$core$Basics$composeR,
			$elm$core$Dict$toList,
			A2(
				$elm$core$Basics$composeR,
				$elm$core$List$filterMap(
					function (_v0) {
						var key = _v0.a;
						var value = _v0.b;
						var _v1 = A2($elm$json$Json$Decode$decodeValue, decoder, value);
						if (_v1.$ === 'Err') {
							return $elm$core$Maybe$Nothing;
						} else {
							var v = _v1.a;
							return $elm$core$Maybe$Just(
								_Utils_Tuple2(key, v));
						}
					}),
				$elm$core$Dict$fromList)),
		$elm$json$Json$Decode$dict($elm$json$Json$Decode$value));
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeAttributes = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$json$Json$Decode$field,
				$elm_explorations$test$Test$Html$Internal$ElmHtml$Constants$attributeKey,
				$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeDictFilterMap(decoder)),
				$elm$json$Json$Decode$succeed($elm$core$Dict$empty)
			]));
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Constants$styleKey = 'a1';
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Constants$knownKeys = _List_fromArray(
	[$elm_explorations$test$Test$Html$Internal$ElmHtml$Constants$styleKey, $elm_explorations$test$Test$Html$Internal$ElmHtml$Constants$eventKey, $elm_explorations$test$Test$Html$Internal$ElmHtml$Constants$attributeKey, $elm_explorations$test$Test$Html$Internal$ElmHtml$Constants$attributeNamespaceKey]);
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Helpers$filterKnownKeys = $elm$core$Dict$filter(
	F2(
		function (key, _v0) {
			return !A2($elm$core$List$member, key, $elm_explorations$test$Test$Html$Internal$ElmHtml$Constants$knownKeys);
		}));
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeOthers = F2(
	function (otherDecoder, validation) {
		return A2(
			$elm$json$Json$Decode$andThen,
			function (attributes) {
				return function () {
					if (validation.$ === 'Nothing') {
						return $elm$core$Basics$identity;
					} else {
						var _v1 = validation.a;
						return $elm$json$Json$Decode$andThen(
							function (dict) {
								return (A2($elm$core$Dict$member, 'class', dict) && A2($elm$core$Dict$member, 'className', dict)) ? $elm$json$Json$Decode$fail($elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$classVsClassNameValidationMessage) : $elm$json$Json$Decode$succeed(dict);
							});
					}
				}()(
					A2(
						$elm$json$Json$Decode$map,
						A2(
							$elm$core$Basics$composeR,
							$elm_explorations$test$Test$Html$Internal$ElmHtml$Helpers$filterKnownKeys,
							$elm$core$Dict$union(attributes)),
						$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeDictFilterMap(otherDecoder)));
			},
			$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeAttributes(otherDecoder));
	});
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeStyles = $elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$json$Json$Decode$field,
			$elm_explorations$test$Test$Html$Internal$ElmHtml$Constants$styleKey,
			$elm$json$Json$Decode$dict($elm$json$Json$Decode$string)),
			$elm$json$Json$Decode$succeed($elm$core$Dict$empty)
		]));
var $elm$json$Json$Decode$map5 = _Json_map5;
var $elm$json$Json$Decode$maybe = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder),
				$elm$json$Json$Decode$succeed($elm$core$Maybe$Nothing)
			]));
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeFacts = function (_v0) {
	var taggers = _v0.a;
	var eventDecoder = _v0.b;
	return A6(
		$elm$json$Json$Decode$map5,
		$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$Facts,
		$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeStyles,
		$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeEvents(
			eventDecoder(taggers)),
		$elm$json$Json$Decode$maybe(
			A2($elm$json$Json$Decode$field, $elm_explorations$test$Test$Html$Internal$ElmHtml$Constants$attributeNamespaceKey, $elm$json$Json$Decode$value)),
		A2(
			$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeOthers,
			$elm$json$Json$Decode$string,
			$elm$core$Maybe$Just($elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$ClassVsClassNameValidation)),
		A2($elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeOthers, $elm$json$Json$Decode$bool, $elm$core$Maybe$Nothing));
};
var $elm_explorations$test$Test$Internal$KernelConstants$kernelConstants = {
	markdown: {markdown: 'b', options: 'a'},
	virtualDom: {descendantsCount: 'b', facts: 'd', kids: 'e', model: 'g', node: 'k', nodeType: '$', nodeTypeCustom: 3, nodeTypeKeyedNode: 2, nodeTypeNode: 1, nodeTypeTagger: 4, nodeTypeText: 0, nodeTypeThunk: 5, refs: 'l', tag: 'c', tagger: 'j', text: 'a'}
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeCustomNodeRecord = function (context) {
	return A3(
		$elm$json$Json$Decode$map2,
		$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$CustomNodeRecord,
		A2(
			$elm$json$Json$Decode$field,
			$elm_explorations$test$Test$Internal$KernelConstants$kernelConstants.virtualDom.facts,
			$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeFacts(context)),
		A2($elm$json$Json$Decode$field, $elm_explorations$test$Test$Internal$KernelConstants$kernelConstants.virtualDom.model, $elm$json$Json$Decode$value));
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$MarkdownNodeRecord = F2(
	function (facts, model) {
		return {facts: facts, model: model};
	});
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Markdown$MarkdownModel = F2(
	function (options, markdown) {
		return {markdown: markdown, options: options};
	});
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Markdown$baseMarkdownModel = {
	markdown: '',
	options: {
		defaultHighlighting: $elm$core$Maybe$Nothing,
		githubFlavored: $elm$core$Maybe$Just(
			{breaks: false, tables: false}),
		sanitize: false,
		smartypants: false
	}
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Markdown$decodeMarkdownModel = A2(
	$elm$json$Json$Decode$map,
	$elm_explorations$test$Test$Html$Internal$ElmHtml$Markdown$MarkdownModel($elm_explorations$test$Test$Html$Internal$ElmHtml$Markdown$baseMarkdownModel.options),
	A2($elm$json$Json$Decode$field, $elm_explorations$test$Test$Internal$KernelConstants$kernelConstants.markdown.markdown, $elm$json$Json$Decode$string));
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeMarkdownNodeRecord = function (context) {
	return A3(
		$elm$json$Json$Decode$map2,
		$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$MarkdownNodeRecord,
		A2(
			$elm$json$Json$Decode$field,
			$elm_explorations$test$Test$Internal$KernelConstants$kernelConstants.virtualDom.facts,
			$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeFacts(context)),
		A2($elm$json$Json$Decode$field, $elm_explorations$test$Test$Internal$KernelConstants$kernelConstants.virtualDom.model, $elm_explorations$test$Test$Html$Internal$ElmHtml$Markdown$decodeMarkdownModel));
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeCustomNode = function (context) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$json$Json$Decode$map,
				$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$MarkdownNode,
				$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeMarkdownNodeRecord(context)),
				A2(
				$elm$json$Json$Decode$map,
				$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$CustomNode,
				$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeCustomNodeRecord(context))
			]));
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeTextTag = A2(
	$elm$json$Json$Decode$field,
	$elm_explorations$test$Test$Internal$KernelConstants$kernelConstants.virtualDom.text,
	A2(
		$elm$json$Json$Decode$andThen,
		function (text) {
			return $elm$json$Json$Decode$succeed(
				{text: text});
		},
		$elm$json$Json$Decode$string));
var $elm$json$Json$Decode$map4 = _Json_map4;
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$contextDecodeElmHtml = function (context) {
	return A2(
		$elm$json$Json$Decode$andThen,
		function (nodeType) {
			return _Utils_eq(nodeType, $elm_explorations$test$Test$Internal$KernelConstants$kernelConstants.virtualDom.nodeTypeText) ? A2($elm$json$Json$Decode$map, $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$TextTag, $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeTextTag) : (_Utils_eq(nodeType, $elm_explorations$test$Test$Internal$KernelConstants$kernelConstants.virtualDom.nodeTypeKeyedNode) ? A2(
				$elm$json$Json$Decode$map,
				$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$NodeEntry,
				$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeKeyedNode(context)) : (_Utils_eq(nodeType, $elm_explorations$test$Test$Internal$KernelConstants$kernelConstants.virtualDom.nodeTypeNode) ? A2(
				$elm$json$Json$Decode$map,
				$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$NodeEntry,
				$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeNode(context)) : (_Utils_eq(nodeType, $elm_explorations$test$Test$Internal$KernelConstants$kernelConstants.virtualDom.nodeTypeCustom) ? $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeCustomNode(context) : (_Utils_eq(nodeType, $elm_explorations$test$Test$Internal$KernelConstants$kernelConstants.virtualDom.nodeTypeTagger) ? $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeTagger(context) : (_Utils_eq(nodeType, $elm_explorations$test$Test$Internal$KernelConstants$kernelConstants.virtualDom.nodeTypeThunk) ? A2(
				$elm$json$Json$Decode$field,
				$elm_explorations$test$Test$Internal$KernelConstants$kernelConstants.virtualDom.node,
				$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$contextDecodeElmHtml(context)) : $elm$json$Json$Decode$fail(
				'No such type as ' + $elm$core$String$fromInt(nodeType)))))));
		},
		A2($elm$json$Json$Decode$field, $elm_explorations$test$Test$Internal$KernelConstants$kernelConstants.virtualDom.nodeType, $elm$json$Json$Decode$int));
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeKeyedNode = function (context) {
	var decodeSecondNode = A2(
		$elm$json$Json$Decode$field,
		'b',
		$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$contextDecodeElmHtml(context));
	return A5(
		$elm$json$Json$Decode$map4,
		$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$NodeRecord,
		A2($elm$json$Json$Decode$field, $elm_explorations$test$Test$Internal$KernelConstants$kernelConstants.virtualDom.tag, $elm$json$Json$Decode$string),
		A2(
			$elm$json$Json$Decode$field,
			$elm_explorations$test$Test$Internal$KernelConstants$kernelConstants.virtualDom.kids,
			$elm$json$Json$Decode$list(decodeSecondNode)),
		A2(
			$elm$json$Json$Decode$field,
			$elm_explorations$test$Test$Internal$KernelConstants$kernelConstants.virtualDom.facts,
			$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeFacts(context)),
		A2($elm$json$Json$Decode$field, $elm_explorations$test$Test$Internal$KernelConstants$kernelConstants.virtualDom.descendantsCount, $elm$json$Json$Decode$int));
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeNode = function (context) {
	return A5(
		$elm$json$Json$Decode$map4,
		$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$NodeRecord,
		A2($elm$json$Json$Decode$field, $elm_explorations$test$Test$Internal$KernelConstants$kernelConstants.virtualDom.tag, $elm$json$Json$Decode$string),
		A2(
			$elm$json$Json$Decode$field,
			$elm_explorations$test$Test$Internal$KernelConstants$kernelConstants.virtualDom.kids,
			$elm$json$Json$Decode$list(
				$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$contextDecodeElmHtml(context))),
		A2(
			$elm$json$Json$Decode$field,
			$elm_explorations$test$Test$Internal$KernelConstants$kernelConstants.virtualDom.facts,
			$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeFacts(context)),
		A2($elm$json$Json$Decode$field, $elm_explorations$test$Test$Internal$KernelConstants$kernelConstants.virtualDom.descendantsCount, $elm$json$Json$Decode$int));
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeTagger = function (_v0) {
	var taggers = _v0.a;
	var eventDecoder = _v0.b;
	return A2(
		$elm$json$Json$Decode$andThen,
		function (tagger) {
			var nodeDecoder = $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$contextDecodeElmHtml(
				A2(
					$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$HtmlContext,
					_Utils_ap(
						taggers,
						_List_fromArray(
							[tagger])),
					eventDecoder));
			return A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					[$elm_explorations$test$Test$Internal$KernelConstants$kernelConstants.virtualDom.node]),
				nodeDecoder);
		},
		A2($elm$json$Json$Decode$field, $elm_explorations$test$Test$Internal$KernelConstants$kernelConstants.virtualDom.tagger, $elm$json$Json$Decode$value));
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeElmHtml = function (eventDecoder) {
	return $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$contextDecodeElmHtml(
		A2($elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$HtmlContext, _List_Nil, eventDecoder));
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$validationFromMessage = function (message) {
	return _Utils_eq(message, $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$classVsClassNameValidationMessage) ? $elm$core$Maybe$Just($elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$ClassVsClassNameValidation) : $elm$core$Maybe$Nothing;
};
var $elm_explorations$test$Test$Html$Internal$Inert$findValidationErrors = function (error) {
	findValidationErrors:
	while (true) {
		switch (error.$) {
			case 'Field':
				var e = error.b;
				var $temp$error = e;
				error = $temp$error;
				continue findValidationErrors;
			case 'Index':
				var e = error.b;
				var $temp$error = e;
				error = $temp$error;
				continue findValidationErrors;
			case 'OneOf':
				var es = error.a;
				return A2($elm$core$List$concatMap, $elm_explorations$test$Test$Html$Internal$Inert$findValidationErrors, es);
			default:
				var stringError = error.a;
				var _v1 = $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$validationFromMessage(stringError);
				if (_v1.$ === 'Nothing') {
					return _List_Nil;
				} else {
					var validation = _v1.a;
					return _List_fromArray(
						[validation]);
				}
		}
	}
};
var $elm_explorations$test$Test$Html$Internal$Inert$eventDecoder = function (eventHandler) {
	return _HtmlAsJson_eventHandler(eventHandler);
};
var $elm$virtual_dom$VirtualDom$Custom = function (a) {
	return {$: 'Custom', a: a};
};
var $elm$virtual_dom$VirtualDom$MayPreventDefault = function (a) {
	return {$: 'MayPreventDefault', a: a};
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm_explorations$test$Test$Html$Internal$Inert$mapHandler = F2(
	function (f, handler) {
		switch (handler.$) {
			case 'Normal':
				var decoder = handler.a;
				return $elm$virtual_dom$VirtualDom$Normal(
					A2($elm$json$Json$Decode$map, f, decoder));
			case 'MayStopPropagation':
				var decoder = handler.a;
				return $elm$virtual_dom$VirtualDom$MayStopPropagation(
					A2(
						$elm$json$Json$Decode$map,
						$elm$core$Tuple$mapFirst(f),
						decoder));
			case 'MayPreventDefault':
				var decoder = handler.a;
				return $elm$virtual_dom$VirtualDom$MayPreventDefault(
					A2(
						$elm$json$Json$Decode$map,
						$elm$core$Tuple$mapFirst(f),
						decoder));
			default:
				var decoder = handler.a;
				return $elm$virtual_dom$VirtualDom$Custom(
					A2(
						$elm$json$Json$Decode$map,
						function (value) {
							return {
								message: f(value.message),
								preventDefault: value.preventDefault,
								stopPropagation: value.stopPropagation
							};
						},
						decoder));
		}
	});
var $elm_explorations$test$Test$Html$Internal$Inert$taggerFunction = function (tagger) {
	return _HtmlAsJson_taggerFunction(tagger);
};
var $elm_explorations$test$Test$Html$Internal$Inert$taggedEventDecoder = F2(
	function (taggers, eventHandler) {
		if (!taggers.b) {
			return $elm_explorations$test$Test$Html$Internal$Inert$eventDecoder(eventHandler);
		} else {
			if (!taggers.b.b) {
				var tagger = taggers.a;
				return A2(
					$elm_explorations$test$Test$Html$Internal$Inert$mapHandler,
					$elm_explorations$test$Test$Html$Internal$Inert$taggerFunction(tagger),
					$elm_explorations$test$Test$Html$Internal$Inert$eventDecoder(eventHandler));
			} else {
				var tagger = taggers.a;
				var rest = taggers.b;
				return A2(
					$elm_explorations$test$Test$Html$Internal$Inert$mapHandler,
					$elm_explorations$test$Test$Html$Internal$Inert$taggerFunction(tagger),
					A2($elm_explorations$test$Test$Html$Internal$Inert$taggedEventDecoder, rest, eventHandler));
			}
		}
	});
var $elm_explorations$test$Test$Html$Internal$Inert$toJson = function (node) {
	return _HtmlAsJson_toJson(node);
};
var $elm_explorations$test$MicroListExtra$uniqueHelp = F4(
	function (f, existing, remaining, accumulator) {
		uniqueHelp:
		while (true) {
			if (!remaining.b) {
				return $elm$core$List$reverse(accumulator);
			} else {
				var first = remaining.a;
				var rest = remaining.b;
				var computedFirst = f(first);
				if (A2($elm$core$List$member, computedFirst, existing)) {
					var $temp$f = f,
						$temp$existing = existing,
						$temp$remaining = rest,
						$temp$accumulator = accumulator;
					f = $temp$f;
					existing = $temp$existing;
					remaining = $temp$remaining;
					accumulator = $temp$accumulator;
					continue uniqueHelp;
				} else {
					var $temp$f = f,
						$temp$existing = A2($elm$core$List$cons, computedFirst, existing),
						$temp$remaining = rest,
						$temp$accumulator = A2($elm$core$List$cons, first, accumulator);
					f = $temp$f;
					existing = $temp$existing;
					remaining = $temp$remaining;
					accumulator = $temp$accumulator;
					continue uniqueHelp;
				}
			}
		}
	});
var $elm_explorations$test$MicroListExtra$unique = function (list) {
	return A4($elm_explorations$test$MicroListExtra$uniqueHelp, $elm$core$Basics$identity, _List_Nil, list, _List_Nil);
};
var $elm_explorations$test$Test$Html$Internal$Inert$fromHtml = function (html) {
	var _v0 = A2(
		$elm$json$Json$Decode$decodeValue,
		$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeElmHtml($elm_explorations$test$Test$Html$Internal$Inert$taggedEventDecoder),
		$elm_explorations$test$Test$Html$Internal$Inert$toJson(html));
	if (_v0.$ === 'Ok') {
		var elmHtml = _v0.a;
		return $elm$core$Result$Ok(
			$elm_explorations$test$Test$Html$Internal$Inert$Node(elmHtml));
	} else {
		var jsonError = _v0.a;
		var _v1 = $elm_explorations$test$Test$Html$Internal$Inert$findValidationErrors(jsonError);
		if (!_v1.b) {
			return $elm$core$Result$Err(
				$elm_explorations$test$Test$Html$Internal$Inert$DecodeError(jsonError));
		} else {
			var failedValidations = _v1;
			return $elm$core$Result$Err(
				$elm_explorations$test$Test$Html$Internal$Inert$ValidationErrors(
					{
						deduped: $elm_explorations$test$MicroListExtra$unique(failedValidations)
					}));
		}
	}
};
var $elm_explorations$test$Test$Html$Query$fromHtml = function (html) {
	return A2(
		$elm_explorations$test$Test$Html$Query$Internal$Single,
		true,
		function () {
			var _v0 = $elm_explorations$test$Test$Html$Internal$Inert$fromHtml(html);
			if (_v0.$ === 'Ok') {
				var node = _v0.a;
				return A2($elm_explorations$test$Test$Html$Query$Internal$Query, node, _List_Nil);
			} else {
				if (_v0.a.$ === 'DecodeError') {
					var decodeError = _v0.a.a;
					return $elm_explorations$test$Test$Html$Query$Internal$InternalError(
						$elm$json$Json$Decode$errorToString(decodeError));
				} else {
					var validations = _v0.a.a;
					return $elm_explorations$test$Test$Html$Query$Internal$ValidationErrors(validations);
				}
			}
		}());
};
var $elm_explorations$test$Test$Html$Query$Internal$baseIndentation = '    ';
var $elm_explorations$test$Test$Html$Query$Internal$prefixOutputLine = $elm$core$Basics$append('▼ ');
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$EscapableRawTextElements = {$: 'EscapableRawTextElements'};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$NormalElements = {$: 'NormalElements'};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$RawTextElements = {$: 'RawTextElements'};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$VoidElements = {$: 'VoidElements'};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$escapableRawTextElements = _List_fromArray(
	['textarea', 'title']);
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$rawTextElements = _List_fromArray(
	['script', 'style']);
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$voidElements = _List_fromArray(
	['area', 'base', 'br', 'col', 'embed', 'hr', 'img', 'input', 'link', 'meta', 'param', 'source', 'track', 'wbr']);
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$toElementKind = function (element) {
	return A2($elm$core$List$member, element, $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$voidElements) ? $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$VoidElements : (A2($elm$core$List$member, element, $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$rawTextElements) ? $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$RawTextElements : (A2($elm$core$List$member, element, $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$escapableRawTextElements) ? $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$EscapableRawTextElements : $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$NormalElements));
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$ToString$nodeRecordToString = F2(
	function (options, _v1) {
		var tag = _v1.tag;
		var children = _v1.children;
		var facts = _v1.facts;
		var styles = function () {
			var _v7 = $elm$core$Dict$toList(facts.styles);
			if (!_v7.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var styleValues = _v7;
				return $elm$core$Maybe$Just(
					function (styleString) {
						return 'style=\"' + (styleString + '\"');
					}(
						A2(
							$elm$core$String$join,
							'',
							A2(
								$elm$core$List$map,
								function (_v8) {
									var key = _v8.a;
									var value = _v8.b;
									return key + (':' + (value + ';'));
								},
								styleValues))));
			}
		}();
		var stringAttributes = $elm$core$Maybe$Just(
			A2(
				$elm$core$String$join,
				' ',
				A2(
					$elm$core$List$map,
					function (_v6) {
						var k = _v6.a;
						var v = _v6.b;
						return k + ('=\"' + (v + '\"'));
					},
					$elm$core$Dict$toList(
						A2(
							$elm$core$Dict$filter,
							F2(
								function (k, _v5) {
									return k !== 'className';
								}),
							facts.stringAttributes)))));
		var openTag = function (extras) {
			var trimmedExtras = A2(
				$elm$core$List$filter,
				$elm$core$Basics$neq(''),
				A2(
					$elm$core$List$map,
					$elm$core$String$trim,
					A2(
						$elm$core$List$filterMap,
						function (x) {
							return x;
						},
						extras)));
			var filling = function () {
				if (!trimmedExtras.b) {
					return '';
				} else {
					var more = trimmedExtras;
					return ' ' + A2($elm$core$String$join, ' ', more);
				}
			}();
			return '<' + (tag + (filling + '>'));
		};
		var closeTag = '</' + (tag + '>');
		var classes = A2(
			$elm$core$Maybe$map,
			function (name) {
				return 'class=\"' + (name + '\"');
			},
			A2($elm$core$Dict$get, 'className', facts.stringAttributes));
		var childrenStrings = A2(
			$elm$core$List$map,
			$elm$core$Basics$append(
				A2($elm$core$String$repeat, options.indent, ' ')),
			$elm$core$List$concat(
				A2(
					$elm$core$List$map,
					$elm_explorations$test$Test$Html$Internal$ElmHtml$ToString$nodeToLines(options),
					children)));
		var boolAttributes = $elm$core$Maybe$Just(
			A2(
				$elm$core$String$join,
				' ',
				A2(
					$elm$core$List$filterMap,
					function (_v3) {
						var k = _v3.a;
						var v = _v3.b;
						return v ? $elm$core$Maybe$Just(k) : $elm$core$Maybe$Nothing;
					},
					$elm$core$Dict$toList(facts.boolAttributes))));
		var _v2 = $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$toElementKind(tag);
		if (_v2.$ === 'VoidElements') {
			return _List_fromArray(
				[
					openTag(
					_List_fromArray(
						[classes, styles, stringAttributes, boolAttributes]))
				]);
		} else {
			return _Utils_ap(
				_List_fromArray(
					[
						openTag(
						_List_fromArray(
							[classes, styles, stringAttributes, boolAttributes]))
					]),
				_Utils_ap(
					childrenStrings,
					_List_fromArray(
						[closeTag])));
		}
	});
var $elm_explorations$test$Test$Html$Internal$ElmHtml$ToString$nodeToLines = F2(
	function (options, nodeType) {
		switch (nodeType.$) {
			case 'TextTag':
				var text = nodeType.a.text;
				return _List_fromArray(
					[text]);
			case 'NodeEntry':
				var record = nodeType.a;
				return A2($elm_explorations$test$Test$Html$Internal$ElmHtml$ToString$nodeRecordToString, options, record);
			case 'CustomNode':
				return _List_Nil;
			default:
				var record = nodeType.a;
				return _List_fromArray(
					[record.model.markdown]);
		}
	});
var $elm_explorations$test$Test$Html$Internal$ElmHtml$ToString$nodeToStringWithOptions = function (options) {
	return A2(
		$elm$core$Basics$composeR,
		$elm_explorations$test$Test$Html$Internal$ElmHtml$ToString$nodeToLines(options),
		$elm$core$String$join(
			options.newLines ? '\n' : ''));
};
var $elm_explorations$test$Test$Html$Query$Internal$prettyPrint = $elm_explorations$test$Test$Html$Internal$ElmHtml$ToString$nodeToStringWithOptions(
	{indent: 4, newLines: true});
var $elm_explorations$test$Test$Html$Internal$Inert$toElmHtml = function (_v0) {
	var elmHtml = _v0.a;
	return elmHtml;
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$validationMessage = function (validation) {
	return $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$classVsClassNameValidationMessage;
};
var $elm_explorations$test$Test$Html$Query$Internal$toOutputLine = function (query) {
	switch (query.$) {
		case 'Query':
			var node = query.a;
			return $elm_explorations$test$Test$Html$Query$Internal$prettyPrint(
				$elm_explorations$test$Test$Html$Internal$Inert$toElmHtml(node));
		case 'InternalError':
			var message = query.a;
			return 'Internal Error: failed to decode the virtual dom.  Please report this at <https://github.com/elm-explorations/test/issues>.  ' + message;
		default:
			var deduped = query.a.deduped;
			return A2(
				$elm$core$String$join,
				'\n\n',
				A2($elm$core$List$map, $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$validationMessage, deduped));
	}
};
var $elm_explorations$test$Test$Html$Query$Internal$addQueryFromHtmlLine = function (query) {
	return A2(
		$elm$core$String$join,
		'\n\n',
		_List_fromArray(
			[
				$elm_explorations$test$Test$Html$Query$Internal$prefixOutputLine('Query.fromHtml'),
				A2(
				$elm$core$String$join,
				'\n',
				A2(
					$elm$core$List$map,
					$elm$core$Basics$append($elm_explorations$test$Test$Html$Query$Internal$baseIndentation),
					A2(
						$elm$core$String$split,
						'\n',
						$elm_explorations$test$Test$Html$Query$Internal$toOutputLine(query))))
			]));
};
var $elm_explorations$test$Test$Html$Query$Internal$getChildren = function (elmHtml) {
	if (elmHtml.$ === 'NodeEntry') {
		var children = elmHtml.a.children;
		return children;
	} else {
		return _List_Nil;
	}
};
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $elm_explorations$test$Test$Html$Query$Internal$getElementAtHelp = F2(
	function (index, list) {
		getElementAtHelp:
		while (true) {
			if (!list.b) {
				return _List_Nil;
			} else {
				var first = list.a;
				var rest = list.b;
				if (!index) {
					return _List_fromArray(
						[first]);
				} else {
					var $temp$index = index - 1,
						$temp$list = rest;
					index = $temp$index;
					list = $temp$list;
					continue getElementAtHelp;
				}
			}
		}
	});
var $elm$core$Basics$modBy = _Basics_modBy;
var $elm_explorations$test$Test$Html$Query$Internal$getElementAt = F2(
	function (index, list) {
		var length = $elm$core$List$length(list);
		return ((!length) || ((_Utils_cmp(index, length) > -1) || ((index < 0) && (_Utils_cmp(
			$elm$core$Basics$abs(index),
			length) > 0)))) ? _List_Nil : A2(
			$elm_explorations$test$Test$Html$Query$Internal$getElementAtHelp,
			A2($elm$core$Basics$modBy, length, index),
			list);
	});
var $elm$core$String$append = _String_append;
var $elm_explorations$test$Test$Html$Query$Internal$printIndented = F3(
	function (maxDigits, index, elmHtml) {
		var caption = A2(
			$elm$core$String$append,
			$elm_explorations$test$Test$Html$Query$Internal$baseIndentation,
			A3(
				$elm$core$String$padRight,
				maxDigits + 3,
				_Utils_chr(' '),
				$elm$core$String$fromInt(index + 1) + ')'));
		var indentation = A2(
			$elm$core$String$repeat,
			$elm$core$String$length(caption),
			' ');
		var _v0 = A2(
			$elm$core$String$split,
			'\n',
			$elm_explorations$test$Test$Html$Query$Internal$prettyPrint(elmHtml));
		if (!_v0.b) {
			return '';
		} else {
			var first = _v0.a;
			var rest = _v0.b;
			return A2(
				$elm$core$String$join,
				'\n',
				A2(
					$elm$core$List$cons,
					_Utils_ap(caption, first),
					A2(
						$elm$core$List$map,
						$elm$core$String$append(indentation),
						rest)));
		}
	});
var $elm_explorations$test$Test$Html$Query$Internal$getHtmlContext = function (elmHtmlList) {
	if ($elm$core$List$isEmpty(elmHtmlList)) {
		return '0 matches found for this query.';
	} else {
		var maxDigits = $elm$core$String$length(
			$elm$core$String$fromInt(
				$elm$core$List$length(elmHtmlList)));
		return A2(
			$elm$core$String$join,
			'\n\n',
			A2(
				$elm$core$List$indexedMap,
				$elm_explorations$test$Test$Html$Query$Internal$printIndented(maxDigits),
				elmHtmlList));
	}
};
var $elm_explorations$test$Test$Html$Query$Internal$joinAsList = F2(
	function (toStr, list) {
		return $elm$core$List$isEmpty(list) ? '[]' : ('[ ' + (A2(
			$elm$core$String$join,
			', ',
			A2($elm$core$List$map, toStr, list)) + ' ]'));
	});
var $elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			$elm$core$List$any,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, isOkay),
			list);
	});
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$hasAttribute = F3(
	function (attribute, queryString, facts) {
		var _v0 = A2($elm$core$Dict$get, attribute, facts.stringAttributes);
		if (_v0.$ === 'Just') {
			var id = _v0.a;
			return _Utils_eq(id, queryString);
		} else {
			return false;
		}
	});
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$hasBoolAttribute = F3(
	function (attribute, value, facts) {
		var _v0 = A2($elm$core$Dict$get, attribute, facts.boolAttributes);
		if (_v0.$ === 'Just') {
			var id = _v0.a;
			return _Utils_eq(id, value);
		} else {
			return false;
		}
	});
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$classnames = function (facts) {
	return A2(
		$elm$core$String$split,
		' ',
		function () {
			var _v0 = _Utils_Tuple2(
				A2($elm$core$Dict$get, 'class', facts.stringAttributes),
				A2($elm$core$Dict$get, 'className', facts.stringAttributes));
			if (_v0.a.$ === 'Just') {
				if (_v0.b.$ === 'Just') {
					return '';
				} else {
					var _class = _v0.a.a;
					var _v1 = _v0.b;
					return _class;
				}
			} else {
				if (_v0.b.$ === 'Just') {
					var _v2 = _v0.a;
					var className = _v0.b.a;
					return className;
				} else {
					var _v3 = _v0.a;
					var _v4 = _v0.b;
					return '';
				}
			}
		}());
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$hasClass = F2(
	function (queryString, facts) {
		return A2(
			$elm$core$List$member,
			queryString,
			$elm_explorations$test$Test$Html$Internal$ElmHtml$Query$classnames(facts));
	});
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$containsAll = F2(
	function (a, b) {
		return $elm$core$List$isEmpty(
			A3(
				$elm$core$List$foldl,
				F2(
					function (i, acc) {
						return A2(
							$elm$core$List$filter,
							$elm$core$Basics$neq(i),
							acc);
					}),
				a,
				b));
	});
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$hasClasses = F2(
	function (classList, facts) {
		return A2(
			$elm_explorations$test$Test$Html$Internal$ElmHtml$Query$containsAll,
			classList,
			$elm_explorations$test$Test$Html$Internal$ElmHtml$Query$classnames(facts));
	});
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$hasStyle = F2(
	function (style, facts) {
		return _Utils_eq(
			A2($elm$core$Dict$get, style.key, facts.styles),
			$elm$core$Maybe$Just(style.value));
	});
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$hasAllSelectors = F2(
	function (selectors, record) {
		return A2(
			$elm$core$List$all,
			$elm$core$Basics$identity,
			A2(
				$elm$core$List$map,
				function (selector) {
					return selector(record);
				},
				A2($elm$core$List$map, $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$predicateFromSelector, selectors)));
	});
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$markdownPredicate = function (selector) {
	switch (selector.$) {
		case 'Id':
			var id = selector.a;
			return A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.facts;
				},
				A2($elm_explorations$test$Test$Html$Internal$ElmHtml$Query$hasAttribute, 'id', id));
		case 'ClassName':
			var classname = selector.a;
			return A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.facts;
				},
				$elm_explorations$test$Test$Html$Internal$ElmHtml$Query$hasClass(classname));
		case 'ClassList':
			var classList = selector.a;
			return A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.facts;
				},
				$elm_explorations$test$Test$Html$Internal$ElmHtml$Query$hasClasses(classList));
		case 'Tag':
			return $elm$core$Basics$always(false);
		case 'Attribute':
			var key = selector.a;
			var value = selector.b;
			return A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.facts;
				},
				A2($elm_explorations$test$Test$Html$Internal$ElmHtml$Query$hasAttribute, key, value));
		case 'BoolAttribute':
			var key = selector.a;
			var value = selector.b;
			return A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.facts;
				},
				A2($elm_explorations$test$Test$Html$Internal$ElmHtml$Query$hasBoolAttribute, key, value));
		case 'Style':
			var style = selector.a;
			return A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.facts;
				},
				$elm_explorations$test$Test$Html$Internal$ElmHtml$Query$hasStyle(style));
		case 'ContainsText':
			var text = selector.a;
			return A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.model;
				},
				A2(
					$elm$core$Basics$composeR,
					function ($) {
						return $.markdown;
					},
					$elm$core$String$contains(text)));
		case 'ContainsExactText':
			var text = selector.a;
			return A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.model;
				},
				A2(
					$elm$core$Basics$composeR,
					function ($) {
						return $.markdown;
					},
					$elm$core$Basics$eq(text)));
		default:
			var selectors = selector.a;
			return A2(
				$elm$core$Basics$composeR,
				$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$MarkdownNode,
				$elm_explorations$test$Test$Html$Internal$ElmHtml$Query$hasAllSelectors(selectors));
	}
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$nodeRecordPredicate = function (selector) {
	switch (selector.$) {
		case 'Id':
			var id = selector.a;
			return A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.facts;
				},
				A2($elm_explorations$test$Test$Html$Internal$ElmHtml$Query$hasAttribute, 'id', id));
		case 'ClassName':
			var classname = selector.a;
			return A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.facts;
				},
				$elm_explorations$test$Test$Html$Internal$ElmHtml$Query$hasClass(classname));
		case 'ClassList':
			var classList = selector.a;
			return A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.facts;
				},
				$elm_explorations$test$Test$Html$Internal$ElmHtml$Query$hasClasses(classList));
		case 'Tag':
			var tag = selector.a;
			return A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.tag;
				},
				$elm$core$Basics$eq(tag));
		case 'Attribute':
			var key = selector.a;
			var value = selector.b;
			return A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.facts;
				},
				A2($elm_explorations$test$Test$Html$Internal$ElmHtml$Query$hasAttribute, key, value));
		case 'BoolAttribute':
			var key = selector.a;
			var value = selector.b;
			return A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.facts;
				},
				A2($elm_explorations$test$Test$Html$Internal$ElmHtml$Query$hasBoolAttribute, key, value));
		case 'Style':
			var style = selector.a;
			return A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.facts;
				},
				$elm_explorations$test$Test$Html$Internal$ElmHtml$Query$hasStyle(style));
		case 'ContainsText':
			return $elm$core$Basics$always(false);
		case 'ContainsExactText':
			return $elm$core$Basics$always(false);
		default:
			var selectors = selector.a;
			return A2(
				$elm$core$Basics$composeR,
				$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$NodeEntry,
				$elm_explorations$test$Test$Html$Internal$ElmHtml$Query$hasAllSelectors(selectors));
	}
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$predicateFromSelector = F2(
	function (selector, html) {
		switch (html.$) {
			case 'NodeEntry':
				var record = html.a;
				return A2($elm_explorations$test$Test$Html$Internal$ElmHtml$Query$nodeRecordPredicate, selector, record);
			case 'MarkdownNode':
				var markdownModel = html.a;
				return A2($elm_explorations$test$Test$Html$Internal$ElmHtml$Query$markdownPredicate, selector, markdownModel);
			default:
				return false;
		}
	});
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$descendInQuery = F3(
	function (maxDescendantDepth, selector, children) {
		if (maxDescendantDepth.$ === 'Nothing') {
			return A2(
				$elm$core$List$concatMap,
				A2($elm_explorations$test$Test$Html$Internal$ElmHtml$Query$queryInNodeHelp, $elm$core$Maybe$Nothing, selector),
				children);
		} else {
			var depth = maxDescendantDepth.a;
			return (depth > 0) ? A2(
				$elm$core$List$concatMap,
				A2(
					$elm_explorations$test$Test$Html$Internal$ElmHtml$Query$queryInNodeHelp,
					$elm$core$Maybe$Just(depth - 1),
					selector),
				children) : _List_Nil;
		}
	});
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$queryInNodeHelp = F3(
	function (maxDescendantDepth, selector, node) {
		switch (node.$) {
			case 'NodeEntry':
				var record = node.a;
				var childEntries = A3($elm_explorations$test$Test$Html$Internal$ElmHtml$Query$descendInQuery, maxDescendantDepth, selector, record.children);
				return A2($elm_explorations$test$Test$Html$Internal$ElmHtml$Query$predicateFromSelector, selector, node) ? A2($elm$core$List$cons, node, childEntries) : childEntries;
			case 'TextTag':
				var text = node.a.text;
				switch (selector.$) {
					case 'ContainsText':
						var innerText = selector.a;
						return A2($elm$core$String$contains, innerText, text) ? _List_fromArray(
							[node]) : _List_Nil;
					case 'ContainsExactText':
						var innerText = selector.a;
						return _Utils_eq(innerText, text) ? _List_fromArray(
							[node]) : _List_Nil;
					default:
						return _List_Nil;
				}
			case 'MarkdownNode':
				return A2($elm_explorations$test$Test$Html$Internal$ElmHtml$Query$predicateFromSelector, selector, node) ? _List_fromArray(
					[node]) : _List_Nil;
			default:
				return _List_Nil;
		}
	});
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$queryInNode = $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$queryInNodeHelp($elm$core$Maybe$Nothing);
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$query = function (selector) {
	return $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$queryInNode(selector);
};
var $elm_explorations$test$Test$Html$Selector$Internal$All = function (a) {
	return {$: 'All', a: a};
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$Attribute = F2(
	function (a, b) {
		return {$: 'Attribute', a: a, b: b};
	});
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$BoolAttribute = F2(
	function (a, b) {
		return {$: 'BoolAttribute', a: a, b: b};
	});
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$ClassList = function (a) {
	return {$: 'ClassList', a: a};
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$ContainsExactText = function (a) {
	return {$: 'ContainsExactText', a: a};
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$ContainsText = function (a) {
	return {$: 'ContainsText', a: a};
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$Style = function (a) {
	return {$: 'Style', a: a};
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$Tag = function (a) {
	return {$: 'Tag', a: a};
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$getChildren = function (elmHtml) {
	if (elmHtml.$ === 'NodeEntry') {
		var children = elmHtml.a.children;
		return children;
	} else {
		return _List_Nil;
	}
};
var $elm_explorations$test$Test$Html$Selector$Internal$query = F4(
	function (fn, fnAll, selector, list) {
		if (!list.b) {
			return list;
		} else {
			var elems = list;
			switch (selector.$) {
				case 'All':
					var selectors = selector.a;
					return A2(fnAll, selectors, elems);
				case 'Classes':
					var classes = selector.a;
					return A2(
						$elm$core$List$concatMap,
						fn(
							$elm_explorations$test$Test$Html$Internal$ElmHtml$Query$ClassList(classes)),
						elems);
				case 'Class':
					var _class = selector.a;
					return A2(
						$elm$core$List$concatMap,
						fn(
							$elm_explorations$test$Test$Html$Internal$ElmHtml$Query$ClassList(
								_List_fromArray(
									[_class]))),
						elems);
				case 'Attribute':
					var name = selector.a.name;
					var value = selector.a.value;
					return A2(
						$elm$core$List$concatMap,
						fn(
							A2($elm_explorations$test$Test$Html$Internal$ElmHtml$Query$Attribute, name, value)),
						elems);
				case 'BoolAttribute':
					var name = selector.a.name;
					var value = selector.a.value;
					return A2(
						$elm$core$List$concatMap,
						fn(
							A2($elm_explorations$test$Test$Html$Internal$ElmHtml$Query$BoolAttribute, name, value)),
						elems);
				case 'Style':
					var style = selector.a;
					return A2(
						$elm$core$List$concatMap,
						fn(
							$elm_explorations$test$Test$Html$Internal$ElmHtml$Query$Style(style)),
						elems);
				case 'Tag':
					var name = selector.a;
					return A2(
						$elm$core$List$concatMap,
						fn(
							$elm_explorations$test$Test$Html$Internal$ElmHtml$Query$Tag(name)),
						elems);
				case 'Text':
					var text = selector.a;
					return A2(
						$elm$core$List$concatMap,
						fn(
							$elm_explorations$test$Test$Html$Internal$ElmHtml$Query$ContainsText(text)),
						elems);
				case 'ExactText':
					var text = selector.a;
					return A2(
						$elm$core$List$concatMap,
						fn(
							$elm_explorations$test$Test$Html$Internal$ElmHtml$Query$ContainsExactText(text)),
						elems);
				case 'Containing':
					var selectors = selector.a;
					var anyDescendantsMatch = function (elem) {
						var _v2 = $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$getChildren(elem);
						if (!_v2.b) {
							return false;
						} else {
							var children = _v2;
							var _v3 = A4(
								$elm_explorations$test$Test$Html$Selector$Internal$query,
								fn,
								fnAll,
								$elm_explorations$test$Test$Html$Selector$Internal$All(selectors),
								children);
							if (!_v3.b) {
								return A2($elm$core$List$any, anyDescendantsMatch, children);
							} else {
								return true;
							}
						}
					};
					return A2($elm$core$List$filter, anyDescendantsMatch, elems);
				default:
					return _List_Nil;
			}
		}
	});
var $elm_explorations$test$Test$Html$Selector$Internal$queryAll = F2(
	function (selectors, list) {
		if (!selectors.b) {
			return list;
		} else {
			var selector = selectors.a;
			var rest = selectors.b;
			return A2(
				$elm_explorations$test$Test$Html$Selector$Internal$queryAll,
				rest,
				A4($elm_explorations$test$Test$Html$Selector$Internal$query, $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$query, $elm_explorations$test$Test$Html$Selector$Internal$queryAll, selector, list));
		}
	});
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$queryChildren = $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$queryInNodeHelp(
	$elm$core$Maybe$Just(1));
var $elm_explorations$test$Test$Html$Selector$Internal$queryAllChildren = F2(
	function (selectors, list) {
		if (!selectors.b) {
			return list;
		} else {
			var selector = selectors.a;
			var rest = selectors.b;
			return A2(
				$elm_explorations$test$Test$Html$Selector$Internal$queryAllChildren,
				rest,
				A4($elm_explorations$test$Test$Html$Selector$Internal$query, $elm_explorations$test$Test$Html$Internal$ElmHtml$Query$queryChildren, $elm_explorations$test$Test$Html$Selector$Internal$queryAllChildren, selector, list));
		}
	});
var $elm_explorations$test$Test$Html$Selector$Internal$styleToString = function (_v0) {
	var key = _v0.key;
	var value = _v0.value;
	return key + (':' + value);
};
var $elm_explorations$test$Test$Html$Selector$Internal$selectorToString = function (criteria) {
	var quoteString = function (s) {
		return '\"' + (s + '\"');
	};
	var boolToString = function (b) {
		if (b) {
			return 'True';
		} else {
			return 'False';
		}
	};
	switch (criteria.$) {
		case 'All':
			var list = criteria.a;
			return A2(
				$elm$core$String$join,
				' ',
				A2($elm$core$List$map, $elm_explorations$test$Test$Html$Selector$Internal$selectorToString, list));
		case 'Classes':
			var list = criteria.a;
			return 'classes ' + quoteString(
				A2($elm$core$String$join, ' ', list));
		case 'Class':
			var _class = criteria.a;
			return 'class ' + quoteString(_class);
		case 'Attribute':
			var name = criteria.a.name;
			var value = criteria.a.value;
			return 'attribute ' + (quoteString(name) + (' ' + quoteString(value)));
		case 'BoolAttribute':
			var name = criteria.a.name;
			var value = criteria.a.value;
			return 'attribute ' + (quoteString(name) + (' ' + boolToString(value)));
		case 'Style':
			var style = criteria.a;
			return 'styles ' + $elm_explorations$test$Test$Html$Selector$Internal$styleToString(style);
		case 'Tag':
			var name = criteria.a;
			return 'tag ' + quoteString(name);
		case 'Text':
			var text = criteria.a;
			return 'text ' + quoteString(text);
		case 'ExactText':
			var text = criteria.a;
			return 'exact text ' + quoteString(text);
		case 'Containing':
			var list = criteria.a;
			var selectors = A2(
				$elm$core$String$join,
				', ',
				A2($elm$core$List$map, $elm_explorations$test$Test$Html$Selector$Internal$selectorToString, list));
			return 'containing [ ' + (selectors + ' ] ');
		default:
			return 'invalid';
	}
};
var $elm_explorations$test$Test$Html$Query$Internal$withHtmlContext = F2(
	function (htmlStr, str) {
		return A2(
			$elm$core$String$join,
			'\n\n',
			_List_fromArray(
				[str, htmlStr]));
	});
var $elm_explorations$test$Test$Html$Query$Internal$toLinesHelp = F5(
	function (expectationFailure, elmHtmlList, selectorQueries, queryName, results) {
		var recurse = F3(
			function (newElmHtmlList, rest, result) {
				return A5(
					$elm_explorations$test$Test$Html$Query$Internal$toLinesHelp,
					expectationFailure,
					newElmHtmlList,
					rest,
					queryName,
					A2($elm$core$List$cons, result, results));
			});
		var bailOut = function (result) {
			return A2(
				$elm$core$List$cons,
				A2(
					$elm$core$String$join,
					'\n\n\n✗ ',
					_List_fromArray(
						[result, expectationFailure])),
				results);
		};
		if (!selectorQueries.b) {
			return A2(
				$elm$core$List$cons,
				A2(
					$elm$core$String$join,
					'\n\n',
					_List_fromArray(
						[queryName, expectationFailure])),
				results);
		} else {
			var selectorQuery = selectorQueries.a;
			var rest = selectorQueries.b;
			switch (selectorQuery.$) {
				case 'FindAll':
					var selectors = selectorQuery.a;
					var elements = A2(
						$elm_explorations$test$Test$Html$Selector$Internal$queryAll,
						selectors,
						A2($elm$core$List$concatMap, $elm_explorations$test$Test$Html$Query$Internal$getChildren, elmHtmlList));
					return A3(
						recurse,
						elements,
						rest,
						A2(
							$elm_explorations$test$Test$Html$Query$Internal$withHtmlContext,
							$elm_explorations$test$Test$Html$Query$Internal$getHtmlContext(elements),
							'Query.findAll ' + A2($elm_explorations$test$Test$Html$Query$Internal$joinAsList, $elm_explorations$test$Test$Html$Selector$Internal$selectorToString, selectors)));
				case 'Find':
					var selectors = selectorQuery.a;
					var elements = A2(
						$elm_explorations$test$Test$Html$Selector$Internal$queryAll,
						selectors,
						A2($elm$core$List$concatMap, $elm_explorations$test$Test$Html$Query$Internal$getChildren, elmHtmlList));
					var result = A2(
						$elm_explorations$test$Test$Html$Query$Internal$withHtmlContext,
						$elm_explorations$test$Test$Html$Query$Internal$getHtmlContext(elements),
						'Query.find ' + A2($elm_explorations$test$Test$Html$Query$Internal$joinAsList, $elm_explorations$test$Test$Html$Selector$Internal$selectorToString, selectors));
					return ($elm$core$List$length(elements) === 1) ? A3(recurse, elements, rest, result) : bailOut(result);
				case 'Children':
					var selectors = selectorQuery.a;
					var elements = A2(
						$elm_explorations$test$Test$Html$Selector$Internal$queryAllChildren,
						selectors,
						A2($elm$core$List$concatMap, $elm_explorations$test$Test$Html$Query$Internal$getChildren, elmHtmlList));
					return A3(
						recurse,
						elements,
						rest,
						A2(
							$elm_explorations$test$Test$Html$Query$Internal$withHtmlContext,
							$elm_explorations$test$Test$Html$Query$Internal$getHtmlContext(elements),
							'Query.children ' + A2($elm_explorations$test$Test$Html$Query$Internal$joinAsList, $elm_explorations$test$Test$Html$Selector$Internal$selectorToString, selectors)));
				case 'First':
					var elements = A2(
						$elm$core$Maybe$withDefault,
						_List_Nil,
						A2(
							$elm$core$Maybe$map,
							function (elem) {
								return _List_fromArray(
									[elem]);
							},
							$elm$core$List$head(elmHtmlList)));
					var result = A2(
						$elm_explorations$test$Test$Html$Query$Internal$withHtmlContext,
						$elm_explorations$test$Test$Html$Query$Internal$getHtmlContext(elements),
						'Query.first');
					return ($elm$core$List$length(elements) === 1) ? A3(recurse, elements, rest, result) : bailOut(result);
				default:
					var index = selectorQuery.a;
					var elements = A2($elm_explorations$test$Test$Html$Query$Internal$getElementAt, index, elmHtmlList);
					var result = A2(
						$elm_explorations$test$Test$Html$Query$Internal$withHtmlContext,
						$elm_explorations$test$Test$Html$Query$Internal$getHtmlContext(elements),
						'Query.index ' + $elm$core$String$fromInt(index));
					return ($elm$core$List$length(elements) === 1) ? A3(recurse, elements, rest, result) : bailOut(result);
			}
		}
	});
var $elm_explorations$test$Test$Html$Query$Internal$toLines = F4(
	function (_v0, expectationFailure, query, queryName) {
		var showQueryError = _v0.showQueryError;
		switch (query.$) {
			case 'Query':
				var node = query.a;
				var selectors = query.b;
				return $elm$core$List$reverse(
					A5(
						$elm_explorations$test$Test$Html$Query$Internal$toLinesHelp,
						expectationFailure,
						_List_fromArray(
							[
								$elm_explorations$test$Test$Html$Internal$Inert$toElmHtml(node)
							]),
						$elm$core$List$reverse(selectors),
						queryName,
						_List_Nil));
			case 'InternalError':
				var message = query.a;
				return showQueryError ? _List_fromArray(
					['Internal Error: failed to decode the virtual dom.  Please report this at <https://github.com/elm-explorations/test/issues>.  ', message]) : _List_Nil;
			default:
				var deduped = query.a.deduped;
				return showQueryError ? A2($elm$core$List$map, $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$validationMessage, deduped) : _List_Nil;
		}
	});
var $elm_explorations$test$Test$Html$Query$Internal$failWithQuery = F4(
	function (showTrace, queryName, query, expectation) {
		var _v0 = $elm_explorations$test$Test$Runner$getFailureReason(expectation);
		if (_v0.$ === 'Just') {
			var description = _v0.a.description;
			var lines = A2(
				$elm$core$List$map,
				$elm_explorations$test$Test$Html$Query$Internal$prefixOutputLine,
				A4(
					$elm_explorations$test$Test$Html$Query$Internal$toLines,
					{showQueryError: !showTrace},
					description,
					query,
					queryName));
			var tracedLines = showTrace ? A2(
				$elm$core$List$cons,
				$elm_explorations$test$Test$Html$Query$Internal$addQueryFromHtmlLine(query),
				lines) : lines;
			return $elm_explorations$test$Expect$fail(
				A2($elm$core$String$join, '\n\n\n', tracedLines));
		} else {
			return expectation;
		}
	});
var $elm_explorations$test$Test$Html$Selector$Internal$hasAll = F2(
	function (selectors, elems) {
		hasAll:
		while (true) {
			if (!selectors.b) {
				return true;
			} else {
				var selector = selectors.a;
				var rest = selectors.b;
				if ($elm$core$List$isEmpty(
					A2(
						$elm_explorations$test$Test$Html$Selector$Internal$queryAll,
						_List_fromArray(
							[selector]),
						elems))) {
					return false;
				} else {
					var $temp$selectors = rest,
						$temp$elems = elems;
					selectors = $temp$selectors;
					elems = $temp$elems;
					continue hasAll;
				}
			}
		}
	});
var $elm_explorations$test$Test$Html$Query$Internal$queryErrorToString = function (error) {
	switch (error.$) {
		case 'NoResultsForSingle':
			var queryName = error.a;
			return queryName + ' always expects to find 1 element, but it found 0 instead.';
		case 'MultipleResultsForSingle':
			var queryName = error.a;
			var resultCount = error.b;
			return queryName + (' always expects to find 1 element, but it found ' + ($elm$core$String$fromInt(resultCount) + (' instead.\n\n\nHINT: If you actually expected ' + ($elm$core$String$fromInt(resultCount) + ' elements, use Query.findAll instead of Query.find.'))));
		case 'OtherInternalError':
			var message = error.a;
			return 'Internal Error: failed to decode the virtual dom.  Please report this at <https://github.com/elm-explorations/test/issues>.  ' + message;
		default:
			var deduped = error.a.deduped;
			return A2(
				$elm$core$String$join,
				'\n\n',
				A2($elm$core$List$map, $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$validationMessage, deduped));
	}
};
var $elm_explorations$test$Test$Html$Query$Internal$showSelectorOutcome = F2(
	function (elmHtmlList, selector) {
		var outcome = function () {
			var _v0 = A2(
				$elm_explorations$test$Test$Html$Selector$Internal$queryAll,
				_List_fromArray(
					[selector]),
				elmHtmlList);
			if (!_v0.b) {
				return '✗';
			} else {
				return '✓';
			}
		}();
		return A2(
			$elm$core$String$join,
			' ',
			_List_fromArray(
				[
					outcome,
					'has',
					$elm_explorations$test$Test$Html$Selector$Internal$selectorToString(selector)
				]));
	});
var $elm_explorations$test$Test$Html$Query$Internal$OtherInternalError = function (a) {
	return {$: 'OtherInternalError', a: a};
};
var $elm_explorations$test$Test$Html$Query$Internal$QueryValidationErrors = function (a) {
	return {$: 'QueryValidationErrors', a: a};
};
var $elm$core$Result$andThen = F2(
	function (callback, result) {
		if (result.$ === 'Ok') {
			var value = result.a;
			return callback(value);
		} else {
			var msg = result.a;
			return $elm$core$Result$Err(msg);
		}
	});
var $elm_explorations$test$Test$Html$Query$Internal$NoResultsForSingle = function (a) {
	return {$: 'NoResultsForSingle', a: a};
};
var $elm_explorations$test$Test$Html$Query$Internal$MultipleResultsForSingle = F2(
	function (a, b) {
		return {$: 'MultipleResultsForSingle', a: a, b: b};
	});
var $elm_explorations$test$Test$Html$Query$Internal$verifySingle = F2(
	function (queryName, list) {
		if (!list.b) {
			return $elm$core$Result$Err(
				$elm_explorations$test$Test$Html$Query$Internal$NoResultsForSingle(queryName));
		} else {
			if (!list.b.b) {
				var singleton = list.a;
				return $elm$core$Result$Ok(singleton);
			} else {
				var multiples = list;
				return $elm$core$Result$Err(
					A2(
						$elm_explorations$test$Test$Html$Query$Internal$MultipleResultsForSingle,
						queryName,
						$elm$core$List$length(multiples)));
			}
		}
	});
var $elm_explorations$test$Test$Html$Query$Internal$traverseSelector = F2(
	function (selectorQuery, elmHtmlList) {
		switch (selectorQuery.$) {
			case 'Find':
				var selectors = selectorQuery.a;
				return A2(
					$elm$core$Result$map,
					function (elem) {
						return _List_fromArray(
							[elem]);
					},
					A2(
						$elm_explorations$test$Test$Html$Query$Internal$verifySingle,
						'Query.find',
						A2(
							$elm_explorations$test$Test$Html$Selector$Internal$queryAll,
							selectors,
							A2($elm$core$List$concatMap, $elm_explorations$test$Test$Html$Query$Internal$getChildren, elmHtmlList))));
			case 'FindAll':
				var selectors = selectorQuery.a;
				return $elm$core$Result$Ok(
					A2(
						$elm_explorations$test$Test$Html$Selector$Internal$queryAll,
						selectors,
						A2($elm$core$List$concatMap, $elm_explorations$test$Test$Html$Query$Internal$getChildren, elmHtmlList)));
			case 'Children':
				var selectors = selectorQuery.a;
				return $elm$core$Result$Ok(
					A2(
						$elm_explorations$test$Test$Html$Selector$Internal$queryAllChildren,
						selectors,
						A2($elm$core$List$concatMap, $elm_explorations$test$Test$Html$Query$Internal$getChildren, elmHtmlList)));
			case 'First':
				return A2(
					$elm$core$Maybe$withDefault,
					$elm$core$Result$Err(
						$elm_explorations$test$Test$Html$Query$Internal$NoResultsForSingle('Query.first')),
					A2(
						$elm$core$Maybe$map,
						function (elem) {
							return $elm$core$Result$Ok(
								_List_fromArray(
									[elem]));
						},
						$elm$core$List$head(elmHtmlList)));
			default:
				var index = selectorQuery.a;
				var elements = A2($elm_explorations$test$Test$Html$Query$Internal$getElementAt, index, elmHtmlList);
				return ($elm$core$List$length(elements) === 1) ? $elm$core$Result$Ok(elements) : $elm$core$Result$Err(
					$elm_explorations$test$Test$Html$Query$Internal$NoResultsForSingle(
						'Query.index ' + $elm$core$String$fromInt(index)));
		}
	});
var $elm_explorations$test$Test$Html$Query$Internal$traverseSelectors = F2(
	function (selectorQueries, elmHtmlList) {
		return A3(
			$elm$core$List$foldr,
			A2($elm$core$Basics$composeR, $elm_explorations$test$Test$Html$Query$Internal$traverseSelector, $elm$core$Result$andThen),
			$elm$core$Result$Ok(elmHtmlList),
			selectorQueries);
	});
var $elm_explorations$test$Test$Html$Query$Internal$traverse = function (query) {
	switch (query.$) {
		case 'Query':
			var node = query.a;
			var selectorQueries = query.b;
			return A2(
				$elm_explorations$test$Test$Html$Query$Internal$traverseSelectors,
				selectorQueries,
				_List_fromArray(
					[
						$elm_explorations$test$Test$Html$Internal$Inert$toElmHtml(node)
					]));
		case 'InternalError':
			var message = query.a;
			return $elm$core$Result$Err(
				$elm_explorations$test$Test$Html$Query$Internal$OtherInternalError(message));
		default:
			var validations = query.a;
			return $elm$core$Result$Err(
				$elm_explorations$test$Test$Html$Query$Internal$QueryValidationErrors(validations));
	}
};
var $elm_explorations$test$Test$Html$Query$Internal$has = F2(
	function (selectors, query) {
		var _v0 = $elm_explorations$test$Test$Html$Query$Internal$traverse(query);
		if (_v0.$ === 'Ok') {
			var elmHtmlList = _v0.a;
			return A2($elm_explorations$test$Test$Html$Selector$Internal$hasAll, selectors, elmHtmlList) ? $elm_explorations$test$Expect$pass : $elm_explorations$test$Expect$fail(
				A2(
					$elm$core$String$join,
					'\n',
					A2(
						$elm$core$List$map,
						$elm_explorations$test$Test$Html$Query$Internal$showSelectorOutcome(elmHtmlList),
						selectors)));
		} else {
			var error = _v0.a;
			return $elm_explorations$test$Expect$fail(
				$elm_explorations$test$Test$Html$Query$Internal$queryErrorToString(error));
		}
	});
var $elm_explorations$test$Test$Html$Query$has = F2(
	function (selectors, _v0) {
		var showTrace = _v0.a;
		var query = _v0.b;
		return A4(
			$elm_explorations$test$Test$Html$Query$Internal$failWithQuery,
			showTrace,
			'Query.has ' + A2($elm_explorations$test$Test$Html$Query$Internal$joinAsList, $elm_explorations$test$Test$Html$Selector$Internal$selectorToString, selectors),
			query,
			A2($elm_explorations$test$Test$Html$Query$Internal$has, selectors, query));
	});
var $elm_explorations$test$Test$Html$Selector$Internal$Tag = function (a) {
	return {$: 'Tag', a: a};
};
var $elm_explorations$test$Test$Html$Selector$tag = function (name) {
	return $elm_explorations$test$Test$Html$Selector$Internal$Tag(name);
};
var $elm_explorations$test$Test$Internal$blankDescriptionFailure = $elm_explorations$test$Test$Internal$failNow(
	{
		description: 'This test has a blank description. Let\'s give it a useful one!',
		reason: $elm_explorations$test$Test$Runner$Failure$Invalid($elm_explorations$test$Test$Runner$Failure$BadDescription)
	});
var $elm_explorations$test$Test$test = F2(
	function (untrimmedDesc, thunk) {
		var desc = $elm$core$String$trim(untrimmedDesc);
		return $elm$core$String$isEmpty(desc) ? $elm_explorations$test$Test$Internal$blankDescriptionFailure : A2(
			$elm_explorations$test$Test$Internal$ElmTestVariant__Labeled,
			desc,
			$elm_explorations$test$Test$Internal$ElmTestVariant__UnitTest(
				function (_v0) {
					return _List_fromArray(
						[
							thunk(_Utils_Tuple0)
						]);
				}));
	});
var $elm_explorations$test$Test$Html$Selector$Internal$Text = function (a) {
	return {$: 'Text', a: a};
};
var $elm_explorations$test$Test$Html$Selector$text = $elm_explorations$test$Test$Html$Selector$Internal$Text;
var $elm$core$String$endsWith = _String_endsWith;
var $author$project$Ui$Activity$timestamp = function (value) {
	return (A2($elm$core$String$contains, 'T', value) && A2($elm$core$String$endsWith, 'Z', value)) ? (A2($elm$core$String$left, 10, value) + (' ' + (A3($elm$core$String$slice, 11, 19, value) + ' UTC'))) : value;
};
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$html$Html$article = _VirtualDom_node('article');
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$html$Html$code = _VirtualDom_node('code');
var $elm$html$Html$details = _VirtualDom_node('details');
var $elm$html$Html$p = _VirtualDom_node('p');
var $elm$html$Html$pre = _VirtualDom_node('pre');
var $elm$html$Html$summary = _VirtualDom_node('summary');
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$Page$Activity$detail = function (event) {
	return A2(
		$elm$html$Html$details,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$summary,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(
						'기록 상세 #' + $elm$core$String$fromInt(event.seq))
					])),
				A2(
				$elm$html$Html$p,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('원본 시각: ' + event.at)
					])),
				A2(
				$elm$html$Html$p,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(
						'기록 주체 ID: ' + A2($elm$core$Maybe$withDefault, '없음', event.actor))
					])),
				A2(
				$elm$html$Html$p,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('대상 ID: ' + event.activity.targetId)
					])),
				A2(
				$elm$html$Html$p,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('원본 설명: ' + event.description)
					])),
				A2(
				$elm$html$Html$pre,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('activity-raw')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$code,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(event.activity.raw)
							]))
					]))
			]));
};
var $elm$html$Html$h3 = _VirtualDom_node('h3');
var $elm$html$Html$span = _VirtualDom_node('span');
var $author$project$Page$Activity$card = F2(
	function (w, event) {
		return A2(
			$elm$html$Html$article,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('panel')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('tag')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							$author$project$Ui$Activity$category(event))
						])),
					A2(
					$elm$html$Html$h3,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							A2($author$project$Ui$Activity$targetName, w, event))
						])),
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							$author$project$Ui$Activity$timestamp(event.at))
						])),
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							A2($author$project$Ui$Activity$actorName, w, event))
						])),
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							A2($author$project$Ui$Activity$description, w, event))
						])),
					$author$project$Page$Activity$detail(event)
				]));
	});
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$html$Html$h2 = _VirtualDom_node('h2');
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$label = _VirtualDom_node('label');
var $author$project$Ui$Common$note = function (content) {
	return A2(
		$elm$html$Html$p,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('note')
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(content)
			]));
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $elm$html$Html$option = _VirtualDom_node('option');
var $elm$html$Html$Attributes$scope = $elm$html$Html$Attributes$stringProperty('scope');
var $elm$html$Html$td = _VirtualDom_node('td');
var $elm$html$Html$th = _VirtualDom_node('th');
var $elm$html$Html$tr = _VirtualDom_node('tr');
var $author$project$Page$Activity$row = F2(
	function (w, event) {
		return A2(
			$elm$html$Html$tr,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$td,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							$author$project$Ui$Activity$timestamp(event.at))
						])),
					A2(
					$elm$html$Html$td,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							$author$project$Ui$Activity$category(event))
						])),
					A2(
					$elm$html$Html$th,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$scope('row')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							A2($author$project$Ui$Activity$targetName, w, event))
						])),
					A2(
					$elm$html$Html$td,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							A2($author$project$Ui$Activity$actorName, w, event))
						])),
					A2(
					$elm$html$Html$td,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$p,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(
									A2($author$project$Ui$Activity$description, w, event))
								])),
							$author$project$Page$Activity$detail(event)
						]))
				]));
	});
var $elm$html$Html$section = _VirtualDom_node('section');
var $elm$html$Html$select = _VirtualDom_node('select');
var $elm$html$Html$Attributes$tabindex = function (n) {
	return A2(
		_VirtualDom_attribute,
		'tabIndex',
		$elm$core$String$fromInt(n));
};
var $elm$html$Html$caption = _VirtualDom_node('caption');
var $elm$html$Html$table = _VirtualDom_node('table');
var $elm$html$Html$tbody = _VirtualDom_node('tbody');
var $elm$html$Html$thead = _VirtualDom_node('thead');
var $author$project$Ui$ListView$tableView = F3(
	function (title, headers, rows) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('list-table-region')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$p,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('note')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('화면이 좁으면 표 영역을 좌우로 스크롤하세요. 키보드는 표에 초점을 맞춘 뒤 방향키를 사용하세요.')
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('table-wrap list-table-wrap'),
							$elm$html$Html$Attributes$tabindex(0),
							A2($elm$html$Html$Attributes$attribute, 'role', 'region'),
							A2($elm$html$Html$Attributes$attribute, 'aria-label', title + ' 표 · 좌우 스크롤')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$table,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('list-table')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$caption,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(title)
										])),
									A2(
									$elm$html$Html$thead,
									_List_Nil,
									_List_fromArray(
										[
											A2(
											$elm$html$Html$tr,
											_List_Nil,
											A2(
												$elm$core$List$map,
												function (heading) {
													return A2(
														$elm$html$Html$th,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$scope('col')
															]),
														_List_fromArray(
															[
																$elm$html$Html$text(heading)
															]));
												},
												headers))
										])),
									A2($elm$html$Html$tbody, _List_Nil, rows)
								]))
						]))
				]));
	});
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$Page$Activity$view = F4(
	function (mode, state, change, w) {
		var field = F4(
			function (label_, kind, val, update) {
				return A2(
					$elm$html$Html$label,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(label_),
							A2(
							$elm$html$Html$input,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$type_(kind),
									$elm$html$Html$Attributes$value(val),
									$elm$html$Html$Events$onInput(update)
								]),
							_List_Nil)
						]));
			});
		var events = A2($author$project$Ui$Activity$filtered, state, w);
		return A2(
			$elm$html$Html$section,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('panel'),
					$elm$html$Html$Attributes$id('audit-history'),
					$elm$html$Html$Attributes$tabindex(-1)
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$h2,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('조직 활동 기록')
						])),
					$author$project$Ui$Common$note('조직 전체의 변경 이력입니다. 기록 주체는 요청에 기록된 값이며 인증된 신원 증명이 아닙니다. 이름은 현재 정보로 표시하며 원본 ID와 기록 데이터는 상세에서 확인할 수 있습니다.'),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('fields activity-filters')
						]),
					_List_fromArray(
						[
							A4(
							field,
							'활동 검색',
							'search',
							state.query,
							function (v) {
								return change(
									_Utils_update(
										state,
										{query: v}));
							}),
							A2(
							$elm$html$Html$label,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('변경 유형'),
									A2(
									$elm$html$Html$select,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$value(state.kind),
											$elm$html$Html$Events$onInput(
											function (v) {
												return change(
													_Utils_update(
														state,
														{kind: v}));
											})
										]),
									A2(
										$elm$core$List$map,
										function (v) {
											return A2(
												$elm$html$Html$option,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$value(v)
													]),
												_List_fromArray(
													[
														$elm$html$Html$text(
														(v === '') ? '전체 유형' : v)
													]));
										},
										_List_fromArray(
											['', '조직', '진단', '구성원', '목표', '책임', '권한', '에이전트', '결과', '학습', '기타'])))
								])),
							A4(
							field,
							'시작일 (UTC)',
							'date',
							state.from,
							function (v) {
								return change(
									_Utils_update(
										state,
										{from: v}));
							}),
							A4(
							field,
							'종료일 (UTC)',
							'date',
							state.until,
							function (v) {
								return change(
									_Utils_update(
										state,
										{until: v}));
							})
						])),
					((state.from !== '') && ((state.until !== '') && (_Utils_cmp(state.from, state.until) > 0))) ? A2(
					$elm$html$Html$p,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$attribute, 'role', 'alert'),
							$elm$html$Html$Attributes$class('error')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('종료일은 시작일 이후로 선택하세요.')
						])) : $elm$html$Html$text(''),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('actions')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$p,
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$attribute, 'role', 'status')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(
									'검색 결과 ' + ($elm$core$String$fromInt(
										$elm$core$List$length(events)) + ('건' + A2(
										$elm$core$Maybe$withDefault,
										'',
										A2(
											$elm$core$Maybe$map,
											function (_v0) {
												return ' · 선택한 회고의 관련 기록';
											},
											state.review)))))
								])),
							A2(
							$elm$html$Html$button,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$type_('button'),
									$elm$html$Html$Attributes$class('secondary'),
									$elm$html$Html$Events$onClick(
									change($author$project$Ui$Activity$init))
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('필터 초기화')
								]))
						])),
					$elm$core$List$isEmpty(events) ? $author$project$Ui$Common$note('조건에 맞는 활동 기록이 없습니다.') : (_Utils_eq(mode, $author$project$Ui$ListView$Table) ? A3(
					$author$project$Ui$ListView$tableView,
					'조직 활동 기록',
					_List_fromArray(
						['시각 (UTC)', '변경 유형', '대상', '기록 주체', '내용']),
					A2(
						$elm$core$List$map,
						$author$project$Page$Activity$row(w),
						events)) : A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('grid')
						]),
					A2(
						$elm$core$List$map,
						$author$project$Page$Activity$card(w),
						events)))
				]));
	});
var $author$project$ActivityTest$workspace = _Utils_update(
	$author$project$ListViewTest$sample,
	{
		events: _List_fromArray(
			[
				_Utils_update(
				$author$project$ActivityTest$event,
				{at: '2026-09-08T00:00:00Z', seq: 5}),
				$author$project$ActivityTest$event,
				_Utils_update(
				$author$project$ActivityTest$event,
				{at: '2026-09-07T00:00:00Z', seq: 3}),
				_Utils_update(
				$author$project$ActivityTest$event,
				{at: '2026-09-06T23:59:59Z', seq: 2})
			])
	});
var $author$project$ActivityTest$tests = A2(
	$elm_explorations$test$Test$describe,
	'활동 기록',
	_List_fromArray(
		[
			A2(
			$elm_explorations$test$Test$test,
			'현재 이름으로 책임 관계를 설명한다',
			function (_v0) {
				return A2(
					$elm_explorations$test$Expect$equal,
					'매출 개선의 책임자를 김직원으로 지정',
					A2($author$project$Ui$Activity$description, $author$project$ActivityTest$workspace, $author$project$ActivityTest$event));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'삭제되거나 누락된 대상은 ID를 보존한다',
			function (_v1) {
				return A2(
					$elm_explorations$test$Expect$equal,
					'g의 책임자를 p으로 지정',
					A2(
						$author$project$Ui$Activity$description,
						_Utils_update(
							$author$project$ActivityTest$workspace,
							{goals: _List_Nil, people: _List_Nil}),
						$author$project$ActivityTest$event));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'UTC 표시에서 소수 초를 줄이고 시간대를 명시한다',
			function (_v2) {
				return A2(
					$elm_explorations$test$Expect$equal,
					'2026-09-07 23:59:59 UTC',
					$author$project$Ui$Activity$timestamp($author$project$ActivityTest$event.at));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'UTC 하루의 처음과 마지막은 포함하고 다음 날은 제외한다',
			function (_v3) {
				return A2(
					$elm_explorations$test$Expect$equal,
					_List_fromArray(
						[4, 3]),
					A2(
						$elm$core$List$map,
						function ($) {
							return $.seq;
						},
						A2(
							$author$project$Ui$Activity$filtered,
							{from: '2026-09-07', kind: '책임', query: '', review: $elm$core$Maybe$Nothing, until: '2026-09-07'},
							$author$project$ActivityTest$workspace)));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'검색과 유형 및 기간은 함께 적용된다',
			function (_v4) {
				return A2(
					$elm_explorations$test$Expect$equal,
					_List_fromArray(
						[4, 3]),
					A2(
						$elm$core$List$map,
						function ($) {
							return $.seq;
						},
						A2(
							$author$project$Ui$Activity$filtered,
							{from: '2026-09-07', kind: '책임', query: ' 김직원 ', review: $elm$core$Maybe$Nothing, until: '2026-09-07'},
							$author$project$ActivityTest$workspace)));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'유형 불일치는 검색어가 일치해도 제외한다',
			function (_v5) {
				return A2(
					$elm_explorations$test$Expect$equal,
					_List_Nil,
					A2(
						$author$project$Ui$Activity$filtered,
						{from: '', kind: '권한', query: '김직원', review: $elm$core$Maybe$Nothing, until: ''},
						$author$project$ActivityTest$workspace));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'알 수 없는 이벤트 설명은 원문을 유지한다',
			function (_v6) {
				return A2(
					$elm_explorations$test$Expect$equal,
					$author$project$ActivityTest$event.description,
					A2(
						$author$project$Ui$Activity$description,
						$author$project$ActivityTest$workspace,
						_Utils_update(
							$author$project$ActivityTest$event,
							{
								activity: {detail: '', personId: $elm$core$Maybe$Nothing, raw: '{}', reviewId: $elm$core$Maybe$Nothing, tag: 'Future', targetId: '', targetKind: ''}
							})));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'기록 주체는 미인증임을 표시한다',
			function (_v7) {
				return A2(
					$elm_explorations$test$Expect$equal,
					_Utils_Tuple2('김직원 (미인증)', '로컬 운영자 (미인증)'),
					_Utils_Tuple2(
						A2($author$project$Ui$Activity$actorName, $author$project$ActivityTest$workspace, $author$project$ActivityTest$event),
						A2(
							$author$project$Ui$Activity$actorName,
							$author$project$ActivityTest$workspace,
							_Utils_update(
								$author$project$ActivityTest$event,
								{actor: $elm$core$Maybe$Nothing}))));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'회고 필터는 같은 목표의 다른 회고를 제외한다',
			function (_v8) {
				var facts = $author$project$ActivityTest$event.activity;
				var reviews = _Utils_update(
					$author$project$ActivityTest$workspace,
					{
						events: _List_fromArray(
							[
								_Utils_update(
								$author$project$ActivityTest$event,
								{
									activity: _Utils_update(
										facts,
										{
											reviewId: $elm$core$Maybe$Just('r-a')
										})
								}),
								_Utils_update(
								$author$project$ActivityTest$event,
								{
									activity: _Utils_update(
										facts,
										{
											reviewId: $elm$core$Maybe$Just('r-b')
										}),
									seq: 9
								})
							])
					});
				return A2(
					$elm_explorations$test$Expect$equal,
					_List_fromArray(
						[4]),
					A2(
						$elm$core$List$map,
						function ($) {
							return $.seq;
						},
						A2(
							$author$project$Ui$Activity$filtered,
							{
								from: '',
								kind: '',
								query: '',
								review: $elm$core$Maybe$Just('r-a'),
								until: ''
							},
							reviews)));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'회고에서 활동 기록을 열면 이전 검색 필터를 초기화한다',
			function (_v9) {
				return A2(
					$elm_explorations$test$Expect$equal,
					_Utils_Tuple2(
						$author$project$Page$ActivityLog,
						{
							from: '',
							kind: '',
							query: '',
							review: $elm$core$Maybe$Just('r'),
							until: ''
						}),
					function (m) {
						return _Utils_Tuple2(m.pageState.page, m.pageState.activity);
					}(
						A2(
							$author$project$ListViewTest$step,
							$author$project$App$Update$OpenReviewActivity('r'),
							A2(
								$author$project$ListViewTest$step,
								$author$project$App$Update$ActivityChange(
									{from: '2030-01-01', kind: '권한', query: 'none', review: $elm$core$Maybe$Nothing, until: '2030-01-02'}),
								$author$project$ListViewTest$ready))));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'조직 변경은 활동 필터를 격리한다',
			function (_v10) {
				return A2(
					$elm_explorations$test$Expect$equal,
					$author$project$Ui$Activity$init,
					A2(
						$author$project$ListViewTest$step,
						A2(
							$author$project$App$Update$Navigate,
							$author$project$Page$Dashboard,
							$elm$core$Maybe$Just('org-b')),
						A2(
							$author$project$ListViewTest$step,
							$author$project$App$Update$ActivityChange(
								{
									from: '',
									kind: '책임',
									query: 'secret',
									review: $elm$core$Maybe$Just('r'),
									until: ''
								}),
							$author$project$ListViewTest$ready)).pageState.activity);
			}),
			A2(
			$elm_explorations$test$Test$test,
			'활동 표는 원본과 기록 순번을 확인할 상세를 제공한다',
			function (_v11) {
				return A2(
					$elm_explorations$test$Test$Html$Query$has,
					_List_fromArray(
						[
							$elm_explorations$test$Test$Html$Selector$tag('details'),
							$elm_explorations$test$Test$Html$Selector$text($author$project$ActivityTest$event.activity.raw)
						]),
					$elm_explorations$test$Test$Html$Query$fromHtml(
						A4(
							$author$project$Page$Activity$view,
							$author$project$Ui$ListView$Table,
							$author$project$Ui$Activity$init,
							$elm$core$Basics$always(_Utils_Tuple0),
							$author$project$ActivityTest$workspace)));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'가이드 전체 활동 기록 링크는 회고와 검색 필터를 초기화한다',
			function (_v12) {
				return A2(
					$elm_explorations$test$Expect$equal,
					$author$project$Ui$Activity$init,
					A2(
						$author$project$ListViewTest$step,
						A2($author$project$App$Update$Guide, $author$project$Page$ActivityLog, 'audit-history'),
						A2(
							$author$project$ListViewTest$step,
							$author$project$App$Update$OpenReviewActivity('r'),
							$author$project$ListViewTest$ready)).pageState.activity);
			})
		]));
var $author$project$Domain$Agent$Evidence = F2(
	function (a, b) {
		return {$: 'Evidence', a: a, b: b};
	});
var $author$project$App$Update$GotAgents = F3(
	function (a, b, c) {
		return {$: 'GotAgents', a: a, b: b, c: c};
	});
var $author$project$Domain$Agent$Handoff = F3(
	function (a, b, c) {
		return {$: 'Handoff', a: a, b: b, c: c};
	});
var $author$project$App$Update$ImportAgentDrafts = {$: 'ImportAgentDrafts'};
var $author$project$Domain$Agent$Level = F2(
	function (a, b) {
		return {$: 'Level', a: a, b: b};
	});
var $author$project$Domain$Agent$Name = F2(
	function (a, b) {
		return {$: 'Name', a: a, b: b};
	});
var $author$project$Domain$Agent$Remove = function (a) {
	return {$: 'Remove', a: a};
};
var $author$project$App$Update$Saved = F3(
	function (a, b, c) {
		return {$: 'Saved', a: a, b: b, c: c};
	});
var $author$project$Domain$Agent$SetApproval = F2(
	function (a, b) {
		return {$: 'SetApproval', a: a, b: b};
	});
var $author$project$App$Update$SubmitAgents = {$: 'SubmitAgents'};
var $author$project$Domain$Agent$Tools = F2(
	function (a, b) {
		return {$: 'Tools', a: a, b: b};
	});
var $elm_explorations$test$Expect$allHelp = F2(
	function (list, query) {
		allHelp:
		while (true) {
			if (!list.b) {
				return $elm_explorations$test$Expect$pass;
			} else {
				var check = list.a;
				var rest = list.b;
				var _v1 = check(query);
				if (_v1.$ === 'Pass') {
					var $temp$list = rest,
						$temp$query = query;
					list = $temp$list;
					query = $temp$query;
					continue allHelp;
				} else {
					var outcome = _v1;
					return outcome;
				}
			}
		}
	});
var $elm_explorations$test$Expect$all = F2(
	function (list, query) {
		return $elm$core$List$isEmpty(list) ? $elm_explorations$test$Test$Expectation$fail(
			{
				description: 'Expect.all was given an empty list. You must make at least one expectation to have a valid test!',
				reason: $elm_explorations$test$Test$Runner$Failure$Invalid($elm_explorations$test$Test$Runner$Failure$EmptyList)
			}) : A2($elm_explorations$test$Expect$allHelp, list, query);
	});
var $elm_explorations$test$Test$Html$Selector$Internal$Classes = function (a) {
	return {$: 'Classes', a: a};
};
var $elm_explorations$test$Test$Html$Selector$Internal$Invalid = {$: 'Invalid'};
var $elm_explorations$test$Test$Html$Selector$Internal$Style = function (a) {
	return {$: 'Style', a: a};
};
var $elm_explorations$test$Test$Html$Selector$Internal$Attribute = function (a) {
	return {$: 'Attribute', a: a};
};
var $elm_explorations$test$Test$Html$Selector$Internal$namedAttr = F2(
	function (name, value) {
		return $elm_explorations$test$Test$Html$Selector$Internal$Attribute(
			{name: name, value: value});
	});
var $elm_explorations$test$Test$Html$Selector$Internal$BoolAttribute = function (a) {
	return {$: 'BoolAttribute', a: a};
};
var $elm_explorations$test$Test$Html$Selector$Internal$namedBoolAttr = F2(
	function (name, value) {
		return $elm_explorations$test$Test$Html$Selector$Internal$BoolAttribute(
			{name: name, value: value});
	});
var $elm_explorations$test$Test$Html$Selector$orElseLazy = F2(
	function (fma, mb) {
		if (mb.$ === 'Err') {
			return fma(_Utils_Tuple0);
		} else {
			return mb;
		}
	});
var $elm_explorations$test$Test$Html$Internal$Inert$attributeToJson = function (attribute) {
	return _HtmlAsJson_attributeToJson(attribute);
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$Attribute = function (a) {
	return {$: 'Attribute', a: a};
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$AttributeRecord = F2(
	function (key, value) {
		return {key: key, value: value};
	});
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$NamespacedAttribute = function (a) {
	return {$: 'NamespacedAttribute', a: a};
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$NamespacedAttributeRecord = F3(
	function (key, value, namespace) {
		return {key: key, namespace: namespace, value: value};
	});
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$Property = function (a) {
	return {$: 'Property', a: a};
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$PropertyRecord = F2(
	function (key, value) {
		return {key: key, value: value};
	});
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$Style = function (a) {
	return {$: 'Style', a: a};
};
var $elm_explorations$test$Test$Html$Internal$ElmHtml$Constants$propKey = 'a2';
var $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeAttribute = A2(
	$elm$json$Json$Decode$andThen,
	function (tag) {
		return _Utils_eq(tag, $elm_explorations$test$Test$Html$Internal$ElmHtml$Constants$attributeKey) ? A3(
			$elm$json$Json$Decode$map2,
			F2(
				function (key, val) {
					return $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$Attribute(
						A2($elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$AttributeRecord, key, val));
				}),
			A2($elm$json$Json$Decode$field, 'n', $elm$json$Json$Decode$string),
			A2($elm$json$Json$Decode$field, 'o', $elm$json$Json$Decode$string)) : (_Utils_eq(tag, $elm_explorations$test$Test$Html$Internal$ElmHtml$Constants$attributeNamespaceKey) ? A2(
			$elm$json$Json$Decode$map,
			$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$NamespacedAttribute,
			A4(
				$elm$json$Json$Decode$map3,
				$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$NamespacedAttributeRecord,
				A2($elm$json$Json$Decode$field, 'n', $elm$json$Json$Decode$string),
				A2(
					$elm$json$Json$Decode$at,
					_List_fromArray(
						['o', 'o']),
					$elm$json$Json$Decode$string),
				A2(
					$elm$json$Json$Decode$at,
					_List_fromArray(
						['o', 'f']),
					$elm$json$Json$Decode$string))) : (_Utils_eq(tag, $elm_explorations$test$Test$Html$Internal$ElmHtml$Constants$styleKey) ? A3(
			$elm$json$Json$Decode$map2,
			F2(
				function (key, val) {
					return $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$Style(
						{key: key, value: val});
				}),
			A2($elm$json$Json$Decode$field, 'n', $elm$json$Json$Decode$string),
			A2($elm$json$Json$Decode$field, 'o', $elm$json$Json$Decode$string)) : (_Utils_eq(tag, $elm_explorations$test$Test$Html$Internal$ElmHtml$Constants$propKey) ? A3(
			$elm$json$Json$Decode$map2,
			F2(
				function (key, val) {
					return $elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$Property(
						A2($elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$PropertyRecord, key, val));
				}),
			A2($elm$json$Json$Decode$field, 'n', $elm$json$Json$Decode$string),
			A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['o', 'a']),
				$elm$json$Json$Decode$value)) : $elm$json$Json$Decode$fail('Unexpected Html.Attribute tag: ' + tag))));
	},
	A2($elm$json$Json$Decode$field, '$', $elm$json$Json$Decode$string));
var $elm_explorations$test$Test$Html$Internal$Inert$parseAttribute = function (attr) {
	var _v0 = A2(
		$elm$json$Json$Decode$decodeValue,
		$elm_explorations$test$Test$Html$Internal$ElmHtml$InternalTypes$decodeAttribute,
		$elm_explorations$test$Test$Html$Internal$Inert$attributeToJson(attr));
	if (_v0.$ === 'Ok') {
		var parsedAttribute = _v0.a;
		return $elm$core$Result$Ok(parsedAttribute);
	} else {
		var jsonError = _v0.a;
		return $elm$core$Result$Err(
			'Error internally processing Attribute for testing - please report this error message as a bug: ' + $elm$json$Json$Decode$errorToString(jsonError));
	}
};
var $elm$core$Result$withDefault = F2(
	function (def, result) {
		if (result.$ === 'Ok') {
			var a = result.a;
			return a;
		} else {
			return def;
		}
	});
var $elm_explorations$test$Test$Html$Selector$attribute = function (attr) {
	var _v0 = $elm_explorations$test$Test$Html$Internal$Inert$parseAttribute(attr);
	_v0$3:
	while (true) {
		if (_v0.$ === 'Ok') {
			switch (_v0.a.$) {
				case 'Attribute':
					var key = _v0.a.a.key;
					var value = _v0.a.a.value;
					return ($elm$core$String$toLower(key) === 'class') ? $elm_explorations$test$Test$Html$Selector$Internal$Classes(
						A2($elm$core$String$split, ' ', value)) : A2($elm_explorations$test$Test$Html$Selector$Internal$namedAttr, key, value);
				case 'Property':
					var key = _v0.a.a.key;
					var value = _v0.a.a.value;
					return (key === 'className') ? $elm_explorations$test$Test$Html$Selector$Internal$Classes(
						A2(
							$elm$core$Result$withDefault,
							_List_Nil,
							A2(
								$elm$core$Result$map,
								$elm$core$String$split(' '),
								A2($elm$json$Json$Decode$decodeValue, $elm$json$Json$Decode$string, value)))) : A2(
						$elm$core$Result$withDefault,
						$elm_explorations$test$Test$Html$Selector$Internal$Invalid,
						A2(
							$elm_explorations$test$Test$Html$Selector$orElseLazy,
							function (_v1) {
								return A2(
									$elm$core$Result$map,
									$elm_explorations$test$Test$Html$Selector$Internal$namedBoolAttr(key),
									A2($elm$json$Json$Decode$decodeValue, $elm$json$Json$Decode$bool, value));
							},
							A2(
								$elm$core$Result$map,
								$elm_explorations$test$Test$Html$Selector$Internal$namedAttr(key),
								A2($elm$json$Json$Decode$decodeValue, $elm$json$Json$Decode$string, value))));
				case 'Style':
					var key = _v0.a.a.key;
					var value = _v0.a.a.value;
					return $elm_explorations$test$Test$Html$Selector$Internal$Style(
						{key: key, value: value});
				default:
					break _v0$3;
			}
		} else {
			break _v0$3;
		}
	}
	return $elm_explorations$test$Test$Html$Selector$Internal$Invalid;
};
var $author$project$AgentTest$controls = function (state) {
	return {
		busy: false,
		edit: $elm$core$Basics$always(_Utils_Tuple0),
		exportHref: '/api/organizations/org-a/agents/export',
		go: $elm$core$Basics$always(_Utils_Tuple0),
		importDrafts: _Utils_Tuple0,
		org: 'org-a',
		rebase: _Utils_Tuple0,
		reset: _Utils_Tuple0,
		review: $elm$html$Html$text(''),
		save: _Utils_Tuple0,
		state: state
	};
};
var $author$project$Domain$Agent$Snapshot = F5(
	function (version, agents, drafts, diagnostics, draftDiagnostics) {
		return {agents: agents, diagnostics: diagnostics, draftDiagnostics: draftDiagnostics, drafts: drafts, version: version};
	});
var $author$project$Domain$Diagnostic = F5(
	function (severity, code, message, subject, details) {
		return {code: code, details: details, message: message, severity: severity, subject: subject};
	});
var $author$project$Api$Decode$andMap = $elm$json$Json$Decode$map2($elm$core$Basics$apR);
var $author$project$Api$Decode$field = F2(
	function (name, decoder) {
		return $author$project$Api$Decode$andMap(
			A2($elm$json$Json$Decode$field, name, decoder));
	});
var $author$project$Api$Decode$diagnosticDecoder = A3(
	$author$project$Api$Decode$field,
	'details',
	$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
	A3(
		$author$project$Api$Decode$field,
		'subject',
		$elm$json$Json$Decode$string,
		A3(
			$author$project$Api$Decode$field,
			'message',
			$elm$json$Json$Decode$string,
			A3(
				$author$project$Api$Decode$field,
				'code',
				$elm$json$Json$Decode$string,
				A3(
					$author$project$Api$Decode$field,
					'severity',
					$elm$json$Json$Decode$string,
					$elm$json$Json$Decode$succeed($author$project$Domain$Diagnostic))))));
var $author$project$Domain$Agent$Role = function (id) {
	return function (name) {
		return function (sourceWorkflow) {
			return function (task) {
				return function (inputs) {
					return function (outputs) {
						return function (tools) {
							return function (level) {
								return function (approval) {
									return function (handoffTo) {
										return function (status) {
											return function (evidence) {
												return {approval: approval, evidence: evidence, handoffTo: handoffTo, id: id, inputs: inputs, level: level, name: name, outputs: outputs, sourceWorkflow: sourceWorkflow, status: status, task: task, tools: tools};
											};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var $author$project$Api$Agents$approvalDecoder = $elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$json$Json$Decode$map,
			$author$project$Domain$Agent$Person,
			A2($elm$json$Json$Decode$field, 'person', $elm$json$Json$Decode$string)),
			A2(
			$elm$json$Json$Decode$map,
			$author$project$Domain$Agent$Permission,
			A2($elm$json$Json$Decode$field, 'permission', $elm$json$Json$Decode$string))
		]));
var $author$project$Api$Agents$levelDecoder = A2(
	$elm$json$Json$Decode$andThen,
	function (value) {
		return A2(
			$elm$core$List$member,
			value,
			_List_fromArray(
				['L0', 'L1', 'L2', 'L3'])) ? $elm$json$Json$Decode$succeed(value) : $elm$json$Json$Decode$fail('권한 등급을 해석할 수 없습니다.');
	},
	$elm$json$Json$Decode$string);
var $elm$json$Json$Decode$null = _Json_decodeNull;
var $elm$json$Json$Decode$nullable = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				$elm$json$Json$Decode$null($elm$core$Maybe$Nothing),
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder)
			]));
};
var $author$project$Api$Decode$optional = F2(
	function (name, decoder) {
		return $elm$json$Json$Decode$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$json$Json$Decode$field,
					name,
					$elm$json$Json$Decode$nullable(decoder)),
					A2(
					$elm$json$Json$Decode$andThen,
					function (fields) {
						return A2(
							$elm$core$List$any,
							A2(
								$elm$core$Basics$composeR,
								$elm$core$Tuple$first,
								$elm$core$Basics$eq(name)),
							fields) ? $elm$json$Json$Decode$fail('잘못된 필드 형식: ' + name) : $elm$json$Json$Decode$succeed($elm$core$Maybe$Nothing);
					},
					$elm$json$Json$Decode$keyValuePairs($elm$json$Json$Decode$value))
				]));
	});
var $author$project$Api$Agents$roleDecoder = A3(
	$author$project$Api$Decode$field,
	'evidence',
	$elm$json$Json$Decode$string,
	A3(
		$author$project$Api$Decode$field,
		'status',
		$elm$json$Json$Decode$string,
		A3(
			$author$project$Api$Decode$field,
			'handoffTo',
			$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
			A2(
				$author$project$Api$Decode$andMap,
				A2($author$project$Api$Decode$optional, 'approvalBy', $author$project$Api$Agents$approvalDecoder),
				A3(
					$author$project$Api$Decode$field,
					'permissionLevel',
					$author$project$Api$Agents$levelDecoder,
					A3(
						$author$project$Api$Decode$field,
						'tools',
						$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
						A3(
							$author$project$Api$Decode$field,
							'outputs',
							$elm$json$Json$Decode$string,
							A3(
								$author$project$Api$Decode$field,
								'inputs',
								$elm$json$Json$Decode$string,
								A3(
									$author$project$Api$Decode$field,
									'task',
									$elm$json$Json$Decode$string,
									A2(
										$author$project$Api$Decode$andMap,
										A2($author$project$Api$Decode$optional, 'sourceWorkflow', $elm$json$Json$Decode$string),
										A3(
											$author$project$Api$Decode$field,
											'name',
											$elm$json$Json$Decode$string,
											A3(
												$author$project$Api$Decode$field,
												'id',
												$elm$json$Json$Decode$string,
												$elm$json$Json$Decode$succeed($author$project$Domain$Agent$Role)))))))))))));
var $author$project$Api$Agents$decoder = A3(
	$author$project$Api$Decode$field,
	'draftDiagnostics',
	$elm$json$Json$Decode$list($author$project$Api$Decode$diagnosticDecoder),
	A3(
		$author$project$Api$Decode$field,
		'diagnostics',
		$elm$json$Json$Decode$list($author$project$Api$Decode$diagnosticDecoder),
		A3(
			$author$project$Api$Decode$field,
			'drafts',
			$elm$json$Json$Decode$list($author$project$Api$Agents$roleDecoder),
			A3(
				$author$project$Api$Decode$field,
				'agents',
				$elm$json$Json$Decode$list($author$project$Api$Agents$roleDecoder),
				A3(
					$author$project$Api$Decode$field,
					'version',
					$elm$json$Json$Decode$int,
					$elm$json$Json$Decode$succeed($author$project$Domain$Agent$Snapshot))))));
var $author$project$Domain$Discovery$Snapshot = F2(
	function (version, discovery) {
		return {discovery: discovery, version: version};
	});
var $author$project$Domain$Discovery$Document = F5(
	function (scope, asOf, observations, workflows, review) {
		return {asOf: asOf, observations: observations, review: review, scope: scope, workflows: workflows};
	});
var $author$project$Domain$Discovery$Review = F2(
	function (status, note) {
		return {note: note, status: status};
	});
var $author$project$Domain$Discovery$Observation = F5(
	function (id, subject, detail, status, evidence) {
		return {detail: detail, evidence: evidence, id: id, status: status, subject: subject};
	});
var $author$project$Api$Discovery$statusDecoder = A2(
	$elm$json$Json$Decode$andThen,
	function (value) {
		return A2(
			$elm$core$List$member,
			value,
			_List_fromArray(
				['confirmed', 'unknown', 'proposed'])) ? $elm$json$Json$Decode$succeed(value) : $elm$json$Json$Decode$fail('현황 확인 상태를 해석할 수 없습니다.');
	},
	$elm$json$Json$Decode$string);
var $author$project$Api$Discovery$observationDecoder = A3(
	$author$project$Api$Decode$field,
	'evidence',
	$elm$json$Json$Decode$string,
	A3(
		$author$project$Api$Decode$field,
		'status',
		$author$project$Api$Discovery$statusDecoder,
		A3(
			$author$project$Api$Decode$field,
			'detail',
			$elm$json$Json$Decode$string,
			A3(
				$author$project$Api$Decode$field,
				'subject',
				$elm$json$Json$Decode$string,
				A3(
					$author$project$Api$Decode$field,
					'id',
					$elm$json$Json$Decode$string,
					$elm$json$Json$Decode$succeed($author$project$Domain$Discovery$Observation))))));
var $author$project$Api$Discovery$reviewStatusDecoder = A2(
	$elm$json$Json$Decode$andThen,
	function (value) {
		return A2(
			$elm$core$List$member,
			value,
			_List_fromArray(
				['pending', 'reviewed'])) ? $elm$json$Json$Decode$succeed(value) : $elm$json$Json$Decode$fail('검토 상태를 해석할 수 없습니다.');
	},
	$elm$json$Json$Decode$string);
var $author$project$Domain$Discovery$Workflow = function (id) {
	return function (name) {
		return function (role) {
			return function (rolePerson) {
				return function (trigger) {
					return function (inputs) {
						return function (tools) {
							return function (outputs) {
								return function (handoff) {
									return function (handoffWorkflows) {
										return function (approval) {
											return function (approvalPerson) {
												return function (approvalPermission) {
													return function (status) {
														return function (evidence) {
															return {approval: approval, approvalPermission: approvalPermission, approvalPerson: approvalPerson, evidence: evidence, handoff: handoff, handoffWorkflows: handoffWorkflows, id: id, inputs: inputs, name: name, outputs: outputs, role: role, rolePerson: rolePerson, status: status, tools: tools, trigger: trigger};
														};
													};
												};
											};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var $author$project$Api$Discovery$workflowDecoder = A3(
	$author$project$Api$Decode$field,
	'evidence',
	$elm$json$Json$Decode$string,
	A3(
		$author$project$Api$Decode$field,
		'status',
		$author$project$Api$Discovery$statusDecoder,
		A2(
			$author$project$Api$Decode$andMap,
			A2($author$project$Api$Decode$optional, 'approvalPermission', $elm$json$Json$Decode$string),
			A2(
				$author$project$Api$Decode$andMap,
				A2($author$project$Api$Decode$optional, 'approvalPerson', $elm$json$Json$Decode$string),
				A3(
					$author$project$Api$Decode$field,
					'approval',
					$elm$json$Json$Decode$string,
					A2(
						$author$project$Api$Decode$andMap,
						A2(
							$elm$json$Json$Decode$map,
							$elm$core$Maybe$withDefault(_List_Nil),
							A2(
								$author$project$Api$Decode$optional,
								'handoffWorkflows',
								$elm$json$Json$Decode$list($elm$json$Json$Decode$string))),
						A3(
							$author$project$Api$Decode$field,
							'handoff',
							$elm$json$Json$Decode$string,
							A3(
								$author$project$Api$Decode$field,
								'outputs',
								$elm$json$Json$Decode$string,
								A3(
									$author$project$Api$Decode$field,
									'tools',
									$elm$json$Json$Decode$string,
									A3(
										$author$project$Api$Decode$field,
										'inputs',
										$elm$json$Json$Decode$string,
										A3(
											$author$project$Api$Decode$field,
											'trigger',
											$elm$json$Json$Decode$string,
											A2(
												$author$project$Api$Decode$andMap,
												A2($author$project$Api$Decode$optional, 'rolePerson', $elm$json$Json$Decode$string),
												A3(
													$author$project$Api$Decode$field,
													'role',
													$elm$json$Json$Decode$string,
													A3(
														$author$project$Api$Decode$field,
														'name',
														$elm$json$Json$Decode$string,
														A3(
															$author$project$Api$Decode$field,
															'id',
															$elm$json$Json$Decode$string,
															$elm$json$Json$Decode$succeed($author$project$Domain$Discovery$Workflow))))))))))))))));
var $author$project$Api$Discovery$documentDecoder = A3(
	$author$project$Api$Decode$field,
	'review',
	A3(
		$elm$json$Json$Decode$map2,
		$author$project$Domain$Discovery$Review,
		A2($elm$json$Json$Decode$field, 'status', $author$project$Api$Discovery$reviewStatusDecoder),
		A2($elm$json$Json$Decode$field, 'note', $elm$json$Json$Decode$string)),
	A3(
		$author$project$Api$Decode$field,
		'workflows',
		$elm$json$Json$Decode$list($author$project$Api$Discovery$workflowDecoder),
		A3(
			$author$project$Api$Decode$field,
			'observations',
			$elm$json$Json$Decode$list($author$project$Api$Discovery$observationDecoder),
			A3(
				$author$project$Api$Decode$field,
				'asOf',
				$elm$json$Json$Decode$string,
				A3(
					$author$project$Api$Decode$field,
					'scope',
					$elm$json$Json$Decode$string,
					$elm$json$Json$Decode$succeed($author$project$Domain$Discovery$Document))))));
var $author$project$Api$Discovery$decoder = A3(
	$elm$json$Json$Decode$map2,
	$author$project$Domain$Discovery$Snapshot,
	A2($elm$json$Json$Decode$field, 'version', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'discovery', $author$project$Api$Discovery$documentDecoder));
var $author$project$Domain$Discovery$empty = {
	asOf: '',
	observations: _List_Nil,
	review: {note: '', status: 'pending'},
	scope: '',
	workflows: _List_Nil
};
var $author$project$Api$Agents$encodeRole = function (role) {
	return $elm$json$Json$Encode$object(
		_Utils_ap(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'id',
					$elm$json$Json$Encode$string(role.id)),
					_Utils_Tuple2(
					'name',
					$elm$json$Json$Encode$string(role.name)),
					_Utils_Tuple2(
					'task',
					$elm$json$Json$Encode$string(role.task)),
					_Utils_Tuple2(
					'inputs',
					$elm$json$Json$Encode$string(role.inputs)),
					_Utils_Tuple2(
					'outputs',
					$elm$json$Json$Encode$string(role.outputs)),
					_Utils_Tuple2(
					'tools',
					A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, role.tools)),
					_Utils_Tuple2(
					'permissionLevel',
					$elm$json$Json$Encode$string(role.level)),
					_Utils_Tuple2(
					'handoffTo',
					A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, role.handoffTo)),
					_Utils_Tuple2(
					'status',
					$elm$json$Json$Encode$string(role.status)),
					_Utils_Tuple2(
					'evidence',
					$elm$json$Json$Encode$string(role.evidence))
				]),
			A2(
				$elm$core$List$filterMap,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						A2(
						$elm$core$Maybe$map,
						function (v) {
							return _Utils_Tuple2(
								'sourceWorkflow',
								$elm$json$Json$Encode$string(v));
						},
						role.sourceWorkflow),
						A2(
						$elm$core$Maybe$map,
						function (approval) {
							return _Utils_Tuple2(
								'approvalBy',
								function () {
									if (approval.$ === 'Person') {
										var uid = approval.a;
										return $elm$json$Json$Encode$object(
											_List_fromArray(
												[
													_Utils_Tuple2(
													'person',
													$elm$json$Json$Encode$string(uid))
												]));
									} else {
										var permission = approval.a;
										return $elm$json$Json$Encode$object(
											_List_fromArray(
												[
													_Utils_Tuple2(
													'permission',
													$elm$json$Json$Encode$string(permission))
												]));
									}
								}());
						},
						role.approval)
					]))));
};
var $author$project$Api$Agents$encode = $elm$json$Json$Encode$list($author$project$Api$Agents$encodeRole);
var $elm$core$Tuple$mapSecond = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			x,
			func(y));
	});
var $author$project$Api$Discovery$encodeWorkflow = function (w) {
	return $elm$json$Json$Encode$object(
		_Utils_ap(
			A2(
				$elm$core$List$map,
				$elm$core$Tuple$mapSecond($elm$json$Json$Encode$string),
				_List_fromArray(
					[
						_Utils_Tuple2('id', w.id),
						_Utils_Tuple2('name', w.name),
						_Utils_Tuple2('role', w.role),
						_Utils_Tuple2('trigger', w.trigger),
						_Utils_Tuple2('inputs', w.inputs),
						_Utils_Tuple2('tools', w.tools),
						_Utils_Tuple2('outputs', w.outputs),
						_Utils_Tuple2('handoff', w.handoff),
						_Utils_Tuple2('approval', w.approval),
						_Utils_Tuple2('status', w.status),
						_Utils_Tuple2('evidence', w.evidence)
					])),
			A2(
				$elm$core$List$filterMap,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						A2(
						$elm$core$Maybe$map,
						function (v) {
							return _Utils_Tuple2(
								'rolePerson',
								$elm$json$Json$Encode$string(v));
						},
						w.rolePerson),
						A2(
						$elm$core$Maybe$map,
						function (v) {
							return _Utils_Tuple2(
								'approvalPerson',
								$elm$json$Json$Encode$string(v));
						},
						w.approvalPerson),
						A2(
						$elm$core$Maybe$map,
						function (v) {
							return _Utils_Tuple2(
								'approvalPermission',
								$elm$json$Json$Encode$string(v));
						},
						w.approvalPermission),
						$elm$core$List$isEmpty(w.handoffWorkflows) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
						_Utils_Tuple2(
							'handoffWorkflows',
							A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, w.handoffWorkflows)))
					]))));
};
var $author$project$Api$Discovery$strings = function (pairs) {
	return $elm$json$Json$Encode$object(
		A2(
			$elm$core$List$map,
			$elm$core$Tuple$mapSecond($elm$json$Json$Encode$string),
			pairs));
};
var $author$project$Api$Discovery$encode = function (doc) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'scope',
				$elm$json$Json$Encode$string(doc.scope)),
				_Utils_Tuple2(
				'asOf',
				$elm$json$Json$Encode$string(doc.asOf)),
				_Utils_Tuple2(
				'observations',
				A2(
					$elm$json$Json$Encode$list,
					function (o) {
						return $author$project$Api$Discovery$strings(
							_List_fromArray(
								[
									_Utils_Tuple2('id', o.id),
									_Utils_Tuple2('subject', o.subject),
									_Utils_Tuple2('detail', o.detail),
									_Utils_Tuple2('status', o.status),
									_Utils_Tuple2('evidence', o.evidence)
								]));
					},
					doc.observations)),
				_Utils_Tuple2(
				'workflows',
				A2($elm$json$Json$Encode$list, $author$project$Api$Discovery$encodeWorkflow, doc.workflows)),
				_Utils_Tuple2(
				'review',
				$author$project$Api$Discovery$strings(
					_List_fromArray(
						[
							_Utils_Tuple2('status', doc.review.status),
							_Utils_Tuple2('note', doc.review.note)
						])))
			]));
};
var $elm_explorations$test$Test$Html$Query$Internal$Find = function (a) {
	return {$: 'Find', a: a};
};
var $elm_explorations$test$Test$Html$Query$Internal$prependSelector = F2(
	function (query, selector) {
		switch (query.$) {
			case 'Query':
				var node = query.a;
				var selectors = query.b;
				return A2(
					$elm_explorations$test$Test$Html$Query$Internal$Query,
					node,
					A2($elm$core$List$cons, selector, selectors));
			case 'InternalError':
				var message = query.a;
				return $elm_explorations$test$Test$Html$Query$Internal$InternalError(message);
			default:
				var validations = query.a;
				return $elm_explorations$test$Test$Html$Query$Internal$ValidationErrors(validations);
		}
	});
var $elm_explorations$test$Test$Html$Query$find = F2(
	function (selectors, _v0) {
		var showTrace = _v0.a;
		var query = _v0.b;
		return A2(
			$elm_explorations$test$Test$Html$Query$Internal$Single,
			showTrace,
			A2(
				$elm_explorations$test$Test$Html$Query$Internal$prependSelector,
				query,
				$elm_explorations$test$Test$Html$Query$Internal$Find(selectors)));
	});
var $elm$html$Html$Attributes$href = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var $elm$json$Json$Decode$index = _Json_decodeIndex;
var $author$project$AgentTest$role = function (ident) {
	return {
		approval: $elm$core$Maybe$Nothing,
		evidence: '인터뷰',
		handoffTo: _List_Nil,
		id: ident,
		inputs: '입력',
		level: 'L1',
		name: '역할 ' + ident,
		outputs: '산출물',
		sourceWorkflow: $elm$core$Maybe$Just('w-' + ident),
		status: 'confirmed',
		task: '업무 ' + ident,
		tools: _List_fromArray(
			['CRM'])
	};
};
var $author$project$AgentTest$intake = function () {
	var base = $author$project$AgentTest$role('intake');
	return _Utils_update(
		base,
		{
			handoffTo: _List_fromArray(
				['refund'])
		});
}();
var $author$project$Ui$AgentGraph$layers = function (roles) {
	var step = F3(
		function (remaining, counts, acc) {
			step:
			while (true) {
				if ($elm$core$List$isEmpty(remaining)) {
					return $elm$core$List$reverse(acc);
				} else {
					var ready = A2(
						$elm$core$List$filter,
						function (role) {
							return _Utils_eq(
								A2($elm$core$Dict$get, role.id, counts),
								$elm$core$Maybe$Nothing) || _Utils_eq(
								A2($elm$core$Dict$get, role.id, counts),
								$elm$core$Maybe$Just(0));
						},
						remaining);
					if ($elm$core$List$isEmpty(ready)) {
						return $elm$core$List$reverse(
							A2($elm$core$List$cons, remaining, acc));
					} else {
						var released = A3(
							$elm$core$List$foldl,
							F2(
								function (role, c) {
									return A3(
										$elm$core$List$foldl,
										function (target) {
											return A2(
												$elm$core$Dict$update,
												target,
												$elm$core$Maybe$map(
													function (n) {
														return n - 1;
													}));
										},
										c,
										role.handoffTo);
								}),
							counts,
							ready);
						var readyIds = $elm$core$Set$fromList(
							A2(
								$elm$core$List$map,
								function ($) {
									return $.id;
								},
								ready));
						var next = A2(
							$elm$core$List$filter,
							function (role) {
								return !A2($elm$core$Set$member, role.id, readyIds);
							},
							remaining);
						var $temp$remaining = next,
							$temp$counts = released,
							$temp$acc = A2($elm$core$List$cons, ready, acc);
						remaining = $temp$remaining;
						counts = $temp$counts;
						acc = $temp$acc;
						continue step;
					}
				}
			}
		});
	var ids = $elm$core$Set$fromList(
		A2(
			$elm$core$List$map,
			function ($) {
				return $.id;
			},
			roles));
	var incoming = A3(
		$elm$core$List$foldl,
		F2(
			function (role, acc) {
				return A3(
					$elm$core$List$foldl,
					function (target) {
						return A2(
							$elm$core$Dict$update,
							target,
							A2(
								$elm$core$Basics$composeR,
								$elm$core$Maybe$withDefault(0),
								A2(
									$elm$core$Basics$composeR,
									$elm$core$Basics$add(1),
									$elm$core$Maybe$Just)));
					},
					acc,
					A2(
						$elm$core$List$filter,
						function (t) {
							return A2($elm$core$Set$member, t, ids);
						},
						role.handoffTo));
			}),
		$elm$core$Dict$empty,
		roles);
	var byId = $elm$core$Dict$fromList(
		A2(
			$elm$core$List$map,
			function (role) {
				return _Utils_Tuple2(role.id, role);
			},
			roles));
	return A3(
		step,
		A2(
			$elm$core$List$filterMap,
			function (role) {
				return A2($elm$core$Dict$get, role.id, byId);
			},
			roles),
		incoming,
		_List_Nil);
};
var $author$project$Page$AgentDrafts = {$: 'AgentDrafts'};
var $author$project$AgentTest$workspace = {
	authorities: _List_Nil,
	compiler: {diagnostics: _List_Nil, errors: 0, warnings: 0},
	decisionShare: $elm$core$Dict$empty,
	demo: false,
	edges: _List_Nil,
	events: _List_Nil,
	goals: _List_Nil,
	organization: {createdAt: '2026-01-01T00:00:00Z', id: 'org-a', name: 'Alpha'},
	people: _List_fromArray(
		[
			{active: true, department: $elm$core$Maybe$Nothing, email: $elm$core$Maybe$Nothing, id: 'lead', name: '팀장', reportsTo: $elm$core$Maybe$Nothing, role: '고객지원 팀장'}
		]),
	reviewWarnings: _List_Nil,
	reviews: _List_Nil,
	version: 4
};
var $author$project$AgentTest$ready = A2(
	$author$project$AppFixture$mapPage,
	function (p) {
		return _Utils_update(
			p,
			{page: $author$project$Page$AgentDrafts});
	},
	A2(
		$author$project$AppFixture$mapSession,
		function (s) {
			return _Utils_update(
				s,
				{
					fresh: true,
					org: $elm$core$Maybe$Just('org-a'),
					syncing: false,
					workspace: $author$project$Remote$Loaded($author$project$AgentTest$workspace)
				});
		},
		$author$project$App$Update$init(
			{deadline: '2026-12-31', seed: 'test', today: '2026-01-01'}).a));
var $author$project$AgentTest$refund = function () {
	var base = $author$project$AgentTest$role('refund');
	return _Utils_update(
		base,
		{
			approval: $elm$core$Maybe$Just(
				$author$project$Domain$Agent$Person('lead')),
			level: 'L2'
		});
}();
var $author$project$AgentTest$snapshot = {
	agents: _List_Nil,
	diagnostics: _List_Nil,
	draftDiagnostics: _List_fromArray(
		[
			{code: 'A009', details: _List_Nil, message: '산출물의 인계 대상이 미확인입니다.', severity: 'Info', subject: 'refund'}
		]),
	drafts: _List_fromArray(
		[$author$project$AgentTest$intake, $author$project$AgentTest$refund]),
	version: 11
};
var $author$project$AgentTest$step = F2(
	function (msg, model) {
		return A2($author$project$App$Update$update, msg, model).a;
	});
var $author$project$Page$AgentGraph = {$: 'AgentGraph'};
var $elm$html$Html$a = _VirtualDom_node('a');
var $author$project$App$Agents$changed = F2(
	function (org, state) {
		return !_Utils_eq(
			A2(
				$elm$core$Maybe$map,
				function ($) {
					return $.agents;
				},
				A2($author$project$App$Agents$current, org, state)),
			A2(
				$elm$core$Maybe$map,
				function ($) {
					return $.agents;
				},
				A2($author$project$App$Agents$saved, org, state)));
	});
var $author$project$Domain$Agent$Inputs = F2(
	function (a, b) {
		return {$: 'Inputs', a: a, b: b};
	});
var $author$project$Domain$Agent$Outputs = F2(
	function (a, b) {
		return {$: 'Outputs', a: a, b: b};
	});
var $author$project$Domain$Agent$Status = F2(
	function (a, b) {
		return {$: 'Status', a: a, b: b};
	});
var $author$project$Domain$Agent$approvalKey = function (approval) {
	if (approval.$ === 'Just') {
		if (approval.a.$ === 'Person') {
			var uid = approval.a.a;
			return 'person:' + uid;
		} else {
			var permission = approval.a.a;
			return 'permission:' + permission;
		}
	} else {
		return '';
	}
};
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$checked = $elm$html$Html$Attributes$boolProperty('checked');
var $elm$html$Html$fieldset = _VirtualDom_node('fieldset');
var $author$project$Ui$Form$fieldError = F3(
	function (kind, required_, current) {
		return (required_ && ($elm$core$String$trim(current) === '')) ? '필수 항목입니다. 내용을 입력하세요.' : (((kind === 'number') && ((current !== '') && _Utils_eq(
			$elm$core$String$toFloat(current),
			$elm$core$Maybe$Nothing))) ? '숫자로 입력하세요.' : '');
	});
var $elm$html$Html$Attributes$for = $elm$html$Html$Attributes$stringProperty('htmlFor');
var $elm$html$Html$Attributes$name = $elm$html$Html$Attributes$stringProperty('name');
var $elm$html$Html$Attributes$required = $elm$html$Html$Attributes$boolProperty('required');
var $elm$html$Html$Attributes$rows = function (n) {
	return A2(
		_VirtualDom_attribute,
		'rows',
		$elm$core$String$fromInt(n));
};
var $elm$html$Html$small = _VirtualDom_node('small');
var $elm$html$Html$textarea = _VirtualDom_node('textarea');
var $author$project$Ui$Form$guidedArea = F6(
	function (key, title, hint, required_, current, edit) {
		var error = A3($author$project$Ui$Form$fieldError, 'text', required_, current);
		return A2(
			$elm$html$Html$label,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$for(key),
					$elm$html$Html$Attributes$class('guided-field')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$span,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							_Utils_ap(
								title,
								required_ ? ' · 필수' : ''))
						])),
					A2(
					$elm$html$Html$textarea,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id(key),
							$elm$html$Html$Attributes$name(key),
							$elm$html$Html$Attributes$value(current),
							$elm$html$Html$Events$onInput(edit),
							$elm$html$Html$Attributes$required(required_),
							$elm$html$Html$Attributes$rows(3),
							A2($elm$html$Html$Attributes$attribute, 'aria-describedby', key + ('-help ' + (key + '-error'))),
							A2(
							$elm$html$Html$Attributes$attribute,
							'aria-invalid',
							(error === '') ? 'false' : 'true')
						]),
					_List_Nil),
					A2(
					$elm$html$Html$small,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id(key + '-help'),
							$elm$html$Html$Attributes$class('field-help')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(hint)
						])),
					A2(
					$elm$html$Html$small,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id(key + '-error'),
							$elm$html$Html$Attributes$class('field-error'),
							A2($elm$html$Html$Attributes$attribute, 'aria-live', 'polite')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(error)
						]))
				]));
	});
var $elm$html$Html$Attributes$autocomplete = function (bool) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'autocomplete',
		bool ? 'on' : 'off');
};
var $elm$html$Html$Attributes$step = function (n) {
	return A2($elm$html$Html$Attributes$stringProperty, 'step', n);
};
var $author$project$Ui$Form$guidedInputNamed = F8(
	function (key, name_, title, hint, kind, required_, current, edit) {
		var error = A3($author$project$Ui$Form$fieldError, kind, required_, current);
		return A2(
			$elm$html$Html$label,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$for(key),
					$elm$html$Html$Attributes$class('guided-field')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$span,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							_Utils_ap(
								title,
								required_ ? ' · 필수' : ''))
						])),
					A2(
					$elm$html$Html$input,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id(key),
							$elm$html$Html$Attributes$name(key),
							$elm$html$Html$Attributes$type_(kind),
							$elm$html$Html$Attributes$value(current),
							$elm$html$Html$Events$onInput(edit),
							$elm$html$Html$Attributes$required(required_),
							$elm$html$Html$Attributes$step('any'),
							$elm$html$Html$Attributes$autocomplete(false),
							A2($elm$html$Html$Attributes$attribute, 'aria-describedby', key + ('-help ' + (key + '-error'))),
							A2(
							$elm$html$Html$Attributes$attribute,
							'aria-invalid',
							(error === '') ? 'false' : 'true')
						]),
					_List_Nil),
					A2(
					$elm$html$Html$small,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id(key + '-help'),
							$elm$html$Html$Attributes$class('field-help')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(hint)
						])),
					A2(
					$elm$html$Html$small,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id(key + '-error'),
							$elm$html$Html$Attributes$class('field-error'),
							A2($elm$html$Html$Attributes$attribute, 'aria-live', 'polite')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(error)
						]))
				]));
	});
var $author$project$Ui$Form$guidedInput = F7(
	function (key, title, hint, kind, required_, current, edit) {
		return A8($author$project$Ui$Form$guidedInputNamed, key, key, title, hint, kind, required_, current, edit);
	});
var $author$project$Page$Agents$known = function (value_) {
	return ($elm$core$String$trim(value_) === '') ? '미확인 · 확인 후 입력' : value_;
};
var $elm$html$Html$legend = _VirtualDom_node('legend');
var $author$project$Domain$Agent$levels = _List_fromArray(
	[
		_Utils_Tuple2('L0', 'L0 읽기'),
		_Utils_Tuple2('L1', 'L1 작업 공간 쓰기'),
		_Utils_Tuple2('L2', 'L2 외부 영향 · 사람 승인 필요'),
		_Utils_Tuple2('L3', 'L3 금지 · 사람이 직접 수행')
	]);
var $elm$html$Html$Events$targetChecked = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'checked']),
	$elm$json$Json$Decode$bool);
var $elm$html$Html$Events$onCheck = function (tagger) {
	return A2(
		$elm$html$Html$Events$on,
		'change',
		A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetChecked));
};
var $author$project$Ui$Form$peopleOptions = function (w) {
	return A2(
		$elm$core$List$cons,
		_Utils_Tuple2('', '구성원 선택'),
		A2(
			$elm$core$List$map,
			function (p) {
				return _Utils_Tuple2(p.id, p.name + (' · ' + p.role));
			},
			A2(
				$elm$core$List$filter,
				function ($) {
					return $.active;
				},
				w.people)));
};
var $author$project$Ui$Label$permissions = _List_fromArray(
	[
		_Utils_Tuple2('Pricing', '가격 결정'),
		_Utils_Tuple2('Hiring', '채용'),
		_Utils_Tuple2('BudgetApproval', '예산 승인'),
		_Utils_Tuple2('Contracting', '계약'),
		_Utils_Tuple2('Marketing', '마케팅'),
		_Utils_Tuple2('Infrastructure', '인프라'),
		_Utils_Tuple2('ProductLaunch', '제품 출시')
	]);
var $author$project$Ui$Form$help = function (key) {
	switch (key) {
		case 'name':
			return '실제 조직 또는 구성원의 이름을 입력하세요.';
		case 'role':
			return '현재 맡은 역할을 적으세요. 예: 고객지원 운영 책임자';
		case 'department':
			return '현재 소속된 부서를 적으세요. 확인 전이면 비워 두어도 됩니다.';
		case 'reportsTo':
			return '현재 업무 보고를 받는 사람입니다. 먼저 구성원을 등록한 뒤 연결할 수 있습니다.';
		case 'owner':
			return '함께 일하는 모두가 아니라 결과에 대해 최종 판단과 설명을 맡는 한 명입니다.';
		case 'description':
			return '현재 관리 중인 결과를 적으세요. 예: 고객 문의 응답 시간 단축';
		case 'metricName':
			return '결과를 확인하는 지표입니다. 예: 평균 첫 응답 시간';
		case 'unit':
			return '숫자의 단위를 적으세요. 예: 분, 건, %, 원';
		case 'baseline':
			return '측정 시작 시점의 값입니다. 예: 평균 첫 응답 60분';
		case 'target':
			return '마감까지 도달하려는 값입니다. 예: 평균 첫 응답 30분';
		case 'direction':
			return '매출은 높을수록, 응답 시간은 낮을수록 좋은 지표입니다.';
		case 'startsAt':
			return '측정이 시작되는 날짜입니다. 날짜는 UTC 기준으로 저장합니다.';
		case 'deadline':
			return '목표 달성 기한입니다. 시작일보다 앞설 수 없습니다.';
		case 'budget':
			return '원(KRW) 단위로 숫자만 입력하세요. 0은 예산 없음이며 미확인과 다릅니다.';
		case 'parent':
			return '이 목표가 기여하는 상위 목표입니다. 지표가 자동 합산되지는 않습니다.';
		case 'reportedBy':
			return '실제 측정값을 확인하고 보고한 구성원입니다.';
		case 'value':
			return '목표에 표시된 KPI 단위로 실제 측정한 수치를 입력하세요.';
		case 'note':
			return '확인한 내용과 근거를 적으세요. 예: 9월 고객지원 보고서에서 확인';
		case 'decision':
			return '학습을 바탕으로 바꿀 행동입니다. 예: 긴급 문의는 당일 담당자에게 전달';
		case 'decisionOwner':
			return '다음 결정을 실행하고 완료 여부를 확인할 사람입니다.';
		case 'decisionDeadline':
			return '결정 실행 기한입니다. 날짜는 UTC 기준입니다.';
		case 'email':
			return '연락처를 참고하기 위한 선택 정보입니다. 예: member@example.com';
		default:
			return '';
	}
};
var $elm$html$Html$Attributes$selected = $elm$html$Html$Attributes$boolProperty('selected');
var $author$project$Ui$Form$selectWithHelp = F7(
	function (key, name_, current, edit, title, required_, options) {
		return A2(
			$elm$html$Html$label,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$for(key),
					$elm$html$Html$Attributes$class('guided-field')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$span,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(title)
						])),
					A2(
					$elm$html$Html$select,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id(key),
							$elm$html$Html$Attributes$name(name_),
							$elm$html$Html$Attributes$value(current),
							$elm$html$Html$Events$onInput(edit),
							$elm$html$Html$Attributes$required(required_),
							A2($elm$html$Html$Attributes$attribute, 'aria-describedby', key + '-help')
						]),
					A2(
						$elm$core$List$map,
						function (_v0) {
							var ident = _v0.a;
							var label_ = _v0.b;
							return A2(
								$elm$html$Html$option,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$value(ident),
										$elm$html$Html$Attributes$selected(
										_Utils_eq(current, ident))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(label_)
									]));
						},
						options)),
					A2(
					$elm$html$Html$small,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id(key + '-help'),
							$elm$html$Html$Attributes$class('field-help')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							$author$project$Ui$Form$help(name_))
						]))
				]));
	});
var $author$project$Ui$Form$selectValue = F6(
	function (key, current, edit, label_, required_, options) {
		return A7($author$project$Ui$Form$selectWithHelp, key, key, current, edit, label_, required_, options);
	});
var $author$project$Domain$Agent$toolsText = $elm$core$String$join(', ');
var $author$project$Page$Agents$designCard = F4(
	function (controls, workspace, roles, role) {
		var others = A2(
			$elm$core$List$filter,
			function (r) {
				return !_Utils_eq(r.id, role.id);
			},
			roles);
		var key = function (suffix) {
			return 'agent-' + (role.id + ('-' + suffix));
		};
		var approvalOptions = A2(
			$elm$core$List$cons,
			_Utils_Tuple2('', '없음 또는 미확인'),
			_Utils_ap(
				A2(
					$elm$core$List$map,
					function (_v0) {
						var uid = _v0.a;
						var label_ = _v0.b;
						return _Utils_Tuple2('person:' + uid, '구성원 · ' + label_);
					},
					A2(
						$elm$core$List$drop,
						1,
						$author$project$Ui$Form$peopleOptions(workspace))),
				A2(
					$elm$core$List$map,
					function (_v1) {
						var permission = _v1.a;
						var label_ = _v1.b;
						return _Utils_Tuple2('permission:' + permission, '권한 보유자 · ' + label_);
					},
					$author$project$Ui$Label$permissions)));
		return A2(
			$elm$html$Html$section,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('panel agent-card discovery-item'),
					$elm$html$Html$Attributes$id('design-' + role.id)
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$h2,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							$author$project$Page$Agents$known(role.name))
						])),
					$author$project$Ui$Common$note(
					'도출 근거 업무: ' + (A2($elm$core$Maybe$withDefault, '없음', role.sourceWorkflow) + (' · 담당 업무: ' + role.task))),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('fields')
						]),
					_List_fromArray(
						[
							A7(
							$author$project$Ui$Form$guidedInput,
							key('name'),
							'역할 이름',
							'에이전트가 맡는 책임을 이름으로 적으세요.',
							'text',
							true,
							role.name,
							A2(
								$elm$core$Basics$composeL,
								controls.edit,
								$author$project$Domain$Agent$Name(role.id))),
							A6(
							$author$project$Ui$Form$selectValue,
							key('level'),
							role.level,
							A2(
								$elm$core$Basics$composeL,
								controls.edit,
								$author$project$Domain$Agent$Level(role.id)),
							'권한 등급',
							true,
							$author$project$Domain$Agent$levels),
							A6(
							$author$project$Ui$Form$guidedArea,
							key('inputs'),
							'입력',
							'이 역할이 받는 정보',
							false,
							role.inputs,
							A2(
								$elm$core$Basics$composeL,
								controls.edit,
								$author$project$Domain$Agent$Inputs(role.id))),
							A6(
							$author$project$Ui$Form$guidedArea,
							key('outputs'),
							'산출물',
							'이 역할이 만드는 결과물',
							false,
							role.outputs,
							A2(
								$elm$core$Basics$composeL,
								controls.edit,
								$author$project$Domain$Agent$Outputs(role.id))),
							A6(
							$author$project$Ui$Form$guidedArea,
							key('tools'),
							'허용 도구 후보',
							'쉼표로 구분합니다. 실제 접근 권한은 별도로 부여합니다.',
							false,
							$author$project$Domain$Agent$toolsText(role.tools),
							A2(
								$elm$core$Basics$composeL,
								controls.edit,
								$author$project$Domain$Agent$Tools(role.id))),
							A6(
							$author$project$Ui$Form$selectValue,
							key('approval'),
							$author$project$Domain$Agent$approvalKey(role.approval),
							A2(
								$elm$core$Basics$composeL,
								controls.edit,
								$author$project$Domain$Agent$SetApproval(role.id)),
							'사람 승인 주체',
							false,
							approvalOptions)
						])),
					$elm$core$List$isEmpty(others) ? $author$project$Ui$Common$note('인계 대상으로 연결할 다른 역할이 없습니다.') : A2(
					$elm$html$Html$fieldset,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('permission-fields')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$legend,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('인계 대상 역할')
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('checks')
								]),
							A2(
								$elm$core$List$map,
								function (other) {
									return A2(
										$elm$html$Html$label,
										_List_Nil,
										_List_fromArray(
											[
												A2(
												$elm$html$Html$input,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$type_('checkbox'),
														$elm$html$Html$Attributes$checked(
														A2($elm$core$List$member, other.id, role.handoffTo)),
														$elm$html$Html$Events$onCheck(
														A2(
															$elm$core$Basics$composeL,
															controls.edit,
															A2($author$project$Domain$Agent$Handoff, role.id, other.id)))
													]),
												_List_Nil),
												$elm$html$Html$text(
												$author$project$Page$Agents$known(other.name))
											]));
								},
								others))
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('fields')
						]),
					_List_fromArray(
						[
							A6(
							$author$project$Ui$Form$selectValue,
							key('status'),
							role.status,
							A2(
								$elm$core$Basics$composeL,
								controls.edit,
								$author$project$Domain$Agent$Status(role.id)),
							'정보 구분',
							true,
							_List_fromArray(
								[
									_Utils_Tuple2('unknown', '미확인'),
									_Utils_Tuple2('confirmed', '확인된 사실'),
									_Utils_Tuple2('proposed', '개선안')
								])),
							A6(
							$author$project$Ui$Form$guidedArea,
							key('evidence'),
							'근거',
							'확인된 사실이면 근거가 필요합니다.',
							role.status === 'confirmed',
							role.evidence,
							A2(
								$elm$core$Basics$composeL,
								controls.edit,
								$author$project$Domain$Agent$Evidence(role.id)))
						])),
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$type_('button'),
							$elm$html$Html$Attributes$class('secondary'),
							$elm$html$Html$Events$onClick(
							controls.edit(
								$author$project$Domain$Agent$Remove(role.id)))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('이 역할 제외 · 저장 전 취소 가능')
						]))
				]));
	});
var $elm$html$Html$Attributes$classList = function (classes) {
	return $elm$html$Html$Attributes$class(
		A2(
			$elm$core$String$join,
			' ',
			A2(
				$elm$core$List$map,
				$elm$core$Tuple$first,
				A2($elm$core$List$filter, $elm$core$Tuple$second, classes))));
};
var $author$project$Ui$Common$panel = F2(
	function (title, children) {
		return A2(
			$elm$html$Html$section,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('panel')
				]),
			A2(
				$elm$core$List$cons,
				A2(
					$elm$html$Html$h2,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(title)
						])),
				children));
	});
var $elm$html$Html$strong = _VirtualDom_node('strong');
var $author$project$Page$Agents$diagnostics = F3(
	function (title, items, emptyText) {
		return A2(
			$author$project$Ui$Common$panel,
			title,
			$elm$core$List$isEmpty(items) ? _List_fromArray(
				[
					$author$project$Ui$Common$note(emptyText)
				]) : A2(
				$elm$core$List$map,
				function (d) {
					return A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$classList(
								_List_fromArray(
									[
										_Utils_Tuple2('diagnostic', true),
										_Utils_Tuple2('error', d.severity === 'Error')
									]))
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$code,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(d.code)
									])),
								A2(
								$elm$html$Html$strong,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(d.message)
									])),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('역할: ' + d.subject)
									])),
								A2(
								$elm$html$Html$div,
								_List_Nil,
								A2(
									$elm$core$List$map,
									function (line) {
										return A2(
											$elm$html$Html$p,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text(line)
												]));
									},
									d.details))
							]));
				},
				items));
	});
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $author$project$Ui$Label$permissionName = function (key) {
	return A2(
		$elm$core$Maybe$withDefault,
		key,
		A2(
			$elm$core$Maybe$map,
			$elm$core$Tuple$second,
			$elm$core$List$head(
				A2(
					$elm$core$List$filter,
					A2(
						$elm$core$Basics$composeR,
						$elm$core$Tuple$first,
						$elm$core$Basics$eq(key)),
					$author$project$Ui$Label$permissions))));
};
var $author$project$Page$Agents$approvalText = F2(
	function (workspace, approval) {
		if (approval.$ === 'Just') {
			if (approval.a.$ === 'Person') {
				var uid = approval.a.a;
				return '구성원 ' + A2($author$project$Ui$Label$personName, workspace, uid);
			} else {
				var permission = approval.a.a;
				return $author$project$Ui$Label$permissionName(permission) + ' 권한 보유자';
			}
		} else {
			return '없음 또는 미확인';
		}
	});
var $elm$html$Html$dd = _VirtualDom_node('dd');
var $elm$html$Html$dl = _VirtualDom_node('dl');
var $elm$html$Html$dt = _VirtualDom_node('dt');
var $author$project$Page$Agents$handoffText = F2(
	function (roles, targets) {
		return $elm$core$List$isEmpty(targets) ? '미확인' : A2(
			$elm$core$String$join,
			', ',
			A2(
				$elm$core$List$map,
				function (t) {
					return A2(
						$elm$core$Maybe$withDefault,
						t,
						A2(
							$elm$core$Maybe$map,
							function ($) {
								return $.name;
							},
							$elm$core$List$head(
								A2(
									$elm$core$List$filter,
									A2(
										$elm$core$Basics$composeR,
										function ($) {
											return $.id;
										},
										$elm$core$Basics$eq(t)),
									roles))));
				},
				targets));
	});
var $author$project$Domain$Agent$levelLabel = function (level) {
	return A2(
		$elm$core$Maybe$withDefault,
		level,
		A2(
			$elm$core$Maybe$map,
			$elm$core$Tuple$second,
			$elm$core$List$head(
				A2(
					$elm$core$List$filter,
					A2(
						$elm$core$Basics$composeR,
						$elm$core$Tuple$first,
						$elm$core$Basics$eq(level)),
					$author$project$Domain$Agent$levels))));
};
var $author$project$Domain$Discovery$statusLabel = function (status) {
	switch (status) {
		case 'confirmed':
			return '확인된 사실';
		case 'proposed':
			return '개선안';
		default:
			return '미확인';
	}
};
var $author$project$Page$Agents$draftCard = F3(
	function (workspace, roles, role) {
		return A2(
			$elm$html$Html$section,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('panel agent-card'),
					$elm$html$Html$Attributes$id('draft-' + role.id)
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('tag')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('규칙 기반 제안 / 추론')
						])),
					A2(
					$elm$html$Html$h2,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(role.name + ' 에이전트 후보')
						])),
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('제안 이유: ‘' + (role.task + '’의 입력을 받아 산출물을 만드는 역할 경계가 필요하기 때문입니다.'))
						])),
					$author$project$Ui$Common$note(
					'정보 구분: ' + ($author$project$Domain$Discovery$statusLabel(role.status) + (' · 근거: ' + $author$project$Page$Agents$known(role.evidence)))),
					A2(
					$elm$html$Html$dl,
					_List_Nil,
					A2(
						$elm$core$List$concatMap,
						function (_v0) {
							var title = _v0.a;
							var value_ = _v0.b;
							return _List_fromArray(
								[
									A2(
									$elm$html$Html$dt,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(title)
										])),
									A2(
									$elm$html$Html$dd,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(value_)
										]))
								]);
						},
						_List_fromArray(
							[
								_Utils_Tuple2('담당 업무', role.task),
								_Utils_Tuple2(
								'입력',
								$author$project$Page$Agents$known(role.inputs)),
								_Utils_Tuple2(
								'산출물',
								$author$project$Page$Agents$known(role.outputs)),
								_Utils_Tuple2(
								'도구 후보',
								$author$project$Page$Agents$known(
									$author$project$Domain$Agent$toolsText(role.tools))),
								_Utils_Tuple2(
								'권한 등급',
								$author$project$Domain$Agent$levelLabel(role.level)),
								_Utils_Tuple2(
								'사람 승인',
								A2($author$project$Page$Agents$approvalText, workspace, role.approval)),
								_Utils_Tuple2(
								'인계 대상',
								A2($author$project$Page$Agents$handoffText, roles, role.handoffTo))
							])))
				]));
	});
var $author$project$Page$Agents$drafts = F3(
	function (controls, workspace, snapshot) {
		return A2(
			$author$project$Ui$Common$panel,
			'저장된 업무에서 도출한 역할 후보',
			_Utils_ap(
				_List_fromArray(
					[
						$author$project$Ui$Common$note('도출 규칙: 담당 역할이 후보 이름이 되고, 승인 조건이 있으면 L2, 도구가 있으면 L1, 그 외 L0입니다. 승인 주체와 인계 대상은 참조 연결을 우선 사용하고, 없으면 텍스트에 포함된 구성원 이름과 업무 이름으로 찾습니다. 비어 있는 정보는 미확인으로 남깁니다.')
					]),
				_Utils_ap(
					$elm$core$List$isEmpty(snapshot.drafts) ? _List_fromArray(
						[
							$author$project$Ui$Common$note('아직 도출할 업무가 없습니다. 업무 흐름 화면에서 업무 이름과 알고 있는 내용을 입력하고 저장하세요.')
						]) : _List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('grid')
								]),
							A2(
								$elm$core$List$map,
								A2($author$project$Page$Agents$draftCard, workspace, snapshot.drafts),
								snapshot.drafts))
						]),
					_List_fromArray(
						[
							A3($author$project$Page$Agents$diagnostics, '초안 진단', snapshot.draftDiagnostics, '초안에서 확인할 사항이 없습니다.')
						]))));
	});
var $elm$html$Html$form = _VirtualDom_node('form');
var $elm$html$Html$Events$alwaysPreventDefault = function (msg) {
	return _Utils_Tuple2(msg, true);
};
var $elm$html$Html$Events$preventDefaultOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayPreventDefault(decoder));
	});
var $elm$html$Html$Events$onSubmit = function (msg) {
	return A2(
		$elm$html$Html$Events$preventDefaultOn,
		'submit',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysPreventDefault,
			$elm$json$Json$Decode$succeed(msg)));
};
var $elm$html$Html$Attributes$rel = _VirtualDom_attribute('rel');
var $elm$html$Html$Attributes$target = $elm$html$Html$Attributes$stringProperty('target');
var $author$project$Page$Agents$view = F2(
	function (controls, workspace) {
		var _v0 = A2($author$project$App$Agents$saved, controls.org, controls.state);
		if (_v0.$ === 'Nothing') {
			return A2(
				$author$project$Ui$Common$panel,
				'설계 불러오기',
				_List_fromArray(
					[
						$author$project$Ui$Common$note(
						A2(
							$elm$core$Maybe$withDefault,
							'에이전트 설계를 불러오고 있습니다…',
							A2($elm$core$Dict$get, controls.org, controls.state.errors)))
					]));
		} else {
			var snapshot = _v0.a;
			var unsaved = A2($author$project$App$Agents$changed, controls.org, controls.state);
			var unavailable = controls.state.loading || A2($elm$core$Dict$member, controls.org, controls.state.errors);
			var design = A2(
				$elm$core$Maybe$withDefault,
				snapshot.agents,
				A2(
					$elm$core$Maybe$map,
					function ($) {
						return $.agents;
					},
					A2($author$project$App$Agents$current, controls.org, controls.state)));
			var conflict = A2($author$project$App$Agents$conflicted, controls.org, controls.state);
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$author$project$Ui$Common$note('규칙 기반 초안은 저장된 업무 흐름에서 결정적으로 만듭니다. 실제 AI 실행, 도구 접근 권한 발급, 권한 부여는 하지 않습니다. 사람이 등급, 승인 주체, 인계 대상을 검토해 설계안으로 저장하세요.'),
						A3($author$project$Page$Agents$drafts, controls, workspace, snapshot),
						unsaved ? A2(
						$elm$html$Html$p,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('draft-notice'),
								A2($elm$html$Html$Attributes$attribute, 'role', 'status')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('저장하지 않은 설계안 입력이 있습니다. 화면 이동 시 유지되지만 브라우저를 닫거나 전체 새로고침하면 사라집니다.')
							])) : $elm$html$Html$text(''),
						conflict ? A2(
						$author$project$Ui$Common$panel,
						'입력 중 조직이 변경되었습니다',
						_List_fromArray(
							[
								$author$project$Ui$Common$note('최신 저장 설계와 현재 입력을 비교하세요. 아래 버튼은 입력을 최신 버전에 다시 적용할 준비를 하며, 저장은 별도로 해야 합니다.'),
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$type_('button'),
										$elm$html$Html$Attributes$disabled(controls.busy || unavailable),
										$elm$html$Html$Events$onClick(controls.rebase)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('최신 내용 확인 후 내 입력 다시 적용')
									]))
							])) : $elm$html$Html$text(''),
						A2(
						$elm$html$Html$form,
						_List_fromArray(
							[
								$elm$html$Html$Events$onSubmit(controls.save)
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$fieldset,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$disabled(controls.busy || unavailable)
									]),
								_List_fromArray(
									[
										A2(
										$author$project$Ui$Common$panel,
										'사람이 검토한 설계안',
										_Utils_ap(
											_List_fromArray(
												[
													$author$project$Ui$Common$note('각 역할의 권한 등급(L0 읽기, L1 작업 공간, L2 외부 영향, L3 금지), 사람 승인 주체, 허용 도구, 인계 대상을 확정합니다. 저장 후 서버가 같은 규칙으로 다시 진단합니다.'),
													A2(
													$elm$html$Html$div,
													_List_fromArray(
														[
															$elm$html$Html$Attributes$class('actions')
														]),
													_List_fromArray(
														[
															A2(
															$elm$html$Html$button,
															_List_fromArray(
																[
																	$elm$html$Html$Attributes$type_('button'),
																	$elm$html$Html$Attributes$id('agent-import'),
																	$elm$html$Html$Attributes$class('secondary'),
																	$elm$html$Html$Attributes$disabled(
																	$elm$core$List$isEmpty(snapshot.drafts)),
																	$elm$html$Html$Events$onClick(controls.importDrafts)
																]),
															_List_fromArray(
																[
																	$elm$html$Html$text('규칙 기반 초안을 설계안으로 가져오기')
																])),
															unsaved ? A2(
															$elm$html$Html$button,
															_List_fromArray(
																[
																	$elm$html$Html$Attributes$type_('button'),
																	$elm$html$Html$Attributes$class('secondary'),
																	$elm$html$Html$Events$onClick(controls.reset)
																]),
															_List_fromArray(
																[
																	$elm$html$Html$text('미저장 설계안 입력 취소')
																])) : $elm$html$Html$text('')
														]))
												]),
											$elm$core$List$isEmpty(design) ? _List_fromArray(
												[
													$author$project$Ui$Common$note('아직 설계안이 없습니다. 초안을 가져온 뒤 검토하세요.')
												]) : A2(
												$elm$core$List$map,
												A3($author$project$Page$Agents$designCard, controls, workspace, design),
												design))),
										A3($author$project$Page$Agents$diagnostics, '저장된 설계 진단', snapshot.diagnostics, '저장된 설계에서 확인할 사항이 없습니다. 저장 전 입력은 저장 후 진단합니다.'),
										$elm$core$List$isEmpty(
										$author$project$Domain$Agent$problems(design)) ? $elm$html$Html$text('') : A2(
										$elm$html$Html$div,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('field-errors'),
												A2($elm$html$Html$Attributes$attribute, 'role', 'status')
											]),
										A2(
											$elm$core$List$map,
											function (message) {
												return A2(
													$elm$html$Html$p,
													_List_Nil,
													_List_fromArray(
														[
															$elm$html$Html$text(message)
														]));
											},
											$author$project$Domain$Agent$problems(design))),
										A2(
										$elm$html$Html$div,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('actions')
											]),
										_List_fromArray(
											[
												A2(
												$elm$html$Html$button,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$type_('submit'),
														$elm$html$Html$Attributes$disabled(
														unavailable || (conflict || (!$elm$core$List$isEmpty(
															$author$project$Domain$Agent$problems(design)))))
													]),
												_List_fromArray(
													[
														$elm$html$Html$text(
														controls.busy ? '저장 중…' : '설계안 저장')
													])),
												A2(
												$elm$html$Html$a,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('button-link secondary'),
														$elm$html$Html$Attributes$href(controls.exportHref),
														$elm$html$Html$Attributes$target('_blank'),
														$elm$html$Html$Attributes$rel('noopener')
													]),
												_List_fromArray(
													[
														$elm$html$Html$text('정의 파일 내보내기 (Markdown)')
													])),
												A2(
												$elm$html$Html$button,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$type_('button'),
														$elm$html$Html$Attributes$class('secondary'),
														$elm$html$Html$Events$onClick(
														controls.go($author$project$Page$AgentGraph))
													]),
												_List_fromArray(
													[
														$elm$html$Html$text('에이전트 구조 보기 →')
													]))
											])),
										$author$project$Ui$Common$note('내보내기는 저장된 설계를 사용하며, 저장된 설계가 없으면 규칙 기반 초안을 내보냅니다. 파일은 .claude/agents/<id>.md 형식의 정의 초안이며 실행 설정이 아닙니다.')
									]))
							])),
						controls.review
					]));
		}
	});
var $author$project$Ui$AgentGraph$approvalLabel = F2(
	function (w, approval) {
		if (approval.$ === 'Person') {
			var uid = approval.a;
			return A2($author$project$Ui$Label$personName, w, uid) + ' 승인';
		} else {
			var permission = approval.a;
			return $author$project$Ui$Label$permissionName(permission) + ' 권한자 승인';
		}
	});
var $elm$html$Html$li = _VirtualDom_node('li');
var $elm$virtual_dom$VirtualDom$nodeNS = F2(
	function (namespace, tag) {
		return A2(
			_VirtualDom_nodeNS,
			namespace,
			_VirtualDom_noScript(tag));
	});
var $author$project$Ui$AgentGraph$svg = $elm$virtual_dom$VirtualDom$nodeNS('http://www.w3.org/2000/svg');
var $elm$html$Html$ul = _VirtualDom_node('ul');
var $author$project$Ui$AgentGraph$view = F3(
	function (w, roles, diagnostics) {
		var issues = function (role) {
			return A2(
				$elm$core$List$filter,
				function (d) {
					return _Utils_eq(d.subject, role.id);
				},
				diagnostics);
		};
		var node = function (pos) {
			var role = pos.role;
			var flagged = A2(
				$elm$core$List$any,
				function (d) {
					return d.severity === 'Error';
				},
				issues(role));
			return A3(
				$author$project$Ui$AgentGraph$svg,
				'g',
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$attribute, 'class', 'graph-node agent-node'),
						A2(
						$elm$html$Html$Attributes$attribute,
						'transform',
						'translate(' + ($elm$core$String$fromFloat(pos.x) + (',' + ($elm$core$String$fromFloat(pos.y) + ')')))),
						A2($elm$html$Html$Attributes$attribute, 'role', 'listitem'),
						$elm$html$Html$Attributes$tabindex(0),
						A2(
						$elm$html$Html$Attributes$attribute,
						'aria-label',
						role.name + (' · ' + ($author$project$Domain$Agent$levelLabel(role.level) + A2(
							$elm$core$Maybe$withDefault,
							'',
							A2(
								$elm$core$Maybe$map,
								function (a) {
									return ' · ' + A2($author$project$Ui$AgentGraph$approvalLabel, w, a);
								},
								role.approval)))))
					]),
				_List_fromArray(
					[
						A3(
						$author$project$Ui$AgentGraph$svg,
						'rect',
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$attribute, 'width', '240'),
								A2($elm$html$Html$Attributes$attribute, 'height', '100'),
								A2($elm$html$Html$Attributes$attribute, 'rx', '10'),
								A2($elm$html$Html$Attributes$attribute, 'fill', '#fff'),
								A2(
								$elm$html$Html$Attributes$attribute,
								'stroke',
								flagged ? '#bd7769' : '#8fb47e'),
								A2($elm$html$Html$Attributes$attribute, 'stroke-width', '2')
							]),
						_List_Nil),
						A3(
						$author$project$Ui$AgentGraph$svg,
						'text',
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$attribute, 'x', '14'),
								A2($elm$html$Html$Attributes$attribute, 'y', '28'),
								A2($elm$html$Html$Attributes$attribute, 'class', 'graph-node-label')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(
								A2($elm$core$String$left, 18, role.name))
							])),
						A3(
						$author$project$Ui$AgentGraph$svg,
						'text',
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$attribute, 'x', '14'),
								A2($elm$html$Html$Attributes$attribute, 'y', '50'),
								A2($elm$html$Html$Attributes$attribute, 'class', 'graph-node-meta')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(
								$author$project$Domain$Agent$levelLabel(role.level))
							])),
						A3(
						$author$project$Ui$AgentGraph$svg,
						'text',
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$attribute, 'x', '14'),
								A2($elm$html$Html$Attributes$attribute, 'y', '70'),
								A2($elm$html$Html$Attributes$attribute, 'class', 'graph-node-meta')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(
								A2(
									$elm$core$Maybe$withDefault,
									'사람 승인 없음 또는 미확인',
									A2(
										$elm$core$Maybe$map,
										$author$project$Ui$AgentGraph$approvalLabel(w),
										role.approval)))
							])),
						A3(
						$author$project$Ui$AgentGraph$svg,
						'text',
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$attribute, 'x', '14'),
								A2($elm$html$Html$Attributes$attribute, 'y', '90'),
								A2($elm$html$Html$Attributes$attribute, 'class', 'graph-node-meta')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(
								$elm$core$List$isEmpty(
									issues(role)) ? '진단 없음' : ($elm$core$String$fromInt(
									$elm$core$List$length(
										issues(role))) + '건 확인 필요'))
							]))
					]));
		};
		var columns = $author$project$Ui$AgentGraph$layers(roles);
		var positions = $elm$core$List$concat(
			A2(
				$elm$core$List$indexedMap,
				F2(
					function (column, layer) {
						return A2(
							$elm$core$List$indexedMap,
							F2(
								function (row, role) {
									return {role: role, x: 24 + (column * 300), y: 40 + (row * 132)};
								}),
							layer);
					}),
				columns));
		var height = A2(
			$elm$core$Maybe$withDefault,
			160,
			$elm$core$List$maximum(
				A2(
					$elm$core$List$map,
					function (pos) {
						return pos.y + 130;
					},
					positions)));
		var lookup = $elm$core$Dict$fromList(
			A2(
				$elm$core$List$map,
				function (pos) {
					return _Utils_Tuple2(pos.role.id, pos);
				},
				positions));
		var edge = F2(
			function (pos, target) {
				var _v0 = A2($elm$core$Dict$get, target, lookup);
				if (_v0.$ === 'Just') {
					var to = _v0.a;
					return A3(
						$author$project$Ui$AgentGraph$svg,
						'line',
						_List_fromArray(
							[
								A2(
								$elm$html$Html$Attributes$attribute,
								'x1',
								$elm$core$String$fromFloat(pos.x + 240)),
								A2(
								$elm$html$Html$Attributes$attribute,
								'y1',
								$elm$core$String$fromFloat(pos.y + 50)),
								A2(
								$elm$html$Html$Attributes$attribute,
								'x2',
								$elm$core$String$fromFloat(to.x)),
								A2(
								$elm$html$Html$Attributes$attribute,
								'y2',
								$elm$core$String$fromFloat(to.y + 50)),
								A2($elm$html$Html$Attributes$attribute, 'stroke', '#466253'),
								A2($elm$html$Html$Attributes$attribute, 'stroke-width', '2'),
								A2($elm$html$Html$Attributes$attribute, 'marker-end', 'url(#agent-arrow)')
							]),
						_List_Nil);
				} else {
					return $elm$html$Html$text('');
				}
			});
		var edges = A2(
			$elm$core$List$concatMap,
			function (pos) {
				return A2(
					$elm$core$List$map,
					edge(pos),
					pos.role.handoffTo);
			},
			positions);
		var width = (A2(
			$elm$core$Basics$max,
			1,
			$elm$core$List$length(columns)) * 300) + 40;
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('agent-graph')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$p,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('note graph-legend')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('왼쪽에서 오른쪽으로 산출물이 인계됩니다. 붉은 테두리는 오류 진단이 있는 역할입니다. 사람 승인은 각 역할 안에 표시합니다.')
						])),
					$elm$core$List$isEmpty(roles) ? A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('아직 표시할 에이전트 역할이 없습니다. 업무 흐름을 저장하면 규칙 기반 초안이 나타나고, 에이전트 초안 화면에서 설계안을 저장할 수 있습니다.')
						])) : A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('graph-viewport'),
							$elm$html$Html$Attributes$tabindex(0),
							A2($elm$html$Html$Attributes$attribute, 'role', 'region'),
							A2($elm$html$Html$Attributes$attribute, 'aria-label', '에이전트 인계 구조 다이어그램')
						]),
					_List_fromArray(
						[
							A3(
							$author$project$Ui$AgentGraph$svg,
							'svg',
							_List_fromArray(
								[
									A2(
									$elm$html$Html$Attributes$attribute,
									'viewBox',
									'0 0 ' + ($elm$core$String$fromFloat(width) + (' ' + $elm$core$String$fromFloat(height)))),
									A2($elm$html$Html$Attributes$attribute, 'width', '100%'),
									A2($elm$html$Html$Attributes$attribute, 'class', 'graph-svg graph-fit'),
									A2($elm$html$Html$Attributes$attribute, 'role', 'list')
								]),
							A2(
								$elm$core$List$cons,
								A3(
									$author$project$Ui$AgentGraph$svg,
									'defs',
									_List_Nil,
									_List_fromArray(
										[
											A3(
											$author$project$Ui$AgentGraph$svg,
											'marker',
											_List_fromArray(
												[
													A2($elm$html$Html$Attributes$attribute, 'id', 'agent-arrow'),
													A2($elm$html$Html$Attributes$attribute, 'markerWidth', '10'),
													A2($elm$html$Html$Attributes$attribute, 'markerHeight', '10'),
													A2($elm$html$Html$Attributes$attribute, 'refX', '9'),
													A2($elm$html$Html$Attributes$attribute, 'refY', '5'),
													A2($elm$html$Html$Attributes$attribute, 'orient', 'auto')
												]),
											_List_fromArray(
												[
													A3(
													$author$project$Ui$AgentGraph$svg,
													'path',
													_List_fromArray(
														[
															A2($elm$html$Html$Attributes$attribute, 'd', 'M0,0 L10,5 L0,10 z'),
															A2($elm$html$Html$Attributes$attribute, 'fill', '#466253')
														]),
													_List_Nil)
												]))
										])),
								_Utils_ap(
									edges,
									A2($elm$core$List$map, node, positions))))
						])),
					A2(
					$elm$html$Html$ul,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('graph-relations')
						]),
					A2(
						$elm$core$List$concatMap,
						function (role) {
							return A2(
								$elm$core$List$map,
								function (target) {
									return A2(
										$elm$html$Html$li,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('graph-edge')
											]),
										_List_fromArray(
											[
												A2(
												$elm$html$Html$strong,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text(role.name)
													])),
												A2(
												$elm$html$Html$span,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text('→ 인계 →')
													])),
												A2(
												$elm$html$Html$strong,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text(
														A2(
															$elm$core$Maybe$withDefault,
															target,
															A2(
																$elm$core$Maybe$map,
																A2(
																	$elm$core$Basics$composeR,
																	function ($) {
																		return $.role;
																	},
																	function ($) {
																		return $.name;
																	}),
																A2($elm$core$Dict$get, target, lookup))))
													]))
											]));
								},
								role.handoffTo);
						},
						roles))
				]));
	});
var $author$project$AgentTest$tests = A2(
	$elm_explorations$test$Test$describe,
	'Agent design',
	_List_fromArray(
		[
			A2(
			$elm_explorations$test$Test$test,
			'design edits keep handoffs consistent and removal drops references',
			function (_v0) {
				var design = A2(
					$author$project$Domain$Agent$apply,
					A3($author$project$Domain$Agent$Handoff, 'refund', 'refund', true),
					A2(
						$author$project$Domain$Agent$apply,
						A2($author$project$Domain$Agent$Tools, 'refund', '결제 콘솔, 장부\n메일'),
						A2(
							$author$project$Domain$Agent$apply,
							A2($author$project$Domain$Agent$SetApproval, 'intake', 'permission:Pricing'),
							A2(
								$author$project$Domain$Agent$apply,
								A2($author$project$Domain$Agent$Level, 'intake', 'L2'),
								_List_fromArray(
									[$author$project$AgentTest$intake, $author$project$AgentTest$refund])))));
				var removed = A2(
					$author$project$Domain$Agent$apply,
					$author$project$Domain$Agent$Remove('refund'),
					design);
				return A2(
					$elm_explorations$test$Expect$all,
					_List_fromArray(
						[
							function (_v1) {
							return A2(
								$elm_explorations$test$Expect$equal,
								$elm$core$Maybe$Just(
									_Utils_Tuple2(
										'L2',
										$elm$core$Maybe$Just(
											$author$project$Domain$Agent$Permission('Pricing')))),
								A2(
									$elm$core$Maybe$map,
									function (r) {
										return _Utils_Tuple2(r.level, r.approval);
									},
									$elm$core$List$head(design)));
						},
							function (_v2) {
							return A2(
								$elm_explorations$test$Expect$equal,
								$elm$core$Maybe$Just(
									_List_fromArray(
										['결제 콘솔', '장부', '메일'])),
								A2(
									$elm$core$Maybe$map,
									function ($) {
										return $.tools;
									},
									$elm$core$List$head(
										A2($elm$core$List$drop, 1, design))));
						},
							function (_v3) {
							return A2(
								$elm_explorations$test$Expect$equal,
								$elm$core$Maybe$Just(_List_Nil),
								A2(
									$elm$core$Maybe$map,
									function ($) {
										return $.handoffTo;
									},
									$elm$core$List$head(
										A2($elm$core$List$drop, 1, design))));
						},
							function (_v4) {
							return A2(
								$elm_explorations$test$Expect$equal,
								_List_fromArray(
									[
										_Utils_Tuple2('intake', _List_Nil)
									]),
								A2(
									$elm$core$List$map,
									function (r) {
										return _Utils_Tuple2(r.id, r.handoffTo);
									},
									removed));
						},
							function (_v5) {
							return A2(
								$elm_explorations$test$Expect$equal,
								_List_fromArray(
									['intake: 역할 이름을 입력하세요.']),
								$author$project$Domain$Agent$problems(
									A2(
										$author$project$Domain$Agent$apply,
										A2($author$project$Domain$Agent$Name, 'intake', ' '),
										_List_fromArray(
											[$author$project$AgentTest$intake]))));
						},
							function (_v6) {
							return A2(
								$elm_explorations$test$Expect$equal,
								_List_fromArray(
									['역할 intake: 확인된 사실에는 근거가 필요합니다.']),
								$author$project$Domain$Agent$problems(
									A2(
										$author$project$Domain$Agent$apply,
										A2($author$project$Domain$Agent$Evidence, 'intake', ''),
										_List_fromArray(
											[$author$project$AgentTest$intake]))));
						}
						]),
					_Utils_Tuple0);
			}),
			A2(
			$elm_explorations$test$Test$test,
			'snapshot decoding round-trips roles and diagnostics',
			function (_v7) {
				var encoded = $elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'version',
							$elm$json$Json$Encode$int(11)),
							_Utils_Tuple2(
							'agents',
							A2($elm$json$Json$Encode$list, $elm$core$Basics$identity, _List_Nil)),
							_Utils_Tuple2(
							'drafts',
							$author$project$Api$Agents$encode(
								_List_fromArray(
									[$author$project$AgentTest$intake, $author$project$AgentTest$refund]))),
							_Utils_Tuple2(
							'diagnostics',
							A2($elm$json$Json$Encode$list, $elm$core$Basics$identity, _List_Nil)),
							_Utils_Tuple2(
							'draftDiagnostics',
							A2(
								$elm$json$Json$Encode$list,
								$elm$core$Basics$identity,
								_List_fromArray(
									[
										$elm$json$Json$Encode$object(
										_List_fromArray(
											[
												_Utils_Tuple2(
												'severity',
												$elm$json$Json$Encode$string('Info')),
												_Utils_Tuple2(
												'code',
												$elm$json$Json$Encode$string('A009')),
												_Utils_Tuple2(
												'message',
												$elm$json$Json$Encode$string('산출물의 인계 대상이 미확인입니다.')),
												_Utils_Tuple2(
												'subject',
												$elm$json$Json$Encode$string('refund')),
												_Utils_Tuple2(
												'details',
												A2($elm$json$Json$Encode$list, $elm$core$Basics$identity, _List_Nil))
											]))
									])))
						]));
				return A2(
					$elm_explorations$test$Expect$equal,
					$elm$core$Result$Ok($author$project$AgentTest$snapshot),
					A2($elm$json$Json$Decode$decodeValue, $author$project$Api$Agents$decoder, encoded));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'workflow references are omitted when empty and decoded when absent',
			function (_v8) {
				var doc = A2(
					$author$project$Domain$Discovery$apply,
					$author$project$Domain$Discovery$AddWorkflow('w'),
					$author$project$Domain$Discovery$empty);
				var keys = A2(
					$elm$json$Json$Decode$decodeValue,
					A2(
						$elm$json$Json$Decode$field,
						'workflows',
						A2(
							$elm$json$Json$Decode$index,
							0,
							A2(
								$elm$json$Json$Decode$map,
								$elm$core$List$map($elm$core$Tuple$first),
								$elm$json$Json$Decode$keyValuePairs($elm$json$Json$Decode$value)))),
					$author$project$Api$Discovery$encode(doc));
				var decoded = A2(
					$elm$json$Json$Decode$decodeValue,
					$author$project$Api$Discovery$decoder,
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'version',
								$elm$json$Json$Encode$int(1)),
								_Utils_Tuple2(
								'discovery',
								$author$project$Api$Discovery$encode(doc))
							])));
				return A2(
					$elm_explorations$test$Expect$all,
					_List_fromArray(
						[
							function (_v9) {
							return A2(
								$elm_explorations$test$Expect$equal,
								$elm$core$Result$Ok(false),
								A2(
									$elm$core$Result$map,
									$elm$core$List$member('handoffWorkflows'),
									keys));
						},
							function (_v10) {
							return A2(
								$elm_explorations$test$Expect$equal,
								$elm$core$Result$Ok(
									$elm$core$Maybe$Just(
										_Utils_Tuple2($elm$core$Maybe$Nothing, _List_Nil))),
								A2(
									$elm$core$Result$map,
									A2(
										$elm$core$Basics$composeR,
										function ($) {
											return $.discovery;
										},
										A2(
											$elm$core$Basics$composeR,
											function ($) {
												return $.workflows;
											},
											A2(
												$elm$core$Basics$composeR,
												$elm$core$List$head,
												$elm$core$Maybe$map(
													function (w) {
														return _Utils_Tuple2(w.rolePerson, w.handoffWorkflows);
													})))),
									decoded));
						}
						]),
					_Utils_Tuple0);
			}),
			A2(
			$elm_explorations$test$Test$test,
			'importing drafts starts a local design that saves with the agents version',
			function (_v11) {
				var loaded = A2(
					$author$project$AgentTest$step,
					A3(
						$author$project$App$Update$GotAgents,
						$author$project$AgentTest$ready.session.request,
						'org-a',
						$elm$core$Result$Ok($author$project$AgentTest$snapshot)),
					$author$project$AgentTest$ready);
				var imported = A2(
					$author$project$AgentTest$step,
					$author$project$App$Update$EditAgents(
						A2($author$project$Domain$Agent$Level, 'intake', 'L0')),
					A2($author$project$AgentTest$step, $author$project$App$Update$ImportAgentDrafts, loaded));
				var _v12 = A2($author$project$App$Update$update, $author$project$App$Update$SubmitAgents, imported).b;
				if (((_v12.b && (_v12.a.$ === 'SaveAgents')) && (_v12.a.b === 'org-a')) && (!_v12.b.b)) {
					var _v13 = _v12.a;
					var version = _v13.c;
					var agents = _v13.d;
					return A2(
						$elm_explorations$test$Expect$equal,
						_Utils_Tuple2(
							11,
							_List_fromArray(
								['L0', 'L2'])),
						_Utils_Tuple2(
							version,
							A2(
								$elm$core$List$map,
								function ($) {
									return $.level;
								},
								agents)));
				} else {
					return $elm_explorations$test$Expect$fail('Expected exactly one scoped agents save');
				}
			}),
			A2(
			$elm_explorations$test$Test$test,
			'a newer server version conflicts until the design is rebased',
			function (_v14) {
				var state = A3(
					$author$project$App$Agents$receive,
					'org-a',
					$elm$core$Result$Ok(
						_Utils_update(
							$author$project$AgentTest$snapshot,
							{version: 12})),
					A3(
						$author$project$App$Agents$edit,
						'org-a',
						$author$project$Domain$Agent$Import($author$project$AgentTest$snapshot.drafts),
						A3(
							$author$project$App$Agents$receive,
							'org-a',
							$elm$core$Result$Ok($author$project$AgentTest$snapshot),
							$author$project$App$Agents$init)));
				return A2(
					$elm_explorations$test$Expect$all,
					_List_fromArray(
						[
							function (_v15) {
							return A2(
								$elm_explorations$test$Expect$equal,
								true,
								A2($author$project$App$Agents$conflicted, 'org-a', state));
						},
							function (_v16) {
							return A2(
								$elm_explorations$test$Expect$equal,
								false,
								A2(
									$author$project$App$Agents$conflicted,
									'org-a',
									A2($author$project$App$Agents$rebase, 'org-a', state)));
						},
							function (_v17) {
							return A2(
								$elm_explorations$test$Expect$equal,
								$elm$core$Maybe$Just(2),
								A2(
									$elm$core$Maybe$map,
									A2(
										$elm$core$Basics$composeR,
										function ($) {
											return $.agents;
										},
										$elm$core$List$length),
									A2(
										$author$project$App$Agents$current,
										'org-a',
										A2($author$project$App$Agents$rebase, 'org-a', state))));
						}
						]),
					_Utils_Tuple0);
			}),
			A2(
			$elm_explorations$test$Test$test,
			'stale and cross-organization agent responses are ignored and deletion clears the design',
			function (_v18) {
				var loaded = A2(
					$author$project$AgentTest$step,
					$author$project$App$Update$ImportAgentDrafts,
					A2(
						$author$project$AgentTest$step,
						A3(
							$author$project$App$Update$GotAgents,
							$author$project$AgentTest$ready.session.request,
							'org-a',
							$elm$core$Result$Ok($author$project$AgentTest$snapshot)),
						$author$project$AgentTest$ready));
				var deleted = A2(
					$author$project$AgentTest$step,
					A3(
						$author$project$App$Update$Saved,
						loaded.session.request,
						$author$project$Form$Action$DeleteOrg,
						$elm$core$Result$Ok(_Utils_Tuple0)),
					loaded);
				return A2(
					$elm_explorations$test$Expect$all,
					_List_fromArray(
						[
							function (_v19) {
							return A2(
								$elm_explorations$test$Expect$equal,
								$author$project$AgentTest$ready,
								A2(
									$author$project$AgentTest$step,
									A3(
										$author$project$App$Update$GotAgents,
										$author$project$AgentTest$ready.session.request,
										'org-b',
										$elm$core$Result$Ok($author$project$AgentTest$snapshot)),
									A2(
										$author$project$AgentTest$step,
										A3(
											$author$project$App$Update$GotAgents,
											0,
											'org-a',
											$elm$core$Result$Ok($author$project$AgentTest$snapshot)),
										$author$project$AgentTest$ready)));
						},
							function (_v20) {
							return A2(
								$elm_explorations$test$Expect$equal,
								_Utils_Tuple2($elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing),
								_Utils_Tuple2(
									A2($author$project$App$Agents$current, 'org-a', deleted.agents),
									A2($author$project$App$Agents$saved, 'org-a', deleted.agents)));
						}
						]),
					_Utils_Tuple0);
			}),
			A2(
			$elm_explorations$test$Test$test,
			'the page shows rule-based drafts, diagnostics and an import action',
			function (_v21) {
				var rendered = $elm_explorations$test$Test$Html$Query$fromHtml(
					A2(
						$author$project$Page$Agents$view,
						$author$project$AgentTest$controls(
							A3(
								$author$project$App$Agents$receive,
								'org-a',
								$elm$core$Result$Ok($author$project$AgentTest$snapshot),
								$author$project$App$Agents$init)),
						$author$project$AgentTest$workspace));
				return A2(
					$elm_explorations$test$Expect$all,
					_List_fromArray(
						[
							function (_v22) {
							return A2(
								$elm_explorations$test$Test$Html$Query$has,
								_List_fromArray(
									[
										$elm_explorations$test$Test$Html$Selector$text('역할 refund 에이전트 후보'),
										$elm_explorations$test$Test$Html$Selector$text('규칙 기반 제안 / 추론'),
										$elm_explorations$test$Test$Html$Selector$text('구성원 팀장'),
										$elm_explorations$test$Test$Html$Selector$text('L2 외부 영향 · 사람 승인 필요'),
										$elm_explorations$test$Test$Html$Selector$text('A009')
									]),
								rendered);
						},
							function (_v23) {
							return A2(
								$elm_explorations$test$Test$Html$Query$has,
								_List_fromArray(
									[
										$elm_explorations$test$Test$Html$Selector$text('정의 파일 내보내기 (Markdown)')
									]),
								A2(
									$elm_explorations$test$Test$Html$Query$find,
									_List_fromArray(
										[
											$elm_explorations$test$Test$Html$Selector$tag('a'),
											$elm_explorations$test$Test$Html$Selector$attribute(
											$elm$html$Html$Attributes$href('/api/organizations/org-a/agents/export'))
										]),
									rendered));
						},
							function (_v24) {
							return A2(
								$elm_explorations$test$Test$Html$Query$has,
								_List_fromArray(
									[
										$elm_explorations$test$Test$Html$Selector$text('규칙 기반 초안을 설계안으로 가져오기')
									]),
								rendered);
						}
						]),
					_Utils_Tuple0);
			}),
			A2(
			$elm_explorations$test$Test$test,
			'handoff layers order roles from sources to sinks and keep cycles',
			function (_v25) {
				var cycleB = _Utils_update(
					$author$project$AgentTest$intake,
					{
						handoffTo: _List_fromArray(
							['a']),
						id: 'b'
					});
				var cycleA = _Utils_update(
					$author$project$AgentTest$intake,
					{
						handoffTo: _List_fromArray(
							['b']),
						id: 'a'
					});
				return A2(
					$elm_explorations$test$Expect$all,
					_List_fromArray(
						[
							function (_v26) {
							return A2(
								$elm_explorations$test$Expect$equal,
								_List_fromArray(
									[
										_List_fromArray(
										['intake']),
										_List_fromArray(
										['refund'])
									]),
								A2(
									$elm$core$List$map,
									$elm$core$List$map(
										function ($) {
											return $.id;
										}),
									$author$project$Ui$AgentGraph$layers(
										_List_fromArray(
											[$author$project$AgentTest$refund, $author$project$AgentTest$intake]))));
						},
							function (_v27) {
							return A2(
								$elm_explorations$test$Expect$equal,
								_List_fromArray(
									[
										_List_fromArray(
										['a', 'b'])
									]),
								A2(
									$elm$core$List$map,
									$elm$core$List$map(
										function ($) {
											return $.id;
										}),
									$author$project$Ui$AgentGraph$layers(
										_List_fromArray(
											[cycleA, cycleB]))));
						},
							function (_v28) {
							return A2(
								$elm_explorations$test$Test$Html$Query$has,
								_List_fromArray(
									[
										$elm_explorations$test$Test$Html$Selector$text('역할 intake'),
										$elm_explorations$test$Test$Html$Selector$text('→ 인계 →'),
										$elm_explorations$test$Test$Html$Selector$text('팀장 승인')
									]),
								$elm_explorations$test$Test$Html$Query$fromHtml(
									A3(
										$author$project$Ui$AgentGraph$view,
										$author$project$AgentTest$workspace,
										_List_fromArray(
											[$author$project$AgentTest$intake, $author$project$AgentTest$refund]),
										_List_Nil)));
						}
						]),
					_Utils_Tuple0);
			})
		]));
var $author$project$Domain$Achieved = {$: 'Achieved'};
var $author$project$Domain$AtRisk = {$: 'AtRisk'};
var $author$project$Domain$OffTrack = {$: 'OffTrack'};
var $author$project$Domain$OnTrack = {$: 'OnTrack'};
var $author$project$Domain$Audit = F7(
	function (seq, at, actor, description, evaluatedGoal, evaluatedStatus, activity) {
		return {activity: activity, actor: actor, at: at, description: description, evaluatedGoal: evaluatedGoal, evaluatedStatus: evaluatedStatus, seq: seq};
	});
var $author$project$Api$Activity$empty = {detail: '', personId: $elm$core$Maybe$Nothing, raw: 'null', reviewId: $elm$core$Maybe$Nothing, tag: '', targetId: '', targetKind: ''};
var $author$project$Api$Activity$permissionLabel = function (key) {
	return A2(
		$elm$core$Maybe$withDefault,
		key,
		A2(
			$elm$core$Maybe$map,
			$elm$core$Tuple$second,
			$elm$core$List$head(
				A2(
					$elm$core$List$filter,
					A2(
						$elm$core$Basics$composeR,
						$elm$core$Tuple$first,
						$elm$core$Basics$eq(key)),
					_List_fromArray(
						[
							_Utils_Tuple2('Pricing', '가격 결정'),
							_Utils_Tuple2('Hiring', '채용'),
							_Utils_Tuple2('BudgetApproval', '예산 승인'),
							_Utils_Tuple2('Contracting', '계약'),
							_Utils_Tuple2('Marketing', '마케팅'),
							_Utils_Tuple2('Infrastructure', '인프라'),
							_Utils_Tuple2('ProductLaunch', '제품 출시')
						])))));
};
var $author$project$Api$Activity$read = F2(
	function (decoder_, raw) {
		return A2(
			$elm$core$Result$withDefault,
			'',
			A2($elm$json$Json$Decode$decodeValue, decoder_, raw));
	});
var $author$project$Api$Activity$statusLabel = function (key) {
	return A2(
		$elm$core$Maybe$withDefault,
		key,
		A2(
			$elm$core$Maybe$map,
			$elm$core$Tuple$second,
			$elm$core$List$head(
				A2(
					$elm$core$List$filter,
					A2(
						$elm$core$Basics$composeR,
						$elm$core$Tuple$first,
						$elm$core$Basics$eq(key)),
					_List_fromArray(
						[
							_Utils_Tuple2('NoData', '결과 대기'),
							_Utils_Tuple2('OnTrack', '정상'),
							_Utils_Tuple2('AtRisk', '위험'),
							_Utils_Tuple2('OffTrack', '이탈'),
							_Utils_Tuple2('Achieved', '달성')
						])))));
};
var $author$project$Api$Activity$interpret = function (raw) {
	var tag = A2(
		$author$project$Api$Activity$read,
		A2($elm$json$Json$Decode$field, 'tag', $elm$json$Json$Decode$string),
		raw);
	var target = F2(
		function (kind, ident) {
			return _Utils_update(
				$author$project$Api$Activity$empty,
				{tag: tag, targetId: ident, targetKind: kind});
		});
	var number = A2($elm$json$Json$Decode$map, $elm$core$String$fromFloat, $elm$json$Json$Decode$float);
	var contents = A2(
		$elm$core$Result$withDefault,
		$elm$json$Json$Encode$null,
		A2(
			$elm$json$Json$Decode$decodeValue,
			A2($elm$json$Json$Decode$field, 'contents', $elm$json$Json$Decode$value),
			raw));
	var detail = function (decoder_) {
		return A2($author$project$Api$Activity$read, decoder_, contents);
	};
	var field = function (key) {
		return A2(
			$author$project$Api$Activity$read,
			A2($elm$json$Json$Decode$field, key, $elm$json$Json$Decode$string),
			contents);
	};
	var str = A2($author$project$Api$Activity$read, $elm$json$Json$Decode$string, contents);
	var at = F2(
		function (n, decoder_) {
			return A2($elm$json$Json$Decode$index, n, decoder_);
		});
	var first = A2(
		$author$project$Api$Activity$read,
		A2(at, 0, $elm$json$Json$Decode$string),
		contents);
	var pair = function (kind) {
		return A2(target, kind, first);
	};
	var objectAt = F2(
		function (n, key) {
			return A2(
				$author$project$Api$Activity$read,
				A2(
					at,
					n,
					A2($elm$json$Json$Decode$field, key, $elm$json$Json$Decode$string)),
				contents);
		});
	var personEvent = function () {
		var event = A2(
			target,
			'person',
			A2(objectAt, 0, 'id'));
		return _Utils_update(
			event,
			{
				detail: A2(objectAt, 0, 'name') + (' · ' + A2(objectAt, 0, 'role'))
			});
	}();
	switch (tag) {
		case 'OrganizationCreated':
			var event = A2(
				target,
				'organization',
				field('id'));
			return _Utils_update(
				event,
				{
					detail: field('name')
				});
		case 'OrganizationRenamed':
			var event = pair('organization');
			return _Utils_update(
				event,
				{
					detail: detail(
						A2(at, 1, $elm$json$Json$Decode$string))
				});
		case 'OrganizationDeleted':
			return A2(target, 'organization', str);
		case 'DemoSeeded':
			return A2(target, 'organization', str);
		case 'PersonAdded':
			var event = A2(
				target,
				'person',
				field('id'));
			return _Utils_update(
				event,
				{
					detail: field('name') + (' · ' + field('role'))
				});
		case 'EmployeeAdded':
			return personEvent;
		case 'PersonUpdated':
			return personEvent;
		case 'PersonDeactivated':
			var event = pair('person');
			return _Utils_update(
				event,
				{
					personId: A2(
						$elm$core$Result$withDefault,
						$elm$core$Maybe$Nothing,
						A2(
							$elm$json$Json$Decode$decodeValue,
							A2(
								at,
								1,
								$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string)),
							contents))
				});
		case 'GoalCreated':
			var event = A2(
				target,
				'goal',
				field('id'));
			return _Utils_update(
				event,
				{
					detail: field('description')
				});
		case 'OwnerAssigned':
			var event = pair('goal');
			return _Utils_update(
				event,
				{
					personId: $elm$core$Maybe$Just(
						detail(
							A2(at, 1, $elm$json$Json$Decode$string)))
				});
		case 'AuthorityGranted':
			var flags = A2(
				$elm$core$List$filterMap,
				function (_v1) {
					var key = _v1.a;
					var label = _v1.b;
					return _Utils_eq(
						A2(
							$elm$json$Json$Decode$decodeValue,
							A2(
								at,
								1,
								A2($elm$json$Json$Decode$field, key, $elm$json$Json$Decode$bool)),
							contents),
						$elm$core$Result$Ok(true)) ? $elm$core$Maybe$Just(label) : $elm$core$Maybe$Nothing;
				},
				_List_fromArray(
					[
						_Utils_Tuple2('canHire', '채용'),
						_Utils_Tuple2('canChangePrice', '가격 결정')
					]));
			var event = pair('person');
			var approvals = A2(
				$elm$core$List$map,
				$author$project$Api$Activity$permissionLabel,
				A2(
					$elm$core$Result$withDefault,
					_List_Nil,
					A2(
						$elm$json$Json$Decode$decodeValue,
						A2(
							at,
							1,
							A2(
								$elm$json$Json$Decode$field,
								'canApprove',
								$elm$json$Json$Decode$list($elm$json$Json$Decode$string))),
						contents)));
			var granted = $elm$core$Set$toList(
				$elm$core$Set$fromList(
					_Utils_ap(flags, approvals)));
			return _Utils_update(
				event,
				{
					detail: '예산 ' + (detail(
						A2(
							at,
							1,
							A2($elm$json$Json$Decode$field, 'budgetLimit', number))) + (' · 보유 권한 전체: ' + ($elm$core$List$isEmpty(granted) ? '없음' : A2($elm$core$String$join, ', ', granted))))
				});
		case 'AuthorityRevoked':
			var event = pair('person');
			return _Utils_update(
				event,
				{
					detail: $author$project$Api$Activity$permissionLabel(
						detail(
							A2(at, 1, $elm$json$Json$Decode$string)))
				});
		case 'GoalActivated':
			return A2(target, 'goal', str);
		case 'ResultReported':
			var event = pair('goal');
			return _Utils_update(
				event,
				{
					detail: '값 ' + (detail(
						A2(
							at,
							1,
							A2($elm$json$Json$Decode$field, 'value', number))) + (' · ' + A2(objectAt, 1, 'note'))),
					personId: A2(
						$elm$core$Result$withDefault,
						$elm$core$Maybe$Nothing,
						A2(
							$elm$json$Json$Decode$decodeValue,
							A2(
								at,
								1,
								A2(
									$elm$json$Json$Decode$field,
									'reportedBy',
									$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string))),
							contents))
				});
		case 'GoalEvaluated':
			var event = pair('goal');
			return _Utils_update(
				event,
				{
					detail: $author$project$Api$Activity$statusLabel(
						A2(objectAt, 1, 'status')) + (' · 진행률 ' + detail(
						A2(
							at,
							1,
							A2(
								$elm$json$Json$Decode$field,
								'progress',
								A2(
									$elm$json$Json$Decode$map,
									function (n) {
										return $elm$core$String$fromFloat(
											$elm$core$Basics$round(n * 10000) / 100) + '%';
									},
									$elm$json$Json$Decode$float)))))
				});
		case 'ReviewHeld':
			var event = A2(
				target,
				'goal',
				field('goal'));
			return _Utils_update(
				event,
				{
					detail: field('note') + (' · 결정 ' + (detail(
						A2(
							$elm$json$Json$Decode$field,
							'decisions',
							A2(
								$elm$json$Json$Decode$map,
								A2($elm$core$Basics$composeR, $elm$core$List$length, $elm$core$String$fromInt),
								$elm$json$Json$Decode$list($elm$json$Json$Decode$value)))) + ('건 · 학습 ' + (detail(
						A2(
							$elm$json$Json$Decode$field,
							'learnings',
							A2(
								$elm$json$Json$Decode$map,
								A2($elm$core$Basics$composeR, $elm$core$List$length, $elm$core$String$fromInt),
								$elm$json$Json$Decode$list($elm$json$Json$Decode$value)))) + '건')))),
					reviewId: $elm$core$Maybe$Just(
						field('id'))
				});
		case 'StrategyChanged':
			var event = pair('goal');
			return _Utils_update(
				event,
				{
					detail: detail(
						A2(at, 1, $elm$json$Json$Decode$string))
				});
		case 'DiscoverySaved':
			var event = A2(target, 'survey', '');
			return _Utils_update(
				event,
				{
					detail: '범위: ' + (field('scope') + (' · 업무 ' + (detail(
						A2(
							$elm$json$Json$Decode$field,
							'workflows',
							A2(
								$elm$json$Json$Decode$map,
								A2($elm$core$Basics$composeR, $elm$core$List$length, $elm$core$String$fromInt),
								$elm$json$Json$Decode$list($elm$json$Json$Decode$value)))) + ('건 · 검토 ' + A2(
						$author$project$Api$Activity$read,
						A2(
							$elm$json$Json$Decode$at,
							_List_fromArray(
								['review', 'status']),
							$elm$json$Json$Decode$string),
						contents)))))
				});
		case 'AgentRolesSaved':
			var event = A2(target, 'agents', '');
			return _Utils_update(
				event,
				{
					detail: '역할 ' + (detail(
						A2(
							$elm$json$Json$Decode$map,
							A2($elm$core$Basics$composeR, $elm$core$List$length, $elm$core$String$fromInt),
							$elm$json$Json$Decode$list($elm$json$Json$Decode$value))) + ('개: ' + A2(
						$elm$core$String$join,
						', ',
						A2(
							$elm$core$Result$withDefault,
							_List_Nil,
							A2(
								$elm$json$Json$Decode$decodeValue,
								$elm$json$Json$Decode$list(
									A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string)),
								contents)))))
				});
		default:
			return _Utils_update(
				$author$project$Api$Activity$empty,
				{tag: tag});
	}
};
var $author$project$Api$Activity$unscoped = function (raw) {
	return (A2(
		$author$project$Api$Activity$read,
		A2($elm$json$Json$Decode$field, 'tag', $elm$json$Json$Decode$string),
		raw) === 'OrganizationScoped') ? A2(
		$elm$core$Result$withDefault,
		raw,
		A2(
			$elm$core$Result$map,
			$author$project$Api$Activity$unscoped,
			A2(
				$elm$json$Json$Decode$decodeValue,
				A2(
					$elm$json$Json$Decode$field,
					'contents',
					A2($elm$json$Json$Decode$index, 1, $elm$json$Json$Decode$value)),
				raw))) : raw;
};
var $author$project$Api$Activity$decoder = A2(
	$elm$json$Json$Decode$map,
	function (raw) {
		return function (event) {
			return _Utils_update(
				event,
				{
					raw: A2($elm$json$Json$Encode$encode, 2, raw)
				});
		}(
			$author$project$Api$Activity$interpret(
				$author$project$Api$Activity$unscoped(raw)));
	},
	$elm$json$Json$Decode$value);
var $elm$core$Result$toMaybe = function (result) {
	if (result.$ === 'Ok') {
		var v = result.a;
		return $elm$core$Maybe$Just(v);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Api$Decode$evaluationField = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$json$Json$Decode$map,
				function (raw) {
					return _Utils_eq(
						A2(
							$elm$json$Json$Decode$decodeValue,
							A2($elm$json$Json$Decode$field, 'tag', $elm$json$Json$Decode$string),
							raw),
						$elm$core$Result$Ok('GoalEvaluated')) ? $elm$core$Result$toMaybe(
						A2($elm$json$Json$Decode$decodeValue, decoder, raw)) : $elm$core$Maybe$Nothing;
				},
				A2(
					$elm$json$Json$Decode$map,
					$author$project$Api$Activity$unscoped,
					A2(
						$elm$json$Json$Decode$at,
						_List_fromArray(
							['record', 'event']),
						$elm$json$Json$Decode$value))),
				$elm$json$Json$Decode$succeed($elm$core$Maybe$Nothing)
			]));
};
var $author$project$Api$Decode$statusDecoder = A2(
	$elm$json$Json$Decode$andThen,
	function (s) {
		switch (s) {
			case 'NoData':
				return $elm$json$Json$Decode$succeed($author$project$Domain$NoData);
			case 'OnTrack':
				return $elm$json$Json$Decode$succeed($author$project$Domain$OnTrack);
			case 'AtRisk':
				return $elm$json$Json$Decode$succeed($author$project$Domain$AtRisk);
			case 'OffTrack':
				return $elm$json$Json$Decode$succeed($author$project$Domain$OffTrack);
			case 'Achieved':
				return $elm$json$Json$Decode$succeed($author$project$Domain$Achieved);
			default:
				return $elm$json$Json$Decode$fail('알 수 없는 성과 상태: ' + s);
		}
	},
	$elm$json$Json$Decode$string);
var $author$project$Api$Decode$auditDecoder = A2(
	$author$project$Api$Decode$andMap,
	$elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['record', 'event']),
				$author$project$Api$Activity$decoder),
				$elm$json$Json$Decode$succeed($author$project$Api$Activity$empty)
			])),
	A2(
		$author$project$Api$Decode$andMap,
		$author$project$Api$Decode$evaluationField(
			A2(
				$elm$json$Json$Decode$field,
				'contents',
				A2(
					$elm$json$Json$Decode$index,
					1,
					A2($elm$json$Json$Decode$field, 'status', $author$project$Api$Decode$statusDecoder)))),
		A2(
			$author$project$Api$Decode$andMap,
			$author$project$Api$Decode$evaluationField(
				A2(
					$elm$json$Json$Decode$field,
					'contents',
					A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$string))),
			A3(
				$author$project$Api$Decode$field,
				'description',
				$elm$json$Json$Decode$string,
				A2(
					$author$project$Api$Decode$andMap,
					A2(
						$elm$json$Json$Decode$field,
						'record',
						A2($author$project$Api$Decode$optional, 'actor', $elm$json$Json$Decode$string)),
					A2(
						$author$project$Api$Decode$andMap,
						A2(
							$elm$json$Json$Decode$at,
							_List_fromArray(
								['record', 'at']),
							$elm$json$Json$Decode$string),
						A2(
							$author$project$Api$Decode$andMap,
							A2(
								$elm$json$Json$Decode$at,
								_List_fromArray(
									['record', 'seq']),
								$elm$json$Json$Decode$int),
							$elm$json$Json$Decode$succeed($author$project$Domain$Audit))))))));
var $elm$json$Json$Decode$decodeString = _Json_runOnString;
var $elm_explorations$test$Test$Runner$Failure$Comparison = F2(
	function (a, b) {
		return {$: 'Comparison', a: a, b: b};
	});
var $elm_explorations$test$Expect$err = function (result) {
	if (result.$ === 'Ok') {
		return $elm_explorations$test$Test$Expectation$fail(
			{
				description: 'Expect.err',
				reason: A2(
					$elm_explorations$test$Test$Runner$Failure$Comparison,
					'Err _',
					$elm_explorations$test$Test$Internal$toString(result))
			});
	} else {
		return $elm_explorations$test$Expect$pass;
	}
};
var $author$project$Domain$Evaluation = F3(
	function (status, progress, latestValue) {
		return {latestValue: latestValue, progress: progress, status: status};
	});
var $author$project$Api$Decode$evaluationDecoder = A2(
	$author$project$Api$Decode$andMap,
	A2($author$project$Api$Decode$optional, 'latestValue', $elm$json$Json$Decode$float),
	A3(
		$author$project$Api$Decode$field,
		'progress',
		$elm$json$Json$Decode$float,
		A3(
			$author$project$Api$Decode$field,
			'status',
			$author$project$Api$Decode$statusDecoder,
			$elm$json$Json$Decode$succeed($author$project$Domain$Evaluation))));
var $author$project$Domain$Person = F7(
	function (id, name, role, reportsTo, department, email, active) {
		return {active: active, department: department, email: email, id: id, name: name, reportsTo: reportsTo, role: role};
	});
var $author$project$Api$Decode$personDecoder = A2(
	$author$project$Api$Decode$andMap,
	A2(
		$elm$json$Json$Decode$andThen,
		function (status) {
			if (status.$ === 'Nothing') {
				return $elm$json$Json$Decode$succeed(true);
			} else {
				switch (status.a) {
					case 'active':
						return $elm$json$Json$Decode$succeed(true);
					case 'inactive':
						return $elm$json$Json$Decode$succeed(false);
					default:
						return $elm$json$Json$Decode$fail('알 수 없는 재직 상태');
				}
			}
		},
		A2($author$project$Api$Decode$optional, 'status', $elm$json$Json$Decode$string)),
	A2(
		$author$project$Api$Decode$andMap,
		A2($author$project$Api$Decode$optional, 'email', $elm$json$Json$Decode$string),
		A2(
			$author$project$Api$Decode$andMap,
			A2($author$project$Api$Decode$optional, 'department', $elm$json$Json$Decode$string),
			A2(
				$author$project$Api$Decode$andMap,
				A2($author$project$Api$Decode$optional, 'reportsTo', $elm$json$Json$Decode$string),
				A3(
					$author$project$Api$Decode$field,
					'role',
					$elm$json$Json$Decode$string,
					A3(
						$author$project$Api$Decode$field,
						'name',
						$elm$json$Json$Decode$string,
						A3(
							$author$project$Api$Decode$field,
							'id',
							$elm$json$Json$Decode$string,
							$elm$json$Json$Decode$succeed($author$project$Domain$Person))))))));
var $author$project$Domain$Review = F7(
	function (id, goal, heldAt, note, evaluation, learnings, decisions) {
		return {decisions: decisions, evaluation: evaluation, goal: goal, heldAt: heldAt, id: id, learnings: learnings, note: note};
	});
var $author$project$Domain$Decision = F3(
	function (text, owner, deadline) {
		return {deadline: deadline, owner: owner, text: text};
	});
var $author$project$Api$Decode$decisionDecoder = A2(
	$author$project$Api$Decode$andMap,
	A2($author$project$Api$Decode$optional, 'deadline', $elm$json$Json$Decode$string),
	A3(
		$author$project$Api$Decode$field,
		'owner',
		$elm$json$Json$Decode$string,
		A3(
			$author$project$Api$Decode$field,
			'text',
			$elm$json$Json$Decode$string,
			$elm$json$Json$Decode$succeed($author$project$Domain$Decision))));
var $author$project$Api$Decode$reviewDecoder = A3(
	$author$project$Api$Decode$field,
	'decisions',
	$elm$json$Json$Decode$list($author$project$Api$Decode$decisionDecoder),
	A3(
		$author$project$Api$Decode$field,
		'learnings',
		$elm$json$Json$Decode$list(
			A2($elm$json$Json$Decode$field, 'text', $elm$json$Json$Decode$string)),
		A3(
			$author$project$Api$Decode$field,
			'evaluation',
			$author$project$Api$Decode$evaluationDecoder,
			A3(
				$author$project$Api$Decode$field,
				'note',
				$elm$json$Json$Decode$string,
				A3(
					$author$project$Api$Decode$field,
					'heldAt',
					$elm$json$Json$Decode$string,
					A3(
						$author$project$Api$Decode$field,
						'goal',
						$elm$json$Json$Decode$string,
						A3(
							$author$project$Api$Decode$field,
							'id',
							$elm$json$Json$Decode$string,
							$elm$json$Json$Decode$succeed($author$project$Domain$Review))))))));
var $author$project$Domain$Summary = F4(
	function (organization, demo, peopleCount, goalCount) {
		return {demo: demo, goalCount: goalCount, organization: organization, peopleCount: peopleCount};
	});
var $author$project$Domain$Organization = F3(
	function (id, name, createdAt) {
		return {createdAt: createdAt, id: id, name: name};
	});
var $author$project$Api$Decode$organizationDecoder = A4(
	$elm$json$Json$Decode$map3,
	$author$project$Domain$Organization,
	A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'createdAt', $elm$json$Json$Decode$string));
var $author$project$Api$Decode$summaryDecoder = A3(
	$author$project$Api$Decode$field,
	'goalCount',
	$elm$json$Json$Decode$int,
	A3(
		$author$project$Api$Decode$field,
		'peopleCount',
		$elm$json$Json$Decode$int,
		A3(
			$author$project$Api$Decode$field,
			'demo',
			$elm$json$Json$Decode$bool,
			A3(
				$author$project$Api$Decode$field,
				'organization',
				$author$project$Api$Decode$organizationDecoder,
				$elm$json$Json$Decode$succeed($author$project$Domain$Summary)))));
var $author$project$Domain$ReviewWarning = F2(
	function (id, warnings) {
		return {id: id, warnings: warnings};
	});
var $author$project$Domain$Workspace = function (organization) {
	return function (version) {
		return function (demo) {
			return function (people) {
				return function (goals) {
					return function (authorities) {
						return function (reviews) {
							return function (compiler) {
								return function (edges) {
									return function (events) {
										return function (decisionShare) {
											return function (reviewWarnings) {
												return {authorities: authorities, compiler: compiler, decisionShare: decisionShare, demo: demo, edges: edges, events: events, goals: goals, organization: organization, people: people, reviewWarnings: reviewWarnings, reviews: reviews, version: version};
											};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var $author$project$Domain$Authority = F5(
	function (owner, budgetLimit, canHire, canChangePrice, canApprove) {
		return {budgetLimit: budgetLimit, canApprove: canApprove, canChangePrice: canChangePrice, canHire: canHire, owner: owner};
	});
var $author$project$Api$Decode$authorityDecoder = A3(
	$author$project$Api$Decode$field,
	'canApprove',
	$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
	A3(
		$author$project$Api$Decode$field,
		'canChangePrice',
		$elm$json$Json$Decode$bool,
		A3(
			$author$project$Api$Decode$field,
			'canHire',
			$elm$json$Json$Decode$bool,
			A3(
				$author$project$Api$Decode$field,
				'budgetLimit',
				$elm$json$Json$Decode$float,
				A3(
					$author$project$Api$Decode$field,
					'owner',
					$elm$json$Json$Decode$string,
					$elm$json$Json$Decode$succeed($author$project$Domain$Authority))))));
var $author$project$Domain$Compiler = F3(
	function (errors, warnings, diagnostics) {
		return {diagnostics: diagnostics, errors: errors, warnings: warnings};
	});
var $author$project$Api$Decode$compilerDecoder = A3(
	$author$project$Api$Decode$field,
	'diagnostics',
	$elm$json$Json$Decode$list($author$project$Api$Decode$diagnosticDecoder),
	A3(
		$author$project$Api$Decode$field,
		'warnings',
		$elm$json$Json$Decode$int,
		A3(
			$author$project$Api$Decode$field,
			'errors',
			$elm$json$Json$Decode$int,
			$elm$json$Json$Decode$succeed($author$project$Domain$Compiler))));
var $author$project$Domain$Edge = F3(
	function (from, to, kind) {
		return {from: from, kind: kind, to: to};
	});
var $author$project$Domain$Node = F2(
	function (tag, contents) {
		return {contents: contents, tag: tag};
	});
var $author$project$Api$Decode$nodeDecoder = A3(
	$author$project$Api$Decode$field,
	'contents',
	$elm$json$Json$Decode$string,
	A3(
		$author$project$Api$Decode$field,
		'tag',
		$elm$json$Json$Decode$string,
		$elm$json$Json$Decode$succeed($author$project$Domain$Node)));
var $author$project$Api$Decode$edgeDecoder = A3(
	$author$project$Api$Decode$field,
	'kind',
	$elm$json$Json$Decode$string,
	A3(
		$author$project$Api$Decode$field,
		'to',
		$author$project$Api$Decode$nodeDecoder,
		A3(
			$author$project$Api$Decode$field,
			'from',
			$author$project$Api$Decode$nodeDecoder,
			$elm$json$Json$Decode$succeed($author$project$Domain$Edge))));
var $author$project$Domain$Analysis = F2(
	function (coverage, possibleCause) {
		return {coverage: coverage, possibleCause: possibleCause};
	});
var $author$project$Domain$GoalView = F7(
	function (goal, owner, active, evaluation, analysis, results, strategies) {
		return {active: active, analysis: analysis, evaluation: evaluation, goal: goal, owner: owner, results: results, strategies: strategies};
	});
var $author$project$Api$Decode$analysisDecoder = A3(
	$author$project$Api$Decode$field,
	'possibleCause',
	$elm$json$Json$Decode$string,
	A3(
		$author$project$Api$Decode$field,
		'coverage',
		$elm$json$Json$Decode$float,
		$elm$json$Json$Decode$succeed($author$project$Domain$Analysis)));
var $author$project$Domain$Goal = F8(
	function (id, description, metric, baseline, target, deadline, requiredBudget, requiredPermissions) {
		return {baseline: baseline, deadline: deadline, description: description, id: id, metric: metric, requiredBudget: requiredBudget, requiredPermissions: requiredPermissions, target: target};
	});
var $author$project$Domain$Metric = F4(
	function (id, name, unit, direction) {
		return {direction: direction, id: id, name: name, unit: unit};
	});
var $author$project$Api$Decode$metricDecoder = A3(
	$author$project$Api$Decode$field,
	'direction',
	$elm$json$Json$Decode$string,
	A3(
		$author$project$Api$Decode$field,
		'unit',
		$elm$json$Json$Decode$string,
		A3(
			$author$project$Api$Decode$field,
			'name',
			$elm$json$Json$Decode$string,
			A3(
				$author$project$Api$Decode$field,
				'id',
				$elm$json$Json$Decode$string,
				$elm$json$Json$Decode$succeed($author$project$Domain$Metric)))));
var $author$project$Api$Decode$goalDecoder = A3(
	$author$project$Api$Decode$field,
	'requiredPermissions',
	$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
	A3(
		$author$project$Api$Decode$field,
		'requiredBudget',
		$elm$json$Json$Decode$float,
		A3(
			$author$project$Api$Decode$field,
			'deadline',
			$elm$json$Json$Decode$string,
			A3(
				$author$project$Api$Decode$field,
				'target',
				$elm$json$Json$Decode$float,
				A3(
					$author$project$Api$Decode$field,
					'baseline',
					$elm$json$Json$Decode$float,
					A3(
						$author$project$Api$Decode$field,
						'metric',
						$author$project$Api$Decode$metricDecoder,
						A3(
							$author$project$Api$Decode$field,
							'description',
							$elm$json$Json$Decode$string,
							A3(
								$author$project$Api$Decode$field,
								'id',
								$elm$json$Json$Decode$string,
								$elm$json$Json$Decode$succeed($author$project$Domain$Goal)))))))));
var $author$project$Domain$Measurement = F4(
	function (value, reportedAt, note, reportedBy) {
		return {note: note, reportedAt: reportedAt, reportedBy: reportedBy, value: value};
	});
var $author$project$Api$Decode$measurementDecoder = A2(
	$author$project$Api$Decode$andMap,
	A2($author$project$Api$Decode$optional, 'reportedBy', $elm$json$Json$Decode$string),
	A3(
		$author$project$Api$Decode$field,
		'note',
		$elm$json$Json$Decode$string,
		A3(
			$author$project$Api$Decode$field,
			'reportedAt',
			$elm$json$Json$Decode$string,
			A3(
				$author$project$Api$Decode$field,
				'value',
				$elm$json$Json$Decode$float,
				$elm$json$Json$Decode$succeed($author$project$Domain$Measurement)))));
var $author$project$Api$Decode$goalViewDecoder = A3(
	$author$project$Api$Decode$field,
	'strategies',
	$elm$json$Json$Decode$list(
		A3(
			$elm$json$Json$Decode$map2,
			$elm$core$Tuple$pair,
			A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$string),
			A2($elm$json$Json$Decode$index, 1, $elm$json$Json$Decode$string))),
	A3(
		$author$project$Api$Decode$field,
		'results',
		$elm$json$Json$Decode$list($author$project$Api$Decode$measurementDecoder),
		A3(
			$author$project$Api$Decode$field,
			'analysis',
			$elm$json$Json$Decode$oneOf(
				_List_fromArray(
					[
						$author$project$Api$Decode$analysisDecoder,
						$elm$json$Json$Decode$null(
						A2($author$project$Domain$Analysis, 0, '구조 분석 대기'))
					])),
			A3(
				$author$project$Api$Decode$field,
				'evaluation',
				$author$project$Api$Decode$evaluationDecoder,
				A3(
					$author$project$Api$Decode$field,
					'active',
					$elm$json$Json$Decode$bool,
					A2(
						$author$project$Api$Decode$andMap,
						A2($author$project$Api$Decode$optional, 'owner', $elm$json$Json$Decode$string),
						A3(
							$author$project$Api$Decode$field,
							'goal',
							$author$project$Api$Decode$goalDecoder,
							$elm$json$Json$Decode$succeed($author$project$Domain$GoalView))))))));
var $author$project$Api$Decode$workspaceDecoder = A3(
	$author$project$Api$Decode$field,
	'reviewWarnings',
	$elm$json$Json$Decode$list(
		A3(
			$elm$json$Json$Decode$map2,
			$author$project$Domain$ReviewWarning,
			A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string),
			A2(
				$elm$json$Json$Decode$field,
				'warnings',
				$elm$json$Json$Decode$list($elm$json$Json$Decode$string)))),
	A3(
		$author$project$Api$Decode$field,
		'decisionShare',
		$elm$json$Json$Decode$dict($elm$json$Json$Decode$float),
		A3(
			$author$project$Api$Decode$field,
			'events',
			$elm$json$Json$Decode$list($author$project$Api$Decode$auditDecoder),
			A2(
				$author$project$Api$Decode$andMap,
				A2(
					$elm$json$Json$Decode$at,
					_List_fromArray(
						['graph', 'edges']),
					$elm$json$Json$Decode$list($author$project$Api$Decode$edgeDecoder)),
				A3(
					$author$project$Api$Decode$field,
					'compiler',
					$author$project$Api$Decode$compilerDecoder,
					A3(
						$author$project$Api$Decode$field,
						'reviews',
						$elm$json$Json$Decode$list($author$project$Api$Decode$reviewDecoder),
						A3(
							$author$project$Api$Decode$field,
							'authorities',
							$elm$json$Json$Decode$list($author$project$Api$Decode$authorityDecoder),
							A3(
								$author$project$Api$Decode$field,
								'goals',
								$elm$json$Json$Decode$list($author$project$Api$Decode$goalViewDecoder),
								A3(
									$author$project$Api$Decode$field,
									'people',
									$elm$json$Json$Decode$list($author$project$Api$Decode$personDecoder),
									A3(
										$author$project$Api$Decode$field,
										'demo',
										$elm$json$Json$Decode$bool,
										A3(
											$author$project$Api$Decode$field,
											'version',
											$elm$json$Json$Decode$int,
											A3(
												$author$project$Api$Decode$field,
												'organization',
												$author$project$Api$Decode$organizationDecoder,
												$elm$json$Json$Decode$succeed($author$project$Domain$Workspace)))))))))))));
var $author$project$DecoderTest$tests = A2(
	$elm_explorations$test$Test$describe,
	'HTTP JSON contract',
	_List_fromArray(
		[
			A2(
			$elm_explorations$test$Test$test,
			'legacy person defaults to active without optional profile',
			function (_v0) {
				return A2(
					$elm_explorations$test$Expect$equal,
					$elm$core$Result$Ok(
						{active: true, department: $elm$core$Maybe$Nothing, email: $elm$core$Maybe$Nothing, id: 'p', name: 'Name', reportsTo: $elm$core$Maybe$Nothing, role: 'Role'}),
					A2($elm$json$Json$Decode$decodeString, $author$project$Api$Decode$personDecoder, '{"id":"p","name":"Name","role":"Role"}'));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'inactive person retains profile and report link',
			function (_v1) {
				return A2(
					$elm_explorations$test$Expect$equal,
					$elm$core$Result$Ok(
						{
							active: false,
							department: $elm$core$Maybe$Just('Team'),
							email: $elm$core$Maybe$Just('a@example.com'),
							id: 'p',
							name: 'Name',
							reportsTo: $elm$core$Maybe$Just('boss'),
							role: 'Role'
						}),
					A2($elm$json$Json$Decode$decodeString, $author$project$Api$Decode$personDecoder, '{"id":"p","name":"Name","role":"Role","reportsTo":"boss","department":"Team","email":"a@example.com","status":"inactive"}'));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'unknown employment status and malformed profile are rejected',
			function (_v2) {
				return A2(
					$elm_explorations$test$Expect$all,
					_List_fromArray(
						[
							function (_v3) {
							return $elm_explorations$test$Expect$err(
								A2($elm$json$Json$Decode$decodeString, $author$project$Api$Decode$personDecoder, '{"id":"p","name":"Name","role":"Role","status":"unknown"}'));
						},
							function (_v4) {
							return $elm_explorations$test$Expect$err(
								A2($elm$json$Json$Decode$decodeString, $author$project$Api$Decode$personDecoder, '{"id":"p","name":"Name","role":"Role","email":42}'));
						}
						]),
					_Utils_Tuple0);
			}),
			A2(
			$elm_explorations$test$Test$test,
			'optional measurement is absent or null, never silently mistyped',
			function (_v5) {
				return A2(
					$elm_explorations$test$Expect$all,
					_List_fromArray(
						[
							function (_v6) {
							return A2(
								$elm_explorations$test$Expect$equal,
								$elm$core$Result$Ok(
									{latestValue: $elm$core$Maybe$Nothing, progress: 0, status: $author$project$Domain$NoData}),
								A2($elm$json$Json$Decode$decodeString, $author$project$Api$Decode$evaluationDecoder, '{\"status\":\"NoData\",\"progress\":0}'));
						},
							function (_v7) {
							return A2(
								$elm_explorations$test$Expect$equal,
								$elm$core$Result$Ok($elm$core$Maybe$Nothing),
								A2(
									$elm$json$Json$Decode$decodeString,
									A2($author$project$Api$Decode$optional, 'latestValue', $elm$json$Json$Decode$float),
									'{\"latestValue\":null}'));
						},
							function (_v8) {
							return $elm_explorations$test$Expect$err(
								A2($elm$json$Json$Decode$decodeString, $author$project$Api$Decode$evaluationDecoder, '{\"status\":\"NoData\",\"progress\":0,\"latestValue\":\"bad\"}'));
						}
						]),
					_Utils_Tuple0);
			}),
			A2(
			$elm_explorations$test$Test$test,
			'unknown performance states are rejected',
			function (_v9) {
				return $elm_explorations$test$Expect$err(
					A2($elm$json$Json$Decode$decodeString, $author$project$Api$Decode$statusDecoder, '\"FutureState\"'));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'all known performance states decode',
			function (_v10) {
				return A2(
					$elm_explorations$test$Expect$equal,
					A2(
						$elm$core$List$map,
						$elm$core$Result$Ok,
						_List_fromArray(
							[$author$project$Domain$NoData, $author$project$Domain$OnTrack, $author$project$Domain$AtRisk, $author$project$Domain$OffTrack, $author$project$Domain$Achieved])),
					A2(
						$elm$core$List$map,
						function (s) {
							return A2($elm$json$Json$Decode$decodeString, $author$project$Api$Decode$statusDecoder, '\"' + (s + '\"'));
						},
						_List_fromArray(
							['NoData', 'OnTrack', 'AtRisk', 'OffTrack', 'Achieved'])));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'summary requires counts and organization metadata',
			function (_v11) {
				return $elm_explorations$test$Expect$err(
					A2($elm$json$Json$Decode$decodeString, $author$project$Api$Decode$summaryDecoder, '{\"organization\":{\"id\":\"o\",\"name\":\"A\",\"createdAt\":\"now\"},\"demo\":false,\"peopleCount\":0}'));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'workspace follows nested graph and compiler response contract',
			function (_v12) {
				return A2(
					$elm_explorations$test$Expect$equal,
					$elm$core$Result$Ok(
						_Utils_Tuple3('o', 2, _List_Nil)),
					A2(
						$elm$core$Result$map,
						function (w) {
							return _Utils_Tuple3(w.organization.id, w.version, w.edges);
						},
						A2($elm$json$Json$Decode$decodeString, $author$project$Api$Decode$workspaceDecoder, '{"organization":{"id":"o","name":"A","createdAt":"now"},"version":2,"demo":false,"people":[],"goals":[],"authorities":[],"reviews":[],"compiler":{"errors":0,"warnings":0,"diagnostics":[]},"graph":{"edges":[]},"events":[],"decisionShare":{},"reviewWarnings":[]}')));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'goal evaluation audit uses tagged event tuple',
			function (_v13) {
				return A2(
					$elm_explorations$test$Expect$equal,
					$elm$core$Result$Ok(
						_Utils_Tuple2(
							$elm$core$Maybe$Just('goal-1'),
							$elm$core$Maybe$Just($author$project$Domain$OnTrack))),
					A2(
						$elm$core$Result$map,
						function (a) {
							return _Utils_Tuple2(a.evaluatedGoal, a.evaluatedStatus);
						},
						A2($elm$json$Json$Decode$decodeString, $author$project$Api$Decode$auditDecoder, '{"record":{"seq":3,"at":"now","actor":null,"event":{"tag":"GoalEvaluated","contents":["goal-1",{"status":"OnTrack"}]}},"description":"evaluated"}')));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'review learning objects and optional decision deadline decode',
			function (_v14) {
				return A2(
					$elm_explorations$test$Expect$equal,
					$elm$core$Result$Ok(
						_Utils_Tuple2(
							_List_fromArray(
								['learned']),
							_List_fromArray(
								[
									{deadline: $elm$core$Maybe$Nothing, owner: 'p', text: 'next'}
								]))),
					A2(
						$elm$core$Result$map,
						function (r) {
							return _Utils_Tuple2(r.learnings, r.decisions);
						},
						A2($elm$json$Json$Decode$decodeString, $author$project$Api$Decode$reviewDecoder, '{"id":"r","goal":"g","heldAt":"now","note":"review","evaluation":{"status":"NoData","progress":0},"learnings":[{"text":"learned"}],"decisions":[{"text":"next","owner":"p"}]}')));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'scoped owner assignment retains structural IDs independent of description',
			function (_v15) {
				return A2(
					$elm_explorations$test$Expect$equal,
					$elm$core$Result$Ok(
						_Utils_Tuple3(
							'OwnerAssigned',
							'goal-a',
							$elm$core$Maybe$Just('person-a'))),
					A2(
						$elm$core$Result$map,
						function (a) {
							return _Utils_Tuple3(a.activity.tag, a.activity.targetId, a.activity.personId);
						},
						A2($elm$json$Json$Decode$decodeString, $author$project$Api$Decode$auditDecoder, '{"record":{"seq":4,"at":"2026-09-07T00:00:00Z","actor":null,"event":{"tag":"OrganizationScoped","contents":["org-a",{"tag":"OwnerAssigned","contents":["goal-a","person-a"]}]}},"description":"arbitrary prose"}')));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'review event retains both goal and exact review identity',
			function (_v16) {
				return A2(
					$elm_explorations$test$Expect$equal,
					$elm$core$Result$Ok(
						_Utils_Tuple2(
							'goal-a',
							$elm$core$Maybe$Just('review-a'))),
					A2(
						$elm$core$Result$map,
						function (a) {
							return _Utils_Tuple2(a.activity.targetId, a.activity.reviewId);
						},
						A2($elm$json$Json$Decode$decodeString, $author$project$Api$Decode$auditDecoder, '{"record":{"seq":5,"at":"2026-09-07T00:00:00Z","actor":"person-a","event":{"tag":"ReviewHeld","contents":{"id":"review-a","goal":"goal-a","note":"회고"}}},"description":"review"}')));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'unknown event preserves raw payload for inspection',
			function (_v17) {
				return A2(
					$elm_explorations$test$Expect$equal,
					$elm$core$Result$Ok(
						_Utils_Tuple2('FutureEvent', true)),
					A2(
						$elm$core$Result$map,
						function (a) {
							return _Utils_Tuple2(
								a.activity.tag,
								A2($elm$core$String$contains, 'futureField', a.activity.raw));
						},
						A2($elm$json$Json$Decode$decodeString, $author$project$Api$Decode$auditDecoder, '{"record":{"seq":6,"at":"2026-09-07T00:00:00Z","actor":null,"event":{"tag":"FutureEvent","contents":{"futureField":"kept"}}},"description":"future"}')));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'employee profile events extract person ID from tuple rather than prose',
			function (_v18) {
				return A2(
					$elm_explorations$test$Expect$equal,
					$elm$core$Result$Ok(
						_Utils_Tuple2('person', 'person-a')),
					A2(
						$elm$core$Result$map,
						function (a) {
							return _Utils_Tuple2(a.activity.targetKind, a.activity.targetId);
						},
						A2($elm$json$Json$Decode$decodeString, $author$project$Api$Decode$auditDecoder, '{"record":{"seq":7,"at":"2026-09-07T00:00:00Z","actor":null,"event":{"tag":"PersonUpdated","contents":[{"id":"person-a","name":"새 이름","role":"개발"},{"department":"제품"}]}},"description":"old name person-b"}')));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'scoped evaluation continues to drive guide progress',
			function (_v19) {
				return A2(
					$elm_explorations$test$Expect$equal,
					$elm$core$Result$Ok(
						_Utils_Tuple2(
							$elm$core$Maybe$Just('demo-revenue'),
							$elm$core$Maybe$Just($author$project$Domain$Achieved))),
					A2(
						$elm$core$Result$map,
						function (a) {
							return _Utils_Tuple2(a.evaluatedGoal, a.evaluatedStatus);
						},
						A2($elm$json$Json$Decode$decodeString, $author$project$Api$Decode$auditDecoder, '{"record":{"seq":8,"at":"2026-09-07T00:00:00Z","actor":null,"event":{"tag":"OrganizationScoped","contents":["org-a",{"tag":"GoalEvaluated","contents":["demo-revenue",{"status":"Achieved"}]}]}},"description":"evaluated"}')));
			})
		]));
var $author$project$Page$Discovery = {$: 'Discovery'};
var $author$project$Domain$Discovery$WorkflowField = F3(
	function (a, b, c) {
		return {$: 'WorkflowField', a: a, b: b, c: c};
	});
var $author$project$Page$Workflows = {$: 'Workflows'};
var $author$project$DiscoveryPageTest$controls = function (doc) {
	return {
		addObservation: _Utils_Tuple0,
		addWorkflow: _Utils_Tuple0,
		busy: false,
		edit: $elm$core$Basics$always(_Utils_Tuple0),
		go: $elm$core$Basics$always(_Utils_Tuple0),
		org: 'a',
		rebase: _Utils_Tuple0,
		reset: _Utils_Tuple0,
		save: _Utils_Tuple0,
		state: A3(
			$author$project$App$Discovery$receive,
			'a',
			$elm$core$Result$Ok(
				{discovery: doc, version: 1}),
			$author$project$App$Discovery$init)
	};
};
var $author$project$Page$Discovery$guide = F4(
	function (expanded, toggle, go, doc) {
		var workflowsDone = !$elm$core$List$isEmpty(doc.workflows);
		var scopeDone = ($elm$core$String$trim(doc.scope) !== '') && (!$elm$core$List$isEmpty(doc.observations));
		var reviewed = workflowsDone && (doc.review.status === 'reviewed');
		var next = (!scopeDone) ? $author$project$Page$Discovery : ((!workflowsDone) ? $author$project$Page$Workflows : $author$project$Page$AgentDrafts);
		return A2(
			$elm$html$Html$section,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('journey-guide')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('actions')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$strong,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('현황 기록 → 업무 연결 → 에이전트 초안 검토')
								])),
							A2(
							$elm$html$Html$button,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$type_('button'),
									$elm$html$Html$Attributes$class('secondary'),
									$elm$html$Html$Events$onClick(
									go(next))
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(
									reviewed ? '검토 완료 · 다시 살펴보기 →' : ('다음 · ' + ($author$project$Page$pageName(next) + ' →')))
								])),
							A2(
							$elm$html$Html$button,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$type_('button'),
									$elm$html$Html$Attributes$class('secondary'),
									$elm$html$Html$Events$onClick(toggle),
									A2(
									$elm$html$Html$Attributes$attribute,
									'aria-expanded',
									expanded ? 'true' : 'false')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(
									expanded ? '단계 안내 접기' : '단계 안내 펼치기')
								]))
						])),
					expanded ? A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							$author$project$Ui$Common$note('진행 상태는 저장된 입력으로 계산합니다. 미확인 사항은 남겨도 됩니다. 목표 활성화나 진단 해소는 현황 입력의 완료 조건이 아닙니다.'),
							A2(
							$elm$html$Html$ul,
							_List_Nil,
							A2(
								$elm$core$List$map,
								function (_v0) {
									var done = _v0.a;
									var title = _v0.b;
									var page = _v0.c;
									return A2(
										$elm$html$Html$li,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text(
												(done ? '입력됨 · ' : '진행 전 · ') + (title + ' ')),
												A2(
												$elm$html$Html$button,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$type_('button'),
														$elm$html$Html$Attributes$class('secondary'),
														$elm$html$Html$Events$onClick(
														go(page))
													]),
												_List_fromArray(
													[
														$elm$html$Html$text('살펴보기')
													]))
											]));
								},
								_List_fromArray(
									[
										_Utils_Tuple3(scopeDone, '1. 조직 범위와 사실·미확인 기록', $author$project$Page$Discovery),
										_Utils_Tuple3(workflowsDone, '2. 업무의 입력·산출물·인계 연결', $author$project$Page$Workflows),
										_Utils_Tuple3(reviewed, '3. 규칙 기반 제안을 사람이 검토', $author$project$Page$AgentDrafts)
									])))
						])) : $elm$html$Html$text('')
				]));
	});
var $author$project$App$Discovery$changed = F2(
	function (org, state) {
		return !_Utils_eq(
			A2(
				$elm$core$Maybe$map,
				function ($) {
					return $.discovery;
				},
				A2($author$project$App$Discovery$current, org, state)),
			A2(
				$elm$core$Maybe$map,
				function ($) {
					return $.discovery;
				},
				A2($author$project$App$Discovery$saved, org, state)));
	});
var $author$project$Page$Discovery$documentSummary = function (doc) {
	return A2(
		$elm$core$String$join,
		'\n',
		_Utils_ap(
			_List_fromArray(
				['범위: ' + doc.scope, '기준일: ' + doc.asOf]),
			_Utils_ap(
				A2(
					$elm$core$List$map,
					function (o) {
						return o.subject + (' / ' + ($author$project$Domain$Discovery$statusLabel(o.status) + (' / ' + (o.detail + (' / 근거: ' + o.evidence)))));
					},
					doc.observations),
				_Utils_ap(
					A2(
						$elm$core$List$map,
						function (w) {
							return A2(
								$elm$core$String$join,
								' / ',
								_List_fromArray(
									[
										w.name,
										w.role,
										w.trigger,
										w.inputs,
										w.tools,
										w.outputs,
										w.handoff,
										w.approval,
										$author$project$Domain$Discovery$statusLabel(w.status),
										w.evidence
									]));
						},
						doc.workflows),
					_List_fromArray(
						['검토: ' + (doc.review.status + (' / ' + doc.review.note))])))));
};
var $author$project$Domain$Discovery$AsOf = function (a) {
	return {$: 'AsOf', a: a};
};
var $author$project$Domain$Discovery$Scope = function (a) {
	return {$: 'Scope', a: a};
};
var $author$project$Domain$Discovery$ObservationField = F3(
	function (a, b, c) {
		return {$: 'ObservationField', a: a, b: b, c: c};
	});
var $author$project$Domain$Discovery$RemoveObservation = function (a) {
	return {$: 'RemoveObservation', a: a};
};
var $author$project$Page$Discovery$evidenceHint = function (status) {
	return (status === 'confirmed') ? '필수: 문서명·확인한 담당자·확인 날짜 등 확인 가능한 근거를 적으세요.' : '예: 9월 운영 매뉴얼, 담당자 인터뷰 또는 확인할 사람과 질문';
};
var $author$project$Page$Discovery$statusField = F3(
	function (key, status, edit) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A6(
					$author$project$Ui$Form$selectValue,
					key,
					status,
					edit,
					'정보 구분',
					true,
					_List_fromArray(
						[
							_Utils_Tuple2('unknown', '미확인'),
							_Utils_Tuple2('confirmed', '확인된 사실'),
							_Utils_Tuple2('proposed', '개선안')
						])),
					$author$project$Ui$Common$note('확인된 사실: 근거가 있는 현재 정보 · 미확인: 추가 확인 필요 · 개선안: 앞으로 바꾸고 싶은 내용')
				]));
	});
var $author$project$Page$Discovery$observation = F2(
	function (controls, item) {
		return A2(
			$elm$html$Html$section,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('discovery-item')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$h3,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							(item.subject === '') ? '새 현황 항목 · 미확인' : item.subject)
						])),
					A7(
					$author$project$Ui$Form$guidedInput,
					item.id + '-subject',
					'현황 항목',
					'예: 고객지원팀의 환불 승인 권한',
					'text',
					true,
					item.subject,
					A2(
						$elm$core$Basics$composeL,
						controls.edit,
						A2($author$project$Domain$Discovery$ObservationField, item.id, 'subject'))),
					A6(
					$author$project$Ui$Form$guidedArea,
					item.id + '-detail',
					'내용',
					'현재 알고 있는 내용만 적으세요. 미확인은 부분 입력도 가능합니다.',
					false,
					item.detail,
					A2(
						$elm$core$Basics$composeL,
						controls.edit,
						A2($author$project$Domain$Discovery$ObservationField, item.id, 'detail'))),
					A3(
					$author$project$Page$Discovery$statusField,
					item.id + '-status',
					item.status,
					A2(
						$elm$core$Basics$composeL,
						controls.edit,
						A2($author$project$Domain$Discovery$ObservationField, item.id, 'status'))),
					A6(
					$author$project$Ui$Form$guidedArea,
					item.id + '-evidence',
					'입력 근거 / 확인할 곳',
					$author$project$Page$Discovery$evidenceHint(item.status),
					item.status === 'confirmed',
					item.evidence,
					A2(
						$elm$core$Basics$composeL,
						controls.edit,
						A2($author$project$Domain$Discovery$ObservationField, item.id, 'evidence'))),
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$type_('button'),
							$elm$html$Html$Attributes$class('secondary'),
							$elm$html$Html$Events$onClick(
							controls.edit(
								$author$project$Domain$Discovery$RemoveObservation(item.id)))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('이 항목 제외 · 저장 전 취소 가능')
						]))
				]));
	});
var $author$project$Page$Discovery$overview = F2(
	function (controls, doc) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$author$project$Ui$Common$panel,
					'1 · 현재 조직의 범위를 정하세요',
					_List_fromArray(
						[
							$author$project$Ui$Common$note('누구의 어떤 시점 정보를 정리하는지 먼저 맞춥니다. 예: 고객지원팀의 9월 운영 현황'),
							A6(
							$author$project$Ui$Form$guidedArea,
							'discovery-scope',
							'분석 범위',
							'포함하는 팀·업무와 조사 목적을 적으세요. 예: 고객지원팀 문의 접수부터 해결까지',
							false,
							doc.scope,
							A2($elm$core$Basics$composeL, controls.edit, $author$project$Domain$Discovery$Scope)),
							A7(
							$author$project$Ui$Form$guidedInput,
							'discovery-asof',
							'현황 기준일',
							'이 정보가 유효한 날짜입니다. 확인 전이면 비워 두세요.',
							'date',
							false,
							doc.asOf,
							A2($elm$core$Basics$composeL, controls.edit, $author$project$Domain$Discovery$AsOf))
						])),
					A2(
					$author$project$Ui$Common$panel,
					'2 · 확인한 내용과 모르는 내용을 나누세요',
					_Utils_ap(
						_List_fromArray(
							[
								$author$project$Ui$Common$note('역할, 책임, 보고 관계, 결정 권한 등을 기록하세요. 예: 긴급 환불 승인자는 미확인 · 재무팀에 확인 예정. 권한 없음은 ‘없음’이라고 명시하고 근거를 남기세요.')
							]),
						_Utils_ap(
							A2(
								$elm$core$List$map,
								$author$project$Page$Discovery$observation(controls),
								doc.observations),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$button,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$type_('button'),
											$elm$html$Html$Attributes$class('secondary'),
											$elm$html$Html$Events$onClick(controls.addObservation)
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('+ 현황 항목 추가')
										]))
								]))))
				]));
	});
var $author$project$Domain$Discovery$ReviewNote = function (a) {
	return {$: 'ReviewNote', a: a};
};
var $author$project$Domain$Discovery$ReviewStatus = function (a) {
	return {$: 'ReviewStatus', a: a};
};
var $author$project$Page$Discovery$sourceChanged = F2(
	function (doc, latest) {
		return (!_Utils_eq(
			_Utils_Tuple3(doc.scope, doc.asOf, doc.observations),
			_Utils_Tuple3(latest.scope, latest.asOf, latest.observations))) || (!_Utils_eq(doc.workflows, latest.workflows));
	});
var $author$project$Page$Discovery$reviewForm = F3(
	function (doc, latest, controls) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$author$project$Ui$Common$panel,
					'사람의 검토와 수정 의견',
					_List_fromArray(
						[
							$author$project$Ui$Common$note('초안과 설계안은 에이전트 초안 화면 위쪽에 있습니다. 여기서는 저장된 근거와 미확인 사항을 사람이 검토했는지 기록합니다.'),
							$author$project$Ui$Common$note(
							(latest.review.status === 'reviewed') ? '저장 상태: 검토 완료. 입력 근거가 바뀌면 다시 검토해야 합니다.' : '저장 상태: 검토 대기. 역할 중복, 인계 누락과 사람 승인 조건을 확인하세요.'),
							A6(
							$author$project$Ui$Form$guidedArea,
							'agent-review-note',
							'검토 의견 / 수정할 제안',
							'예: 분류와 답변 역할을 분리하고 환불 실행은 사람 승인 후에만 허용',
							false,
							doc.review.note,
							A2($elm$core$Basics$composeL, controls.edit, $author$project$Domain$Discovery$ReviewNote)),
							A2(
							$elm$html$Html$label,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('review-confirmation')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$input,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$type_('checkbox'),
											$elm$html$Html$Attributes$checked(doc.review.status === 'reviewed'),
											$elm$html$Html$Attributes$disabled(
											A2($author$project$Page$Discovery$sourceChanged, doc, latest) || $elm$core$List$isEmpty(latest.workflows)),
											$elm$html$Html$Events$onCheck(
											function (checked_) {
												return controls.edit(
													$author$project$Domain$Discovery$ReviewStatus(
														checked_ ? 'reviewed' : 'pending'));
											})
										]),
									_List_Nil),
									$elm$html$Html$text('저장된 근거와 미확인 사항을 검토했습니다')
								])),
							A2($author$project$Page$Discovery$sourceChanged, doc, latest) ? $author$project$Ui$Common$note('업무 또는 현황에 미저장 변경이 있습니다. 먼저 저장하면 새 근거를 바탕으로 검토할 수 있습니다.') : $elm$html$Html$text('')
						]))
				]));
	});
var $author$project$Domain$Discovery$RemoveWorkflow = function (a) {
	return {$: 'RemoveWorkflow', a: a};
};
var $author$project$Domain$Discovery$WorkflowApprovalPermission = F2(
	function (a, b) {
		return {$: 'WorkflowApprovalPermission', a: a, b: b};
	});
var $author$project$Domain$Discovery$WorkflowApprovalPerson = F2(
	function (a, b) {
		return {$: 'WorkflowApprovalPerson', a: a, b: b};
	});
var $author$project$Domain$Discovery$WorkflowHandoff = F3(
	function (a, b, c) {
		return {$: 'WorkflowHandoff', a: a, b: b, c: c};
	});
var $author$project$Domain$Discovery$WorkflowRolePerson = F2(
	function (a, b) {
		return {$: 'WorkflowRolePerson', a: a, b: b};
	});
var $author$project$Page$Discovery$references = F4(
	function (controls, workspace, doc, item) {
		var permissionOptions = A2(
			$elm$core$List$cons,
			_Utils_Tuple2('', '권한 선택 안 함'),
			$author$project$Ui$Label$permissions);
		var others = A2(
			$elm$core$List$filter,
			function (w) {
				return !_Utils_eq(w.id, item.id);
			},
			doc.workflows);
		return A2(
			$elm$html$Html$fieldset,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('form-section')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$legend,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('참조 연결 · 조직 데이터와 이어지는 정보')
						])),
					$author$project$Ui$Common$note('텍스트로 적은 담당자, 승인 조건, 인계 대상을 등록된 구성원, 결정 권한, 다른 업무에 연결합니다. 모르면 비워 두세요.'),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('fields')
						]),
					_List_fromArray(
						[
							A6(
							$author$project$Ui$Form$selectValue,
							item.id + '-role-person',
							A2($elm$core$Maybe$withDefault, '', item.rolePerson),
							A2(
								$elm$core$Basics$composeL,
								controls.edit,
								$author$project$Domain$Discovery$WorkflowRolePerson(item.id)),
							'담당 구성원',
							false,
							$author$project$Ui$Form$peopleOptions(workspace)),
							A6(
							$author$project$Ui$Form$selectValue,
							item.id + '-approval-person',
							A2($elm$core$Maybe$withDefault, '', item.approvalPerson),
							A2(
								$elm$core$Basics$composeL,
								controls.edit,
								$author$project$Domain$Discovery$WorkflowApprovalPerson(item.id)),
							'승인 구성원',
							false,
							$author$project$Ui$Form$peopleOptions(workspace)),
							A6(
							$author$project$Ui$Form$selectValue,
							item.id + '-approval-permission',
							A2($elm$core$Maybe$withDefault, '', item.approvalPermission),
							A2(
								$elm$core$Basics$composeL,
								controls.edit,
								$author$project$Domain$Discovery$WorkflowApprovalPermission(item.id)),
							'승인에 필요한 결정 권한',
							false,
							permissionOptions)
						])),
					$elm$core$List$isEmpty(others) ? $author$project$Ui$Common$note('인계 대상으로 연결할 다른 업무가 아직 없습니다.') : A2(
					$elm$html$Html$fieldset,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('permission-fields')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$legend,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('인계 대상 업무')
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('checks')
								]),
							A2(
								$elm$core$List$map,
								function (other) {
									return A2(
										$elm$html$Html$label,
										_List_Nil,
										_List_fromArray(
											[
												A2(
												$elm$html$Html$input,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$type_('checkbox'),
														$elm$html$Html$Attributes$checked(
														A2($elm$core$List$member, other.id, item.handoffWorkflows)),
														$elm$html$Html$Events$onCheck(
														A2(
															$elm$core$Basics$composeL,
															controls.edit,
															A2($author$project$Domain$Discovery$WorkflowHandoff, item.id, other.id)))
													]),
												_List_Nil),
												$elm$html$Html$text(
												($elm$core$String$trim(other.name) === '') ? ('이름 없는 업무 (' + (other.id + ')')) : other.name)
											]));
								},
								others))
						]))
				]));
	});
var $author$project$Page$Discovery$workflow = F4(
	function (controls, workspace, doc, item) {
		return A2(
			$elm$html$Html$section,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('panel discovery-item')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$h2,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							(item.name === '') ? '새 업무 흐름' : item.name)
						])),
					A7(
					$author$project$Ui$Form$guidedInput,
					item.id + '-name',
					'업무 이름',
					'예: 고객 문의 분류와 답변',
					'text',
					true,
					item.name,
					A2(
						$elm$core$Basics$composeL,
						controls.edit,
						A2($author$project$Domain$Discovery$WorkflowField, item.id, 'name'))),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('fields')
						]),
					A2(
						$elm$core$List$map,
						function (_v0) {
							var key = _v0.a;
							var title = _v0.b;
							var _v1 = _v0.c;
							var hint = _v1.a;
							var value_ = _v1.b;
							return A6(
								$author$project$Ui$Form$guidedArea,
								item.id + ('-' + key),
								title,
								hint,
								false,
								value_,
								A2(
									$elm$core$Basics$composeL,
									controls.edit,
									A2($author$project$Domain$Discovery$WorkflowField, item.id, key)));
						},
						_List_fromArray(
							[
								_Utils_Tuple3(
								'role',
								'현재 담당 역할 / 구성원',
								_Utils_Tuple2('직급보다 실제 책임을 적으세요. 예: 고객지원 담당 김민서', item.role)),
								_Utils_Tuple3(
								'trigger',
								'시작 조건',
								_Utils_Tuple2('무엇이 발생하면 시작하나요? 예: 새 문의 접수', item.trigger)),
								_Utils_Tuple3(
								'inputs',
								'입력 정보',
								_Utils_Tuple2('예: 문의 내용, 고객 계약 정보', item.inputs)),
								_Utils_Tuple3(
								'tools',
								'현재 사용하는 도구',
								_Utils_Tuple2('예: CRM, 고객지원 문서. 실제 접근 권한은 별도 확인합니다.', item.tools)),
								_Utils_Tuple3(
								'outputs',
								'산출물',
								_Utils_Tuple2('예: 문의 분류와 답변 초안', item.outputs)),
								_Utils_Tuple3(
								'handoff',
								'전달 대상 / 인계 조건',
								_Utils_Tuple2('예: 환불 문의는 재무 담당자에게 금액과 사유 전달', item.handoff)),
								_Utils_Tuple3(
								'approval',
								'사람의 승인 조건',
								_Utils_Tuple2('예: 환불 집행 전 재무 책임자 승인. 없음과 미확인을 구분하세요.', item.approval))
							]))),
					A4($author$project$Page$Discovery$references, controls, workspace, doc, item),
					A3(
					$author$project$Page$Discovery$statusField,
					item.id + '-status',
					item.status,
					A2(
						$elm$core$Basics$composeL,
						controls.edit,
						A2($author$project$Domain$Discovery$WorkflowField, item.id, 'status'))),
					A6(
					$author$project$Ui$Form$guidedArea,
					item.id + '-evidence',
					'입력 근거 / 확인할 곳',
					$author$project$Page$Discovery$evidenceHint(item.status),
					item.status === 'confirmed',
					item.evidence,
					A2(
						$elm$core$Basics$composeL,
						controls.edit,
						A2($author$project$Domain$Discovery$WorkflowField, item.id, 'evidence'))),
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$type_('button'),
							$elm$html$Html$Attributes$class('secondary'),
							$elm$html$Html$Events$onClick(
							controls.edit(
								$author$project$Domain$Discovery$RemoveWorkflow(item.id)))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('이 업무 제외 · 저장 전 취소 가능')
						]))
				]));
	});
var $author$project$Page$Discovery$workflows = F3(
	function (controls, workspace, doc) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$author$project$Ui$Common$panel,
					'업무가 시작되어 다른 역할로 전달되는 흐름',
					_List_fromArray(
						[
							$author$project$Ui$Common$note('한 업무가 시작되는 조건부터 입력, 도구, 산출물, 다음 전달 대상을 적습니다. 완벽히 알지 못해도 업무 이름과 미확인 상태로 시작할 수 있습니다.'),
							$author$project$Ui$Common$note(
							'등록된 구성원 역할 참고: ' + ($elm$core$List$isEmpty(workspace.people) ? '구성원 화면에서 현재 담당자를 등록할 수 있습니다.' : A2(
								$elm$core$String$join,
								' · ',
								A2(
									$elm$core$List$map,
									function (p) {
										return p.name + (' / ' + p.role);
									},
									workspace.people)))),
							$author$project$Ui$Common$note('예: 문의 접수 → 고객지원 담당 → CRM 고객 정보 확인 → 답변 초안 → 환불 건은 재무 담당자의 승인 후 처리')
						])),
					A2(
					$elm$html$Html$div,
					_List_Nil,
					A2(
						$elm$core$List$map,
						A3($author$project$Page$Discovery$workflow, controls, workspace, doc),
						doc.workflows)),
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$type_('button'),
							$elm$html$Html$Attributes$class('secondary'),
							$elm$html$Html$Events$onClick(controls.addWorkflow)
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('+ 업무 흐름 추가')
						]))
				]));
	});
var $author$project$Page$Discovery$view = F3(
	function (page, controls, workspace) {
		var _v0 = A2($author$project$App$Discovery$current, controls.org, controls.state);
		if (_v0.$ === 'Nothing') {
			return A2(
				$author$project$Ui$Common$panel,
				'현황 불러오기',
				_List_fromArray(
					[
						$author$project$Ui$Common$note(
						A2(
							$elm$core$Maybe$withDefault,
							'조직 현황을 불러오고 있습니다…',
							A2($elm$core$Dict$get, controls.org, controls.state.errors)))
					]));
		} else {
			var snapshot = _v0.a;
			var unsaved = A2($author$project$App$Discovery$changed, controls.org, controls.state);
			var unavailable = controls.state.loading || A2($elm$core$Dict$member, controls.org, controls.state.errors);
			var latest = A2(
				$elm$core$Maybe$withDefault,
				$author$project$Domain$Discovery$empty,
				A2(
					$elm$core$Maybe$map,
					function ($) {
						return $.discovery;
					},
					A2($author$project$App$Discovery$saved, controls.org, controls.state)));
			var doc = snapshot.discovery;
			var conflict = A2($author$project$App$Discovery$conflicted, controls.org, controls.state);
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$author$project$Ui$Common$note('확인된 사실에는 근거를 남기고, 모르는 내용은 미확인으로 보존하세요. 개선안은 현재 사실과 구분합니다. 이 입력은 운영 목표나 실제 권한을 자동으로 바꾸지 않습니다.'),
						unavailable ? $author$project$Ui$Common$note(
						A2(
							$elm$core$Maybe$withDefault,
							'최신 현황 확인 중 · 저장은 조회 완료 후 가능합니다.',
							A2($elm$core$Dict$get, controls.org, controls.state.errors))) : $elm$html$Html$text(''),
						unsaved ? A2(
						$elm$html$Html$p,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('draft-notice'),
								A2($elm$html$Html$Attributes$attribute, 'role', 'status')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('저장하지 않은 입력이 있습니다. 화면 이동 시 유지되지만 브라우저를 닫거나 전체 새로고침하면 사라집니다. 에이전트 초안은 마지막 저장 내용을 사용합니다.')
							])) : $elm$html$Html$text(''),
						conflict ? A2(
						$author$project$Ui$Common$panel,
						'입력 중 조직이 변경되었습니다',
						_List_fromArray(
							[
								$author$project$Ui$Common$note('최신 저장 내용과 현재 입력을 비교하세요. 아래 버튼은 입력을 최신 버전에 다시 적용할 준비를 하며, 저장은 별도로 해야 합니다.'),
								A2(
								$elm$html$Html$details,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$summary,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('최신 저장 내용 확인')
											])),
										A2(
										$elm$html$Html$pre,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text(
												$author$project$Page$Discovery$documentSummary(latest))
											]))
									])),
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$type_('button'),
										$elm$html$Html$Attributes$disabled(controls.busy || unavailable),
										$elm$html$Html$Events$onClick(controls.rebase)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('최신 내용 확인 후 내 입력 다시 적용')
									]))
							])) : $elm$html$Html$text(''),
						unsaved ? A2(
						$elm$html$Html$details,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$summary,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('미저장 입력 되돌리기')
									])),
								$author$project$Ui$Common$note('현재 조직의 저장하지 않은 현황·업무·검토 입력 전체를 마지막 저장 내용으로 되돌립니다.'),
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$type_('button'),
										$elm$html$Html$Attributes$class('secondary'),
										$elm$html$Html$Attributes$disabled(controls.busy),
										$elm$html$Html$Events$onClick(controls.reset)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('미저장 입력 취소')
									]))
							])) : $elm$html$Html$text(''),
						A2(
						$elm$html$Html$form,
						_List_fromArray(
							[
								$elm$html$Html$Events$onSubmit(controls.save)
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$fieldset,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$disabled(controls.busy || unavailable)
									]),
								_List_fromArray(
									[
										function () {
										switch (page.$) {
											case 'Discovery':
												return A2($author$project$Page$Discovery$overview, controls, doc);
											case 'Workflows':
												return A3($author$project$Page$Discovery$workflows, controls, workspace, doc);
											default:
												return A3($author$project$Page$Discovery$reviewForm, doc, latest, controls);
										}
									}(),
										$elm$core$List$isEmpty(
										$author$project$Domain$Discovery$problems(doc)) ? $elm$html$Html$text('') : A2(
										$elm$html$Html$div,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('field-errors'),
												A2($elm$html$Html$Attributes$attribute, 'role', 'status')
											]),
										A2(
											$elm$core$List$map,
											function (message) {
												return A2(
													$elm$html$Html$p,
													_List_Nil,
													_List_fromArray(
														[
															$elm$html$Html$text(message)
														]));
											},
											$author$project$Domain$Discovery$problems(doc))),
										A2(
										$elm$html$Html$div,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('actions')
											]),
										_List_fromArray(
											[
												A2(
												$elm$html$Html$button,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$type_('submit'),
														$elm$html$Html$Attributes$disabled(
														unavailable || (conflict || (!$elm$core$List$isEmpty(
															$author$project$Domain$Discovery$problems(doc)))))
													]),
												_List_fromArray(
													[
														$elm$html$Html$text(
														controls.busy ? '저장 중…' : (_Utils_eq(page, $author$project$Page$AgentDrafts) ? '검토 의견과 상태 저장' : '현황 저장'))
													])),
												A2(
												$elm$html$Html$button,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$type_('button'),
														$elm$html$Html$Attributes$class('secondary'),
														$elm$html$Html$Events$onClick(
														controls.go(
															_Utils_eq(page, $author$project$Page$Discovery) ? $author$project$Page$Workflows : (_Utils_eq(page, $author$project$Page$Workflows) ? $author$project$Page$AgentDrafts : $author$project$Page$Responsibility)))
													]),
												_List_fromArray(
													[
														$elm$html$Html$text(
														_Utils_eq(page, $author$project$Page$Discovery) ? '다음 · 업무 흐름 →' : (_Utils_eq(page, $author$project$Page$Workflows) ? '다음 · 에이전트 초안 →' : '책임 관계 살펴보기 →'))
													]))
											]))
									]))
							]))
					]));
		}
	});
var $author$project$DiscoveryPageTest$workspace = {
	authorities: _List_Nil,
	compiler: {diagnostics: _List_Nil, errors: 0, warnings: 0},
	decisionShare: $elm$core$Dict$empty,
	demo: false,
	edges: _List_Nil,
	events: _List_Nil,
	goals: _List_Nil,
	organization: {createdAt: '2026-09-07', id: 'a', name: 'Synthetic team'},
	people: _List_Nil,
	reviewWarnings: _List_Nil,
	reviews: _List_Nil,
	version: 1
};
var $author$project$DiscoveryPageTest$tests = A2(
	$elm_explorations$test$Test$describe,
	'Discovery guidance and evidence',
	_List_fromArray(
		[
			A2(
			$elm_explorations$test$Test$test,
			'generic journey points to investigation without demo identities or activation requirements',
			function (_v0) {
				return A2(
					$elm_explorations$test$Test$Html$Query$has,
					_List_fromArray(
						[
							$elm_explorations$test$Test$Html$Selector$text('미확인 사항은 남겨도 됩니다. 목표 활성화나 진단 해소는 현황 입력의 완료 조건이 아닙니다.')
						]),
					$elm_explorations$test$Test$Html$Query$fromHtml(
						A4(
							$author$project$Page$Discovery$guide,
							true,
							_Utils_Tuple0,
							$elm$core$Basics$always(_Utils_Tuple0),
							$author$project$Domain$Discovery$empty)));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'workflow references offer registered people, permissions and other workflows',
			function (_v1) {
				var withPeople = _Utils_update(
					$author$project$DiscoveryPageTest$workspace,
					{
						people: _List_fromArray(
							[
								{active: true, department: $elm$core$Maybe$Nothing, email: $elm$core$Maybe$Nothing, id: 'lead', name: '팀장', reportsTo: $elm$core$Maybe$Nothing, role: '고객지원 팀장'}
							])
					});
				var doc = A2(
					$author$project$Domain$Discovery$apply,
					A3($author$project$Domain$Discovery$WorkflowField, 'x', 'name', '환불 검토'),
					A2(
						$author$project$Domain$Discovery$apply,
						$author$project$Domain$Discovery$AddWorkflow('x'),
						A2(
							$author$project$Domain$Discovery$apply,
							A3($author$project$Domain$Discovery$WorkflowField, 'w', 'name', '문의 접수'),
							A2(
								$author$project$Domain$Discovery$apply,
								$author$project$Domain$Discovery$AddWorkflow('w'),
								$author$project$Domain$Discovery$empty))));
				return A2(
					$elm_explorations$test$Test$Html$Query$has,
					_List_fromArray(
						[
							$elm_explorations$test$Test$Html$Selector$text('참조 연결 · 조직 데이터와 이어지는 정보'),
							$elm_explorations$test$Test$Html$Selector$text('팀장 · 고객지원 팀장'),
							$elm_explorations$test$Test$Html$Selector$text('가격 결정'),
							$elm_explorations$test$Test$Html$Selector$tag('input'),
							$elm_explorations$test$Test$Html$Selector$attribute(
							$elm$html$Html$Attributes$type_('checkbox'))
						]),
					$elm_explorations$test$Test$Html$Query$fromHtml(
						A3(
							$author$project$Page$Discovery$view,
							$author$project$Page$Workflows,
							$author$project$DiscoveryPageTest$controls(doc),
							withPeople)));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'unsaved workflow edits keep the review confirmation disabled',
			function (_v2) {
				var doc = A2(
					$author$project$Domain$Discovery$apply,
					A3($author$project$Domain$Discovery$WorkflowField, 'w', 'name', '저장된 업무'),
					A2(
						$author$project$Domain$Discovery$apply,
						$author$project$Domain$Discovery$AddWorkflow('w'),
						$author$project$Domain$Discovery$empty));
				var initial = $author$project$DiscoveryPageTest$controls(doc);
				var changed = _Utils_update(
					initial,
					{
						state: A3(
							$author$project$App$Discovery$edit,
							'a',
							A3($author$project$Domain$Discovery$WorkflowField, 'w', 'name', '미저장 업무'),
							initial.state)
					});
				var rendered = $elm_explorations$test$Test$Html$Query$fromHtml(
					A3($author$project$Page$Discovery$view, $author$project$Page$AgentDrafts, changed, $author$project$DiscoveryPageTest$workspace));
				return A2(
					$elm_explorations$test$Expect$all,
					_List_fromArray(
						[
							function (_v3) {
							return A2(
								$elm_explorations$test$Test$Html$Query$has,
								_List_fromArray(
									[
										$elm_explorations$test$Test$Html$Selector$text('업무 또는 현황에 미저장 변경이 있습니다. 먼저 저장하면 새 근거를 바탕으로 검토할 수 있습니다.')
									]),
								rendered);
						},
							function (_v4) {
							return A2(
								$elm_explorations$test$Test$Html$Query$has,
								_List_fromArray(
									[
										$elm_explorations$test$Test$Html$Selector$attribute(
										$elm$html$Html$Attributes$disabled(true))
									]),
								A2(
									$elm_explorations$test$Test$Html$Query$find,
									_List_fromArray(
										[
											$elm_explorations$test$Test$Html$Selector$tag('input'),
											$elm_explorations$test$Test$Html$Selector$attribute(
											$elm$html$Html$Attributes$type_('checkbox'))
										]),
									rendered));
						}
						]),
					_Utils_Tuple0);
			}),
			A2(
			$elm_explorations$test$Test$test,
			'scope help is permanently associated with its input',
			function (_v5) {
				return A2(
					$elm_explorations$test$Test$Html$Query$has,
					_List_fromArray(
						[
							$elm_explorations$test$Test$Html$Selector$attribute(
							A2($elm$html$Html$Attributes$attribute, 'aria-describedby', 'discovery-scope-help discovery-scope-error'))
						]),
					A2(
						$elm_explorations$test$Test$Html$Query$find,
						_List_fromArray(
							[
								$elm_explorations$test$Test$Html$Selector$tag('textarea'),
								$elm_explorations$test$Test$Html$Selector$attribute(
								$elm$html$Html$Attributes$id('discovery-scope'))
							]),
						$elm_explorations$test$Test$Html$Query$fromHtml(
							A3(
								$author$project$Page$Discovery$view,
								$author$project$Page$Discovery,
								$author$project$DiscoveryPageTest$controls($author$project$Domain$Discovery$empty),
								$author$project$DiscoveryPageTest$workspace))));
			})
		]));
var $author$project$DiscoveryTest$tests = A2(
	$elm_explorations$test$Test$describe,
	'Organization discovery drafts',
	_List_fromArray(
		[
			A2(
			$elm_explorations$test$Test$test,
			'receiving a newer server version preserves local evidence and requires explicit rebase',
			function (_v0) {
				var initial = A3(
					$author$project$App$Discovery$receive,
					'a',
					$elm$core$Result$Ok(
						{discovery: $author$project$Domain$Discovery$empty, version: 7}),
					$author$project$App$Discovery$init);
				var edited = A3(
					$author$project$App$Discovery$edit,
					'a',
					$author$project$Domain$Discovery$Scope('Local investigation'),
					initial);
				var received = A3(
					$author$project$App$Discovery$receive,
					'a',
					$elm$core$Result$Ok(
						{
							discovery: A2(
								$author$project$Domain$Discovery$apply,
								$author$project$Domain$Discovery$Scope('Server scope'),
								$author$project$Domain$Discovery$empty),
							version: 9
						}),
					edited);
				var rebased = A2($author$project$App$Discovery$rebase, 'a', received);
				return A2(
					$elm_explorations$test$Expect$all,
					_List_fromArray(
						[
							function (_v1) {
							return A2(
								$elm_explorations$test$Expect$equal,
								$elm$core$Maybe$Just(
									_Utils_Tuple2(7, 'Local investigation')),
								A2(
									$elm$core$Maybe$map,
									function (s) {
										return _Utils_Tuple2(s.version, s.discovery.scope);
									},
									A2($author$project$App$Discovery$current, 'a', received)));
						},
							function (_v2) {
							return A2(
								$elm_explorations$test$Expect$equal,
								true,
								A2($author$project$App$Discovery$conflicted, 'a', received));
						},
							function (_v3) {
							return A2(
								$elm_explorations$test$Expect$equal,
								$elm$core$Maybe$Just(
									_Utils_Tuple2(9, 'Local investigation')),
								A2(
									$elm$core$Maybe$map,
									function (s) {
										return _Utils_Tuple2(s.version, s.discovery.scope);
									},
									A2($author$project$App$Discovery$current, 'a', rebased)));
						},
							function (_v4) {
							return A2(
								$elm_explorations$test$Expect$equal,
								false,
								A2($author$project$App$Discovery$conflicted, 'a', rebased));
						}
						]),
					_Utils_Tuple0);
			}),
			A2(
			$elm_explorations$test$Test$test,
			'organization drafts remain isolated through failure and refresh',
			function (_v5) {
				var state = A3(
					$author$project$App$Discovery$receive,
					'b',
					$elm$core$Result$Ok(
						{discovery: $author$project$Domain$Discovery$empty, version: 9}),
					A3(
						$author$project$App$Discovery$receive,
						'a',
						$elm$core$Result$Err('offline'),
						A3(
							$author$project$App$Discovery$edit,
							'b',
							$author$project$Domain$Discovery$Scope('Beta'),
							A3(
								$author$project$App$Discovery$edit,
								'a',
								$author$project$Domain$Discovery$Scope('Alpha'),
								A3(
									$author$project$App$Discovery$receive,
									'b',
									$elm$core$Result$Ok(
										{discovery: $author$project$Domain$Discovery$empty, version: 8}),
									A3(
										$author$project$App$Discovery$receive,
										'a',
										$elm$core$Result$Ok(
											{discovery: $author$project$Domain$Discovery$empty, version: 2}),
										$author$project$App$Discovery$init))))));
				return A2(
					$elm_explorations$test$Expect$equal,
					_Utils_Tuple2(
						$elm$core$Maybe$Just('Alpha'),
						$elm$core$Maybe$Just('Beta')),
					_Utils_Tuple2(
						A2(
							$elm$core$Maybe$map,
							A2(
								$elm$core$Basics$composeR,
								function ($) {
									return $.discovery;
								},
								function ($) {
									return $.scope;
								}),
							A2($author$project$App$Discovery$current, 'a', state)),
						A2(
							$elm$core$Maybe$map,
							A2(
								$elm$core$Basics$composeR,
								function ($) {
									return $.discovery;
								},
								function ($) {
									return $.scope;
								}),
							A2($author$project$App$Discovery$current, 'b', state))));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'unknown workflow fields stay empty without inventing evidence',
			function (_v6) {
				var doc = A2(
					$author$project$Domain$Discovery$apply,
					A3($author$project$Domain$Discovery$WorkflowField, 'w', 'name', 'Intake'),
					A2(
						$author$project$Domain$Discovery$apply,
						$author$project$Domain$Discovery$AddWorkflow('w'),
						$author$project$Domain$Discovery$empty));
				return A2(
					$elm_explorations$test$Expect$all,
					_List_fromArray(
						[
							function (_v7) {
							return A2(
								$elm_explorations$test$Expect$equal,
								_List_Nil,
								$author$project$Domain$Discovery$problems(doc));
						},
							function (_v8) {
							return A2(
								$elm_explorations$test$Expect$equal,
								$elm$core$Maybe$Just(
									_Utils_Tuple3('unknown', '', '')),
								A2(
									$elm$core$Maybe$map,
									function (w) {
										return _Utils_Tuple3(w.status, w.evidence, w.approval);
									},
									$elm$core$List$head(doc.workflows)));
						},
							function (_v9) {
							return A2(
								$elm_explorations$test$Expect$equal,
								false,
								$elm$core$List$isEmpty(
									$author$project$Domain$Discovery$problems(
										A2(
											$author$project$Domain$Discovery$apply,
											A3($author$project$Domain$Discovery$WorkflowField, 'w', 'status', 'confirmed'),
											doc))));
						}
						]),
					_Utils_Tuple0);
			}),
			A2(
			$elm_explorations$test$Test$test,
			'removing a workflow drops references to it and blanks clear references',
			function (_v10) {
				var first = function (d) {
					return $elm$core$List$head(d.workflows);
				};
				var doc = A2(
					$author$project$Domain$Discovery$apply,
					A2($author$project$Domain$Discovery$WorkflowApprovalPermission, 'a', 'Pricing'),
					A2(
						$author$project$Domain$Discovery$apply,
						A2($author$project$Domain$Discovery$WorkflowRolePerson, 'a', 'lead'),
						A2(
							$author$project$Domain$Discovery$apply,
							A3($author$project$Domain$Discovery$WorkflowHandoff, 'a', 'a', true),
							A2(
								$author$project$Domain$Discovery$apply,
								A3($author$project$Domain$Discovery$WorkflowHandoff, 'a', 'b', true),
								A2(
									$author$project$Domain$Discovery$apply,
									$author$project$Domain$Discovery$AddWorkflow('b'),
									A2(
										$author$project$Domain$Discovery$apply,
										$author$project$Domain$Discovery$AddWorkflow('a'),
										$author$project$Domain$Discovery$empty))))));
				var cleared = A2(
					$author$project$Domain$Discovery$apply,
					$author$project$Domain$Discovery$RemoveWorkflow('b'),
					A2(
						$author$project$Domain$Discovery$apply,
						A2($author$project$Domain$Discovery$WorkflowRolePerson, 'a', ''),
						doc));
				return A2(
					$elm_explorations$test$Expect$all,
					_List_fromArray(
						[
							function (_v11) {
							return A2(
								$elm_explorations$test$Expect$equal,
								$elm$core$Maybe$Just(
									_Utils_Tuple3(
										_List_fromArray(
											['b']),
										$elm$core$Maybe$Just('lead'),
										$elm$core$Maybe$Just('Pricing'))),
								A2(
									$elm$core$Maybe$map,
									function (w) {
										return _Utils_Tuple3(w.handoffWorkflows, w.rolePerson, w.approvalPermission);
									},
									first(doc)));
						},
							function (_v12) {
							return A2(
								$elm_explorations$test$Expect$equal,
								$elm$core$Maybe$Just(
									_Utils_Tuple2(_List_Nil, $elm$core$Maybe$Nothing)),
								A2(
									$elm$core$Maybe$map,
									function (w) {
										return _Utils_Tuple2(w.handoffWorkflows, w.rolePerson);
									},
									first(cleared)));
						},
							function (_v13) {
							return A2(
								$elm_explorations$test$Expect$equal,
								1,
								$elm$core$List$length(cleared.workflows));
						}
						]),
					_Utils_Tuple0);
			}),
			A2(
			$elm_explorations$test$Test$test,
			'source edits invalidate review while retaining review notes',
			function (_v14) {
				var doc = A2(
					$author$project$Domain$Discovery$apply,
					$author$project$Domain$Discovery$ReviewStatus('reviewed'),
					A2(
						$author$project$Domain$Discovery$apply,
						$author$project$Domain$Discovery$ReviewNote('Check approvals'),
						$author$project$Domain$Discovery$empty));
				return A2(
					$elm_explorations$test$Expect$equal,
					{note: 'Check approvals', status: 'pending'},
					A2(
						$author$project$Domain$Discovery$apply,
						$author$project$Domain$Discovery$Scope('Changed scope'),
						doc).review);
			}),
			A2(
			$elm_explorations$test$Test$test,
			'confirmed observations require evidence and proposed work stays distinct',
			function (_v15) {
				var doc = A2(
					$author$project$Domain$Discovery$apply,
					A3($author$project$Domain$Discovery$ObservationField, 'o', 'status', 'confirmed'),
					A2(
						$author$project$Domain$Discovery$apply,
						A3($author$project$Domain$Discovery$ObservationField, 'o', 'subject', 'Responsibility'),
						A2(
							$author$project$Domain$Discovery$apply,
							$author$project$Domain$Discovery$AddObservation('o'),
							$author$project$Domain$Discovery$empty)));
				return A2(
					$elm_explorations$test$Expect$all,
					_List_fromArray(
						[
							function (_v16) {
							return A2(
								$elm_explorations$test$Expect$equal,
								false,
								$elm$core$List$isEmpty(
									$author$project$Domain$Discovery$problems(doc)));
						},
							function (_v17) {
							return A2(
								$elm_explorations$test$Expect$equal,
								_List_Nil,
								$author$project$Domain$Discovery$problems(
									A2(
										$author$project$Domain$Discovery$apply,
										A3($author$project$Domain$Discovery$ObservationField, 'o', 'evidence', 'Interview notes'),
										doc)));
						},
							function (_v18) {
							return A2(
								$elm_explorations$test$Expect$equal,
								_List_fromArray(
									['확인된 사실', '미확인', '개선안']),
								A2(
									$elm$core$List$map,
									$author$project$Domain$Discovery$statusLabel,
									_List_fromArray(
										['confirmed', 'unknown', 'proposed'])));
						}
						]),
					_Utils_Tuple0);
			})
		]));
var $author$project$FormTest$goal = {
	baseline: '0',
	budget: '12.5',
	deadline: '2026-12-31',
	description: 'Revenue',
	direction: 'HigherIsBetter',
	metricId: 'sales',
	metricName: 'Sales',
	parent: '',
	permissions: _List_fromArray(
		['Pricing']),
	startsAt: '2026-01-01',
	target: '100',
	unit: 'KRW'
};
var $author$project$FormTest$review = {decision: '', decisionDeadline: '', decisionOwner: '', goal: 'goal-a', learning: 'Learning', note: 'Reflection'};
var $author$project$FormTest$tests = A2(
	$elm_explorations$test$Test$describe,
	'Typed drafts validate at submission boundary',
	_List_fromArray(
		[
			A2(
			$elm_explorations$test$Test$test,
			'goal converts numeric text only on submission',
			function (_v0) {
				return A2(
					$elm_explorations$test$Expect$equal,
					$elm$core$Result$Ok(
						_Utils_Tuple3(0, 100, 12.5)),
					A2(
						$elm$core$Result$map,
						function (g) {
							return _Utils_Tuple3(g.baseline, g.target, g.budget);
						},
						$author$project$Form$Goal$validate($author$project$FormTest$goal)));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'incomplete number remains editable and cannot submit',
			function (_v1) {
				var edited = A3($author$project$Form$Goal$edit, $author$project$Form$Goal$Target, '-', $author$project$FormTest$goal);
				return A2(
					$elm_explorations$test$Expect$all,
					_List_fromArray(
						[
							function (_v2) {
							return A2($elm_explorations$test$Expect$equal, '-', edited.target);
						},
							function (_v3) {
							return $elm_explorations$test$Expect$err(
								$author$project$Form$Goal$validate(edited));
						}
						]),
					_Utils_Tuple0);
			}),
			A2(
			$elm_explorations$test$Test$test,
			'goal rejects missing description',
			function (_v4) {
				return $elm_explorations$test$Expect$err(
					$author$project$Form$Goal$validate(
						_Utils_update(
							$author$project$FormTest$goal,
							{description: '  '})));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'goal rejects deadline before start',
			function (_v5) {
				return $elm_explorations$test$Expect$err(
					$author$project$Form$Goal$validate(
						_Utils_update(
							$author$project$FormTest$goal,
							{deadline: '2025-12-31'})));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'permission edits preserve other draft fields',
			function (_v6) {
				return A2(
					$elm_explorations$test$Expect$equal,
					_Utils_Tuple3(
						_List_fromArray(
							['Hiring']),
						'Revenue',
						'12.5'),
					function (g) {
						return _Utils_Tuple3(g.permissions, g.description, g.budget);
					}(
						A3(
							$author$project$Form$Goal$edit,
							$author$project$Form$Goal$Permission('Pricing'),
							'false',
							A3(
								$author$project$Form$Goal$edit,
								$author$project$Form$Goal$Permission('Hiring'),
								'true',
								$author$project$FormTest$goal))));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'learning without a decision is valid',
			function (_v7) {
				return A2(
					$elm_explorations$test$Expect$equal,
					$elm$core$Result$Ok($author$project$FormTest$review),
					$author$project$Form$Review$validate($author$project$FormTest$review));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'decision requires its owner',
			function (_v8) {
				return $elm_explorations$test$Expect$err(
					$author$project$Form$Review$validate(
						_Utils_update(
							$author$project$FormTest$review,
							{decision: 'Launch'})));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'decision can omit its deadline',
			function (_v9) {
				return A2(
					$elm_explorations$test$Expect$equal,
					$elm$core$Result$Ok(''),
					A2(
						$elm$core$Result$map,
						function ($) {
							return $.decisionDeadline;
						},
						$author$project$Form$Review$validate(
							_Utils_update(
								$author$project$FormTest$review,
								{decision: 'Launch', decisionOwner: 'owner'}))));
			})
		]));
var $author$project$Form$Action$Activate = function (a) {
	return {$: 'Activate', a: a};
};
var $author$project$Form$Action$AddPerson = {$: 'AddPerson'};
var $author$project$Form$Action$Assign = function (a) {
	return {$: 'Assign', a: a};
};
var $author$project$Ui$ListView$Cards = {$: 'Cards'};
var $author$project$ListViewTest$Change = function (a) {
	return {$: 'Change', a: a};
};
var $author$project$App$Update$Edit = F3(
	function (a, b, c) {
		return {$: 'Edit', a: a, b: b, c: c};
	});
var $author$project$ListViewTest$Edit = F3(
	function (a, b, c) {
		return {$: 'Edit', a: a, b: b, c: c};
	});
var $author$project$Form$Action$Evaluate = function (a) {
	return {$: 'Evaluate', a: a};
};
var $author$project$App$Update$FilterPeople = function (a) {
	return {$: 'FilterPeople', a: a};
};
var $author$project$Form$Action$Grant = function (a) {
	return {$: 'Grant', a: a};
};
var $author$project$ListViewTest$Open = function (a) {
	return {$: 'Open', a: a};
};
var $author$project$Form$Action$Report = function (a) {
	return {$: 'Report', a: a};
};
var $author$project$Page$Results = {$: 'Results'};
var $author$project$App$Update$SearchPeople = function (a) {
	return {$: 'SearchPeople', a: a};
};
var $author$project$App$Update$SetListMode = F2(
	function (a, b) {
		return {$: 'SetListMode', a: a, b: b};
	});
var $author$project$ListViewTest$SettingsFor = function (a) {
	return {$: 'SettingsFor', a: a};
};
var $author$project$ListViewTest$Submit = function (a) {
	return {$: 'Submit', a: a};
};
var $elm_explorations$test$Expect$compareWith = $elm_explorations$test$Expect$testWith($elm_explorations$test$Test$Runner$Failure$Comparison);
var $elm_explorations$test$Expect$atLeast = A2($elm_explorations$test$Expect$compareWith, 'Expect.atLeast', $elm$core$Basics$ge);
var $elm_explorations$test$Test$Html$Event$emptyObject = $elm$json$Json$Encode$object(_List_Nil);
var $elm_explorations$test$Test$Html$Event$click = _Utils_Tuple2('click', $elm_explorations$test$Test$Html$Event$emptyObject);
var $elm_explorations$test$Test$Html$Selector$Internal$Containing = function (a) {
	return {$: 'Containing', a: a};
};
var $elm_explorations$test$Test$Html$Selector$containing = $elm_explorations$test$Test$Html$Selector$Internal$Containing;
var $elm_explorations$test$Test$Html$Event$Event = F2(
	function (a, b) {
		return {$: 'Event', a: a, b: b};
	});
var $elm_explorations$test$Test$Html$Event$eventPayload = function (_v0) {
	var _v1 = _v0.a;
	var payload = _v1.b;
	return payload;
};
var $elm_explorations$test$Test$Html$Event$Handling = F3(
	function (message, stopPropagation, preventDefault) {
		return {message: message, preventDefault: preventDefault, stopPropagation: stopPropagation};
	});
var $elm$core$Result$fromMaybe = F2(
	function (err, maybe) {
		if (maybe.$ === 'Just') {
			var v = maybe.a;
			return $elm$core$Result$Ok(v);
		} else {
			return $elm$core$Result$Err(err);
		}
	});
var $elm_explorations$test$Test$Html$Event$findEvent = F2(
	function (eventName, element) {
		var handlerToDecoder = function (handler) {
			switch (handler.$) {
				case 'Normal':
					var decoder = handler.a;
					return A2(
						$elm$json$Json$Decode$map,
						function (msg) {
							return A3($elm_explorations$test$Test$Html$Event$Handling, msg, false, false);
						},
						decoder);
				case 'MayStopPropagation':
					var decoder = handler.a;
					return A2(
						$elm$json$Json$Decode$map,
						function (_v2) {
							var msg = _v2.a;
							var sp = _v2.b;
							return A3($elm_explorations$test$Test$Html$Event$Handling, msg, sp, false);
						},
						decoder);
				case 'MayPreventDefault':
					var decoder = handler.a;
					return A2(
						$elm$json$Json$Decode$map,
						function (_v3) {
							var msg = _v3.a;
							var pd = _v3.b;
							return A3($elm_explorations$test$Test$Html$Event$Handling, msg, false, pd);
						},
						decoder);
				default:
					var decoder = handler.a;
					return decoder;
			}
		};
		var elementOutput = $elm_explorations$test$Test$Html$Query$Internal$prettyPrint(element);
		var eventDecoder = function (node) {
			return A2(
				$elm$core$Result$fromMaybe,
				'Event.expectEvent: I found a node, but it does not listen for \"' + (eventName + ('\" events like I expected it would.\n\n' + elementOutput)),
				A2(
					$elm$core$Maybe$map,
					handlerToDecoder,
					A2($elm$core$Dict$get, eventName, node.facts.events)));
		};
		switch (element.$) {
			case 'TextTag':
				return $elm$core$Result$Err('I found a text node instead of an element. Text nodes do not receive events, so it would be impossible to simulate \"' + (eventName + ('\" events on it. The text in the node was: \"' + (elementOutput + '\"'))));
			case 'NodeEntry':
				var node = element.a;
				return eventDecoder(node);
			case 'CustomNode':
				var node = element.a;
				return eventDecoder(node);
			default:
				var node = element.a;
				return eventDecoder(node);
		}
	});
var $elm$core$Result$mapError = F2(
	function (f, result) {
		if (result.$ === 'Ok') {
			var v = result.a;
			return $elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return $elm$core$Result$Err(
				f(e));
		}
	});
var $elm_explorations$test$Test$Html$Event$findHandler = function (_v0) {
	var _v1 = _v0.a;
	var eventName = _v1.a;
	var _v2 = _v0.b;
	var query = _v2.b;
	return A2(
		$elm$core$Result$andThen,
		$elm_explorations$test$Test$Html$Event$findEvent(eventName),
		A2(
			$elm$core$Result$mapError,
			$elm_explorations$test$Test$Html$Query$Internal$queryErrorToString,
			A2(
				$elm$core$Result$andThen,
				$elm_explorations$test$Test$Html$Query$Internal$verifySingle(eventName),
				$elm_explorations$test$Test$Html$Query$Internal$traverse(query))));
};
var $elm_explorations$test$Test$Html$Event$toResult = function (event) {
	return A2(
		$elm$core$Result$andThen,
		function (handler) {
			return A2(
				$elm$core$Result$mapError,
				$elm$json$Json$Decode$errorToString,
				A2(
					$elm$json$Json$Decode$decodeValue,
					handler,
					$elm_explorations$test$Test$Html$Event$eventPayload(event)));
		},
		A2(
			$elm$core$Result$map,
			$elm$json$Json$Decode$map(
				function ($) {
					return $.message;
				}),
			$elm_explorations$test$Test$Html$Event$findHandler(event)));
};
var $elm_explorations$test$Test$Html$Event$expect = F2(
	function (msg, _v0) {
		var event = _v0.a;
		var _v1 = _v0.b;
		var showTrace = _v1.a;
		var query = _v1.b;
		var _v2 = $elm_explorations$test$Test$Html$Event$toResult(
			A2(
				$elm_explorations$test$Test$Html$Event$Event,
				event,
				A2($elm_explorations$test$Test$Html$Query$Internal$Single, showTrace, query)));
		if (_v2.$ === 'Err') {
			var noEvent = _v2.a;
			return A4(
				$elm_explorations$test$Test$Html$Query$Internal$failWithQuery,
				showTrace,
				'',
				query,
				$elm_explorations$test$Expect$fail(noEvent));
		} else {
			var foundMsg = _v2.a;
			return A4(
				$elm_explorations$test$Test$Html$Query$Internal$failWithQuery,
				showTrace,
				'Event.expectEvent: Expected the msg \u001B[32m' + ($elm_explorations$test$Test$Internal$toString(msg) + ('\u001B[39m from the event \u001B[31m' + ($elm_explorations$test$Test$Internal$toString(event) + '\u001B[39m but could not find the event.'))),
				query,
				A2($elm_explorations$test$Expect$equal, msg, foundMsg));
		}
	});
var $elm_explorations$test$Test$Html$Event$simulate = $elm_explorations$test$Test$Html$Event$Event;
var $author$project$ListViewTest$clickButton = F3(
	function (label, expected, html) {
		return A2(
			$elm_explorations$test$Test$Html$Event$expect,
			expected,
			A2(
				$elm_explorations$test$Test$Html$Event$simulate,
				$elm_explorations$test$Test$Html$Event$click,
				A2(
					$elm_explorations$test$Test$Html$Query$find,
					_List_fromArray(
						[
							$elm_explorations$test$Test$Html$Selector$tag('button'),
							$elm_explorations$test$Test$Html$Selector$containing(
							_List_fromArray(
								[
									$elm_explorations$test$Test$Html$Selector$text(label)
								]))
						]),
					$elm_explorations$test$Test$Html$Query$fromHtml(html))));
	});
var $author$project$Ui$ListView$controls = F2(
	function (mode, change) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('list-view-toolbar'),
					A2($elm$html$Html$Attributes$attribute, 'role', 'group'),
					A2($elm$html$Html$Attributes$attribute, 'aria-label', '목록 보기')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$span,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('목록 보기')
						])),
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$type_('button'),
							$elm$html$Html$Attributes$classList(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'secondary',
									!_Utils_eq(mode, $author$project$Ui$ListView$Cards))
								])),
							A2(
							$elm$html$Html$Attributes$attribute,
							'aria-pressed',
							_Utils_eq(mode, $author$project$Ui$ListView$Cards) ? 'true' : 'false'),
							$elm$html$Html$Events$onClick(
							change($author$project$Ui$ListView$Cards))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('카드')
						])),
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$type_('button'),
							$elm$html$Html$Attributes$classList(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'secondary',
									!_Utils_eq(mode, $author$project$Ui$ListView$Table))
								])),
							A2(
							$elm$html$Html$Attributes$attribute,
							'aria-pressed',
							_Utils_eq(mode, $author$project$Ui$ListView$Table) ? 'true' : 'false'),
							$elm$html$Html$Events$onClick(
							change($author$project$Ui$ListView$Table))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('표')
						]))
				]));
	});
var $elm_explorations$test$Test$Html$Query$Internal$multipleToExpectation = F2(
	function (_v0, check) {
		var query = _v0.b;
		var _v1 = $elm_explorations$test$Test$Html$Query$Internal$traverse(query);
		if (_v1.$ === 'Ok') {
			var list = _v1.a;
			return check(list);
		} else {
			var error = _v1.a;
			return $elm_explorations$test$Expect$fail(
				$elm_explorations$test$Test$Html$Query$Internal$queryErrorToString(error));
		}
	});
var $elm_explorations$test$Test$Html$Query$count = F2(
	function (expect, multiple) {
		var showTrace = multiple.a;
		var query = multiple.b;
		return A2(
			$elm_explorations$test$Test$Html$Query$Internal$multipleToExpectation,
			multiple,
			A2(
				$elm$core$Basics$composeR,
				$elm$core$List$length,
				A2(
					$elm$core$Basics$composeR,
					expect,
					A3($elm_explorations$test$Test$Html$Query$Internal$failWithQuery, showTrace, 'Query.count', query))));
	});
var $elm_explorations$test$Test$Html$Query$Internal$FindAll = function (a) {
	return {$: 'FindAll', a: a};
};
var $elm_explorations$test$Test$Html$Query$Internal$Multiple = F2(
	function (a, b) {
		return {$: 'Multiple', a: a, b: b};
	});
var $elm_explorations$test$Test$Html$Query$findAll = F2(
	function (selectors, _v0) {
		var showTrace = _v0.a;
		var query = _v0.b;
		return A2(
			$elm_explorations$test$Test$Html$Query$Internal$Multiple,
			showTrace,
			A2(
				$elm_explorations$test$Test$Html$Query$Internal$prependSelector,
				query,
				$elm_explorations$test$Test$Html$Query$Internal$FindAll(selectors)));
	});
var $elm_explorations$test$Test$Html$Query$Internal$First = {$: 'First'};
var $elm_explorations$test$Test$Html$Query$first = function (_v0) {
	var showTrace = _v0.a;
	var query = _v0.b;
	return A2(
		$elm_explorations$test$Test$Html$Query$Internal$Single,
		showTrace,
		A2($elm_explorations$test$Test$Html$Query$Internal$prependSelector, query, $elm_explorations$test$Test$Html$Query$Internal$First));
};
var $author$project$ListViewTest$forms = {
	busy: false,
	edit: $author$project$ListViewTest$Edit,
	fresh: true,
	saving: $elm$core$Maybe$Nothing,
	submit: $author$project$ListViewTest$Submit,
	value: F2(
		function (_v0, _v1) {
			return '';
		})
};
var $elm_explorations$test$Test$Html$Query$Internal$showSelectorOutcomeInverse = F2(
	function (elmHtmlList, selector) {
		var outcome = function () {
			var _v0 = A2(
				$elm_explorations$test$Test$Html$Selector$Internal$queryAll,
				_List_fromArray(
					[selector]),
				elmHtmlList);
			if (!_v0.b) {
				return '✓';
			} else {
				return '✗';
			}
		}();
		return A2(
			$elm$core$String$join,
			' ',
			_List_fromArray(
				[
					outcome,
					'has not',
					$elm_explorations$test$Test$Html$Selector$Internal$selectorToString(selector)
				]));
	});
var $elm_explorations$test$Test$Html$Query$Internal$hasNot = F2(
	function (selectors, query) {
		var _v0 = $elm_explorations$test$Test$Html$Query$Internal$traverse(query);
		if (_v0.$ === 'Ok') {
			if (!_v0.a.b) {
				return $elm_explorations$test$Expect$pass;
			} else {
				var elmHtmlList = _v0.a;
				var _v1 = A2($elm_explorations$test$Test$Html$Selector$Internal$queryAll, selectors, elmHtmlList);
				if (!_v1.b) {
					return $elm_explorations$test$Expect$pass;
				} else {
					return $elm_explorations$test$Expect$fail(
						A2(
							$elm$core$String$join,
							'\n',
							A2(
								$elm$core$List$map,
								$elm_explorations$test$Test$Html$Query$Internal$showSelectorOutcomeInverse(elmHtmlList),
								selectors)));
				}
			}
		} else {
			return $elm_explorations$test$Expect$pass;
		}
	});
var $elm_explorations$test$Test$Html$Query$hasNot = F2(
	function (selectors, _v0) {
		var showTrace = _v0.a;
		var query = _v0.b;
		var queryName = 'Query.hasNot ' + A2($elm_explorations$test$Test$Html$Query$Internal$joinAsList, $elm_explorations$test$Test$Html$Selector$Internal$selectorToString, selectors);
		return A4(
			$elm_explorations$test$Test$Html$Query$Internal$failWithQuery,
			showTrace,
			queryName,
			query,
			A2($elm_explorations$test$Test$Html$Query$Internal$hasNot, selectors, query));
	});
var $elm_explorations$test$Test$Html$Event$input = function (value) {
	return _Utils_Tuple2(
		'input',
		$elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'target',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'value',
								$elm$json$Json$Encode$string(value))
							])))
				])));
};
var $author$project$ListViewTest$Other = {$: 'Other'};
var $author$project$Ui$Form$checks = F2(
	function (model, action) {
		return A2(
			$elm$html$Html$fieldset,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('permission-fields')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$legend,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('결정 권한')
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('checks')
						]),
					A2(
						$elm$core$List$map,
						function (_v0) {
							var key = _v0.a;
							var label_ = _v0.b;
							return A2(
								$elm$html$Html$label,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$input,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$type_('checkbox'),
												$elm$html$Html$Attributes$checked(
												A2(model.value, action, key) === 'true'),
												$elm$html$Html$Events$onCheck(
												function (checked_) {
													return A3(
														model.edit,
														action,
														key,
														checked_ ? 'true' : 'false');
												})
											]),
										_List_Nil),
										$elm$html$Html$text(label_)
									]));
						},
						$author$project$Ui$Label$permissions))
				]));
	});
var $author$project$Ui$Form$formView = F4(
	function (model, action, label_, children) {
		return A2(
			$elm$html$Html$form,
			_List_fromArray(
				[
					$elm$html$Html$Events$onSubmit(
					model.submit(action))
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$fieldset,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$disabled(model.busy)
						]),
					_Utils_ap(
						children,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$type_('submit'),
										$elm$html$Html$Attributes$disabled(!model.fresh)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(
										_Utils_eq(
											model.saving,
											$elm$core$Maybe$Just(
												$author$project$Form$Action$actionKey(action))) ? '저장 중…' : label_)
									]))
							])))
				]));
	});
var $author$project$Ui$Form$inputField = F6(
	function (model, action, label_, key, kind, required_) {
		return A8(
			$author$project$Ui$Form$guidedInputNamed,
			$author$project$Form$Action$actionKey(action) + ('-' + key),
			key,
			label_,
			$author$project$Ui$Form$help(key),
			kind,
			required_,
			A2(model.value, action, key),
			A2(model.edit, action, key));
	});
var $author$project$Page$Authorities$authorityForm = F2(
	function (model, person) {
		return A4(
			$author$project$Ui$Form$formView,
			model.forms,
			$author$project$Form$Action$Grant(person.id),
			'권한 저장',
			_List_fromArray(
				[
					A6(
					$author$project$Ui$Form$inputField,
					model.forms,
					$author$project$Form$Action$Grant(person.id),
					'현재 집행 가능한 예산 한도 (KRW)',
					'budget',
					'number',
					true),
					A2(
					$author$project$Ui$Form$checks,
					model.forms,
					$author$project$Form$Action$Grant(person.id))
				]));
	});
var $author$project$Page$Authorities$authorityShare = F2(
	function (w, person) {
		return $elm$core$String$fromInt(
			$elm$core$Basics$round(
				100 * A2(
					$elm$core$Maybe$withDefault,
					0,
					A2($elm$core$Dict$get, person.id, w.decisionShare)))) + '%';
	});
var $elm$html$Html$Attributes$colspan = function (n) {
	return A2(
		_VirtualDom_attribute,
		'colspan',
		$elm$core$String$fromInt(n));
};
var $author$project$Ui$ListView$detailRow = F4(
	function (columns, attrs, title, children) {
		return A2(
			$elm$html$Html$tr,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('list-detail-row')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$td,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$colspan(columns)
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$details,
							attrs,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$summary,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(title)
										])),
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('list-detail-content')
										]),
									children)
								]))
						]))
				]));
	});
var $author$project$Ui$Common$diagnosticView = function (w) {
	return A2(
		$author$project$Ui$Common$panel,
		'조직 구조 검사',
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('section-head')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('기록된 목표·권한에서 확인할 사항'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('tag warn')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(
								$elm$core$String$fromInt(w.compiler.errors) + (' 오류 · ' + ($elm$core$String$fromInt(w.compiler.warnings) + ' 경고')))
							]))
					])),
				$elm$core$List$isEmpty(w.compiler.diagnostics) ? $author$project$Ui$Common$note('현재 입력에 적용한 규칙에서 추가 확인 사항이 발견되지 않았습니다. 미입력 정보나 실제 업무까지 검증한 것은 아닙니다.') : A2(
				$elm$html$Html$div,
				_List_Nil,
				A2(
					$elm$core$List$map,
					function (d) {
						return A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$classList(
									_List_fromArray(
										[
											_Utils_Tuple2('diagnostic', true),
											_Utils_Tuple2('error', d.severity === 'Error')
										]))
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$code,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(d.code)
										])),
									A2(
									$elm$html$Html$strong,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(d.message)
										])),
									A2(
									$elm$html$Html$p,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(d.subject)
										])),
									A2(
									$elm$html$Html$div,
									_List_Nil,
									A2(
										$elm$core$List$map,
										function (line) {
											return A2(
												$elm$html$Html$p,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text(line)
													]));
										},
										d.details))
								]));
					},
					w.compiler.diagnostics)),
				$author$project$Ui$Common$note('권한 집중도는 권한 종류와 예산 보유를 각각 1점으로 세는 규칙 기반 추정치입니다.')
			]));
};
var $author$project$Ui$Common$emptyState = F2(
	function (title, content) {
		return A2(
			$elm$html$Html$section,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('panel empty')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$h2,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(title)
						])),
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(content)
						]))
				]));
	});
var $author$project$Page$Authorities$goalCount = F2(
	function (w, person) {
		return $elm$core$String$fromInt(
			$elm$core$List$length(
				A2(
					$elm$core$List$filter,
					A2(
						$elm$core$Basics$composeR,
						function ($) {
							return $.owner;
						},
						$elm$core$Basics$eq(
							$elm$core$Maybe$Just(person.id))),
					w.goals))) + '개';
	});
var $elm$core$String$reverse = _String_reverse;
var $author$project$Ui$Label$formatNumber = function (number) {
	var parts = A2(
		$elm$core$String$split,
		'.',
		$elm$core$String$fromFloat(
			$elm$core$Basics$round(
				$elm$core$Basics$abs(number) * 100) / 100));
	var group = function (reversed) {
		return ($elm$core$String$length(reversed) <= 3) ? reversed : (A2($elm$core$String$left, 3, reversed) + (',' + group(
			A2($elm$core$String$dropLeft, 3, reversed))));
	};
	var whole = $elm$core$String$reverse(
		group(
			$elm$core$String$reverse(
				A2(
					$elm$core$Maybe$withDefault,
					'0',
					$elm$core$List$head(parts)))));
	var fraction = function () {
		var _v0 = $elm$core$List$head(
			A2($elm$core$List$drop, 1, parts));
		if (_v0.$ === 'Just') {
			var digits = _v0.a;
			return '.' + digits;
		} else {
			return '';
		}
	}();
	return _Utils_ap(
		(number < 0) ? '-' : '',
		_Utils_ap(whole, fraction));
};
var $author$project$Page$Authorities$savedAuthority = F2(
	function (w, person) {
		return $elm$core$List$head(
			A2(
				$elm$core$List$filter,
				A2(
					$elm$core$Basics$composeR,
					function ($) {
						return $.owner;
					},
					$elm$core$Basics$eq(person.id)),
				w.authorities));
	});
var $author$project$Page$Authorities$savedBudget = F2(
	function (w, person) {
		return A2(
			$elm$core$Maybe$withDefault,
			'미설정',
			A2(
				$elm$core$Maybe$map,
				function (a) {
					return $author$project$Ui$Label$formatNumber(a.budgetLimit) + '원';
				},
				A2($author$project$Page$Authorities$savedAuthority, w, person)));
	});
var $author$project$Page$Authorities$savedPermissions = F2(
	function (w, person) {
		var _v0 = A2($author$project$Page$Authorities$savedAuthority, w, person);
		if (_v0.$ === 'Nothing') {
			return '없음';
		} else {
			var authority = _v0.a;
			var labels = A2(
				$elm$core$List$map,
				$elm$core$Tuple$second,
				A2(
					$elm$core$List$filter,
					function (_v1) {
						var key = _v1.a;
						return A2($elm$core$List$member, key, authority.canApprove) || (((key === 'Hiring') && authority.canHire) || ((key === 'Pricing') && authority.canChangePrice));
					},
					$author$project$Ui$Label$permissions));
			return $elm$core$List$isEmpty(labels) ? '없음' : A2($elm$core$String$join, ' · ', labels);
		}
	});
var $author$project$Page$Authorities$viewWith = F3(
	function (mode, model, w) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$author$project$Ui$Common$panel,
					'책임을 실행할 수 있는 권한',
					_List_fromArray(
						[
							$author$project$Ui$Common$note('현재 실제로 행사할 수 있는 결정 권한과 예산 한도를 기록하세요. 예: 환불 승인 가능 / 채용 승인 불가. 모르는 권한은 조직 진단에 미확인으로 남깁니다. 권한을 줄여 활성 목표의 요건이 깨지면 초안으로 돌아갑니다.'),
							$author$project$Ui$Common$note('집중도 = 보유 권한 종류 수 + 예산 보유 1점 / 조직 전체 점수. 실제 의사결정 빈도나 권력의 측정값은 아닙니다.')
						])),
					$elm$core$List$isEmpty(
					A2(
						$elm$core$List$filter,
						function ($) {
							return $.active;
						},
						w.people)) ? A2($author$project$Ui$Common$emptyState, '구성원을 먼저 추가하세요', '구성원 메뉴에서 재직 구성원을 추가한 뒤 권한을 부여할 수 있습니다.') : (_Utils_eq(mode, $author$project$Ui$ListView$Table) ? A3(
					$author$project$Ui$ListView$tableView,
					'구성원별 권한',
					_List_fromArray(
						['구성원', '역할', '현재 예산 한도', '보유 권한', '권한 비중', '담당 목표']),
					A2(
						$elm$core$List$concatMap,
						function (person) {
							return _List_fromArray(
								[
									A2(
									$elm$html$Html$tr,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$id('authority-' + person.id),
											$elm$html$Html$Attributes$tabindex(-1)
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$th,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$scope('row')
												]),
											_List_fromArray(
												[
													$elm$html$Html$text(person.name)
												])),
											A2(
											$elm$html$Html$td,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text(person.role)
												])),
											A2(
											$elm$html$Html$td,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text(
													A2($author$project$Page$Authorities$savedBudget, w, person))
												])),
											A2(
											$elm$html$Html$td,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text(
													A2($author$project$Page$Authorities$savedPermissions, w, person))
												])),
											A2(
											$elm$html$Html$td,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text(
													A2($author$project$Page$Authorities$authorityShare, w, person))
												])),
											A2(
											$elm$html$Html$td,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text(
													A2($author$project$Page$Authorities$goalCount, w, person))
												]))
										])),
									A4(
									$author$project$Ui$ListView$detailRow,
									6,
									_List_Nil,
									person.name + ' · 예산 · 권한 편집',
									_List_fromArray(
										[
											A2($author$project$Page$Authorities$authorityForm, model, person)
										]))
								]);
						},
						A2(
							$elm$core$List$filter,
							function ($) {
								return $.active;
							},
							w.people))) : A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('grid')
						]),
					A2(
						$elm$core$List$map,
						function (person) {
							return A2(
								$elm$html$Html$section,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('panel'),
										$elm$html$Html$Attributes$id('authority-' + person.id),
										$elm$html$Html$Attributes$tabindex(-1)
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$span,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('tag')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(
												'권한 비중 ' + A2($author$project$Page$Authorities$authorityShare, w, person))
											])),
										A2(
										$elm$html$Html$h2,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('form-heading')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(person.name)
											])),
										A2(
										$elm$html$Html$p,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('muted')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(person.role)
											])),
										A2($author$project$Page$Authorities$authorityForm, model, person),
										$author$project$Ui$Common$note(
										'담당 목표 ' + A2($author$project$Page$Authorities$goalCount, w, person))
									]));
						},
						A2(
							$elm$core$List$filter,
							function ($) {
								return $.active;
							},
							w.people)))),
					$author$project$Ui$Common$diagnosticView(w)
				]));
	});
var $author$project$Form$Action$Strategy = function (a) {
	return {$: 'Strategy', a: a};
};
var $author$project$Ui$Form$selectField = F6(
	function (model, action, label_, key, required_, options) {
		return A7(
			$author$project$Ui$Form$selectWithHelp,
			$author$project$Form$Action$actionKey(action) + ('-' + key),
			key,
			A2(model.value, action, key),
			A2(model.edit, action, key),
			label_,
			required_,
			options);
	});
var $author$project$Page$Goals$goalManagement = F3(
	function (model, w, g) {
		return _List_fromArray(
			[
				$author$project$Ui$Common$note(g.analysis.possibleCause),
				A4(
				$author$project$Ui$Form$formView,
				model.forms,
				$author$project$Form$Action$Assign(g.goal.id),
				'책임자 지정',
				_List_fromArray(
					[
						A6(
						$author$project$Ui$Form$selectField,
						model.forms,
						$author$project$Form$Action$Assign(g.goal.id),
						'단일 최종 책임자',
						'owner',
						true,
						$author$project$Ui$Form$peopleOptions(w))
					])),
				$author$project$Ui$Common$note('책임자 변경 또는 권한 부족 시 초안으로 돌아갑니다. 권한 메뉴에서 결정 권한을 조정하세요.'),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('actions')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$disabled(model.forms.busy || ((!model.forms.fresh) || g.active)),
								$elm$html$Html$Events$onClick(
								model.forms.submit(
									$author$project$Form$Action$Activate(g.goal.id)))
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(
								g.active ? '활성화됨' : '목표 활성화')
							]))
					])),
				A4(
				$author$project$Ui$Form$formView,
				model.forms,
				$author$project$Form$Action$Strategy(g.goal.id),
				'전략 변경 기록',
				_List_fromArray(
					[
						A6(
						$author$project$Ui$Form$inputField,
						model.forms,
						$author$project$Form$Action$Strategy(g.goal.id),
						'새로운 전략과 변경 이유',
						'note',
						'text',
						true)
					])),
				A2(
				$elm$html$Html$div,
				_List_Nil,
				A2(
					$elm$core$List$map,
					function (_v0) {
						var at = _v0.a;
						var message = _v0.b;
						return $author$project$Ui$Common$note(
							A2($elm$core$String$left, 10, at) + (' · ' + message));
					},
					g.strategies))
			]);
	});
var $author$project$Ui$Label$statusName = function (s) {
	switch (s.$) {
		case 'NoData':
			return '결과 대기';
		case 'OnTrack':
			return '정상';
		case 'AtRisk':
			return '위험';
		case 'OffTrack':
			return '이탈';
		default:
			return '달성';
	}
};
var $author$project$Ui$Common$badge = function (g) {
	return A2(
		$elm$html$Html$span,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$classList(
				_List_fromArray(
					[
						_Utils_Tuple2('tag', true),
						_Utils_Tuple2('draft', !g.active),
						_Utils_Tuple2(
						'error',
						_Utils_eq(g.evaluation.status, $author$project$Domain$OffTrack)),
						_Utils_Tuple2(
						'warn',
						_Utils_eq(g.evaluation.status, $author$project$Domain$AtRisk))
					]))
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(
				g.active ? $author$project$Ui$Label$statusName(g.evaluation.status) : '초안')
			]));
};
var $elm$html$Html$Attributes$max = $elm$html$Html$Attributes$stringProperty('max');
var $elm$html$Html$progress = _VirtualDom_node('progress');
var $author$project$Ui$Common$goalSummary = F2(
	function (w, g) {
		return _List_fromArray(
			[
				$author$project$Ui$Common$badge(g),
				A2(
				$elm$html$Html$h2,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(g.goal.description)
					])),
				A2(
				$elm$html$Html$small,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(
						g.goal.metric.name + (' · ' + (((g.goal.metric.direction === 'HigherIsBetter') ? '↑ 증가' : '↓ 감소') + ' 목표')))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('goal-values')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$strong,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(
								A2(
									$elm$core$Maybe$withDefault,
									'—',
									A2($elm$core$Maybe$map, $author$project$Ui$Label$formatNumber, g.evaluation.latestValue)))
							])),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('muted')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(
								'/ ' + ($author$project$Ui$Label$formatNumber(g.goal.target) + (' ' + g.goal.metric.unit)))
							]))
					])),
				A2(
				$elm$html$Html$progress,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$max('1'),
						$elm$html$Html$Attributes$value(
						$elm$core$String$fromFloat(
							A3($elm$core$Basics$clamp, 0, 1, g.evaluation.progress))),
						A2($elm$html$Html$Attributes$attribute, 'aria-label', '목표 달성률')
					]),
				_List_Nil),
				A2(
				$elm$html$Html$small,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(
						$elm$core$String$fromInt(
							$elm$core$Basics$round(g.evaluation.progress * 100)) + ('% 달성 · 기준 ' + $author$project$Ui$Label$formatNumber(g.goal.baseline)))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('meta')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(
								A2(
									$elm$core$Maybe$withDefault,
									'책임자 미지정',
									A2(
										$elm$core$Maybe$map,
										$author$project$Ui$Label$personName(w),
										g.owner)))
							])),
						A2(
						$elm$html$Html$span,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(
								A2($elm$core$String$left, 10, g.goal.deadline) + ' 마감')
							]))
					]))
			]);
	});
var $elm$virtual_dom$VirtualDom$property = F2(
	function (key, value) {
		return A2(
			_VirtualDom_property,
			_VirtualDom_noInnerHtmlOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlJson(value));
	});
var $elm$html$Html$Attributes$property = $elm$virtual_dom$VirtualDom$property;
var $author$project$Page$Goals$resultLink = F2(
	function (model, g) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('actions')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('secondary'),
							$elm$html$Html$Attributes$disabled(model.forms.busy),
							$elm$html$Html$Events$onClick(
							model.results('goal-' + g.goal.id))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('결과 보고 · 평가 →')
						]))
				]));
	});
var $author$project$Page$Goals$goalContent = F3(
	function (model, w, g) {
		return _Utils_ap(
			A2($author$project$Ui$Common$goalSummary, w, g),
			_List_fromArray(
				[
					A2($author$project$Page$Goals$resultLink, model, g),
					A2(
					$elm$html$Html$details,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$Attributes$property,
							'open',
							$elm$json$Json$Encode$bool(
								_Utils_eq(
									model.expandedGoal,
									$elm$core$Maybe$Just(g.goal.id))))
						]),
					A2(
						$elm$core$List$cons,
						A2(
							$elm$html$Html$summary,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('책임 · 권한 · 전략 관리')
								])),
						A3($author$project$Page$Goals$goalManagement, model, w, g)))
				]));
	});
var $author$project$Page$Goals$goalCard = F3(
	function (model, w, g) {
		return A2(
			$elm$html$Html$article,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('goal-card'),
					$elm$html$Html$Attributes$id('goal-' + g.goal.id),
					$elm$html$Html$Attributes$tabindex(-1)
				]),
			A3($author$project$Page$Goals$goalContent, model, w, g));
	});
var $author$project$Ui$Form$checkValues = F2(
	function (current, edit) {
		return A2(
			$elm$html$Html$fieldset,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('permission-fields')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$legend,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('결정 권한')
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('checks')
						]),
					A2(
						$elm$core$List$map,
						function (_v0) {
							var key = _v0.a;
							var label_ = _v0.b;
							return A2(
								$elm$html$Html$label,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$input,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$type_('checkbox'),
												$elm$html$Html$Attributes$checked(
												current(key) === 'true'),
												$elm$html$Html$Events$onCheck(
												function (checked_) {
													return A2(
														edit,
														key,
														checked_ ? 'true' : 'false');
												})
											]),
										_List_Nil),
										$elm$html$Html$text(label_)
									]));
						},
						$author$project$Ui$Label$permissions))
				]));
	});
var $author$project$Form$Goal$fieldName = function (field) {
	switch (field.$) {
		case 'Description':
			return 'description';
		case 'MetricName':
			return 'metricName';
		case 'Unit':
			return 'unit';
		case 'MetricId':
			return 'metricId';
		case 'Direction':
			return 'direction';
		case 'Baseline':
			return 'baseline';
		case 'Target':
			return 'target';
		case 'StartsAt':
			return 'startsAt';
		case 'Deadline':
			return 'deadline';
		case 'Budget':
			return 'budget';
		case 'Parent':
			return 'parent';
		default:
			var key = field.a;
			return key;
	}
};
var $author$project$Ui$Form$inputValue = F6(
	function (key, current, edit, label_, kind, required_) {
		return A7(
			$author$project$Ui$Form$guidedInput,
			key,
			label_,
			$author$project$Ui$Form$help(key),
			kind,
			required_,
			current,
			edit);
	});
var $author$project$Page$Goals$formInput = F5(
	function (model, label_, field, kind, required_) {
		return A6(
			$author$project$Ui$Form$inputValue,
			$author$project$Form$Goal$fieldName(field),
			A2($author$project$Form$Goal$value, model.draft, field),
			model.edit(field),
			label_,
			kind,
			required_);
	});
var $author$project$Page$Goals$formSelect = F5(
	function (model, label_, field, required_, options) {
		return A6(
			$author$project$Ui$Form$selectValue,
			$author$project$Form$Goal$fieldName(field),
			A2($author$project$Form$Goal$value, model.draft, field),
			model.edit(field),
			label_,
			required_,
			options);
	});
var $author$project$Ui$Form$goalOptions = function (w) {
	return A2(
		$elm$core$List$cons,
		_Utils_Tuple2('', '목표 선택'),
		A2(
			$elm$core$List$map,
			function (g) {
				return _Utils_Tuple2(g.goal.id, g.goal.description);
			},
			w.goals));
};
var $elm$core$Dict$values = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return A2($elm$core$List$cons, value, valueList);
			}),
		_List_Nil,
		dict);
};
var $author$project$Page$Goals$metricPicker = F2(
	function (model, w) {
		var metrics = $elm$core$Dict$fromList(
			A2(
				$elm$core$List$map,
				function (metric) {
					return _Utils_Tuple2(metric.id, metric);
				},
				A2(
					$elm$core$List$map,
					A2(
						$elm$core$Basics$composeR,
						function ($) {
							return $.goal;
						},
						function ($) {
							return $.metric;
						}),
					w.goals)));
		var selected = A2($elm$core$Dict$get, model.draft.metricId, metrics);
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A6(
					$author$project$Ui$Form$selectValue,
					'goal-metric-choice',
					A2(
						$elm$core$Maybe$withDefault,
						'',
						A2(
							$elm$core$Maybe$map,
							function ($) {
								return $.id;
							},
							selected)),
					model.edit($author$project$Form$Goal$MetricId),
					'사용할 지표',
					false,
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2('', '새 지표 만들기 · ID 자동 생성'),
						A2(
							$elm$core$List$map,
							function (metric) {
								return _Utils_Tuple2(metric.id, metric.name + (' · ' + metric.unit));
							},
							$elm$core$Dict$values(metrics)))),
					$author$project$Ui$Common$note('같은 지표를 공유하는 목표는 기존 지표를 선택하세요. 동일 지표의 책임 관계를 연결하는 데 사용합니다. 이름이 같아도 정의가 다르면 새 지표를 만드세요.'),
					function () {
					if (selected.$ === 'Just') {
						var metric = selected.a;
						return $author$project$Ui$Common$note(
							'선택한 지표: ' + (metric.name + (' / ' + (metric.unit + (' / ' + ((metric.direction === 'HigherIsBetter') ? '높을수록 좋음' : '낮을수록 좋음'))))));
					} else {
						return A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('fields')
								]),
							_List_fromArray(
								[
									A5($author$project$Page$Goals$formInput, model, 'KPI 이름', $author$project$Form$Goal$MetricName, 'text', true),
									A5($author$project$Page$Goals$formInput, model, '단위', $author$project$Form$Goal$Unit, 'text', true),
									A5(
									$author$project$Page$Goals$formSelect,
									model,
									'좋은 결과의 방향',
									$author$project$Form$Goal$Direction,
									true,
									_List_fromArray(
										[
											_Utils_Tuple2('HigherIsBetter', '높을수록 좋음'),
											_Utils_Tuple2('LowerIsBetter', '낮을수록 좋음')
										]))
								]));
					}
				}()
				]));
	});
var $author$project$Page$Goals$goalForm = F2(
	function (model, w) {
		return A4(
			$author$project$Ui$Form$formView,
			model.forms,
			$author$project$Form$Action$AddGoal,
			'목표 초안 생성',
			_List_fromArray(
				[
					$author$project$Ui$Common$note('현재 관리 중인 목표를 정리하는 운영 화면입니다. 아직 목표나 측정 기준을 모른다면 조직 진단에 미확인으로 남기고 나중에 입력하세요. 초안 생성 후 책임·권한을 확인하여 활성화합니다.'),
					A2(
					$elm$html$Html$fieldset,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('form-section')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$legend,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('1 · 책임져야 하는 결과')
								])),
							A5($author$project$Page$Goals$formInput, model, '현재 관리 중인 목표 / 결과', $author$project$Form$Goal$Description, 'text', true),
							A5(
							$author$project$Page$Goals$formSelect,
							model,
							'상위 목표 (선택)',
							$author$project$Form$Goal$Parent,
							false,
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2('', '없음'),
								A2(
									$elm$core$List$drop,
									1,
									$author$project$Ui$Form$goalOptions(w))))
						])),
					A2(
					$elm$html$Html$fieldset,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('form-section')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$legend,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('2 · 결과를 확인하는 측정 기준')
								])),
							A2($author$project$Page$Goals$metricPicker, model, w),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('fields')
								]),
							_List_fromArray(
								[
									A5($author$project$Page$Goals$formInput, model, '기준값', $author$project$Form$Goal$Baseline, 'number', true),
									A5($author$project$Page$Goals$formInput, model, '목표값', $author$project$Form$Goal$Target, 'number', true),
									A5($author$project$Page$Goals$formInput, model, '시작일 (UTC)', $author$project$Form$Goal$StartsAt, 'date', true),
									A5($author$project$Page$Goals$formInput, model, '마감일 (UTC)', $author$project$Form$Goal$Deadline, 'date', true)
								]))
						])),
					A2(
					$elm$html$Html$fieldset,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('form-section')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$legend,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('3 · 목표 실행에 필요한 조건')
								])),
							$author$project$Ui$Common$note('이 목표에 필요한 권한과 예산입니다. 현재 책임자가 보유한 권한은 권한 화면에서 별도로 기록합니다. 확인되지 않은 조건을 0이나 권한 없음으로 대신 입력하지 마세요.'),
							A5($author$project$Page$Goals$formInput, model, '필요 예산 (KRW)', $author$project$Form$Goal$Budget, 'number', true),
							A2(
							$author$project$Ui$Form$checkValues,
							function (key) {
								return A2(
									$author$project$Form$Goal$value,
									model.draft,
									$author$project$Form$Goal$Permission(key));
							},
							function (key) {
								return model.edit(
									$author$project$Form$Goal$Permission(key));
							})
						]))
				]));
	});
var $author$project$Page$Goals$goalTable = F2(
	function (model, w) {
		return A3(
			$author$project$Ui$ListView$tableView,
			'목표 포트폴리오',
			_List_fromArray(
				['목표 / KPI', '최종 책임자', '현재값 / 목표값', '달성률', '마감', '상태']),
			A2(
				$elm$core$List$concatMap,
				function (g) {
					return _List_fromArray(
						[
							A2(
							$elm$html$Html$tr,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$id('goal-' + g.goal.id),
									$elm$html$Html$Attributes$tabindex(-1)
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$th,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$scope('row')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$strong,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text(g.goal.description)
												])),
											A2(
											$elm$html$Html$small,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text(
													g.goal.metric.name + (' · ' + ((g.goal.metric.direction === 'HigherIsBetter') ? '↑ 증가' : '↓ 감소')))
												]))
										])),
									A2(
									$elm$html$Html$td,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(
											A2(
												$elm$core$Maybe$withDefault,
												'책임자 미지정',
												A2(
													$elm$core$Maybe$map,
													$author$project$Ui$Label$personName(w),
													g.owner)))
										])),
									A2(
									$elm$html$Html$td,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(
											A2(
												$elm$core$Maybe$withDefault,
												'—',
												A2($elm$core$Maybe$map, $author$project$Ui$Label$formatNumber, g.evaluation.latestValue)) + (' / ' + ($author$project$Ui$Label$formatNumber(g.goal.target) + (' ' + g.goal.metric.unit))))
										])),
									A2(
									$elm$html$Html$td,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(
											$elm$core$String$fromInt(
												$elm$core$Basics$round(g.evaluation.progress * 100)) + '%'),
											A2(
											$elm$html$Html$small,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text(
													'기준 ' + $author$project$Ui$Label$formatNumber(g.goal.baseline))
												]))
										])),
									A2(
									$elm$html$Html$td,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(
											A2($elm$core$String$left, 10, g.goal.deadline))
										])),
									A2(
									$elm$html$Html$td,
									_List_Nil,
									_List_fromArray(
										[
											$author$project$Ui$Common$badge(g)
										]))
								])),
							A4(
							$author$project$Ui$ListView$detailRow,
							6,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$Attributes$property,
									'open',
									$elm$json$Json$Encode$bool(
										_Utils_eq(
											model.expandedGoal,
											$elm$core$Maybe$Just(g.goal.id))))
								]),
							g.goal.description + ' · 책임 · 권한 · 전략 관리',
							A2(
								$elm$core$List$cons,
								A2($author$project$Page$Goals$resultLink, model, g),
								A3($author$project$Page$Goals$goalManagement, model, w, g)))
						]);
				},
				w.goals));
	});
var $elm$core$List$sum = function (numbers) {
	return A3($elm$core$List$foldl, $elm$core$Basics$add, 0, numbers);
};
var $author$project$Page$Goals$viewWith = F3(
	function (mode, model, w) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('metrics')
						]),
					A2(
						$elm$core$List$map,
						function (_v0) {
							var label_ = _v0.a;
							var amount = _v0.b;
							var desc = _v0.c;
							return A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('metric')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$span,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text(label_)
											])),
										A2(
										$elm$html$Html$strong,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text(
												$elm$core$String$fromInt(amount))
											])),
										A2(
										$elm$html$Html$small,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text(desc)
											]))
									]));
						},
						_List_fromArray(
							[
								_Utils_Tuple3(
								'전체 목표',
								$elm$core$List$length(w.goals),
								'측정 가능한 결과'),
								_Utils_Tuple3(
								'활성 목표',
								$elm$core$List$length(
									A2(
										$elm$core$List$filter,
										function ($) {
											return $.active;
										},
										w.goals)),
								'책임과 권한 검증 완료'),
								_Utils_Tuple3('구조 진단', w.compiler.errors + w.compiler.warnings, '확인이 필요한 항목'),
								_Utils_Tuple3(
								'누적 학습',
								$elm$core$List$sum(
									A2(
										$elm$core$List$map,
										A2(
											$elm$core$Basics$composeR,
											function ($) {
												return $.learnings;
											},
											$elm$core$List$length),
										w.reviews)),
								'다음 결정의 근거')
							]))),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('section-head')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$h2,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('목표 포트폴리오')
								])),
							A2(
							$elm$html$Html$a,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$href('#new-goal')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('+ 목표 만들기')
								]))
						])),
					$elm$core$List$isEmpty(w.goals) ? A2($author$project$Ui$Common$emptyState, '현재 관리 중인 목표가 있나요?', '확인된 측정 기준이 있다면 아래에서 목표 초안을 만드세요. 모르는 내용은 조직 진단에 미확인으로 남길 수 있습니다.') : (_Utils_eq(mode, $author$project$Ui$ListView$Table) ? A2($author$project$Page$Goals$goalTable, model, w) : A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('grid')
						]),
					A2(
						$elm$core$List$map,
						A2($author$project$Page$Goals$goalCard, model, w),
						w.goals))),
					A2(
					$elm$html$Html$details,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('panel'),
							$elm$html$Html$Attributes$id('new-goal')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$summary,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('+ 목표 만들기')
								])),
							A2($author$project$Page$Goals$goalForm, model, w)
						])),
					$author$project$Ui$Common$diagnosticView(w)
				]));
	});
var $author$project$Form$Review$fieldName = function (field) {
	switch (field.$) {
		case 'Goal':
			return 'goal';
		case 'Note':
			return 'note';
		case 'Learning':
			return 'learning';
		case 'Decision':
			return 'decision';
		case 'DecisionOwner':
			return 'decisionOwner';
		default:
			return 'decisionDeadline';
	}
};
var $author$project$Page$Learning$formInput = F5(
	function (model, label_, field, kind, required_) {
		return A6(
			$author$project$Ui$Form$inputValue,
			$author$project$Form$Review$fieldName(field),
			A2($author$project$Form$Review$value, model.draft, field),
			model.edit(field),
			label_,
			kind,
			required_);
	});
var $author$project$Page$Learning$formSelect = F5(
	function (model, label_, field, required_, options) {
		return A6(
			$author$project$Ui$Form$selectValue,
			$author$project$Form$Review$fieldName(field),
			A2($author$project$Form$Review$value, model.draft, field),
			model.edit(field),
			label_,
			required_,
			options);
	});
var $author$project$Page$Learning$activityLink = F2(
	function (activity, review) {
		return A2(
			$elm$core$Maybe$withDefault,
			$elm$html$Html$text(''),
			A2(
				$elm$core$Maybe$map,
				function (go) {
					return A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$type_('button'),
								$elm$html$Html$Attributes$class('secondary'),
								$elm$html$Html$Events$onClick(
								go(review))
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('관련 활동 기록 →')
							]));
				},
				activity));
	});
var $elm$html$Html$br = _VirtualDom_node('br');
var $author$project$Page$Learning$reviewCard = F3(
	function (activity, w, r) {
		return A2(
			$elm$html$Html$section,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('panel')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('tag')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							A2($elm$core$String$left, 10, r.heldAt) + (' · ' + $author$project$Ui$Label$statusName(r.evaluation.status)))
						])),
					A2(
					$elm$html$Html$h2,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('form-heading')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							A2($author$project$Ui$Label$goalName, w, r.goal))
						])),
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(r.note)
						])),
					A2($author$project$Page$Learning$activityLink, activity, r.id),
					A2(
					$elm$html$Html$h3,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('학습')
						])),
					$elm$core$List$isEmpty(r.learnings) ? $author$project$Ui$Common$note('기록된 학습 없음') : A2(
					$elm$html$Html$div,
					_List_Nil,
					A2(
						$elm$core$List$map,
						function (learning) {
							return A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(learning)
									]));
						},
						r.learnings)),
					A2(
					$elm$html$Html$h3,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('다음 결정')
						])),
					$elm$core$List$isEmpty(r.decisions) ? $author$project$Ui$Common$note('기록된 결정 없음') : A2(
					$elm$html$Html$div,
					_List_Nil,
					A2(
						$elm$core$List$map,
						function (d) {
							return A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(d.text),
										A2($elm$html$Html$br, _List_Nil, _List_Nil),
										A2(
										$elm$html$Html$small,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text(
												A2($author$project$Ui$Label$personName, w, d.owner) + (' · ' + A2(
													$elm$core$Maybe$withDefault,
													'기한 미정',
													A2(
														$elm$core$Maybe$map,
														$elm$core$String$left(10),
														d.deadline))))
											]))
									]));
						},
						r.decisions)),
					A2(
					$elm$html$Html$div,
					_List_Nil,
					A2(
						$elm$core$List$map,
						function (warning) {
							return A2(
								$elm$html$Html$p,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('tag warn')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(warning)
									]));
						},
						A2(
							$elm$core$List$concatMap,
							function ($) {
								return $.warnings;
							},
							A2(
								$elm$core$List$filter,
								A2(
									$elm$core$Basics$composeR,
									function ($) {
										return $.id;
									},
									$elm$core$Basics$eq(r.id)),
								w.reviewWarnings))))
				]));
	});
var $author$project$Page$Learning$reviewTable = F2(
	function (activity, w) {
		return A3(
			$author$project$Ui$ListView$tableView,
			'회고와 학습',
			_List_fromArray(
				['회고일', '목표', '평가', '회고 요약', '학습', '다음 결정', '경고', '활동 기록']),
			A2(
				$elm$core$List$map,
				function (r) {
					return A2(
						$elm$html$Html$tr,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										A2($elm$core$String$left, 10, r.heldAt))
									])),
								A2(
								$elm$html$Html$th,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$scope('row')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(
										A2($author$project$Ui$Label$goalName, w, r.goal))
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										$author$project$Ui$Label$statusName(r.evaluation.status))
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(r.note)
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								$elm$core$List$isEmpty(r.learnings) ? _List_fromArray(
									[
										$author$project$Ui$Common$note('기록된 학습 없음')
									]) : A2(
									$elm$core$List$map,
									function (learning) {
										return A2(
											$elm$html$Html$p,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text(learning)
												]));
									},
									r.learnings)),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								$elm$core$List$isEmpty(r.decisions) ? _List_fromArray(
									[
										$author$project$Ui$Common$note('기록된 결정 없음')
									]) : A2(
									$elm$core$List$map,
									function (d) {
										return A2(
											$elm$html$Html$p,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text(d.text),
													A2($elm$html$Html$br, _List_Nil, _List_Nil),
													A2(
													$elm$html$Html$small,
													_List_Nil,
													_List_fromArray(
														[
															$elm$html$Html$text(
															A2($author$project$Ui$Label$personName, w, d.owner) + (' · ' + A2(
																$elm$core$Maybe$withDefault,
																'기한 미정',
																A2(
																	$elm$core$Maybe$map,
																	$elm$core$String$left(10),
																	d.deadline))))
														]))
												]));
									},
									r.decisions)),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								A2(
									$elm$core$List$map,
									function (warning) {
										return A2(
											$elm$html$Html$p,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$class('tag warn')
												]),
											_List_fromArray(
												[
													$elm$html$Html$text(warning)
												]));
									},
									A2(
										$elm$core$List$concatMap,
										function ($) {
											return $.warnings;
										},
										A2(
											$elm$core$List$filter,
											A2(
												$elm$core$Basics$composeR,
												function ($) {
													return $.id;
												},
												$elm$core$Basics$eq(r.id)),
											w.reviewWarnings)))),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										A2($author$project$Page$Learning$activityLink, activity, r.id)
									]))
							]));
				},
				w.reviews));
	});
var $author$project$Page$Learning$viewWithActivity = F4(
	function (activity, mode, model, w) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$author$project$Ui$Common$panel,
					'관찰한 결과를 다음 결정으로 연결하세요',
					_List_fromArray(
						[
							$author$project$Ui$Common$note('조직과 에이전트 역할을 검토한 뒤 사용하는 운영 단계입니다. 관찰한 사실, 새롭게 배운 점, 바꿀 결정을 나누어 기록하세요.'),
							$author$project$Ui$Common$note('예: 긴급 문의 응답 지연(사실) → 승인 대기 원인 확인(학습) → 승인 담당과 시간 기준을 명시(다음 결정)')
						])),
					A2(
					$elm$html$Html$section,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('panel'),
							$elm$html$Html$Attributes$id('review-form'),
							$elm$html$Html$Attributes$tabindex(-1)
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$h2,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('회고와 다음 결정 기록')
								])),
							A4(
							$author$project$Ui$Form$formView,
							model.forms,
							$author$project$Form$Action$AddReview,
							'회고 기록',
							_List_fromArray(
								[
									A5(
									$author$project$Page$Learning$formSelect,
									model,
									'회고할 목표',
									$author$project$Form$Review$Goal,
									true,
									$author$project$Ui$Form$goalOptions(w)),
									A5($author$project$Page$Learning$formInput, model, '회고 요약', $author$project$Form$Review$Note, 'text', true),
									A2(
									$elm$html$Html$label,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text('새롭게 배운 점 (선택)'),
											A2(
											$elm$html$Html$textarea,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$value(
													A2($author$project$Form$Review$value, model.draft, $author$project$Form$Review$Learning)),
													$elm$html$Html$Events$onInput(
													model.edit($author$project$Form$Review$Learning))
												]),
											_List_Nil)
										])),
									A5($author$project$Page$Learning$formInput, model, '다음 결정 (선택)', $author$project$Form$Review$Decision, 'text', false),
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('fields')
										]),
									_List_fromArray(
										[
											A5(
											$author$project$Page$Learning$formSelect,
											model,
											'결정 담당자',
											$author$project$Form$Review$DecisionOwner,
											false,
											$author$project$Ui$Form$peopleOptions(w)),
											A5($author$project$Page$Learning$formInput, model, '결정 기한 (UTC, 선택)', $author$project$Form$Review$DecisionDeadline, 'date', false)
										])),
									$author$project$Ui$Common$note('현재 최신 결과와 평가가 함께 보존됩니다. 결정과 학습이 모두 없으면 구조 검사가 경고합니다.')
								]))
						])),
					$elm$core$List$isEmpty(w.reviews) ? A2($author$project$Ui$Common$emptyState, '아직 회고 기록이 없습니다', '위에서 회고를 기록해 학습과 다음 결정을 남기세요.') : (_Utils_eq(mode, $author$project$Ui$ListView$Table) ? A2($author$project$Page$Learning$reviewTable, activity, w) : A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('grid')
						]),
					A2(
						$elm$core$List$map,
						A2($author$project$Page$Learning$reviewCard, activity, w),
						w.reviews)))
				]));
	});
var $author$project$Page$Learning$viewWith = $author$project$Page$Learning$viewWithActivity($elm$core$Maybe$Nothing);
var $author$project$Form$Action$CreateOrg = {$: 'CreateOrg'};
var $author$project$Form$Action$ImportDemo = {$: 'ImportDemo'};
var $author$project$Page$Organizations$organizationTable = F2(
	function (model, items) {
		return A3(
			$author$project$Ui$ListView$tableView,
			'등록된 조직',
			_List_fromArray(
				['조직명', '구분', '구성원 수', '목표 수', '등록일', '관리']),
			A2(
				$elm$core$List$map,
				function (item) {
					return A2(
						$elm$html$Html$tr,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$th,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$scope('row')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(item.organization.name)
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										item.demo ? '가상 데이터 · 데모' : '내 조직')
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										$elm$core$String$fromInt(item.peopleCount) + '명')
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										$elm$core$String$fromInt(item.goalCount) + '개')
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										A2($elm$core$String$left, 10, item.organization.createdAt))
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$div,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('actions')
											]),
										_List_fromArray(
											[
												A2(
												$elm$html$Html$button,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$disabled(model.forms.busy),
														$elm$html$Html$Events$onClick(
														model.open(item.organization.id))
													]),
												_List_fromArray(
													[
														$elm$html$Html$text('조직 열기 →')
													])),
												A2(
												$elm$html$Html$button,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('secondary'),
														$elm$html$Html$Attributes$disabled(model.forms.busy),
														$elm$html$Html$Events$onClick(
														model.settings(item.organization.id))
													]),
												_List_fromArray(
													[
														$elm$html$Html$text('상세 · 수정 · 삭제')
													]))
											]))
									]))
							]));
				},
				items));
	});
var $author$project$Remote$view = F2(
	function (remote, render) {
		switch (remote.$) {
			case 'Loading':
				return A2(
					$elm$html$Html$section,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('panel'),
							A2($elm$html$Html$Attributes$attribute, 'role', 'status')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('워크스페이스를 불러오는 중…')
						]));
			case 'Failed':
				var message = remote.a;
				return A2($author$project$Ui$Common$emptyState, '조회하지 못했습니다', message);
			default:
				var data = remote.a;
				return render(data);
		}
	});
var $author$project$Page$Organizations$viewWith = F2(
	function (mode, model) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$author$project$Ui$Common$panel,
					'현재 조직을 이해하고 멀티 AI 에이전트 구조를 설계하세요',
					_List_fromArray(
						[
							$author$project$Ui$Common$note('현재 조직의 역할·책임·업무 흐름을 기록하면, 저장된 근거로 에이전트 역할과 인계 구조의 초안을 검토할 수 있습니다.'),
							$author$project$Ui$Common$note('1. 현재 사실과 미확인 내용을 기록 → 2. 업무의 입력·산출물·인계를 연결 → 3. 규칙 기반 에이전트 제안을 사람이 검토'),
							$author$project$Ui$Common$note('지금 확인할 수 있는 정보부터 시작하세요. 실제 AI 에이전트를 실행하거나 외부 도구의 권한을 부여하는 기능은 아닙니다.')
						])),
					A2(
					$author$project$Ui$Common$panel,
					'새 조직 등록',
					_List_fromArray(
						[
							$author$project$Ui$Common$note('정리할 실제 조직이나 팀의 이름을 입력하세요. 예: 고객지원팀. 현황·업무·검토와 운영 기록은 조직별로 분리됩니다.'),
							A4(
							$author$project$Ui$Form$formView,
							model.forms,
							$author$project$Form$Action$CreateOrg,
							'조직 등록',
							_List_fromArray(
								[
									A6($author$project$Ui$Form$inputField, model.forms, $author$project$Form$Action$CreateOrg, '조직 이름', 'name', 'text', true)
								])),
							A2(
							$elm$html$Html$button,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('secondary'),
									$elm$html$Html$Attributes$disabled(
									model.forms.busy || ((!model.forms.fresh) || function () {
										var _v0 = model.organizations;
										if (_v0.$ === 'Loaded') {
											var items = _v0.a;
											return A2(
												$elm$core$List$any,
												A2(
													$elm$core$Basics$composeR,
													function ($) {
														return $.organization;
													},
													A2(
														$elm$core$Basics$composeR,
														function ($) {
															return $.id;
														},
														$elm$core$Basics$eq('demo-northstar-v2'))),
												items);
										} else {
											return true;
										}
									}())),
									$elm$html$Html$Events$onClick(
									model.forms.submit($author$project$Form$Action$ImportDemo))
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('체험용 데모 조직 추가')
								]))
						])),
					A2(
					$author$project$Remote$view,
					model.organizations,
					function (items) {
						return A2(
							$elm$html$Html$div,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('section-head')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$h2,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text('등록된 조직')
												])),
											A2(
											$elm$html$Html$span,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$class('tag')
												]),
											_List_fromArray(
												[
													$elm$html$Html$text(
													$elm$core$String$fromInt(
														$elm$core$List$length(items)) + '개')
												]))
										])),
									$elm$core$List$isEmpty(items) ? A2($author$project$Ui$Common$emptyState, '첫 조직을 시작하세요', '조직 이름을 등록한 뒤 조직 열기로 현황을 입력하세요. 데모 조직에서는 기존 목표·책임·권한 운영 흐름을 체험할 수 있습니다.') : (_Utils_eq(mode, $author$project$Ui$ListView$Table) ? A2($author$project$Page$Organizations$organizationTable, model, items) : A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('grid')
										]),
									A2(
										$elm$core$List$map,
										function (item) {
											return A2(
												$elm$html$Html$section,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('panel organization-card')
													]),
												_List_fromArray(
													[
														A2(
														$elm$html$Html$span,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$class('tag')
															]),
														_List_fromArray(
															[
																$elm$html$Html$text(
																item.demo ? '가상 데이터 · 데모' : '내 조직')
															])),
														A2(
														$elm$html$Html$h2,
														_List_Nil,
														_List_fromArray(
															[
																$elm$html$Html$text(item.organization.name)
															])),
														A2(
														$elm$html$Html$p,
														_List_Nil,
														_List_fromArray(
															[
																$elm$html$Html$text(
																'구성원 ' + ($elm$core$String$fromInt(item.peopleCount) + ('명 · 목표 ' + ($elm$core$String$fromInt(item.goalCount) + '개'))))
															])),
														A2(
														$elm$html$Html$small,
														_List_Nil,
														_List_fromArray(
															[
																$elm$html$Html$text(
																'등록 ' + A2($elm$core$String$left, 10, item.organization.createdAt))
															])),
														A2(
														$elm$html$Html$div,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$class('actions')
															]),
														_List_fromArray(
															[
																A2(
																$elm$html$Html$button,
																_List_fromArray(
																	[
																		$elm$html$Html$Attributes$disabled(model.forms.busy),
																		$elm$html$Html$Events$onClick(
																		model.open(item.organization.id))
																	]),
																_List_fromArray(
																	[
																		$elm$html$Html$text('조직 열기 →')
																	])),
																A2(
																$elm$html$Html$button,
																_List_fromArray(
																	[
																		$elm$html$Html$Attributes$class('secondary'),
																		$elm$html$Html$Attributes$disabled(model.forms.busy),
																		$elm$html$Html$Events$onClick(
																		model.settings(item.organization.id))
																	]),
																_List_fromArray(
																	[
																		$elm$html$Html$text('상세 · 수정 · 삭제')
																	]))
															]))
													]));
										},
										items)))
								]));
					})
				]));
	});
var $author$project$Page$People$profileFields = F4(
	function (forms, w, action, personId) {
		return _List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('fields')
					]),
				_List_fromArray(
					[
						A6($author$project$Ui$Form$inputField, forms, action, '이름', 'name', 'text', true),
						A6($author$project$Ui$Form$inputField, forms, action, '역할', 'role', 'text', true),
						A6($author$project$Ui$Form$inputField, forms, action, '부서 (선택)', 'department', 'text', false),
						A6($author$project$Ui$Form$inputField, forms, action, '이메일 (선택)', 'email', 'email', false),
						A6(
						$author$project$Ui$Form$selectField,
						forms,
						action,
						'보고 대상 (선택)',
						'reportsTo',
						false,
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2('', '없음'),
							A2(
								$elm$core$List$map,
								function (p) {
									return _Utils_Tuple2(p.id, p.name + (' · ' + p.role));
								},
								A2(
									$elm$core$List$filter,
									function (p) {
										return p.active && (!_Utils_eq(
											$elm$core$Maybe$Just(p.id),
											personId));
									},
									w.people))))
					]))
			]);
	});
var $author$project$Page$People$detail = F3(
	function (model, w, person) {
		var reports = A2(
			$elm$core$List$filter,
			A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.reportsTo;
				},
				$elm$core$Basics$eq(
					$elm$core$Maybe$Just(person.id))),
			w.people);
		var goals = A2(
			$elm$core$List$filter,
			A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.owner;
				},
				$elm$core$Basics$eq(
					$elm$core$Maybe$Just(person.id))),
			w.goals);
		var requiresSuccessor = !($elm$core$List$isEmpty(goals) && $elm$core$List$isEmpty(reports));
		var action = $author$project$Form$Action$DeactivatePerson(person.id);
		return A2(
			$elm$html$Html$section,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('panel'),
					$elm$html$Html$Attributes$id('person-detail'),
					$elm$html$Html$Attributes$tabindex(-1)
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$h2,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							A2($author$project$Ui$Label$personName, w, person.id) + ' · 상세')
						])),
					$author$project$Ui$Common$note('구성원 ID: ' + person.id),
					$author$project$Ui$Common$note(
					'보고 대상: ' + A2(
						$elm$core$Maybe$withDefault,
						'없음',
						A2(
							$elm$core$Maybe$map,
							$author$project$Ui$Label$personName(w),
							person.reportsTo))),
					$author$project$Ui$Common$note(
					'직속 보고자: ' + ($elm$core$List$isEmpty(reports) ? '없음' : A2(
						$elm$core$String$join,
						', ',
						A2(
							$elm$core$List$map,
							function (p) {
								return A2($author$project$Ui$Label$personName, w, p.id);
							},
							reports)))),
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('secondary'),
							$elm$html$Html$Attributes$disabled(model.forms.busy || (!model.forms.fresh)),
							$elm$html$Html$Events$onClick(
							model.reset(person.id))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('최신 정보로 다시 불러오기')
						])),
					$author$project$Ui$Common$note('다시 불러오면 이 구성원의 저장하지 않은 기본정보와 인계 입력이 초기화됩니다.'),
					A4(
					$author$project$Ui$Form$formView,
					model.forms,
					$author$project$Form$Action$UpdatePerson(person.id),
					'기본정보 저장',
					A4(
						$author$project$Page$People$profileFields,
						model.forms,
						w,
						$author$project$Form$Action$UpdatePerson(person.id),
						$elm$core$Maybe$Just(person.id))),
					A2(
					$elm$html$Html$h3,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('담당 목표')
						])),
					$elm$core$List$isEmpty(goals) ? $author$project$Ui$Common$note('현재 담당 목표가 없습니다.') : A2(
					$elm$html$Html$ul,
					_List_Nil,
					A2(
						$elm$core$List$map,
						function (g) {
							return A2(
								$elm$html$Html$li,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(g.goal.description)
									]));
						},
						goals)),
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('secondary'),
							$elm$html$Html$Attributes$disabled(model.forms.busy),
							$elm$html$Html$Events$onClick(model.goals)
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('목표 관리 →')
						])),
					person.active ? A2(
					$elm$html$Html$details,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('person-deactivate')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$summary,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('구성원 비활성화 · 업무 인계')
								])),
							$author$project$Ui$Common$note(
							'담당 목표 ' + ($elm$core$String$fromInt(
								$elm$core$List$length(goals)) + ('개와 직속 보고자 ' + ($elm$core$String$fromInt(
								$elm$core$List$length(reports)) + '명을 인계합니다. 연결된 업무 또는 보고자가 있으면 후임을 지정해야 합니다.')))),
							$author$project$Ui$Common$note('인계한 목표는 초안으로 전환됩니다. 후임의 권한과 예산을 확인한 뒤 다시 활성화하세요. 기존 권한은 자동 복사되지 않습니다. 직속 보고자가 후임이면 기존 상위 보고자에게 연결됩니다.'),
							$author$project$Ui$Common$note('과거 결과·회고·감사 기록은 기존 구성원을 유지합니다. 비활성화 후 신규 배정은 제한됩니다.'),
							A4(
							$author$project$Ui$Form$formView,
							model.forms,
							action,
							'비활성화 및 인계 확정',
							_List_fromArray(
								[
									A6(
									$author$project$Ui$Form$selectField,
									model.forms,
									action,
									'후임 구성원',
									'successor',
									requiresSuccessor,
									A2(
										$elm$core$List$cons,
										_Utils_Tuple2(
											'',
											requiresSuccessor ? '후임 선택 (필수)' : '인계 대상 없음'),
										A2(
											$elm$core$List$map,
											function (p) {
												return _Utils_Tuple2(p.id, p.name + (' · ' + p.role));
											},
											A2(
												$elm$core$List$filter,
												function (p) {
													return p.active && (!_Utils_eq(p.id, person.id));
												},
												w.people))))
								]))
						])) : $author$project$Ui$Common$note('비활성 구성원입니다. 기본정보를 수정하고 과거 기록을 조회할 수 있으며 새 업무를 배정할 수 없습니다.')
				]));
	});
var $author$project$Page$People$matches = F3(
	function (query, status, person) {
		return ((status === 'all') || (((status === 'active') && person.active) || ((status === 'inactive') && (!person.active)))) && A2(
			$elm$core$String$contains,
			$elm$core$String$toLower(
				$elm$core$String$trim(query)),
			$elm$core$String$toLower(
				A2(
					$elm$core$String$join,
					' ',
					_List_fromArray(
						[
							person.name,
							person.role,
							A2($elm$core$Maybe$withDefault, '', person.department),
							A2($elm$core$Maybe$withDefault, '', person.email)
						]))));
	});
var $author$project$Page$People$peopleTable = F3(
	function (model, w, people) {
		return A3(
			$author$project$Ui$ListView$tableView,
			'구성원',
			_List_fromArray(
				['이름', '역할', '부서', '이메일', '재직 상태', '담당 목표', '관리']),
			A2(
				$elm$core$List$map,
				function (person) {
					return A2(
						$elm$html$Html$tr,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$th,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$scope('row')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(person.name)
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(person.role)
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										A2($elm$core$Maybe$withDefault, '부서 미입력', person.department))
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										A2($elm$core$Maybe$withDefault, '이메일 미입력', person.email))
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										person.active ? '재직' : '비활성')
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										$elm$core$String$fromInt(
											$elm$core$List$length(
												A2(
													$elm$core$List$filter,
													A2(
														$elm$core$Basics$composeR,
														function ($) {
															return $.owner;
														},
														$elm$core$Basics$eq(
															$elm$core$Maybe$Just(person.id))),
													w.goals))) + '개')
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('secondary'),
												$elm$html$Html$Attributes$disabled(model.forms.busy),
												$elm$html$Html$Events$onClick(
												model.open(person.id))
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('상세 · 수정')
											]))
									]))
							]));
				},
				people));
	});
var $author$project$Page$People$personCard = F3(
	function (model, w, person) {
		return A2(
			$elm$html$Html$article,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('person-card goal-card')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$h3,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(person.name)
						])),
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('tag')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							person.active ? '재직' : '비활성')
						])),
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							person.role + (' · ' + A2($elm$core$Maybe$withDefault, '부서 미입력', person.department)))
						])),
					A2(
					$elm$html$Html$p,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('muted')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							A2($elm$core$Maybe$withDefault, '이메일 미입력', person.email))
						])),
					$author$project$Ui$Common$note(
					'담당 목표 ' + ($elm$core$String$fromInt(
						$elm$core$List$length(
							A2(
								$elm$core$List$filter,
								A2(
									$elm$core$Basics$composeR,
									function ($) {
										return $.owner;
									},
									$elm$core$Basics$eq(
										$elm$core$Maybe$Just(person.id))),
								w.goals))) + '개')),
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('secondary'),
							$elm$html$Html$Attributes$disabled(model.forms.busy),
							$elm$html$Html$Events$onClick(
							model.open(person.id))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('상세 · 수정')
						]))
				]));
	});
var $author$project$Page$People$viewWith = F3(
	function (mode, model, w) {
		var selected = $elm$core$List$head(
			A2(
				$elm$core$List$filter,
				function (p) {
					return _Utils_eq(
						$elm$core$Maybe$Just(p.id),
						model.selected);
				},
				w.people));
		var people = A2(
			$elm$core$List$filter,
			A2($author$project$Page$People$matches, model.query, model.status),
			w.people);
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$author$project$Ui$Common$panel,
					'구성원 관리',
					_List_fromArray(
						[
							$author$project$Ui$Common$note('현재 조직에서 실제로 일하는 구성원의 역할과 보고 관계를 기록합니다. 직함만 적기보다 무엇을 책임지는지 설명하세요. 비활성화한 구성원의 과거 기록은 보존됩니다.'),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('fields')
								]),
							_List_fromArray(
								[
									A6($author$project$Ui$Form$inputValue, 'people-search', model.query, model.search, '이름 · 역할 · 부서 · 이메일 검색', 'search', false),
									A6(
									$author$project$Ui$Form$selectValue,
									'people-status',
									model.status,
									model.filter,
									'재직 상태',
									true,
									_List_fromArray(
										[
											_Utils_Tuple2('active', '재직'),
											_Utils_Tuple2('inactive', '비활성'),
											_Utils_Tuple2('all', '전체')
										]))
								])),
							A2(
							$elm$html$Html$p,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(
									'검색 결과 ' + ($elm$core$String$fromInt(
										$elm$core$List$length(people)) + ('명 / 전체 ' + ($elm$core$String$fromInt(
										$elm$core$List$length(w.people)) + '명'))))
								])),
							$elm$core$List$isEmpty(people) ? A2($author$project$Ui$Common$emptyState, '표시할 구성원이 없습니다', '아래에서 구성원을 등록하거나 검색어와 재직 상태 필터를 변경하세요.') : (_Utils_eq(mode, $author$project$Ui$ListView$Table) ? A3($author$project$Page$People$peopleTable, model, w, people) : A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('grid')
								]),
							A2(
								$elm$core$List$map,
								A2($author$project$Page$People$personCard, model, w),
								people)))
						])),
					function () {
					if (selected.$ === 'Just') {
						var person = selected.a;
						return A3($author$project$Page$People$detail, model, w, person);
					} else {
						return $author$project$Ui$Common$note('목록에서 ‘상세 · 수정’을 눌러 구성원 정보와 담당 목표를 확인하세요.');
					}
				}(),
					A2(
					$elm$html$Html$section,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('panel'),
							$elm$html$Html$Attributes$id('new-person'),
							$elm$html$Html$Attributes$tabindex(-1)
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$h2,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('구성원 등록')
								])),
							$author$project$Ui$Common$note('예: 김민서 / 고객 문의 운영 책임 / 고객지원팀. 보고 대상이 아직 없다면 먼저 구성원을 등록한 뒤 연결하세요. 다음으로 업무 흐름에서 이 역할이 맡는 일을 기록합니다.'),
							A4(
							$author$project$Ui$Form$formView,
							model.forms,
							$author$project$Form$Action$AddPerson,
							'구성원 등록',
							A4($author$project$Page$People$profileFields, model.forms, w, $author$project$Form$Action$AddPerson, $elm$core$Maybe$Nothing))
						]))
				]));
	});
var $author$project$Page$Responsibility$ownerForm = F3(
	function (model, w, g) {
		return A4(
			$author$project$Ui$Form$formView,
			model.forms,
			$author$project$Form$Action$Assign(g.goal.id),
			'책임자 지정',
			_List_fromArray(
				[
					A6(
					$author$project$Ui$Form$selectField,
					model.forms,
					$author$project$Form$Action$Assign(g.goal.id),
					'책임자',
					'owner',
					true,
					$author$project$Ui$Form$peopleOptions(w))
				]));
	});
var $author$project$Page$Responsibility$requirements = function (g) {
	return _List_fromArray(
		[
			A2(
			$elm$html$Html$p,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text(
					A2(
						$elm$core$String$join,
						' · ',
						A2($elm$core$List$map, $author$project$Ui$Label$permissionName, g.goal.requiredPermissions)))
				])),
			$author$project$Ui$Common$note(
			'예산 ' + ($author$project$Ui$Label$formatNumber(g.goal.requiredBudget) + ('원 · ' + ($elm$core$String$fromInt(
				$elm$core$Basics$round(g.analysis.coverage * 100)) + '% 통제'))))
		]);
};
var $author$project$Page$Responsibility$responsibilityCard = F3(
	function (model, w, g) {
		return A2(
			$elm$html$Html$article,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('goal-card'),
					$elm$html$Html$Attributes$id('owner-' + g.goal.id),
					$elm$html$Html$Attributes$tabindex(-1)
				]),
			_Utils_ap(
				_List_fromArray(
					[
						$author$project$Ui$Common$badge(g),
						A2(
						$elm$html$Html$h2,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(g.goal.description)
							])),
						A2(
						$elm$html$Html$small,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(g.goal.metric.name)
							])),
						A2(
						$elm$html$Html$p,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(
								'목표값 ' + ($author$project$Ui$Label$formatNumber(g.goal.target) + (' ' + g.goal.metric.unit)))
							])),
						$author$project$Ui$Common$note(
						'최종 책임자: ' + A2(
							$elm$core$Maybe$withDefault,
							'책임자 미지정',
							A2(
								$elm$core$Maybe$map,
								$author$project$Ui$Label$personName(w),
								g.owner))),
						A3($author$project$Page$Responsibility$ownerForm, model, w, g)
					]),
				$author$project$Page$Responsibility$requirements(g)));
	});
var $author$project$Page$Responsibility$responsibilityRow = F3(
	function (model, w, g) {
		return A2(
			$elm$html$Html$tr,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$id('owner-' + g.goal.id),
					$elm$html$Html$Attributes$tabindex(-1)
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$th,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$scope('row')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$strong,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(g.goal.description)
								])),
							A2(
							$elm$html$Html$small,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(g.goal.metric.name)
								]))
						])),
					A2(
					$elm$html$Html$td,
					_List_Nil,
					_List_fromArray(
						[
							$author$project$Ui$Common$note(
							A2(
								$elm$core$Maybe$withDefault,
								'책임자 미지정',
								A2(
									$elm$core$Maybe$map,
									$author$project$Ui$Label$personName(w),
									g.owner))),
							A3($author$project$Page$Responsibility$ownerForm, model, w, g)
						])),
					A2(
					$elm$html$Html$td,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							$author$project$Ui$Label$formatNumber(g.goal.target) + (' ' + g.goal.metric.unit))
						])),
					A2(
					$elm$html$Html$td,
					_List_Nil,
					$author$project$Page$Responsibility$requirements(g)),
					A2(
					$elm$html$Html$td,
					_List_Nil,
					_List_fromArray(
						[
							$author$project$Ui$Common$badge(g)
						]))
				]));
	});
var $elm$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		if (ma.$ === 'Nothing') {
			return $elm$core$Maybe$Nothing;
		} else {
			var a = ma.a;
			if (mb.$ === 'Nothing') {
				return $elm$core$Maybe$Nothing;
			} else {
				var b = mb.a;
				return $elm$core$Maybe$Just(
					A2(func, a, b));
			}
		}
	});
var $author$project$Ui$ResponsibilityGraph$nodeKey = function (node) {
	return node.tag + (':' + node.contents);
};
var $author$project$Ui$ResponsibilityGraph$pair = F2(
	function (x, y) {
		return $elm$core$String$fromFloat(x) + (' ' + $elm$core$String$fromFloat(y));
	});
var $author$project$Ui$ResponsibilityGraph$relationLabel = function (kind) {
	switch (kind) {
		case 'Owns':
			return '책임';
		case 'DependsOn':
			return '하위 목표';
		case 'Measures':
			return '측정 지표';
		case 'Controls':
			return '보유 권한';
		default:
			return kind;
	}
};
var $author$project$Ui$ResponsibilityGraph$svg = $elm$virtual_dom$VirtualDom$nodeNS('http://www.w3.org/2000/svg');
var $author$project$Ui$ResponsibilityGraph$edgeView = F3(
	function (state, positions, edge) {
		var find = function (node) {
			return $elm$core$List$head(
				A2(
					$elm$core$List$filter,
					A2(
						$elm$core$Basics$composeR,
						function ($) {
							return $.node;
						},
						A2(
							$elm$core$Basics$composeR,
							$author$project$Ui$ResponsibilityGraph$nodeKey,
							$elm$core$Basics$eq(
								$author$project$Ui$ResponsibilityGraph$nodeKey(node)))),
					positions));
		};
		var active = A2(
			$elm$core$Maybe$withDefault,
			true,
			A2(
				$elm$core$Maybe$map,
				function (key) {
					return _Utils_eq(
						$author$project$Ui$ResponsibilityGraph$nodeKey(edge.from),
						key) || _Utils_eq(
						$author$project$Ui$ResponsibilityGraph$nodeKey(edge.to),
						key);
				},
				state.selected));
		return A3(
			$elm$core$Maybe$map2,
			F2(
				function (from, to) {
					var y2 = to.y + 42;
					var y1 = from.y + 42;
					var x2 = to.x;
					var x1 = from.x + 260;
					var d = (edge.kind === 'DependsOn') ? ('M ' + (A2($author$project$Ui$ResponsibilityGraph$pair, x1, y1) + (' C ' + (A2($author$project$Ui$ResponsibilityGraph$pair, x1 + 55, y1) + (' ' + (A2($author$project$Ui$ResponsibilityGraph$pair, x1 + 55, y2) + (' ' + A2($author$project$Ui$ResponsibilityGraph$pair, x1 + 3, y2)))))))) : ((edge.kind === 'Controls') ? ('M ' + (A2($author$project$Ui$ResponsibilityGraph$pair, x1, y1 + 25) + (' C ' + (A2($author$project$Ui$ResponsibilityGraph$pair, x1 + 45, y1 + 65) + (' ' + (A2($author$project$Ui$ResponsibilityGraph$pair, x2 - 45, y2 + 65) + (' ' + A2($author$project$Ui$ResponsibilityGraph$pair, x2, y2 + 25)))))))) : ('M ' + (A2($author$project$Ui$ResponsibilityGraph$pair, x1, y1) + (' C ' + (A2($author$project$Ui$ResponsibilityGraph$pair, x1 + 40, y1) + (' ' + (A2($author$project$Ui$ResponsibilityGraph$pair, x2 - 40, y2) + (' ' + A2($author$project$Ui$ResponsibilityGraph$pair, x2, y2)))))))));
					return A3(
						$author$project$Ui$ResponsibilityGraph$svg,
						'g',
						_List_fromArray(
							[
								A2(
								$elm$html$Html$Attributes$attribute,
								'opacity',
								active ? '1' : '0.16')
							]),
						_List_fromArray(
							[
								A3(
								$author$project$Ui$ResponsibilityGraph$svg,
								'title',
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										$author$project$Ui$ResponsibilityGraph$relationLabel(edge.kind))
									])),
								A3(
								$author$project$Ui$ResponsibilityGraph$svg,
								'path',
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$attribute, 'd', d),
										A2($elm$html$Html$Attributes$attribute, 'fill', 'none'),
										A2($elm$html$Html$Attributes$attribute, 'stroke', '#557467'),
										A2(
										$elm$html$Html$Attributes$attribute,
										'stroke-width',
										((!_Utils_eq(state.selected, $elm$core$Maybe$Nothing)) && active) ? '3' : '1.5'),
										A2(
										$elm$html$Html$Attributes$attribute,
										'stroke-dasharray',
										((edge.kind === 'DependsOn') || (edge.kind === 'Controls')) ? '6 4' : 'none'),
										A2($elm$html$Html$Attributes$attribute, 'marker-end', 'url(#responsibility-arrow)')
									]),
								_List_Nil)
							]));
				}),
			find(edge.from),
			find(edge.to));
	});
var $author$project$Ui$ResponsibilityGraph$Select = function (a) {
	return {$: 'Select', a: a};
};
var $author$project$Ui$ResponsibilityGraph$bool = function (value) {
	return value ? 'true' : 'false';
};
var $author$project$Ui$ResponsibilityGraph$goalWarning = F2(
	function (w, node) {
		return (node.tag !== 'GoalNode') ? '' : A2(
			$elm$core$Maybe$withDefault,
			'',
			A2(
				$elm$core$Maybe$map,
				function (g) {
					return _Utils_eq(g.owner, $elm$core$Maybe$Nothing) ? '책임자 미지정' : ((g.analysis.coverage < 1) ? '권한 부족' : '');
				},
				$elm$core$List$head(
					A2(
						$elm$core$List$filter,
						A2(
							$elm$core$Basics$composeR,
							function ($) {
								return $.goal;
							},
							A2(
								$elm$core$Basics$composeR,
								function ($) {
									return $.id;
								},
								$elm$core$Basics$eq(node.contents))),
						w.goals))));
	});
var $author$project$Ui$ResponsibilityGraph$nodeLabel = F2(
	function (w, node) {
		var _v0 = node.tag;
		switch (_v0) {
			case 'PersonNode':
				return A2($author$project$Ui$Label$personName, w, node.contents);
			case 'GoalNode':
				return A2($author$project$Ui$Label$goalName, w, node.contents);
			case 'MetricNode':
				return A2(
					$elm$core$Maybe$withDefault,
					node.contents,
					A2(
						$elm$core$Maybe$map,
						A2(
							$elm$core$Basics$composeR,
							function ($) {
								return $.goal;
							},
							A2(
								$elm$core$Basics$composeR,
								function ($) {
									return $.metric;
								},
								function ($) {
									return $.name;
								})),
						$elm$core$List$head(
							A2(
								$elm$core$List$filter,
								A2(
									$elm$core$Basics$composeR,
									function ($) {
										return $.goal;
									},
									A2(
										$elm$core$Basics$composeR,
										function ($) {
											return $.metric;
										},
										A2(
											$elm$core$Basics$composeR,
											function ($) {
												return $.id;
											},
											$elm$core$Basics$eq(node.contents)))),
								w.goals))));
			case 'ResourceNode':
				return (node.contents === 'Budget') ? '예산' : $author$project$Ui$Label$permissionName(node.contents);
			default:
				return node.contents;
		}
	});
var $author$project$Ui$ResponsibilityGraph$matches = F3(
	function (query, w, node) {
		return A2(
			$elm$core$String$contains,
			$elm$core$String$toLower(
				$elm$core$String$trim(query)),
			$elm$core$String$toLower(
				A2($author$project$Ui$ResponsibilityGraph$nodeLabel, w, node) + (' ' + node.contents)));
	});
var $author$project$Ui$ResponsibilityGraph$nodeType = function (node) {
	var _v0 = node.tag;
	switch (_v0) {
		case 'PersonNode':
			return '사람';
		case 'GoalNode':
			return '목표';
		case 'MetricNode':
			return '측정 지표';
		default:
			return '권한 · 예산';
	}
};
var $author$project$Ui$ResponsibilityGraph$related = F3(
	function (edges, selected, key) {
		return _Utils_eq(selected, key) || A2(
			$elm$core$List$any,
			function (edge) {
				return (_Utils_eq(
					$author$project$Ui$ResponsibilityGraph$nodeKey(edge.from),
					selected) && _Utils_eq(
					$author$project$Ui$ResponsibilityGraph$nodeKey(edge.to),
					key)) || (_Utils_eq(
					$author$project$Ui$ResponsibilityGraph$nodeKey(edge.to),
					selected) && _Utils_eq(
					$author$project$Ui$ResponsibilityGraph$nodeKey(edge.from),
					key));
			},
			edges);
	});
var $author$project$Ui$ResponsibilityGraph$wrapped = function (value) {
	return ($elm$core$String$length(value) <= 17) ? _List_fromArray(
		[value]) : _List_fromArray(
		[
			A2($elm$core$String$left, 17, value),
			_Utils_ap(
			A3($elm$core$String$slice, 17, 33, value),
			($elm$core$String$length(value) > 33) ? '…' : '')
		]);
};
var $author$project$Ui$ResponsibilityGraph$nodeView = F5(
	function (state, dispatch, w, edges, pos) {
		var warning = A2($author$project$Ui$ResponsibilityGraph$goalWarning, w, pos.node);
		var title = A2($author$project$Ui$ResponsibilityGraph$nodeLabel, w, pos.node);
		var key = $author$project$Ui$ResponsibilityGraph$nodeKey(pos.node);
		var selected = _Utils_eq(
			state.selected,
			$elm$core$Maybe$Just(key));
		var illuminated = A2(
			$elm$core$Maybe$withDefault,
			true,
			A2(
				$elm$core$Maybe$map,
				function (chosen) {
					return A3($author$project$Ui$ResponsibilityGraph$related, edges, chosen, key);
				},
				state.selected));
		var found = A3($author$project$Ui$ResponsibilityGraph$matches, state.query, w, pos.node);
		var events = A2(
			$elm$core$Maybe$withDefault,
			_List_Nil,
			A2(
				$elm$core$Maybe$map,
				function (send) {
					return _List_fromArray(
						[
							$elm$html$Html$Events$onClick(
							send(
								$author$project$Ui$ResponsibilityGraph$Select(key))),
							A2(
							$elm$html$Html$Events$preventDefaultOn,
							'keydown',
							A2(
								$elm$json$Json$Decode$andThen,
								function (pressed) {
									return ((pressed === 'Enter') || (pressed === ' ')) ? $elm$json$Json$Decode$succeed(
										_Utils_Tuple2(
											send(
												$author$project$Ui$ResponsibilityGraph$Select(key)),
											true)) : $elm$json$Json$Decode$fail('not an activation key');
								},
								A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string)))
						]);
				},
				dispatch));
		var color = (warning !== '') ? '#fff2dc' : ((pos.node.tag === 'PersonNode') ? '#eaf3ec' : ((pos.node.tag === 'MetricNode') ? '#eaf1fa' : ((pos.node.tag === 'ResourceNode') ? '#f1edf8' : '#fff')));
		return A3(
			$author$project$Ui$ResponsibilityGraph$svg,
			'g',
			_Utils_ap(
				_List_fromArray(
					[
						A2(
						$elm$html$Html$Attributes$attribute,
						'transform',
						'translate(' + (A2($author$project$Ui$ResponsibilityGraph$pair, pos.x, pos.y) + ')')),
						A2($elm$html$Html$Attributes$attribute, 'class', 'graph-node'),
						A2($elm$html$Html$Attributes$attribute, 'role', 'button'),
						A2($elm$html$Html$Attributes$attribute, 'tabindex', '0'),
						A2(
						$elm$html$Html$Attributes$attribute,
						'aria-label',
						_Utils_ap(
							title,
							(warning === '') ? '' : (' · ' + warning))),
						A2(
						$elm$html$Html$Attributes$attribute,
						'aria-pressed',
						$author$project$Ui$ResponsibilityGraph$bool(selected)),
						A2(
						$elm$html$Html$Attributes$attribute,
						'opacity',
						(illuminated && found) ? '1' : '0.3')
					]),
				events),
			_List_fromArray(
				[
					A3(
					$author$project$Ui$ResponsibilityGraph$svg,
					'title',
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(title + (' · ' + warning))
						])),
					A3(
					$author$project$Ui$ResponsibilityGraph$svg,
					'rect',
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$attribute, 'width', '260'),
							A2($elm$html$Html$Attributes$attribute, 'height', '84'),
							A2($elm$html$Html$Attributes$attribute, 'rx', '10'),
							A2($elm$html$Html$Attributes$attribute, 'fill', color),
							A2(
							$elm$html$Html$Attributes$attribute,
							'stroke',
							selected ? '#1c6147' : ((found && ($elm$core$String$trim(state.query) !== '')) ? '#3479b3' : '#b8cbbd')),
							A2(
							$elm$html$Html$Attributes$attribute,
							'stroke-width',
							(selected || (($elm$core$String$trim(state.query) !== '') && found)) ? '3' : '1.5')
						]),
					_List_Nil),
					A3(
					$author$project$Ui$ResponsibilityGraph$svg,
					'text',
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$attribute, 'x', '14'),
							A2($elm$html$Html$Attributes$attribute, 'y', '26'),
							A2($elm$html$Html$Attributes$attribute, 'class', 'graph-node-label')
						]),
					A2(
						$elm$core$List$indexedMap,
						F2(
							function (i, line) {
								return A3(
									$author$project$Ui$ResponsibilityGraph$svg,
									'tspan',
									_List_fromArray(
										[
											A2($elm$html$Html$Attributes$attribute, 'x', '14'),
											A2(
											$elm$html$Html$Attributes$attribute,
											'dy',
											(!i) ? '0' : '19')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(line)
										]));
							}),
						$author$project$Ui$ResponsibilityGraph$wrapped(title))),
					A3(
					$author$project$Ui$ResponsibilityGraph$svg,
					'text',
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$attribute, 'x', '14'),
							A2($elm$html$Html$Attributes$attribute, 'y', '71'),
							A2($elm$html$Html$Attributes$attribute, 'class', 'graph-node-meta')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							(warning !== '') ? ('⚠ ' + warning) : $author$project$Ui$ResponsibilityGraph$nodeType(pos.node))
						]))
				]));
	});
var $author$project$Ui$ResponsibilityGraph$diagram = F5(
	function (state, dispatch, w, positions, edges) {
		var width = state.showResources ? 1360 : 1020;
		var height = A2(
			$elm$core$Maybe$withDefault,
			180,
			$elm$core$List$maximum(
				A2(
					$elm$core$List$map,
					function (pos) {
						return pos.y + 110;
					},
					positions)));
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$p,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('note')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('노드를 선택하면 연결 관계를 강조합니다. Tab과 Enter/Space로도 선택할 수 있습니다. 확대 후 스크롤로 이동하세요.')
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('graph-viewport'),
							$elm$html$Html$Attributes$tabindex(0),
							A2($elm$html$Html$Attributes$attribute, 'role', 'region'),
							A2($elm$html$Html$Attributes$attribute, 'aria-label', '책임 관계 다이어그램 · 스크롤 탐색')
						]),
					_List_fromArray(
						[
							A3(
							$author$project$Ui$ResponsibilityGraph$svg,
							'svg',
							_List_fromArray(
								[
									A2(
									$elm$html$Html$Attributes$attribute,
									'viewBox',
									'0 0 ' + ($elm$core$String$fromFloat(width) + (' ' + $elm$core$String$fromFloat(height)))),
									A2(
									$elm$html$Html$Attributes$attribute,
									'width',
									$elm$core$String$fromFloat(state.zoom * 100) + '%'),
									A2(
									$elm$html$Html$Attributes$attribute,
									'class',
									(state.zoom === 1) ? 'graph-svg graph-fit' : 'graph-svg'),
									A2($elm$html$Html$Attributes$attribute, 'preserveAspectRatio', 'xMidYMin meet'),
									A2($elm$html$Html$Attributes$attribute, 'role', 'group'),
									A2($elm$html$Html$Attributes$attribute, 'aria-label', '사람, 목표, 지표와 자원 관계')
								]),
							_Utils_ap(
								_List_fromArray(
									[
										A3(
										$author$project$Ui$ResponsibilityGraph$svg,
										'defs',
										_List_Nil,
										_List_fromArray(
											[
												A3(
												$author$project$Ui$ResponsibilityGraph$svg,
												'marker',
												_List_fromArray(
													[
														A2($elm$html$Html$Attributes$attribute, 'id', 'responsibility-arrow'),
														A2($elm$html$Html$Attributes$attribute, 'viewBox', '0 0 10 10'),
														A2($elm$html$Html$Attributes$attribute, 'refX', '9'),
														A2($elm$html$Html$Attributes$attribute, 'refY', '5'),
														A2($elm$html$Html$Attributes$attribute, 'markerWidth', '7'),
														A2($elm$html$Html$Attributes$attribute, 'markerHeight', '7'),
														A2($elm$html$Html$Attributes$attribute, 'orient', 'auto-start-reverse')
													]),
												_List_fromArray(
													[
														A3(
														$author$project$Ui$ResponsibilityGraph$svg,
														'path',
														_List_fromArray(
															[
																A2($elm$html$Html$Attributes$attribute, 'd', 'M 0 0 L 10 5 L 0 10 z'),
																A2($elm$html$Html$Attributes$attribute, 'fill', '#557467')
															]),
														_List_Nil)
													]))
											]))
									]),
								_Utils_ap(
									A2(
										$elm$core$List$indexedMap,
										F2(
											function (i, name) {
												return A3(
													$author$project$Ui$ResponsibilityGraph$svg,
													'text',
													_List_fromArray(
														[
															A2(
															$elm$html$Html$Attributes$attribute,
															'x',
															$elm$core$String$fromInt(24 + (i * 340))),
															A2($elm$html$Html$Attributes$attribute, 'y', '29'),
															A2($elm$html$Html$Attributes$attribute, 'class', 'graph-column')
														]),
													_List_fromArray(
														[
															$elm$html$Html$text(name)
														]));
											}),
										state.showResources ? _List_fromArray(
											['사람', '목표', '지표', '권한 · 예산']) : _List_fromArray(
											['사람', '목표', '지표'])),
									_Utils_ap(
										A2(
											$elm$core$List$filterMap,
											A2($author$project$Ui$ResponsibilityGraph$edgeView, state, positions),
											edges),
										A2(
											$elm$core$List$map,
											A4($author$project$Ui$ResponsibilityGraph$nodeView, state, dispatch, w, edges),
											positions)))))
						]))
				]));
	});
var $author$project$Ui$ResponsibilityGraph$nodes = function (w) {
	return $elm$core$Dict$values(
		$elm$core$Dict$fromList(
			A2(
				$elm$core$List$map,
				function (node) {
					return _Utils_Tuple2(
						$author$project$Ui$ResponsibilityGraph$nodeKey(node),
						node);
				},
				_Utils_ap(
					A2(
						$elm$core$List$map,
						function (person) {
							return A2($author$project$Domain$Node, 'PersonNode', person.id);
						},
						w.people),
					_Utils_ap(
						A2(
							$elm$core$List$map,
							function (g) {
								return A2($author$project$Domain$Node, 'GoalNode', g.goal.id);
							},
							w.goals),
						A2(
							$elm$core$List$concatMap,
							function (edge) {
								return _List_fromArray(
									[edge.from, edge.to]);
							},
							w.edges))))));
};
var $author$project$Ui$ResponsibilityGraph$layout = F2(
	function (state, w) {
		return $elm$core$List$concat(
			A2(
				$elm$core$List$indexedMap,
				F2(
					function (column, tag) {
						return A2(
							$elm$core$List$indexedMap,
							F2(
								function (row, node) {
									return {node: node, x: 24 + (column * 340), y: 56 + (row * 118)};
								}),
							A2(
								$elm$core$List$sortBy,
								$author$project$Ui$ResponsibilityGraph$nodeLabel(w),
								A2(
									$elm$core$List$filter,
									function (node) {
										return _Utils_eq(node.tag, tag) && ((tag !== 'ResourceNode') || state.showResources);
									},
									$author$project$Ui$ResponsibilityGraph$nodes(w))));
					}),
				_List_fromArray(
					['PersonNode', 'GoalNode', 'MetricNode', 'ResourceNode'])));
	});
var $author$project$Ui$ResponsibilityGraph$relationList = F5(
	function (state, dispatch, w, positions, edges) {
		var shown = A2(
			$elm$core$List$filter,
			function (edge) {
				return A3($author$project$Ui$ResponsibilityGraph$matches, state.query, w, edge.from) || A3($author$project$Ui$ResponsibilityGraph$matches, state.query, w, edge.to);
			},
			edges);
		var pick = function (node) {
			return A2(
				$elm$html$Html$button,
				_Utils_ap(
					_List_fromArray(
						[
							$elm$html$Html$Attributes$type_('button'),
							$elm$html$Html$Attributes$class('secondary')
						]),
					A2(
						$elm$core$Maybe$withDefault,
						_List_Nil,
						A2(
							$elm$core$Maybe$map,
							function (send) {
								return _List_fromArray(
									[
										$elm$html$Html$Events$onClick(
										send(
											$author$project$Ui$ResponsibilityGraph$Select(
												$author$project$Ui$ResponsibilityGraph$nodeKey(node))))
									]);
							},
							dispatch))),
				_List_fromArray(
					[
						$elm$html$Html$text(
						A2($author$project$Ui$ResponsibilityGraph$nodeLabel, w, node))
					]));
		};
		var isolated = A2(
			$elm$core$List$filter,
			function (pos) {
				return (!A2(
					$elm$core$List$any,
					function (edge) {
						return _Utils_eq(
							$author$project$Ui$ResponsibilityGraph$nodeKey(edge.from),
							$author$project$Ui$ResponsibilityGraph$nodeKey(pos.node)) || _Utils_eq(
							$author$project$Ui$ResponsibilityGraph$nodeKey(edge.to),
							$author$project$Ui$ResponsibilityGraph$nodeKey(pos.node));
					},
					edges)) && A3($author$project$Ui$ResponsibilityGraph$matches, state.query, w, pos.node);
			},
			positions);
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('graph-relations')
				]),
			_Utils_ap(
				A2(
					$elm$core$List$map,
					function (edge) {
						return A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('graph-edge')
								]),
							_List_fromArray(
								[
									pick(edge.from),
									A2(
									$elm$html$Html$span,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(
											'─ ' + ($author$project$Ui$ResponsibilityGraph$relationLabel(edge.kind) + ' →'))
										])),
									pick(edge.to)
								]));
					},
					shown),
				_Utils_ap(
					A2(
						$elm$core$List$map,
						function (pos) {
							return A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('graph-edge')
									]),
								_List_fromArray(
									[
										pick(pos.node),
										A2(
										$elm$html$Html$span,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('표시 중인 연결 없음')
											]))
									]));
						},
						isolated),
					($elm$core$List$isEmpty(shown) && $elm$core$List$isEmpty(isolated)) ? _List_fromArray(
						[
							A2(
							$elm$html$Html$p,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('검색 조건에 맞는 관계가 없습니다.')
								]))
						]) : _List_Nil)));
	});
var $author$project$Ui$ResponsibilityGraph$ClearSelection = {$: 'ClearSelection'};
var $author$project$Ui$ResponsibilityGraph$personDetails = F3(
	function (go, w, node) {
		var person = $elm$core$List$head(
			A2(
				$elm$core$List$filter,
				A2(
					$elm$core$Basics$composeR,
					function ($) {
						return $.id;
					},
					$elm$core$Basics$eq(node.contents)),
				w.people));
		var owned = A2(
			$elm$core$List$filter,
			A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.owner;
				},
				$elm$core$Basics$eq(
					$elm$core$Maybe$Just(node.contents))),
			w.goals);
		var navigate = F2(
			function (target, title) {
				return A2(
					$elm$html$Html$button,
					_Utils_ap(
						_List_fromArray(
							[
								$elm$html$Html$Attributes$type_('button'),
								$elm$html$Html$Attributes$disabled(
								_Utils_eq(go, $elm$core$Maybe$Nothing))
							]),
						A2(
							$elm$core$Maybe$withDefault,
							_List_Nil,
							A2(
								$elm$core$Maybe$map,
								function (send) {
									return _List_fromArray(
										[
											$elm$html$Html$Events$onClick(
											send(target))
										]);
								},
								go))),
					_List_fromArray(
						[
							$elm$html$Html$text(title)
						]));
			});
		return _Utils_ap(
			A2(
				$elm$core$Maybe$withDefault,
				_List_Nil,
				A2(
					$elm$core$Maybe$map,
					function (p) {
						return _List_fromArray(
							[
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										'역할: ' + (p.role + (' · ' + (p.active ? '재직' : '비활성'))))
									]))
							]);
					},
					person)),
			_Utils_ap(
				_List_fromArray(
					[
						A2(
						$elm$html$Html$p,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(
								'담당 목표 ' + ($elm$core$String$fromInt(
									$elm$core$List$length(owned)) + '개'))
							]))
					]),
				_Utils_ap(
					A2(
						$elm$core$Maybe$withDefault,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('부여된 권한 정보 없음')
									]))
							]),
						A2(
							$elm$core$Maybe$map,
							function (authority) {
								return _List_fromArray(
									[
										A2(
										$elm$html$Html$p,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text(
												'보유 예산 ' + ($author$project$Ui$Label$formatNumber(authority.budgetLimit) + '원'))
											]))
									]);
							},
							$elm$core$List$head(
								A2(
									$elm$core$List$filter,
									A2(
										$elm$core$Basics$composeR,
										function ($) {
											return $.owner;
										},
										$elm$core$Basics$eq(node.contents)),
									w.authorities)))),
					_Utils_ap(
						_List_fromArray(
							[
								A2(navigate, 'person:' + node.contents, '구성원 상세로 이동')
							]),
						A2(
							$elm$core$Maybe$withDefault,
							false,
							A2(
								$elm$core$Maybe$map,
								function ($) {
									return $.active;
								},
								person)) ? _List_fromArray(
							[
								A2(navigate, 'authority-' + node.contents, '권한 관리로 이동')
							]) : _List_Nil))));
	});
var $author$project$Ui$ResponsibilityGraph$selectionDetails = F4(
	function (state, dispatch, go, w) {
		var _v0 = A2(
			$elm$core$Maybe$andThen,
			function (key) {
				return $elm$core$List$head(
					A2(
						$elm$core$List$filter,
						A2(
							$elm$core$Basics$composeR,
							$author$project$Ui$ResponsibilityGraph$nodeKey,
							$elm$core$Basics$eq(key)),
						$author$project$Ui$ResponsibilityGraph$nodes(w)));
			},
			state.selected);
		if (_v0.$ === 'Nothing') {
			return A2(
				$elm$html$Html$p,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('note')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('노드를 선택하면 전체 이름, 상태와 연결된 관계를 확인할 수 있습니다.')
					]));
		} else {
			var node = _v0.a;
			var goal = $elm$core$List$head(
				A2(
					$elm$core$List$filter,
					A2(
						$elm$core$Basics$composeR,
						function ($) {
							return $.goal;
						},
						A2(
							$elm$core$Basics$composeR,
							function ($) {
								return $.id;
							},
							$elm$core$Basics$eq(node.contents))),
					w.goals));
			var edges = A2(
				$elm$core$List$filter,
				function (edge) {
					return _Utils_eq(edge.from, node) || _Utils_eq(edge.to, node);
				},
				w.edges);
			var details = (node.tag === 'GoalNode') ? A2(
				$elm$core$Maybe$withDefault,
				_List_Nil,
				A2(
					$elm$core$Maybe$map,
					function (g) {
						return _List_fromArray(
							[
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										'책임자: ' + A2(
											$elm$core$Maybe$withDefault,
											'미지정',
											A2(
												$elm$core$Maybe$map,
												$author$project$Ui$Label$personName(w),
												g.owner)))
									])),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										(g.active ? '활성' : '초안') + (' · ' + ($author$project$Ui$Label$statusName(g.evaluation.status) + (' · 권한 통제율 ' + ($elm$core$String$fromInt(
											$elm$core$Basics$round(g.analysis.coverage * 100)) + '%')))))
									])),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										'필요 권한: ' + (A2(
											$elm$core$String$join,
											', ',
											A2($elm$core$List$map, $author$project$Ui$Label$permissionName, g.goal.requiredPermissions)) + (' · 필요 예산 ' + ($author$project$Ui$Label$formatNumber(g.goal.requiredBudget) + '원'))))
									])),
								A2(
								$elm$html$Html$button,
								_Utils_ap(
									_List_fromArray(
										[
											$elm$html$Html$Attributes$type_('button'),
											$elm$html$Html$Attributes$disabled(
											_Utils_eq(go, $elm$core$Maybe$Nothing))
										]),
									A2(
										$elm$core$Maybe$withDefault,
										_List_Nil,
										A2(
											$elm$core$Maybe$map,
											function (navigate) {
												return _List_fromArray(
													[
														$elm$html$Html$Events$onClick(
														navigate('owner-' + node.contents))
													]);
											},
											go))),
								_List_fromArray(
									[
										$elm$html$Html$text('책임자 지정 폼으로 이동')
									]))
							]);
					},
					goal)) : ((node.tag === 'PersonNode') ? A3($author$project$Ui$ResponsibilityGraph$personDetails, go, w, node) : _List_Nil);
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('graph-detail'),
						A2($elm$html$Html$Attributes$attribute, 'aria-live', 'polite')
					]),
				_Utils_ap(
					_List_fromArray(
						[
							A2(
							$elm$html$Html$h3,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(
									A2($author$project$Ui$ResponsibilityGraph$nodeLabel, w, node))
								])),
							A2(
							$elm$html$Html$p,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('note')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(
									$author$project$Ui$ResponsibilityGraph$nodeType(node) + (' · 전체 연결 ' + ($elm$core$String$fromInt(
										$elm$core$List$length(edges)) + '개 (숨긴 관계 포함)')))
								]))
						]),
					_Utils_ap(
						details,
						_Utils_ap(
							A2(
								$elm$core$List$map,
								function (edge) {
									return A2(
										$elm$html$Html$p,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text(
												A2($author$project$Ui$ResponsibilityGraph$nodeLabel, w, edge.from) + (' ─ ' + ($author$project$Ui$ResponsibilityGraph$relationLabel(edge.kind) + (' → ' + A2($author$project$Ui$ResponsibilityGraph$nodeLabel, w, edge.to)))))
											]));
								},
								edges),
							A2(
								$elm$core$Maybe$withDefault,
								_List_Nil,
								A2(
									$elm$core$Maybe$map,
									function (send) {
										return _List_fromArray(
											[
												A2(
												$elm$html$Html$button,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$type_('button'),
														$elm$html$Html$Attributes$class('secondary'),
														$elm$html$Html$Events$onClick(
														send($author$project$Ui$ResponsibilityGraph$ClearSelection))
													]),
												_List_fromArray(
													[
														$elm$html$Html$text('선택 해제')
													]))
											]);
									},
									dispatch))))));
		}
	});
var $author$project$Ui$ResponsibilityGraph$Dependencies = function (a) {
	return {$: 'Dependencies', a: a};
};
var $author$project$Ui$ResponsibilityGraph$Fit = {$: 'Fit'};
var $author$project$Ui$ResponsibilityGraph$Resources = function (a) {
	return {$: 'Resources', a: a};
};
var $author$project$Ui$ResponsibilityGraph$Search = function (a) {
	return {$: 'Search', a: a};
};
var $author$project$Ui$ResponsibilityGraph$SetDiagram = function (a) {
	return {$: 'SetDiagram', a: a};
};
var $author$project$Ui$ResponsibilityGraph$Zoom = function (a) {
	return {$: 'Zoom', a: a};
};
var $elm$html$Html$Attributes$placeholder = $elm$html$Html$Attributes$stringProperty('placeholder');
var $author$project$Ui$ResponsibilityGraph$toolbar = F2(
	function (state, dispatch) {
		var check = F3(
			function (title, value, constructor) {
				return A2(
					$elm$html$Html$label,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$input,
							_Utils_ap(
								_List_fromArray(
									[
										$elm$html$Html$Attributes$type_('checkbox'),
										$elm$html$Html$Attributes$checked(value),
										$elm$html$Html$Attributes$disabled(
										_Utils_eq(dispatch, $elm$core$Maybe$Nothing))
									]),
								A2(
									$elm$core$Maybe$withDefault,
									_List_Nil,
									A2(
										$elm$core$Maybe$map,
										function (send) {
											return _List_fromArray(
												[
													$elm$html$Html$Events$onCheck(
													A2($elm$core$Basics$composeR, constructor, send))
												]);
										},
										dispatch))),
							_List_Nil),
							$elm$html$Html$text(title)
						]));
			});
		var action = function (message) {
			return A2(
				$elm$core$Maybe$withDefault,
				_List_Nil,
				A2(
					$elm$core$Maybe$map,
					function (send) {
						return _List_fromArray(
							[
								$elm$html$Html$Events$onClick(
								send(message))
							]);
					},
					dispatch));
		};
		var toggle = F3(
			function (title, active, message) {
				return A2(
					$elm$html$Html$button,
					_Utils_ap(
						_List_fromArray(
							[
								$elm$html$Html$Attributes$type_('button'),
								$elm$html$Html$Attributes$classList(
								_List_fromArray(
									[
										_Utils_Tuple2('secondary', !active)
									])),
								A2(
								$elm$html$Html$Attributes$attribute,
								'aria-pressed',
								$author$project$Ui$ResponsibilityGraph$bool(active)),
								$elm$html$Html$Attributes$disabled(
								_Utils_eq(dispatch, $elm$core$Maybe$Nothing))
							]),
						action(message)),
					_List_fromArray(
						[
							$elm$html$Html$text(title)
						]));
			});
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('graph-toolbar')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('graph-view-toggle'),
							A2($elm$html$Html$Attributes$attribute, 'role', 'group'),
							A2($elm$html$Html$Attributes$attribute, 'aria-label', '책임 관계 보기')
						]),
					_List_fromArray(
						[
							A3(
							toggle,
							'다이어그램',
							state.diagram,
							$author$project$Ui$ResponsibilityGraph$SetDiagram(true)),
							A3(
							toggle,
							'관계 목록',
							!state.diagram,
							$author$project$Ui$ResponsibilityGraph$SetDiagram(false))
						])),
					A2(
					$elm$html$Html$label,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('graph-search')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('관계 검색'),
							A2(
							$elm$html$Html$input,
							_Utils_ap(
								_List_fromArray(
									[
										$elm$html$Html$Attributes$type_('search'),
										$elm$html$Html$Attributes$value(state.query),
										$elm$html$Html$Attributes$placeholder('사람, 목표, 지표, 권한 검색'),
										$elm$html$Html$Attributes$disabled(
										_Utils_eq(dispatch, $elm$core$Maybe$Nothing))
									]),
								A2(
									$elm$core$Maybe$withDefault,
									_List_Nil,
									A2(
										$elm$core$Maybe$map,
										function (send) {
											return _List_fromArray(
												[
													$elm$html$Html$Events$onInput(
													A2($elm$core$Basics$composeR, $author$project$Ui$ResponsibilityGraph$Search, send))
												]);
										},
										dispatch))),
							_List_Nil)
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('graph-options')
						]),
					_List_fromArray(
						[
							A3(check, '목표 간 관계', state.showDependencies, $author$project$Ui$ResponsibilityGraph$Dependencies),
							A3(check, '권한·예산', state.showResources, $author$project$Ui$ResponsibilityGraph$Resources)
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('graph-zoom'),
							A2($elm$html$Html$Attributes$attribute, 'role', 'group'),
							A2($elm$html$Html$Attributes$attribute, 'aria-label', '다이어그램 확대')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$button,
							_Utils_ap(
								_List_fromArray(
									[
										$elm$html$Html$Attributes$type_('button'),
										$elm$html$Html$Attributes$class('secondary'),
										$elm$html$Html$Attributes$disabled(
										(state.zoom <= 1) || _Utils_eq(dispatch, $elm$core$Maybe$Nothing)),
										A2($elm$html$Html$Attributes$attribute, 'aria-label', '축소')
									]),
								action(
									$author$project$Ui$ResponsibilityGraph$Zoom(-0.25))),
							_List_fromArray(
								[
									$elm$html$Html$text('−')
								])),
							A2(
							$elm$html$Html$span,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(
									$elm$core$String$fromInt(
										$elm$core$Basics$round(state.zoom * 100)) + '%')
								])),
							A2(
							$elm$html$Html$button,
							_Utils_ap(
								_List_fromArray(
									[
										$elm$html$Html$Attributes$type_('button'),
										$elm$html$Html$Attributes$class('secondary'),
										$elm$html$Html$Attributes$disabled(
										(state.zoom >= 3) || _Utils_eq(dispatch, $elm$core$Maybe$Nothing)),
										A2($elm$html$Html$Attributes$attribute, 'aria-label', '확대')
									]),
								action(
									$author$project$Ui$ResponsibilityGraph$Zoom(0.25))),
							_List_fromArray(
								[
									$elm$html$Html$text('+')
								])),
							A2(
							$elm$html$Html$button,
							_Utils_ap(
								_List_fromArray(
									[
										$elm$html$Html$Attributes$type_('button'),
										$elm$html$Html$Attributes$class('secondary'),
										$elm$html$Html$Attributes$disabled(
										_Utils_eq(dispatch, $elm$core$Maybe$Nothing))
									]),
								action($author$project$Ui$ResponsibilityGraph$Fit)),
							_List_fromArray(
								[
									$elm$html$Html$text('전체 맞춤')
								]))
						]))
				]));
	});
var $author$project$Ui$ResponsibilityGraph$visibleEdges = function (state) {
	return $elm$core$List$filter(
		function (edge) {
			return ((edge.kind !== 'DependsOn') || state.showDependencies) && ((edge.kind !== 'Controls') || state.showResources);
		});
};
var $author$project$Ui$ResponsibilityGraph$view = F4(
	function (state, dispatch, go, w) {
		var positions = A2($author$project$Ui$ResponsibilityGraph$layout, state, w);
		var edges = A2($author$project$Ui$ResponsibilityGraph$visibleEdges, state, w.edges);
		var count = $elm$core$List$length(
			A2(
				$elm$core$List$filter,
				A2(
					$elm$core$Basics$composeR,
					function ($) {
						return $.node;
					},
					A2($author$project$Ui$ResponsibilityGraph$matches, state.query, w)),
				positions));
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('responsibility-network')
				]),
			_List_fromArray(
				[
					A2($author$project$Ui$ResponsibilityGraph$toolbar, state, dispatch),
					A2(
					$elm$html$Html$p,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('note graph-legend')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('사람 → 책임 → 목표 → 측정 지표 | 점선 곡선: 상위 → 하위 목표 · 긴 점선: 사람 → 보유 권한/예산 | ⚠ 책임자 미지정·권한 부족')
						])),
					A2(
					$elm$html$Html$p,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('note'),
							A2($elm$html$Html$Attributes$attribute, 'role', 'status')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							'노드 ' + ($elm$core$String$fromInt(
								$elm$core$List$length(positions)) + ('개 · 관계 ' + ($elm$core$String$fromInt(
								$elm$core$List$length(edges)) + ('개' + (($elm$core$String$trim(state.query) === '') ? '' : (' · 검색 일치 ' + ($elm$core$String$fromInt(count) + '개'))))))))
						])),
					((!count) && ($elm$core$String$trim(state.query) !== '')) ? A2(
					$elm$html$Html$p,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('note')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('검색 조건에 맞는 노드가 없습니다. 검색어를 변경하세요.')
						])) : $elm$html$Html$text(''),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$classList(
							_List_fromArray(
								[
									_Utils_Tuple2('graph-content', true),
									_Utils_Tuple2(
									'has-selection',
									!_Utils_eq(state.selected, $elm$core$Maybe$Nothing))
								]))
						]),
					_List_fromArray(
						[
							$elm$core$List$isEmpty(positions) ? A2(
							$elm$html$Html$p,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('아직 구성원과 목표가 없습니다. 구성원이나 목표를 추가하면 관계를 확인할 수 있습니다.')
								])) : (state.diagram ? A5($author$project$Ui$ResponsibilityGraph$diagram, state, dispatch, w, positions, edges) : A5($author$project$Ui$ResponsibilityGraph$relationList, state, dispatch, w, positions, edges)),
							A4($author$project$Ui$ResponsibilityGraph$selectionDetails, state, dispatch, go, w)
						]))
				]));
	});
var $author$project$Page$Responsibility$viewInteractive = F6(
	function (mode, graphState, graphMsg, go, model, w) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$author$project$Ui$Common$panel,
					'누가 어떤 결과를 책임지는가',
					_List_fromArray(
						[
							$author$project$Ui$Common$note('현재 목표에 대해 최종 설명과 판단을 맡는 한 사람을 기록하세요. 함께 일하는 모든 수행자를 뜻하지 않습니다. 책임자가 불명확하면 임의로 지정하지 말고 조직 진단에 미확인으로 남기세요.'),
							$elm$core$List$isEmpty(w.goals) ? A2($author$project$Ui$Common$emptyState, '아직 책임을 배정할 목표가 없습니다', '목표 메뉴에서 목표를 만든 뒤 책임자를 지정하세요.') : (_Utils_eq(mode, $author$project$Ui$ListView$Table) ? A3(
							$author$project$Ui$ListView$tableView,
							'목표별 책임',
							_List_fromArray(
								['결과 / KPI', '최종 책임자', '목표값', '필요 권한 / 통제율', '상태']),
							A2(
								$elm$core$List$map,
								A2($author$project$Page$Responsibility$responsibilityRow, model, w),
								w.goals)) : A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('grid')
								]),
							A2(
								$elm$core$List$map,
								A2($author$project$Page$Responsibility$responsibilityCard, model, w),
								w.goals)))
						])),
					A2(
					$elm$html$Html$section,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('panel'),
							$elm$html$Html$Attributes$id('responsibility-graph'),
							$elm$html$Html$Attributes$tabindex(-1)
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$h2,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('책임 관계 그래프')
								])),
							$author$project$Ui$Common$note('목표 중심 책임 관계입니다. 사람 → 목표 → 지표와 자원 통제를 연결하며 보고 계층 전체를 보여주는 조직도와는 범위가 다릅니다. 책임 공백과 권한 부족은 확인할 조직 현황입니다.'),
							A4($author$project$Ui$ResponsibilityGraph$view, graphState, graphMsg, go, w)
						])),
					$author$project$Ui$Common$diagnosticView(w)
				]));
	});
var $author$project$Page$Responsibility$viewWith = F3(
	function (mode, model, w) {
		return A6($author$project$Page$Responsibility$viewInteractive, mode, $author$project$Ui$ResponsibilityGraph$init, $elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing, model, w);
	});
var $author$project$Page$Results$resultContent = F3(
	function (model, w, g) {
		return _List_fromArray(
			[
				$author$project$Ui$Common$note('입력할 지표: ' + (g.goal.metric.name + (' / 단위: ' + (g.goal.metric.unit + '. 목표값이나 예상값 대신 실제 측정값을 입력하세요.')))),
				$author$project$Ui$Common$note(g.analysis.possibleCause),
				A2(
				$elm$html$Html$h3,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('form-heading')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('결과 보고')
					])),
				A4(
				$author$project$Ui$Form$formView,
				model.forms,
				$author$project$Form$Action$Report(g.goal.id),
				'결과 보고',
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('fields')
							]),
						_List_fromArray(
							[
								A6(
								$author$project$Ui$Form$inputField,
								model.forms,
								$author$project$Form$Action$Report(g.goal.id),
								'실측값',
								'value',
								'number',
								true),
								A6(
								$author$project$Ui$Form$selectField,
								model.forms,
								$author$project$Form$Action$Report(g.goal.id),
								'보고자',
								'reportedBy',
								true,
								$author$project$Ui$Form$peopleOptions(w))
							])),
						A6(
						$author$project$Ui$Form$inputField,
						model.forms,
						$author$project$Form$Action$Report(g.goal.id),
						'결과 설명',
						'note',
						'text',
						true)
					])),
				$elm$core$List$isEmpty(g.results) ? $author$project$Ui$Common$note('아직 결과가 없습니다.') : A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('table-wrap')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h3,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('form-heading')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('결과 추이 · 최근 순')
							])),
						A2(
						$elm$html$Html$table,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$thead,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$tr,
										_List_Nil,
										_List_fromArray(
											[
												A2(
												$elm$html$Html$th,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text('기록 시각')
													])),
												A2(
												$elm$html$Html$th,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text('측정값')
													])),
												A2(
												$elm$html$Html$th,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text('보고자')
													])),
												A2(
												$elm$html$Html$th,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text('설명')
													]))
											]))
									])),
								A2(
								$elm$html$Html$tbody,
								_List_Nil,
								A2(
									$elm$core$List$map,
									function (r) {
										return A2(
											$elm$html$Html$tr,
											_List_Nil,
											_List_fromArray(
												[
													A2(
													$elm$html$Html$td,
													_List_Nil,
													_List_fromArray(
														[
															$elm$html$Html$text(r.reportedAt)
														])),
													A2(
													$elm$html$Html$td,
													_List_Nil,
													_List_fromArray(
														[
															$elm$html$Html$text(
															$author$project$Ui$Label$formatNumber(r.value))
														])),
													A2(
													$elm$html$Html$td,
													_List_Nil,
													_List_fromArray(
														[
															$elm$html$Html$text(
															A2(
																$elm$core$Maybe$withDefault,
																'미기록',
																A2(
																	$elm$core$Maybe$map,
																	$author$project$Ui$Label$personName(w),
																	r.reportedBy)))
														])),
													A2(
													$elm$html$Html$td,
													_List_Nil,
													_List_fromArray(
														[
															$elm$html$Html$text(r.note)
														]))
												]));
									},
									g.results))
							]))
					])),
				$author$project$Ui$Common$note('평가 기록은 현재 실측값·목표값·기간을 기준으로 계산한 평가를 저장합니다. 이 기록을 바탕으로 학습 화면에서 다음 결정을 남기세요.'),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('actions')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('secondary'),
								$elm$html$Html$Attributes$disabled(model.forms.busy || (!model.forms.fresh)),
								$elm$html$Html$Events$onClick(
								model.forms.submit(
									$author$project$Form$Action$Evaluate(g.goal.id)))
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('평가 기록')
							])),
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('secondary'),
								$elm$html$Html$Attributes$disabled(model.forms.busy),
								$elm$html$Html$Events$onClick(
								model.goals('goal-' + g.goal.id))
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('목표 관리 →')
							]))
					]))
			]);
	});
var $author$project$Page$Results$goalTable = F2(
	function (model, w) {
		return A3(
			$author$project$Ui$ListView$tableView,
			'목표별 결과와 평가',
			_List_fromArray(
				['목표 / KPI', '최종 책임자', '현재값 / 목표값', '달성률', '마감', '상태']),
			A2(
				$elm$core$List$concatMap,
				function (g) {
					return _List_fromArray(
						[
							A2(
							$elm$html$Html$tr,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$id('goal-' + g.goal.id),
									$elm$html$Html$Attributes$tabindex(-1)
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$th,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$scope('row')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$strong,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text(g.goal.description)
												])),
											A2(
											$elm$html$Html$small,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text(
													g.goal.metric.name + (' · ' + ((g.goal.metric.direction === 'HigherIsBetter') ? '↑ 증가' : '↓ 감소')))
												]))
										])),
									A2(
									$elm$html$Html$td,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(
											A2(
												$elm$core$Maybe$withDefault,
												'책임자 미지정',
												A2(
													$elm$core$Maybe$map,
													$author$project$Ui$Label$personName(w),
													g.owner)))
										])),
									A2(
									$elm$html$Html$td,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(
											A2(
												$elm$core$Maybe$withDefault,
												'—',
												A2($elm$core$Maybe$map, $author$project$Ui$Label$formatNumber, g.evaluation.latestValue)) + (' / ' + ($author$project$Ui$Label$formatNumber(g.goal.target) + (' ' + g.goal.metric.unit))))
										])),
									A2(
									$elm$html$Html$td,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(
											$elm$core$String$fromInt(
												$elm$core$Basics$round(g.evaluation.progress * 100)) + '%'),
											A2(
											$elm$html$Html$small,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text(
													'기준 ' + $author$project$Ui$Label$formatNumber(g.goal.baseline))
												]))
										])),
									A2(
									$elm$html$Html$td,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(
											A2($elm$core$String$left, 10, g.goal.deadline))
										])),
									A2(
									$elm$html$Html$td,
									_List_Nil,
									_List_fromArray(
										[
											$author$project$Ui$Common$badge(g)
										]))
								])),
							A4(
							$author$project$Ui$ListView$detailRow,
							6,
							_List_Nil,
							g.goal.description + ' · 결과 보고 · 평가 · 이력',
							A3($author$project$Page$Results$resultContent, model, w, g))
						]);
				},
				w.goals));
	});
var $author$project$Page$Results$resultCard = F3(
	function (model, w, g) {
		return A2(
			$elm$html$Html$article,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('goal-card'),
					$elm$html$Html$Attributes$id('goal-' + g.goal.id),
					$elm$html$Html$Attributes$tabindex(-1)
				]),
			_Utils_ap(
				A2($author$project$Ui$Common$goalSummary, w, g),
				A3($author$project$Page$Results$resultContent, model, w, g)));
	});
var $author$project$Page$Results$viewWith = F3(
	function (mode, model, w) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('section-head')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$h2,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('목표별 결과와 평가')
								]))
						])),
					$author$project$Ui$Common$note('조직 구조를 정리한 뒤 성과를 추적하는 운영 단계입니다. 실제로 측정한 값과 근거를 보고하면 결과 이력이 다음 학습의 근거가 됩니다.'),
					$elm$core$List$isEmpty(w.goals) ? A2($author$project$Ui$Common$emptyState, '아직 측정할 목표가 없습니다', '목표 메뉴에서 목표를 만든 뒤 결과를 기록하세요.') : (_Utils_eq(mode, $author$project$Ui$ListView$Table) ? A2($author$project$Page$Results$goalTable, model, w) : A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('grid')
						]),
					A2(
						$elm$core$List$map,
						A2($author$project$Page$Results$resultCard, model, w),
						w.goals)))
				]));
	});
var $author$project$ListViewTest$pageHtml = F3(
	function (mode, page, w) {
		switch (page.$) {
			case 'Organizations':
				return A2(
					$author$project$Page$Organizations$viewWith,
					mode,
					{
						forms: $author$project$ListViewTest$forms,
						open: $author$project$ListViewTest$Open,
						organizations: $author$project$Remote$Loaded(
							_List_fromArray(
								[
									{
									demo: false,
									goalCount: $elm$core$List$length(w.goals),
									organization: w.organization,
									peopleCount: $elm$core$List$length(w.people)
								}
								])),
						settings: $author$project$ListViewTest$SettingsFor
					});
			case 'People':
				return A3(
					$author$project$Page$People$viewWith,
					mode,
					{
						filter: $elm$core$Basics$always($author$project$ListViewTest$Other),
						forms: $author$project$ListViewTest$forms,
						goals: $author$project$ListViewTest$Other,
						open: $author$project$ListViewTest$Open,
						query: '',
						reset: $elm$core$Basics$always($author$project$ListViewTest$Other),
						search: $elm$core$Basics$always($author$project$ListViewTest$Other),
						selected: $elm$core$Maybe$Nothing,
						status: 'active'
					},
					w);
			case 'Dashboard':
				return A3(
					$author$project$Page$Goals$viewWith,
					mode,
					{
						draft: $author$project$Form$Goal$fromValues(
							$elm$core$Basics$always('')),
						edit: F2(
							function (_v1, _v2) {
								return $author$project$ListViewTest$Other;
							}),
						expandedGoal: $elm$core$Maybe$Nothing,
						forms: $author$project$ListViewTest$forms,
						results: $author$project$ListViewTest$Open
					},
					w);
			case 'Responsibility':
				return A3(
					$author$project$Page$Responsibility$viewWith,
					mode,
					{forms: $author$project$ListViewTest$forms},
					w);
			case 'Authorities':
				return A3(
					$author$project$Page$Authorities$viewWith,
					mode,
					{forms: $author$project$ListViewTest$forms},
					w);
			case 'Results':
				return A3(
					$author$project$Page$Results$viewWith,
					mode,
					{forms: $author$project$ListViewTest$forms, goals: $author$project$ListViewTest$Open},
					w);
			case 'Reviews':
				return A3(
					$author$project$Page$Learning$viewWith,
					mode,
					{
						draft: $author$project$Form$Review$fromValues(
							$elm$core$Basics$always('')),
						edit: F2(
							function (_v3, _v4) {
								return $author$project$ListViewTest$Other;
							}),
						forms: $author$project$ListViewTest$forms
					},
					w);
			case 'ActivityLog':
				return A4(
					$author$project$Page$Activity$view,
					mode,
					$author$project$Ui$Activity$init,
					$elm$core$Basics$always($author$project$ListViewTest$Other),
					w);
			case 'Settings':
				return $elm$html$Html$text('');
			case 'Discovery':
				return $elm$html$Html$text('');
			case 'Workflows':
				return $elm$html$Html$text('');
			case 'AgentDrafts':
				return $elm$html$Html$text('');
			default:
				return $elm$html$Html$text('');
		}
	});
var $author$project$ListViewTest$pages = _List_fromArray(
	[$author$project$Page$Organizations, $author$project$Page$People, $author$project$Page$Dashboard, $author$project$Page$Responsibility, $author$project$Page$Authorities, $author$project$Page$Results, $author$project$Page$Reviews, $author$project$Page$ActivityLog]);
var $elm$core$Dict$sizeHelp = F2(
	function (n, dict) {
		sizeHelp:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return n;
			} else {
				var left = dict.d;
				var right = dict.e;
				var $temp$n = A2($elm$core$Dict$sizeHelp, n + 1, right),
					$temp$dict = left;
				n = $temp$n;
				dict = $temp$dict;
				continue sizeHelp;
			}
		}
	});
var $elm$core$Dict$size = function (dict) {
	return A2($elm$core$Dict$sizeHelp, 0, dict);
};
var $author$project$Page$Organizations$view = $author$project$Page$Organizations$viewWith($author$project$Ui$ListView$Table);
var $author$project$Page$Responsibility$view = $author$project$Page$Responsibility$viewWith($author$project$Ui$ListView$Table);
var $author$project$ListViewTest$tests = A2(
	$elm_explorations$test$Test$describe,
	'카드와 표 보기',
	_List_fromArray(
		[
			A2(
			$elm_explorations$test$Test$test,
			'보기 버튼은 접근 가능한 선택 상태와 전환 이벤트를 제공한다',
			function (_v0) {
				return A2(
					$elm_explorations$test$Test$Html$Event$expect,
					$author$project$ListViewTest$Change($author$project$Ui$ListView$Table),
					A2(
						$elm_explorations$test$Test$Html$Event$simulate,
						$elm_explorations$test$Test$Html$Event$click,
						A2(
							$elm_explorations$test$Test$Html$Query$find,
							_List_fromArray(
								[
									$elm_explorations$test$Test$Html$Selector$tag('button'),
									$elm_explorations$test$Test$Html$Selector$attribute(
									A2($elm$html$Html$Attributes$attribute, 'aria-pressed', 'false'))
								]),
							$elm_explorations$test$Test$Html$Query$fromHtml(
								A2($author$project$Ui$ListView$controls, $author$project$Ui$ListView$Cards, $author$project$ListViewTest$Change)))));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'카드 버튼은 현재 선택됨을 알린다',
			function (_v1) {
				return A2(
					$elm_explorations$test$Test$Html$Query$has,
					_List_fromArray(
						[
							$elm_explorations$test$Test$Html$Selector$tag('button'),
							$elm_explorations$test$Test$Html$Selector$text('카드'),
							$elm_explorations$test$Test$Html$Selector$attribute(
							A2($elm$html$Html$Attributes$attribute, 'aria-pressed', 'true'))
						]),
					$elm_explorations$test$Test$Html$Query$fromHtml(
						A2($author$project$Ui$ListView$controls, $author$project$Ui$ListView$Cards, $author$project$ListViewTest$Change)));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'각 메뉴의 선택 값은 서로 덮어쓰지 않는다',
			function (_v2) {
				return A2(
					$elm_explorations$test$Expect$equal,
					_List_fromArray(
						[
							$elm$core$Maybe$Just($author$project$Ui$ListView$Table),
							$elm$core$Maybe$Just($author$project$Ui$ListView$Cards),
							$elm$core$Maybe$Just($author$project$Ui$ListView$Table)
						]),
					function (m) {
						return A2(
							$elm$core$List$map,
							function (page) {
								return A2(
									$elm$core$Dict$get,
									$author$project$Page$pageName(page),
									m.pageState.listModes);
							},
							_List_fromArray(
								[$author$project$Page$People, $author$project$Page$Dashboard, $author$project$Page$Results]));
					}(
						A2(
							$author$project$ListViewTest$step,
							A2($author$project$App$Update$SetListMode, $author$project$Page$Results, $author$project$Ui$ListView$Table),
							A2(
								$author$project$ListViewTest$step,
								A2($author$project$App$Update$SetListMode, $author$project$Page$Dashboard, $author$project$Ui$ListView$Cards),
								A2(
									$author$project$ListViewTest$step,
									A2($author$project$App$Update$SetListMode, $author$project$Page$People, $author$project$Ui$ListView$Table),
									$author$project$ListViewTest$ready)))));
			}),
			A2(
			$elm_explorations$test$Test$describe,
			'각 메뉴가 실제 표를 렌더링한다',
			A2(
				$elm$core$List$map,
				function (page) {
					return A2(
						$elm_explorations$test$Test$test,
						$author$project$Page$pageName(page),
						function (_v3) {
							return A2(
								$elm_explorations$test$Test$Html$Query$count,
								$elm_explorations$test$Expect$atLeast(1),
								A2(
									$elm_explorations$test$Test$Html$Query$findAll,
									_List_fromArray(
										[
											$elm_explorations$test$Test$Html$Selector$tag('table')
										]),
									$elm_explorations$test$Test$Html$Query$fromHtml(
										A3($author$project$ListViewTest$pageHtml, $author$project$Ui$ListView$Table, page, $author$project$ListViewTest$sample))));
						});
				},
				$author$project$ListViewTest$pages)),
			A2(
			$elm_explorations$test$Test$describe,
			'빈 목록에서 빈 상태를 유지한다',
			A2(
				$elm$core$List$map,
				function (_v4) {
					var page = _v4.a;
					var message = _v4.b;
					return A2(
						$elm_explorations$test$Test$test,
						$author$project$Page$pageName(page),
						function (_v5) {
							return A2(
								$elm_explorations$test$Test$Html$Query$has,
								_List_fromArray(
									[
										$elm_explorations$test$Test$Html$Selector$text(message)
									]),
								$elm_explorations$test$Test$Html$Query$fromHtml(
									A3($author$project$ListViewTest$pageHtml, $author$project$Ui$ListView$Table, page, $author$project$ListViewTest$workspace)));
						});
				},
				_List_fromArray(
					[
						_Utils_Tuple2($author$project$Page$People, '표시할 구성원이 없습니다'),
						_Utils_Tuple2($author$project$Page$Dashboard, '현재 관리 중인 목표가 있나요?'),
						_Utils_Tuple2($author$project$Page$Authorities, '구성원을 먼저 추가하세요'),
						_Utils_Tuple2($author$project$Page$Results, '아직 측정할 목표가 없습니다')
					]))),
			A2(
			$elm_explorations$test$Test$test,
			'표 컨테이너는 키보드 접근과 열 제목을 제공한다',
			function (_v6) {
				return A2(
					$elm_explorations$test$Test$Html$Query$has,
					_List_fromArray(
						[
							$elm_explorations$test$Test$Html$Selector$attribute(
							$elm$html$Html$Attributes$tabindex(0)),
							$elm_explorations$test$Test$Html$Selector$attribute(
							A2($elm$html$Html$Attributes$attribute, 'role', 'region'))
						]),
					$elm_explorations$test$Test$Html$Query$fromHtml(
						A3(
							$author$project$Ui$ListView$tableView,
							'테스트',
							_List_fromArray(
								['이름']),
							_List_Nil)));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'기존 책임 view는 표를 기본으로 유지한다',
			function (_v7) {
				return A2(
					$elm_explorations$test$Test$Html$Query$count,
					$elm_explorations$test$Expect$atLeast(1),
					A2(
						$elm_explorations$test$Test$Html$Query$findAll,
						_List_fromArray(
							[
								$elm_explorations$test$Test$Html$Selector$tag('table')
							]),
						$elm_explorations$test$Test$Html$Query$fromHtml(
							A2(
								$author$project$Page$Responsibility$view,
								{forms: $author$project$ListViewTest$forms},
								$author$project$ListViewTest$sample))));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'기존 조직 view는 표를 기본으로 제공한다',
			function (_v8) {
				return A2(
					$elm_explorations$test$Test$Html$Query$count,
					$elm_explorations$test$Expect$equal(1),
					A2(
						$elm_explorations$test$Test$Html$Query$findAll,
						_List_fromArray(
							[
								$elm_explorations$test$Test$Html$Selector$tag('table')
							]),
						$elm_explorations$test$Test$Html$Query$fromHtml(
							$author$project$Page$Organizations$view(
								{
									forms: $author$project$ListViewTest$forms,
									open: $author$project$ListViewTest$Open,
									organizations: $author$project$Remote$Loaded(
										_List_fromArray(
											[
												{demo: false, goalCount: 1, organization: $author$project$ListViewTest$sample.organization, peopleCount: 1}
											])),
									settings: $author$project$ListViewTest$SettingsFor
								}))));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'보기 전환은 입력 초안과 검색 선택 및 서버 상태를 보존한다',
			function (_v9) {
				var drafted = A2(
					$author$project$ListViewTest$step,
					$author$project$App$Update$OpenPerson('p'),
					A2(
						$author$project$ListViewTest$step,
						$author$project$App$Update$FilterPeople('all'),
						A2(
							$author$project$ListViewTest$step,
							$author$project$App$Update$SearchPeople('검색'),
							A2(
								$author$project$ListViewTest$step,
								A2($author$project$App$Update$EditReview, $author$project$Form$Review$Note, '회고 초안'),
								A2(
									$author$project$ListViewTest$step,
									A2($author$project$App$Update$EditGoal, $author$project$Form$Goal$Description, '목표 초안'),
									A2(
										$author$project$ListViewTest$step,
										A3($author$project$App$Update$Edit, $author$project$Form$Action$AddPerson, 'name', '작성 중'),
										$author$project$ListViewTest$ready))))));
				var switched = A2(
					$author$project$ListViewTest$step,
					A2($author$project$App$Update$SetListMode, $author$project$Page$People, $author$project$Ui$ListView$Table),
					drafted);
				return A2(
					$elm_explorations$test$Expect$equal,
					drafted,
					A2(
						$author$project$AppFixture$mapPage,
						function (p) {
							return _Utils_update(
								p,
								{listModes: drafted.pageState.listModes});
						},
						switched));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'각 메뉴 선택은 독립적이며 왕복 탐색 후 유지된다',
			function (_v10) {
				var selected = A2(
					$author$project$ListViewTest$step,
					A2($author$project$App$Update$SetListMode, $author$project$Page$Results, $author$project$Ui$ListView$Table),
					A2(
						$author$project$ListViewTest$step,
						A2($author$project$App$Update$SetListMode, $author$project$Page$Dashboard, $author$project$Ui$ListView$Cards),
						A2(
							$author$project$ListViewTest$step,
							A2($author$project$App$Update$SetListMode, $author$project$Page$People, $author$project$Ui$ListView$Table),
							$author$project$ListViewTest$ready)));
				var navigated = A2(
					$author$project$ListViewTest$step,
					A2(
						$author$project$App$Update$Navigate,
						$author$project$Page$People,
						$elm$core$Maybe$Just('org-a')),
					A2(
						$author$project$ListViewTest$step,
						A2(
							$author$project$App$Update$Navigate,
							$author$project$Page$Results,
							$elm$core$Maybe$Just('org-a')),
						selected));
				return A2(
					$elm_explorations$test$Expect$all,
					_List_fromArray(
						[
							function (_v11) {
							return A2($elm_explorations$test$Expect$equal, selected.pageState.listModes, navigated.pageState.listModes);
						},
							function (_v12) {
							return A2(
								$elm_explorations$test$Expect$equal,
								3,
								$elm$core$Dict$size(selected.pageState.listModes));
						}
						]),
					_Utils_Tuple0);
			}),
			A2(
			$elm_explorations$test$Test$test,
			'조직 표에서 조직을 열 수 있다',
			function (_v13) {
				return A3(
					$author$project$ListViewTest$clickButton,
					'조직 열기 →',
					$author$project$ListViewTest$Open('org-a'),
					A3($author$project$ListViewTest$pageHtml, $author$project$Ui$ListView$Table, $author$project$Page$Organizations, $author$project$ListViewTest$sample));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'조직 표에서 설정으로 이동한다',
			function (_v14) {
				return A3(
					$author$project$ListViewTest$clickButton,
					'상세 · 수정 · 삭제',
					$author$project$ListViewTest$SettingsFor('org-a'),
					A3($author$project$ListViewTest$pageHtml, $author$project$Ui$ListView$Table, $author$project$Page$Organizations, $author$project$ListViewTest$sample));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'구성원 표에서 상세 선택을 전달한다',
			function (_v15) {
				return A3(
					$author$project$ListViewTest$clickButton,
					'상세 · 수정',
					$author$project$ListViewTest$Open('p'),
					A3($author$project$ListViewTest$pageHtml, $author$project$Ui$ListView$Table, $author$project$Page$People, $author$project$ListViewTest$sample));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'목표 표에서 활성화 액션을 전달한다',
			function (_v16) {
				return A3(
					$author$project$ListViewTest$clickButton,
					'목표 활성화',
					$author$project$ListViewTest$Submit(
						$author$project$Form$Action$Activate('g')),
					A3($author$project$ListViewTest$pageHtml, $author$project$Ui$ListView$Table, $author$project$Page$Dashboard, $author$project$ListViewTest$sample));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'목표 표에서 결과로 이동한다',
			function (_v17) {
				return A3(
					$author$project$ListViewTest$clickButton,
					'결과 보고 · 평가 →',
					$author$project$ListViewTest$Open('goal-g'),
					A3($author$project$ListViewTest$pageHtml, $author$project$Ui$ListView$Table, $author$project$Page$Dashboard, $author$project$ListViewTest$sample));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'책임 표에서 책임자 입력을 전달한다',
			function (_v18) {
				return A2(
					$elm_explorations$test$Test$Html$Event$expect,
					A3(
						$author$project$ListViewTest$Edit,
						$author$project$Form$Action$Assign('g'),
						'owner',
						'p'),
					A2(
						$elm_explorations$test$Test$Html$Event$simulate,
						$elm_explorations$test$Test$Html$Event$input('p'),
						A2(
							$elm_explorations$test$Test$Html$Query$find,
							_List_fromArray(
								[
									$elm_explorations$test$Test$Html$Selector$tag('select'),
									$elm_explorations$test$Test$Html$Selector$attribute(
									$elm$html$Html$Attributes$name('owner'))
								]),
							$elm_explorations$test$Test$Html$Query$fromHtml(
								A3($author$project$ListViewTest$pageHtml, $author$project$Ui$ListView$Table, $author$project$Page$Responsibility, $author$project$ListViewTest$sample)))));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'권한 요약은 편집 초안이 아닌 저장된 예산과 권한을 표시한다',
			function (_v19) {
				var saved = _Utils_update(
					$author$project$ListViewTest$sample,
					{
						authorities: _List_fromArray(
							[
								{budgetLimit: 300, canApprove: _List_Nil, canChangePrice: false, canHire: true, owner: 'p'}
							])
					});
				var editing = _Utils_update(
					$author$project$ListViewTest$forms,
					{
						value: F2(
							function (_v20, key) {
								return (key === 'budget') ? '999' : 'false';
							})
					});
				return A2(
					$elm_explorations$test$Test$Html$Query$has,
					_List_fromArray(
						[
							$elm_explorations$test$Test$Html$Selector$text('300'),
							$elm_explorations$test$Test$Html$Selector$text('채용')
						]),
					$elm_explorations$test$Test$Html$Query$first(
						A2(
							$elm_explorations$test$Test$Html$Query$findAll,
							_List_fromArray(
								[
									$elm_explorations$test$Test$Html$Selector$tag('tbody')
								]),
							$elm_explorations$test$Test$Html$Query$fromHtml(
								A3(
									$author$project$Page$Authorities$viewWith,
									$author$project$Ui$ListView$Table,
									{forms: editing},
									saved)))));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'권한 표에서 예산 입력을 전달한다',
			function (_v21) {
				return A2(
					$elm_explorations$test$Test$Html$Event$expect,
					A3(
						$author$project$ListViewTest$Edit,
						$author$project$Form$Action$Grant('p'),
						'budget',
						'100'),
					A2(
						$elm_explorations$test$Test$Html$Event$simulate,
						$elm_explorations$test$Test$Html$Event$input('100'),
						A2(
							$elm_explorations$test$Test$Html$Query$find,
							_List_fromArray(
								[
									$elm_explorations$test$Test$Html$Selector$tag('input'),
									$elm_explorations$test$Test$Html$Selector$attribute(
									$elm$html$Html$Attributes$id('authority-p-budget'))
								]),
							$elm_explorations$test$Test$Html$Query$fromHtml(
								A3($author$project$ListViewTest$pageHtml, $author$project$Ui$ListView$Table, $author$project$Page$Authorities, $author$project$ListViewTest$sample)))));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'결과 표에서 평가 기록 액션을 전달한다',
			function (_v22) {
				return A3(
					$author$project$ListViewTest$clickButton,
					'평가 기록',
					$author$project$ListViewTest$Submit(
						$author$project$Form$Action$Evaluate('g')),
					A3($author$project$ListViewTest$pageHtml, $author$project$Ui$ListView$Table, $author$project$Page$Results, $author$project$ListViewTest$sample));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'결과 표에서 결과 보고 입력을 전달한다',
			function (_v23) {
				return A2(
					$elm_explorations$test$Test$Html$Event$expect,
					A3(
						$author$project$ListViewTest$Edit,
						$author$project$Form$Action$Report('g'),
						'value',
						'42'),
					A2(
						$elm_explorations$test$Test$Html$Event$simulate,
						$elm_explorations$test$Test$Html$Event$input('42'),
						A2(
							$elm_explorations$test$Test$Html$Query$find,
							_List_fromArray(
								[
									$elm_explorations$test$Test$Html$Selector$tag('input'),
									$elm_explorations$test$Test$Html$Selector$attribute(
									$elm$html$Html$Attributes$id('result-g-value'))
								]),
							$elm_explorations$test$Test$Html$Query$fromHtml(
								A3($author$project$ListViewTest$pageHtml, $author$project$Ui$ListView$Table, $author$project$Page$Results, $author$project$ListViewTest$sample)))));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'학습 표는 학습 결정 담당자를 보존하고 전체 활동 기록은 분리한다',
			function (_v24) {
				return A2(
					$elm_explorations$test$Expect$all,
					_List_fromArray(
						[
							function (_v25) {
							return A2(
								$elm_explorations$test$Test$Html$Query$has,
								_List_fromArray(
									[
										$elm_explorations$test$Test$Html$Selector$text('고객 의견'),
										$elm_explorations$test$Test$Html$Selector$text('개선 진행'),
										$elm_explorations$test$Test$Html$Selector$text('김직원')
									]),
								$elm_explorations$test$Test$Html$Query$fromHtml(
									A3($author$project$ListViewTest$pageHtml, $author$project$Ui$ListView$Table, $author$project$Page$Reviews, $author$project$ListViewTest$sample)));
						},
							function (_v26) {
							return A2(
								$elm_explorations$test$Test$Html$Query$hasNot,
								_List_fromArray(
									[
										$elm_explorations$test$Test$Html$Selector$text('목표 등록')
									]),
								$elm_explorations$test$Test$Html$Query$fromHtml(
									A3($author$project$ListViewTest$pageHtml, $author$project$Ui$ListView$Table, $author$project$Page$Reviews, $author$project$ListViewTest$sample)));
						}
						]),
					_Utils_Tuple0);
			})
		]));
var $author$project$PageTest$GoalEdited = F2(
	function (a, b) {
		return {$: 'GoalEdited', a: a, b: b};
	});
var $author$project$PageTest$Other = {$: 'Other'};
var $author$project$PageTest$ReviewEdited = F2(
	function (a, b) {
		return {$: 'ReviewEdited', a: a, b: b};
	});
var $author$project$PageTest$forms = {
	busy: false,
	edit: F3(
		function (_v0, _v1, _v2) {
			return $author$project$PageTest$Other;
		}),
	fresh: true,
	saving: $elm$core$Maybe$Nothing,
	submit: function (_v3) {
		return $author$project$PageTest$Other;
	},
	value: F2(
		function (_v4, _v5) {
			return '';
		})
};
var $author$project$Page$Goals$view = $author$project$Page$Goals$viewWith($author$project$Ui$ListView$Table);
var $author$project$Page$Learning$view = $author$project$Page$Learning$viewWith($author$project$Ui$ListView$Table);
var $author$project$PageTest$tests = A2(
	$elm_explorations$test$Test$describe,
	'Page fields emit typed edits',
	_List_fromArray(
		[
			A2(
			$elm_explorations$test$Test$test,
			'goal target has a typed field callback',
			function (_v0) {
				return A2(
					$elm_explorations$test$Test$Html$Event$expect,
					A2($author$project$PageTest$GoalEdited, $author$project$Form$Goal$Target, '42'),
					A2(
						$elm_explorations$test$Test$Html$Event$simulate,
						$elm_explorations$test$Test$Html$Event$input('42'),
						A2(
							$elm_explorations$test$Test$Html$Query$find,
							_List_fromArray(
								[
									$elm_explorations$test$Test$Html$Selector$tag('input'),
									$elm_explorations$test$Test$Html$Selector$attribute(
									$elm$html$Html$Attributes$name('target'))
								]),
							$elm_explorations$test$Test$Html$Query$fromHtml(
								A2(
									$author$project$Page$Goals$view,
									{
										draft: $author$project$Form$Goal$fromValues(
											$elm$core$Basics$always('')),
										edit: $author$project$PageTest$GoalEdited,
										expandedGoal: $elm$core$Maybe$Nothing,
										forms: $author$project$PageTest$forms,
										results: function (_v1) {
											return $author$project$PageTest$Other;
										}
									},
									{
										compiler: {diagnostics: _List_Nil, errors: 0, warnings: 0},
										goals: _List_Nil,
										people: _List_Nil,
										reviews: _List_Nil
									})))));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'learning textarea has a typed review callback',
			function (_v2) {
				return A2(
					$elm_explorations$test$Test$Html$Event$expect,
					A2($author$project$PageTest$ReviewEdited, $author$project$Form$Review$Learning, 'Keep this'),
					A2(
						$elm_explorations$test$Test$Html$Event$simulate,
						$elm_explorations$test$Test$Html$Event$input('Keep this'),
						A2(
							$elm_explorations$test$Test$Html$Query$find,
							_List_fromArray(
								[
									$elm_explorations$test$Test$Html$Selector$tag('textarea')
								]),
							$elm_explorations$test$Test$Html$Query$fromHtml(
								A2(
									$author$project$Page$Learning$view,
									{
										draft: $author$project$Form$Review$fromValues(
											$elm$core$Basics$always('')),
										edit: $author$project$PageTest$ReviewEdited,
										forms: $author$project$PageTest$forms
									},
									{events: _List_Nil, goals: _List_Nil, people: _List_Nil, reviewWarnings: _List_Nil, reviews: _List_Nil})))));
			})
		]));
var $author$project$PeopleTest$person = {
	active: true,
	department: $elm$core$Maybe$Just('Platform'),
	email: $elm$core$Maybe$Just('person@example.com'),
	id: 'p',
	name: '김직원',
	reportsTo: $elm$core$Maybe$Nothing,
	role: 'Engineer'
};
var $author$project$PeopleTest$tests = A2(
	$elm_explorations$test$Test$describe,
	'구성원 검색과 상태 필터',
	_List_fromArray(
		[
			A2(
			$elm_explorations$test$Test$test,
			'이름 역할 부서 이메일을 공백 제거와 대소문자 무시로 검색한다',
			function (_v0) {
				return A2(
					$elm_explorations$test$Expect$equal,
					_List_fromArray(
						[true, true, true, true]),
					A2(
						$elm$core$List$map,
						function (query) {
							return A3($author$project$Page$People$matches, query, 'active', $author$project$PeopleTest$person);
						},
						_List_fromArray(
							[' 김직원 ', 'ENGINEER', 'platform', 'EXAMPLE.COM'])));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'비활성 구성원은 재직 필터에서 제외하고 전체와 비활성에서 조회한다',
			function (_v1) {
				return A2(
					$elm_explorations$test$Expect$equal,
					_List_fromArray(
						[false, true, true]),
					A2(
						$elm$core$List$map,
						function (status) {
							return A3(
								$author$project$Page$People$matches,
								'',
								status,
								_Utils_update(
									$author$project$PeopleTest$person,
									{active: false}));
						},
						_List_fromArray(
							['active', 'inactive', 'all'])));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'검색어 불일치와 비어 있는 선택 프로필을 처리한다',
			function (_v2) {
				return A2(
					$elm_explorations$test$Expect$equal,
					false,
					A3(
						$author$project$Page$People$matches,
						'missing',
						'all',
						_Utils_update(
							$author$project$PeopleTest$person,
							{department: $elm$core$Maybe$Nothing, email: $elm$core$Maybe$Nothing})));
			})
		]));
var $author$project$ResponsibilityGraphTest$Go = function (a) {
	return {$: 'Go', a: a};
};
var $author$project$ResponsibilityGraphTest$GraphMessage = function (a) {
	return {$: 'GraphMessage', a: a};
};
var $author$project$App$Update$GraphMsg = function (a) {
	return {$: 'GraphMsg', a: a};
};
var $elm_explorations$test$Test$Html$Event$custom = $elm$core$Tuple$pair;
var $elm_explorations$test$Test$Html$Event$checkEffect = F2(
	function (extractor, event) {
		return A2(
			$elm$core$Result$andThen,
			function (handler) {
				return A2(
					$elm$core$Result$mapError,
					$elm$json$Json$Decode$errorToString,
					A2(
						$elm$json$Json$Decode$decodeValue,
						handler,
						$elm_explorations$test$Test$Html$Event$eventPayload(event)));
			},
			A2(
				$elm$core$Result$map,
				$elm$json$Json$Decode$map(extractor),
				$elm_explorations$test$Test$Html$Event$findHandler(event)));
	});
var $elm_explorations$test$Test$Html$Event$checkPreventDefault = $elm_explorations$test$Test$Html$Event$checkEffect(
	function ($) {
		return $.preventDefault;
	});
var $elm_explorations$test$Test$Html$Event$expectPreventDefault = function (event) {
	var _v0 = $elm_explorations$test$Test$Html$Event$checkPreventDefault(event);
	if (_v0.$ === 'Err') {
		var reason = _v0.a;
		return $elm_explorations$test$Expect$fail(reason);
	} else {
		if (!_v0.a) {
			return $elm_explorations$test$Expect$fail('I found a handler that could have prevented default action of the event, but it didn\'t.');
		} else {
			return $elm_explorations$test$Expect$pass;
		}
	}
};
var $author$project$ResponsibilityGraphTest$render = F2(
	function (state, w) {
		return $elm_explorations$test$Test$Html$Query$fromHtml(
			A4(
				$author$project$Ui$ResponsibilityGraph$view,
				state,
				$elm$core$Maybe$Just($author$project$ResponsibilityGraphTest$GraphMessage),
				$elm$core$Maybe$Just($author$project$ResponsibilityGraphTest$Go),
				w));
	});
var $author$project$ResponsibilityGraphTest$selected = function (key) {
	return A2(
		$author$project$Ui$ResponsibilityGraph$update,
		$author$project$Ui$ResponsibilityGraph$Select(key),
		$author$project$Ui$ResponsibilityGraph$init);
};
var $author$project$ResponsibilityGraphTest$tests = A2(
	$elm_explorations$test$Test$describe,
	'책임 관계 다이어그램',
	_List_fromArray(
		[
			A2(
			$elm_explorations$test$Test$test,
			'새 검색은 이전 노드 선택을 해제하여 강조 충돌을 방지한다',
			function (_v0) {
				return A2(
					$elm_explorations$test$Expect$equal,
					$elm$core$Maybe$Nothing,
					A2(
						$author$project$Ui$ResponsibilityGraph$update,
						$author$project$Ui$ResponsibilityGraph$Search('같은 이름'),
						$author$project$ResponsibilityGraphTest$selected('GoalNode:g')).selected);
			}),
			A2(
			$elm_explorations$test$Test$test,
			'자원을 숨기면 선택된 자원 상세도 해제한다',
			function (_v1) {
				return A2(
					$elm_explorations$test$Expect$equal,
					$elm$core$Maybe$Nothing,
					A2(
						$author$project$Ui$ResponsibilityGraph$update,
						$author$project$Ui$ResponsibilityGraph$Resources(false),
						A2(
							$author$project$Ui$ResponsibilityGraph$update,
							$author$project$Ui$ResponsibilityGraph$Select('ResourceNode:Budget'),
							A2(
								$author$project$Ui$ResponsibilityGraph$update,
								$author$project$Ui$ResponsibilityGraph$Resources(true),
								$author$project$Ui$ResponsibilityGraph$init))).selected);
			}),
			A2(
			$elm_explorations$test$Test$test,
			'Space 노드 활성화는 페이지 스크롤을 막는다',
			function (_v2) {
				return $elm_explorations$test$Test$Html$Event$expectPreventDefault(
					A2(
						$elm_explorations$test$Test$Html$Event$simulate,
						A2(
							$elm_explorations$test$Test$Html$Event$custom,
							'keydown',
							$elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'key',
										$elm$json$Json$Encode$string(' '))
									]))),
						A2(
							$elm_explorations$test$Test$Html$Query$find,
							_List_fromArray(
								[
									$elm_explorations$test$Test$Html$Selector$tag('g'),
									$elm_explorations$test$Test$Html$Selector$attribute(
									A2($elm$html$Html$Attributes$attribute, 'aria-label', '고객 성장 · 권한 부족'))
								]),
							A2($author$project$ResponsibilityGraphTest$render, $author$project$Ui$ResponsibilityGraph$init, $author$project$GraphFixture$sample))));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'그래프 보기와 메뉴별 카드표 및 책임자 초안은 서로 독립적이다',
			function (_v3) {
				var step = F2(
					function (msg, m) {
						return A2($author$project$App$Update$update, msg, m).a;
					});
				var initial = $author$project$App$Update$init(
					{deadline: '2026-12-31', seed: 'graph', today: '2026-01-01'}).a;
				var ready = A2(
					$author$project$AppFixture$mapSession,
					function (s) {
						return _Utils_update(
							s,
							{
								fresh: true,
								org: $elm$core$Maybe$Just('org-a'),
								syncing: false,
								workspace: $author$project$Remote$Loaded($author$project$GraphFixture$sample)
							});
					},
					initial);
				var drafted = A2(
					step,
					A2($author$project$App$Update$SetListMode, $author$project$Page$Responsibility, $author$project$Ui$ListView$Table),
					A2(
						step,
						A3(
							$author$project$App$Update$Edit,
							$author$project$Form$Action$Assign('g'),
							'owner',
							'p2'),
						ready));
				var graphChanged = A2(
					step,
					$author$project$App$Update$GraphMsg(
						$author$project$Ui$ResponsibilityGraph$Search('고객')),
					A2(
						step,
						$author$project$App$Update$GraphMsg(
							$author$project$Ui$ResponsibilityGraph$Select('GoalNode:g')),
						drafted));
				return A2(
					$elm_explorations$test$Expect$equal,
					drafted,
					A2(
						$author$project$AppFixture$mapPage,
						function (p) {
							return _Utils_update(
								p,
								{graph: drafted.pageState.graph});
						},
						graphChanged));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'같은 조직 메뉴 왕복은 그래프 상태를 보존하고 조직 변경은 초기화한다',
			function (_v4) {
				var step = F2(
					function (msg, m) {
						return A2($author$project$App$Update$update, msg, m).a;
					});
				var initial = $author$project$App$Update$init(
					{deadline: '2026-12-31', seed: 'graph', today: '2026-01-01'}).a;
				var ready = A2(
					$author$project$AppFixture$mapSession,
					function (s) {
						return _Utils_update(
							s,
							{
								fresh: true,
								org: $elm$core$Maybe$Just('org-a'),
								syncing: false,
								workspace: $author$project$Remote$Loaded($author$project$GraphFixture$sample)
							});
					},
					initial);
				var changed = A2(
					step,
					$author$project$App$Update$GraphMsg(
						$author$project$Ui$ResponsibilityGraph$Search('고객')),
					A2(
						step,
						$author$project$App$Update$GraphMsg(
							$author$project$Ui$ResponsibilityGraph$Select('GoalNode:g')),
						ready));
				var returned = A2(
					step,
					A2(
						$author$project$App$Update$Navigate,
						$author$project$Page$Responsibility,
						$elm$core$Maybe$Just('org-a')),
					A2(
						step,
						A2(
							$author$project$App$Update$Navigate,
							$author$project$Page$Dashboard,
							$elm$core$Maybe$Just('org-a')),
						changed));
				var other = A2(
					step,
					A2(
						$author$project$App$Update$Navigate,
						$author$project$Page$Responsibility,
						$elm$core$Maybe$Just('org-b')),
					returned);
				return A2(
					$elm_explorations$test$Expect$equal,
					_Utils_Tuple2(changed.pageState.graph, $author$project$Ui$ResponsibilityGraph$init),
					_Utils_Tuple2(returned.pageState.graph, other.pageState.graph));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'같은 노드는 중복 연결에도 한 번만 생성하고 같은 이름의 서로 다른 ID는 보존한다',
			function (_v5) {
				return A2(
					$elm_explorations$test$Expect$equal,
					_List_fromArray(
						['GoalNode:g', 'GoalNode:isolated', 'MetricNode:m', 'PersonNode:p', 'PersonNode:p2', 'ResourceNode:Budget', 'ResourceNode:Pricing']),
					A2(
						$elm$core$List$map,
						$author$project$Ui$ResponsibilityGraph$nodeKey,
						$author$project$Ui$ResponsibilityGraph$nodes($author$project$GraphFixture$sample)));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'연결이 전혀 없는 목표도 노드로 포함한다',
			function (_v6) {
				return A2(
					$elm_explorations$test$Expect$equal,
					_List_fromArray(
						[
							A2($author$project$GraphFixture$node, 'GoalNode', 'g')
						]),
					$author$project$Ui$ResponsibilityGraph$nodes(
						_Utils_update(
							$author$project$GraphFixture$workspace,
							{
								goals: _List_fromArray(
									[$author$project$GraphFixture$goal])
							})));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'실제 지표 이름과 한국어 자원 이름을 표시한다',
			function (_v7) {
				return A2(
					$elm_explorations$test$Expect$equal,
					_List_fromArray(
						['신규 고객 수', '예산', '가격 결정']),
					A2(
						$elm$core$List$map,
						$author$project$Ui$ResponsibilityGraph$nodeLabel($author$project$GraphFixture$sample),
						_List_fromArray(
							[
								A2($author$project$GraphFixture$node, 'MetricNode', 'm'),
								A2($author$project$GraphFixture$node, 'ResourceNode', 'Budget'),
								A2($author$project$GraphFixture$node, 'ResourceNode', 'Pricing')
							])));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'하위 목표 방향은 변경하지 않고 한글 관계명을 제공한다',
			function (_v8) {
				return A2(
					$elm_explorations$test$Expect$equal,
					_List_fromArray(
						['책임', '하위 목표', '측정 지표', '보유 권한']),
					A2(
						$elm$core$List$map,
						$author$project$Ui$ResponsibilityGraph$relationLabel,
						_List_fromArray(
							['Owns', 'DependsOn', 'Measures', 'Controls'])));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'기본 화면은 책임과 측정 지표 관계만 표시한다',
			function (_v9) {
				return A2(
					$elm_explorations$test$Expect$equal,
					_List_fromArray(
						['Owns', 'Owns', 'Measures']),
					A2(
						$elm$core$List$map,
						function ($) {
							return $.kind;
						},
						A2($author$project$Ui$ResponsibilityGraph$visibleEdges, $author$project$Ui$ResponsibilityGraph$init, $author$project$GraphFixture$sample.edges)));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'하위 목표와 자원 토글은 독립적이다',
			function (_v10) {
				return A2(
					$elm_explorations$test$Expect$equal,
					_List_fromArray(
						['Owns', 'Owns', 'Measures', 'DependsOn']),
					A2(
						$elm$core$List$map,
						function ($) {
							return $.kind;
						},
						A2(
							$author$project$Ui$ResponsibilityGraph$visibleEdges,
							A2(
								$author$project$Ui$ResponsibilityGraph$update,
								$author$project$Ui$ResponsibilityGraph$Dependencies(true),
								$author$project$Ui$ResponsibilityGraph$init),
							$author$project$GraphFixture$sample.edges)));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'자원 토글은 권한 및 예산 연결을 포함한다',
			function (_v11) {
				return A2(
					$elm_explorations$test$Expect$equal,
					2,
					$elm$core$List$length(
						A2(
							$elm$core$List$filter,
							A2(
								$elm$core$Basics$composeR,
								function ($) {
									return $.kind;
								},
								$elm$core$Basics$eq('Controls')),
							A2(
								$author$project$Ui$ResponsibilityGraph$visibleEdges,
								A2(
									$author$project$Ui$ResponsibilityGraph$update,
									$author$project$Ui$ResponsibilityGraph$Resources(true),
									$author$project$Ui$ResponsibilityGraph$init),
								$author$project$GraphFixture$sample.edges))));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'확대에는 상한과 하한이 있다',
			function (_v12) {
				return A2(
					$elm_explorations$test$Expect$equal,
					_Utils_Tuple2(3, 1),
					_Utils_Tuple2(
						A2(
							$author$project$Ui$ResponsibilityGraph$update,
							$author$project$Ui$ResponsibilityGraph$Zoom(100),
							$author$project$Ui$ResponsibilityGraph$init).zoom,
						A2(
							$author$project$Ui$ResponsibilityGraph$update,
							$author$project$Ui$ResponsibilityGraph$Zoom(-100),
							$author$project$Ui$ResponsibilityGraph$init).zoom));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'전체 맞춤은 검색 및 선택을 유지한 채 확대율을 초기화한다',
			function (_v13) {
				var state = A2(
					$author$project$Ui$ResponsibilityGraph$update,
					$author$project$Ui$ResponsibilityGraph$Search('고객'),
					$author$project$ResponsibilityGraphTest$selected('GoalNode:g'));
				return A2(
					$elm_explorations$test$Expect$equal,
					state,
					A2(
						$author$project$Ui$ResponsibilityGraph$update,
						$author$project$Ui$ResponsibilityGraph$Fit,
						A2(
							$author$project$Ui$ResponsibilityGraph$update,
							$author$project$Ui$ResponsibilityGraph$Zoom(1),
							state)));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'보기 전환은 선택 검색 및 토글을 보존한다',
			function (_v14) {
				var state = A2(
					$author$project$Ui$ResponsibilityGraph$update,
					$author$project$Ui$ResponsibilityGraph$Resources(true),
					A2(
						$author$project$Ui$ResponsibilityGraph$update,
						$author$project$Ui$ResponsibilityGraph$Search('고객'),
						$author$project$ResponsibilityGraphTest$selected('PersonNode:p')));
				return A2(
					$elm_explorations$test$Expect$equal,
					state,
					A2(
						$author$project$Ui$ResponsibilityGraph$update,
						$author$project$Ui$ResponsibilityGraph$SetDiagram(true),
						A2(
							$author$project$Ui$ResponsibilityGraph$update,
							$author$project$Ui$ResponsibilityGraph$SetDiagram(false),
							state)));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'목표 노드는 미지정과 권한 부족을 구분한다',
			function (_v15) {
				return A2(
					$elm_explorations$test$Test$Html$Query$has,
					_List_fromArray(
						[
							$elm_explorations$test$Test$Html$Selector$attribute(
							A2($elm$html$Html$Attributes$attribute, 'aria-label', '고객 성장 · 권한 부족'))
						]),
					A2($author$project$ResponsibilityGraphTest$render, $author$project$Ui$ResponsibilityGraph$init, $author$project$GraphFixture$sample));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'미지정 목표에는 권한 부족 대신 책임자 미지정을 표시한다',
			function (_v16) {
				return A2(
					$elm_explorations$test$Test$Html$Query$has,
					_List_fromArray(
						[
							$elm_explorations$test$Test$Html$Selector$attribute(
							A2($elm$html$Html$Attributes$attribute, 'aria-label', '연결 없는 목표 · 책임자 미지정'))
						]),
					A2($author$project$ResponsibilityGraphTest$render, $author$project$Ui$ResponsibilityGraph$init, $author$project$GraphFixture$sample));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'노드 클릭으로 상세 선택 메시지를 전달한다',
			function (_v17) {
				return A2(
					$elm_explorations$test$Test$Html$Event$expect,
					$author$project$ResponsibilityGraphTest$GraphMessage(
						$author$project$Ui$ResponsibilityGraph$Select('GoalNode:g')),
					A2(
						$elm_explorations$test$Test$Html$Event$simulate,
						$elm_explorations$test$Test$Html$Event$click,
						A2(
							$elm_explorations$test$Test$Html$Query$find,
							_List_fromArray(
								[
									$elm_explorations$test$Test$Html$Selector$tag('g'),
									$elm_explorations$test$Test$Html$Selector$attribute(
									A2($elm$html$Html$Attributes$attribute, 'aria-label', '고객 성장 · 권한 부족'))
								]),
							A2($author$project$ResponsibilityGraphTest$render, $author$project$Ui$ResponsibilityGraph$init, $author$project$GraphFixture$sample))));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'Enter 키로 노드를 선택할 수 있다',
			function (_v18) {
				return A2(
					$elm_explorations$test$Test$Html$Event$expect,
					$author$project$ResponsibilityGraphTest$GraphMessage(
						$author$project$Ui$ResponsibilityGraph$Select('GoalNode:g')),
					A2(
						$elm_explorations$test$Test$Html$Event$simulate,
						A2(
							$elm_explorations$test$Test$Html$Event$custom,
							'keydown',
							$elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'key',
										$elm$json$Json$Encode$string('Enter'))
									]))),
						A2(
							$elm_explorations$test$Test$Html$Query$find,
							_List_fromArray(
								[
									$elm_explorations$test$Test$Html$Selector$tag('g'),
									$elm_explorations$test$Test$Html$Selector$attribute(
									A2($elm$html$Html$Attributes$attribute, 'aria-label', '고객 성장 · 권한 부족'))
								]),
							A2($author$project$ResponsibilityGraphTest$render, $author$project$Ui$ResponsibilityGraph$init, $author$project$GraphFixture$sample))));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'Space 키로 노드를 선택할 수 있다',
			function (_v19) {
				return A2(
					$elm_explorations$test$Test$Html$Event$expect,
					$author$project$ResponsibilityGraphTest$GraphMessage(
						$author$project$Ui$ResponsibilityGraph$Select('GoalNode:g')),
					A2(
						$elm_explorations$test$Test$Html$Event$simulate,
						A2(
							$elm_explorations$test$Test$Html$Event$custom,
							'keydown',
							$elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'key',
										$elm$json$Json$Encode$string(' '))
									]))),
						A2(
							$elm_explorations$test$Test$Html$Query$find,
							_List_fromArray(
								[
									$elm_explorations$test$Test$Html$Selector$tag('g'),
									$elm_explorations$test$Test$Html$Selector$attribute(
									A2($elm$html$Html$Attributes$attribute, 'aria-label', '고객 성장 · 권한 부족'))
								]),
							A2($author$project$ResponsibilityGraphTest$render, $author$project$Ui$ResponsibilityGraph$init, $author$project$GraphFixture$sample))));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'상세에서 기존 책임자 폼으로 이동한다',
			function (_v20) {
				return A2(
					$elm_explorations$test$Test$Html$Event$expect,
					$author$project$ResponsibilityGraphTest$Go('owner-g'),
					A2(
						$elm_explorations$test$Test$Html$Event$simulate,
						$elm_explorations$test$Test$Html$Event$click,
						A2(
							$elm_explorations$test$Test$Html$Query$find,
							_List_fromArray(
								[
									$elm_explorations$test$Test$Html$Selector$tag('button'),
									$elm_explorations$test$Test$Html$Selector$containing(
									_List_fromArray(
										[
											$elm_explorations$test$Test$Html$Selector$text('책임자 지정 폼으로 이동')
										]))
								]),
							A2(
								$author$project$ResponsibilityGraphTest$render,
								$author$project$ResponsibilityGraphTest$selected('GoalNode:g'),
								$author$project$GraphFixture$sample))));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'검색은 다이어그램 노드를 제거하지 않아 연결 맥락을 유지한다',
			function (_v21) {
				return A2(
					$elm_explorations$test$Test$Html$Query$count,
					$elm_explorations$test$Expect$equal(5),
					A2(
						$elm_explorations$test$Test$Html$Query$findAll,
						_List_fromArray(
							[
								$elm_explorations$test$Test$Html$Selector$tag('g'),
								$elm_explorations$test$Test$Html$Selector$attribute(
								A2($elm$html$Html$Attributes$attribute, 'role', 'button'))
							]),
						A2(
							$author$project$ResponsibilityGraphTest$render,
							A2(
								$author$project$Ui$ResponsibilityGraph$update,
								$author$project$Ui$ResponsibilityGraph$Search('없는 검색어'),
								$author$project$Ui$ResponsibilityGraph$init),
							$author$project$GraphFixture$sample)));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'검색 일치가 없는 관계 목록은 빈 상태를 안내한다',
			function (_v22) {
				return A2(
					$elm_explorations$test$Test$Html$Query$has,
					_List_fromArray(
						[
							$elm_explorations$test$Test$Html$Selector$text('검색 조건에 맞는 관계가 없습니다.')
						]),
					A2(
						$author$project$ResponsibilityGraphTest$render,
						A2(
							$author$project$Ui$ResponsibilityGraph$update,
							$author$project$Ui$ResponsibilityGraph$SetDiagram(false),
							A2(
								$author$project$Ui$ResponsibilityGraph$update,
								$author$project$Ui$ResponsibilityGraph$Search('없는 검색어'),
								$author$project$Ui$ResponsibilityGraph$init)),
						$author$project$GraphFixture$sample));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'빈 조직도 안내와 보기 도구를 렌더링한다',
			function (_v23) {
				return A2(
					$elm_explorations$test$Test$Html$Query$has,
					_List_fromArray(
						[
							$elm_explorations$test$Test$Html$Selector$text('아직 구성원과 목표가 없습니다. 구성원이나 목표를 추가하면 관계를 확인할 수 있습니다.')
						]),
					A2($author$project$ResponsibilityGraphTest$render, $author$project$Ui$ResponsibilityGraph$init, $author$project$GraphFixture$workspace));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'선택 상세에 숨긴 관계와 필요한 예산도 표시한다',
			function (_v24) {
				return A2(
					$elm_explorations$test$Test$Html$Query$has,
					_List_fromArray(
						[
							$elm_explorations$test$Test$Html$Selector$text('하위 목표'),
							$elm_explorations$test$Test$Html$Selector$text('100원'),
							$elm_explorations$test$Test$Html$Selector$text('권한 통제율 50%')
						]),
					A2(
						$author$project$ResponsibilityGraphTest$render,
						$author$project$ResponsibilityGraphTest$selected('GoalNode:g'),
						$author$project$GraphFixture$sample));
			})
		]));
var $author$project$App$Update$ConfirmDelete = function (a) {
	return {$: 'ConfirmDelete', a: a};
};
var $author$project$App$Update$GotDiscovery = F3(
	function (a, b, c) {
		return {$: 'GotDiscovery', a: a, b: b, c: c};
	});
var $author$project$App$Update$GotOrganizations = F2(
	function (a, b) {
		return {$: 'GotOrganizations', a: a, b: b};
	});
var $author$project$App$Update$GotWorkspace = F2(
	function (a, b) {
		return {$: 'GotWorkspace', a: a, b: b};
	});
var $author$project$App$Update$OpenDelete = {$: 'OpenDelete'};
var $author$project$App$Update$Refresh = {$: 'Refresh'};
var $author$project$Form$Action$Rename = {$: 'Rename'};
var $author$project$App$Update$ResetPerson = function (a) {
	return {$: 'ResetPerson', a: a};
};
var $author$project$App$Update$SavedDiscovery = F3(
	function (a, b, c) {
		return {$: 'SavedDiscovery', a: a, b: b, c: c};
	});
var $author$project$App$Update$Submit = function (a) {
	return {$: 'Submit', a: a};
};
var $author$project$App$Update$SubmitDiscovery = {$: 'SubmitDiscovery'};
var $author$project$StateTest$bodyField = F3(
	function (decoder, model, action) {
		return A2(
			$elm$core$Result$andThen,
			function (_v0) {
				var body = _v0.c;
				return A2(
					$elm$core$Result$mapError,
					$elm$json$Json$Decode$errorToString,
					A2($elm$json$Json$Decode$decodeValue, decoder, body));
			},
			A2($author$project$App$Update$payload, model, action));
	});
var $author$project$App$Update$get = $author$project$App$Drafts$get;
var $elm_explorations$test$Expect$notEqual = A2($elm_explorations$test$Expect$equateWith, 'Expect.notEqual', $elm$core$Basics$neq);
var $author$project$StateTest$workspace = {
	authorities: _List_Nil,
	compiler: {diagnostics: _List_Nil, errors: 0, warnings: 0},
	decisionShare: $elm$core$Dict$empty,
	demo: false,
	edges: _List_Nil,
	events: _List_Nil,
	goals: _List_Nil,
	organization: {createdAt: '2026-01-01T00:00:00Z', id: 'org-a', name: 'Alpha'},
	people: _List_Nil,
	reviewWarnings: _List_Nil,
	reviews: _List_Nil,
	version: 4
};
var $author$project$StateTest$ready = function () {
	var initial = $author$project$App$Update$init(
		{deadline: '2026-12-31', seed: 'test', today: '2026-01-01'}).a;
	return A2(
		$author$project$AppFixture$mapPage,
		function (p) {
			return _Utils_update(
				p,
				{page: $author$project$Page$Dashboard});
		},
		A2(
			$author$project$AppFixture$mapSession,
			function (s) {
				return _Utils_update(
					s,
					{
						fresh: true,
						org: $elm$core$Maybe$Just('org-a'),
						syncing: false,
						workspace: $author$project$Remote$Loaded($author$project$StateTest$workspace)
					});
			},
			initial));
}();
var $author$project$StateTest$step = F2(
	function (msg, model) {
		return A2($author$project$App$Update$update, msg, model).a;
	});
var $author$project$StateTest$tests = A2(
	$elm_explorations$test$Test$describe,
	'Application state safety',
	_List_fromArray(
		[
			A2(
			$elm_explorations$test$Test$test,
			'stale responses and duplicate or blocked actions produce no effects',
			function (_v0) {
				var stale = A2($author$project$StateTest$step, $author$project$App$Update$Refresh, $author$project$StateTest$ready);
				var saving = A2(
					$author$project$StateTest$step,
					$author$project$App$Update$Submit($author$project$Form$Action$ImportDemo),
					$author$project$StateTest$ready);
				return A2(
					$elm_explorations$test$Expect$all,
					_List_fromArray(
						[
							function (_v1) {
							return A2(
								$elm_explorations$test$Expect$equal,
								_List_fromArray(
									[_List_Nil, _List_Nil, _List_Nil, _List_Nil, _List_Nil]),
								A2(
									$elm$core$List$map,
									function (msg) {
										return A2($author$project$App$Update$update, msg, $author$project$StateTest$ready).b;
									},
									_List_fromArray(
										[
											A2(
											$author$project$App$Update$GotWorkspace,
											0,
											$elm$core$Result$Ok($author$project$StateTest$workspace)),
											A2(
											$author$project$App$Update$GotWorkspace,
											0,
											$elm$core$Result$Err('offline')),
											A2(
											$author$project$App$Update$GotOrganizations,
											0,
											$elm$core$Result$Ok(_List_Nil)),
											A3(
											$author$project$App$Update$Saved,
											0,
											$author$project$Form$Action$ImportDemo,
											$elm$core$Result$Ok(_Utils_Tuple0)),
											A3(
											$author$project$App$Update$Saved,
											0,
											$author$project$Form$Action$ImportDemo,
											$elm$core$Result$Err('conflict'))
										])));
						},
							function (_v2) {
							return A2(
								$elm_explorations$test$Expect$equal,
								_List_fromArray(
									[_List_Nil, _List_Nil, _List_Nil, _List_Nil, _List_Nil, _List_Nil]),
								A2(
									$elm$core$List$map,
									function (msg) {
										return A2($author$project$App$Update$update, msg, saving).b;
									},
									_List_fromArray(
										[
											$author$project$App$Update$Submit($author$project$Form$Action$ImportDemo),
											A2($author$project$App$Update$Navigate, $author$project$Page$Organizations, $elm$core$Maybe$Nothing),
											$author$project$App$Update$Refresh,
											A2($author$project$App$Update$Guide, $author$project$Page$Reviews, 'review-form'),
											$author$project$App$Update$OpenPerson('p'),
											$author$project$App$Update$OpenDelete
										])));
						},
							function (_v3) {
							return A2(
								$elm_explorations$test$Expect$equal,
								_List_Nil,
								A2(
									$author$project$App$Update$update,
									$author$project$App$Update$Submit($author$project$Form$Action$ImportDemo),
									stale).b);
						}
						]),
					_Utils_Tuple0);
			}),
			A2(
			$elm_explorations$test$Test$test,
			'a valid submit emits exactly one scoped write with captured request and payload',
			function (_v4) {
				var drafted = A2(
					$author$project$StateTest$step,
					A3($author$project$App$Update$Edit, $author$project$Form$Action$AddPerson, 'role', 'Engineer'),
					A2(
						$author$project$StateTest$step,
						A3($author$project$App$Update$Edit, $author$project$Form$Action$AddPerson, 'name', 'Alice'),
						$author$project$StateTest$ready));
				var _v5 = A2(
					$author$project$App$Update$update,
					$author$project$App$Update$Submit($author$project$Form$Action$AddPerson),
					drafted);
				var saving = _v5.a;
				var effects = _v5.b;
				if ((effects.b && (effects.a.$ === 'SaveCommand')) && (!effects.b.b)) {
					var _v7 = effects.a;
					var token = _v7.a;
					var action = _v7.b;
					var method = _v7.c;
					var path = _v7.d;
					var body = _v7.e;
					return A2(
						$elm_explorations$test$Expect$all,
						_List_fromArray(
							[
								function (_v8) {
								return A2(
									$elm_explorations$test$Expect$equal,
									_Utils_Tuple2($author$project$StateTest$ready.session.request, $author$project$Form$Action$AddPerson),
									_Utils_Tuple2(token, action));
							},
								function (_v9) {
								return A2(
									$elm_explorations$test$Expect$equal,
									_Utils_Tuple2('POST', '/api/organizations/org-a/people'),
									_Utils_Tuple2(method, path));
							},
								function (_v10) {
								return A2(
									$elm_explorations$test$Expect$equal,
									$elm$core$Result$Ok(
										_Utils_Tuple3('person-test-0', 'Alice', 'Engineer')),
									A2(
										$elm$json$Json$Decode$decodeValue,
										A4(
											$elm$json$Json$Decode$map3,
											F3(
												function (id, name, role) {
													return _Utils_Tuple3(id, name, role);
												}),
											A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string),
											A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string),
											A2($elm$json$Json$Decode$field, 'role', $elm$json$Json$Decode$string)),
										body));
							},
								function (_v11) {
								return A2(
									$elm_explorations$test$Expect$equal,
									_Utils_Tuple2(
										$author$project$App$Session$Saving('person'),
										1),
									_Utils_Tuple2(saving.session.saving, saving.forms.serial));
							}
							]),
						_Utils_Tuple0);
				} else {
					return $elm_explorations$test$Expect$fail('Expected exactly one SaveCommand');
				}
			}),
			A2(
			$elm_explorations$test$Test$test,
			'failed writes preserve drafts and emit only one read without retrying',
			function (_v12) {
				var drafted = A2(
					$author$project$StateTest$step,
					A3($author$project$App$Update$Edit, $author$project$Form$Action$AddPerson, 'role', 'Engineer'),
					A2(
						$author$project$StateTest$step,
						A3($author$project$App$Update$Edit, $author$project$Form$Action$AddPerson, 'name', 'Alice'),
						$author$project$StateTest$ready));
				var saving = A2(
					$author$project$StateTest$step,
					$author$project$App$Update$Submit($author$project$Form$Action$AddPerson),
					drafted);
				var _v13 = A2(
					$author$project$App$Update$update,
					A3(
						$author$project$App$Update$Saved,
						saving.session.request,
						$author$project$Form$Action$AddPerson,
						$elm$core$Result$Err('conflict')),
					saving);
				var failed = _v13.a;
				var effects = _v13.b;
				return A2(
					$elm_explorations$test$Expect$equal,
					_Utils_Tuple3(
						'Alice',
						_List_fromArray(
							[
								A2($author$project$App$Effect$LoadWorkspace, saving.session.request + 1, 'org-a'),
								A2($author$project$App$Effect$LoadDiscovery, saving.session.request + 1, 'org-a'),
								A2($author$project$App$Effect$LoadAgents, saving.session.request + 1, 'org-a')
							]),
						$author$project$App$Session$Idle),
					_Utils_Tuple3(
						A3($author$project$App$Update$get, failed, $author$project$Form$Action$AddPerson, 'name'),
						effects,
						failed.session.saving));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'organization navigation emits the new scope and generation while same scope stays local',
			function (_v14) {
				var _v15 = A2(
					$author$project$App$Update$update,
					A2(
						$author$project$App$Update$Navigate,
						$author$project$Page$Dashboard,
						$elm$core$Maybe$Just('org-b')),
					$author$project$StateTest$ready);
				var changed = _v15.a;
				var effects = _v15.b;
				return A2(
					$elm_explorations$test$Expect$all,
					_List_fromArray(
						[
							function (_v16) {
							return A2(
								$elm_explorations$test$Expect$equal,
								_Utils_Tuple3(
									$elm$core$Maybe$Just('org-b'),
									$author$project$StateTest$ready.session.request + 1,
									_List_fromArray(
										[
											A2($author$project$App$Effect$LoadWorkspace, $author$project$StateTest$ready.session.request + 1, 'org-b'),
											A2($author$project$App$Effect$LoadDiscovery, $author$project$StateTest$ready.session.request + 1, 'org-b'),
											A2($author$project$App$Effect$LoadAgents, $author$project$StateTest$ready.session.request + 1, 'org-b')
										])),
								_Utils_Tuple3(changed.session.org, changed.session.request, effects));
						},
							function (_v17) {
							return A2(
								$elm_explorations$test$Expect$equal,
								_List_Nil,
								A2(
									$author$project$App$Update$update,
									A2(
										$author$project$App$Update$Navigate,
										$author$project$Page$Reviews,
										$elm$core$Maybe$Just('org-a')),
									$author$project$StateTest$ready).b);
						},
							function (_v18) {
							return A2(
								$elm_explorations$test$Expect$equal,
								_List_fromArray(
									[
										$author$project$App$Effect$LoadOrganizations(changed.session.request + 1)
									]),
								A2(
									$author$project$App$Update$update,
									A2($author$project$App$Update$Navigate, $author$project$Page$Organizations, $elm$core$Maybe$Nothing),
									changed).b);
						},
							function (_v19) {
							return A2(
								$elm_explorations$test$Expect$equal,
								_List_fromArray(
									[
										$author$project$App$Effect$LoadOrganizations(1)
									]),
								$author$project$App$Update$init(
									{deadline: '2026-12-31', seed: 'test', today: '2026-01-01'}).b);
						}
						]),
					_Utils_Tuple0);
			}),
			A2(
			$elm_explorations$test$Test$test,
			'guides, person detail and delete confirmation emit their actual focus targets',
			function (_v20) {
				return A2(
					$elm_explorations$test$Expect$all,
					_List_fromArray(
						[
							function (_v21) {
							return A2(
								$elm_explorations$test$Expect$equal,
								_List_fromArray(
									[
										$author$project$App$Effect$FocusElement('review-form')
									]),
								A2(
									$author$project$App$Update$update,
									A2($author$project$App$Update$Guide, $author$project$Page$Reviews, 'review-form'),
									$author$project$StateTest$ready).b);
						},
							function (_v22) {
							return A2(
								$elm_explorations$test$Expect$equal,
								_List_fromArray(
									[
										$author$project$App$Effect$FocusElement('person-detail')
									]),
								A2(
									$author$project$App$Update$update,
									$author$project$App$Update$OpenPerson('p'),
									$author$project$StateTest$ready).b);
						},
							function (_v23) {
							return A2(
								$elm_explorations$test$Expect$equal,
								_List_fromArray(
									[
										$author$project$App$Effect$FocusElement('delete-confirm')
									]),
								A2($author$project$App$Update$update, $author$project$App$Update$OpenDelete, $author$project$StateTest$ready).b);
						}
						]),
					_Utils_Tuple0);
			}),
			A2(
			$elm_explorations$test$Test$test,
			'outdated workspace success and failure cannot replace current state',
			function (_v24) {
				return A2(
					$elm_explorations$test$Expect$equal,
					$author$project$StateTest$ready,
					A2(
						$author$project$StateTest$step,
						A2(
							$author$project$App$Update$GotWorkspace,
							0,
							$elm$core$Result$Err('network failure')),
						A2(
							$author$project$StateTest$step,
							A2(
								$author$project$App$Update$GotWorkspace,
								0,
								$elm$core$Result$Ok(
									_Utils_update(
										$author$project$StateTest$workspace,
										{version: 99}))),
							$author$project$StateTest$ready)));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'outdated organization list is ignored',
			function (_v25) {
				return A2(
					$elm_explorations$test$Expect$equal,
					$author$project$StateTest$ready,
					A2(
						$author$project$StateTest$step,
						A2(
							$author$project$App$Update$GotOrganizations,
							0,
							$elm$core$Result$Ok(_List_Nil)),
						$author$project$StateTest$ready));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'current workspace response marks data fresh',
			function (_v26) {
				return A2(
					$elm_explorations$test$Expect$equal,
					_Utils_Tuple3(
						true,
						false,
						$author$project$Remote$Loaded(
							_Utils_update(
								$author$project$StateTest$workspace,
								{version: 5}))),
					function (m) {
						return _Utils_Tuple3(m.session.fresh, m.session.syncing, m.session.workspace);
					}(
						A2(
							$author$project$StateTest$step,
							A2(
								$author$project$App$Update$GotWorkspace,
								2,
								$elm$core$Result$Ok(
									_Utils_update(
										$author$project$StateTest$workspace,
										{version: 5}))),
							A2($author$project$StateTest$step, $author$project$App$Update$Refresh, $author$project$StateTest$ready))));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'mutation blocks duplicate submit, edit, refresh and navigation',
			function (_v27) {
				var saving = A2(
					$author$project$StateTest$step,
					$author$project$App$Update$Submit($author$project$Form$Action$ImportDemo),
					$author$project$StateTest$ready);
				return A2(
					$elm_explorations$test$Expect$equal,
					saving,
					A2(
						$author$project$StateTest$step,
						A3($author$project$App$Update$Edit, $author$project$Form$Action$AddPerson, 'name', 'changed'),
						A2(
							$author$project$StateTest$step,
							$author$project$App$Update$Refresh,
							A2(
								$author$project$StateTest$step,
								A2($author$project$App$Update$Navigate, $author$project$Page$Organizations, $elm$core$Maybe$Nothing),
								A2(
									$author$project$StateTest$step,
									$author$project$App$Update$Submit($author$project$Form$Action$ImportDemo),
									saving)))));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'stale state refuses a new save',
			function (_v28) {
				return A2(
					$elm_explorations$test$Expect$equal,
					_Utils_Tuple3($author$project$App$Session$Idle, true, 0),
					function (m) {
						return _Utils_Tuple3(m.session.saving, m.error, m.forms.serial);
					}(
						A2(
							$author$project$StateTest$step,
							$author$project$App$Update$Submit($author$project$Form$Action$ImportDemo),
							A2($author$project$StateTest$step, $author$project$App$Update$Refresh, $author$project$StateTest$ready))));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'drafts survive organization navigation independently',
			function (_v29) {
				return A2(
					$elm_explorations$test$Expect$equal,
					'Alice',
					function (m) {
						return A3($author$project$App$Update$get, m, $author$project$Form$Action$AddPerson, 'name');
					}(
						A2(
							$author$project$StateTest$step,
							A2(
								$author$project$App$Update$Navigate,
								$author$project$Page$Dashboard,
								$elm$core$Maybe$Just('org-a')),
							A2(
								$author$project$StateTest$step,
								A3($author$project$App$Update$Edit, $author$project$Form$Action$AddPerson, 'name', 'Bob'),
								A2(
									$author$project$StateTest$step,
									A2(
										$author$project$App$Update$Navigate,
										$author$project$Page$Dashboard,
										$elm$core$Maybe$Just('org-b')),
									A2(
										$author$project$StateTest$step,
										A3($author$project$App$Update$Edit, $author$project$Form$Action$AddPerson, 'name', 'Alice'),
										$author$project$StateTest$ready))))));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'failed save preserves draft and metric ID while refreshing',
			function (_v30) {
				var drafted = A2(
					$author$project$StateTest$step,
					A2($author$project$App$Update$EditGoal, $author$project$Form$Goal$Description, 'Revenue'),
					$author$project$StateTest$ready);
				var failed = A2(
					$author$project$StateTest$step,
					A3(
						$author$project$App$Update$Saved,
						$author$project$StateTest$ready.session.request,
						$author$project$Form$Action$AddGoal,
						$elm$core$Result$Err('conflict')),
					drafted);
				return A2(
					$elm_explorations$test$Expect$equal,
					_Utils_Tuple2(
						_Utils_Tuple2(
							'Revenue',
							A3($author$project$App$Update$get, drafted, $author$project$Form$Action$AddGoal, 'metricId')),
						_Utils_Tuple3($author$project$App$Session$Idle, false, true)),
					_Utils_Tuple2(
						_Utils_Tuple2(
							A3($author$project$App$Update$get, failed, $author$project$Form$Action$AddGoal, 'description'),
							A3($author$project$App$Update$get, failed, $author$project$Form$Action$AddGoal, 'metricId')),
						_Utils_Tuple3(failed.session.saving, failed.session.fresh, failed.error)));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'rename refuses a version changed since last edit',
			function (_v31) {
				return $elm_explorations$test$Expect$err(
					function (m) {
						return A2($author$project$App$Update$payload, m, $author$project$Form$Action$Rename);
					}(
						A2(
							$author$project$StateTest$step,
							A2(
								$author$project$App$Update$GotWorkspace,
								$author$project$StateTest$ready.session.request,
								$elm$core$Result$Ok(
									_Utils_update(
										$author$project$StateTest$workspace,
										{version: 5}))),
							A2(
								$author$project$StateTest$step,
								A3($author$project$App$Update$Edit, $author$project$Form$Action$Rename, 'name', 'New'),
								$author$project$StateTest$ready))));
			}),
			A2(
			$elm_explorations$test$Test$test,
			're-editing rename captures current version',
			function (_v32) {
				return A2(
					$elm_explorations$test$Expect$equal,
					$elm$core$Result$Ok(5),
					function (m) {
						return A3(
							$author$project$StateTest$bodyField,
							A2($elm$json$Json$Decode$field, 'expectedVersion', $elm$json$Json$Decode$int),
							m,
							$author$project$Form$Action$Rename);
					}(
						A2(
							$author$project$StateTest$step,
							A3($author$project$App$Update$Edit, $author$project$Form$Action$Rename, 'name', 'Newer'),
							A2(
								$author$project$StateTest$step,
								A2(
									$author$project$App$Update$GotWorkspace,
									$author$project$StateTest$ready.session.request,
									$elm$core$Result$Ok(
										_Utils_update(
											$author$project$StateTest$workspace,
											{version: 5}))),
								A2(
									$author$project$StateTest$step,
									A3($author$project$App$Update$Edit, $author$project$Form$Action$Rename, 'name', 'New'),
									$author$project$StateTest$ready)))));
			}),
			A2(
			$elm_explorations$test$Test$test,
			're-editing a refreshed person draft preserves the original version and refuses stale save',
			function (_v33) {
				var w = _Utils_update(
					$author$project$StateTest$workspace,
					{
						people: _List_fromArray(
							[
								{active: true, department: $elm$core$Maybe$Nothing, email: $elm$core$Maybe$Nothing, id: 'p', name: 'Original', reportsTo: $elm$core$Maybe$Nothing, role: 'Original role'}
							])
					});
				var m = A2(
					$author$project$AppFixture$mapSession,
					function (s) {
						return _Utils_update(
							s,
							{
								workspace: $author$project$Remote$Loaded(w)
							});
					},
					$author$project$StateTest$ready);
				var freshWorkspace = _Utils_update(
					w,
					{
						people: _List_fromArray(
							[
								{active: true, department: $elm$core$Maybe$Nothing, email: $elm$core$Maybe$Nothing, id: 'p', name: 'Original', reportsTo: $elm$core$Maybe$Nothing, role: 'Server changed role'}
							]),
						version: 5
					});
				return $elm_explorations$test$Expect$err(
					function (state) {
						return A2(
							$author$project$App$Update$payload,
							state,
							$author$project$Form$Action$UpdatePerson('p'));
					}(
						A2(
							$author$project$StateTest$step,
							A3(
								$author$project$App$Update$Edit,
								$author$project$Form$Action$UpdatePerson('p'),
								'name',
								'More edits'),
							A2(
								$author$project$StateTest$step,
								A2(
									$author$project$App$Update$GotWorkspace,
									m.session.request,
									$elm$core$Result$Ok(freshWorkspace)),
								A2(
									$author$project$StateTest$step,
									A3(
										$author$project$App$Update$Edit,
										$author$project$Form$Action$UpdatePerson('p'),
										'name',
										'Draft'),
									m)))));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'explicit person draft reset adopts current server fields and version',
			function (_v34) {
				var w = _Utils_update(
					$author$project$StateTest$workspace,
					{
						people: _List_fromArray(
							[
								{active: true, department: $elm$core$Maybe$Nothing, email: $elm$core$Maybe$Nothing, id: 'p', name: 'Server', reportsTo: $elm$core$Maybe$Nothing, role: 'Server role'}
							]),
						version: 5
					});
				var drafted = A2(
					$author$project$StateTest$step,
					A3(
						$author$project$App$Update$Edit,
						$author$project$Form$Action$DeactivatePerson('p'),
						'successor',
						'other'),
					A2(
						$author$project$StateTest$step,
						A3(
							$author$project$App$Update$Edit,
							$author$project$Form$Action$UpdatePerson('p'),
							'name',
							'Draft'),
						$author$project$StateTest$ready));
				var reset = A2(
					$author$project$StateTest$step,
					$author$project$App$Update$ResetPerson('p'),
					drafted);
				var loaded = A2(
					$author$project$StateTest$step,
					A3(
						$author$project$App$Update$Edit,
						$author$project$Form$Action$UpdatePerson('p'),
						'name',
						'Fresh edit'),
					A2(
						$author$project$StateTest$step,
						A2(
							$author$project$App$Update$GotWorkspace,
							reset.session.request,
							$elm$core$Result$Ok(w)),
						reset));
				return A2(
					$elm_explorations$test$Expect$equal,
					_Utils_Tuple3(
						$elm$core$Result$Ok(5),
						'Server role',
						''),
					_Utils_Tuple3(
						A3(
							$author$project$StateTest$bodyField,
							A2($elm$json$Json$Decode$field, 'expectedVersion', $elm$json$Json$Decode$int),
							loaded,
							$author$project$Form$Action$UpdatePerson('p')),
						A3(
							$author$project$App$Update$get,
							loaded,
							$author$project$Form$Action$UpdatePerson('p'),
							'role'),
						A3(
							$author$project$App$Update$get,
							loaded,
							$author$project$Form$Action$DeactivatePerson('p'),
							'successor')));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'delete requires exact confirmation and captures the displayed version',
			function (_v35) {
				var opened = A2($author$project$StateTest$step, $author$project$App$Update$OpenDelete, $author$project$StateTest$ready);
				var confirmed = A2(
					$author$project$StateTest$step,
					$author$project$App$Update$ConfirmDelete('Alpha'),
					opened);
				return A2(
					$elm_explorations$test$Expect$all,
					_List_fromArray(
						[
							function (_v36) {
							return $elm_explorations$test$Expect$err(
								A2(
									$author$project$App$Update$payload,
									A2(
										$author$project$StateTest$step,
										$author$project$App$Update$ConfirmDelete('alpha'),
										opened),
									$author$project$Form$Action$DeleteOrg));
						},
							function (_v37) {
							return A2(
								$elm_explorations$test$Expect$equal,
								$elm$core$Result$Ok(
									_Utils_Tuple2('Alpha', 4)),
								A3(
									$author$project$StateTest$bodyField,
									A3(
										$elm$json$Json$Decode$map2,
										$elm$core$Tuple$pair,
										A2($elm$json$Json$Decode$field, 'confirmName', $elm$json$Json$Decode$string),
										A2($elm$json$Json$Decode$field, 'expectedVersion', $elm$json$Json$Decode$int)),
									confirmed,
									$author$project$Form$Action$DeleteOrg));
						},
							function (_v38) {
							return $elm_explorations$test$Expect$err(
								A2(
									$author$project$App$Update$payload,
									A2(
										$author$project$AppFixture$mapSession,
										function (s) {
											return _Utils_update(
												s,
												{
													org: $elm$core$Maybe$Just('org-b')
												});
										},
										confirmed),
									$author$project$Form$Action$DeleteOrg));
						}
						]),
					_Utils_Tuple0);
			}),
			A2(
			$elm_explorations$test$Test$test,
			'metric ID changes only after successful goal creation',
			function (_v39) {
				var drafted = A2(
					$author$project$StateTest$step,
					A2($author$project$App$Update$EditGoal, $author$project$Form$Goal$Description, 'Revenue'),
					$author$project$StateTest$ready);
				var unrelated = A2(
					$author$project$StateTest$step,
					A3(
						$author$project$App$Update$Saved,
						$author$project$StateTest$ready.session.request,
						$author$project$Form$Action$AddPerson,
						$elm$core$Result$Ok(_Utils_Tuple0)),
					drafted);
				var created = A2(
					$author$project$StateTest$step,
					A3(
						$author$project$App$Update$Saved,
						$author$project$StateTest$ready.session.request,
						$author$project$Form$Action$AddGoal,
						$elm$core$Result$Ok(_Utils_Tuple0)),
					drafted);
				return A2(
					$elm_explorations$test$Expect$equal,
					_Utils_Tuple2('metric-test-0', 'metric-test-1'),
					_Utils_Tuple2(
						A3($author$project$App$Update$get, unrelated, $author$project$Form$Action$AddGoal, 'metricId'),
						A3($author$project$App$Update$get, created, $author$project$Form$Action$AddGoal, 'metricId')));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'guide preselects goal without losing review draft',
			function (_v40) {
				return A2(
					$elm_explorations$test$Expect$equal,
					_Utils_Tuple3('demo-revenue', 'Keep reflection', 'Keep learning'),
					function (m) {
						return _Utils_Tuple3(
							A3($author$project$App$Update$get, m, $author$project$Form$Action$AddReview, 'goal'),
							A3($author$project$App$Update$get, m, $author$project$Form$Action$AddReview, 'note'),
							A3($author$project$App$Update$get, m, $author$project$Form$Action$AddReview, 'learning'));
					}(
						A2(
							$author$project$StateTest$step,
							A2($author$project$App$Update$Guide, $author$project$Page$Reviews, 'review-form'),
							A2(
								$author$project$StateTest$step,
								A2($author$project$App$Update$EditReview, $author$project$Form$Review$Learning, 'Keep learning'),
								A2(
									$author$project$StateTest$step,
									A2($author$project$App$Update$EditReview, $author$project$Form$Review$Note, 'Keep reflection'),
									$author$project$StateTest$ready)))));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'guide preserves an explicitly selected review goal',
			function (_v41) {
				return A2(
					$elm_explorations$test$Expect$equal,
					'other-goal',
					function (m) {
						return A3($author$project$App$Update$get, m, $author$project$Form$Action$AddReview, 'goal');
					}(
						A2(
							$author$project$StateTest$step,
							A2($author$project$App$Update$Guide, $author$project$Page$Reviews, 'review-form'),
							A2(
								$author$project$StateTest$step,
								A2($author$project$App$Update$EditReview, $author$project$Form$Review$Goal, 'other-goal'),
								$author$project$StateTest$ready))));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'review decision requires an owner',
			function (_v42) {
				return $elm_explorations$test$Expect$err(
					function (m) {
						return A2($author$project$App$Update$payload, m, $author$project$Form$Action$AddReview);
					}(
						A2(
							$author$project$StateTest$step,
							A2($author$project$App$Update$EditReview, $author$project$Form$Review$Decision, 'change'),
							A2(
								$author$project$StateTest$step,
								A2($author$project$App$Update$EditReview, $author$project$Form$Review$Note, 'review'),
								A2(
									$author$project$StateTest$step,
									A2($author$project$App$Update$EditReview, $author$project$Form$Review$Goal, 'g'),
									$author$project$StateTest$ready)))));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'discovery ignores another organization and an older request generation',
			function (_v43) {
				var snapshot = {discovery: $author$project$Domain$Discovery$empty, version: 9};
				return A2(
					$elm_explorations$test$Expect$equal,
					$author$project$StateTest$ready,
					A2(
						$author$project$StateTest$step,
						A3(
							$author$project$App$Update$GotDiscovery,
							$author$project$StateTest$ready.session.request,
							'org-b',
							$elm$core$Result$Ok(snapshot)),
						A2(
							$author$project$StateTest$step,
							A3(
								$author$project$App$Update$GotDiscovery,
								0,
								'org-a',
								$elm$core$Result$Ok(snapshot)),
							$author$project$StateTest$ready)));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'discovery saves using its own response version rather than dashboard version',
			function (_v44) {
				var loaded = A2(
					$author$project$StateTest$step,
					A3(
						$author$project$App$Update$GotDiscovery,
						$author$project$StateTest$ready.session.request,
						'org-a',
						$elm$core$Result$Ok(
							{discovery: $author$project$Domain$Discovery$empty, version: 11})),
					$author$project$StateTest$ready);
				var edited = A2(
					$author$project$StateTest$step,
					$author$project$App$Update$EditDiscovery(
						$author$project$Domain$Discovery$Scope('Investigation')),
					loaded);
				var _v45 = A2($author$project$App$Update$update, $author$project$App$Update$SubmitDiscovery, edited).b;
				if (((_v45.b && (_v45.a.$ === 'SaveDiscovery')) && (_v45.a.b === 'org-a')) && (!_v45.b.b)) {
					var _v46 = _v45.a;
					var snapshot = _v46.c;
					return A2(
						$elm_explorations$test$Expect$equal,
						_Utils_Tuple2(11, 'Investigation'),
						_Utils_Tuple2(snapshot.version, snapshot.discovery.scope));
				} else {
					return $elm_explorations$test$Expect$fail('Expected a scoped discovery save');
				}
			}),
			A2(
			$elm_explorations$test$Test$test,
			'successful save blocks discovery edits until the saved document is reloaded',
			function (_v47) {
				var loaded = A2(
					$author$project$StateTest$step,
					A3(
						$author$project$App$Update$GotDiscovery,
						$author$project$StateTest$ready.session.request,
						'org-a',
						$elm$core$Result$Ok(
							{discovery: $author$project$Domain$Discovery$empty, version: 11})),
					$author$project$StateTest$ready);
				var saved = A2(
					$author$project$StateTest$step,
					A3(
						$author$project$App$Update$SavedDiscovery,
						loaded.session.request,
						'org-a',
						$elm$core$Result$Ok(_Utils_Tuple0)),
					loaded);
				var edited = A2(
					$author$project$StateTest$step,
					$author$project$App$Update$EditDiscovery(
						$author$project$Domain$Discovery$Scope('Stale edit')),
					saved);
				return A2($elm_explorations$test$Expect$equal, saved, edited);
			}),
			A2(
			$elm_explorations$test$Test$test,
			'organization deletion removes both saved discovery and local drafts',
			function (_v48) {
				var loaded = A2(
					$author$project$StateTest$step,
					$author$project$App$Update$EditDiscovery(
						$author$project$Domain$Discovery$Scope('Draft')),
					A2(
						$author$project$StateTest$step,
						A3(
							$author$project$App$Update$GotDiscovery,
							$author$project$StateTest$ready.session.request,
							'org-a',
							$elm$core$Result$Ok(
								{discovery: $author$project$Domain$Discovery$empty, version: 11})),
						$author$project$StateTest$ready));
				var deleted = A2(
					$author$project$StateTest$step,
					A3(
						$author$project$App$Update$Saved,
						loaded.session.request,
						$author$project$Form$Action$DeleteOrg,
						$elm$core$Result$Ok(_Utils_Tuple0)),
					loaded);
				return A2(
					$elm_explorations$test$Expect$equal,
					_Utils_Tuple2($elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing),
					_Utils_Tuple2(
						A2($author$project$App$Discovery$current, 'org-a', deleted.discovery),
						A2($author$project$App$Discovery$saved, 'org-a', deleted.discovery)));
			}),
			A2(
			$elm_explorations$test$Test$test,
			'choosing a shared metric copies its definition and choosing new creates a distinct identity',
			function (_v49) {
				var loaded = A2(
					$author$project$AppFixture$mapSession,
					function (s) {
						return _Utils_update(
							s,
							{
								workspace: $author$project$Remote$Loaded(
									_Utils_update(
										$author$project$StateTest$workspace,
										{
											goals: _List_fromArray(
												[$author$project$GraphFixture$goal])
										}))
							});
					},
					$author$project$StateTest$ready);
				var selected = A2(
					$author$project$StateTest$step,
					A2($author$project$App$Update$EditGoal, $author$project$Form$Goal$MetricId, 'm'),
					loaded);
				var fresh = A2(
					$author$project$StateTest$step,
					A2($author$project$App$Update$EditGoal, $author$project$Form$Goal$MetricId, ''),
					selected);
				return A2(
					$elm_explorations$test$Expect$all,
					_List_fromArray(
						[
							function (_v50) {
							return A2(
								$elm_explorations$test$Expect$equal,
								_List_fromArray(
									['m', '신규 고객 수', '명', 'HigherIsBetter']),
								A2(
									$elm$core$List$map,
									A2($author$project$App$Update$get, selected, $author$project$Form$Action$AddGoal),
									_List_fromArray(
										['metricId', 'metricName', 'unit', 'direction'])));
						},
							function (_v51) {
							return A2(
								$elm_explorations$test$Expect$notEqual,
								'm',
								A3($author$project$App$Update$get, fresh, $author$project$Form$Action$AddGoal, 'metricId'));
						},
							function (_v52) {
							return A2(
								$elm_explorations$test$Expect$equal,
								'',
								A3($author$project$App$Update$get, fresh, $author$project$Form$Action$AddGoal, 'metricName'));
						}
						]),
					_Utils_Tuple0);
			})
		]));
var $author$project$Test$Generated$Main$main = A2(
	$author$project$Test$Runner$Node$run,
	{
		globs: _List_Nil,
		paths: _List_fromArray(
			['/Users/a81720/github/mingyuchoo/study-series-for-haskell/Web/my-org/static/tests/ActivityTest.elm', '/Users/a81720/github/mingyuchoo/study-series-for-haskell/Web/my-org/static/tests/AgentTest.elm', '/Users/a81720/github/mingyuchoo/study-series-for-haskell/Web/my-org/static/tests/AppFixture.elm', '/Users/a81720/github/mingyuchoo/study-series-for-haskell/Web/my-org/static/tests/DecoderTest.elm', '/Users/a81720/github/mingyuchoo/study-series-for-haskell/Web/my-org/static/tests/DiscoveryPageTest.elm', '/Users/a81720/github/mingyuchoo/study-series-for-haskell/Web/my-org/static/tests/DiscoveryTest.elm', '/Users/a81720/github/mingyuchoo/study-series-for-haskell/Web/my-org/static/tests/FormTest.elm', '/Users/a81720/github/mingyuchoo/study-series-for-haskell/Web/my-org/static/tests/GraphFixture.elm', '/Users/a81720/github/mingyuchoo/study-series-for-haskell/Web/my-org/static/tests/ListViewTest.elm', '/Users/a81720/github/mingyuchoo/study-series-for-haskell/Web/my-org/static/tests/PageTest.elm', '/Users/a81720/github/mingyuchoo/study-series-for-haskell/Web/my-org/static/tests/PeopleTest.elm', '/Users/a81720/github/mingyuchoo/study-series-for-haskell/Web/my-org/static/tests/ResponsibilityGraphTest.elm', '/Users/a81720/github/mingyuchoo/study-series-for-haskell/Web/my-org/static/tests/StateTest.elm']),
		processes: 2,
		report: $author$project$Test$Reporter$Reporter$ConsoleReport($author$project$Console$Text$Monochrome),
		runs: 100,
		seed: 52605835732413
	},
	_List_fromArray(
		[
			_Utils_Tuple2(
			'ActivityTest',
			_List_fromArray(
				[
					$author$project$Test$Runner$Node$check($author$project$ActivityTest$tests)
				])),
			_Utils_Tuple2(
			'AgentTest',
			_List_fromArray(
				[
					$author$project$Test$Runner$Node$check($author$project$AgentTest$tests)
				])),
			_Utils_Tuple2(
			'AppFixture',
			_List_fromArray(
				[
					$author$project$Test$Runner$Node$check($author$project$AppFixture$mapPage),
					$author$project$Test$Runner$Node$check($author$project$AppFixture$mapSession)
				])),
			_Utils_Tuple2(
			'DecoderTest',
			_List_fromArray(
				[
					$author$project$Test$Runner$Node$check($author$project$DecoderTest$tests)
				])),
			_Utils_Tuple2(
			'DiscoveryPageTest',
			_List_fromArray(
				[
					$author$project$Test$Runner$Node$check($author$project$DiscoveryPageTest$tests)
				])),
			_Utils_Tuple2(
			'DiscoveryTest',
			_List_fromArray(
				[
					$author$project$Test$Runner$Node$check($author$project$DiscoveryTest$tests)
				])),
			_Utils_Tuple2(
			'FormTest',
			_List_fromArray(
				[
					$author$project$Test$Runner$Node$check($author$project$FormTest$tests)
				])),
			_Utils_Tuple2(
			'GraphFixture',
			_List_fromArray(
				[
					$author$project$Test$Runner$Node$check($author$project$GraphFixture$edge),
					$author$project$Test$Runner$Node$check($author$project$GraphFixture$goal),
					$author$project$Test$Runner$Node$check($author$project$GraphFixture$node),
					$author$project$Test$Runner$Node$check($author$project$GraphFixture$sample),
					$author$project$Test$Runner$Node$check($author$project$GraphFixture$workspace)
				])),
			_Utils_Tuple2(
			'ListViewTest',
			_List_fromArray(
				[
					$author$project$Test$Runner$Node$check($author$project$ListViewTest$ready),
					$author$project$Test$Runner$Node$check($author$project$ListViewTest$sample),
					$author$project$Test$Runner$Node$check($author$project$ListViewTest$step),
					$author$project$Test$Runner$Node$check($author$project$ListViewTest$tests)
				])),
			_Utils_Tuple2(
			'PageTest',
			_List_fromArray(
				[
					$author$project$Test$Runner$Node$check($author$project$PageTest$tests)
				])),
			_Utils_Tuple2(
			'PeopleTest',
			_List_fromArray(
				[
					$author$project$Test$Runner$Node$check($author$project$PeopleTest$tests)
				])),
			_Utils_Tuple2(
			'ResponsibilityGraphTest',
			_List_fromArray(
				[
					$author$project$Test$Runner$Node$check($author$project$ResponsibilityGraphTest$tests)
				])),
			_Utils_Tuple2(
			'StateTest',
			_List_fromArray(
				[
					$author$project$Test$Runner$Node$check($author$project$StateTest$tests)
				]))
		]));
_Platform_export({'Test':{'Generated':{'Main':{'init':$author$project$Test$Generated$Main$main($elm$json$Json$Decode$int)(0)}}}});}(this));
return this.Elm;
})({});
var pipeFilename = "/tmp/elm_test-26186.sock";
var net = require('net'),
  client = net.createConnection(pipeFilename);

client.on('error', function (error) {
  console.error(error);
  client.end();
  process.exit(1);
});

client.setEncoding('utf8');
client.setNoDelay(true);

// Run the Elm app.
var app = Elm.Test.Generated.Main.init({ flags: Date.now() });

client.on('data', function (msg) {
  app.ports.elmTestPort__receive.send(JSON.parse(msg));
});

// Use ports for inter-process communication.
app.ports.elmTestPort__send.subscribe(function (msg) {
  // We split incoming messages on the socket on newlines. The gist is that node
  // is rather unpredictable in whether or not a single `write` will result in a
  // single `on('data')` callback. Sometimes it does, sometimes multiple writes
  // result in a single callback and - worst of all - sometimes a single read
  // results in multiple callbacks, each receiving a piece of the data. The
  // horror.
  client.write(msg + '\n');
});