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

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
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

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
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


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
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
	if (region.aN.am === region.a5.am)
	{
		return 'on line ' + region.aN.am;
	}
	return 'on lines ' + region.aN.am + ' through ' + region.a5.am;
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

	/**_UNUSED/
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

	/**/
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

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
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

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


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



var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


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



/**_UNUSED/
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

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

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
		impl.cV,
		impl.d1,
		impl.dR,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
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


function _Platform_export(exports)
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


function _Platform_export_UNUSED(exports)
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

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
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
		? /**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return _VirtualDom_RE_js_html.test(value)
		? /**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlJson(value)
{
	return (typeof _Json_unwrap(value) === 'string' && _VirtualDom_RE_js_html.test(_Json_unwrap(value)))
		? _Json_wrap(
			/**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
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
		c0: func(record.c0),
		aO: record.aO,
		aL: record.aL
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
		var message = !tag ? value : tag < 3 ? value.a : value.c0;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.aO;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.aL) && event.preventDefault(),
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




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.cV,
		impl.d1,
		impl.dR,
		function(sendToApp, initialModel) {
			var view = impl.d2;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.cV,
		impl.d1,
		impl.dR,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.aM && impl.aM(sendToApp)
			var view = impl.d2;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.b9);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.bP) && (_VirtualDom_doc.title = title = doc.bP);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.di;
	var onUrlRequest = impl.dj;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		aM: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.by === next.by
							&& curr.be === next.be
							&& curr.bv.a === next.bv.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		cV: function(flags)
		{
			return A3(impl.cV, flags, _Browser_getUrl(), key);
		},
		d2: impl.d2,
		d1: impl.d1,
		dR: impl.dR
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { cS: 'hidden', ch: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { cS: 'mozHidden', ch: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { cS: 'msHidden', ch: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { cS: 'webkitHidden', ch: 'webkitvisibilitychange' }
		: { cS: 'hidden', ch: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		bF: _Browser_getScene(),
		bS: {
			bX: _Browser_window.pageXOffset,
			bY: _Browser_window.pageYOffset,
			bV: _Browser_doc.documentElement.clientWidth,
			bd: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		bV: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		bd: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			bF: {
				bV: node.scrollWidth,
				bd: node.scrollHeight
			},
			bS: {
				bX: node.scrollLeft,
				bY: node.scrollTop,
				bV: node.clientWidth,
				bd: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			bF: _Browser_getScene(),
			bS: {
				bX: x,
				bY: y,
				bV: _Browser_doc.documentElement.clientWidth,
				bd: _Browser_doc.documentElement.clientHeight
			},
			cD: {
				bX: x + rect.left,
				bY: y + rect.top,
				bV: rect.width,
				bd: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



// SEND REQUEST

var _Http_toTask = F3(function(router, toTask, request)
{
	return _Scheduler_binding(function(callback)
	{
		function done(response) {
			callback(toTask(request.aD.a(response)));
		}

		var xhr = new XMLHttpRequest();
		xhr.addEventListener('error', function() { done($elm$http$Http$NetworkError_); });
		xhr.addEventListener('timeout', function() { done($elm$http$Http$Timeout_); });
		xhr.addEventListener('load', function() { done(_Http_toResponse(request.aD.b, xhr)); });
		$elm$core$Maybe$isJust(request.d_) && _Http_track(router, xhr, request.d_.a);

		try {
			xhr.open(request.c1, request.aP, true);
		} catch (e) {
			return done($elm$http$Http$BadUrl_(request.aP));
		}

		_Http_configureRequest(xhr, request);

		request.b9.a && xhr.setRequestHeader('Content-Type', request.b9.a);
		xhr.send(request.b9.b);

		return function() { xhr.c = true; xhr.abort(); };
	});
});


// CONFIGURE

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.cQ; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}
	xhr.timeout = request.dX.a || 0;
	xhr.responseType = request.aD.d;
	xhr.withCredentials = request.b1;
}


// RESPONSES

function _Http_toResponse(toBody, xhr)
{
	return A2(
		200 <= xhr.status && xhr.status < 300 ? $elm$http$Http$GoodStatus_ : $elm$http$Http$BadStatus_,
		_Http_toMetadata(xhr),
		toBody(xhr.response)
	);
}


// METADATA

function _Http_toMetadata(xhr)
{
	return {
		aP: xhr.responseURL,
		dM: xhr.status,
		dN: xhr.statusText,
		cQ: _Http_parseHeaders(xhr.getAllResponseHeaders())
	};
}


// HEADERS

function _Http_parseHeaders(rawHeaders)
{
	if (!rawHeaders)
	{
		return $elm$core$Dict$empty;
	}

	var headers = $elm$core$Dict$empty;
	var headerPairs = rawHeaders.split('\r\n');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf(': ');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3($elm$core$Dict$update, key, function(oldValue) {
				return $elm$core$Maybe$Just($elm$core$Maybe$isJust(oldValue)
					? value + ', ' + oldValue.a
					: value
				);
			}, headers);
		}
	}
	return headers;
}


// EXPECT

var _Http_expect = F3(function(type, toBody, toValue)
{
	return {
		$: 0,
		d: type,
		b: toBody,
		a: toValue
	};
});

var _Http_mapExpect = F2(function(func, expect)
{
	return {
		$: 0,
		d: expect.d,
		b: expect.b,
		a: function(x) { return func(expect.a(x)); }
	};
});

function _Http_toDataView(arrayBuffer)
{
	return new DataView(arrayBuffer);
}


// BODY and PARTS

var _Http_emptyBody = { $: 0 };
var _Http_pair = F2(function(a, b) { return { $: 0, a: a, b: b }; });

function _Http_toFormData(parts)
{
	for (var formData = new FormData(); parts.b; parts = parts.b) // WHILE_CONS
	{
		var part = parts.a;
		formData.append(part.a, part.b);
	}
	return formData;
}

var _Http_bytesToBlob = F2(function(mime, bytes)
{
	return new Blob([bytes], { type: mime });
});


// PROGRESS

function _Http_track(router, xhr, tracker)
{
	// TODO check out lengthComputable on loadstart event

	xhr.upload.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Sending({
			dG: event.loaded,
			bJ: event.total
		}))));
	});
	xhr.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Receiving({
			dt: event.loaded,
			bJ: event.lengthComputable ? $elm$core$Maybe$Just(event.total) : $elm$core$Maybe$Nothing
		}))));
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
}var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm$core$List$cons = _List_cons;
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
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
			if (t.$ === -2) {
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
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$GT = 2;
var $elm$core$Basics$LT = 0;
var $elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Maybe$Nothing = {$: 1};
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
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 1) {
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
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
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
		return {$: 0, a: a, b: b, c: c, d: d};
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
	return {$: 1, a: a};
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
	return {$: 0, a: a};
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
		if (!builder.f) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.i),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.i);
		} else {
			var treeLen = builder.f * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.j) : builder.j;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.f);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.i) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.i);
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
					{j: nodeList, f: (len / $elm$core$Array$branchFactor) | 0, i: tail});
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
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = $elm$core$Basics$identity;
var $elm$url$Url$Http = 0;
var $elm$url$Url$Https = 1;
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {a8: fragment, be: host, bt: path, bv: port_, by: protocol, bz: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 1) {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		0,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		1,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = $elm$core$Basics$identity;
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(0);
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
		var task = _v0;
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
				return 0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0;
		return A2($elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			A2($elm$core$Task$map, toMessage, task));
	});
var $elm$browser$Browser$element = _Browser_element;
var $elm$json$Json$Decode$field = _Json_decodeField;
var $author$project$Main$Idle = {$: 0};
var $author$project$Remote$Loading = {$: 0};
var $author$project$Page$Organizations = 0;
var $elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $author$project$Main$GotOrganizations = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $author$project$Main$GotWorkspace = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$decodeString = _Json_runOnString;
var $elm$http$Http$BadStatus_ = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$http$Http$BadUrl_ = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$GoodStatus_ = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $elm$http$Http$NetworkError_ = {$: 2};
var $elm$http$Http$Receiving = function (a) {
	return {$: 1, a: a};
};
var $elm$http$Http$Sending = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$Timeout_ = {$: 1};
var $elm$core$Maybe$isJust = function (maybe) {
	if (!maybe.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === -2) {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
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
var $elm$core$Dict$Black = 1;
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = 0;
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
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
					0,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1) {
				case 0:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
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
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === -1) && (dict.d.$ === -1)) {
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
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.e.d.$ === -1) && (!dict.e.d.a)) {
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
				0,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rlR, rRight));
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
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.d.d.$ === -1) && (!dict.d.d.a)) {
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
				0,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight)));
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
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === -1) && (!left.a)) {
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
				A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === -1) && (right.a === 1)) {
					if (right.d.$ === -1) {
						if (right.d.a === 1) {
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
	if ((dict.$ === -1) && (dict.d.$ === -1)) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor === 1) {
			if ((lLeft.$ === -1) && (!lLeft.a)) {
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
				if (_v4.$ === -1) {
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
		if (dict.$ === -2) {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === -1) && (left.a === 1)) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === -1) && (!lLeft.a)) {
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
						if (_v7.$ === -1) {
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
		if (dict.$ === -1) {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === -1) {
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
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (!_v0.$) {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$http$Http$expectStringResponse = F2(
	function (toMsg, toResult) {
		return A3(
			_Http_expect,
			'',
			$elm$core$Basics$identity,
			A2($elm$core$Basics$composeR, toResult, toMsg));
	});
var $elm$core$Result$mapError = F2(
	function (f, result) {
		if (!result.$) {
			var v = result.a;
			return $elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return $elm$core$Result$Err(
				f(e));
		}
	});
var $elm$http$Http$BadBody = function (a) {
	return {$: 4, a: a};
};
var $elm$http$Http$BadStatus = function (a) {
	return {$: 3, a: a};
};
var $elm$http$Http$BadUrl = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$NetworkError = {$: 2};
var $elm$http$Http$Timeout = {$: 1};
var $elm$http$Http$resolve = F2(
	function (toResult, response) {
		switch (response.$) {
			case 0:
				var url = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadUrl(url));
			case 1:
				return $elm$core$Result$Err($elm$http$Http$Timeout);
			case 2:
				return $elm$core$Result$Err($elm$http$Http$NetworkError);
			case 3:
				var metadata = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadStatus(metadata.dM));
			default:
				var body = response.b;
				return A2(
					$elm$core$Result$mapError,
					$elm$http$Http$BadBody,
					toResult(body));
		}
	});
var $elm$http$Http$expectJson = F2(
	function (toMsg, decoder) {
		return A2(
			$elm$http$Http$expectStringResponse,
			toMsg,
			$elm$http$Http$resolve(
				function (string) {
					return A2(
						$elm$core$Result$mapError,
						$elm$json$Json$Decode$errorToString,
						A2($elm$json$Json$Decode$decodeString, decoder, string));
				}));
	});
var $elm$http$Http$emptyBody = _Http_emptyBody;
var $elm$http$Http$Request = function (a) {
	return {$: 1, a: a};
};
var $elm$http$Http$State = F2(
	function (reqs, subs) {
		return {bB: reqs, bN: subs};
	});
var $elm$http$Http$init = $elm$core$Task$succeed(
	A2($elm$http$Http$State, $elm$core$Dict$empty, _List_Nil));
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$http$Http$updateReqs = F3(
	function (router, cmds, reqs) {
		updateReqs:
		while (true) {
			if (!cmds.b) {
				return $elm$core$Task$succeed(reqs);
			} else {
				var cmd = cmds.a;
				var otherCmds = cmds.b;
				if (!cmd.$) {
					var tracker = cmd.a;
					var _v2 = A2($elm$core$Dict$get, tracker, reqs);
					if (_v2.$ === 1) {
						var $temp$router = router,
							$temp$cmds = otherCmds,
							$temp$reqs = reqs;
						router = $temp$router;
						cmds = $temp$cmds;
						reqs = $temp$reqs;
						continue updateReqs;
					} else {
						var pid = _v2.a;
						return A2(
							$elm$core$Task$andThen,
							function (_v3) {
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A2($elm$core$Dict$remove, tracker, reqs));
							},
							$elm$core$Process$kill(pid));
					}
				} else {
					var req = cmd.a;
					return A2(
						$elm$core$Task$andThen,
						function (pid) {
							var _v4 = req.d_;
							if (_v4.$ === 1) {
								return A3($elm$http$Http$updateReqs, router, otherCmds, reqs);
							} else {
								var tracker = _v4.a;
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A3($elm$core$Dict$insert, tracker, pid, reqs));
							}
						},
						$elm$core$Process$spawn(
							A3(
								_Http_toTask,
								router,
								$elm$core$Platform$sendToApp(router),
								req)));
				}
			}
		}
	});
var $elm$http$Http$onEffects = F4(
	function (router, cmds, subs, state) {
		return A2(
			$elm$core$Task$andThen,
			function (reqs) {
				return $elm$core$Task$succeed(
					A2($elm$http$Http$State, reqs, subs));
			},
			A3($elm$http$Http$updateReqs, router, cmds, state.bB));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (!_v0.$) {
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
var $elm$http$Http$maybeSend = F4(
	function (router, desiredTracker, progress, _v0) {
		var actualTracker = _v0.a;
		var toMsg = _v0.b;
		return _Utils_eq(desiredTracker, actualTracker) ? $elm$core$Maybe$Just(
			A2(
				$elm$core$Platform$sendToApp,
				router,
				toMsg(progress))) : $elm$core$Maybe$Nothing;
	});
var $elm$http$Http$onSelfMsg = F3(
	function (router, _v0, state) {
		var tracker = _v0.a;
		var progress = _v0.b;
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$filterMap,
					A3($elm$http$Http$maybeSend, router, tracker, progress),
					state.bN)));
	});
var $elm$http$Http$Cancel = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$cmdMap = F2(
	function (func, cmd) {
		if (!cmd.$) {
			var tracker = cmd.a;
			return $elm$http$Http$Cancel(tracker);
		} else {
			var r = cmd.a;
			return $elm$http$Http$Request(
				{
					b1: r.b1,
					b9: r.b9,
					aD: A2(_Http_mapExpect, func, r.aD),
					cQ: r.cQ,
					c1: r.c1,
					dX: r.dX,
					d_: r.d_,
					aP: r.aP
				});
		}
	});
var $elm$http$Http$MySub = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$http$Http$subMap = F2(
	function (func, _v0) {
		var tracker = _v0.a;
		var toMsg = _v0.b;
		return A2(
			$elm$http$Http$MySub,
			tracker,
			A2($elm$core$Basics$composeR, toMsg, func));
	});
_Platform_effectManagers['Http'] = _Platform_createManager($elm$http$Http$init, $elm$http$Http$onEffects, $elm$http$Http$onSelfMsg, $elm$http$Http$cmdMap, $elm$http$Http$subMap);
var $elm$http$Http$command = _Platform_leaf('Http');
var $elm$http$Http$subscription = _Platform_leaf('Http');
var $elm$http$Http$request = function (r) {
	return $elm$http$Http$command(
		$elm$http$Http$Request(
			{b1: false, b9: r.b9, aD: r.aD, cQ: r.cQ, c1: r.c1, dX: r.dX, d_: r.d_, aP: r.aP}));
};
var $elm$http$Http$get = function (r) {
	return $elm$http$Http$request(
		{b9: $elm$http$Http$emptyBody, aD: r.aD, cQ: _List_Nil, c1: 'GET', dX: $elm$core$Maybe$Nothing, d_: $elm$core$Maybe$Nothing, aP: r.aP});
};
var $elm$json$Json$Decode$list = _Json_decodeList;
var $author$project$Domain$Summary = F4(
	function (organization, demo, peopleCount, goalCount) {
		return {a2: demo, cO: goalCount, bs: organization, dp: peopleCount};
	});
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $author$project$Api$Decode$andMap = $elm$json$Json$Decode$map2($elm$core$Basics$apR);
var $author$project$Api$Decode$field = F2(
	function (name, decoder) {
		return $author$project$Api$Decode$andMap(
			A2($elm$json$Json$Decode$field, name, decoder));
	});
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $author$project$Domain$Organization = F3(
	function (id, name, createdAt) {
		return {cp: createdAt, bf: id, c7: name};
	});
var $elm$json$Json$Decode$map3 = _Json_map3;
var $elm$json$Json$Decode$string = _Json_decodeString;
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
var $author$project$Api$Http$organizations = function (onResult) {
	return $elm$http$Http$get(
		{
			aD: A2(
				$elm$http$Http$expectJson,
				onResult,
				$elm$json$Json$Decode$list($author$project$Api$Decode$summaryDecoder)),
			aP: '/api/organizations'
		});
};
var $elm$url$Url$percentEncode = _Url_percentEncode;
var $author$project$Api$Path$orgPath = F2(
	function (org, tail) {
		return '/api/organizations/' + ($elm$url$Url$percentEncode(org) + ((tail === '') ? '' : ('/' + tail)));
	});
var $author$project$Domain$ReviewWarning = F2(
	function (id, warnings) {
		return {bf: id, bT: warnings};
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
												return {b5: authorities, cl: compiler, cs: decisionShare, a2: demo, cC: edges, cH: events, cP: goals, bs: organization, $7: people, dB: reviewWarnings, dC: reviews, aA: version};
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
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $author$project$Domain$Audit = F6(
	function (seq, at, actor, description, evaluatedGoal, evaluatedStatus) {
		return {b$: actor, b4: at, cw: description, cF: evaluatedGoal, cG: evaluatedStatus, dH: seq};
	});
var $elm$json$Json$Decode$index = _Json_decodeIndex;
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
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
var $elm$json$Json$Decode$fail = _Json_fail;
var $elm$json$Json$Decode$keyValuePairs = _Json_decodeKeyValuePairs;
var $elm$json$Json$Decode$null = _Json_decodeNull;
var $elm$json$Json$Decode$nullable = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				$elm$json$Json$Decode$null($elm$core$Maybe$Nothing),
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder)
			]));
};
var $elm$json$Json$Decode$value = _Json_decodeValue;
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
var $author$project$Domain$Achieved = 4;
var $author$project$Domain$AtRisk = 2;
var $author$project$Domain$NoData = 0;
var $author$project$Domain$OffTrack = 3;
var $author$project$Domain$OnTrack = 1;
var $author$project$Api$Decode$statusDecoder = A2(
	$elm$json$Json$Decode$andThen,
	function (s) {
		switch (s) {
			case 'NoData':
				return $elm$json$Json$Decode$succeed(0);
			case 'OnTrack':
				return $elm$json$Json$Decode$succeed(1);
			case 'AtRisk':
				return $elm$json$Json$Decode$succeed(2);
			case 'OffTrack':
				return $elm$json$Json$Decode$succeed(3);
			case 'Achieved':
				return $elm$json$Json$Decode$succeed(4);
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
				$elm$json$Json$Decode$map,
				$elm$core$Maybe$Just,
				A2(
					$elm$json$Json$Decode$at,
					_List_fromArray(
						['record', 'event', 'contents']),
					A2(
						$elm$json$Json$Decode$index,
						1,
						A2($elm$json$Json$Decode$field, 'status', $author$project$Api$Decode$statusDecoder)))),
				$elm$json$Json$Decode$succeed($elm$core$Maybe$Nothing)
			])),
	A2(
		$author$project$Api$Decode$andMap,
		$elm$json$Json$Decode$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$json$Json$Decode$at,
					_List_fromArray(
						['record', 'event']),
					A2(
						$elm$json$Json$Decode$andThen,
						function (tag) {
							return (tag === 'GoalEvaluated') ? A2(
								$elm$json$Json$Decode$field,
								'contents',
								A2(
									$elm$json$Json$Decode$map,
									$elm$core$Maybe$Just,
									A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$string))) : $elm$json$Json$Decode$succeed($elm$core$Maybe$Nothing);
						},
						A2($elm$json$Json$Decode$field, 'tag', $elm$json$Json$Decode$string))),
					$elm$json$Json$Decode$succeed($elm$core$Maybe$Nothing)
				])),
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
						$elm$json$Json$Decode$succeed($author$project$Domain$Audit)))))));
var $author$project$Domain$Authority = F5(
	function (owner, budgetLimit, canHire, canChangePrice, canApprove) {
		return {cc: budgetLimit, ce: canApprove, cf: canChangePrice, cg: canHire, aK: owner};
	});
var $elm$json$Json$Decode$float = _Json_decodeFloat;
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
		return {cy: diagnostics, cE: errors, bT: warnings};
	});
var $author$project$Domain$Diagnostic = F5(
	function (severity, code, message, subject, details) {
		return {ck: code, cx: details, c0: message, dJ: severity, dP: subject};
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
var $elm$json$Json$Decode$dict = function (decoder) {
	return A2(
		$elm$json$Json$Decode$map,
		$elm$core$Dict$fromList,
		$elm$json$Json$Decode$keyValuePairs(decoder));
};
var $author$project$Domain$Edge = F3(
	function (from, to, kind) {
		return {cM: from, cX: kind, dY: to};
	});
var $author$project$Domain$Node = F2(
	function (tag, contents) {
		return {cn: contents, dT: tag};
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
		return {co: coverage, dr: possibleCause};
	});
var $author$project$Domain$GoalView = F7(
	function (goal, owner, active, evaluation, analysis, results, strategies) {
		return {b_: active, b2: analysis, a6: evaluation, aF: goal, aK: owner, dz: results, dO: strategies};
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
var $author$project$Domain$Evaluation = F3(
	function (status, progress, latestValue) {
		return {cY: latestValue, ds: progress, dL: status};
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
var $author$project$Domain$Goal = F8(
	function (id, description, metric, baseline, target, deadline, requiredBudget, requiredPermissions) {
		return {b8: baseline, cq: deadline, cw: description, bf: id, c2: metric, dx: requiredBudget, dy: requiredPermissions, dV: target};
	});
var $author$project$Domain$Metric = F4(
	function (id, name, unit, direction) {
		return {cz: direction, bf: id, c7: name, d0: unit};
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
var $author$project$Domain$Measurement = F3(
	function (value, reportedAt, note) {
		return {dh: note, dw: reportedAt, aQ: value};
	});
var $author$project$Api$Decode$measurementDecoder = A3(
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
			$elm$json$Json$Decode$succeed($author$project$Domain$Measurement))));
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
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
var $author$project$Domain$Person = F3(
	function (id, name, role) {
		return {bf: id, c7: name, dF: role};
	});
var $author$project$Api$Decode$personDecoder = A3(
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
			$elm$json$Json$Decode$succeed($author$project$Domain$Person))));
var $author$project$Domain$Review = F7(
	function (id, goal, heldAt, note, evaluation, learnings, decisions) {
		return {ct: decisions, a6: evaluation, aF: goal, cR: heldAt, bf: id, cZ: learnings, dh: note};
	});
var $author$project$Domain$Decision = F3(
	function (text, owner, deadline) {
		return {cq: deadline, aK: owner, dW: text};
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
var $author$project$Api$Http$workspace = F2(
	function (org, onResult) {
		return $elm$http$Http$get(
			{
				aD: A2($elm$http$Http$expectJson, onResult, $author$project$Api$Decode$workspaceDecoder),
				aP: A2($author$project$Api$Path$orgPath, org, 'dashboard')
			});
	});
var $author$project$Main$refresh = function (model) {
	var token = model.L + 1;
	var next = _Utils_update(
		model,
		{a1: $elm$core$Maybe$Nothing, x: false, L: token, N: true});
	var _v0 = model.au;
	if (_v0.$ === 1) {
		return _Utils_Tuple2(
			next,
			$author$project$Api$Http$organizations(
				$author$project$Main$GotOrganizations(token)));
	} else {
		var org = _v0.a;
		return _Utils_Tuple2(
			next,
			A2(
				$author$project$Api$Http$workspace,
				org,
				$author$project$Main$GotWorkspace(token)));
	}
};
var $author$project$Main$init = function (flags) {
	return $author$project$Main$refresh(
		{a1: $elm$core$Maybe$Nothing, B: $elm$core$Dict$empty, p: false, Z: $elm$core$Maybe$Nothing, aa: flags, x: false, C: $elm$core$Dict$empty, ab: $elm$core$Dict$empty, ac: true, s: '', au: $elm$core$Maybe$Nothing, T: $author$project$Remote$Loading, y: 0, L: 0, t: $elm$core$Dict$empty, M: $author$project$Main$Idle, bH: 0, N: true, bW: $author$project$Remote$Loading});
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $author$project$Form$Action$AddGoal = {$: 4};
var $author$project$Form$Action$AddReview = {$: 9};
var $author$project$Form$Action$DeleteOrg = {$: 12};
var $author$project$Main$EditGoal = F2(
	function (a, b) {
		return {$: 5, a: a, b: b};
	});
var $author$project$Main$EditReview = F2(
	function (a, b) {
		return {$: 6, a: a, b: b};
	});
var $author$project$Remote$Failed = function (a) {
	return {$: 2, a: a};
};
var $author$project$Form$Review$Goal = 0;
var $author$project$Remote$Loaded = function (a) {
	return {$: 1, a: a};
};
var $author$project$Main$NoOp = {$: 14};
var $author$project$Page$Reviews = 5;
var $author$project$Main$Saved = F3(
	function (a, b, c) {
		return {$: 8, a: a, b: b, c: c};
	});
var $author$project$Main$Saving = function (a) {
	return {$: 1, a: a};
};
var $author$project$Form$Action$actionKey = function (action) {
	switch (action.$) {
		case 0:
			return 'organization';
		case 1:
			return 'demo';
		case 2:
			return 'rename';
		case 3:
			return 'person';
		case 4:
			return 'goal';
		case 5:
			var key = action.a;
			return 'owner-' + key;
		case 6:
			var key = action.a;
			return 'authority-' + key;
		case 7:
			var key = action.a;
			return 'result-' + key;
		case 8:
			var key = action.a;
			return 'strategy-' + key;
		case 9:
			return 'review';
		case 10:
			var key = action.a;
			return 'activate-' + key;
		case 11:
			var key = action.a;
			return 'evaluate-' + key;
		default:
			return 'delete';
	}
};
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$core$Task$onError = _Scheduler_onError;
var $elm$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return $elm$core$Task$command(
			A2(
				$elm$core$Task$onError,
				A2(
					$elm$core$Basics$composeL,
					A2($elm$core$Basics$composeL, $elm$core$Task$succeed, resultToMessage),
					$elm$core$Result$Err),
				A2(
					$elm$core$Task$andThen,
					A2(
						$elm$core$Basics$composeL,
						A2($elm$core$Basics$composeL, $elm$core$Task$succeed, resultToMessage),
						$elm$core$Result$Ok),
					task)));
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $author$project$Main$busy = function (model) {
	return !_Utils_eq(model.M, $author$project$Main$Idle);
};
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Main$defaultContext = function (model) {
	return {
		cq: model.aa.cq,
		bc: A2(
			$elm$core$Maybe$withDefault,
			0,
			A2(
				$elm$core$Dict$get,
				A2($elm$core$Maybe$withDefault, '', model.au),
				model.ab)),
		bG: model.aa.bG,
		bR: model.aa.bR,
		bW: function () {
			var _v0 = model.bW;
			if (_v0.$ === 1) {
				var data = _v0.a;
				return $elm$core$Maybe$Just(data);
			} else {
				return $elm$core$Maybe$Nothing;
			}
		}()
	};
};
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (!maybeValue.$) {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
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
var $elm$core$String$fromFloat = _String_fromNumber;
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
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
		var w = model.bW;
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
								return $.aK;
							},
							$elm$core$List$head(
								A2(
									$elm$core$List$filter,
									A2(
										$elm$core$Basics$composeR,
										function ($) {
											return $.aF;
										},
										A2(
											$elm$core$Basics$composeR,
											function ($) {
												return $.bf;
											},
											$elm$core$Basics$eq(key))),
									data.cP)));
					},
					w));
		};
		switch (action.$) {
			case 2:
				return (name === 'name') ? A2(
					$elm$core$Maybe$withDefault,
					'',
					A2(
						$elm$core$Maybe$map,
						A2(
							$elm$core$Basics$composeR,
							function ($) {
								return $.bs;
							},
							function ($) {
								return $.c7;
							}),
						w)) : '';
			case 4:
				switch (name) {
					case 'baseline':
						return '0';
					case 'target':
						return '100';
					case 'budget':
						return '0';
					case 'metricId':
						return 'metric-' + (model.bG + ('-' + $elm$core$String$fromInt(model.bc)));
					case 'direction':
						return 'HigherIsBetter';
					case 'startsAt':
						return model.bR;
					case 'deadline':
						return model.cq;
					default:
						return '';
				}
			case 5:
				var key = action.a;
				return (name === 'owner') ? owner(key) : '';
			case 7:
				var key = action.a;
				return (name === 'reportedBy') ? owner(key) : '';
			case 6:
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
										return $.aK;
									},
									$elm$core$Basics$eq(key)),
								data.b5));
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
								return $.cc;
							},
							$elm$core$String$fromFloat),
						authority)) : (A2(
					$elm$core$Maybe$withDefault,
					false,
					A2(
						$elm$core$Maybe$map,
						function (a) {
							return A2($elm$core$List$member, name, a.ce) || (((name === 'Hiring') && a.cg) || ((name === 'Pricing') && a.cf));
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
						['name', 'role', 'budget', 'owner', 'reportedBy', 'note', 'value']),
					$author$project$Domain$Permission$permissionKeys)));
	});
var $author$project$Main$draftDefaults = function (model) {
	return $author$project$Form$Defaults$draftDefaults(
		$author$project$Main$defaultContext(model));
};
var $author$project$Form$Goal$edit = F3(
	function (key, content, draft) {
		switch (key.$) {
			case 0:
				return _Utils_update(
					draft,
					{cw: content});
			case 1:
				return _Utils_update(
					draft,
					{c4: content});
			case 2:
				return _Utils_update(
					draft,
					{d0: content});
			case 3:
				return _Utils_update(
					draft,
					{c3: content});
			case 4:
				return _Utils_update(
					draft,
					{cz: content});
			case 5:
				return _Utils_update(
					draft,
					{b8: content});
			case 6:
				return _Utils_update(
					draft,
					{dV: content});
			case 7:
				return _Utils_update(
					draft,
					{dK: content});
			case 8:
				return _Utils_update(
					draft,
					{cq: content});
			case 9:
				return _Utils_update(
					draft,
					{cb: content});
			case 10:
				return _Utils_update(
					draft,
					{dn: content});
			default:
				var permission = key.a;
				return _Utils_update(
					draft,
					{
						dq: (content === 'true') ? A2(
							$elm$core$List$cons,
							permission,
							A2(
								$elm$core$List$filter,
								$elm$core$Basics$neq(permission),
								draft.dq)) : A2(
							$elm$core$List$filter,
							$elm$core$Basics$neq(permission),
							draft.dq)
					});
		}
	});
var $author$project$Form$Review$edit = F3(
	function (key, content, draft) {
		switch (key) {
			case 0:
				return _Utils_update(
					draft,
					{aF: content});
			case 1:
				return _Utils_update(
					draft,
					{dh: content});
			case 2:
				return _Utils_update(
					draft,
					{bl: content});
			case 3:
				return _Utils_update(
					draft,
					{a_: content});
			case 4:
				return _Utils_update(
					draft,
					{cr: content});
			default:
				return _Utils_update(
					draft,
					{a$: content});
		}
	});
var $author$project$Api$Http$errorText = function (err) {
	switch (err.$) {
		case 0:
			return '요청 주소를 확인할 수 없습니다.';
		case 1:
			return '서버 응답 시간이 초과되었습니다. 입력 내용은 보존됩니다.';
		case 2:
			return '서버에 연결할 수 없습니다. 연결을 확인하고 다시 시도하세요.';
		case 3:
			var code = err.a;
			return '서버 조회 실패 (' + ($elm$core$String$fromInt(code) + '). 새로고침해 주세요.');
		default:
			return '서버 응답 형식이 예상과 다릅니다. 입력 내용은 보존됩니다.';
	}
};
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === -2) {
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
var $elm$browser$Browser$Dom$focus = _Browser_call('focus');
var $author$project$Main$formKey = F2(
	function (model, action) {
		return A2($elm$core$Maybe$withDefault, 'list', model.au) + ('/' + $author$project$Form$Action$actionKey(action));
	});
var $author$project$Form$Goal$Baseline = {$: 5};
var $author$project$Form$Goal$Budget = {$: 9};
var $author$project$Form$Goal$Deadline = {$: 8};
var $author$project$Form$Goal$Description = {$: 0};
var $author$project$Form$Goal$Direction = {$: 4};
var $author$project$Form$Goal$MetricId = {$: 3};
var $author$project$Form$Goal$MetricName = {$: 1};
var $author$project$Form$Goal$Parent = {$: 10};
var $author$project$Form$Goal$Permission = function (a) {
	return {$: 11, a: a};
};
var $author$project$Form$Goal$StartsAt = {$: 7};
var $author$project$Form$Goal$Target = {$: 6};
var $author$project$Form$Goal$Unit = {$: 2};
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
var $author$project$Form$Review$Decision = 3;
var $author$project$Form$Review$DecisionDeadline = 5;
var $author$project$Form$Review$DecisionOwner = 4;
var $author$project$Form$Review$Learning = 2;
var $author$project$Form$Review$Note = 1;
var $author$project$Form$Review$fromKey = function (key) {
	switch (key) {
		case 'goal':
			return $elm$core$Maybe$Just(0);
		case 'note':
			return $elm$core$Maybe$Just(1);
		case 'learning':
			return $elm$core$Maybe$Just(2);
		case 'decision':
			return $elm$core$Maybe$Just(3);
		case 'decisionOwner':
			return $elm$core$Maybe$Just(4);
		case 'decisionDeadline':
			return $elm$core$Maybe$Just(5);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Main$defaultValue = function (model) {
	return $author$project$Form$Defaults$defaultValue(
		$author$project$Main$defaultContext(model));
};
var $author$project$Form$Goal$fromValues = function (get) {
	return {
		b8: get('baseline'),
		cb: get('budget'),
		cq: get('deadline'),
		cw: get('description'),
		cz: get('direction'),
		c3: get('metricId'),
		c4: get('metricName'),
		dn: get('parent'),
		dq: A2(
			$elm$core$List$filter,
			function (key) {
				return get(key) === 'true';
			},
			$author$project$Domain$Permission$permissionKeys),
		dK: get('startsAt'),
		dV: get('target'),
		d0: get('unit')
	};
};
var $author$project$Main$goalDraft = function (model) {
	return A2(
		$elm$core$Maybe$withDefault,
		$author$project$Form$Goal$fromValues(
			A2($author$project$Main$defaultValue, model, $author$project$Form$Action$AddGoal)),
		A2(
			$elm$core$Dict$get,
			A2($author$project$Main$formKey, model, $author$project$Form$Action$AddGoal),
			model.C));
};
var $author$project$Form$Review$fromValues = function (get) {
	return {
		a_: get('decision'),
		a$: get('decisionDeadline'),
		cr: get('decisionOwner'),
		aF: get('goal'),
		bl: get('learning'),
		dh: get('note')
	};
};
var $author$project$Main$reviewDraft = function (model) {
	return A2(
		$elm$core$Maybe$withDefault,
		$author$project$Form$Review$fromValues(
			A2($author$project$Main$defaultValue, model, $author$project$Form$Action$AddReview)),
		A2(
			$elm$core$Dict$get,
			A2($author$project$Main$formKey, model, $author$project$Form$Action$AddReview),
			model.t));
};
var $author$project$Form$Goal$value = F2(
	function (draft, key) {
		switch (key.$) {
			case 0:
				return draft.cw;
			case 1:
				return draft.c4;
			case 2:
				return draft.d0;
			case 3:
				return draft.c3;
			case 4:
				return draft.cz;
			case 5:
				return draft.b8;
			case 6:
				return draft.dV;
			case 7:
				return draft.dK;
			case 8:
				return draft.cq;
			case 9:
				return draft.cb;
			case 10:
				return draft.dn;
			default:
				var permission = key.a;
				return A2($elm$core$List$member, permission, draft.dq) ? 'true' : 'false';
		}
	});
var $author$project$Form$Review$value = F2(
	function (draft, key) {
		switch (key) {
			case 0:
				return draft.aF;
			case 1:
				return draft.dh;
			case 2:
				return draft.bl;
			case 3:
				return draft.a_;
			case 4:
				return draft.cr;
			default:
				return draft.a$;
		}
	});
var $author$project$Main$get = F3(
	function (model, action, name) {
		switch (action.$) {
			case 4:
				return A2(
					$elm$core$Maybe$withDefault,
					'',
					A2(
						$elm$core$Maybe$map,
						$author$project$Form$Goal$value(
							$author$project$Main$goalDraft(model)),
						$author$project$Form$Goal$fromKey(name)));
			case 9:
				return A2(
					$elm$core$Maybe$withDefault,
					'',
					A2(
						$elm$core$Maybe$map,
						$author$project$Form$Review$value(
							$author$project$Main$reviewDraft(model)),
						$author$project$Form$Review$fromKey(name)));
			default:
				return A2(
					$elm$core$Maybe$withDefault,
					A3($author$project$Main$defaultValue, model, action, name),
					A2(
						$elm$core$Maybe$andThen,
						$elm$core$Dict$get(name),
						A2(
							$elm$core$Dict$get,
							A2($author$project$Main$formKey, model, action),
							model.B)));
		}
	});
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $elm$core$Basics$not = _Basics_not;
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$json$Json$Encode$float = _Json_wrap;
var $elm$json$Json$Encode$int = _Json_wrap;
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(0),
				entries));
	});
var $elm$core$Result$map = F2(
	function (func, ra) {
		if (!ra.$) {
			var a = ra.a;
			return $elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return $elm$core$Result$Err(e);
		}
	});
var $elm$json$Json$Encode$null = _Json_encodeNull;
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
			_Json_emptyObject(0),
			pairs));
};
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$core$String$toFloat = _String_toFloat;
var $elm$core$String$trim = _String_trim;
var $author$project$Form$Goal$validate = function (draft) {
	if (A2(
		$elm$core$List$any,
		A2(
			$elm$core$Basics$composeR,
			$elm$core$String$trim,
			$elm$core$Basics$eq('')),
		_List_fromArray(
			[draft.cw, draft.c3, draft.c4, draft.d0, draft.dK, draft.cq]))) {
		return $elm$core$Result$Err('필수 항목을 모두 입력하세요.');
	} else {
		var _v0 = _Utils_Tuple3(
			$elm$core$String$toFloat(draft.b8),
			$elm$core$String$toFloat(draft.dV),
			$elm$core$String$toFloat(draft.cb));
		if (((!_v0.a.$) && (!_v0.b.$)) && (!_v0.c.$)) {
			var baseline = _v0.a.a;
			var target = _v0.b.a;
			var budget = _v0.c.a;
			return (_Utils_cmp(draft.cq, draft.dK) < 0) ? $elm$core$Result$Err('마감일은 시작일 이후여야 합니다.') : $elm$core$Result$Ok(
				{b8: baseline, cb: budget, cq: draft.cq, cw: draft.cw, cz: draft.cz, c3: draft.c3, c4: draft.c4, dn: draft.dn, dq: draft.dq, dK: draft.dK, dV: target, d0: draft.d0});
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
			[draft.aF, draft.dh])) ? $elm$core$Result$Err('필수 항목을 모두 입력하세요.') : ((($elm$core$String$trim(draft.a_) !== '') && (draft.cr === '')) ? $elm$core$Result$Err('다음 결정의 담당자를 선택하세요.') : $elm$core$Result$Ok(draft));
};
var $author$project$Api$Command$payload = F2(
	function (model, action) {
		var version = A2(
			$elm$core$Maybe$withDefault,
			model.aA,
			$elm$core$String$toInt(
				A2(model.aQ, action, '__version')));
		var val = model.aQ(action);
		var uid = function (prefix) {
			return $elm$json$Json$Encode$string(
				prefix + ('-' + (model.bG + ('-' + $elm$core$String$fromInt(model.bH)))));
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
				A2($elm$core$Maybe$withDefault, '', model.au),
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
			case 0:
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
			case 1:
				return A2(post, '/api/demo', _List_Nil);
			case 2:
				return A3(
					validate,
					_List_fromArray(
						['name']),
					_List_Nil,
					(!_Utils_eq(version, model.aA)) ? $elm$core$Result$Err('작성 중 조직이 변경되었습니다. 최신 조직 이름을 확인하고 수정 입력을 다시 해 주세요.') : $elm$core$Result$Ok(
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
			case 3:
				return A3(
					validate,
					_List_fromArray(
						['name', 'role']),
					_List_Nil,
					A2(
						post,
						path('people'),
						_List_fromArray(
							[
								_Utils_Tuple2(
								'id',
								uid('person')),
								str('name'),
								str('role')
							])));
			case 4:
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
											A2($elm$core$Maybe$withDefault, '', model.au))),
										_Utils_Tuple2(
										'description',
										$elm$json$Json$Encode$string(goal.cw)),
										_Utils_Tuple2(
										'metric',
										$elm$json$Json$Encode$object(
											_List_fromArray(
												[
													_Utils_Tuple2(
													'id',
													$elm$json$Json$Encode$string(goal.c3)),
													_Utils_Tuple2(
													'name',
													$elm$json$Json$Encode$string(goal.c4)),
													_Utils_Tuple2(
													'unit',
													$elm$json$Json$Encode$string(goal.d0)),
													_Utils_Tuple2(
													'direction',
													$elm$json$Json$Encode$string(goal.cz))
												]))),
										_Utils_Tuple2(
										'baseline',
										$elm$json$Json$Encode$float(goal.b8)),
										_Utils_Tuple2(
										'target',
										$elm$json$Json$Encode$float(goal.dV)),
										_Utils_Tuple2(
										'startsAt',
										$elm$json$Json$Encode$string(goal.dK + 'T00:00:00Z')),
										_Utils_Tuple2(
										'deadline',
										$elm$json$Json$Encode$string(goal.cq + 'T00:00:00Z')),
										_Utils_Tuple2(
										'parent',
										nullable(goal.dn)),
										_Utils_Tuple2(
										'requiredPermissions',
										A2(
											$elm$json$Json$Encode$list,
											$elm$json$Json$Encode$string,
											A2(
												$elm$core$List$filter,
												function (key) {
													return A2($elm$core$List$member, key, goal.dq);
												},
												$author$project$Domain$Permission$permissionKeys))),
										_Utils_Tuple2(
										'requiredBudget',
										$elm$json$Json$Encode$float(goal.cb))
									])));
					},
					$author$project$Form$Goal$validate(model.aF));
			case 5:
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
			case 6:
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
			case 7:
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
			case 8:
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
			case 10:
				var key = action.a;
				return A2(
					post,
					path(
						'goals/' + ($elm$url$Url$percentEncode(key) + '/activate')),
					_List_Nil);
			case 11:
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
			case 9:
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
										$elm$json$Json$Encode$string(review.aF)),
										_Utils_Tuple2(
										'note',
										$elm$json$Json$Encode$string(review.dh)),
										_Utils_Tuple2(
										'learnings',
										A2(
											$elm$json$Json$Encode$list,
											$elm$core$Basics$identity,
											($elm$core$String$trim(review.bl) === '') ? _List_Nil : _List_fromArray(
												[
													$elm$json$Json$Encode$object(
													_List_fromArray(
														[
															_Utils_Tuple2(
															'text',
															$elm$json$Json$Encode$string(review.bl))
														]))
												]))),
										_Utils_Tuple2(
										'decisions',
										A2(
											$elm$json$Json$Encode$list,
											$elm$core$Basics$identity,
											($elm$core$String$trim(review.a_) === '') ? _List_Nil : _List_fromArray(
												[
													$elm$json$Json$Encode$object(
													_List_fromArray(
														[
															_Utils_Tuple2(
															'text',
															$elm$json$Json$Encode$string(review.a_)),
															_Utils_Tuple2(
															'owner',
															$elm$json$Json$Encode$string(review.cr)),
															_Utils_Tuple2(
															'deadline',
															(review.a$ === '') ? $elm$json$Json$Encode$null : $elm$json$Json$Encode$string(review.a$ + 'T23:59:59Z'))
														]))
												])))
									])));
					},
					$author$project$Form$Review$validate(model.bD));
			default:
				var _v1 = model.a1;
				if (!_v1.$) {
					var snapshot = _v1.a;
					return (_Utils_eq(snapshot.aX, snapshot.c7) && _Utils_eq(
						model.au,
						$elm$core$Maybe$Just(snapshot.bf))) ? $elm$core$Result$Ok(
						_Utils_Tuple3(
							'DELETE',
							A2($author$project$Api$Path$orgPath, snapshot.bf, ''),
							$elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'confirmName',
										$elm$json$Json$Encode$string(snapshot.aX)),
										_Utils_Tuple2(
										'expectedVersion',
										$elm$json$Json$Encode$int(snapshot.aA))
									])))) : $elm$core$Result$Err('조직 이름을 정확히 입력하세요.');
				} else {
					return $elm$core$Result$Err('삭제 확인을 먼저 열어 주세요.');
				}
		}
	});
var $author$project$Main$workspaceVersion = function (model) {
	var _v0 = model.bW;
	if (_v0.$ === 1) {
		var w = _v0.a;
		return w.aA;
	} else {
		return 0;
	}
};
var $author$project$Main$payload = F2(
	function (model, action) {
		return A2(
			$author$project$Api$Command$payload,
			{
				a1: model.a1,
				aF: $author$project$Main$goalDraft(model),
				au: model.au,
				bD: $author$project$Main$reviewDraft(model),
				bG: model.aa.bG,
				bH: model.bH,
				aQ: $author$project$Main$get(model),
				aA: $author$project$Main$workspaceVersion(model)
			},
			action);
	});
var $elm$http$Http$jsonBody = function (value) {
	return A2(
		_Http_pair,
		'application/json',
		A2($elm$json$Json$Encode$encode, 0, value));
};
var $elm$core$Result$withDefault = F2(
	function (def, result) {
		if (!result.$) {
			var a = result.a;
			return a;
		} else {
			return def;
		}
	});
var $author$project$Api$Http$send = F4(
	function (onResult, method, path, body) {
		return $elm$http$Http$request(
			{
				b9: $elm$http$Http$jsonBody(body),
				aD: A2(
					$elm$http$Http$expectStringResponse,
					onResult,
					function (response) {
						switch (response.$) {
							case 0:
								return $elm$core$Result$Err('잘못된 요청 주소입니다.');
							case 1:
								return $elm$core$Result$Err('응답 시간이 초과되었습니다. 서버에서 이미 저장됐을 수 있으므로 최신 기록을 확인하세요.');
							case 2:
								return $elm$core$Result$Err('연결이 끊겼습니다. 서버에서 이미 저장됐을 수 있으므로 최신 기록을 확인하세요.');
							case 3:
								var metadata = response.a;
								var content = response.b;
								return $elm$core$Result$Err(
									A2(
										$elm$core$Result$withDefault,
										'요청을 처리할 수 없습니다.',
										A2(
											$elm$json$Json$Decode$decodeString,
											A2($elm$json$Json$Decode$field, 'error', $elm$json$Json$Decode$string),
											content)) + (' (' + ($elm$core$String$fromInt(metadata.dM) + ')')));
							default:
								return $elm$core$Result$Ok(0);
						}
					}),
				cQ: _List_Nil,
				c1: method,
				dX: $elm$core$Maybe$Just(30000),
				d_: $elm$core$Maybe$Nothing,
				aP: path
			});
	});
var $author$project$Main$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 0:
				var page = msg.a;
				var org = msg.b;
				return $author$project$Main$busy(model) ? _Utils_Tuple2(model, $elm$core$Platform$Cmd$none) : ((_Utils_eq(org, model.au) && (!_Utils_eq(org, $elm$core$Maybe$Nothing))) ? _Utils_Tuple2(
					_Utils_update(
						model,
						{a1: $elm$core$Maybe$Nothing, y: page}),
					$elm$core$Platform$Cmd$none) : $author$project$Main$refresh(
					_Utils_update(
						model,
						{a1: $elm$core$Maybe$Nothing, p: false, s: '', au: org, y: page, bW: $author$project$Remote$Loading})));
			case 1:
				return $author$project$Main$busy(model) ? _Utils_Tuple2(model, $elm$core$Platform$Cmd$none) : $author$project$Main$refresh(model);
			case 2:
				var token = msg.a;
				var response = msg.b;
				if (!_Utils_eq(token, model.L)) {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				} else {
					if (!response.$) {
						var items = response.a;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									x: true,
									T: $author$project$Remote$Loaded(items),
									N: false
								}),
							$elm$core$Platform$Cmd$none);
					} else {
						var err = response.a;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									p: true,
									x: false,
									s: $author$project$Api$Http$errorText(err),
									T: $author$project$Remote$Failed(
										$author$project$Api$Http$errorText(err)),
									N: false
								}),
							$elm$core$Platform$Cmd$none);
					}
				}
			case 3:
				var token = msg.a;
				var response = msg.b;
				if (!_Utils_eq(token, model.L)) {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				} else {
					if (!response.$) {
						var workspace = response.a;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									x: true,
									N: false,
									bW: $author$project$Remote$Loaded(workspace)
								}),
							$elm$core$Platform$Cmd$none);
					} else {
						var err = response.a;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									p: true,
									x: false,
									s: $author$project$Api$Http$errorText(err),
									N: false,
									bW: $author$project$Remote$Failed(
										$author$project$Api$Http$errorText(err))
								}),
							$elm$core$Platform$Cmd$none);
					}
				}
			case 4:
				var action = msg.a;
				var key = msg.b;
				var val = msg.c;
				if ($author$project$Main$busy(model)) {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				} else {
					switch (action.$) {
						case 4:
							return A2(
								$elm$core$Maybe$withDefault,
								_Utils_Tuple2(model, $elm$core$Platform$Cmd$none),
								A2(
									$elm$core$Maybe$map,
									function (field) {
										return A2(
											$author$project$Main$update,
											A2($author$project$Main$EditGoal, field, val),
											model);
									},
									$author$project$Form$Goal$fromKey(key)));
						case 9:
							return A2(
								$elm$core$Maybe$withDefault,
								_Utils_Tuple2(model, $elm$core$Platform$Cmd$none),
								A2(
									$elm$core$Maybe$map,
									function (field) {
										return A2(
											$author$project$Main$update,
											A2($author$project$Main$EditReview, field, val),
											model);
									},
									$author$project$Form$Review$fromKey(key)));
						default:
							var draftKey = A2($author$project$Main$formKey, model, action);
							var current = A2(
								$elm$core$Maybe$withDefault,
								A2($author$project$Main$draftDefaults, model, action),
								A2($elm$core$Dict$get, draftKey, model.B));
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										B: A3(
											$elm$core$Dict$insert,
											draftKey,
											A3(
												$elm$core$Dict$insert,
												'__version',
												$elm$core$String$fromInt(
													$author$project$Main$workspaceVersion(model)),
												A3($elm$core$Dict$insert, key, val, current)),
											model.B)
									}),
								$elm$core$Platform$Cmd$none);
					}
				}
			case 5:
				var field = msg.a;
				var val = msg.b;
				return $author$project$Main$busy(model) ? _Utils_Tuple2(model, $elm$core$Platform$Cmd$none) : _Utils_Tuple2(
					_Utils_update(
						model,
						{
							C: A3(
								$elm$core$Dict$insert,
								A2($author$project$Main$formKey, model, $author$project$Form$Action$AddGoal),
								A3(
									$author$project$Form$Goal$edit,
									field,
									val,
									$author$project$Main$goalDraft(model)),
								model.C)
						}),
					$elm$core$Platform$Cmd$none);
			case 6:
				var field = msg.a;
				var val = msg.b;
				return $author$project$Main$busy(model) ? _Utils_Tuple2(model, $elm$core$Platform$Cmd$none) : _Utils_Tuple2(
					_Utils_update(
						model,
						{
							t: A3(
								$elm$core$Dict$insert,
								A2($author$project$Main$formKey, model, $author$project$Form$Action$AddReview),
								A3(
									$author$project$Form$Review$edit,
									field,
									val,
									$author$project$Main$reviewDraft(model)),
								model.t)
						}),
					$elm$core$Platform$Cmd$none);
			case 7:
				var action = msg.a;
				if ($author$project$Main$busy(model)) {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				} else {
					if (!model.x) {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{p: true, s: '최신 상태를 먼저 불러와 주세요. 입력 내용은 보존됩니다.'}),
							$elm$core$Platform$Cmd$none);
					} else {
						var _v4 = A2($author$project$Main$payload, model, action);
						if (_v4.$ === 1) {
							var message = _v4.a;
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{p: true, s: message}),
								$elm$core$Platform$Cmd$none);
						} else {
							var _v5 = _v4.a;
							var method = _v5.a;
							var path = _v5.b;
							var body = _v5.c;
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										p: false,
										s: '저장 중입니다…',
										M: $author$project$Main$Saving(
											$author$project$Form$Action$actionKey(action)),
										bH: model.bH + 1
									}),
								A4(
									$author$project$Api$Http$send,
									A2($author$project$Main$Saved, model.L, action),
									method,
									path,
									body));
						}
					}
				}
			case 8:
				var token = msg.a;
				var action = msg.b;
				var response = msg.c;
				if (!_Utils_eq(token, model.L)) {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				} else {
					if (response.$ === 1) {
						var message = response.a;
						return $author$project$Main$refresh(
							_Utils_update(
								model,
								{a1: $elm$core$Maybe$Nothing, p: true, s: message + ' 자동 재시도하지 않았습니다. 최신 상태를 확인한 뒤 다시 저장하세요. 입력 내용은 보존됩니다.', M: $author$project$Main$Idle}));
					} else {
						var saved = _Utils_update(
							model,
							{
								a1: $elm$core$Maybe$Nothing,
								B: A2(
									$elm$core$Dict$remove,
									A2($author$project$Main$formKey, model, action),
									model.B),
								p: false,
								C: _Utils_eq(action, $author$project$Form$Action$AddGoal) ? A2(
									$elm$core$Dict$remove,
									A2($author$project$Main$formKey, model, action),
									model.C) : model.C,
								ab: _Utils_eq(action, $author$project$Form$Action$AddGoal) ? A3(
									$elm$core$Dict$update,
									A2($elm$core$Maybe$withDefault, '', model.au),
									A2(
										$elm$core$Basics$composeR,
										$elm$core$Maybe$withDefault(0),
										A2(
											$elm$core$Basics$composeR,
											$elm$core$Basics$add(1),
											$elm$core$Maybe$Just)),
									model.ab) : model.ab,
								s: '저장했습니다. 최신 조직 상태와 감사 기록을 확인하세요.',
								t: _Utils_eq(action, $author$project$Form$Action$AddReview) ? A2(
									$elm$core$Dict$remove,
									A2($author$project$Main$formKey, model, action),
									model.t) : model.t,
								M: $author$project$Main$Idle
							});
						return _Utils_eq(action, $author$project$Form$Action$DeleteOrg) ? $author$project$Main$refresh(
							_Utils_update(
								saved,
								{
									B: A2(
										$elm$core$Dict$filter,
										F2(
											function (key, _v7) {
												return !A2(
													$elm$core$String$startsWith,
													A2($elm$core$Maybe$withDefault, '', model.au) + '/',
													key);
											}),
										model.B),
									C: A2(
										$elm$core$Dict$filter,
										F2(
											function (key, _v8) {
												return !A2(
													$elm$core$String$startsWith,
													A2($elm$core$Maybe$withDefault, '', model.au) + '/',
													key);
											}),
										saved.C),
									s: '조직을 논리 삭제했습니다. 원본 감사 기록과 다른 조직은 보존됩니다.',
									au: $elm$core$Maybe$Nothing,
									T: $author$project$Remote$Loading,
									y: 0,
									t: A2(
										$elm$core$Dict$filter,
										F2(
											function (key, _v9) {
												return !A2(
													$elm$core$String$startsWith,
													A2($elm$core$Maybe$withDefault, '', model.au) + '/',
													key);
											}),
										saved.t),
									bW: $author$project$Remote$Loading
								})) : $author$project$Main$refresh(saved);
					}
				}
			case 9:
				var _v10 = model.bW;
				if (_v10.$ === 1) {
					var w = _v10.a;
					return (model.x && (!$author$project$Main$busy(model))) ? _Utils_Tuple2(
						_Utils_update(
							model,
							{
								a1: $elm$core$Maybe$Just(
									{aX: '', bf: w.bs.bf, c7: w.bs.c7, aA: w.aA})
							}),
						A2(
							$elm$core$Task$attempt,
							$elm$core$Basics$always($author$project$Main$NoOp),
							$elm$browser$Browser$Dom$focus('delete-confirm'))) : _Utils_Tuple2(
						_Utils_update(
							model,
							{p: true, s: '최신 조직 정보를 불러온 뒤 다시 확인하세요.'}),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 10:
				var name = msg.a;
				return $author$project$Main$busy(model) ? _Utils_Tuple2(model, $elm$core$Platform$Cmd$none) : _Utils_Tuple2(
					_Utils_update(
						model,
						{
							a1: A2(
								$elm$core$Maybe$map,
								function (snapshot) {
									return _Utils_update(
										snapshot,
										{aX: name});
								},
								model.a1)
						}),
					$elm$core$Platform$Cmd$none);
			case 11:
				return $author$project$Main$busy(model) ? _Utils_Tuple2(model, $elm$core$Platform$Cmd$none) : _Utils_Tuple2(
					_Utils_update(
						model,
						{a1: $elm$core$Maybe$Nothing}),
					$elm$core$Platform$Cmd$none);
			case 12:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{ac: !model.ac}),
					$elm$core$Platform$Cmd$none);
			case 13:
				var page = msg.a;
				var target = msg.b;
				if ($author$project$Main$busy(model)) {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				} else {
					var reviews = ((page === 5) && ((target === 'review-form') && (A3($author$project$Main$get, model, $author$project$Form$Action$AddReview, 'goal') === ''))) ? A3(
						$elm$core$Dict$insert,
						A2($author$project$Main$formKey, model, $author$project$Form$Action$AddReview),
						A3(
							$author$project$Form$Review$edit,
							0,
							'demo-revenue',
							$author$project$Main$reviewDraft(model)),
						model.t) : model.t;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								Z: A2($elm$core$String$startsWith, 'goal-', target) ? $elm$core$Maybe$Just(
									A2($elm$core$String$dropLeft, 5, target)) : model.Z,
								y: page,
								t: reviews
							}),
						A2(
							$elm$core$Task$attempt,
							$elm$core$Basics$always($author$project$Main$NoOp),
							$elm$browser$Browser$Dom$focus(target)));
				}
			default:
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Page$Authorities = 3;
var $author$project$Page$Dashboard = 1;
var $author$project$Main$Navigate = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$Main$Refresh = {$: 1};
var $author$project$Page$Responsibility = 2;
var $author$project$Page$Results = 4;
var $author$project$Page$Settings = 6;
var $elm$html$Html$a = _VirtualDom_node('a');
var $elm$html$Html$aside = _VirtualDom_node('aside');
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $elm$html$Html$br = _VirtualDom_node('br');
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
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
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$html$Html$footer = _VirtualDom_node('footer');
var $author$project$Main$Edit = F3(
	function (a, b, c) {
		return {$: 4, a: a, b: b, c: c};
	});
var $author$project$Main$Submit = function (a) {
	return {$: 7, a: a};
};
var $author$project$Main$formConfig = function (model) {
	return {
		aV: $author$project$Main$busy(model),
		aC: $author$project$Main$Edit,
		x: model.x,
		M: function () {
			var _v0 = model.M;
			if (!_v0.$) {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = _v0.a;
				return $elm$core$Maybe$Just(key);
			}
		}(),
		dQ: $author$project$Main$Submit,
		aQ: $author$project$Main$get(model)
	};
};
var $elm$html$Html$h1 = _VirtualDom_node('h1');
var $elm$html$Html$header = _VirtualDom_node('header');
var $elm$html$Html$Attributes$href = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $elm$html$Html$main_ = _VirtualDom_node('main');
var $elm$html$Html$nav = _VirtualDom_node('nav');
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
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
var $elm$html$Html$p = _VirtualDom_node('p');
var $author$project$Page$pageName = function (page) {
	switch (page) {
		case 0:
			return '조직 목록';
		case 1:
			return '목표';
		case 2:
			return '책임';
		case 3:
			return '권한';
		case 4:
			return '결과';
		case 5:
			return '학습';
		default:
			return '조직 설정';
	}
};
var $elm$html$Html$small = _VirtualDom_node('small');
var $elm$html$Html$span = _VirtualDom_node('span');
var $elm$html$Html$strong = _VirtualDom_node('strong');
var $elm$html$Html$Attributes$tabindex = function (n) {
	return A2(
		_VirtualDom_attribute,
		'tabIndex',
		$elm$core$String$fromInt(n));
};
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $author$project$Form$Action$CreateOrg = {$: 0};
var $author$project$Form$Action$ImportDemo = {$: 1};
var $elm$html$Html$h2 = _VirtualDom_node('h2');
var $elm$html$Html$section = _VirtualDom_node('section');
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
var $elm$html$Html$fieldset = _VirtualDom_node('fieldset');
var $elm$html$Html$form = _VirtualDom_node('form');
var $elm$html$Html$Events$alwaysPreventDefault = function (msg) {
	return _Utils_Tuple2(msg, true);
};
var $elm$virtual_dom$VirtualDom$MayPreventDefault = function (a) {
	return {$: 2, a: a};
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
var $author$project$Ui$Form$formView = F4(
	function (model, action, label_, children) {
		return A2(
			$elm$html$Html$form,
			_List_fromArray(
				[
					$elm$html$Html$Events$onSubmit(
					model.dQ(action))
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$fieldset,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$disabled(model.aV)
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
										$elm$html$Html$Attributes$disabled(!model.x)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(
										_Utils_eq(
											model.M,
											$elm$core$Maybe$Just(
												$author$project$Form$Action$actionKey(action))) ? '저장 중…' : label_)
									]))
							])))
				]));
	});
var $elm$html$Html$Attributes$autocomplete = function (bool) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'autocomplete',
		bool ? 'on' : 'off');
};
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$label = _VirtualDom_node('label');
var $elm$html$Html$Attributes$name = $elm$html$Html$Attributes$stringProperty('name');
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 1, a: a};
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
var $elm$html$Html$Attributes$required = $elm$html$Html$Attributes$boolProperty('required');
var $elm$html$Html$Attributes$step = function (n) {
	return A2($elm$html$Html$Attributes$stringProperty, 'step', n);
};
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$Ui$Form$inputField = F6(
	function (model, action, label_, key, kind, required_) {
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
							$elm$html$Html$Attributes$name(key),
							$elm$html$Html$Attributes$type_(kind),
							$elm$html$Html$Attributes$value(
							A2(model.aQ, action, key)),
							$elm$html$Html$Events$onInput(
							A2(model.aC, action, key)),
							$elm$html$Html$Attributes$required(required_),
							$elm$html$Html$Attributes$step('any'),
							$elm$html$Html$Attributes$autocomplete(false)
						]),
					_List_Nil)
				]));
	});
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
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
var $author$project$Remote$view = F2(
	function (remote, render) {
		switch (remote.$) {
			case 0:
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
			case 2:
				var message = remote.a;
				return A2($author$project$Ui$Common$emptyState, '조회하지 못했습니다', message);
			default:
				var data = remote.a;
				return render(data);
		}
	});
var $author$project$Page$Organizations$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$author$project$Ui$Common$panel,
				'새 조직 등록',
				_List_fromArray(
					[
						$author$project$Ui$Common$note('각 조직의 구성원, 목표와 학습은 독립적으로 관리됩니다.'),
						A4(
						$author$project$Ui$Form$formView,
						model.Q,
						$author$project$Form$Action$CreateOrg,
						'조직 등록',
						_List_fromArray(
							[
								A6($author$project$Ui$Form$inputField, model.Q, $author$project$Form$Action$CreateOrg, '조직 이름', 'name', 'text', true)
							])),
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('secondary'),
								$elm$html$Html$Attributes$disabled(
								model.Q.aV || ((!model.Q.x) || function () {
									var _v0 = model.T;
									if (_v0.$ === 1) {
										var items = _v0.a;
										return A2(
											$elm$core$List$any,
											A2(
												$elm$core$Basics$composeR,
												function ($) {
													return $.bs;
												},
												A2(
													$elm$core$Basics$composeR,
													function ($) {
														return $.bf;
													},
													$elm$core$Basics$eq('demo-northstar-v2'))),
											items);
									} else {
										return true;
									}
								}())),
								$elm$html$Html$Events$onClick(
								model.Q.dQ($author$project$Form$Action$ImportDemo))
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('체험용 데모 조직 추가')
							]))
					])),
				A2(
				$author$project$Remote$view,
				model.T,
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
								$elm$core$List$isEmpty(items) ? A2($author$project$Ui$Common$emptyState, '첫 조직을 시작하세요', '조직 이름을 입력하거나 가상 데이터로 운영 흐름을 체험하세요.') : A2(
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
															item.a2 ? '가상 데이터 · 데모' : '내 조직')
														])),
													A2(
													$elm$html$Html$h2,
													_List_Nil,
													_List_fromArray(
														[
															$elm$html$Html$text(item.bs.c7)
														])),
													A2(
													$elm$html$Html$p,
													_List_Nil,
													_List_fromArray(
														[
															$elm$html$Html$text(
															'구성원 ' + ($elm$core$String$fromInt(item.dp) + ('명 · 목표 ' + ($elm$core$String$fromInt(item.cO) + '개'))))
														])),
													A2(
													$elm$html$Html$small,
													_List_Nil,
													_List_fromArray(
														[
															$elm$html$Html$text(
															'등록 ' + A2($elm$core$String$left, 10, item.bs.cp))
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
																	$elm$html$Html$Attributes$disabled(model.Q.aV),
																	$elm$html$Html$Events$onClick(
																	model.dk(item.bs.bf))
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
																	$elm$html$Html$Attributes$disabled(model.Q.aV),
																	$elm$html$Html$Events$onClick(
																	model.dI(item.bs.bf))
																]),
															_List_fromArray(
																[
																	$elm$html$Html$text('상세 · 수정 · 삭제')
																]))
														]))
												]));
									},
									items))
							]));
				})
			]));
};
var $author$project$Main$CloseDelete = {$: 11};
var $author$project$Main$ConfirmDelete = function (a) {
	return {$: 10, a: a};
};
var $author$project$Main$Guide = F2(
	function (a, b) {
		return {$: 13, a: a, b: b};
	});
var $author$project$Main$OpenDelete = {$: 9};
var $author$project$Main$ToggleGuide = {$: 12};
var $author$project$Form$Action$Grant = function (a) {
	return {$: 6, a: a};
};
var $elm$html$Html$Attributes$checked = $elm$html$Html$Attributes$boolProperty('checked');
var $elm$html$Html$legend = _VirtualDom_node('legend');
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
												A2(model.aQ, action, key) === 'true'),
												$elm$html$Html$Events$onCheck(
												function (checked_) {
													return A3(
														model.aC,
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
var $elm$html$Html$code = _VirtualDom_node('code');
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
						$elm$html$Html$text('다음 행동을 위한 피드백'),
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('tag warn')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(
								$elm$core$String$fromInt(w.cl.cE) + (' 오류 · ' + ($elm$core$String$fromInt(w.cl.bT) + ' 경고')))
							]))
					])),
				$elm$core$List$isEmpty(w.cl.cy) ? $author$project$Ui$Common$note('구조 검사를 통과했습니다. 결과를 보고하고 학습을 이어가세요.') : A2(
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
											_Utils_Tuple2('error', d.dJ === 'Error')
										]))
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$code,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(d.ck)
										])),
									A2(
									$elm$html$Html$strong,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(d.c0)
										])),
									A2(
									$elm$html$Html$p,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(d.dP)
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
										d.cx))
								]));
					},
					w.cl.cy)),
				$author$project$Ui$Common$note('권한 집중도는 권한 종류와 예산 보유를 각각 1점으로 세는 규칙 기반 추정치입니다.')
			]));
};
var $elm$core$Basics$round = _Basics_round;
var $author$project$Page$Authorities$view = F2(
	function (model, w) {
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
							$author$project$Ui$Common$note('권한을 줄여 활성 목표의 요건이 깨지면 해당 목표는 자동으로 초안으로 돌아갑니다.'),
							$author$project$Ui$Common$note('집중도 = 보유 권한 종류 수 + 예산 보유 1점 / 조직 전체 점수. 실제 의사결정 빈도나 권력의 측정값은 아닙니다.')
						])),
					$elm$core$List$isEmpty(w.$7) ? A2($author$project$Ui$Common$emptyState, '구성원을 먼저 추가하세요', '목표 메뉴에서 구성원을 추가한 뒤 권한을 부여할 수 있습니다.') : A2(
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
										$elm$html$Html$Attributes$id('authority-' + person.bf),
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
												'권한 비중 ' + ($elm$core$String$fromInt(
													$elm$core$Basics$round(
														100 * A2(
															$elm$core$Maybe$withDefault,
															0,
															A2($elm$core$Dict$get, person.bf, w.cs)))) + '%'))
											])),
										A2(
										$elm$html$Html$h2,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('form-heading')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(person.c7)
											])),
										A2(
										$elm$html$Html$p,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('muted')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(person.dF)
											])),
										A4(
										$author$project$Ui$Form$formView,
										model.Q,
										$author$project$Form$Action$Grant(person.bf),
										'권한 저장',
										_List_fromArray(
											[
												A6(
												$author$project$Ui$Form$inputField,
												model.Q,
												$author$project$Form$Action$Grant(person.bf),
												'집행 가능한 예산 한도 (KRW)',
												'budget',
												'number',
												true),
												A2(
												$author$project$Ui$Form$checks,
												model.Q,
												$author$project$Form$Action$Grant(person.bf))
											])),
										$author$project$Ui$Common$note(
										'담당 목표 ' + ($elm$core$String$fromInt(
											$elm$core$List$length(
												A2(
													$elm$core$List$filter,
													A2(
														$elm$core$Basics$composeR,
														function ($) {
															return $.aK;
														},
														$elm$core$Basics$eq(
															$elm$core$Maybe$Just(person.bf))),
													w.cP))) + '개'))
									]));
						},
						w.$7)),
					$author$project$Ui$Common$diagnosticView(w)
				]));
	});
var $author$project$Form$Action$AddPerson = {$: 3};
var $elm$html$Html$details = _VirtualDom_node('details');
var $author$project$Form$Action$Activate = function (a) {
	return {$: 10, a: a};
};
var $author$project$Form$Action$Assign = function (a) {
	return {$: 5, a: a};
};
var $author$project$Form$Action$Strategy = function (a) {
	return {$: 8, a: a};
};
var $elm$html$Html$article = _VirtualDom_node('article');
var $author$project$Ui$Label$statusName = function (s) {
	switch (s) {
		case 0:
			return '결과 대기';
		case 1:
			return '정상';
		case 2:
			return '위험';
		case 3:
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
						_Utils_Tuple2('draft', !g.b_),
						_Utils_Tuple2('error', g.a6.dL === 3),
						_Utils_Tuple2('warn', g.a6.dL === 2)
					]))
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(
				g.b_ ? $author$project$Ui$Label$statusName(g.a6.dL) : '초안')
			]));
};
var $elm$core$Basics$clamp = F3(
	function (low, high, number) {
		return (_Utils_cmp(number, low) < 0) ? low : ((_Utils_cmp(number, high) > 0) ? high : number);
	});
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
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
		if (!_v0.$) {
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
var $elm$html$Html$Attributes$max = $elm$html$Html$Attributes$stringProperty('max');
var $author$project$Ui$Label$personName = F2(
	function (w, key) {
		return A2(
			$elm$core$Maybe$withDefault,
			key,
			A2(
				$elm$core$Maybe$map,
				function ($) {
					return $.c7;
				},
				$elm$core$List$head(
					A2(
						$elm$core$List$filter,
						A2(
							$elm$core$Basics$composeR,
							function ($) {
								return $.bf;
							},
							$elm$core$Basics$eq(key)),
						w.$7))));
	});
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
						$elm$html$Html$text(g.aF.cw)
					])),
				A2(
				$elm$html$Html$small,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(
						g.aF.c2.c7 + (' · ' + (((g.aF.c2.cz === 'HigherIsBetter') ? '↑ 증가' : '↓ 감소') + ' 목표')))
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
									A2($elm$core$Maybe$map, $author$project$Ui$Label$formatNumber, g.a6.cY)))
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
								'/ ' + ($author$project$Ui$Label$formatNumber(g.aF.dV) + (' ' + g.aF.c2.d0)))
							]))
					])),
				A2(
				$elm$html$Html$progress,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$max('1'),
						$elm$html$Html$Attributes$value(
						$elm$core$String$fromFloat(
							A3($elm$core$Basics$clamp, 0, 1, g.a6.ds))),
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
							$elm$core$Basics$round(g.a6.ds * 100)) + ('% 달성 · 기준 ' + $author$project$Ui$Label$formatNumber(g.aF.b8)))
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
										g.aK)))
							])),
						A2(
						$elm$html$Html$span,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(
								A2($elm$core$String$left, 10, g.aF.cq) + ' 마감')
							]))
					]))
			]);
	});
var $author$project$Ui$Form$peopleOptions = function (w) {
	return A2(
		$elm$core$List$cons,
		_Utils_Tuple2('', '구성원 선택'),
		A2(
			$elm$core$List$map,
			function (p) {
				return _Utils_Tuple2(p.bf, p.c7 + (' · ' + p.dF));
			},
			w.$7));
};
var $elm$virtual_dom$VirtualDom$property = F2(
	function (key, value) {
		return A2(
			_VirtualDom_property,
			_VirtualDom_noInnerHtmlOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlJson(value));
	});
var $elm$html$Html$Attributes$property = $elm$virtual_dom$VirtualDom$property;
var $elm$html$Html$option = _VirtualDom_node('option');
var $elm$html$Html$select = _VirtualDom_node('select');
var $elm$html$Html$Attributes$selected = $elm$html$Html$Attributes$boolProperty('selected');
var $author$project$Ui$Form$selectField = F6(
	function (model, action, label_, key, required_, options) {
		return A2(
			$elm$html$Html$label,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text(label_),
					A2(
					$elm$html$Html$select,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$name(key),
							$elm$html$Html$Attributes$value(
							A2(model.aQ, action, key)),
							$elm$html$Html$Events$onInput(
							A2(model.aC, action, key)),
							$elm$html$Html$Attributes$required(required_)
						]),
					A2(
						$elm$core$List$map,
						function (_v0) {
							var key_ = _v0.a;
							var label__ = _v0.b;
							return A2(
								$elm$html$Html$option,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$value(key_),
										$elm$html$Html$Attributes$selected(
										_Utils_eq(
											A2(model.aQ, action, key),
											key_))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(label__)
									]));
						},
						options))
				]));
	});
var $elm$html$Html$summary = _VirtualDom_node('summary');
var $author$project$Page$Goals$goalCard = F3(
	function (model, w, g) {
		return A2(
			$elm$html$Html$article,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('goal-card'),
					$elm$html$Html$Attributes$id('goal-' + g.aF.bf),
					$elm$html$Html$Attributes$tabindex(-1)
				]),
			_Utils_ap(
				A2($author$project$Ui$Common$goalSummary, w, g),
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
										$elm$html$Html$Attributes$class('secondary'),
										$elm$html$Html$Attributes$disabled(model.Q.aV),
										$elm$html$Html$Events$onClick(
										model.dz('goal-' + g.aF.bf))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('결과 보고 · 평가 →')
									]))
							])),
						A2(
						$elm$html$Html$details,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$Attributes$property,
								'open',
								$elm$json$Json$Encode$bool(
									_Utils_eq(
										model.Z,
										$elm$core$Maybe$Just(g.aF.bf))))
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$summary,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('책임 · 권한 · 전략 관리')
									])),
								$author$project$Ui$Common$note(g.b2.dr),
								A4(
								$author$project$Ui$Form$formView,
								model.Q,
								$author$project$Form$Action$Assign(g.aF.bf),
								'책임자 지정',
								_List_fromArray(
									[
										A6(
										$author$project$Ui$Form$selectField,
										model.Q,
										$author$project$Form$Action$Assign(g.aF.bf),
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
												$elm$html$Html$Attributes$disabled(model.Q.aV || ((!model.Q.x) || g.b_)),
												$elm$html$Html$Events$onClick(
												model.Q.dQ(
													$author$project$Form$Action$Activate(g.aF.bf)))
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(
												g.b_ ? '활성화됨' : '목표 활성화')
											]))
									])),
								A4(
								$author$project$Ui$Form$formView,
								model.Q,
								$author$project$Form$Action$Strategy(g.aF.bf),
								'전략 변경 기록',
								_List_fromArray(
									[
										A6(
										$author$project$Ui$Form$inputField,
										model.Q,
										$author$project$Form$Action$Strategy(g.aF.bf),
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
									g.dO))
							]))
					])));
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
		case 0:
			return 'description';
		case 1:
			return 'metricName';
		case 2:
			return 'unit';
		case 3:
			return 'metricId';
		case 4:
			return 'direction';
		case 5:
			return 'baseline';
		case 6:
			return 'target';
		case 7:
			return 'startsAt';
		case 8:
			return 'deadline';
		case 9:
			return 'budget';
		case 10:
			return 'parent';
		default:
			var key = field.a;
			return key;
	}
};
var $author$project$Ui$Form$inputValue = F6(
	function (key, current, edit, label_, kind, required_) {
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
							$elm$html$Html$Attributes$name(key),
							$elm$html$Html$Attributes$type_(kind),
							$elm$html$Html$Attributes$value(current),
							$elm$html$Html$Events$onInput(edit),
							$elm$html$Html$Attributes$required(required_),
							$elm$html$Html$Attributes$step('any'),
							$elm$html$Html$Attributes$autocomplete(false)
						]),
					_List_Nil)
				]));
	});
var $author$project$Page$Goals$formInput = F5(
	function (model, label_, field, kind, required_) {
		return A6(
			$author$project$Ui$Form$inputValue,
			$author$project$Form$Goal$fieldName(field),
			A2($author$project$Form$Goal$value, model.a4, field),
			model.aC(field),
			label_,
			kind,
			required_);
	});
var $author$project$Ui$Form$selectValue = F6(
	function (key, current, edit, label_, required_, options) {
		return A2(
			$elm$html$Html$label,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text(label_),
					A2(
					$elm$html$Html$select,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$name(key),
							$elm$html$Html$Attributes$value(current),
							$elm$html$Html$Events$onInput(edit),
							$elm$html$Html$Attributes$required(required_)
						]),
					A2(
						$elm$core$List$map,
						function (_v0) {
							var key_ = _v0.a;
							var label__ = _v0.b;
							return A2(
								$elm$html$Html$option,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$value(key_),
										$elm$html$Html$Attributes$selected(
										_Utils_eq(current, key_))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(label__)
									]));
						},
						options))
				]));
	});
var $author$project$Page$Goals$formSelect = F5(
	function (model, label_, field, required_, options) {
		return A6(
			$author$project$Ui$Form$selectValue,
			$author$project$Form$Goal$fieldName(field),
			A2($author$project$Form$Goal$value, model.a4, field),
			model.aC(field),
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
				return _Utils_Tuple2(g.aF.bf, g.aF.cw);
			},
			w.cP));
};
var $author$project$Page$Goals$goalForm = F2(
	function (model, w) {
		return A4(
			$author$project$Ui$Form$formView,
			model.Q,
			$author$project$Form$Action$AddGoal,
			'목표 초안 생성',
			_List_fromArray(
				[
					$author$project$Ui$Common$note('초안 → 책임자 지정 → 권한 확인 → 활성화. 필요한 조건을 갖춘 뒤 실행합니다.'),
					A5($author$project$Page$Goals$formInput, model, '어떤 결과를 만들고 싶나요?', $author$project$Form$Goal$Description, 'text', true),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('fields')
						]),
					_List_fromArray(
						[
							A5($author$project$Page$Goals$formInput, model, 'KPI 이름', $author$project$Form$Goal$MetricName, 'text', true),
							A5($author$project$Page$Goals$formInput, model, '단위', $author$project$Form$Goal$Unit, 'text', true),
							A5($author$project$Page$Goals$formInput, model, '지표 식별자 · 같은 지표는 같은 ID', $author$project$Form$Goal$MetricId, 'text', true),
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
								])),
							A5($author$project$Page$Goals$formInput, model, '기준값', $author$project$Form$Goal$Baseline, 'number', true),
							A5($author$project$Page$Goals$formInput, model, '목표값', $author$project$Form$Goal$Target, 'number', true),
							A5($author$project$Page$Goals$formInput, model, '시작일 (UTC)', $author$project$Form$Goal$StartsAt, 'date', true),
							A5($author$project$Page$Goals$formInput, model, '마감일 (UTC)', $author$project$Form$Goal$Deadline, 'date', true),
							A5($author$project$Page$Goals$formInput, model, '필요 예산 (KRW)', $author$project$Form$Goal$Budget, 'number', true),
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
					$author$project$Ui$Form$checkValues,
					function (key) {
						return A2(
							$author$project$Form$Goal$value,
							model.a4,
							$author$project$Form$Goal$Permission(key));
					},
					function (key) {
						return model.aC(
							$author$project$Form$Goal$Permission(key));
					})
				]));
	});
var $elm$core$List$sum = function (numbers) {
	return A3($elm$core$List$foldl, $elm$core$Basics$add, 0, numbers);
};
var $author$project$Page$Goals$view = F2(
	function (model, w) {
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
								$elm$core$List$length(w.cP),
								'측정 가능한 결과'),
								_Utils_Tuple3(
								'활성 목표',
								$elm$core$List$length(
									A2(
										$elm$core$List$filter,
										function ($) {
											return $.b_;
										},
										w.cP)),
								'책임과 권한 검증 완료'),
								_Utils_Tuple3('구조 진단', w.cl.cE + w.cl.bT, '확인이 필요한 항목'),
								_Utils_Tuple3(
								'누적 학습',
								$elm$core$List$sum(
									A2(
										$elm$core$List$map,
										A2(
											$elm$core$Basics$composeR,
											function ($) {
												return $.cZ;
											},
											$elm$core$List$length),
										w.dC)),
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
					$elm$core$List$isEmpty(w.cP) ? A2($author$project$Ui$Common$emptyState, '어떤 결과를 만들고 싶나요?', '아래에서 측정 가능한 목표를 정의하고 책임자를 연결하세요.') : A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('grid')
						]),
					A2(
						$elm$core$List$map,
						A2($author$project$Page$Goals$goalCard, model, w),
						w.cP)),
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
					A2(
					$elm$html$Html$details,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('panel')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$summary,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(
									'구성원 추가 · 현재 ' + ($elm$core$String$fromInt(
										$elm$core$List$length(w.$7)) + '명'))
								])),
							A4(
							$author$project$Ui$Form$formView,
							model.Q,
							$author$project$Form$Action$AddPerson,
							'구성원 추가',
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
											A6($author$project$Ui$Form$inputField, model.Q, $author$project$Form$Action$AddPerson, '이름', 'name', 'text', true),
											A6($author$project$Ui$Form$inputField, model.Q, $author$project$Form$Action$AddPerson, '역할', 'role', 'text', true)
										]))
								]))
						])),
					$author$project$Ui$Common$diagnosticView(w)
				]));
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
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $author$project$Form$Review$fieldName = function (field) {
	switch (field) {
		case 0:
			return 'goal';
		case 1:
			return 'note';
		case 2:
			return 'learning';
		case 3:
			return 'decision';
		case 4:
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
			A2($author$project$Form$Review$value, model.a4, field),
			model.aC(field),
			label_,
			kind,
			required_);
	});
var $author$project$Page$Learning$formSelect = F5(
	function (model, label_, field, required_, options) {
		return A6(
			$author$project$Ui$Form$selectValue,
			$author$project$Form$Review$fieldName(field),
			A2($author$project$Form$Review$value, model.a4, field),
			model.aC(field),
			label_,
			required_,
			options);
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
						return $.aF;
					},
					function ($) {
						return $.cw;
					}),
				$elm$core$List$head(
					A2(
						$elm$core$List$filter,
						A2(
							$elm$core$Basics$composeR,
							function ($) {
								return $.aF;
							},
							A2(
								$elm$core$Basics$composeR,
								function ($) {
									return $.bf;
								},
								$elm$core$Basics$eq(key))),
						w.cP))));
	});
var $elm$html$Html$h3 = _VirtualDom_node('h3');
var $elm$html$Html$textarea = _VirtualDom_node('textarea');
var $author$project$Page$Learning$view = F2(
	function (model, w) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
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
							model.Q,
							$author$project$Form$Action$AddReview,
							'회고 기록',
							_List_fromArray(
								[
									A5(
									$author$project$Page$Learning$formSelect,
									model,
									'회고할 목표',
									0,
									true,
									$author$project$Ui$Form$goalOptions(w)),
									A5($author$project$Page$Learning$formInput, model, '회고 요약', 1, 'text', true),
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
													A2($author$project$Form$Review$value, model.a4, 2)),
													$elm$html$Html$Events$onInput(
													model.aC(2))
												]),
											_List_Nil)
										])),
									A5($author$project$Page$Learning$formInput, model, '다음 결정 (선택)', 3, 'text', false),
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
											4,
											false,
											$author$project$Ui$Form$peopleOptions(w)),
											A5($author$project$Page$Learning$formInput, model, '결정 기한 (UTC, 선택)', 5, 'date', false)
										])),
									$author$project$Ui$Common$note('현재 최신 결과와 평가가 함께 보존됩니다. 결정과 학습이 모두 없으면 구조 검사가 경고합니다.')
								]))
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('grid')
						]),
					A2(
						$elm$core$List$map,
						function (r) {
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
												A2($elm$core$String$left, 10, r.cR) + (' · ' + $author$project$Ui$Label$statusName(r.a6.dL)))
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
												A2($author$project$Ui$Label$goalName, w, r.aF))
											])),
										A2(
										$elm$html$Html$p,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text(r.dh)
											])),
										A2(
										$elm$html$Html$h3,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('학습')
											])),
										$elm$core$List$isEmpty(r.cZ) ? $author$project$Ui$Common$note('기록된 학습 없음') : A2(
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
											r.cZ)),
										A2(
										$elm$html$Html$h3,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('다음 결정')
											])),
										$elm$core$List$isEmpty(r.ct) ? $author$project$Ui$Common$note('기록된 결정 없음') : A2(
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
															$elm$html$Html$text(d.dW),
															A2($elm$html$Html$br, _List_Nil, _List_Nil),
															A2(
															$elm$html$Html$small,
															_List_Nil,
															_List_fromArray(
																[
																	$elm$html$Html$text(
																	A2($author$project$Ui$Label$personName, w, d.aK) + (' · ' + A2(
																		$elm$core$Maybe$withDefault,
																		'기한 미정',
																		A2(
																			$elm$core$Maybe$map,
																			$elm$core$String$left(10),
																			d.cq))))
																]))
														]));
											},
											r.ct)),
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
													return $.bT;
												},
												A2(
													$elm$core$List$filter,
													A2(
														$elm$core$Basics$composeR,
														function ($) {
															return $.bf;
														},
														$elm$core$Basics$eq(r.bf)),
													w.dB))))
									]));
						},
						w.dC)),
					A2(
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
									$elm$html$Html$text('조직의 의사결정 기록')
								])),
							$author$project$Ui$Common$note('서버가 시각과 순번을 부여합니다. 행위자는 요청의 기록 주체이며 인증된 신원 증명이 아닙니다.'),
							$elm$core$List$isEmpty(w.cH) ? $author$project$Ui$Common$note('아직 기록이 없습니다.') : A2(
							$elm$html$Html$div,
							_List_Nil,
							A2(
								$elm$core$List$map,
								function (event) {
									return A2(
										$elm$html$Html$div,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('event')
											]),
										_List_fromArray(
											[
												A2(
												$elm$html$Html$small,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text(
														'#' + ($elm$core$String$fromInt(event.dH) + (' · ' + event.b4))),
														A2($elm$html$Html$br, _List_Nil, _List_Nil),
														$elm$html$Html$text(
														A2(
															$elm$core$Maybe$withDefault,
															'로컬 운영자 (미인증)',
															A2(
																$elm$core$Maybe$map,
																$author$project$Ui$Label$personName(w),
																event.b$)))
													])),
												A2(
												$elm$html$Html$p,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text(event.cw)
													]))
											]));
								},
								w.cH))
						]))
				]));
	});
var $elm$html$Html$b = _VirtualDom_node('b');
var $author$project$Page$Responsibility$nodeName = F2(
	function (w, node) {
		var _v0 = node.dT;
		switch (_v0) {
			case 'PersonNode':
				return A2($author$project$Ui$Label$personName, w, node.cn);
			case 'GoalNode':
				return A2($author$project$Ui$Label$goalName, w, node.cn);
			default:
				return node.cn;
		}
	});
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
var $elm$html$Html$table = _VirtualDom_node('table');
var $elm$html$Html$tbody = _VirtualDom_node('tbody');
var $elm$html$Html$td = _VirtualDom_node('td');
var $elm$html$Html$th = _VirtualDom_node('th');
var $elm$html$Html$thead = _VirtualDom_node('thead');
var $elm$html$Html$tr = _VirtualDom_node('tr');
var $author$project$Page$Responsibility$view = F2(
	function (model, w) {
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
							$author$project$Ui$Common$note('각 목표에는 최종 책임자가 한 명 있습니다.'),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('table-wrap')
								]),
							_List_fromArray(
								[
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
													A2(
														$elm$core$List$map,
														function (title) {
															return A2(
																$elm$html$Html$th,
																_List_Nil,
																_List_fromArray(
																	[
																		$elm$html$Html$text(title)
																	]));
														},
														_List_fromArray(
															['결과 / KPI', '최종 책임자', '목표값', '필요 권한 / 통제율', '상태'])))
												])),
											A2(
											$elm$html$Html$tbody,
											_List_Nil,
											A2(
												$elm$core$List$map,
												function (g) {
													return A2(
														$elm$html$Html$tr,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$id('owner-' + g.aF.bf),
																$elm$html$Html$Attributes$tabindex(-1)
															]),
														_List_fromArray(
															[
																A2(
																$elm$html$Html$td,
																_List_Nil,
																_List_fromArray(
																	[
																		A2(
																		$elm$html$Html$strong,
																		_List_Nil,
																		_List_fromArray(
																			[
																				$elm$html$Html$text(g.aF.cw)
																			])),
																		$elm$html$Html$text(g.aF.c2.c7)
																	])),
																A2(
																$elm$html$Html$td,
																_List_Nil,
																_List_fromArray(
																	[
																		A4(
																		$author$project$Ui$Form$formView,
																		model.Q,
																		$author$project$Form$Action$Assign(g.aF.bf),
																		'책임자 지정',
																		_List_fromArray(
																			[
																				A6(
																				$author$project$Ui$Form$selectField,
																				model.Q,
																				$author$project$Form$Action$Assign(g.aF.bf),
																				'책임자',
																				'owner',
																				true,
																				$author$project$Ui$Form$peopleOptions(w))
																			]))
																	])),
																A2(
																$elm$html$Html$td,
																_List_Nil,
																_List_fromArray(
																	[
																		$elm$html$Html$text(
																		$author$project$Ui$Label$formatNumber(g.aF.dV) + (' ' + g.aF.c2.d0))
																	])),
																A2(
																$elm$html$Html$td,
																_List_Nil,
																_List_fromArray(
																	[
																		$elm$html$Html$text(
																		A2(
																			$elm$core$String$join,
																			' · ',
																			A2($elm$core$List$map, $author$project$Ui$Label$permissionName, g.aF.dy))),
																		$author$project$Ui$Common$note(
																		'예산 ' + ($author$project$Ui$Label$formatNumber(g.aF.dx) + ('원 · ' + ($elm$core$String$fromInt(
																			$elm$core$Basics$round(g.b2.co * 100)) + '% 통제'))))
																	])),
																A2(
																$elm$html$Html$td,
																_List_Nil,
																_List_fromArray(
																	[
																		$author$project$Ui$Common$badge(g)
																	]))
															]));
												},
												w.cP))
										]))
								]))
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
							$author$project$Ui$Common$note('사람 → 목표 → 지표. 목표 간 의존 관계와 자원 통제를 연결합니다.'),
							$elm$core$List$isEmpty(w.cC) ? $author$project$Ui$Common$note('책임자와 목표를 연결하면 그래프가 만들어집니다.') : A2(
							$elm$html$Html$div,
							_List_Nil,
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
												A2(
												$elm$html$Html$span,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text(
														A2($author$project$Page$Responsibility$nodeName, w, edge.cM))
													])),
												A2(
												$elm$html$Html$b,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text('─ ' + (edge.cX + ' →'))
													])),
												A2(
												$elm$html$Html$span,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text(
														A2($author$project$Page$Responsibility$nodeName, w, edge.dY))
													]))
											]));
								},
								w.cC))
						])),
					$author$project$Ui$Common$diagnosticView(w)
				]));
	});
var $author$project$Form$Action$Evaluate = function (a) {
	return {$: 11, a: a};
};
var $author$project$Form$Action$Report = function (a) {
	return {$: 7, a: a};
};
var $author$project$Page$Results$resultCard = F3(
	function (model, w, g) {
		return A2(
			$elm$html$Html$article,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('goal-card'),
					$elm$html$Html$Attributes$id('goal-' + g.aF.bf),
					$elm$html$Html$Attributes$tabindex(-1)
				]),
			_Utils_ap(
				A2($author$project$Ui$Common$goalSummary, w, g),
				_List_fromArray(
					[
						$author$project$Ui$Common$note(g.b2.dr),
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
						model.Q,
						$author$project$Form$Action$Report(g.aF.bf),
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
										model.Q,
										$author$project$Form$Action$Report(g.aF.bf),
										'실측값',
										'value',
										'number',
										true),
										A6(
										$author$project$Ui$Form$selectField,
										model.Q,
										$author$project$Form$Action$Report(g.aF.bf),
										'보고자',
										'reportedBy',
										true,
										$author$project$Ui$Form$peopleOptions(w))
									])),
								A6(
								$author$project$Ui$Form$inputField,
								model.Q,
								$author$project$Form$Action$Report(g.aF.bf),
								'결과 설명',
								'note',
								'text',
								true)
							])),
						$elm$core$List$isEmpty(g.dz) ? $author$project$Ui$Common$note('아직 결과가 없습니다.') : A2(
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
																	$elm$html$Html$text(r.dw)
																])),
															A2(
															$elm$html$Html$td,
															_List_Nil,
															_List_fromArray(
																[
																	$elm$html$Html$text(
																	$author$project$Ui$Label$formatNumber(r.aQ))
																])),
															A2(
															$elm$html$Html$td,
															_List_Nil,
															_List_fromArray(
																[
																	$elm$html$Html$text(r.dh)
																]))
														]));
											},
											g.dz))
									]))
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
										$elm$html$Html$Attributes$class('secondary'),
										$elm$html$Html$Attributes$disabled(model.Q.aV || (!model.Q.x)),
										$elm$html$Html$Events$onClick(
										model.Q.dQ(
											$author$project$Form$Action$Evaluate(g.aF.bf)))
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
										$elm$html$Html$Attributes$disabled(model.Q.aV),
										$elm$html$Html$Events$onClick(
										model.cP('goal-' + g.aF.bf))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('목표 관리 →')
									]))
							]))
					])));
	});
var $author$project$Page$Results$view = F2(
	function (model, w) {
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
					$author$project$Ui$Common$note('실측값을 보고하고 현재 성과를 평가하세요. 결과 이력은 다음 학습의 근거가 됩니다.'),
					$elm$core$List$isEmpty(w.cP) ? A2($author$project$Ui$Common$emptyState, '아직 측정할 목표가 없습니다', '목표 메뉴에서 목표를 만든 뒤 결과를 기록하세요.') : A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('grid')
						]),
					A2(
						$elm$core$List$map,
						A2($author$project$Page$Results$resultCard, model, w),
						w.cP))
				]));
	});
var $author$project$Form$Action$Rename = {$: 2};
var $elm$html$Html$dd = _VirtualDom_node('dd');
var $elm$html$Html$dl = _VirtualDom_node('dl');
var $elm$html$Html$dt = _VirtualDom_node('dt');
var $author$project$Page$Settings$view = F2(
	function (model, w) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$author$project$Ui$Common$panel,
					w.bs.c7,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$dl,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('organization-meta')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$dt,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text('조직 ID')
										])),
									A2(
									$elm$html$Html$dd,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(w.bs.bf)
										])),
									A2(
									$elm$html$Html$dt,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text('등록일')
										])),
									A2(
									$elm$html$Html$dd,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(
											A2($elm$core$String$left, 10, w.bs.cp))
										])),
									A2(
									$elm$html$Html$dt,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text('구성원')
										])),
									A2(
									$elm$html$Html$dd,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(
											$elm$core$String$fromInt(
												$elm$core$List$length(w.$7)) + '명')
										])),
									A2(
									$elm$html$Html$dt,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text('목표')
										])),
									A2(
									$elm$html$Html$dd,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(
											$elm$core$String$fromInt(
												$elm$core$List$length(w.cP)) + '개')
										]))
								])),
							A2(
							$elm$html$Html$button,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$disabled(model.Q.aV),
									$elm$html$Html$Events$onClick(model.cP)
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('목표 →')
								]))
						])),
					A2(
					$author$project$Ui$Common$panel,
					'조직 이름 수정',
					_List_fromArray(
						[
							A4(
							$author$project$Ui$Form$formView,
							model.Q,
							$author$project$Form$Action$Rename,
							'이름 저장',
							_List_fromArray(
								[
									A6($author$project$Ui$Form$inputField, model.Q, $author$project$Form$Action$Rename, '조직 이름', 'name', 'text', true),
									$author$project$Ui$Common$note('구성원과 목표, 기존 기록을 유지합니다. 다른 변경과 충돌하면 최신 상태를 확인한 뒤 다시 저장하세요.')
								]))
						])),
					A2(
					$elm$html$Html$section,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('panel danger-zone')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$h2,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('조직 삭제')
								])),
							A2(
							$elm$html$Html$p,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('구성원, 목표, 책임, 권한, 결과, 평가, 회고와 전략이 현재 워크스페이스에서 제거됩니다.')
								])),
							$author$project$Ui$Common$note('논리 삭제입니다. 원본 감사 이벤트는 파일·DB에 보존되며 완전히 지워지지 않습니다. 다른 조직은 삭제되지 않습니다.'),
							function () {
							var _v0 = model.a1;
							if (_v0.$ === 1) {
								return A2(
									$elm$html$Html$button,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('danger-outline'),
											$elm$html$Html$Attributes$disabled(model.Q.aV || (!model.Q.x)),
											$elm$html$Html$Events$onClick(model.dl)
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('삭제 확인 열기…')
										]));
							} else {
								var snapshot = _v0.a;
								return A2(
									$elm$html$Html$form,
									_List_fromArray(
										[
											$elm$html$Html$Events$onSubmit(
											model.Q.dQ($author$project$Form$Action$DeleteOrg)),
											A2(
											$elm$html$Html$Events$preventDefaultOn,
											'keydown',
											A2(
												$elm$json$Json$Decode$map,
												function (key) {
													return (key === 'Escape') ? _Utils_Tuple2(model.cj, true) : _Utils_Tuple2(model.c9, false);
												},
												A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string))),
											$elm$html$Html$Attributes$class('delete-confirmation'),
											A2($elm$html$Html$Attributes$attribute, 'aria-labelledby', 'delete-title')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$h3,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$id('delete-title')
												]),
											_List_fromArray(
												[
													$elm$html$Html$text(snapshot.c7 + ' 조직을 삭제할까요?')
												])),
											A2(
											$elm$html$Html$fieldset,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$disabled(model.Q.aV)
												]),
											_List_fromArray(
												[
													A2(
													$elm$html$Html$label,
													_List_Nil,
													_List_fromArray(
														[
															$elm$html$Html$text('확인하려면 조직 이름을 정확히 입력하세요'),
															A2(
															$elm$html$Html$input,
															_List_fromArray(
																[
																	$elm$html$Html$Attributes$id('delete-confirm'),
																	$elm$html$Html$Attributes$value(snapshot.aX),
																	$elm$html$Html$Events$onInput(model.cm),
																	$elm$html$Html$Attributes$autocomplete(false),
																	$elm$html$Html$Attributes$required(true)
																]),
															_List_Nil)
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
																	$elm$html$Html$Attributes$type_('button'),
																	$elm$html$Html$Attributes$class('secondary'),
																	$elm$html$Html$Events$onClick(model.cj)
																]),
															_List_fromArray(
																[
																	$elm$html$Html$text('취소')
																])),
															A2(
															$elm$html$Html$button,
															_List_fromArray(
																[
																	$elm$html$Html$Attributes$type_('submit'),
																	$elm$html$Html$Attributes$class('danger'),
																	$elm$html$Html$Attributes$disabled(
																	(!_Utils_eq(snapshot.aX, snapshot.c7)) || (!model.Q.x))
																]),
															_List_fromArray(
																[
																	$elm$html$Html$text(
																	model.Q.aV ? '삭제 중…' : '조직 삭제')
																]))
														]))
												]))
										]));
							}
						}()
						]))
				]));
	});
var $author$project$Ui$Guide$GuideStep = F5(
	function (done, title, instruction, page, target) {
		return {ak: done, bh: instruction, y: page, dV: target, bP: title};
	});
var $author$project$Ui$Guide$view = F2(
	function (model, w) {
		var reviewed = A2(
			$elm$core$List$any,
			function (r) {
				return (r.aF === 'demo-revenue') && ((r.a6.dL === 4) && ((!$elm$core$List$isEmpty(r.cZ)) && A2(
					$elm$core$List$any,
					function (d) {
						return (d.aK !== '') && (!_Utils_eq(d.cq, $elm$core$Maybe$Nothing));
					},
					r.ct)));
			},
			w.dC);
		var goal = function (key) {
			return $elm$core$List$head(
				A2(
					$elm$core$List$filter,
					A2(
						$elm$core$Basics$composeR,
						function ($) {
							return $.aF;
						},
						A2(
							$elm$core$Basics$composeR,
							function ($) {
								return $.bf;
							},
							$elm$core$Basics$eq('demo-' + key))),
					w.cP));
		};
		var ready = A2(
			$elm$core$Maybe$withDefault,
			false,
			A2(
				$elm$core$Maybe$map,
				A2(
					$elm$core$Basics$composeR,
					function ($) {
						return $.b2;
					},
					A2(
						$elm$core$Basics$composeR,
						function ($) {
							return $.co;
						},
						$elm$core$Basics$eq(1))),
				goal('launch')));
		var evaluated = A2(
			$elm$core$List$any,
			function (e) {
				return _Utils_eq(
					e.cF,
					$elm$core$Maybe$Just('demo-revenue')) && _Utils_eq(
					e.cG,
					$elm$core$Maybe$Just(4));
			},
			w.cH);
		var assigned = !_Utils_eq(
			$elm$core$Maybe$Nothing,
			A2(
				$elm$core$Maybe$andThen,
				function ($) {
					return $.aK;
				},
				goal('partners')));
		var active = function (key) {
			return A2(
				$elm$core$Maybe$withDefault,
				false,
				A2(
					$elm$core$Maybe$map,
					function ($) {
						return $.b_;
					},
					goal(key)));
		};
		var achieved = A2(
			$elm$core$Maybe$withDefault,
			false,
			A2(
				$elm$core$Maybe$map,
				A2(
					$elm$core$Basics$composeR,
					function ($) {
						return $.a6;
					},
					A2(
						$elm$core$Basics$composeR,
						function ($) {
							return $.dL;
						},
						$elm$core$Basics$eq(4))),
				goal('revenue')));
		var steps = _List_fromArray(
			[
				A5(
				$author$project$Ui$Guide$GuideStep,
				active('partners'),
				'01 · 빈 책임 자리 채우기',
				'파트너십 목표에 계약 권한을 가진 한유진을 최종 책임자로 지정하고 활성화하세요.',
				assigned ? 1 : 2,
				assigned ? 'goal-demo-partners' : 'owner-demo-partners'),
				A5(
				$author$project$Ui$Guide$GuideStep,
				active('launch'),
				'02 · 책임에 맞는 권한 주기',
				'이지원에게 채용 권한과 예산 30,000,000원을 부여하세요. 제품 출시 권한을 유지하고 신제품 출시 목표를 활성화하세요.',
				ready ? 1 : 3,
				ready ? 'goal-demo-launch' : 'authority-demo-product'),
				A5($author$project$Ui$Guide$GuideStep, achieved && evaluated, '03 · 결과에서 평가까지', '매출 실측값 50 (단위: 억원)과 보고자, 설명을 보고한 뒤 평가 기록을 누르세요.', 4, 'goal-demo-revenue'),
				A5($author$project$Ui$Guide$GuideStep, reviewed, '04 · 배움을 다음 결정으로', '매출 목표의 학습과 다음 결정, 담당자, 미래 기한을 기록하세요. 달성 결과와 평가가 함께 보존됩니다.', 5, 'review-form')
			]);
		var count = $elm$core$List$length(
			A2(
				$elm$core$List$filter,
				function ($) {
					return $.ak;
				},
				steps));
		return A2(
			$elm$html$Html$section,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('demo-guide')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('demo-heading')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_Nil,
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
											$elm$html$Html$text('DEMO · 가상 데이터')
										])),
									A2(
									$elm$html$Html$h2,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text('조직의 운영 흐름, 네 단계로 체험하세요')
										])),
									A2(
									$elm$html$Html$p,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text('6명 · 7개 목표 · 5가지 성과 상태. 실제 저장 상태로 진행률을 계산합니다.')
										]))
								])),
							A2(
							$elm$html$Html$span,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('guide-count')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(
									$elm$core$String$fromInt(count) + ' / 4 완료')
								]))
						])),
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('guide-toggle secondary'),
							$elm$html$Html$Events$onClick(model.dZ),
							A2(
							$elm$html$Html$Attributes$attribute,
							'aria-expanded',
							model.ac ? 'true' : 'false')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							model.ac ? '체험 가이드 접기' : '체험 가이드 열기')
						])),
					model.ac ? A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('guide-steps')
								]),
							A2(
								$elm$core$List$map,
								function (step_) {
									return A2(
										$elm$html$Html$article,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$classList(
												_List_fromArray(
													[
														_Utils_Tuple2('guide-step', true),
														_Utils_Tuple2('complete', step_.ak)
													]))
											]),
										_List_fromArray(
											[
												A2(
												$elm$html$Html$span,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('step-state')
													]),
												_List_fromArray(
													[
														$elm$html$Html$text(
														step_.ak ? '✓ 완료' : '○ 체험 대기')
													])),
												A2(
												$elm$html$Html$h3,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text(step_.bP)
													])),
												A2(
												$elm$html$Html$p,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text(step_.bh)
													])),
												A2(
												$elm$html$Html$button,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('secondary'),
														$elm$html$Html$Attributes$disabled(model.aV),
														$elm$html$Html$Events$onClick(
														A2(model.cN, step_.y, step_.dV))
													]),
												_List_fromArray(
													[
														$elm$html$Html$text(
														step_.ak ? '다시 살펴보기 →' : '이 단계 진행 →')
													]))
											]));
								},
								steps)),
							$author$project$Ui$Common$note('전사 성장 지수는 하위 목표의 자동 합계가 아닌 별도 보고 KPI입니다. 초기 진단과 결과 샘플은 의도한 가상 체험 사례입니다. 감사 시각은 실제 가져온 시각입니다.'),
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
											$elm$html$Html$Attributes$disabled(model.aV),
											$elm$html$Html$Events$onClick(
											A2(model.cN, 2, 'responsibility-graph'))
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('관계 그래프 →')
										])),
									A2(
									$elm$html$Html$button,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('secondary'),
											$elm$html$Html$Attributes$disabled(model.aV),
											$elm$html$Html$Events$onClick(
											A2(model.cN, 5, 'audit-history'))
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('감사 기록 →')
										]))
								]))
						])) : $elm$html$Html$text('')
				]));
	});
var $author$project$Main$workspaceView = function (model) {
	return A2(
		$author$project$Remote$view,
		model.bW,
		function (w) {
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						(w.a2 && (model.y !== 6)) ? A2(
						$author$project$Ui$Guide$view,
						{
							aV: $author$project$Main$busy(model),
							cN: $author$project$Main$Guide,
							ac: model.ac,
							dZ: $author$project$Main$ToggleGuide
						},
						w) : $elm$html$Html$text(''),
						function () {
						var _v0 = model.y;
						switch (_v0) {
							case 1:
								return A2(
									$author$project$Page$Goals$view,
									{
										a4: $author$project$Main$goalDraft(model),
										aC: $author$project$Main$EditGoal,
										Z: model.Z,
										Q: $author$project$Main$formConfig(model),
										dz: $author$project$Main$Guide(4)
									},
									w);
							case 2:
								return A2(
									$author$project$Page$Responsibility$view,
									{
										Q: $author$project$Main$formConfig(model)
									},
									w);
							case 3:
								return A2(
									$author$project$Page$Authorities$view,
									{
										Q: $author$project$Main$formConfig(model)
									},
									w);
							case 4:
								return A2(
									$author$project$Page$Results$view,
									{
										Q: $author$project$Main$formConfig(model),
										cP: $author$project$Main$Guide(1)
									},
									w);
							case 5:
								return A2(
									$author$project$Page$Learning$view,
									{
										a4: $author$project$Main$reviewDraft(model),
										aC: $author$project$Main$EditReview,
										Q: $author$project$Main$formConfig(model)
									},
									w);
							case 6:
								return A2(
									$author$project$Page$Settings$view,
									{
										cj: $author$project$Main$CloseDelete,
										cm: $author$project$Main$ConfirmDelete,
										a1: model.a1,
										Q: $author$project$Main$formConfig(model),
										cP: A2($author$project$Main$Navigate, 1, model.au),
										c9: $author$project$Main$NoOp,
										dl: $author$project$Main$OpenDelete
									},
									w);
							default:
								return $elm$html$Html$text('');
						}
					}()
					]));
		});
};
var $author$project$Main$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$a,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$href('#main-content'),
						$elm$html$Html$Attributes$class('skip-link')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('본문으로 이동')
					])),
				A2(
				$elm$html$Html$aside,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$a,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('brand'),
								$elm$html$Html$Attributes$href('/')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('◈ '),
								A2(
								$elm$html$Html$strong,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('my org')
									])),
								A2(
								$elm$html$Html$span,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('CLARITY → ACTION → LEARNING')
									]))
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('workspace')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(
								function () {
									var _v0 = model.bW;
									if (_v0.$ === 1) {
										var w = _v0.a;
										return w.bs.c7;
									} else {
										return '조직 워크스페이스';
									}
								}())
							])),
						A2(
						$elm$html$Html$nav,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$attribute, 'aria-label', '주요 화면')
							]),
						A2(
							$elm$core$List$map,
							function (page) {
								return A2(
									$elm$html$Html$button,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$type_('button'),
											$elm$html$Html$Attributes$classList(
											_List_fromArray(
												[
													_Utils_Tuple2(
													'selected',
													_Utils_eq(model.y, page))
												])),
											A2(
											$elm$html$Html$Attributes$attribute,
											'aria-current',
											_Utils_eq(model.y, page) ? 'page' : 'false'),
											$elm$html$Html$Attributes$disabled(
											$author$project$Main$busy(model) || ((!(!page)) && _Utils_eq(model.au, $elm$core$Maybe$Nothing))),
											$elm$html$Html$Events$onClick(
											A2(
												$author$project$Main$Navigate,
												page,
												(!page) ? $elm$core$Maybe$Nothing : model.au))
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(
											$author$project$Page$pageName(page))
										]));
							},
							_List_fromArray(
								[0, 1, 2, 3, 4, 5]))),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('aside-foot')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$span,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('dot')
									]),
								_List_Nil),
								$elm$html$Html$text('명확한 상태, 예측 가능한 변화'),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('결과를 정의하고'),
										A2($elm$html$Html$br, _List_Nil, _List_Nil),
										$elm$html$Html$text('함께 배우는 조직.')
									])),
								A2(
								$elm$html$Html$small,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Elm UI · Haskell API')
									]))
							]))
					])),
				A2(
				$elm$html$Html$main_,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$id('main-content'),
						$elm$html$Html$Attributes$tabindex(-1)
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$header,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$span,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('eyebrow')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('WORKSPACE / MY ORG')
											])),
										A2(
										$elm$html$Html$h1,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text(
												$author$project$Page$pageName(model.y))
											])),
										A2(
										$elm$html$Html$p,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('목표 → 책임 → 권한 → 결과 → 학습. 다음 행동을 명확하게.')
											]))
									])),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('header-actions')
									]),
								_List_fromArray(
									[
										(!_Utils_eq(model.au, $elm$core$Maybe$Nothing)) ? A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('secondary'),
												$elm$html$Html$Attributes$disabled(
												$author$project$Main$busy(model)),
												$elm$html$Html$Events$onClick(
												A2($author$project$Main$Navigate, 6, model.au))
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('조직 설정')
											])) : $elm$html$Html$text(''),
										A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('secondary'),
												$elm$html$Html$Attributes$disabled(
												$author$project$Main$busy(model)),
												$elm$html$Html$Events$onClick($author$project$Main$Refresh)
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('↻ 새로고침')
											]))
									]))
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$id('notice'),
								$elm$html$Html$Attributes$classList(
								_List_fromArray(
									[
										_Utils_Tuple2('error', model.p)
									])),
								A2(
								$elm$html$Html$Attributes$attribute,
								'role',
								model.p ? 'alert' : 'status'),
								A2($elm$html$Html$Attributes$attribute, 'aria-live', 'polite')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(model.s)
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('sync-state'),
								A2($elm$html$Html$Attributes$attribute, 'role', 'status')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$span,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('dot')
									]),
								_List_Nil),
								$elm$html$Html$text(
								$author$project$Main$busy(model) ? '저장 중 · 완료 후 다음 작업을 진행하세요' : (model.x ? '최신 상태 · 입력은 화면을 이동해도 유지됩니다' : (model.N ? '최신 상태를 확인하고 있습니다…' : '최신 상태 확인 실패 · 새로고침해 주세요')))
							])),
						(!model.y) ? $author$project$Page$Organizations$view(
						{
							Q: $author$project$Main$formConfig(model),
							dk: function (org) {
								return A2(
									$author$project$Main$Navigate,
									1,
									$elm$core$Maybe$Just(org));
							},
							T: model.T,
							dI: function (org) {
								return A2(
									$author$project$Main$Navigate,
									6,
									$elm$core$Maybe$Just(org));
							}
						}) : $author$project$Main$workspaceView(model),
						A2(
						$elm$html$Html$footer,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('기록된 권한은 실제 시스템 접근 제어와 연결되지 않습니다. 감사 기록의 행위자는 인증된 신원 증명이 아닙니다.')
							]))
					]))
			]));
};
var $author$project$Main$main = $elm$browser$Browser$element(
	{
		cV: $author$project$Main$init,
		dR: $elm$core$Basics$always($elm$core$Platform$Sub$none),
		d1: $author$project$Main$update,
		d2: $author$project$Main$view
	});
_Platform_export({'Main':{'init':$author$project$Main$main(
	A2(
		$elm$json$Json$Decode$andThen,
		function (today) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (seed) {
					return A2(
						$elm$json$Json$Decode$andThen,
						function (deadline) {
							return $elm$json$Json$Decode$succeed(
								{cq: deadline, bG: seed, bR: today});
						},
						A2($elm$json$Json$Decode$field, 'deadline', $elm$json$Json$Decode$string));
				},
				A2($elm$json$Json$Decode$field, 'seed', $elm$json$Json$Decode$string));
		},
		A2($elm$json$Json$Decode$field, 'today', $elm$json$Json$Decode$string)))(0)}});}(this));