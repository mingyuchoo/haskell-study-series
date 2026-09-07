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
	if (region.a7.ar === region.bs.ar)
	{
		return 'on line ' + region.a7.ar;
	}
	return 'on lines ' + region.a7.ar + ' through ' + region.bs.ar;
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
		impl.dy,
		impl.eV,
		impl.eF,
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
		dH: func(record.dH),
		a8: record.a8,
		a4: record.a4
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
		var message = !tag ? value : tag < 3 ? value.a : value.dH;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.a8;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.a4) && event.preventDefault(),
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
		impl.dy,
		impl.eV,
		impl.eF,
		function(sendToApp, initialModel) {
			var view = impl.eW;
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
		impl.dy,
		impl.eV,
		impl.eF,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.a6 && impl.a6(sendToApp)
			var view = impl.eW;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.cF);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.ci) && (_VirtualDom_doc.title = title = doc.ci);
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
	var onUrlChange = impl.d_;
	var onUrlRequest = impl.d$;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		a6: function(sendToApp)
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
							&& curr.bX === next.bX
							&& curr.bC === next.bC
							&& curr.bU.a === next.bU.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		dy: function(flags)
		{
			return A3(impl.dy, flags, _Browser_getUrl(), key);
		},
		eW: impl.eW,
		eV: impl.eV,
		eF: impl.eF
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
		? { dt: 'hidden', cN: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { dt: 'mozHidden', cN: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { dt: 'msHidden', cN: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { dt: 'webkitHidden', cN: 'webkitvisibilitychange' }
		: { dt: 'hidden', cN: 'visibilitychange' };
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
		b5: _Browser_getScene(),
		ck: {
			aD: _Browser_window.pageXOffset,
			ag: _Browser_window.pageYOffset,
			cn: _Browser_doc.documentElement.clientWidth,
			bB: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		cn: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		bB: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
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
			b5: {
				cn: node.scrollWidth,
				bB: node.scrollHeight
			},
			ck: {
				aD: node.scrollLeft,
				ag: node.scrollTop,
				cn: node.clientWidth,
				bB: node.clientHeight
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
			b5: _Browser_getScene(),
			ck: {
				aD: x,
				ag: y,
				cn: _Browser_doc.documentElement.clientWidth,
				bB: _Browser_doc.documentElement.clientHeight
			},
			c9: {
				aD: x + rect.left,
				ag: y + rect.top,
				cn: rect.width,
				bB: rect.height
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
			callback(toTask(request.an.a(response)));
		}

		var xhr = new XMLHttpRequest();
		xhr.addEventListener('error', function() { done($elm$http$Http$NetworkError_); });
		xhr.addEventListener('timeout', function() { done($elm$http$Http$Timeout_); });
		xhr.addEventListener('load', function() { done(_Http_toResponse(request.an.b, xhr)); });
		$elm$core$Maybe$isJust(request.eQ) && _Http_track(router, xhr, request.eQ.a);

		try {
			xhr.open(request.dI, request.aB, true);
		} catch (e) {
			return done($elm$http$Http$BadUrl_(request.aB));
		}

		_Http_configureRequest(xhr, request);

		request.cF.a && xhr.setRequestHeader('Content-Type', request.cF.a);
		xhr.send(request.cF.b);

		return function() { xhr.c = true; xhr.abort(); };
	});
});


// CONFIGURE

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.dr; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}
	xhr.timeout = request.eL.a || 0;
	xhr.responseType = request.an.d;
	xhr.withCredentials = request.ct;
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
		aB: xhr.responseURL,
		eA: xhr.status,
		eB: xhr.statusText,
		dr: _Http_parseHeaders(xhr.getAllResponseHeaders())
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
			et: event.loaded,
			b9: event.total
		}))));
	});
	xhr.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Receiving({
			ea: event.loaded,
			b9: event.lengthComputable ? $elm$core$Maybe$Just(event.total) : $elm$core$Maybe$Nothing
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
				$elm$core$Elm$JsArray$length(builder.h),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.h);
		} else {
			var treeLen = builder.f * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.i) : builder.i;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.f);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.h) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.h);
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
					{i: nodeList, f: (len / $elm$core$Array$branchFactor) | 0, h: tail});
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
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
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
		return {bw: fragment, bC: host, bS: path, bU: port_, bX: protocol, d9: query};
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
var $elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $author$project$App$Agents$init = {q: $elm$core$Dict$empty, I: $elm$core$Dict$empty, aJ: false, U: $elm$core$Dict$empty};
var $author$project$App$Discovery$init = {P: $elm$core$Dict$empty, q: $elm$core$Dict$empty, I: $elm$core$Dict$empty, aJ: false};
var $author$project$App$Drafts$init = {bo: $elm$core$Maybe$Nothing, q: $elm$core$Dict$empty, z: $elm$core$Dict$empty, ab: $elm$core$Dict$empty, D: $elm$core$Dict$empty, b7: 0};
var $author$project$Page$Organizations = 0;
var $author$project$Ui$Activity$init = {dl: '', dB: '', d9: '', b1: $elm$core$Maybe$Nothing, eU: ''};
var $author$project$Ui$ResponsibilityGraph$init = {Y: true, d9: '', es: $elm$core$Maybe$Nothing, av: false, L: false, F: 1};
var $author$project$App$PageState$init = {X: $author$project$Ui$Activity$init, aF: $elm$core$Maybe$Nothing, ap: $author$project$Ui$ResponsibilityGraph$init, aI: false, as: $elm$core$Dict$empty, ac: 0, aN: '', aO: 'active', aU: $elm$core$Maybe$Nothing};
var $author$project$App$Session$Idle = {$: 0};
var $author$project$Remote$Loading = {$: 0};
var $author$project$App$Session$init = {ao: false, aL: $elm$core$Maybe$Nothing, aM: $author$project$Remote$Loading, aQ: 0, au: $author$project$App$Session$Idle, aw: true, bb: $author$project$Remote$Loading};
var $author$project$App$Model$init = function (flags) {
	return {N: $author$project$App$Agents$init, Z: $author$project$App$Discovery$init, bt: false, aH: flags, w: $author$project$App$Drafts$init, bN: '', bR: $author$project$App$PageState$init, ad: $author$project$App$Session$init};
};
var $author$project$App$Effect$LoadAgents = F2(
	function (a, b) {
		return {$: 5, a: a, b: b};
	});
var $author$project$App$Effect$LoadDiscovery = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
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
var $elm$core$Basics$neq = _Utils_notEqual;
var $author$project$App$Effect$LoadOrganizations = function (a) {
	return {$: 0, a: a};
};
var $author$project$App$Effect$LoadWorkspace = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $author$project$App$Session$refresh = function (state) {
	var token = state.aQ + 1;
	var next = _Utils_update(
		state,
		{ao: false, aQ: token, aw: true});
	return _Utils_Tuple2(
		next,
		_List_fromArray(
			[
				function () {
				var _v0 = state.aL;
				if (_v0.$ === 1) {
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
			{aJ: loading});
	});
var $author$project$App$Update$setDiscoveryLoading = F2(
	function (loading, state) {
		return _Utils_update(
			state,
			{aJ: loading});
	});
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$App$Update$refresh = function (model) {
	var forms = model.w;
	var _v0 = $author$project$App$Session$refresh(model.ad);
	var session = _v0.a;
	var effects = _v0.b;
	return _Utils_Tuple2(
		_Utils_update(
			model,
			{
				N: A2(
					$author$project$App$Update$setAgentsLoading,
					!_Utils_eq(session.aL, $elm$core$Maybe$Nothing),
					model.N),
				Z: A2(
					$author$project$App$Update$setDiscoveryLoading,
					!_Utils_eq(session.aL, $elm$core$Maybe$Nothing),
					model.Z),
				w: _Utils_update(
					forms,
					{bo: $elm$core$Maybe$Nothing}),
				ad: session
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
								A2($author$project$App$Effect$LoadDiscovery, session.aQ, org),
								A2($author$project$App$Effect$LoadAgents, session.aQ, org)
							]);
					},
					session.aL))));
};
var $author$project$App$Update$init = function (flags) {
	return $author$project$App$Update$refresh(
		$author$project$App$Model$init(flags));
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $author$project$App$Update$GotAgents = F3(
	function (a, b, c) {
		return {$: 31, a: a, b: b, c: c};
	});
var $author$project$App$Update$GotDiscovery = F3(
	function (a, b, c) {
		return {$: 23, a: a, b: b, c: c};
	});
var $author$project$App$Update$GotOrganizations = F2(
	function (a, b) {
		return {$: 7, a: a, b: b};
	});
var $author$project$App$Update$GotWorkspace = F2(
	function (a, b) {
		return {$: 8, a: a, b: b};
	});
var $author$project$App$Update$NoOp = {$: 38};
var $author$project$App$Update$Saved = F3(
	function (a, b, c) {
		return {$: 13, a: a, b: b, c: c};
	});
var $author$project$App$Update$SavedAgents = F3(
	function (a, b, c) {
		return {$: 35, a: a, b: b, c: c};
	});
var $author$project$App$Update$SavedDiscovery = F3(
	function (a, b, c) {
		return {$: 28, a: a, b: b, c: c};
	});
var $author$project$Domain$Agent$Snapshot = F5(
	function (version, agents, drafts, diagnostics, draftDiagnostics) {
		return {N: agents, c3: diagnostics, c7: draftDiagnostics, q: drafts, aC: version};
	});
var $author$project$Domain$Diagnostic = F5(
	function (severity, code, message, subject, details) {
		return {cQ: code, c2: details, dH: message, ew: severity, eD: subject};
	});
var $author$project$Api$Decode$andMap = $elm$json$Json$Decode$map2($elm$core$Basics$apR);
var $author$project$Api$Decode$field = F2(
	function (name, decoder) {
		return $author$project$Api$Decode$andMap(
			A2($elm$json$Json$Decode$field, name, decoder));
	});
var $elm$json$Json$Decode$list = _Json_decodeList;
var $elm$json$Json$Decode$string = _Json_decodeString;
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
var $elm$json$Json$Decode$int = _Json_decodeInt;
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
												return {cv: approval, de: evidence, dq: handoffTo, du: id, dz: inputs, dG: level, dO: name, d2: outputs, ex: sourceWorkflow, ez: status, eJ: task, eP: tools};
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
var $author$project$Domain$Agent$Permission = function (a) {
	return {$: 1, a: a};
};
var $author$project$Domain$Agent$Person = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
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
var $elm$json$Json$Decode$fail = _Json_fail;
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
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
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
					$elm$http$Http$BadStatus(metadata.eA));
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
		return {b$: reqs, ce: subs};
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
							var _v4 = req.eQ;
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
			A3($elm$http$Http$updateReqs, router, cmds, state.b$));
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
					state.ce)));
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
					ct: r.ct,
					cF: r.cF,
					an: A2(_Http_mapExpect, func, r.an),
					dr: r.dr,
					dI: r.dI,
					eL: r.eL,
					eQ: r.eQ,
					aB: r.aB
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
			{ct: false, cF: r.cF, an: r.an, dr: r.dr, dI: r.dI, eL: r.eL, eQ: r.eQ, aB: r.aB}));
};
var $elm$http$Http$get = function (r) {
	return $elm$http$Http$request(
		{cF: $elm$http$Http$emptyBody, an: r.an, dr: _List_Nil, dI: 'GET', eL: $elm$core$Maybe$Nothing, eQ: $elm$core$Maybe$Nothing, aB: r.aB});
};
var $elm$url$Url$percentEncode = _Url_percentEncode;
var $author$project$Api$Path$orgPath = F2(
	function (org, tail) {
		return '/api/organizations/' + ($elm$url$Url$percentEncode(org) + ((tail === '') ? '' : ('/' + tail)));
	});
var $author$project$Api$Http$agents = F2(
	function (org, onResult) {
		return $elm$http$Http$get(
			{
				an: A2($elm$http$Http$expectJson, onResult, $author$project$Api$Agents$decoder),
				aB: A2($author$project$Api$Path$orgPath, org, 'agents')
			});
	});
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
var $author$project$Domain$Discovery$Snapshot = F2(
	function (version, discovery) {
		return {Z: discovery, aC: version};
	});
var $author$project$Domain$Discovery$Document = F5(
	function (scope, asOf, observations, workflows, review) {
		return {cz: asOf, dZ: observations, b1: review, eq: scope, eX: workflows};
	});
var $author$project$Domain$Discovery$Review = F2(
	function (status, note) {
		return {dY: note, ez: status};
	});
var $author$project$Domain$Discovery$Observation = F5(
	function (id, subject, detail, status, evidence) {
		return {o: detail, de: evidence, du: id, ez: status, eD: subject};
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
															return {cv: approval, cw: approvalPermission, cx: approvalPerson, de: evidence, dp: handoff, bA: handoffWorkflows, du: id, dz: inputs, dO: name, d2: outputs, eo: role, ep: rolePerson, ez: status, eP: tools, eR: trigger};
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
var $author$project$Api$Http$discovery = F2(
	function (org, onResult) {
		return $elm$http$Http$get(
			{
				an: A2($elm$http$Http$expectJson, onResult, $author$project$Api$Discovery$decoder),
				aB: A2($author$project$Api$Path$orgPath, org, 'discovery')
			});
	});
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(0),
				entries));
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
			_Json_emptyObject(0),
			pairs));
};
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$Api$Agents$encodeRole = function (role) {
	return $elm$json$Json$Encode$object(
		_Utils_ap(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'id',
					$elm$json$Json$Encode$string(role.du)),
					_Utils_Tuple2(
					'name',
					$elm$json$Json$Encode$string(role.dO)),
					_Utils_Tuple2(
					'task',
					$elm$json$Json$Encode$string(role.eJ)),
					_Utils_Tuple2(
					'inputs',
					$elm$json$Json$Encode$string(role.dz)),
					_Utils_Tuple2(
					'outputs',
					$elm$json$Json$Encode$string(role.d2)),
					_Utils_Tuple2(
					'tools',
					A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, role.eP)),
					_Utils_Tuple2(
					'permissionLevel',
					$elm$json$Json$Encode$string(role.dG)),
					_Utils_Tuple2(
					'handoffTo',
					A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, role.dq)),
					_Utils_Tuple2(
					'status',
					$elm$json$Json$Encode$string(role.ez)),
					_Utils_Tuple2(
					'evidence',
					$elm$json$Json$Encode$string(role.de))
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
						role.ex),
						A2(
						$elm$core$Maybe$map,
						function (approval) {
							return _Utils_Tuple2(
								'approvalBy',
								function () {
									if (!approval.$) {
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
						role.cv)
					]))));
};
var $author$project$Api$Agents$encode = $elm$json$Json$Encode$list($author$project$Api$Agents$encodeRole);
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
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
						_Utils_Tuple2('id', w.du),
						_Utils_Tuple2('name', w.dO),
						_Utils_Tuple2('role', w.eo),
						_Utils_Tuple2('trigger', w.eR),
						_Utils_Tuple2('inputs', w.dz),
						_Utils_Tuple2('tools', w.eP),
						_Utils_Tuple2('outputs', w.d2),
						_Utils_Tuple2('handoff', w.dp),
						_Utils_Tuple2('approval', w.cv),
						_Utils_Tuple2('status', w.ez),
						_Utils_Tuple2('evidence', w.de)
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
						w.ep),
						A2(
						$elm$core$Maybe$map,
						function (v) {
							return _Utils_Tuple2(
								'approvalPerson',
								$elm$json$Json$Encode$string(v));
						},
						w.cx),
						A2(
						$elm$core$Maybe$map,
						function (v) {
							return _Utils_Tuple2(
								'approvalPermission',
								$elm$json$Json$Encode$string(v));
						},
						w.cw),
						$elm$core$List$isEmpty(w.bA) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
						_Utils_Tuple2(
							'handoffWorkflows',
							A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, w.bA)))
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
				$elm$json$Json$Encode$string(doc.eq)),
				_Utils_Tuple2(
				'asOf',
				$elm$json$Json$Encode$string(doc.cz)),
				_Utils_Tuple2(
				'observations',
				A2(
					$elm$json$Json$Encode$list,
					function (o) {
						return $author$project$Api$Discovery$strings(
							_List_fromArray(
								[
									_Utils_Tuple2('id', o.du),
									_Utils_Tuple2('subject', o.eD),
									_Utils_Tuple2('detail', o.o),
									_Utils_Tuple2('status', o.ez),
									_Utils_Tuple2('evidence', o.de)
								]));
					},
					doc.dZ)),
				_Utils_Tuple2(
				'workflows',
				A2($elm$json$Json$Encode$list, $author$project$Api$Discovery$encodeWorkflow, doc.eX)),
				_Utils_Tuple2(
				'review',
				$author$project$Api$Discovery$strings(
					_List_fromArray(
						[
							_Utils_Tuple2('status', doc.b1.ez),
							_Utils_Tuple2('note', doc.b1.dY)
						])))
			]));
};
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
var $elm$browser$Browser$Dom$focus = _Browser_call('focus');
var $elm$json$Json$Encode$int = _Json_wrap;
var $author$project$Domain$Summary = F4(
	function (organization, demo, peopleCount, goalCount) {
		return {bp: demo, dm: goalCount, bQ: organization, d5: peopleCount};
	});
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $author$project$Domain$Organization = F3(
	function (id, name, createdAt) {
		return {cV: createdAt, du: id, dO: name};
	});
var $elm$json$Json$Decode$map3 = _Json_map3;
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
			an: A2(
				$elm$http$Http$expectJson,
				onResult,
				$elm$json$Json$Decode$list($author$project$Api$Decode$summaryDecoder)),
			aB: '/api/organizations'
		});
};
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
				cF: $elm$http$Http$jsonBody(body),
				an: A2(
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
											content)) + (' (' + ($elm$core$String$fromInt(metadata.eA) + ')')));
							default:
								return $elm$core$Result$Ok(0);
						}
					}),
				dr: _List_Nil,
				dI: method,
				eL: $elm$core$Maybe$Just(30000),
				eQ: $elm$core$Maybe$Nothing,
				aB: path
			});
	});
var $author$project$Domain$ReviewWarning = F2(
	function (id, warnings) {
		return {du: id, cl: warnings};
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
												return {cB: authorities, cR: compiler, cY: decisionShare, bp: demo, c8: edges, dd: events, $7: goals, bQ: organization, d4: people, ek: reviewWarnings, el: reviews, aC: version};
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
var $author$project$Domain$Audit = F7(
	function (seq, at, actor, description, evaluatedGoal, evaluatedStatus, activity) {
		return {X: activity, cp: actor, cA: at, c1: description, db: evaluatedGoal, dc: evaluatedStatus, eu: seq};
	});
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $author$project$Api$Activity$empty = {o: '', aP: $elm$core$Maybe$Nothing, bY: 'null', b2: $elm$core$Maybe$Nothing, a9: '', cg: '', ch: ''};
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $elm$core$String$fromFloat = _String_fromNumber;
var $elm$core$Set$Set_elm_builtin = $elm$core$Basics$identity;
var $elm$core$Set$empty = $elm$core$Dict$empty;
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0;
		return A3($elm$core$Dict$insert, key, 0, dict);
	});
var $elm$core$Set$fromList = function (list) {
	return A3($elm$core$List$foldl, $elm$core$Set$insert, $elm$core$Set$empty, list);
};
var $elm$json$Json$Decode$index = _Json_decodeIndex;
var $elm$json$Json$Encode$null = _Json_encodeNull;
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
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
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
var $elm$core$Basics$round = _Basics_round;
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
				{a9: tag, cg: ident, ch: kind});
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
				o: A2(objectAt, 0, 'name') + (' · ' + A2(objectAt, 0, 'role'))
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
					o: field('name')
				});
		case 'OrganizationRenamed':
			var event = pair('organization');
			return _Utils_update(
				event,
				{
					o: detail(
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
					o: field('name') + (' · ' + field('role'))
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
					aP: A2(
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
					o: field('description')
				});
		case 'OwnerAssigned':
			var event = pair('goal');
			return _Utils_update(
				event,
				{
					aP: $elm$core$Maybe$Just(
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
					o: '예산 ' + (detail(
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
					o: $author$project$Api$Activity$permissionLabel(
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
					o: '값 ' + (detail(
						A2(
							at,
							1,
							A2($elm$json$Json$Decode$field, 'value', number))) + (' · ' + A2(objectAt, 1, 'note'))),
					aP: A2(
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
					o: $author$project$Api$Activity$statusLabel(
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
					o: field('note') + (' · 결정 ' + (detail(
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
					b2: $elm$core$Maybe$Just(
						field('id'))
				});
		case 'StrategyChanged':
			var event = pair('goal');
			return _Utils_update(
				event,
				{
					o: detail(
						A2(at, 1, $elm$json$Json$Decode$string))
				});
		case 'DiscoverySaved':
			var event = A2(target, 'survey', '');
			return _Utils_update(
				event,
				{
					o: '범위: ' + (field('scope') + (' · 업무 ' + (detail(
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
					o: '역할 ' + (detail(
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
				{a9: tag});
	}
};
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
					bY: A2($elm$json$Json$Encode$encode, 2, raw)
				});
		}(
			$author$project$Api$Activity$interpret(
				$author$project$Api$Activity$unscoped(raw)));
	},
	$elm$json$Json$Decode$value);
var $elm$core$Result$toMaybe = function (result) {
	if (!result.$) {
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
var $author$project$Domain$Authority = F5(
	function (owner, budgetLimit, canHire, canChangePrice, canApprove) {
		return {cI: budgetLimit, cK: canApprove, cL: canChangePrice, cM: canHire, a3: owner};
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
		return {c3: diagnostics, I: errors, cl: warnings};
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
		return {dl: from, dB: kind, eM: to};
	});
var $author$project$Domain$Node = F2(
	function (tag, contents) {
		return {cT: contents, a9: tag};
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
		return {cU: coverage, d7: possibleCause};
	});
var $author$project$Domain$GoalView = F7(
	function (goal, owner, active, evaluation, analysis, results, strategies) {
		return {bc: active, cu: analysis, bu: evaluation, a_: goal, a3: owner, ei: results, eC: strategies};
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
		return {dC: latestValue, d8: progress, ez: status};
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
		return {cE: baseline, cW: deadline, c1: description, du: id, dJ: metric, eg: requiredBudget, eh: requiredPermissions, eI: target};
	});
var $author$project$Domain$Metric = F4(
	function (id, name, unit, direction) {
		return {c4: direction, du: id, dO: name, eT: unit};
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
		return {dY: note, ed: reportedAt, ee: reportedBy, ba: value};
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
var $author$project$Domain$Person = F7(
	function (id, name, role, reportsTo, department, email, active) {
		return {bc: active, c0: department, da: email, du: id, dO: name, ef: reportsTo, eo: role};
	});
var $author$project$Api$Decode$personDecoder = A2(
	$author$project$Api$Decode$andMap,
	A2(
		$elm$json$Json$Decode$andThen,
		function (status) {
			if (status.$ === 1) {
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
		return {cZ: decisions, bu: evaluation, a_: goal, ds: heldAt, du: id, dD: learnings, dY: note};
	});
var $author$project$Domain$Decision = F3(
	function (text, owner, deadline) {
		return {cW: deadline, a3: owner, eK: text};
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
				an: A2($elm$http$Http$expectJson, onResult, $author$project$Api$Decode$workspaceDecoder),
				aB: A2($author$project$Api$Path$orgPath, org, 'dashboard')
			});
	});
var $author$project$Main$perform = function (effect) {
	switch (effect.$) {
		case 0:
			var token = effect.a;
			return $author$project$Api$Http$organizations(
				A2(
					$elm$core$Basics$composeR,
					$elm$core$Result$mapError($author$project$Api$Http$errorText),
					$author$project$App$Update$GotOrganizations(token)));
		case 1:
			var token = effect.a;
			var org = effect.b;
			return A2(
				$author$project$Api$Http$workspace,
				org,
				A2(
					$elm$core$Basics$composeR,
					$elm$core$Result$mapError($author$project$Api$Http$errorText),
					$author$project$App$Update$GotWorkspace(token)));
		case 2:
			var token = effect.a;
			var action = effect.b;
			var method = effect.c;
			var path = effect.d;
			var body = effect.e;
			return A4(
				$author$project$Api$Http$send,
				A2($author$project$App$Update$Saved, token, action),
				method,
				path,
				body);
		case 3:
			var token = effect.a;
			var org = effect.b;
			return A2(
				$author$project$Api$Http$discovery,
				org,
				A2(
					$elm$core$Basics$composeR,
					$elm$core$Result$mapError($author$project$Api$Http$errorText),
					A2($author$project$App$Update$GotDiscovery, token, org)));
		case 4:
			var token = effect.a;
			var org = effect.b;
			var snapshot = effect.c;
			return A4(
				$author$project$Api$Http$send,
				A2($author$project$App$Update$SavedDiscovery, token, org),
				'POST',
				A2($author$project$Api$Path$orgPath, org, 'discovery'),
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'expectedVersion',
							$elm$json$Json$Encode$int(snapshot.aC)),
							_Utils_Tuple2(
							'discovery',
							$author$project$Api$Discovery$encode(snapshot.Z))
						])));
		case 5:
			var token = effect.a;
			var org = effect.b;
			return A2(
				$author$project$Api$Http$agents,
				org,
				A2(
					$elm$core$Basics$composeR,
					$elm$core$Result$mapError($author$project$Api$Http$errorText),
					A2($author$project$App$Update$GotAgents, token, org)));
		case 6:
			var token = effect.a;
			var org = effect.b;
			var version = effect.c;
			var agents = effect.d;
			return A4(
				$author$project$Api$Http$send,
				A2($author$project$App$Update$SavedAgents, token, org),
				'POST',
				A2($author$project$Api$Path$orgPath, org, 'agents'),
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'expectedVersion',
							$elm$json$Json$Encode$int(version)),
							_Utils_Tuple2(
							'agents',
							$author$project$Api$Agents$encode(agents))
						])));
		default:
			var target = effect.a;
			return A2(
				$elm$core$Task$attempt,
				$elm$core$Basics$always($author$project$App$Update$NoOp),
				$elm$browser$Browser$Dom$focus(target));
	}
};
var $author$project$Main$runEffects = function (_v0) {
	var model = _v0.a;
	var effects = _v0.b;
	return _Utils_Tuple2(
		model,
		$elm$core$Platform$Cmd$batch(
			A2($elm$core$List$map, $author$project$Main$perform, effects)));
};
var $author$project$Page$ActivityLog = 11;
var $author$project$Domain$Discovery$AddObservation = function (a) {
	return {$: 2, a: a};
};
var $author$project$Domain$Discovery$AddWorkflow = function (a) {
	return {$: 4, a: a};
};
var $author$project$Page$Authorities = 8;
var $author$project$Form$Goal$Direction = {$: 4};
var $author$project$App$Update$EditAgents = function (a) {
	return {$: 32, a: a};
};
var $author$project$App$Update$EditDiscovery = function (a) {
	return {$: 24, a: a};
};
var $author$project$App$Update$EditGoal = F2(
	function (a, b) {
		return {$: 10, a: a, b: b};
	});
var $author$project$App$Update$EditReview = F2(
	function (a, b) {
		return {$: 11, a: a, b: b};
	});
var $author$project$App$Effect$FocusElement = function (a) {
	return {$: 7, a: a};
};
var $author$project$App$Update$Guide = F2(
	function (a, b) {
		return {$: 18, a: a, b: b};
	});
var $author$project$Domain$Agent$Import = function (a) {
	return {$: 0, a: a};
};
var $author$project$Form$Goal$MetricId = {$: 3};
var $author$project$Form$Goal$MetricName = {$: 1};
var $author$project$App$Update$OpenPerson = function (a) {
	return {$: 22, a: a};
};
var $author$project$Page$People = 5;
var $author$project$Page$Responsibility = 7;
var $author$project$Page$Reviews = 10;
var $author$project$Form$Goal$Unit = {$: 2};
var $author$project$App$Drafts$advanceSerial = function (state) {
	return _Utils_update(
		state,
		{b7: state.b7 + 1});
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
var $author$project$App$Session$busy = function (state) {
	return !_Utils_eq(state.au, $author$project$App$Session$Idle);
};
var $author$project$App$Update$busy = function (model) {
	return $author$project$App$Session$busy(model.ad);
};
var $author$project$App$Agents$clearDraft = F2(
	function (org, state) {
		return _Utils_update(
			state,
			{
				q: A2($elm$core$Dict$remove, org, state.q)
			});
	});
var $author$project$App$Discovery$clearDraft = F2(
	function (org, state) {
		return _Utils_update(
			state,
			{
				q: A2($elm$core$Dict$remove, org, state.q)
			});
	});
var $author$project$App$Drafts$closeDelete = function (state) {
	return _Utils_update(
		state,
		{bo: $elm$core$Maybe$Nothing});
};
var $author$project$App$Drafts$confirmDelete = F2(
	function (name, state) {
		return _Utils_update(
			state,
			{
				bo: A2(
					$elm$core$Maybe$map,
					function (snapshot) {
						return _Utils_update(
							snapshot,
							{bi: name});
					},
					state.bo)
			});
	});
var $author$project$Domain$Agent$mapRole = F2(
	function (ident, f) {
		return $elm$core$List$map(
			function (role) {
				return _Utils_eq(role.du, ident) ? f(role) : role;
			});
	});
var $author$project$Domain$Agent$parseApproval = function (key) {
	return A2($elm$core$String$startsWith, 'person:', key) ? $elm$core$Maybe$Just(
		$author$project$Domain$Agent$Person(
			A2($elm$core$String$dropLeft, 7, key))) : (A2($elm$core$String$startsWith, 'permission:', key) ? $elm$core$Maybe$Just(
		$author$project$Domain$Agent$Permission(
			A2($elm$core$String$dropLeft, 11, key))) : $elm$core$Maybe$Nothing);
};
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
var $elm$core$Basics$not = _Basics_not;
var $elm$core$String$trim = _String_trim;
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
			case 0:
				var drafts = change.a;
				return drafts;
			case 1:
				var ident = change.a;
				var value = change.b;
				return A3(
					$author$project$Domain$Agent$mapRole,
					ident,
					function (r) {
						return _Utils_update(
							r,
							{dO: value});
					},
					roles);
			case 2:
				var ident = change.a;
				var value = change.b;
				return A3(
					$author$project$Domain$Agent$mapRole,
					ident,
					function (r) {
						return _Utils_update(
							r,
							{dz: value});
					},
					roles);
			case 3:
				var ident = change.a;
				var value = change.b;
				return A3(
					$author$project$Domain$Agent$mapRole,
					ident,
					function (r) {
						return _Utils_update(
							r,
							{d2: value});
					},
					roles);
			case 4:
				var ident = change.a;
				var value = change.b;
				return A3(
					$author$project$Domain$Agent$mapRole,
					ident,
					function (r) {
						return _Utils_update(
							r,
							{
								eP: $author$project$Domain$Agent$splitTools(value)
							});
					},
					roles);
			case 5:
				var ident = change.a;
				var value = change.b;
				return A3(
					$author$project$Domain$Agent$mapRole,
					ident,
					function (r) {
						return _Utils_update(
							r,
							{dG: value});
					},
					roles);
			case 6:
				var ident = change.a;
				var value = change.b;
				return A3(
					$author$project$Domain$Agent$mapRole,
					ident,
					function (r) {
						return _Utils_update(
							r,
							{
								cv: $author$project$Domain$Agent$parseApproval(value)
							});
					},
					roles);
			case 7:
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
							r.dq);
						return _Utils_update(
							r,
							{
								dq: (selected && (!_Utils_eq(target, ident))) ? _Utils_ap(
									without,
									_List_fromArray(
										[target])) : without
							});
					},
					roles);
			case 8:
				var ident = change.a;
				var value = change.b;
				return A3(
					$author$project$Domain$Agent$mapRole,
					ident,
					function (r) {
						return _Utils_update(
							r,
							{ez: value});
					},
					roles);
			case 9:
				var ident = change.a;
				var value = change.b;
				return A3(
					$author$project$Domain$Agent$mapRole,
					ident,
					function (r) {
						return _Utils_update(
							r,
							{de: value});
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
								dq: A2(
									$elm$core$List$filter,
									$elm$core$Basics$neq(ident),
									r.dq)
							});
					},
					A2(
						$elm$core$List$filter,
						function (r) {
							return !_Utils_eq(r.du, ident);
						},
						roles));
		}
	});
var $author$project$App$Agents$saved = F2(
	function (org, state) {
		return A2($elm$core$Dict$get, org, state.U);
	});
var $author$project$App$Agents$current = F2(
	function (org, state) {
		var _v0 = A2($elm$core$Dict$get, org, state.q);
		if (!_v0.$) {
			var draft = _v0.a;
			return $elm$core$Maybe$Just(draft);
		} else {
			return A2(
				$elm$core$Maybe$map,
				function (snapshot) {
					return {N: snapshot.N, aC: snapshot.aC};
				},
				A2($author$project$App$Agents$saved, org, state));
		}
	});
var $author$project$App$Agents$edit = F3(
	function (org, change, state) {
		var _v0 = A2($author$project$App$Agents$current, org, state);
		if (_v0.$ === 1) {
			return state;
		} else {
			var design = _v0.a;
			return _Utils_update(
				state,
				{
					q: A3(
						$elm$core$Dict$insert,
						org,
						_Utils_update(
							design,
							{
								N: A2($author$project$Domain$Agent$apply, change, design.N)
							}),
						state.q)
				});
		}
	});
var $author$project$Domain$Discovery$editObservation = F3(
	function (key, value, o) {
		switch (key) {
			case 'subject':
				return _Utils_update(
					o,
					{eD: value});
			case 'detail':
				return _Utils_update(
					o,
					{o: value});
			case 'status':
				return _Utils_update(
					o,
					{ez: value});
			case 'evidence':
				return _Utils_update(
					o,
					{de: value});
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
					{dO: value});
			case 'role':
				return _Utils_update(
					w,
					{eo: value});
			case 'trigger':
				return _Utils_update(
					w,
					{eR: value});
			case 'inputs':
				return _Utils_update(
					w,
					{dz: value});
			case 'tools':
				return _Utils_update(
					w,
					{eP: value});
			case 'outputs':
				return _Utils_update(
					w,
					{d2: value});
			case 'handoff':
				return _Utils_update(
					w,
					{dp: value});
			case 'approval':
				return _Utils_update(
					w,
					{cv: value});
			case 'status':
				return _Utils_update(
					w,
					{ez: value});
			case 'evidence':
				return _Utils_update(
					w,
					{de: value});
			default:
				return w;
		}
	});
var $author$project$Domain$Discovery$emptyWorkflow = function (ident) {
	return {cv: '', cw: $elm$core$Maybe$Nothing, cx: $elm$core$Maybe$Nothing, de: '', dp: '', bA: _List_Nil, du: ident, dz: '', dO: '', d2: '', eo: '', ep: $elm$core$Maybe$Nothing, ez: 'unknown', eP: '', eR: ''};
};
var $author$project$Domain$Discovery$mapWorkflow = F3(
	function (ident, f, doc) {
		return _Utils_update(
			doc,
			{
				eX: A2(
					$elm$core$List$map,
					function (w) {
						return _Utils_eq(w.du, ident) ? f(w) : w;
					},
					doc.eX)
			});
	});
var $author$project$Domain$Discovery$optional = function (value) {
	return ($elm$core$String$trim(value) === '') ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(value);
};
var $author$project$Domain$Discovery$apply = F2(
	function (change, doc) {
		var updated = function () {
			switch (change.$) {
				case 0:
					var value = change.a;
					return _Utils_update(
						doc,
						{eq: value});
				case 1:
					var value = change.a;
					return _Utils_update(
						doc,
						{cz: value});
				case 2:
					var ident = change.a;
					return _Utils_update(
						doc,
						{
							dZ: _Utils_ap(
								doc.dZ,
								_List_fromArray(
									[
										{o: '', de: '', du: ident, ez: 'unknown', eD: ''}
									]))
						});
				case 3:
					var ident = change.a;
					var key = change.b;
					var value = change.c;
					return _Utils_update(
						doc,
						{
							dZ: A2(
								$elm$core$List$map,
								function (o) {
									return _Utils_eq(o.du, ident) ? A3($author$project$Domain$Discovery$editObservation, key, value, o) : o;
								},
								doc.dZ)
						});
				case 4:
					var ident = change.a;
					return _Utils_update(
						doc,
						{
							eX: _Utils_ap(
								doc.eX,
								_List_fromArray(
									[
										$author$project$Domain$Discovery$emptyWorkflow(ident)
									]))
						});
				case 5:
					var ident = change.a;
					var key = change.b;
					var value = change.c;
					return A3(
						$author$project$Domain$Discovery$mapWorkflow,
						ident,
						A2($author$project$Domain$Discovery$editWorkflow, key, value),
						doc);
				case 6:
					var ident = change.a;
					var value = change.b;
					return A3(
						$author$project$Domain$Discovery$mapWorkflow,
						ident,
						function (w) {
							return _Utils_update(
								w,
								{
									ep: $author$project$Domain$Discovery$optional(value)
								});
						},
						doc);
				case 7:
					var ident = change.a;
					var value = change.b;
					return A3(
						$author$project$Domain$Discovery$mapWorkflow,
						ident,
						function (w) {
							return _Utils_update(
								w,
								{
									cx: $author$project$Domain$Discovery$optional(value)
								});
						},
						doc);
				case 8:
					var ident = change.a;
					var value = change.b;
					return A3(
						$author$project$Domain$Discovery$mapWorkflow,
						ident,
						function (w) {
							return _Utils_update(
								w,
								{
									cw: $author$project$Domain$Discovery$optional(value)
								});
						},
						doc);
				case 9:
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
								w.bA);
							return _Utils_update(
								w,
								{
									bA: (selected && (!_Utils_eq(target, ident))) ? _Utils_ap(
										without,
										_List_fromArray(
											[target])) : without
								});
						},
						doc);
				case 10:
					var ident = change.a;
					return _Utils_update(
						doc,
						{
							dZ: A2(
								$elm$core$List$filter,
								function (o) {
									return !_Utils_eq(o.du, ident);
								},
								doc.dZ)
						});
				case 11:
					var ident = change.a;
					return _Utils_update(
						doc,
						{
							eX: A2(
								$elm$core$List$map,
								function (w) {
									return _Utils_update(
										w,
										{
											bA: A2(
												$elm$core$List$filter,
												$elm$core$Basics$neq(ident),
												w.bA)
										});
								},
								A2(
									$elm$core$List$filter,
									function (w) {
										return !_Utils_eq(w.du, ident);
									},
									doc.eX))
						});
				case 12:
					var value = change.a;
					return _Utils_update(
						doc,
						{
							b1: {dY: value, ez: doc.b1.ez}
						});
				default:
					var value = change.a;
					return _Utils_update(
						doc,
						{
							b1: {dY: doc.b1.dY, ez: value}
						});
			}
		}();
		switch (change.$) {
			case 12:
				return updated;
			case 13:
				return updated;
			default:
				return _Utils_update(
					updated,
					{
						b1: {dY: updated.b1.dY, ez: 'pending'}
					});
		}
	});
var $author$project$App$Discovery$saved = F2(
	function (org, state) {
		return A2($elm$core$Dict$get, org, state.P);
	});
var $author$project$App$Discovery$current = F2(
	function (org, state) {
		var _v0 = A2($elm$core$Dict$get, org, state.q);
		if (!_v0.$) {
			var draft = _v0.a;
			return $elm$core$Maybe$Just(draft);
		} else {
			return A2($author$project$App$Discovery$saved, org, state);
		}
	});
var $author$project$App$Discovery$edit = F3(
	function (org, change, state) {
		var _v0 = A2($author$project$App$Discovery$current, org, state);
		if (_v0.$ === 1) {
			return state;
		} else {
			var snapshot = _v0.a;
			return _Utils_update(
				state,
				{
					q: A3(
						$elm$core$Dict$insert,
						org,
						_Utils_update(
							snapshot,
							{
								Z: A2($author$project$Domain$Discovery$apply, change, snapshot.Z)
							}),
						state.q)
				});
		}
	});
var $author$project$App$Drafts$defaultContext = function (model) {
	return {
		cW: model.aH.cW,
		dn: A2(
			$elm$core$Maybe$withDefault,
			0,
			A2(
				$elm$core$Dict$get,
				A2($elm$core$Maybe$withDefault, '', model.ad.aL),
				model.w.ab)),
		b6: model.aH.b6,
		eN: model.aH.eN,
		bb: function () {
			var _v0 = model.ad.bb;
			if (_v0.$ === 1) {
				var data = _v0.a;
				return $elm$core$Maybe$Just(data);
			} else {
				return $elm$core$Maybe$Nothing;
			}
		}()
	};
};
var $author$project$Form$Defaults$defaultValue = F3(
	function (model, action, name) {
		var w = model.bb;
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
								return $.a3;
							},
							$elm$core$List$head(
								A2(
									$elm$core$List$filter,
									A2(
										$elm$core$Basics$composeR,
										function ($) {
											return $.a_;
										},
										A2(
											$elm$core$Basics$composeR,
											function ($) {
												return $.du;
											},
											$elm$core$Basics$eq(key))),
									data.$7)));
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
								return $.bQ;
							},
							function ($) {
								return $.dO;
							}),
						w)) : '';
			case 4:
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
										return $.du;
									},
									$elm$core$Basics$eq(key)),
								data.d4));
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
									return p.dO;
								case 'role':
									return p.eo;
								case 'department':
									return A2($elm$core$Maybe$withDefault, '', p.c0);
								case 'email':
									return A2($elm$core$Maybe$withDefault, '', p.da);
								case 'reportsTo':
									return A2($elm$core$Maybe$withDefault, '', p.ef);
								default:
									return '';
							}
						},
						person));
			case 6:
				switch (name) {
					case 'baseline':
						return '0';
					case 'target':
						return '100';
					case 'budget':
						return '0';
					case 'metricId':
						return 'metric-' + (model.b6 + ('-' + $elm$core$String$fromInt(model.dn)));
					case 'direction':
						return 'HigherIsBetter';
					case 'startsAt':
						return model.eN;
					case 'deadline':
						return model.cW;
					default:
						return '';
				}
			case 7:
				var key = action.a;
				return (name === 'owner') ? owner(key) : '';
			case 9:
				var key = action.a;
				return (name === 'reportedBy') ? owner(key) : '';
			case 8:
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
										return $.a3;
									},
									$elm$core$Basics$eq(key)),
								data.cB));
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
								return $.cI;
							},
							$elm$core$String$fromFloat),
						authority)) : (A2(
					$elm$core$Maybe$withDefault,
					false,
					A2(
						$elm$core$Maybe$map,
						function (a) {
							return A2($elm$core$List$member, name, a.cK) || (((name === 'Hiring') && a.cM) || ((name === 'Pricing') && a.cL));
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
		case 0:
			return 'organization';
		case 1:
			return 'demo';
		case 2:
			return 'rename';
		case 3:
			return 'person';
		case 4:
			var key = action.a;
			return 'person-edit-' + key;
		case 5:
			var key = action.a;
			return 'person-deactivate-' + key;
		case 6:
			return 'goal';
		case 7:
			var key = action.a;
			return 'owner-' + key;
		case 8:
			var key = action.a;
			return 'authority-' + key;
		case 9:
			var key = action.a;
			return 'result-' + key;
		case 10:
			var key = action.a;
			return 'strategy-' + key;
		case 11:
			return 'review';
		case 12:
			var key = action.a;
			return 'activate-' + key;
		case 13:
			var key = action.a;
			return 'evaluate-' + key;
		default:
			return 'delete';
	}
};
var $author$project$App$Drafts$formKey = F2(
	function (model, action) {
		return A2($elm$core$Maybe$withDefault, 'list', model.ad.aL) + ('/' + $author$project$Form$Action$actionKey(action));
	});
var $author$project$App$Drafts$workspaceVersion = function (model) {
	var _v0 = model.ad.bb;
	if (_v0.$ === 1) {
		var w = _v0.a;
		return w.aC;
	} else {
		return 0;
	}
};
var $author$project$App$Drafts$edit = F4(
	function (action, key, value, model) {
		var state = model.w;
		var draftKey = A2($author$project$App$Drafts$formKey, model, action);
		var current = A2(
			$elm$core$Maybe$withDefault,
			A2($author$project$App$Drafts$draftDefaults, model, action),
			A2($elm$core$Dict$get, draftKey, state.q));
		var version = function () {
			switch (action.$) {
				case 4:
					return A2(
						$elm$core$Maybe$withDefault,
						$elm$core$String$fromInt(
							$author$project$App$Drafts$workspaceVersion(model)),
						A2($elm$core$Dict$get, '__version', current));
				case 5:
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
				q: A3(
					$elm$core$Dict$insert,
					draftKey,
					A3(
						$elm$core$Dict$insert,
						'__version',
						version,
						A3($elm$core$Dict$insert, key, value, current)),
					state.q)
			});
	});
var $author$project$Form$Action$AddGoal = {$: 6};
var $author$project$Form$Goal$edit = F3(
	function (key, content, draft) {
		switch (key.$) {
			case 0:
				return _Utils_update(
					draft,
					{c1: content});
			case 1:
				return _Utils_update(
					draft,
					{dL: content});
			case 2:
				return _Utils_update(
					draft,
					{eT: content});
			case 3:
				return _Utils_update(
					draft,
					{dK: content});
			case 4:
				return _Utils_update(
					draft,
					{c4: content});
			case 5:
				return _Utils_update(
					draft,
					{cE: content});
			case 6:
				return _Utils_update(
					draft,
					{eI: content});
			case 7:
				return _Utils_update(
					draft,
					{ey: content});
			case 8:
				return _Utils_update(
					draft,
					{cW: content});
			case 9:
				return _Utils_update(
					draft,
					{cH: content});
			case 10:
				return _Utils_update(
					draft,
					{d3: content});
			default:
				var permission = key.a;
				return _Utils_update(
					draft,
					{
						d6: (content === 'true') ? A2(
							$elm$core$List$cons,
							permission,
							A2(
								$elm$core$List$filter,
								$elm$core$Basics$neq(permission),
								draft.d6)) : A2(
							$elm$core$List$filter,
							$elm$core$Basics$neq(permission),
							draft.d6)
					});
		}
	});
var $author$project$App$Drafts$defaultValue = function (model) {
	return $author$project$Form$Defaults$defaultValue(
		$author$project$App$Drafts$defaultContext(model));
};
var $author$project$Form$Goal$fromValues = function (get) {
	return {
		cE: get('baseline'),
		cH: get('budget'),
		cW: get('deadline'),
		c1: get('description'),
		c4: get('direction'),
		dK: get('metricId'),
		dL: get('metricName'),
		d3: get('parent'),
		d6: A2(
			$elm$core$List$filter,
			function (key) {
				return get(key) === 'true';
			},
			$author$project$Domain$Permission$permissionKeys),
		ey: get('startsAt'),
		eI: get('target'),
		eT: get('unit')
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
			model.w.z));
};
var $author$project$App$Drafts$editGoal = F3(
	function (field, value, model) {
		var state = model.w;
		return _Utils_update(
			state,
			{
				z: A3(
					$elm$core$Dict$insert,
					A2($author$project$App$Drafts$formKey, model, $author$project$Form$Action$AddGoal),
					A3(
						$author$project$Form$Goal$edit,
						field,
						value,
						$author$project$App$Drafts$goalDraft(model)),
					state.z)
			});
	});
var $author$project$Form$Action$AddReview = {$: 11};
var $author$project$Form$Review$edit = F3(
	function (key, content, draft) {
		switch (key) {
			case 0:
				return _Utils_update(
					draft,
					{a_: content});
			case 1:
				return _Utils_update(
					draft,
					{dY: content});
			case 2:
				return _Utils_update(
					draft,
					{bI: content});
			case 3:
				return _Utils_update(
					draft,
					{bl: content});
			case 4:
				return _Utils_update(
					draft,
					{cX: content});
			default:
				return _Utils_update(
					draft,
					{bm: content});
		}
	});
var $author$project$Form$Review$fromValues = function (get) {
	return {
		bl: get('decision'),
		bm: get('decisionDeadline'),
		cX: get('decisionOwner'),
		a_: get('goal'),
		bI: get('learning'),
		dY: get('note')
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
			model.w.D));
};
var $author$project$App$Drafts$editReview = F3(
	function (field, value, model) {
		var state = model.w;
		return _Utils_update(
			state,
			{
				D: A3(
					$elm$core$Dict$insert,
					A2($author$project$App$Drafts$formKey, model, $author$project$Form$Action$AddReview),
					A3(
						$author$project$Form$Review$edit,
						field,
						value,
						$author$project$App$Drafts$reviewDraft(model)),
					state.D)
			});
	});
var $author$project$App$PageState$filterPeople = F2(
	function (status, state) {
		return _Utils_update(
			state,
			{aO: status});
	});
var $author$project$App$Session$finishSave = function (state) {
	return _Utils_update(
		state,
		{au: $author$project$App$Session$Idle});
};
var $author$project$Form$Goal$Baseline = {$: 5};
var $author$project$Form$Goal$Budget = {$: 9};
var $author$project$Form$Goal$Deadline = {$: 8};
var $author$project$Form$Goal$Description = {$: 0};
var $author$project$Form$Goal$Parent = {$: 10};
var $author$project$Form$Goal$Permission = function (a) {
	return {$: 11, a: a};
};
var $author$project$Form$Goal$StartsAt = {$: 7};
var $author$project$Form$Goal$Target = {$: 6};
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
var $author$project$Form$Review$Goal = 0;
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
var $author$project$App$PageState$guide = F3(
	function (page, target, state) {
		return _Utils_update(
			state,
			{
				X: (page === 11) ? $author$project$Ui$Activity$init : state.X,
				aF: A2($elm$core$String$startsWith, 'goal-', target) ? $elm$core$Maybe$Just(
					A2($elm$core$String$dropLeft, 5, target)) : state.aF,
				ac: page
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
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (!_v0.$) {
			return true;
		} else {
			return false;
		}
	});
var $author$project$App$PageState$navigate = F2(
	function (page, state) {
		return _Utils_update(
			state,
			{X: $author$project$Ui$Activity$init, ap: $author$project$Ui$ResponsibilityGraph$init, ac: page, aN: '', aO: 'active', aU: $elm$core$Maybe$Nothing});
	});
var $author$project$App$Drafts$openDelete = F2(
	function (snapshot, state) {
		return _Utils_update(
			state,
			{
				bo: $elm$core$Maybe$Just(snapshot)
			});
	});
var $author$project$App$PageState$openPerson = F2(
	function (key, state) {
		return _Utils_update(
			state,
			{
				aU: $elm$core$Maybe$Just(key)
			});
	});
var $author$project$Form$Goal$value = F2(
	function (draft, key) {
		switch (key.$) {
			case 0:
				return draft.c1;
			case 1:
				return draft.dL;
			case 2:
				return draft.eT;
			case 3:
				return draft.dK;
			case 4:
				return draft.c4;
			case 5:
				return draft.cE;
			case 6:
				return draft.eI;
			case 7:
				return draft.ey;
			case 8:
				return draft.cW;
			case 9:
				return draft.cH;
			case 10:
				return draft.d3;
			default:
				var permission = key.a;
				return A2($elm$core$List$member, permission, draft.d6) ? 'true' : 'false';
		}
	});
var $author$project$Form$Review$value = F2(
	function (draft, key) {
		switch (key) {
			case 0:
				return draft.a_;
			case 1:
				return draft.dY;
			case 2:
				return draft.bI;
			case 3:
				return draft.bl;
			case 4:
				return draft.cX;
			default:
				return draft.bm;
		}
	});
var $author$project$App$Drafts$get = F3(
	function (model, action, name) {
		switch (action.$) {
			case 6:
				return A2(
					$elm$core$Maybe$withDefault,
					'',
					A2(
						$elm$core$Maybe$map,
						$author$project$Form$Goal$value(
							$author$project$App$Drafts$goalDraft(model)),
						$author$project$Form$Goal$fromKey(name)));
			case 11:
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
							model.w.q)));
		}
	});
var $author$project$App$Drafts$prepareReview = function (model) {
	return (A3($author$project$App$Drafts$get, model, $author$project$Form$Action$AddReview, 'goal') === '') ? A3($author$project$App$Drafts$editReview, 0, 'demo-revenue', model) : model.w;
};
var $author$project$App$Agents$rebase = F2(
	function (org, state) {
		var _v0 = _Utils_Tuple2(
			A2($elm$core$Dict$get, org, state.q),
			A2($author$project$App$Agents$saved, org, state));
		if ((!_v0.a.$) && (!_v0.b.$)) {
			var draft = _v0.a.a;
			var latest = _v0.b.a;
			return _Utils_update(
				state,
				{
					q: A3(
						$elm$core$Dict$insert,
						org,
						_Utils_update(
							draft,
							{aC: latest.aC}),
						state.q)
				});
		} else {
			return state;
		}
	});
var $author$project$App$Discovery$rebase = F2(
	function (org, state) {
		var _v0 = _Utils_Tuple2(
			A2($elm$core$Dict$get, org, state.q),
			A2($author$project$App$Discovery$saved, org, state));
		if ((!_v0.a.$) && (!_v0.b.$)) {
			var draft = _v0.a.a;
			var latest = _v0.b.a;
			var doc = draft.Z;
			return _Utils_update(
				state,
				{
					q: A3(
						$elm$core$Dict$insert,
						org,
						_Utils_update(
							draft,
							{
								Z: _Utils_update(
									doc,
									{
										b1: {dY: doc.b1.dY, ez: 'pending'}
									}),
								aC: latest.aC
							}),
						state.q)
				});
		} else {
			return state;
		}
	});
var $author$project$App$Agents$receive = F3(
	function (org, result, state) {
		if (!result.$) {
			var snapshot = result.a;
			return _Utils_update(
				state,
				{
					I: A2($elm$core$Dict$remove, org, state.I),
					aJ: false,
					U: A3($elm$core$Dict$insert, org, snapshot, state.U)
				});
		} else {
			var error = result.a;
			return _Utils_update(
				state,
				{
					I: A3($elm$core$Dict$insert, org, error, state.I),
					aJ: false
				});
		}
	});
var $author$project$App$Discovery$receive = F3(
	function (org, result, state) {
		if (!result.$) {
			var snapshot = result.a;
			return _Utils_update(
				state,
				{
					P: A3($elm$core$Dict$insert, org, snapshot, state.P),
					I: A2($elm$core$Dict$remove, org, state.I),
					aJ: false
				});
		} else {
			var error = result.a;
			return _Utils_update(
				state,
				{
					I: A3($elm$core$Dict$insert, org, error, state.I),
					aJ: false
				});
		}
	});
var $author$project$App$Update$receive = F4(
	function (token, result, session, model) {
		if (!_Utils_eq(token, model.ad.aQ)) {
			return _Utils_Tuple2(model, _List_Nil);
		} else {
			if (!result.$) {
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{ad: session}),
					_List_Nil);
			} else {
				var message = result.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{bt: true, bN: message, ad: session}),
					_List_Nil);
			}
		}
	});
var $author$project$Remote$Failed = function (a) {
	return {$: 2, a: a};
};
var $author$project$Remote$Loaded = function (a) {
	return {$: 1, a: a};
};
var $author$project$App$Session$response = function (result) {
	if (!result.$) {
		var value = result.a;
		return $author$project$Remote$Loaded(value);
	} else {
		var message = result.a;
		return $author$project$Remote$Failed(message);
	}
};
var $author$project$App$Session$succeeded = function (result) {
	if (!result.$) {
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
				ao: $author$project$App$Session$succeeded(result),
				aM: $author$project$App$Session$response(result),
				aw: false
			});
	});
var $author$project$App$Session$receiveWorkspace = F2(
	function (result, state) {
		return _Utils_update(
			state,
			{
				ao: $author$project$App$Session$succeeded(result),
				aw: false,
				bb: $author$project$App$Session$response(result)
			});
	});
var $author$project$Form$Action$DeactivatePerson = function (a) {
	return {$: 5, a: a};
};
var $author$project$Form$Action$UpdatePerson = function (a) {
	return {$: 4, a: a};
};
var $author$project$App$Drafts$resetPerson = F2(
	function (key, model) {
		var state = model.w;
		return _Utils_update(
			state,
			{
				q: A2(
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
						state.q))
			});
	});
var $author$project$Form$Action$DeleteOrg = {$: 14};
var $author$project$App$Session$organizationDeleted = function (state) {
	return _Utils_update(
		state,
		{aL: $elm$core$Maybe$Nothing, aM: $author$project$Remote$Loading, bb: $author$project$Remote$Loading});
};
var $author$project$App$Agents$remove = F2(
	function (org, state) {
		return _Utils_update(
			state,
			{
				q: A2($elm$core$Dict$remove, org, state.q),
				I: A2($elm$core$Dict$remove, org, state.I),
				U: A2($elm$core$Dict$remove, org, state.U)
			});
	});
var $author$project$App$Discovery$remove = F2(
	function (org, state) {
		return _Utils_update(
			state,
			{
				P: A2($elm$core$Dict$remove, org, state.P),
				q: A2($elm$core$Dict$remove, org, state.q),
				I: A2($elm$core$Dict$remove, org, state.I)
			});
	});
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
				q: A2($elm$core$Dict$filter, keep, state.q),
				z: A2($elm$core$Dict$filter, keep, state.z),
				D: A2($elm$core$Dict$filter, keep, state.D)
			});
	});
var $author$project$App$Drafts$saved = F2(
	function (action, model) {
		var state = model.w;
		var key = A2($author$project$App$Drafts$formKey, model, action);
		return _Utils_update(
			state,
			{
				bo: $elm$core$Maybe$Nothing,
				q: A2($elm$core$Dict$remove, key, state.q),
				z: _Utils_eq(action, $author$project$Form$Action$AddGoal) ? A2($elm$core$Dict$remove, key, state.z) : state.z,
				ab: _Utils_eq(action, $author$project$Form$Action$AddGoal) ? A3(
					$elm$core$Dict$update,
					A2($elm$core$Maybe$withDefault, '', model.ad.aL),
					A2(
						$elm$core$Basics$composeR,
						$elm$core$Maybe$withDefault(0),
						A2(
							$elm$core$Basics$composeR,
							$elm$core$Basics$add(1),
							$elm$core$Maybe$Just)),
					state.ab) : state.ab,
				D: _Utils_eq(action, $author$project$Form$Action$AddReview) ? A2($elm$core$Dict$remove, key, state.D) : state.D
			});
	});
var $author$project$App$PageState$setPage = F2(
	function (page, state) {
		return _Utils_update(
			state,
			{ac: page});
	});
var $author$project$App$Update$saved = F3(
	function (action, response, model) {
		if (response.$ === 1) {
			var message = response.a;
			return $author$project$App$Update$refresh(
				_Utils_update(
					model,
					{
						bt: true,
						w: $author$project$App$Drafts$closeDelete(model.w),
						bN: message + ' 자동 재시도하지 않았습니다. 최신 상태를 확인한 뒤 다시 저장하세요. 입력 내용은 보존됩니다.',
						ad: $author$project$App$Session$finishSave(model.ad)
					}));
		} else {
			var next = _Utils_update(
				model,
				{
					bt: false,
					w: A2($author$project$App$Drafts$saved, action, model),
					bN: '저장했습니다. 최신 조직 상태와 감사 기록을 확인하세요.',
					ad: $author$project$App$Session$finishSave(model.ad)
				});
			return _Utils_eq(action, $author$project$Form$Action$DeleteOrg) ? $author$project$App$Update$refresh(
				_Utils_update(
					next,
					{
						N: A2(
							$elm$core$Maybe$withDefault,
							next.N,
							A2(
								$elm$core$Maybe$map,
								function (org) {
									return A2($author$project$App$Agents$remove, org, next.N);
								},
								model.ad.aL)),
						Z: A2(
							$elm$core$Maybe$withDefault,
							next.Z,
							A2(
								$elm$core$Maybe$map,
								function (org) {
									return A2($author$project$App$Discovery$remove, org, next.Z);
								},
								model.ad.aL)),
						w: A2($author$project$App$Drafts$removeOrganization, model.ad.aL, next.w),
						bN: '조직을 논리 삭제했습니다. 원본 감사 기록과 다른 조직은 보존됩니다.',
						bR: A2($author$project$App$PageState$setPage, 0, next.bR),
						ad: $author$project$App$Session$organizationDeleted(next.ad)
					})) : $author$project$App$Update$refresh(next);
		}
	});
var $author$project$App$PageState$searchPeople = F2(
	function (query, state) {
		return _Utils_update(
			state,
			{aN: query});
	});
var $author$project$App$Session$selectOrganization = F2(
	function (org, state) {
		return _Utils_update(
			state,
			{aL: org, bb: $author$project$Remote$Loading});
	});
var $author$project$App$PageState$setActivity = F2(
	function (activity, state) {
		return _Utils_update(
			state,
			{X: activity});
	});
var $author$project$Page$pageName = function (page) {
	switch (page) {
		case 0:
			return '조직 목록';
		case 1:
			return '조직 진단';
		case 2:
			return '업무 흐름';
		case 3:
			return '에이전트 초안';
		case 4:
			return '에이전트 구조';
		case 5:
			return '구성원';
		case 6:
			return '목표';
		case 7:
			return '책임';
		case 8:
			return '권한';
		case 9:
			return '결과';
		case 10:
			return '학습';
		case 11:
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
				as: A3(
					$elm$core$Dict$insert,
					$author$project$Page$pageName(page),
					mode,
					state.as)
			});
	});
var $author$project$App$Effect$SaveCommand = F5(
	function (a, b, c, d, e) {
		return {$: 2, a: a, b: b, c: c, d: d, e: e};
	});
var $author$project$App$Session$Saving = function (a) {
	return {$: 1, a: a};
};
var $author$project$App$Session$beginSave = F2(
	function (key, state) {
		return _Utils_update(
			state,
			{
				au: $author$project$App$Session$Saving(key)
			});
	});
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$json$Json$Encode$float = _Json_wrap;
var $elm$core$String$toFloat = _String_toFloat;
var $author$project$Form$Goal$validate = function (draft) {
	if (A2(
		$elm$core$List$any,
		A2(
			$elm$core$Basics$composeR,
			$elm$core$String$trim,
			$elm$core$Basics$eq('')),
		_List_fromArray(
			[draft.c1, draft.dK, draft.dL, draft.eT, draft.ey, draft.cW]))) {
		return $elm$core$Result$Err('필수 항목을 모두 입력하세요.');
	} else {
		var _v0 = _Utils_Tuple3(
			$elm$core$String$toFloat(draft.cE),
			$elm$core$String$toFloat(draft.eI),
			$elm$core$String$toFloat(draft.cH));
		if (((!_v0.a.$) && (!_v0.b.$)) && (!_v0.c.$)) {
			var baseline = _v0.a.a;
			var target = _v0.b.a;
			var budget = _v0.c.a;
			return (_Utils_cmp(draft.cW, draft.ey) < 0) ? $elm$core$Result$Err('마감일은 시작일 이후여야 합니다.') : $elm$core$Result$Ok(
				{cE: baseline, cH: budget, cW: draft.cW, c1: draft.c1, c4: draft.c4, dK: draft.dK, dL: draft.dL, d3: draft.d3, d6: draft.d6, ey: draft.ey, eI: target, eT: draft.eT});
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
			[draft.a_, draft.dY])) ? $elm$core$Result$Err('필수 항목을 모두 입력하세요.') : ((($elm$core$String$trim(draft.bl) !== '') && (draft.cX === '')) ? $elm$core$Result$Err('다음 결정의 담당자를 선택하세요.') : $elm$core$Result$Ok(draft));
};
var $author$project$Api$Command$payload = F2(
	function (model, action) {
		var version = A2(
			$elm$core$Maybe$withDefault,
			model.aC,
			$elm$core$String$toInt(
				A2(model.ba, action, '__version')));
		var val = model.ba(action);
		var uid = function (prefix) {
			return $elm$json$Json$Encode$string(
				prefix + ('-' + (model.b6 + ('-' + $elm$core$String$fromInt(model.b7)))));
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
				A2($elm$core$Maybe$withDefault, '', model.aL),
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
			return (!_Utils_eq(version, model.aC)) ? $elm$core$Result$Err('작성 중 조직이 변경되었습니다. ‘최신 정보로 다시 불러오기’를 눌러 변경 내용을 확인한 뒤 다시 작성해 주세요.') : result;
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
					(!_Utils_eq(version, model.aC)) ? $elm$core$Result$Err('작성 중 조직이 변경되었습니다. 최신 조직 이름을 확인하고 수정 입력을 다시 해 주세요.') : $elm$core$Result$Ok(
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
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								'id',
								uid('person')),
							profile)));
			case 4:
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
			case 5:
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
			case 6:
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
											A2($elm$core$Maybe$withDefault, '', model.aL))),
										_Utils_Tuple2(
										'description',
										$elm$json$Json$Encode$string(goal.c1)),
										_Utils_Tuple2(
										'metric',
										$elm$json$Json$Encode$object(
											_List_fromArray(
												[
													_Utils_Tuple2(
													'id',
													$elm$json$Json$Encode$string(goal.dK)),
													_Utils_Tuple2(
													'name',
													$elm$json$Json$Encode$string(goal.dL)),
													_Utils_Tuple2(
													'unit',
													$elm$json$Json$Encode$string(goal.eT)),
													_Utils_Tuple2(
													'direction',
													$elm$json$Json$Encode$string(goal.c4))
												]))),
										_Utils_Tuple2(
										'baseline',
										$elm$json$Json$Encode$float(goal.cE)),
										_Utils_Tuple2(
										'target',
										$elm$json$Json$Encode$float(goal.eI)),
										_Utils_Tuple2(
										'startsAt',
										$elm$json$Json$Encode$string(goal.ey + 'T00:00:00Z')),
										_Utils_Tuple2(
										'deadline',
										$elm$json$Json$Encode$string(goal.cW + 'T00:00:00Z')),
										_Utils_Tuple2(
										'parent',
										nullable(goal.d3)),
										_Utils_Tuple2(
										'requiredPermissions',
										A2(
											$elm$json$Json$Encode$list,
											$elm$json$Json$Encode$string,
											A2(
												$elm$core$List$filter,
												function (key) {
													return A2($elm$core$List$member, key, goal.d6);
												},
												$author$project$Domain$Permission$permissionKeys))),
										_Utils_Tuple2(
										'requiredBudget',
										$elm$json$Json$Encode$float(goal.cH))
									])));
					},
					$author$project$Form$Goal$validate(model.a_));
			case 7:
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
			case 8:
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
			case 9:
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
			case 10:
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
			case 12:
				var key = action.a;
				return A2(
					post,
					path(
						'goals/' + ($elm$url$Url$percentEncode(key) + '/activate')),
					_List_Nil);
			case 13:
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
			case 11:
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
										$elm$json$Json$Encode$string(review.a_)),
										_Utils_Tuple2(
										'note',
										$elm$json$Json$Encode$string(review.dY)),
										_Utils_Tuple2(
										'learnings',
										A2(
											$elm$json$Json$Encode$list,
											$elm$core$Basics$identity,
											($elm$core$String$trim(review.bI) === '') ? _List_Nil : _List_fromArray(
												[
													$elm$json$Json$Encode$object(
													_List_fromArray(
														[
															_Utils_Tuple2(
															'text',
															$elm$json$Json$Encode$string(review.bI))
														]))
												]))),
										_Utils_Tuple2(
										'decisions',
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
															$elm$json$Json$Encode$string(review.bl)),
															_Utils_Tuple2(
															'owner',
															$elm$json$Json$Encode$string(review.cX)),
															_Utils_Tuple2(
															'deadline',
															(review.bm === '') ? $elm$json$Json$Encode$null : $elm$json$Json$Encode$string(review.bm + 'T23:59:59Z'))
														]))
												])))
									])));
					},
					$author$project$Form$Review$validate(model.b1));
			default:
				var _v1 = model.bo;
				if (!_v1.$) {
					var snapshot = _v1.a;
					return (_Utils_eq(snapshot.bi, snapshot.dO) && _Utils_eq(
						model.aL,
						$elm$core$Maybe$Just(snapshot.du))) ? $elm$core$Result$Ok(
						_Utils_Tuple3(
							'DELETE',
							A2($author$project$Api$Path$orgPath, snapshot.du, ''),
							$elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'confirmName',
										$elm$json$Json$Encode$string(snapshot.bi)),
										_Utils_Tuple2(
										'expectedVersion',
										$elm$json$Json$Encode$int(snapshot.aC))
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
				bo: model.w.bo,
				a_: $author$project$App$Drafts$goalDraft(model),
				aL: model.ad.aL,
				b1: $author$project$App$Drafts$reviewDraft(model),
				b6: model.aH.b6,
				b7: model.w.b7,
				ba: $author$project$App$Drafts$get(model),
				aC: $author$project$App$Drafts$workspaceVersion(model)
			},
			action);
	});
var $author$project$App$Update$payload = $author$project$App$Drafts$payload;
var $author$project$App$Update$submit = F2(
	function (action, model) {
		if ($author$project$App$Update$busy(model)) {
			return _Utils_Tuple2(model, _List_Nil);
		} else {
			if (!model.ad.ao) {
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{bt: true, bN: '최신 상태를 먼저 불러와 주세요. 입력 내용은 보존됩니다.'}),
					_List_Nil);
			} else {
				var _v0 = A2($author$project$App$Update$payload, model, action);
				if (_v0.$ === 1) {
					var message = _v0.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{bt: true, bN: message}),
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
								bt: false,
								w: $author$project$App$Drafts$advanceSerial(model.w),
								bN: '저장 중입니다…',
								ad: A2(
									$author$project$App$Session$beginSave,
									$author$project$Form$Action$actionKey(action),
									model.ad)
							}),
						_List_fromArray(
							[
								A5($author$project$App$Effect$SaveCommand, model.ad.aQ, action, method, path, body)
							]));
				}
			}
		}
	});
var $author$project$App$Effect$SaveAgents = F4(
	function (a, b, c, d) {
		return {$: 6, a: a, b: b, c: c, d: d};
	});
var $author$project$App$Agents$conflicted = F2(
	function (org, state) {
		var _v0 = _Utils_Tuple2(
			A2($elm$core$Dict$get, org, state.q),
			A2($author$project$App$Agents$saved, org, state));
		if ((!_v0.a.$) && (!_v0.b.$)) {
			var draft = _v0.a.a;
			var latest = _v0.b.a;
			return !_Utils_eq(draft.aC, latest.aC);
		} else {
			return false;
		}
	});
var $author$project$Domain$Agent$problems = function (roles) {
	return A2(
		$elm$core$List$concatMap,
		function (role) {
			return _Utils_ap(
				($elm$core$String$trim(role.dO) === '') ? _List_fromArray(
					[role.du + ': 역할 이름을 입력하세요.']) : _List_Nil,
				((role.ez === 'confirmed') && ($elm$core$String$trim(role.de) === '')) ? _List_fromArray(
					[role.dO + ': 확인된 사실에는 근거가 필요합니다.']) : _List_Nil);
		},
		roles);
};
var $author$project$App$Update$submitAgents = function (model) {
	var _v0 = model.ad.aL;
	if (_v0.$ === 1) {
		return _Utils_Tuple2(model, _List_Nil);
	} else {
		var org = _v0.a;
		var _v1 = A2($author$project$App$Agents$current, org, model.N);
		if (_v1.$ === 1) {
			return _Utils_Tuple2(model, _List_Nil);
		} else {
			var design = _v1.a;
			return ($author$project$App$Update$busy(model) || (model.N.aJ || ((!model.ad.ao) || A2($elm$core$Dict$member, org, model.N.I)))) ? _Utils_Tuple2(
				_Utils_update(
					model,
					{bt: true, bN: '최신 설계를 불러온 뒤 저장하세요. 입력은 보존됩니다.'}),
				_List_Nil) : (A2($author$project$App$Agents$conflicted, org, model.N) ? _Utils_Tuple2(
				_Utils_update(
					model,
					{bt: true, bN: '입력 중 저장된 조직이 변경되었습니다. 최신 저장 내용과 비교한 뒤 다시 적용하세요.'}),
				_List_Nil) : ((!$elm$core$List$isEmpty(
				$author$project$Domain$Agent$problems(design.N))) ? _Utils_Tuple2(
				_Utils_update(
					model,
					{
						bt: true,
						bN: A2(
							$elm$core$String$join,
							' ',
							$author$project$Domain$Agent$problems(design.N))
					}),
				_List_Nil) : _Utils_Tuple2(
				_Utils_update(
					model,
					{
						bt: false,
						bN: '설계안 저장 중…',
						ad: A2($author$project$App$Session$beginSave, 'agents', model.ad)
					}),
				_List_fromArray(
					[
						A4($author$project$App$Effect$SaveAgents, model.ad.aQ, org, design.aC, design.N)
					]))));
		}
	}
};
var $author$project$App$Effect$SaveDiscovery = F3(
	function (a, b, c) {
		return {$: 4, a: a, b: b, c: c};
	});
var $author$project$App$Discovery$conflicted = F2(
	function (org, state) {
		var _v0 = _Utils_Tuple2(
			A2($author$project$App$Discovery$current, org, state),
			A2($author$project$App$Discovery$saved, org, state));
		if ((!_v0.a.$) && (!_v0.b.$)) {
			var draft = _v0.a.a;
			var latest = _v0.b.a;
			return !_Utils_eq(draft.aC, latest.aC);
		} else {
			return false;
		}
	});
var $author$project$Domain$Discovery$problems = function (doc) {
	var blank = A2($elm$core$Basics$composeR, $elm$core$String$trim, $elm$core$String$isEmpty);
	var observation = function (o) {
		return _Utils_ap(
			blank(o.eD) ? _List_fromArray(
				['현황 항목의 제목을 입력하세요.']) : _List_Nil,
			((o.ez === 'confirmed') && blank(o.de)) ? _List_fromArray(
				[o.eD + ': 확인된 사실에는 근거가 필요합니다.']) : _List_Nil);
	};
	var workflow = function (w) {
		return _Utils_ap(
			blank(w.dO) ? _List_fromArray(
				['업무 이름을 입력하세요.']) : _List_Nil,
			((w.ez === 'confirmed') && blank(w.de)) ? _List_fromArray(
				[w.dO + ': 확인된 사실에는 근거가 필요합니다.']) : _List_Nil);
	};
	return _Utils_ap(
		A2($elm$core$List$concatMap, observation, doc.dZ),
		A2($elm$core$List$concatMap, workflow, doc.eX));
};
var $author$project$App$Update$submitDiscovery = function (model) {
	var _v0 = model.ad.aL;
	if (_v0.$ === 1) {
		return _Utils_Tuple2(model, _List_Nil);
	} else {
		var org = _v0.a;
		var _v1 = A2($author$project$App$Discovery$current, org, model.Z);
		if (_v1.$ === 1) {
			return _Utils_Tuple2(model, _List_Nil);
		} else {
			var snapshot = _v1.a;
			return ($author$project$App$Update$busy(model) || (model.Z.aJ || ((!model.ad.ao) || A2($elm$core$Dict$member, org, model.Z.I)))) ? _Utils_Tuple2(
				_Utils_update(
					model,
					{bt: true, bN: '최신 현황을 불러온 뒤 저장하세요. 입력은 보존됩니다.'}),
				_List_Nil) : (A2($author$project$App$Discovery$conflicted, org, model.Z) ? _Utils_Tuple2(
				_Utils_update(
					model,
					{bt: true, bN: '입력 중 저장된 조직이 변경되었습니다. 최신 저장 내용과 비교한 뒤 다시 적용하세요.'}),
				_List_Nil) : ((!$elm$core$List$isEmpty(
				$author$project$Domain$Discovery$problems(snapshot.Z))) ? _Utils_Tuple2(
				_Utils_update(
					model,
					{
						bt: true,
						bN: A2(
							$elm$core$String$join,
							' ',
							$author$project$Domain$Discovery$problems(snapshot.Z))
					}),
				_List_Nil) : _Utils_Tuple2(
				_Utils_update(
					model,
					{
						bt: false,
						bN: '현황 저장 중…',
						ad: A2($author$project$App$Session$beginSave, 'discovery', model.ad)
					}),
				_List_fromArray(
					[
						A3($author$project$App$Effect$SaveDiscovery, model.ad.aQ, org, snapshot)
					]))));
		}
	}
};
var $author$project$App$PageState$toggleGuide = function (state) {
	return _Utils_update(
		state,
		{aI: !state.aI});
};
var $elm$core$Basics$clamp = F3(
	function (low, high, number) {
		return (_Utils_cmp(number, low) < 0) ? low : ((_Utils_cmp(number, high) > 0) ? high : number);
	});
var $author$project$Ui$ResponsibilityGraph$update = F2(
	function (msg, state) {
		switch (msg.$) {
			case 0:
				var value = msg.a;
				return _Utils_update(
					state,
					{Y: value});
			case 1:
				var value = msg.a;
				return _Utils_update(
					state,
					{d9: value, es: $elm$core$Maybe$Nothing});
			case 2:
				var value = msg.a;
				return _Utils_update(
					state,
					{av: value});
			case 3:
				var value = msg.a;
				return _Utils_update(
					state,
					{
						es: ((!value) && A2(
							$elm$core$Maybe$withDefault,
							false,
							A2(
								$elm$core$Maybe$map,
								$elm$core$String$startsWith('ResourceNode:'),
								state.es))) ? $elm$core$Maybe$Nothing : state.es,
						L: value
					});
			case 4:
				var key = msg.a;
				return _Utils_update(
					state,
					{
						es: $elm$core$Maybe$Just(key)
					});
			case 5:
				return _Utils_update(
					state,
					{es: $elm$core$Maybe$Nothing});
			case 6:
				var delta = msg.a;
				return _Utils_update(
					state,
					{
						F: A3($elm$core$Basics$clamp, 1, 3, state.F + delta)
					});
			default:
				return _Utils_update(
					state,
					{F: 1});
		}
	});
var $author$project$App$PageState$updateGraph = F2(
	function (message, state) {
		return _Utils_update(
			state,
			{
				ap: A2($author$project$Ui$ResponsibilityGraph$update, message, state.ap)
			});
	});
var $author$project$App$Update$update = F2(
	function (msg, model) {
		update:
		while (true) {
			switch (msg.$) {
				case 23:
					var token = msg.a;
					var org = msg.b;
					var response = msg.c;
					return ((!_Utils_eq(token, model.ad.aQ)) || (!_Utils_eq(
						$elm$core$Maybe$Just(org),
						model.ad.aL))) ? _Utils_Tuple2(model, _List_Nil) : _Utils_Tuple2(
						_Utils_update(
							model,
							{
								Z: A3($author$project$App$Discovery$receive, org, response, model.Z)
							}),
						_List_Nil);
				case 24:
					var change = msg.a;
					return ($author$project$App$Update$busy(model) || (model.Z.aJ || A2(
						$elm$core$Maybe$withDefault,
						false,
						A2(
							$elm$core$Maybe$map,
							function (org) {
								return A2($elm$core$Dict$member, org, model.Z.I);
							},
							model.ad.aL)))) ? _Utils_Tuple2(model, _List_Nil) : _Utils_Tuple2(
						_Utils_update(
							model,
							{
								Z: A2(
									$elm$core$Maybe$withDefault,
									model.Z,
									A2(
										$elm$core$Maybe$map,
										function (org) {
											return A3($author$project$App$Discovery$edit, org, change, model.Z);
										},
										model.ad.aL))
							}),
						_List_Nil);
				case 25:
					var $temp$msg = $author$project$App$Update$EditDiscovery(
						$author$project$Domain$Discovery$AddObservation(
							'observation-' + (model.aH.b6 + ('-' + $elm$core$String$fromInt(model.w.b7))))),
						$temp$model = _Utils_update(
						model,
						{
							w: $author$project$App$Drafts$advanceSerial(model.w)
						});
					msg = $temp$msg;
					model = $temp$model;
					continue update;
				case 26:
					var $temp$msg = $author$project$App$Update$EditDiscovery(
						$author$project$Domain$Discovery$AddWorkflow(
							'workflow-' + (model.aH.b6 + ('-' + $elm$core$String$fromInt(model.w.b7))))),
						$temp$model = _Utils_update(
						model,
						{
							w: $author$project$App$Drafts$advanceSerial(model.w)
						});
					msg = $temp$msg;
					model = $temp$model;
					continue update;
				case 29:
					return $author$project$App$Update$busy(model) ? _Utils_Tuple2(model, _List_Nil) : _Utils_Tuple2(
						_Utils_update(
							model,
							{
								Z: A2(
									$elm$core$Maybe$withDefault,
									model.Z,
									A2(
										$elm$core$Maybe$map,
										function (org) {
											return A2($author$project$App$Discovery$clearDraft, org, model.Z);
										},
										model.ad.aL))
							}),
						_List_Nil);
				case 30:
					return $author$project$App$Update$busy(model) ? _Utils_Tuple2(model, _List_Nil) : _Utils_Tuple2(
						_Utils_update(
							model,
							{
								Z: A2(
									$elm$core$Maybe$withDefault,
									model.Z,
									A2(
										$elm$core$Maybe$map,
										function (org) {
											return A2($author$project$App$Discovery$rebase, org, model.Z);
										},
										model.ad.aL)),
								bN: '최신 버전에 입력을 다시 적용했습니다. 내용을 검토한 뒤 저장하세요.'
							}),
						_List_Nil);
				case 27:
					return $author$project$App$Update$submitDiscovery(model);
				case 28:
					var token = msg.a;
					var org = msg.b;
					var response = msg.c;
					if ((!_Utils_eq(token, model.ad.aQ)) || (!_Utils_eq(
						$elm$core$Maybe$Just(org),
						model.ad.aL))) {
						return _Utils_Tuple2(model, _List_Nil);
					} else {
						if (!response.$) {
							return $author$project$App$Update$refresh(
								_Utils_update(
									model,
									{
										Z: A2($author$project$App$Discovery$clearDraft, org, model.Z),
										bt: false,
										bN: '현황을 저장했습니다. 저장된 근거로 에이전트 초안을 다시 확인하세요.',
										ad: $author$project$App$Session$finishSave(model.ad)
									}));
						} else {
							var message = response.a;
							return $author$project$App$Update$refresh(
								_Utils_update(
									model,
									{
										bt: true,
										bN: message + ' 입력은 보존했습니다. 최신 저장 내용과 비교한 뒤 다시 적용하세요.',
										ad: $author$project$App$Session$finishSave(model.ad)
									}));
						}
					}
				case 31:
					var token = msg.a;
					var org = msg.b;
					var response = msg.c;
					return ((!_Utils_eq(token, model.ad.aQ)) || (!_Utils_eq(
						$elm$core$Maybe$Just(org),
						model.ad.aL))) ? _Utils_Tuple2(model, _List_Nil) : _Utils_Tuple2(
						_Utils_update(
							model,
							{
								N: A3($author$project$App$Agents$receive, org, response, model.N)
							}),
						_List_Nil);
				case 32:
					var change = msg.a;
					return ($author$project$App$Update$busy(model) || (model.N.aJ || A2(
						$elm$core$Maybe$withDefault,
						false,
						A2(
							$elm$core$Maybe$map,
							function (org) {
								return A2($elm$core$Dict$member, org, model.N.I);
							},
							model.ad.aL)))) ? _Utils_Tuple2(model, _List_Nil) : _Utils_Tuple2(
						_Utils_update(
							model,
							{
								N: A2(
									$elm$core$Maybe$withDefault,
									model.N,
									A2(
										$elm$core$Maybe$map,
										function (org) {
											return A3($author$project$App$Agents$edit, org, change, model.N);
										},
										model.ad.aL))
							}),
						_List_Nil);
				case 33:
					var _v2 = A2(
						$elm$core$Maybe$andThen,
						function (org) {
							return A2($author$project$App$Agents$saved, org, model.N);
						},
						model.ad.aL);
					if (!_v2.$) {
						var snapshot = _v2.a;
						var $temp$msg = $author$project$App$Update$EditAgents(
							$author$project$Domain$Agent$Import(snapshot.q)),
							$temp$model = _Utils_update(
							model,
							{bt: false, bN: '규칙 기반 초안을 설계안으로 가져왔습니다. 등급, 승인 주체, 인계 대상을 검토한 뒤 저장하세요.'});
						msg = $temp$msg;
						model = $temp$model;
						continue update;
					} else {
						return _Utils_Tuple2(model, _List_Nil);
					}
				case 36:
					return $author$project$App$Update$busy(model) ? _Utils_Tuple2(model, _List_Nil) : _Utils_Tuple2(
						_Utils_update(
							model,
							{
								N: A2(
									$elm$core$Maybe$withDefault,
									model.N,
									A2(
										$elm$core$Maybe$map,
										function (org) {
											return A2($author$project$App$Agents$clearDraft, org, model.N);
										},
										model.ad.aL))
							}),
						_List_Nil);
				case 37:
					return $author$project$App$Update$busy(model) ? _Utils_Tuple2(model, _List_Nil) : _Utils_Tuple2(
						_Utils_update(
							model,
							{
								N: A2(
									$elm$core$Maybe$withDefault,
									model.N,
									A2(
										$elm$core$Maybe$map,
										function (org) {
											return A2($author$project$App$Agents$rebase, org, model.N);
										},
										model.ad.aL)),
								bN: '최신 버전에 설계안 입력을 다시 적용했습니다. 내용을 검토한 뒤 저장하세요.'
							}),
						_List_Nil);
				case 34:
					return $author$project$App$Update$submitAgents(model);
				case 35:
					var token = msg.a;
					var org = msg.b;
					var response = msg.c;
					if ((!_Utils_eq(token, model.ad.aQ)) || (!_Utils_eq(
						$elm$core$Maybe$Just(org),
						model.ad.aL))) {
						return _Utils_Tuple2(model, _List_Nil);
					} else {
						if (!response.$) {
							return $author$project$App$Update$refresh(
								_Utils_update(
									model,
									{
										N: A2($author$project$App$Agents$clearDraft, org, model.N),
										bt: false,
										bN: '에이전트 설계안을 저장했습니다. 저장된 설계의 진단과 구조 화면을 확인하세요.',
										ad: $author$project$App$Session$finishSave(model.ad)
									}));
						} else {
							var message = response.a;
							return $author$project$App$Update$refresh(
								_Utils_update(
									model,
									{
										bt: true,
										bN: message + ' 설계안 입력은 보존했습니다. 최신 저장 내용과 비교한 뒤 다시 적용하세요.',
										ad: $author$project$App$Session$finishSave(model.ad)
									}));
						}
					}
				case 2:
					var state = msg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								bR: A2($author$project$App$PageState$setActivity, state, model.bR)
							}),
						_List_Nil);
				case 3:
					var review = msg.a;
					return $author$project$App$Update$busy(model) ? _Utils_Tuple2(model, _List_Nil) : A2(
						$elm$core$Tuple$mapFirst,
						function (next) {
							return _Utils_update(
								next,
								{
									bR: A2(
										$author$project$App$PageState$setActivity,
										{
											dl: '',
											dB: '',
											d9: '',
											b1: $elm$core$Maybe$Just(review),
											eU: ''
										},
										next.bR)
								});
						},
						A2(
							$author$project$App$Update$update,
							A2($author$project$App$Update$Guide, 11, 'audit-history'),
							model));
				case 5:
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
									bR: A2($author$project$App$PageState$setPage, 5, model.bR)
								});
							msg = $temp$msg;
							model = $temp$model;
							continue update;
						} else {
							if (A2($elm$core$String$startsWith, 'authority-', target)) {
								var $temp$msg = A2($author$project$App$Update$Guide, 8, target),
									$temp$model = model;
								msg = $temp$msg;
								model = $temp$model;
								continue update;
							} else {
								var $temp$msg = A2($author$project$App$Update$Guide, 7, target),
									$temp$model = model;
								msg = $temp$msg;
								model = $temp$model;
								continue update;
							}
						}
					}
				case 4:
					var graphMsg = msg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								bR: A2($author$project$App$PageState$updateGraph, graphMsg, model.bR)
							}),
						_List_Nil);
				case 1:
					var page = msg.a;
					var mode = msg.b;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								bR: A3($author$project$App$PageState$setListMode, page, mode, model.bR)
							}),
						_List_Nil);
				case 0:
					var page = msg.a;
					var org = msg.b;
					return $author$project$App$Update$busy(model) ? _Utils_Tuple2(model, _List_Nil) : ((_Utils_eq(org, model.ad.aL) && (!_Utils_eq(org, $elm$core$Maybe$Nothing))) ? _Utils_Tuple2(
						_Utils_update(
							model,
							{
								w: $author$project$App$Drafts$closeDelete(model.w),
								bR: A2($author$project$App$PageState$setPage, page, model.bR)
							}),
						_List_Nil) : $author$project$App$Update$refresh(
						_Utils_update(
							model,
							{
								bt: false,
								w: $author$project$App$Drafts$closeDelete(model.w),
								bN: '',
								bR: A2($author$project$App$PageState$navigate, page, model.bR),
								ad: A2($author$project$App$Session$selectOrganization, org, model.ad)
							})));
				case 6:
					return $author$project$App$Update$busy(model) ? _Utils_Tuple2(model, _List_Nil) : $author$project$App$Update$refresh(model);
				case 7:
					var token = msg.a;
					var response = msg.b;
					return A4(
						$author$project$App$Update$receive,
						token,
						response,
						A2($author$project$App$Session$receiveOrganizations, response, model.ad),
						model);
				case 8:
					var token = msg.a;
					var response = msg.b;
					return A4(
						$author$project$App$Update$receive,
						token,
						response,
						A2($author$project$App$Session$receiveWorkspace, response, model.ad),
						model);
				case 9:
					var action = msg.a;
					var key = msg.b;
					var val = msg.c;
					if ($author$project$App$Update$busy(model)) {
						return _Utils_Tuple2(model, _List_Nil);
					} else {
						switch (action.$) {
							case 6:
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
							case 11:
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
											w: A4($author$project$App$Drafts$edit, action, key, val, model)
										}),
									_List_Nil);
						}
					}
				case 10:
					var field = msg.a;
					var val = msg.b;
					if ($author$project$App$Update$busy(model)) {
						return _Utils_Tuple2(model, _List_Nil);
					} else {
						var metric = function () {
							var _v8 = model.ad.bb;
							if (_v8.$ === 1) {
								var workspace = _v8.a;
								return A2(
									$elm$core$Dict$get,
									val,
									$elm$core$Dict$fromList(
										A2(
											$elm$core$List$map,
											function (metric_) {
												return _Utils_Tuple2(metric_.du, metric_);
											},
											A2(
												$elm$core$List$map,
												A2(
													$elm$core$Basics$composeR,
													function ($) {
														return $.a_;
													},
													function ($) {
														return $.dJ;
													}),
												workspace.$7))));
							} else {
								return $elm$core$Maybe$Nothing;
							}
						}();
						var next = function () {
							if (_Utils_eq(field, $author$project$Form$Goal$MetricId)) {
								if (!metric.$) {
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
														w: A3($author$project$App$Drafts$editGoal, key, content, current)
													});
											}),
										model,
										_List_fromArray(
											[
												_Utils_Tuple2($author$project$Form$Goal$MetricId, selected.du),
												_Utils_Tuple2($author$project$Form$Goal$MetricName, selected.dO),
												_Utils_Tuple2($author$project$Form$Goal$Unit, selected.eT),
												_Utils_Tuple2($author$project$Form$Goal$Direction, selected.c4)
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
														w: A3($author$project$App$Drafts$editGoal, key, content, current)
													});
											}),
										_Utils_update(
											model,
											{
												w: $author$project$App$Drafts$advanceSerial(model.w)
											}),
										_List_fromArray(
											[
												_Utils_Tuple2(
												$author$project$Form$Goal$MetricId,
												'metric-' + (model.aH.b6 + ('-new-' + $elm$core$String$fromInt(model.w.b7)))),
												_Utils_Tuple2($author$project$Form$Goal$MetricName, ''),
												_Utils_Tuple2($author$project$Form$Goal$Unit, ''),
												_Utils_Tuple2($author$project$Form$Goal$Direction, 'HigherIsBetter')
											]));
								}
							} else {
								return _Utils_update(
									model,
									{
										w: A3($author$project$App$Drafts$editGoal, field, val, model)
									});
							}
						}();
						return _Utils_Tuple2(next, _List_Nil);
					}
				case 11:
					var field = msg.a;
					var val = msg.b;
					return $author$project$App$Update$busy(model) ? _Utils_Tuple2(model, _List_Nil) : _Utils_Tuple2(
						_Utils_update(
							model,
							{
								w: A3($author$project$App$Drafts$editReview, field, val, model)
							}),
						_List_Nil);
				case 12:
					var action = msg.a;
					return A2($author$project$App$Update$submit, action, model);
				case 13:
					var token = msg.a;
					var action = msg.b;
					var response = msg.c;
					return (!_Utils_eq(token, model.ad.aQ)) ? _Utils_Tuple2(model, _List_Nil) : A3($author$project$App$Update$saved, action, response, model);
				case 14:
					var _v9 = model.ad.bb;
					if (_v9.$ === 1) {
						var workspace = _v9.a;
						return (model.ad.ao && (!$author$project$App$Update$busy(model))) ? _Utils_Tuple2(
							_Utils_update(
								model,
								{
									w: A2(
										$author$project$App$Drafts$openDelete,
										{bi: '', du: workspace.bQ.du, dO: workspace.bQ.dO, aC: workspace.aC},
										model.w)
								}),
							_List_fromArray(
								[
									$author$project$App$Effect$FocusElement('delete-confirm')
								])) : _Utils_Tuple2(
							_Utils_update(
								model,
								{bt: true, bN: '최신 조직 정보를 불러온 뒤 다시 확인하세요.'}),
							_List_Nil);
					} else {
						return _Utils_Tuple2(model, _List_Nil);
					}
				case 15:
					var name = msg.a;
					return $author$project$App$Update$busy(model) ? _Utils_Tuple2(model, _List_Nil) : _Utils_Tuple2(
						_Utils_update(
							model,
							{
								w: A2($author$project$App$Drafts$confirmDelete, name, model.w)
							}),
						_List_Nil);
				case 16:
					return $author$project$App$Update$busy(model) ? _Utils_Tuple2(model, _List_Nil) : _Utils_Tuple2(
						_Utils_update(
							model,
							{
								w: $author$project$App$Drafts$closeDelete(model.w)
							}),
						_List_Nil);
				case 17:
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								bR: $author$project$App$PageState$toggleGuide(model.bR)
							}),
						_List_Nil);
				case 18:
					var page = msg.a;
					var target = msg.b;
					return $author$project$App$Update$busy(model) ? _Utils_Tuple2(model, _List_Nil) : _Utils_Tuple2(
						_Utils_update(
							model,
							{
								w: ((page === 10) && (target === 'review-form')) ? $author$project$App$Drafts$prepareReview(model) : model.w,
								bR: A3($author$project$App$PageState$guide, page, target, model.bR)
							}),
						_List_fromArray(
							[
								$author$project$App$Effect$FocusElement(target)
							]));
				case 19:
					var query = msg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								bR: A2($author$project$App$PageState$searchPeople, query, model.bR)
							}),
						_List_Nil);
				case 20:
					var status = msg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								bR: A2($author$project$App$PageState$filterPeople, status, model.bR)
							}),
						_List_Nil);
				case 21:
					var key = msg.a;
					return $author$project$App$Update$busy(model) ? _Utils_Tuple2(model, _List_Nil) : $author$project$App$Update$refresh(
						_Utils_update(
							model,
							{
								bt: false,
								w: A2($author$project$App$Drafts$resetPerson, key, model),
								bN: '구성원 수정·인계 입력을 초기화하고 최신 정보를 불러옵니다. 확인한 뒤 다시 작성하세요.'
							}));
				case 22:
					var key = msg.a;
					return $author$project$App$Update$busy(model) ? _Utils_Tuple2(model, _List_Nil) : _Utils_Tuple2(
						_Utils_update(
							model,
							{
								bR: A2($author$project$App$PageState$openPerson, key, model.bR)
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
var $author$project$Page$AgentDrafts = 3;
var $author$project$Page$AgentGraph = 4;
var $author$project$Page$Dashboard = 6;
var $author$project$Page$Discovery = 1;
var $author$project$App$Update$Navigate = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$App$Update$Refresh = {$: 6};
var $author$project$Page$Results = 9;
var $author$project$App$Update$SetListMode = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $author$project$Page$Settings = 12;
var $author$project$Page$Workflows = 2;
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
var $author$project$Main$busy = function (model) {
	return $author$project$App$Session$busy(model.ad);
};
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
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
var $author$project$Ui$ListView$Cards = 0;
var $author$project$Ui$ListView$Table = 1;
var $elm$html$Html$div = _VirtualDom_node('div');
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
var $elm$html$Html$span = _VirtualDom_node('span');
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
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
									_Utils_Tuple2('secondary', !(!mode))
								])),
							A2(
							$elm$html$Html$Attributes$attribute,
							'aria-pressed',
							(!mode) ? 'true' : 'false'),
							$elm$html$Html$Events$onClick(
							change(0))
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
									_Utils_Tuple2('secondary', mode !== 1)
								])),
							A2(
							$elm$html$Html$Attributes$attribute,
							'aria-pressed',
							(mode === 1) ? 'true' : 'false'),
							$elm$html$Html$Events$onClick(
							change(1))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('표')
						]))
				]));
	});
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $elm$html$Html$footer = _VirtualDom_node('footer');
var $author$project$App$Update$Edit = F3(
	function (a, b, c) {
		return {$: 9, a: a, b: b, c: c};
	});
var $author$project$App$Update$Submit = function (a) {
	return {$: 12, a: a};
};
var $author$project$Main$formConfig = function (model) {
	return {
		aE: $author$project$Main$busy(model),
		am: $author$project$App$Update$Edit,
		ao: model.ad.ao,
		au: function () {
			var _v0 = model.ad.au;
			if (!_v0.$) {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = _v0.a;
				return $elm$core$Maybe$Just(key);
			}
		}(),
		eE: $author$project$App$Update$Submit,
		ba: $author$project$App$Drafts$get(model)
	};
};
var $elm$html$Html$h1 = _VirtualDom_node('h1');
var $elm$html$Html$h2 = _VirtualDom_node('h2');
var $elm$html$Html$header = _VirtualDom_node('header');
var $elm$html$Html$Attributes$href = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $author$project$App$PageState$listMode = function (state) {
	return A2(
		$elm$core$Maybe$withDefault,
		1,
		A2(
			$elm$core$Dict$get,
			$author$project$Page$pageName(state.ac),
			state.as));
};
var $author$project$Main$listMode = function (model) {
	return $author$project$App$PageState$listMode(model.bR);
};
var $elm$html$Html$main_ = _VirtualDom_node('main');
var $elm$html$Html$nav = _VirtualDom_node('nav');
var $author$project$Main$navigationButton = F2(
	function (model, page) {
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
							_Utils_eq(model.bR.ac, page))
						])),
					A2(
					$elm$html$Html$Attributes$attribute,
					'aria-current',
					_Utils_eq(model.bR.ac, page) ? 'page' : 'false'),
					$elm$html$Html$Attributes$disabled(
					$author$project$Main$busy(model) || _Utils_eq(model.ad.aL, $elm$core$Maybe$Nothing)),
					$elm$html$Html$Events$onClick(
					A2($author$project$App$Update$Navigate, page, model.ad.aL))
				]),
			_List_fromArray(
				[
					$elm$html$Html$text(
					$author$project$Page$pageName(page))
				]));
	});
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$html$Html$p = _VirtualDom_node('p');
var $elm$html$Html$section = _VirtualDom_node('section');
var $elm$html$Html$strong = _VirtualDom_node('strong');
var $elm$html$Html$Attributes$tabindex = function (n) {
	return A2(
		_VirtualDom_attribute,
		'tabIndex',
		$elm$core$String$fromInt(n));
};
var $author$project$Form$Action$CreateOrg = {$: 0};
var $author$project$Form$Action$ImportDemo = {$: 1};
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
					model.eE(action))
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$fieldset,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$disabled(model.aE)
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
										$elm$html$Html$Attributes$disabled(!model.ao)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(
										_Utils_eq(
											model.au,
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
var $author$project$Ui$Form$fieldError = F3(
	function (kind, required_, current) {
		return (required_ && ($elm$core$String$trim(current) === '')) ? '필수 항목입니다. 내용을 입력하세요.' : (((kind === 'number') && ((current !== '') && _Utils_eq(
			$elm$core$String$toFloat(current),
			$elm$core$Maybe$Nothing))) ? '숫자로 입력하세요.' : '');
	});
var $elm$html$Html$Attributes$for = $elm$html$Html$Attributes$stringProperty('htmlFor');
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
var $elm$html$Html$small = _VirtualDom_node('small');
var $elm$html$Html$Attributes$step = function (n) {
	return A2($elm$html$Html$Attributes$stringProperty, 'step', n);
};
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
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
			A2(model.ba, action, key),
			A2(model.am, action, key));
	});
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
var $elm$html$Html$Attributes$scope = $elm$html$Html$Attributes$stringProperty('scope');
var $elm$html$Html$caption = _VirtualDom_node('caption');
var $elm$html$Html$table = _VirtualDom_node('table');
var $elm$html$Html$tbody = _VirtualDom_node('tbody');
var $elm$html$Html$th = _VirtualDom_node('th');
var $elm$html$Html$thead = _VirtualDom_node('thead');
var $elm$html$Html$tr = _VirtualDom_node('tr');
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
var $elm$html$Html$td = _VirtualDom_node('td');
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
										$elm$html$Html$text(item.bQ.dO)
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										item.bp ? '가상 데이터 · 데모' : '내 조직')
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										$elm$core$String$fromInt(item.d5) + '명')
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										$elm$core$String$fromInt(item.dm) + '개')
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										A2($elm$core$String$left, 10, item.bQ.cV))
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
														$elm$html$Html$Attributes$disabled(model.w.aE),
														$elm$html$Html$Events$onClick(
														model.bP(item.bQ.du))
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
														$elm$html$Html$Attributes$disabled(model.w.aE),
														$elm$html$Html$Events$onClick(
														model.ev(item.bQ.du))
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
							model.w,
							$author$project$Form$Action$CreateOrg,
							'조직 등록',
							_List_fromArray(
								[
									A6($author$project$Ui$Form$inputField, model.w, $author$project$Form$Action$CreateOrg, '조직 이름', 'name', 'text', true)
								])),
							A2(
							$elm$html$Html$button,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('secondary'),
									$elm$html$Html$Attributes$disabled(
									model.w.aE || ((!model.w.ao) || function () {
										var _v0 = model.aM;
										if (_v0.$ === 1) {
											var items = _v0.a;
											return A2(
												$elm$core$List$any,
												A2(
													$elm$core$Basics$composeR,
													function ($) {
														return $.bQ;
													},
													A2(
														$elm$core$Basics$composeR,
														function ($) {
															return $.du;
														},
														$elm$core$Basics$eq('demo-northstar-v2'))),
												items);
										} else {
											return true;
										}
									}())),
									$elm$html$Html$Events$onClick(
									model.w.eE($author$project$Form$Action$ImportDemo))
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('체험용 데모 조직 추가')
								]))
						])),
					A2(
					$author$project$Remote$view,
					model.aM,
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
									$elm$core$List$isEmpty(items) ? A2($author$project$Ui$Common$emptyState, '첫 조직을 시작하세요', '조직 이름을 등록한 뒤 조직 열기로 현황을 입력하세요. 데모 조직에서는 기존 목표·책임·권한 운영 흐름을 체험할 수 있습니다.') : ((mode === 1) ? A2($author$project$Page$Organizations$organizationTable, model, items) : A2(
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
																item.bp ? '가상 데이터 · 데모' : '내 조직')
															])),
														A2(
														$elm$html$Html$h2,
														_List_Nil,
														_List_fromArray(
															[
																$elm$html$Html$text(item.bQ.dO)
															])),
														A2(
														$elm$html$Html$p,
														_List_Nil,
														_List_fromArray(
															[
																$elm$html$Html$text(
																'구성원 ' + ($elm$core$String$fromInt(item.d5) + ('명 · 목표 ' + ($elm$core$String$fromInt(item.dm) + '개'))))
															])),
														A2(
														$elm$html$Html$small,
														_List_Nil,
														_List_fromArray(
															[
																$elm$html$Html$text(
																'등록 ' + A2($elm$core$String$left, 10, item.bQ.cV))
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
																		$elm$html$Html$Attributes$disabled(model.w.aE),
																		$elm$html$Html$Events$onClick(
																		model.bP(item.bQ.du))
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
																		$elm$html$Html$Attributes$disabled(model.w.aE),
																		$elm$html$Html$Events$onClick(
																		model.ev(item.bQ.du))
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
var $author$project$App$Update$ActivityChange = function (a) {
	return {$: 2, a: a};
};
var $author$project$App$Update$CloseDelete = {$: 16};
var $author$project$App$Update$ConfirmDelete = function (a) {
	return {$: 15, a: a};
};
var $author$project$App$Update$FilterPeople = function (a) {
	return {$: 20, a: a};
};
var $author$project$App$Update$GraphGo = function (a) {
	return {$: 5, a: a};
};
var $author$project$App$Update$GraphMsg = function (a) {
	return {$: 4, a: a};
};
var $author$project$App$Update$ImportAgentDrafts = {$: 33};
var $author$project$App$Update$OpenDelete = {$: 14};
var $author$project$App$Update$OpenReviewActivity = function (a) {
	return {$: 3, a: a};
};
var $author$project$App$Update$RebaseAgents = {$: 37};
var $author$project$App$Update$ResetAgents = {$: 36};
var $author$project$App$Update$ResetPerson = function (a) {
	return {$: 21, a: a};
};
var $author$project$App$Update$SearchPeople = function (a) {
	return {$: 19, a: a};
};
var $author$project$App$Update$SubmitAgents = {$: 34};
var $author$project$App$Update$ToggleGuide = {$: 17};
var $author$project$App$Update$AddObservation = {$: 25};
var $author$project$App$Update$AddWorkflow = {$: 26};
var $author$project$App$Update$RebaseDiscovery = {$: 30};
var $author$project$App$Update$ResetDiscovery = {$: 29};
var $author$project$App$Update$SubmitDiscovery = {$: 27};
var $author$project$App$Discovery$changed = F2(
	function (org, state) {
		return !_Utils_eq(
			A2(
				$elm$core$Maybe$map,
				function ($) {
					return $.Z;
				},
				A2($author$project$App$Discovery$current, org, state)),
			A2(
				$elm$core$Maybe$map,
				function ($) {
					return $.Z;
				},
				A2($author$project$App$Discovery$saved, org, state)));
	});
var $elm$html$Html$details = _VirtualDom_node('details');
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
var $author$project$Page$Discovery$documentSummary = function (doc) {
	return A2(
		$elm$core$String$join,
		'\n',
		_Utils_ap(
			_List_fromArray(
				['범위: ' + doc.eq, '기준일: ' + doc.cz]),
			_Utils_ap(
				A2(
					$elm$core$List$map,
					function (o) {
						return o.eD + (' / ' + ($author$project$Domain$Discovery$statusLabel(o.ez) + (' / ' + (o.o + (' / 근거: ' + o.de)))));
					},
					doc.dZ),
				_Utils_ap(
					A2(
						$elm$core$List$map,
						function (w) {
							return A2(
								$elm$core$String$join,
								' / ',
								_List_fromArray(
									[
										w.dO,
										w.eo,
										w.eR,
										w.dz,
										w.eP,
										w.d2,
										w.dp,
										w.cv,
										$author$project$Domain$Discovery$statusLabel(w.ez),
										w.de
									]));
						},
						doc.eX),
					_List_fromArray(
						['검토: ' + (doc.b1.ez + (' / ' + doc.b1.dY))])))));
};
var $author$project$Domain$Discovery$empty = {
	cz: '',
	dZ: _List_Nil,
	b1: {dY: '', ez: 'pending'},
	eq: '',
	eX: _List_Nil
};
var $author$project$Domain$Discovery$AsOf = function (a) {
	return {$: 1, a: a};
};
var $author$project$Domain$Discovery$Scope = function (a) {
	return {$: 0, a: a};
};
var $elm$html$Html$Attributes$rows = function (n) {
	return A2(
		_VirtualDom_attribute,
		'rows',
		$elm$core$String$fromInt(n));
};
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
var $author$project$Ui$Form$guidedInput = F7(
	function (key, title, hint, kind, required_, current, edit) {
		return A8($author$project$Ui$Form$guidedInputNamed, key, key, title, hint, kind, required_, current, edit);
	});
var $author$project$Domain$Discovery$ObservationField = F3(
	function (a, b, c) {
		return {$: 3, a: a, b: b, c: c};
	});
var $author$project$Domain$Discovery$RemoveObservation = function (a) {
	return {$: 10, a: a};
};
var $author$project$Page$Discovery$evidenceHint = function (status) {
	return (status === 'confirmed') ? '필수: 문서명·확인한 담당자·확인 날짜 등 확인 가능한 근거를 적으세요.' : '예: 9월 운영 매뉴얼, 담당자 인터뷰 또는 확인할 사람과 질문';
};
var $elm$html$Html$h3 = _VirtualDom_node('h3');
var $elm$html$Html$option = _VirtualDom_node('option');
var $elm$html$Html$select = _VirtualDom_node('select');
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
							(item.eD === '') ? '새 현황 항목 · 미확인' : item.eD)
						])),
					A7(
					$author$project$Ui$Form$guidedInput,
					item.du + '-subject',
					'현황 항목',
					'예: 고객지원팀의 환불 승인 권한',
					'text',
					true,
					item.eD,
					A2(
						$elm$core$Basics$composeL,
						controls.am,
						A2($author$project$Domain$Discovery$ObservationField, item.du, 'subject'))),
					A6(
					$author$project$Ui$Form$guidedArea,
					item.du + '-detail',
					'내용',
					'현재 알고 있는 내용만 적으세요. 미확인은 부분 입력도 가능합니다.',
					false,
					item.o,
					A2(
						$elm$core$Basics$composeL,
						controls.am,
						A2($author$project$Domain$Discovery$ObservationField, item.du, 'detail'))),
					A3(
					$author$project$Page$Discovery$statusField,
					item.du + '-status',
					item.ez,
					A2(
						$elm$core$Basics$composeL,
						controls.am,
						A2($author$project$Domain$Discovery$ObservationField, item.du, 'status'))),
					A6(
					$author$project$Ui$Form$guidedArea,
					item.du + '-evidence',
					'입력 근거 / 확인할 곳',
					$author$project$Page$Discovery$evidenceHint(item.ez),
					item.ez === 'confirmed',
					item.de,
					A2(
						$elm$core$Basics$composeL,
						controls.am,
						A2($author$project$Domain$Discovery$ObservationField, item.du, 'evidence'))),
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$type_('button'),
							$elm$html$Html$Attributes$class('secondary'),
							$elm$html$Html$Events$onClick(
							controls.am(
								$author$project$Domain$Discovery$RemoveObservation(item.du)))
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
							doc.eq,
							A2($elm$core$Basics$composeL, controls.am, $author$project$Domain$Discovery$Scope)),
							A7(
							$author$project$Ui$Form$guidedInput,
							'discovery-asof',
							'현황 기준일',
							'이 정보가 유효한 날짜입니다. 확인 전이면 비워 두세요.',
							'date',
							false,
							doc.cz,
							A2($elm$core$Basics$composeL, controls.am, $author$project$Domain$Discovery$AsOf))
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
								doc.dZ),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$button,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$type_('button'),
											$elm$html$Html$Attributes$class('secondary'),
											$elm$html$Html$Events$onClick(controls.cr)
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('+ 현황 항목 추가')
										]))
								]))))
				]));
	});
var $elm$html$Html$pre = _VirtualDom_node('pre');
var $author$project$Domain$Discovery$ReviewNote = function (a) {
	return {$: 12, a: a};
};
var $author$project$Domain$Discovery$ReviewStatus = function (a) {
	return {$: 13, a: a};
};
var $elm$html$Html$Attributes$checked = $elm$html$Html$Attributes$boolProperty('checked');
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
var $author$project$Page$Discovery$sourceChanged = F2(
	function (doc, latest) {
		return (!_Utils_eq(
			_Utils_Tuple3(doc.eq, doc.cz, doc.dZ),
			_Utils_Tuple3(latest.eq, latest.cz, latest.dZ))) || (!_Utils_eq(doc.eX, latest.eX));
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
							(latest.b1.ez === 'reviewed') ? '저장 상태: 검토 완료. 입력 근거가 바뀌면 다시 검토해야 합니다.' : '저장 상태: 검토 대기. 역할 중복, 인계 누락과 사람 승인 조건을 확인하세요.'),
							A6(
							$author$project$Ui$Form$guidedArea,
							'agent-review-note',
							'검토 의견 / 수정할 제안',
							'예: 분류와 답변 역할을 분리하고 환불 실행은 사람 승인 후에만 허용',
							false,
							doc.b1.dY,
							A2($elm$core$Basics$composeL, controls.am, $author$project$Domain$Discovery$ReviewNote)),
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
											$elm$html$Html$Attributes$checked(doc.b1.ez === 'reviewed'),
											$elm$html$Html$Attributes$disabled(
											A2($author$project$Page$Discovery$sourceChanged, doc, latest) || $elm$core$List$isEmpty(latest.eX)),
											$elm$html$Html$Events$onCheck(
											function (checked_) {
												return controls.am(
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
var $elm$html$Html$summary = _VirtualDom_node('summary');
var $author$project$Domain$Discovery$RemoveWorkflow = function (a) {
	return {$: 11, a: a};
};
var $author$project$Domain$Discovery$WorkflowField = F3(
	function (a, b, c) {
		return {$: 5, a: a, b: b, c: c};
	});
var $author$project$Domain$Discovery$WorkflowApprovalPermission = F2(
	function (a, b) {
		return {$: 8, a: a, b: b};
	});
var $author$project$Domain$Discovery$WorkflowApprovalPerson = F2(
	function (a, b) {
		return {$: 7, a: a, b: b};
	});
var $author$project$Domain$Discovery$WorkflowHandoff = F3(
	function (a, b, c) {
		return {$: 9, a: a, b: b, c: c};
	});
var $author$project$Domain$Discovery$WorkflowRolePerson = F2(
	function (a, b) {
		return {$: 6, a: a, b: b};
	});
var $elm$html$Html$legend = _VirtualDom_node('legend');
var $author$project$Ui$Form$peopleOptions = function (w) {
	return A2(
		$elm$core$List$cons,
		_Utils_Tuple2('', '구성원 선택'),
		A2(
			$elm$core$List$map,
			function (p) {
				return _Utils_Tuple2(p.du, p.dO + (' · ' + p.eo));
			},
			A2(
				$elm$core$List$filter,
				function ($) {
					return $.bc;
				},
				w.d4)));
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
var $author$project$Page$Discovery$references = F4(
	function (controls, workspace, doc, item) {
		var permissionOptions = A2(
			$elm$core$List$cons,
			_Utils_Tuple2('', '권한 선택 안 함'),
			$author$project$Ui$Label$permissions);
		var others = A2(
			$elm$core$List$filter,
			function (w) {
				return !_Utils_eq(w.du, item.du);
			},
			doc.eX);
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
							item.du + '-role-person',
							A2($elm$core$Maybe$withDefault, '', item.ep),
							A2(
								$elm$core$Basics$composeL,
								controls.am,
								$author$project$Domain$Discovery$WorkflowRolePerson(item.du)),
							'담당 구성원',
							false,
							$author$project$Ui$Form$peopleOptions(workspace)),
							A6(
							$author$project$Ui$Form$selectValue,
							item.du + '-approval-person',
							A2($elm$core$Maybe$withDefault, '', item.cx),
							A2(
								$elm$core$Basics$composeL,
								controls.am,
								$author$project$Domain$Discovery$WorkflowApprovalPerson(item.du)),
							'승인 구성원',
							false,
							$author$project$Ui$Form$peopleOptions(workspace)),
							A6(
							$author$project$Ui$Form$selectValue,
							item.du + '-approval-permission',
							A2($elm$core$Maybe$withDefault, '', item.cw),
							A2(
								$elm$core$Basics$composeL,
								controls.am,
								$author$project$Domain$Discovery$WorkflowApprovalPermission(item.du)),
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
														A2($elm$core$List$member, other.du, item.bA)),
														$elm$html$Html$Events$onCheck(
														A2(
															$elm$core$Basics$composeL,
															controls.am,
															A2($author$project$Domain$Discovery$WorkflowHandoff, item.du, other.du)))
													]),
												_List_Nil),
												$elm$html$Html$text(
												($elm$core$String$trim(other.dO) === '') ? ('이름 없는 업무 (' + (other.du + ')')) : other.dO)
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
							(item.dO === '') ? '새 업무 흐름' : item.dO)
						])),
					A7(
					$author$project$Ui$Form$guidedInput,
					item.du + '-name',
					'업무 이름',
					'예: 고객 문의 분류와 답변',
					'text',
					true,
					item.dO,
					A2(
						$elm$core$Basics$composeL,
						controls.am,
						A2($author$project$Domain$Discovery$WorkflowField, item.du, 'name'))),
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
								item.du + ('-' + key),
								title,
								hint,
								false,
								value_,
								A2(
									$elm$core$Basics$composeL,
									controls.am,
									A2($author$project$Domain$Discovery$WorkflowField, item.du, key)));
						},
						_List_fromArray(
							[
								_Utils_Tuple3(
								'role',
								'현재 담당 역할 / 구성원',
								_Utils_Tuple2('직급보다 실제 책임을 적으세요. 예: 고객지원 담당 김민서', item.eo)),
								_Utils_Tuple3(
								'trigger',
								'시작 조건',
								_Utils_Tuple2('무엇이 발생하면 시작하나요? 예: 새 문의 접수', item.eR)),
								_Utils_Tuple3(
								'inputs',
								'입력 정보',
								_Utils_Tuple2('예: 문의 내용, 고객 계약 정보', item.dz)),
								_Utils_Tuple3(
								'tools',
								'현재 사용하는 도구',
								_Utils_Tuple2('예: CRM, 고객지원 문서. 실제 접근 권한은 별도 확인합니다.', item.eP)),
								_Utils_Tuple3(
								'outputs',
								'산출물',
								_Utils_Tuple2('예: 문의 분류와 답변 초안', item.d2)),
								_Utils_Tuple3(
								'handoff',
								'전달 대상 / 인계 조건',
								_Utils_Tuple2('예: 환불 문의는 재무 담당자에게 금액과 사유 전달', item.dp)),
								_Utils_Tuple3(
								'approval',
								'사람의 승인 조건',
								_Utils_Tuple2('예: 환불 집행 전 재무 책임자 승인. 없음과 미확인을 구분하세요.', item.cv))
							]))),
					A4($author$project$Page$Discovery$references, controls, workspace, doc, item),
					A3(
					$author$project$Page$Discovery$statusField,
					item.du + '-status',
					item.ez,
					A2(
						$elm$core$Basics$composeL,
						controls.am,
						A2($author$project$Domain$Discovery$WorkflowField, item.du, 'status'))),
					A6(
					$author$project$Ui$Form$guidedArea,
					item.du + '-evidence',
					'입력 근거 / 확인할 곳',
					$author$project$Page$Discovery$evidenceHint(item.ez),
					item.ez === 'confirmed',
					item.de,
					A2(
						$elm$core$Basics$composeL,
						controls.am,
						A2($author$project$Domain$Discovery$WorkflowField, item.du, 'evidence'))),
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$type_('button'),
							$elm$html$Html$Attributes$class('secondary'),
							$elm$html$Html$Events$onClick(
							controls.am(
								$author$project$Domain$Discovery$RemoveWorkflow(item.du)))
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
							'등록된 구성원 역할 참고: ' + ($elm$core$List$isEmpty(workspace.d4) ? '구성원 화면에서 현재 담당자를 등록할 수 있습니다.' : A2(
								$elm$core$String$join,
								' · ',
								A2(
									$elm$core$List$map,
									function (p) {
										return p.dO + (' / ' + p.eo);
									},
									workspace.d4)))),
							$author$project$Ui$Common$note('예: 문의 접수 → 고객지원 담당 → CRM 고객 정보 확인 → 답변 초안 → 환불 건은 재무 담당자의 승인 후 처리')
						])),
					A2(
					$elm$html$Html$div,
					_List_Nil,
					A2(
						$elm$core$List$map,
						A3($author$project$Page$Discovery$workflow, controls, workspace, doc),
						doc.eX)),
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$type_('button'),
							$elm$html$Html$Attributes$class('secondary'),
							$elm$html$Html$Events$onClick(controls.cs)
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('+ 업무 흐름 추가')
						]))
				]));
	});
var $author$project$Page$Discovery$view = F3(
	function (page, controls, workspace) {
		var _v0 = A2($author$project$App$Discovery$current, controls.aL, controls.cb);
		if (_v0.$ === 1) {
			return A2(
				$author$project$Ui$Common$panel,
				'현황 불러오기',
				_List_fromArray(
					[
						$author$project$Ui$Common$note(
						A2(
							$elm$core$Maybe$withDefault,
							'조직 현황을 불러오고 있습니다…',
							A2($elm$core$Dict$get, controls.aL, controls.cb.I)))
					]));
		} else {
			var snapshot = _v0.a;
			var unsaved = A2($author$project$App$Discovery$changed, controls.aL, controls.cb);
			var unavailable = controls.cb.aJ || A2($elm$core$Dict$member, controls.aL, controls.cb.I);
			var latest = A2(
				$elm$core$Maybe$withDefault,
				$author$project$Domain$Discovery$empty,
				A2(
					$elm$core$Maybe$map,
					function ($) {
						return $.Z;
					},
					A2($author$project$App$Discovery$saved, controls.aL, controls.cb)));
			var doc = snapshot.Z;
			var conflict = A2($author$project$App$Discovery$conflicted, controls.aL, controls.cb);
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
							A2($elm$core$Dict$get, controls.aL, controls.cb.I))) : $elm$html$Html$text(''),
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
										$elm$html$Html$Attributes$disabled(controls.aE || unavailable),
										$elm$html$Html$Events$onClick(controls.bZ)
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
										$elm$html$Html$Attributes$disabled(controls.aE),
										$elm$html$Html$Events$onClick(controls.a5)
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
								$elm$html$Html$Events$onSubmit(controls.b4)
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$fieldset,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$disabled(controls.aE || unavailable)
									]),
								_List_fromArray(
									[
										function () {
										switch (page) {
											case 1:
												return A2($author$project$Page$Discovery$overview, controls, doc);
											case 2:
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
														controls.aE ? '저장 중…' : ((page === 3) ? '검토 의견과 상태 저장' : '현황 저장'))
													])),
												A2(
												$elm$html$Html$button,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$type_('button'),
														$elm$html$Html$Attributes$class('secondary'),
														$elm$html$Html$Events$onClick(
														controls.aZ(
															(page === 1) ? 2 : ((page === 2) ? 3 : 7)))
													]),
												_List_fromArray(
													[
														$elm$html$Html$text(
														(page === 1) ? '다음 · 업무 흐름 →' : ((page === 2) ? '다음 · 에이전트 초안 →' : '책임 관계 살펴보기 →'))
													]))
											]))
									]))
							]))
					]));
		}
	});
var $author$project$Main$discoveryPage = F3(
	function (page, model, w) {
		return A3(
			$author$project$Page$Discovery$view,
			page,
			{
				cr: $author$project$App$Update$AddObservation,
				cs: $author$project$App$Update$AddWorkflow,
				aE: $author$project$Main$busy(model),
				am: $author$project$App$Update$EditDiscovery,
				aZ: function (target) {
					return A2($author$project$App$Update$Navigate, target, model.ad.aL);
				},
				aL: w.bQ.du,
				bZ: $author$project$App$Update$RebaseDiscovery,
				a5: $author$project$App$Update$ResetDiscovery,
				b4: $author$project$App$Update$SubmitDiscovery,
				cb: model.Z
			},
			w);
	});
var $elm$html$Html$code = _VirtualDom_node('code');
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
										_Utils_Tuple2('error', d.ew === 'Error')
									]))
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$code,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(d.cQ)
									])),
								A2(
								$elm$html$Html$strong,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(d.dH)
									])),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('역할: ' + d.eD)
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
									d.c2))
							]));
				},
				items));
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
var $author$project$Ui$Label$personName = F2(
	function (w, key) {
		return A2(
			$elm$core$Maybe$withDefault,
			key,
			A2(
				$elm$core$Maybe$map,
				function (p) {
					return _Utils_ap(
						p.dO,
						p.bc ? '' : ' (비활성)');
				},
				$elm$core$List$head(
					A2(
						$elm$core$List$filter,
						A2(
							$elm$core$Basics$composeR,
							function ($) {
								return $.du;
							},
							$elm$core$Basics$eq(key)),
						w.d4))));
	});
var $author$project$Ui$AgentGraph$approvalLabel = F2(
	function (w, approval) {
		if (!approval.$) {
			var uid = approval.a;
			return A2($author$project$Ui$Label$personName, w, uid) + ' 승인';
		} else {
			var permission = approval.a;
			return $author$project$Ui$Label$permissionName(permission) + ' 권한자 승인';
		}
	});
var $elm$core$Set$member = F2(
	function (key, _v0) {
		var dict = _v0;
		return A2($elm$core$Dict$member, key, dict);
	});
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
								A2($elm$core$Dict$get, role.du, counts),
								$elm$core$Maybe$Nothing) || _Utils_eq(
								A2($elm$core$Dict$get, role.du, counts),
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
										role.dq);
								}),
							counts,
							ready);
						var readyIds = $elm$core$Set$fromList(
							A2(
								$elm$core$List$map,
								function ($) {
									return $.du;
								},
								ready));
						var next = A2(
							$elm$core$List$filter,
							function (role) {
								return !A2($elm$core$Set$member, role.du, readyIds);
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
				return $.du;
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
						role.dq));
			}),
		$elm$core$Dict$empty,
		roles);
	var byId = $elm$core$Dict$fromList(
		A2(
			$elm$core$List$map,
			function (role) {
				return _Utils_Tuple2(role.du, role);
			},
			roles));
	return A3(
		step,
		A2(
			$elm$core$List$filterMap,
			function (role) {
				return A2($elm$core$Dict$get, role.du, byId);
			},
			roles),
		incoming,
		_List_Nil);
};
var $author$project$Domain$Agent$levels = _List_fromArray(
	[
		_Utils_Tuple2('L0', 'L0 읽기'),
		_Utils_Tuple2('L1', 'L1 작업 공간 쓰기'),
		_Utils_Tuple2('L2', 'L2 외부 영향 · 사람 승인 필요'),
		_Utils_Tuple2('L3', 'L3 금지 · 사람이 직접 수행')
	]);
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
var $elm$html$Html$li = _VirtualDom_node('li');
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
					return _Utils_eq(d.eD, role.du);
				},
				diagnostics);
		};
		var node = function (pos) {
			var role = pos.eo;
			var flagged = A2(
				$elm$core$List$any,
				function (d) {
					return d.ew === 'Error';
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
						'translate(' + ($elm$core$String$fromFloat(pos.aD) + (',' + ($elm$core$String$fromFloat(pos.ag) + ')')))),
						A2($elm$html$Html$Attributes$attribute, 'role', 'listitem'),
						$elm$html$Html$Attributes$tabindex(0),
						A2(
						$elm$html$Html$Attributes$attribute,
						'aria-label',
						role.dO + (' · ' + ($author$project$Domain$Agent$levelLabel(role.dG) + A2(
							$elm$core$Maybe$withDefault,
							'',
							A2(
								$elm$core$Maybe$map,
								function (a) {
									return ' · ' + A2($author$project$Ui$AgentGraph$approvalLabel, w, a);
								},
								role.cv)))))
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
								A2($elm$core$String$left, 18, role.dO))
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
								$author$project$Domain$Agent$levelLabel(role.dG))
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
										role.cv)))
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
									return {eo: role, aD: 24 + (column * 300), ag: 40 + (row * 132)};
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
						return pos.ag + 130;
					},
					positions)));
		var lookup = $elm$core$Dict$fromList(
			A2(
				$elm$core$List$map,
				function (pos) {
					return _Utils_Tuple2(pos.eo.du, pos);
				},
				positions));
		var edge = F2(
			function (pos, target) {
				var _v0 = A2($elm$core$Dict$get, target, lookup);
				if (!_v0.$) {
					var to = _v0.a;
					return A3(
						$author$project$Ui$AgentGraph$svg,
						'line',
						_List_fromArray(
							[
								A2(
								$elm$html$Html$Attributes$attribute,
								'x1',
								$elm$core$String$fromFloat(pos.aD + 240)),
								A2(
								$elm$html$Html$Attributes$attribute,
								'y1',
								$elm$core$String$fromFloat(pos.ag + 50)),
								A2(
								$elm$html$Html$Attributes$attribute,
								'x2',
								$elm$core$String$fromFloat(to.aD)),
								A2(
								$elm$html$Html$Attributes$attribute,
								'y2',
								$elm$core$String$fromFloat(to.ag + 50)),
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
					pos.eo.dq);
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
														$elm$html$Html$text(role.dO)
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
																		return $.eo;
																	},
																	function ($) {
																		return $.dO;
																	}),
																A2($elm$core$Dict$get, target, lookup))))
													]))
											]));
								},
								role.dq);
						},
						roles))
				]));
	});
var $author$project$Page$Agents$graph = F3(
	function (state, org, workspace) {
		var _v0 = A2($author$project$App$Agents$saved, org, state);
		if (_v0.$ === 1) {
			return A2(
				$author$project$Ui$Common$panel,
				'설계 불러오기',
				_List_fromArray(
					[
						$author$project$Ui$Common$note(
						A2(
							$elm$core$Maybe$withDefault,
							'에이전트 설계를 불러오고 있습니다…',
							A2($elm$core$Dict$get, org, state.I)))
					]));
		} else {
			var snapshot = _v0.a;
			var _v1 = $elm$core$List$isEmpty(snapshot.N) ? _Utils_Tuple3(snapshot.q, snapshot.c7, '규칙 기반 초안 (저장된 설계 없음)') : _Utils_Tuple3(snapshot.N, snapshot.c3, '저장된 설계');
			var roles = _v1.a;
			var items = _v1.b;
			var source = _v1.c;
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$author$project$Ui$Common$panel,
						'에이전트 인계 구조 · ' + source,
						_List_fromArray(
							[
								$author$project$Ui$Common$note('역할 사이의 인계와 사람 승인 지점을 확인합니다. 조직 운영의 책임 그래프와 달리 이 구조는 설계 기록이며 실제 실행 경로가 아닙니다.'),
								A3($author$project$Ui$AgentGraph$view, workspace, roles, items)
							])),
						A3($author$project$Page$Agents$diagnostics, '구조 진단', items, '확인할 사항이 없습니다.')
					]));
		}
	});
var $author$project$Page$Discovery$guide = F4(
	function (expanded, toggle, go, doc) {
		var workflowsDone = !$elm$core$List$isEmpty(doc.eX);
		var scopeDone = ($elm$core$String$trim(doc.eq) !== '') && (!$elm$core$List$isEmpty(doc.dZ));
		var reviewed = workflowsDone && (doc.b1.ez === 'reviewed');
		var next = (!scopeDone) ? 1 : ((!workflowsDone) ? 2 : 3);
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
										_Utils_Tuple3(scopeDone, '1. 조직 범위와 사실·미확인 기록', 1),
										_Utils_Tuple3(workflowsDone, '2. 업무의 입력·산출물·인계 연결', 2),
										_Utils_Tuple3(reviewed, '3. 규칙 기반 제안을 사람이 검토', 3)
									])))
						])) : $elm$html$Html$text('')
				]));
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
				event.cp));
	});
var $elm$html$Html$article = _VirtualDom_node('article');
var $author$project$Ui$Activity$category = function (event) {
	var _v0 = event.X.a9;
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
			var _v1 = event.X.ch;
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
						return $.a_;
					},
					function ($) {
						return $.c1;
					}),
				$elm$core$List$head(
					A2(
						$elm$core$List$filter,
						A2(
							$elm$core$Basics$composeR,
							function ($) {
								return $.a_;
							},
							A2(
								$elm$core$Basics$composeR,
								function ($) {
									return $.du;
								},
								$elm$core$Basics$eq(key))),
						w.$7))));
	});
var $author$project$Ui$Activity$targetName = F2(
	function (w, event) {
		var _v0 = event.X.ch;
		switch (_v0) {
			case 'person':
				return A2($author$project$Ui$Label$personName, w, event.X.cg);
			case 'goal':
				return A2($author$project$Ui$Label$goalName, w, event.X.cg);
			case 'organization':
				return _Utils_eq(event.X.cg, w.bQ.du) ? w.bQ.dO : event.X.cg;
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
			return target + (' · ' + (label + ((event.X.o === '') ? '' : (' · ' + event.X.o))));
		};
		var person = A2(
			$elm$core$Maybe$withDefault,
			'',
			A2(
				$elm$core$Maybe$map,
				$author$project$Ui$Label$personName(w),
				event.X.aP));
		var _v0 = event.X.a9;
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
				return event.c1;
		}
	});
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
						'기록 상세 #' + $elm$core$String$fromInt(event.eu))
					])),
				A2(
				$elm$html$Html$p,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('원본 시각: ' + event.cA)
					])),
				A2(
				$elm$html$Html$p,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(
						'기록 주체 ID: ' + A2($elm$core$Maybe$withDefault, '없음', event.cp))
					])),
				A2(
				$elm$html$Html$p,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('대상 ID: ' + event.X.cg)
					])),
				A2(
				$elm$html$Html$p,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('원본 설명: ' + event.c1)
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
								$elm$html$Html$text(event.X.bY)
							]))
					]))
			]));
};
var $elm$core$String$endsWith = _String_endsWith;
var $author$project$Ui$Activity$timestamp = function (value) {
	return (A2($elm$core$String$contains, 'T', value) && A2($elm$core$String$endsWith, 'Z', value)) ? (A2($elm$core$String$left, 10, value) + (' ' + (A3($elm$core$String$slice, 11, 19, value) + ' UTC'))) : value;
};
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
							$author$project$Ui$Activity$timestamp(event.cA))
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
var $elm$core$Basics$ge = _Utils_ge;
var $elm$core$List$sortBy = _List_sortBy;
var $elm$core$String$toLower = _String_toLower;
var $author$project$Ui$Activity$filtered = F2(
	function (state, w) {
		return A2(
			$elm$core$List$sortBy,
			A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.eu;
				},
				$elm$core$Basics$negate),
			A2(
				$elm$core$List$filter,
				function (event) {
					return ((state.dB === '') || _Utils_eq(
						$author$project$Ui$Activity$category(event),
						state.dB)) && (((state.dl === '') || (_Utils_cmp(
						A2($elm$core$String$left, 10, event.cA),
						state.dl) > -1)) && (((state.eU === '') || (_Utils_cmp(
						A2($elm$core$String$left, 10, event.cA),
						state.eU) < 1)) && ((_Utils_eq(state.b1, $elm$core$Maybe$Nothing) || _Utils_eq(event.X.b2, state.b1)) && A2(
						$elm$core$String$contains,
						$elm$core$String$toLower(
							$elm$core$String$trim(state.d9)),
						$elm$core$String$toLower(
							A2(
								$elm$core$String$join,
								' ',
								_List_fromArray(
									[
										A2($author$project$Ui$Activity$targetName, w, event),
										A2($author$project$Ui$Activity$actorName, w, event),
										A2($author$project$Ui$Activity$description, w, event),
										event.X.cg,
										A2($elm$core$Maybe$withDefault, '', event.X.aP),
										A2($elm$core$Maybe$withDefault, '', event.X.b2),
										A2($elm$core$Maybe$withDefault, '', event.cp),
										$author$project$Ui$Activity$category(event)
									])))))));
				},
				w.dd));
	});
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
							$author$project$Ui$Activity$timestamp(event.cA))
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
							state.d9,
							function (v) {
								return change(
									_Utils_update(
										state,
										{d9: v}));
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
											$elm$html$Html$Attributes$value(state.dB),
											$elm$html$Html$Events$onInput(
											function (v) {
												return change(
													_Utils_update(
														state,
														{dB: v}));
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
							state.dl,
							function (v) {
								return change(
									_Utils_update(
										state,
										{dl: v}));
							}),
							A4(
							field,
							'종료일 (UTC)',
							'date',
							state.eU,
							function (v) {
								return change(
									_Utils_update(
										state,
										{eU: v}));
							})
						])),
					((state.dl !== '') && ((state.eU !== '') && (_Utils_cmp(state.dl, state.eU) > 0))) ? A2(
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
											state.b1)))))
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
					$elm$core$List$isEmpty(events) ? $author$project$Ui$Common$note('조건에 맞는 활동 기록이 없습니다.') : ((mode === 1) ? A3(
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
var $author$project$App$Agents$changed = F2(
	function (org, state) {
		return !_Utils_eq(
			A2(
				$elm$core$Maybe$map,
				function ($) {
					return $.N;
				},
				A2($author$project$App$Agents$current, org, state)),
			A2(
				$elm$core$Maybe$map,
				function ($) {
					return $.N;
				},
				A2($author$project$App$Agents$saved, org, state)));
	});
var $author$project$Domain$Agent$Evidence = F2(
	function (a, b) {
		return {$: 9, a: a, b: b};
	});
var $author$project$Domain$Agent$Handoff = F3(
	function (a, b, c) {
		return {$: 7, a: a, b: b, c: c};
	});
var $author$project$Domain$Agent$Inputs = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $author$project$Domain$Agent$Level = F2(
	function (a, b) {
		return {$: 5, a: a, b: b};
	});
var $author$project$Domain$Agent$Name = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $author$project$Domain$Agent$Outputs = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $author$project$Domain$Agent$Remove = function (a) {
	return {$: 10, a: a};
};
var $author$project$Domain$Agent$SetApproval = F2(
	function (a, b) {
		return {$: 6, a: a, b: b};
	});
var $author$project$Domain$Agent$Status = F2(
	function (a, b) {
		return {$: 8, a: a, b: b};
	});
var $author$project$Domain$Agent$Tools = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $author$project$Domain$Agent$approvalKey = function (approval) {
	if (!approval.$) {
		if (!approval.a.$) {
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
var $author$project$Page$Agents$known = function (value_) {
	return ($elm$core$String$trim(value_) === '') ? '미확인 · 확인 후 입력' : value_;
};
var $author$project$Domain$Agent$toolsText = $elm$core$String$join(', ');
var $author$project$Page$Agents$designCard = F4(
	function (controls, workspace, roles, role) {
		var others = A2(
			$elm$core$List$filter,
			function (r) {
				return !_Utils_eq(r.du, role.du);
			},
			roles);
		var key = function (suffix) {
			return 'agent-' + (role.du + ('-' + suffix));
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
					$elm$html$Html$Attributes$id('design-' + role.du)
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$h2,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							$author$project$Page$Agents$known(role.dO))
						])),
					$author$project$Ui$Common$note(
					'도출 근거 업무: ' + (A2($elm$core$Maybe$withDefault, '없음', role.ex) + (' · 담당 업무: ' + role.eJ))),
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
							role.dO,
							A2(
								$elm$core$Basics$composeL,
								controls.am,
								$author$project$Domain$Agent$Name(role.du))),
							A6(
							$author$project$Ui$Form$selectValue,
							key('level'),
							role.dG,
							A2(
								$elm$core$Basics$composeL,
								controls.am,
								$author$project$Domain$Agent$Level(role.du)),
							'권한 등급',
							true,
							$author$project$Domain$Agent$levels),
							A6(
							$author$project$Ui$Form$guidedArea,
							key('inputs'),
							'입력',
							'이 역할이 받는 정보',
							false,
							role.dz,
							A2(
								$elm$core$Basics$composeL,
								controls.am,
								$author$project$Domain$Agent$Inputs(role.du))),
							A6(
							$author$project$Ui$Form$guidedArea,
							key('outputs'),
							'산출물',
							'이 역할이 만드는 결과물',
							false,
							role.d2,
							A2(
								$elm$core$Basics$composeL,
								controls.am,
								$author$project$Domain$Agent$Outputs(role.du))),
							A6(
							$author$project$Ui$Form$guidedArea,
							key('tools'),
							'허용 도구 후보',
							'쉼표로 구분합니다. 실제 접근 권한은 별도로 부여합니다.',
							false,
							$author$project$Domain$Agent$toolsText(role.eP),
							A2(
								$elm$core$Basics$composeL,
								controls.am,
								$author$project$Domain$Agent$Tools(role.du))),
							A6(
							$author$project$Ui$Form$selectValue,
							key('approval'),
							$author$project$Domain$Agent$approvalKey(role.cv),
							A2(
								$elm$core$Basics$composeL,
								controls.am,
								$author$project$Domain$Agent$SetApproval(role.du)),
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
														A2($elm$core$List$member, other.du, role.dq)),
														$elm$html$Html$Events$onCheck(
														A2(
															$elm$core$Basics$composeL,
															controls.am,
															A2($author$project$Domain$Agent$Handoff, role.du, other.du)))
													]),
												_List_Nil),
												$elm$html$Html$text(
												$author$project$Page$Agents$known(other.dO))
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
							role.ez,
							A2(
								$elm$core$Basics$composeL,
								controls.am,
								$author$project$Domain$Agent$Status(role.du)),
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
							role.ez === 'confirmed',
							role.de,
							A2(
								$elm$core$Basics$composeL,
								controls.am,
								$author$project$Domain$Agent$Evidence(role.du)))
						])),
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$type_('button'),
							$elm$html$Html$Attributes$class('secondary'),
							$elm$html$Html$Events$onClick(
							controls.am(
								$author$project$Domain$Agent$Remove(role.du)))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('이 역할 제외 · 저장 전 취소 가능')
						]))
				]));
	});
var $author$project$Page$Agents$approvalText = F2(
	function (workspace, approval) {
		if (!approval.$) {
			if (!approval.a.$) {
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
								return $.dO;
							},
							$elm$core$List$head(
								A2(
									$elm$core$List$filter,
									A2(
										$elm$core$Basics$composeR,
										function ($) {
											return $.du;
										},
										$elm$core$Basics$eq(t)),
									roles))));
				},
				targets));
	});
var $author$project$Page$Agents$draftCard = F3(
	function (workspace, roles, role) {
		return A2(
			$elm$html$Html$section,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('panel agent-card'),
					$elm$html$Html$Attributes$id('draft-' + role.du)
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
							$elm$html$Html$text(role.dO + ' 에이전트 후보')
						])),
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('제안 이유: ‘' + (role.eJ + '’의 입력을 받아 산출물을 만드는 역할 경계가 필요하기 때문입니다.'))
						])),
					$author$project$Ui$Common$note(
					'정보 구분: ' + ($author$project$Domain$Discovery$statusLabel(role.ez) + (' · 근거: ' + $author$project$Page$Agents$known(role.de)))),
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
								_Utils_Tuple2('담당 업무', role.eJ),
								_Utils_Tuple2(
								'입력',
								$author$project$Page$Agents$known(role.dz)),
								_Utils_Tuple2(
								'산출물',
								$author$project$Page$Agents$known(role.d2)),
								_Utils_Tuple2(
								'도구 후보',
								$author$project$Page$Agents$known(
									$author$project$Domain$Agent$toolsText(role.eP))),
								_Utils_Tuple2(
								'권한 등급',
								$author$project$Domain$Agent$levelLabel(role.dG)),
								_Utils_Tuple2(
								'사람 승인',
								A2($author$project$Page$Agents$approvalText, workspace, role.cv)),
								_Utils_Tuple2(
								'인계 대상',
								A2($author$project$Page$Agents$handoffText, roles, role.dq))
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
					$elm$core$List$isEmpty(snapshot.q) ? _List_fromArray(
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
								A2($author$project$Page$Agents$draftCard, workspace, snapshot.q),
								snapshot.q))
						]),
					_List_fromArray(
						[
							A3($author$project$Page$Agents$diagnostics, '초안 진단', snapshot.c7, '초안에서 확인할 사항이 없습니다.')
						]))));
	});
var $elm$html$Html$Attributes$rel = _VirtualDom_attribute('rel');
var $elm$html$Html$Attributes$target = $elm$html$Html$Attributes$stringProperty('target');
var $author$project$Page$Agents$view = F2(
	function (controls, workspace) {
		var _v0 = A2($author$project$App$Agents$saved, controls.aL, controls.cb);
		if (_v0.$ === 1) {
			return A2(
				$author$project$Ui$Common$panel,
				'설계 불러오기',
				_List_fromArray(
					[
						$author$project$Ui$Common$note(
						A2(
							$elm$core$Maybe$withDefault,
							'에이전트 설계를 불러오고 있습니다…',
							A2($elm$core$Dict$get, controls.aL, controls.cb.I)))
					]));
		} else {
			var snapshot = _v0.a;
			var unsaved = A2($author$project$App$Agents$changed, controls.aL, controls.cb);
			var unavailable = controls.cb.aJ || A2($elm$core$Dict$member, controls.aL, controls.cb.I);
			var design = A2(
				$elm$core$Maybe$withDefault,
				snapshot.N,
				A2(
					$elm$core$Maybe$map,
					function ($) {
						return $.N;
					},
					A2($author$project$App$Agents$current, controls.aL, controls.cb)));
			var conflict = A2($author$project$App$Agents$conflicted, controls.aL, controls.cb);
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
										$elm$html$Html$Attributes$disabled(controls.aE || unavailable),
										$elm$html$Html$Events$onClick(controls.bZ)
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
								$elm$html$Html$Events$onSubmit(controls.b4)
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$fieldset,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$disabled(controls.aE || unavailable)
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
																	$elm$core$List$isEmpty(snapshot.q)),
																	$elm$html$Html$Events$onClick(controls.dv)
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
																	$elm$html$Html$Events$onClick(controls.a5)
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
										A3($author$project$Page$Agents$diagnostics, '저장된 설계 진단', snapshot.c3, '저장된 설계에서 확인할 사항이 없습니다. 저장 전 입력은 저장 후 진단합니다.'),
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
														controls.aE ? '저장 중…' : '설계안 저장')
													])),
												A2(
												$elm$html$Html$a,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('button-link secondary'),
														$elm$html$Html$Attributes$href(controls.dh),
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
														controls.aZ(4))
													]),
												_List_fromArray(
													[
														$elm$html$Html$text('에이전트 구조 보기 →')
													]))
											])),
										$author$project$Ui$Common$note('내보내기는 저장된 설계를 사용하며, 저장된 설계가 없으면 규칙 기반 초안을 내보냅니다. 파일은 .claude/agents/<id>.md 형식의 정의 초안이며 실행 설정이 아닙니다.')
									]))
							])),
						controls.b1
					]));
		}
	});
var $author$project$Form$Action$Rename = {$: 2};
var $author$project$Page$Settings$view = F2(
	function (model, w) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$author$project$Ui$Common$panel,
					w.bQ.dO,
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
											$elm$html$Html$text(w.bQ.du)
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
											A2($elm$core$String$left, 10, w.bQ.cV))
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
												$elm$core$List$length(w.d4)) + '명')
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
												$elm$core$List$length(w.$7)) + '개')
										]))
								])),
							A2(
							$elm$html$Html$button,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$disabled(model.w.aE),
									$elm$html$Html$Events$onClick(model.$7)
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
							model.w,
							$author$project$Form$Action$Rename,
							'이름 저장',
							_List_fromArray(
								[
									A6($author$project$Ui$Form$inputField, model.w, $author$project$Form$Action$Rename, '조직 이름', 'name', 'text', true),
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
									$elm$html$Html$text('조직 진단 기록, 업무 흐름, 에이전트 검토, 구성원, 목표, 책임, 권한, 결과, 평가, 회고와 전략이 현재 워크스페이스에서 제거됩니다.')
								])),
							$author$project$Ui$Common$note('논리 삭제입니다. 원본 감사 이벤트는 파일·DB에 보존되며 완전히 지워지지 않습니다. 다른 조직은 삭제되지 않습니다.'),
							function () {
							var _v0 = model.bo;
							if (_v0.$ === 1) {
								return A2(
									$elm$html$Html$button,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('danger-outline'),
											$elm$html$Html$Attributes$disabled(model.w.aE || (!model.w.ao)),
											$elm$html$Html$Events$onClick(model.d0)
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
											model.w.eE($author$project$Form$Action$DeleteOrg)),
											A2(
											$elm$html$Html$Events$preventDefaultOn,
											'keydown',
											A2(
												$elm$json$Json$Decode$map,
												function (key) {
													return (key === 'Escape') ? _Utils_Tuple2(model.cP, true) : _Utils_Tuple2(model.dQ, false);
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
													$elm$html$Html$text(snapshot.dO + ' 조직을 삭제할까요?')
												])),
											A2(
											$elm$html$Html$fieldset,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$disabled(model.w.aE)
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
																	$elm$html$Html$Attributes$value(snapshot.bi),
																	$elm$html$Html$Events$onInput(model.cS),
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
																	$elm$html$Html$Events$onClick(model.cP)
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
																	(!_Utils_eq(snapshot.bi, snapshot.dO)) || (!model.w.ao))
																]),
															_List_fromArray(
																[
																	$elm$html$Html$text(
																	model.w.aE ? '삭제 중…' : '조직 삭제')
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
		return {al: done, bE: instruction, ac: page, eI: target, ci: title};
	});
var $author$project$Ui$Guide$view = F2(
	function (model, w) {
		var reviewed = A2(
			$elm$core$List$any,
			function (r) {
				return (r.a_ === 'demo-revenue') && ((r.bu.ez === 4) && ((!$elm$core$List$isEmpty(r.dD)) && A2(
					$elm$core$List$any,
					function (d) {
						return (d.a3 !== '') && (!_Utils_eq(d.cW, $elm$core$Maybe$Nothing));
					},
					r.cZ)));
			},
			w.el);
		var goal = function (key) {
			return $elm$core$List$head(
				A2(
					$elm$core$List$filter,
					A2(
						$elm$core$Basics$composeR,
						function ($) {
							return $.a_;
						},
						A2(
							$elm$core$Basics$composeR,
							function ($) {
								return $.du;
							},
							$elm$core$Basics$eq('demo-' + key))),
					w.$7));
		};
		var ready = A2(
			$elm$core$Maybe$withDefault,
			false,
			A2(
				$elm$core$Maybe$map,
				A2(
					$elm$core$Basics$composeR,
					function ($) {
						return $.cu;
					},
					A2(
						$elm$core$Basics$composeR,
						function ($) {
							return $.cU;
						},
						$elm$core$Basics$eq(1))),
				goal('launch')));
		var evaluated = A2(
			$elm$core$List$any,
			function (e) {
				return _Utils_eq(
					e.db,
					$elm$core$Maybe$Just('demo-revenue')) && _Utils_eq(
					e.dc,
					$elm$core$Maybe$Just(4));
			},
			w.dd);
		var designed = A2(
			$elm$core$List$any,
			function (e) {
				return e.X.a9 === 'AgentRolesSaved';
			},
			w.dd);
		var assigned = !_Utils_eq(
			$elm$core$Maybe$Nothing,
			A2(
				$elm$core$Maybe$andThen,
				function ($) {
					return $.a3;
				},
				goal('partners')));
		var active = function (key) {
			return A2(
				$elm$core$Maybe$withDefault,
				false,
				A2(
					$elm$core$Maybe$map,
					function ($) {
						return $.bc;
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
						return $.bu;
					},
					A2(
						$elm$core$Basics$composeR,
						function ($) {
							return $.ez;
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
				assigned ? 6 : 7,
				assigned ? 'goal-demo-partners' : 'owner-demo-partners'),
				A5(
				$author$project$Ui$Guide$GuideStep,
				active('launch'),
				'02 · 책임에 맞는 권한 주기',
				'이지원에게 채용 권한과 예산 30,000,000원을 부여하세요. 제품 출시 권한을 유지하고 신제품 출시 목표를 활성화하세요.',
				ready ? 6 : 8,
				ready ? 'goal-demo-launch' : 'authority-demo-product'),
				A5($author$project$Ui$Guide$GuideStep, achieved && evaluated, '03 · 결과에서 평가까지', '매출 실측값 50 (단위: 억원)과 보고자, 설명을 보고한 뒤 평가 기록을 누르세요.', 9, 'goal-demo-revenue'),
				A5($author$project$Ui$Guide$GuideStep, reviewed, '04 · 배움을 다음 결정으로', '매출 목표의 학습과 다음 결정, 담당자, 미래 기한을 기록하세요. 달성 결과와 평가가 함께 보존됩니다.', 10, 'review-form'),
				A5($author$project$Ui$Guide$GuideStep, designed, '05 · 업무에서 에이전트 설계로', '저장된 업무 흐름 4건에서 도출한 역할 후보를 설계안으로 가져오고, 등급과 승인 주체, 인계 대상을 검토해 저장하세요. 저장하면 구조 화면과 내보내기를 사용할 수 있습니다.', 3, 'agent-import')
			]);
		var count = $elm$core$List$length(
			A2(
				$elm$core$List$filter,
				function ($) {
					return $.al;
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
											$elm$html$Html$text('조직의 운영 흐름, 다섯 단계로 체험하세요')
										])),
									A2(
									$elm$html$Html$p,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text('6명 · 7개 목표 · 5가지 성과 상태 · 업무 흐름 4건. 실제 저장 상태로 진행률을 계산합니다.')
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
									$elm$core$String$fromInt(count) + ' / 5 완료')
								]))
						])),
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('guide-toggle secondary'),
							$elm$html$Html$Events$onClick(model.eO),
							A2(
							$elm$html$Html$Attributes$attribute,
							'aria-expanded',
							model.aI ? 'true' : 'false')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							model.aI ? '체험 가이드 접기' : '체험 가이드 열기')
						])),
					model.aI ? A2(
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
														_Utils_Tuple2('complete', step_.al)
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
														step_.al ? '✓ 완료' : '○ 체험 대기')
													])),
												A2(
												$elm$html$Html$h3,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text(step_.ci)
													])),
												A2(
												$elm$html$Html$p,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text(step_.bE)
													])),
												A2(
												$elm$html$Html$button,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('secondary'),
														$elm$html$Html$Attributes$disabled(model.aE),
														$elm$html$Html$Events$onClick(
														A2(model.aZ, step_.ac, step_.eI))
													]),
												_List_fromArray(
													[
														$elm$html$Html$text(
														step_.al ? '다시 살펴보기 →' : '이 단계 진행 →')
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
											$elm$html$Html$Attributes$disabled(model.aE),
											$elm$html$Html$Events$onClick(
											A2(model.aZ, 5, 'new-person'))
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('구성원 관리 →')
										])),
									A2(
									$elm$html$Html$button,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('secondary'),
											$elm$html$Html$Attributes$disabled(model.aE),
											$elm$html$Html$Events$onClick(
											A2(model.aZ, 7, 'responsibility-graph'))
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
											$elm$html$Html$Attributes$disabled(model.aE),
											$elm$html$Html$Events$onClick(
											A2(model.aZ, 11, 'audit-history'))
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('활동 기록 →')
										]))
								]))
						])) : $elm$html$Html$text('')
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
								$elm$core$String$fromInt(w.cR.I) + (' 오류 · ' + ($elm$core$String$fromInt(w.cR.cl) + ' 경고')))
							]))
					])),
				$elm$core$List$isEmpty(w.cR.c3) ? $author$project$Ui$Common$note('현재 입력에 적용한 규칙에서 추가 확인 사항이 발견되지 않았습니다. 미입력 정보나 실제 업무까지 검증한 것은 아닙니다.') : A2(
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
											_Utils_Tuple2('error', d.ew === 'Error')
										]))
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$code,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(d.cQ)
										])),
									A2(
									$elm$html$Html$strong,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(d.dH)
										])),
									A2(
									$elm$html$Html$p,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(d.eD)
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
										d.c2))
								]));
					},
					w.cR.c3)),
				$author$project$Ui$Common$note('권한 집중도는 권한 종류와 예산 보유를 각각 1점으로 세는 규칙 기반 추정치입니다.')
			]));
};
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
						_Utils_Tuple2('draft', !g.bc),
						_Utils_Tuple2('error', g.bu.ez === 3),
						_Utils_Tuple2('warn', g.bu.ez === 2)
					]))
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(
				g.bc ? $author$project$Ui$Label$statusName(g.bu.ez) : '초안')
			]));
};
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
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
var $author$project$Form$Action$Assign = function (a) {
	return {$: 7, a: a};
};
var $author$project$Ui$Form$selectField = F6(
	function (model, action, label_, key, required_, options) {
		return A7(
			$author$project$Ui$Form$selectWithHelp,
			$author$project$Form$Action$actionKey(action) + ('-' + key),
			key,
			A2(model.ba, action, key),
			A2(model.am, action, key),
			label_,
			required_,
			options);
	});
var $author$project$Page$Responsibility$ownerForm = F3(
	function (model, w, g) {
		return A4(
			$author$project$Ui$Form$formView,
			model.w,
			$author$project$Form$Action$Assign(g.a_.du),
			'책임자 지정',
			_List_fromArray(
				[
					A6(
					$author$project$Ui$Form$selectField,
					model.w,
					$author$project$Form$Action$Assign(g.a_.du),
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
						A2($elm$core$List$map, $author$project$Ui$Label$permissionName, g.a_.eh)))
				])),
			$author$project$Ui$Common$note(
			'예산 ' + ($author$project$Ui$Label$formatNumber(g.a_.eg) + ('원 · ' + ($elm$core$String$fromInt(
				$elm$core$Basics$round(g.cu.cU * 100)) + '% 통제'))))
		]);
};
var $author$project$Page$Responsibility$responsibilityCard = F3(
	function (model, w, g) {
		return A2(
			$elm$html$Html$article,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('goal-card'),
					$elm$html$Html$Attributes$id('owner-' + g.a_.du),
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
								$elm$html$Html$text(g.a_.c1)
							])),
						A2(
						$elm$html$Html$small,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(g.a_.dJ.dO)
							])),
						A2(
						$elm$html$Html$p,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(
								'목표값 ' + ($author$project$Ui$Label$formatNumber(g.a_.eI) + (' ' + g.a_.dJ.eT)))
							])),
						$author$project$Ui$Common$note(
						'최종 책임자: ' + A2(
							$elm$core$Maybe$withDefault,
							'책임자 미지정',
							A2(
								$elm$core$Maybe$map,
								$author$project$Ui$Label$personName(w),
								g.a3))),
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
					$elm$html$Html$Attributes$id('owner-' + g.a_.du),
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
									$elm$html$Html$text(g.a_.c1)
								])),
							A2(
							$elm$html$Html$small,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(g.a_.dJ.dO)
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
									g.a3))),
							A3($author$project$Page$Responsibility$ownerForm, model, w, g)
						])),
					A2(
					$elm$html$Html$td,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							$author$project$Ui$Label$formatNumber(g.a_.eI) + (' ' + g.a_.dJ.eT))
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
		if (ma.$ === 1) {
			return $elm$core$Maybe$Nothing;
		} else {
			var a = ma.a;
			if (mb.$ === 1) {
				return $elm$core$Maybe$Nothing;
			} else {
				var b = mb.a;
				return $elm$core$Maybe$Just(
					A2(func, a, b));
			}
		}
	});
var $author$project$Ui$ResponsibilityGraph$nodeKey = function (node) {
	return node.a9 + (':' + node.cT);
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
							return $.k;
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
						$author$project$Ui$ResponsibilityGraph$nodeKey(edge.dl),
						key) || _Utils_eq(
						$author$project$Ui$ResponsibilityGraph$nodeKey(edge.eM),
						key);
				},
				state.es));
		return A3(
			$elm$core$Maybe$map2,
			F2(
				function (from, to) {
					var y2 = to.ag + 42;
					var y1 = from.ag + 42;
					var x2 = to.aD;
					var x1 = from.aD + 260;
					var d = (edge.dB === 'DependsOn') ? ('M ' + (A2($author$project$Ui$ResponsibilityGraph$pair, x1, y1) + (' C ' + (A2($author$project$Ui$ResponsibilityGraph$pair, x1 + 55, y1) + (' ' + (A2($author$project$Ui$ResponsibilityGraph$pair, x1 + 55, y2) + (' ' + A2($author$project$Ui$ResponsibilityGraph$pair, x1 + 3, y2)))))))) : ((edge.dB === 'Controls') ? ('M ' + (A2($author$project$Ui$ResponsibilityGraph$pair, x1, y1 + 25) + (' C ' + (A2($author$project$Ui$ResponsibilityGraph$pair, x1 + 45, y1 + 65) + (' ' + (A2($author$project$Ui$ResponsibilityGraph$pair, x2 - 45, y2 + 65) + (' ' + A2($author$project$Ui$ResponsibilityGraph$pair, x2, y2 + 25)))))))) : ('M ' + (A2($author$project$Ui$ResponsibilityGraph$pair, x1, y1) + (' C ' + (A2($author$project$Ui$ResponsibilityGraph$pair, x1 + 40, y1) + (' ' + (A2($author$project$Ui$ResponsibilityGraph$pair, x2 - 40, y2) + (' ' + A2($author$project$Ui$ResponsibilityGraph$pair, x2, y2)))))))));
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
										$author$project$Ui$ResponsibilityGraph$relationLabel(edge.dB))
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
										((!_Utils_eq(state.es, $elm$core$Maybe$Nothing)) && active) ? '3' : '1.5'),
										A2(
										$elm$html$Html$Attributes$attribute,
										'stroke-dasharray',
										((edge.dB === 'DependsOn') || (edge.dB === 'Controls')) ? '6 4' : 'none'),
										A2($elm$html$Html$Attributes$attribute, 'marker-end', 'url(#responsibility-arrow)')
									]),
								_List_Nil)
							]));
				}),
			find(edge.dl),
			find(edge.eM));
	});
var $author$project$Ui$ResponsibilityGraph$Select = function (a) {
	return {$: 4, a: a};
};
var $author$project$Ui$ResponsibilityGraph$bool = function (value) {
	return value ? 'true' : 'false';
};
var $author$project$Ui$ResponsibilityGraph$goalWarning = F2(
	function (w, node) {
		return (node.a9 !== 'GoalNode') ? '' : A2(
			$elm$core$Maybe$withDefault,
			'',
			A2(
				$elm$core$Maybe$map,
				function (g) {
					return _Utils_eq(g.a3, $elm$core$Maybe$Nothing) ? '책임자 미지정' : ((g.cu.cU < 1) ? '권한 부족' : '');
				},
				$elm$core$List$head(
					A2(
						$elm$core$List$filter,
						A2(
							$elm$core$Basics$composeR,
							function ($) {
								return $.a_;
							},
							A2(
								$elm$core$Basics$composeR,
								function ($) {
									return $.du;
								},
								$elm$core$Basics$eq(node.cT))),
						w.$7))));
	});
var $author$project$Ui$ResponsibilityGraph$nodeLabel = F2(
	function (w, node) {
		var _v0 = node.a9;
		switch (_v0) {
			case 'PersonNode':
				return A2($author$project$Ui$Label$personName, w, node.cT);
			case 'GoalNode':
				return A2($author$project$Ui$Label$goalName, w, node.cT);
			case 'MetricNode':
				return A2(
					$elm$core$Maybe$withDefault,
					node.cT,
					A2(
						$elm$core$Maybe$map,
						A2(
							$elm$core$Basics$composeR,
							function ($) {
								return $.a_;
							},
							A2(
								$elm$core$Basics$composeR,
								function ($) {
									return $.dJ;
								},
								function ($) {
									return $.dO;
								})),
						$elm$core$List$head(
							A2(
								$elm$core$List$filter,
								A2(
									$elm$core$Basics$composeR,
									function ($) {
										return $.a_;
									},
									A2(
										$elm$core$Basics$composeR,
										function ($) {
											return $.dJ;
										},
										A2(
											$elm$core$Basics$composeR,
											function ($) {
												return $.du;
											},
											$elm$core$Basics$eq(node.cT)))),
								w.$7))));
			case 'ResourceNode':
				return (node.cT === 'Budget') ? '예산' : $author$project$Ui$Label$permissionName(node.cT);
			default:
				return node.cT;
		}
	});
var $author$project$Ui$ResponsibilityGraph$matches = F3(
	function (query, w, node) {
		return A2(
			$elm$core$String$contains,
			$elm$core$String$toLower(
				$elm$core$String$trim(query)),
			$elm$core$String$toLower(
				A2($author$project$Ui$ResponsibilityGraph$nodeLabel, w, node) + (' ' + node.cT)));
	});
var $author$project$Ui$ResponsibilityGraph$nodeType = function (node) {
	var _v0 = node.a9;
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
					$author$project$Ui$ResponsibilityGraph$nodeKey(edge.dl),
					selected) && _Utils_eq(
					$author$project$Ui$ResponsibilityGraph$nodeKey(edge.eM),
					key)) || (_Utils_eq(
					$author$project$Ui$ResponsibilityGraph$nodeKey(edge.eM),
					selected) && _Utils_eq(
					$author$project$Ui$ResponsibilityGraph$nodeKey(edge.dl),
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
		var warning = A2($author$project$Ui$ResponsibilityGraph$goalWarning, w, pos.k);
		var title = A2($author$project$Ui$ResponsibilityGraph$nodeLabel, w, pos.k);
		var key = $author$project$Ui$ResponsibilityGraph$nodeKey(pos.k);
		var selected = _Utils_eq(
			state.es,
			$elm$core$Maybe$Just(key));
		var illuminated = A2(
			$elm$core$Maybe$withDefault,
			true,
			A2(
				$elm$core$Maybe$map,
				function (chosen) {
					return A3($author$project$Ui$ResponsibilityGraph$related, edges, chosen, key);
				},
				state.es));
		var found = A3($author$project$Ui$ResponsibilityGraph$matches, state.d9, w, pos.k);
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
		var color = (warning !== '') ? '#fff2dc' : ((pos.k.a9 === 'PersonNode') ? '#eaf3ec' : ((pos.k.a9 === 'MetricNode') ? '#eaf1fa' : ((pos.k.a9 === 'ResourceNode') ? '#f1edf8' : '#fff')));
		return A3(
			$author$project$Ui$ResponsibilityGraph$svg,
			'g',
			_Utils_ap(
				_List_fromArray(
					[
						A2(
						$elm$html$Html$Attributes$attribute,
						'transform',
						'translate(' + (A2($author$project$Ui$ResponsibilityGraph$pair, pos.aD, pos.ag) + ')')),
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
							selected ? '#1c6147' : ((found && ($elm$core$String$trim(state.d9) !== '')) ? '#3479b3' : '#b8cbbd')),
							A2(
							$elm$html$Html$Attributes$attribute,
							'stroke-width',
							(selected || (($elm$core$String$trim(state.d9) !== '') && found)) ? '3' : '1.5')
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
							(warning !== '') ? ('⚠ ' + warning) : $author$project$Ui$ResponsibilityGraph$nodeType(pos.k))
						]))
				]));
	});
var $author$project$Ui$ResponsibilityGraph$diagram = F5(
	function (state, dispatch, w, positions, edges) {
		var width = state.L ? 1360 : 1020;
		var height = A2(
			$elm$core$Maybe$withDefault,
			180,
			$elm$core$List$maximum(
				A2(
					$elm$core$List$map,
					function (pos) {
						return pos.ag + 110;
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
									$elm$core$String$fromFloat(state.F * 100) + '%'),
									A2(
									$elm$html$Html$Attributes$attribute,
									'class',
									(state.F === 1) ? 'graph-svg graph-fit' : 'graph-svg'),
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
										state.L ? _List_fromArray(
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
							return A2($author$project$Domain$Node, 'PersonNode', person.du);
						},
						w.d4),
					_Utils_ap(
						A2(
							$elm$core$List$map,
							function (g) {
								return A2($author$project$Domain$Node, 'GoalNode', g.a_.du);
							},
							w.$7),
						A2(
							$elm$core$List$concatMap,
							function (edge) {
								return _List_fromArray(
									[edge.dl, edge.eM]);
							},
							w.c8))))));
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
									return {k: node, aD: 24 + (column * 340), ag: 56 + (row * 118)};
								}),
							A2(
								$elm$core$List$sortBy,
								$author$project$Ui$ResponsibilityGraph$nodeLabel(w),
								A2(
									$elm$core$List$filter,
									function (node) {
										return _Utils_eq(node.a9, tag) && ((tag !== 'ResourceNode') || state.L);
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
				return A3($author$project$Ui$ResponsibilityGraph$matches, state.d9, w, edge.dl) || A3($author$project$Ui$ResponsibilityGraph$matches, state.d9, w, edge.eM);
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
							$author$project$Ui$ResponsibilityGraph$nodeKey(edge.dl),
							$author$project$Ui$ResponsibilityGraph$nodeKey(pos.k)) || _Utils_eq(
							$author$project$Ui$ResponsibilityGraph$nodeKey(edge.eM),
							$author$project$Ui$ResponsibilityGraph$nodeKey(pos.k));
					},
					edges)) && A3($author$project$Ui$ResponsibilityGraph$matches, state.d9, w, pos.k);
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
									pick(edge.dl),
									A2(
									$elm$html$Html$span,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(
											'─ ' + ($author$project$Ui$ResponsibilityGraph$relationLabel(edge.dB) + ' →'))
										])),
									pick(edge.eM)
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
										pick(pos.k),
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
var $author$project$Ui$ResponsibilityGraph$ClearSelection = {$: 5};
var $author$project$Ui$ResponsibilityGraph$personDetails = F3(
	function (go, w, node) {
		var person = $elm$core$List$head(
			A2(
				$elm$core$List$filter,
				A2(
					$elm$core$Basics$composeR,
					function ($) {
						return $.du;
					},
					$elm$core$Basics$eq(node.cT)),
				w.d4));
		var owned = A2(
			$elm$core$List$filter,
			A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.a3;
				},
				$elm$core$Basics$eq(
					$elm$core$Maybe$Just(node.cT))),
			w.$7);
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
										'역할: ' + (p.eo + (' · ' + (p.bc ? '재직' : '비활성'))))
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
												'보유 예산 ' + ($author$project$Ui$Label$formatNumber(authority.cI) + '원'))
											]))
									]);
							},
							$elm$core$List$head(
								A2(
									$elm$core$List$filter,
									A2(
										$elm$core$Basics$composeR,
										function ($) {
											return $.a3;
										},
										$elm$core$Basics$eq(node.cT)),
									w.cB)))),
					_Utils_ap(
						_List_fromArray(
							[
								A2(navigate, 'person:' + node.cT, '구성원 상세로 이동')
							]),
						A2(
							$elm$core$Maybe$withDefault,
							false,
							A2(
								$elm$core$Maybe$map,
								function ($) {
									return $.bc;
								},
								person)) ? _List_fromArray(
							[
								A2(navigate, 'authority-' + node.cT, '권한 관리로 이동')
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
			state.es);
		if (_v0.$ === 1) {
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
							return $.a_;
						},
						A2(
							$elm$core$Basics$composeR,
							function ($) {
								return $.du;
							},
							$elm$core$Basics$eq(node.cT))),
					w.$7));
			var edges = A2(
				$elm$core$List$filter,
				function (edge) {
					return _Utils_eq(edge.dl, node) || _Utils_eq(edge.eM, node);
				},
				w.c8);
			var details = (node.a9 === 'GoalNode') ? A2(
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
												g.a3)))
									])),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										(g.bc ? '활성' : '초안') + (' · ' + ($author$project$Ui$Label$statusName(g.bu.ez) + (' · 권한 통제율 ' + ($elm$core$String$fromInt(
											$elm$core$Basics$round(g.cu.cU * 100)) + '%')))))
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
											A2($elm$core$List$map, $author$project$Ui$Label$permissionName, g.a_.eh)) + (' · 필요 예산 ' + ($author$project$Ui$Label$formatNumber(g.a_.eg) + '원'))))
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
														navigate('owner-' + node.cT))
													]);
											},
											go))),
								_List_fromArray(
									[
										$elm$html$Html$text('책임자 지정 폼으로 이동')
									]))
							]);
					},
					goal)) : ((node.a9 === 'PersonNode') ? A3($author$project$Ui$ResponsibilityGraph$personDetails, go, w, node) : _List_Nil);
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
												A2($author$project$Ui$ResponsibilityGraph$nodeLabel, w, edge.dl) + (' ─ ' + ($author$project$Ui$ResponsibilityGraph$relationLabel(edge.dB) + (' → ' + A2($author$project$Ui$ResponsibilityGraph$nodeLabel, w, edge.eM)))))
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
	return {$: 2, a: a};
};
var $author$project$Ui$ResponsibilityGraph$Fit = {$: 7};
var $author$project$Ui$ResponsibilityGraph$Resources = function (a) {
	return {$: 3, a: a};
};
var $author$project$Ui$ResponsibilityGraph$Search = function (a) {
	return {$: 1, a: a};
};
var $author$project$Ui$ResponsibilityGraph$SetDiagram = function (a) {
	return {$: 0, a: a};
};
var $author$project$Ui$ResponsibilityGraph$Zoom = function (a) {
	return {$: 6, a: a};
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
							state.Y,
							$author$project$Ui$ResponsibilityGraph$SetDiagram(true)),
							A3(
							toggle,
							'관계 목록',
							!state.Y,
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
										$elm$html$Html$Attributes$value(state.d9),
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
							A3(check, '목표 간 관계', state.av, $author$project$Ui$ResponsibilityGraph$Dependencies),
							A3(check, '권한·예산', state.L, $author$project$Ui$ResponsibilityGraph$Resources)
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
										(state.F <= 1) || _Utils_eq(dispatch, $elm$core$Maybe$Nothing)),
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
										$elm$core$Basics$round(state.F * 100)) + '%')
								])),
							A2(
							$elm$html$Html$button,
							_Utils_ap(
								_List_fromArray(
									[
										$elm$html$Html$Attributes$type_('button'),
										$elm$html$Html$Attributes$class('secondary'),
										$elm$html$Html$Attributes$disabled(
										(state.F >= 3) || _Utils_eq(dispatch, $elm$core$Maybe$Nothing)),
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
			return ((edge.dB !== 'DependsOn') || state.av) && ((edge.dB !== 'Controls') || state.L);
		});
};
var $author$project$Ui$ResponsibilityGraph$view = F4(
	function (state, dispatch, go, w) {
		var positions = A2($author$project$Ui$ResponsibilityGraph$layout, state, w);
		var edges = A2($author$project$Ui$ResponsibilityGraph$visibleEdges, state, w.c8);
		var count = $elm$core$List$length(
			A2(
				$elm$core$List$filter,
				A2(
					$elm$core$Basics$composeR,
					function ($) {
						return $.k;
					},
					A2($author$project$Ui$ResponsibilityGraph$matches, state.d9, w)),
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
								$elm$core$List$length(edges)) + ('개' + (($elm$core$String$trim(state.d9) === '') ? '' : (' · 검색 일치 ' + ($elm$core$String$fromInt(count) + '개'))))))))
						])),
					((!count) && ($elm$core$String$trim(state.d9) !== '')) ? A2(
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
									!_Utils_eq(state.es, $elm$core$Maybe$Nothing))
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
								])) : (state.Y ? A5($author$project$Ui$ResponsibilityGraph$diagram, state, dispatch, w, positions, edges) : A5($author$project$Ui$ResponsibilityGraph$relationList, state, dispatch, w, positions, edges)),
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
							$elm$core$List$isEmpty(w.$7) ? A2($author$project$Ui$Common$emptyState, '아직 책임을 배정할 목표가 없습니다', '목표 메뉴에서 목표를 만든 뒤 책임자를 지정하세요.') : ((mode === 1) ? A3(
							$author$project$Ui$ListView$tableView,
							'목표별 책임',
							_List_fromArray(
								['결과 / KPI', '최종 책임자', '목표값', '필요 권한 / 통제율', '상태']),
							A2(
								$elm$core$List$map,
								A2($author$project$Page$Responsibility$responsibilityRow, model, w),
								w.$7)) : A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('grid')
								]),
							A2(
								$elm$core$List$map,
								A2($author$project$Page$Responsibility$responsibilityCard, model, w),
								w.$7)))
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
var $author$project$Form$Action$Grant = function (a) {
	return {$: 8, a: a};
};
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
												A2(model.ba, action, key) === 'true'),
												$elm$html$Html$Events$onCheck(
												function (checked_) {
													return A3(
														model.am,
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
var $author$project$Page$Authorities$authorityForm = F2(
	function (model, person) {
		return A4(
			$author$project$Ui$Form$formView,
			model.w,
			$author$project$Form$Action$Grant(person.du),
			'권한 저장',
			_List_fromArray(
				[
					A6(
					$author$project$Ui$Form$inputField,
					model.w,
					$author$project$Form$Action$Grant(person.du),
					'현재 집행 가능한 예산 한도 (KRW)',
					'budget',
					'number',
					true),
					A2(
					$author$project$Ui$Form$checks,
					model.w,
					$author$project$Form$Action$Grant(person.du))
				]));
	});
var $author$project$Page$Authorities$authorityShare = F2(
	function (w, person) {
		return $elm$core$String$fromInt(
			$elm$core$Basics$round(
				100 * A2(
					$elm$core$Maybe$withDefault,
					0,
					A2($elm$core$Dict$get, person.du, w.cY)))) + '%';
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
var $author$project$Page$Authorities$goalCount = F2(
	function (w, person) {
		return $elm$core$String$fromInt(
			$elm$core$List$length(
				A2(
					$elm$core$List$filter,
					A2(
						$elm$core$Basics$composeR,
						function ($) {
							return $.a3;
						},
						$elm$core$Basics$eq(
							$elm$core$Maybe$Just(person.du))),
					w.$7))) + '개';
	});
var $author$project$Page$Authorities$savedAuthority = F2(
	function (w, person) {
		return $elm$core$List$head(
			A2(
				$elm$core$List$filter,
				A2(
					$elm$core$Basics$composeR,
					function ($) {
						return $.a3;
					},
					$elm$core$Basics$eq(person.du)),
				w.cB));
	});
var $author$project$Page$Authorities$savedBudget = F2(
	function (w, person) {
		return A2(
			$elm$core$Maybe$withDefault,
			'미설정',
			A2(
				$elm$core$Maybe$map,
				function (a) {
					return $author$project$Ui$Label$formatNumber(a.cI) + '원';
				},
				A2($author$project$Page$Authorities$savedAuthority, w, person)));
	});
var $author$project$Page$Authorities$savedPermissions = F2(
	function (w, person) {
		var _v0 = A2($author$project$Page$Authorities$savedAuthority, w, person);
		if (_v0.$ === 1) {
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
						return A2($elm$core$List$member, key, authority.cK) || (((key === 'Hiring') && authority.cM) || ((key === 'Pricing') && authority.cL));
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
							return $.bc;
						},
						w.d4)) ? A2($author$project$Ui$Common$emptyState, '구성원을 먼저 추가하세요', '구성원 메뉴에서 재직 구성원을 추가한 뒤 권한을 부여할 수 있습니다.') : ((mode === 1) ? A3(
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
											$elm$html$Html$Attributes$id('authority-' + person.du),
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
													$elm$html$Html$text(person.dO)
												])),
											A2(
											$elm$html$Html$td,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text(person.eo)
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
									person.dO + ' · 예산 · 권한 편집',
									_List_fromArray(
										[
											A2($author$project$Page$Authorities$authorityForm, model, person)
										]))
								]);
						},
						A2(
							$elm$core$List$filter,
							function ($) {
								return $.bc;
							},
							w.d4))) : A2(
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
										$elm$html$Html$Attributes$id('authority-' + person.du),
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
												$elm$html$Html$text(person.dO)
											])),
										A2(
										$elm$html$Html$p,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('muted')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(person.eo)
											])),
										A2($author$project$Page$Authorities$authorityForm, model, person),
										$author$project$Ui$Common$note(
										'담당 목표 ' + A2($author$project$Page$Authorities$goalCount, w, person))
									]));
						},
						A2(
							$elm$core$List$filter,
							function ($) {
								return $.bc;
							},
							w.d4)))),
					$author$project$Ui$Common$diagnosticView(w)
				]));
	});
var $author$project$Form$Action$Activate = function (a) {
	return {$: 12, a: a};
};
var $author$project$Form$Action$Strategy = function (a) {
	return {$: 10, a: a};
};
var $author$project$Page$Goals$goalManagement = F3(
	function (model, w, g) {
		return _List_fromArray(
			[
				$author$project$Ui$Common$note(g.cu.d7),
				A4(
				$author$project$Ui$Form$formView,
				model.w,
				$author$project$Form$Action$Assign(g.a_.du),
				'책임자 지정',
				_List_fromArray(
					[
						A6(
						$author$project$Ui$Form$selectField,
						model.w,
						$author$project$Form$Action$Assign(g.a_.du),
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
								$elm$html$Html$Attributes$disabled(model.w.aE || ((!model.w.ao) || g.bc)),
								$elm$html$Html$Events$onClick(
								model.w.eE(
									$author$project$Form$Action$Activate(g.a_.du)))
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(
								g.bc ? '활성화됨' : '목표 활성화')
							]))
					])),
				A4(
				$author$project$Ui$Form$formView,
				model.w,
				$author$project$Form$Action$Strategy(g.a_.du),
				'전략 변경 기록',
				_List_fromArray(
					[
						A6(
						$author$project$Ui$Form$inputField,
						model.w,
						$author$project$Form$Action$Strategy(g.a_.du),
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
					g.eC))
			]);
	});
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
						$elm$html$Html$text(g.a_.c1)
					])),
				A2(
				$elm$html$Html$small,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(
						g.a_.dJ.dO + (' · ' + (((g.a_.dJ.c4 === 'HigherIsBetter') ? '↑ 증가' : '↓ 감소') + ' 목표')))
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
									A2($elm$core$Maybe$map, $author$project$Ui$Label$formatNumber, g.bu.dC)))
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
								'/ ' + ($author$project$Ui$Label$formatNumber(g.a_.eI) + (' ' + g.a_.dJ.eT)))
							]))
					])),
				A2(
				$elm$html$Html$progress,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$max('1'),
						$elm$html$Html$Attributes$value(
						$elm$core$String$fromFloat(
							A3($elm$core$Basics$clamp, 0, 1, g.bu.d8))),
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
							$elm$core$Basics$round(g.bu.d8 * 100)) + ('% 달성 · 기준 ' + $author$project$Ui$Label$formatNumber(g.a_.cE)))
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
										g.a3)))
							])),
						A2(
						$elm$html$Html$span,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(
								A2($elm$core$String$left, 10, g.a_.cW) + ' 마감')
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
							$elm$html$Html$Attributes$disabled(model.w.aE),
							$elm$html$Html$Events$onClick(
							model.ei('goal-' + g.a_.du))
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
									model.aF,
									$elm$core$Maybe$Just(g.a_.du))))
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
					$elm$html$Html$Attributes$id('goal-' + g.a_.du),
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
			A2($author$project$Form$Goal$value, model.br, field),
			model.am(field),
			label_,
			kind,
			required_);
	});
var $author$project$Page$Goals$formSelect = F5(
	function (model, label_, field, required_, options) {
		return A6(
			$author$project$Ui$Form$selectValue,
			$author$project$Form$Goal$fieldName(field),
			A2($author$project$Form$Goal$value, model.br, field),
			model.am(field),
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
				return _Utils_Tuple2(g.a_.du, g.a_.c1);
			},
			w.$7));
};
var $author$project$Page$Goals$metricPicker = F2(
	function (model, w) {
		var metrics = $elm$core$Dict$fromList(
			A2(
				$elm$core$List$map,
				function (metric) {
					return _Utils_Tuple2(metric.du, metric);
				},
				A2(
					$elm$core$List$map,
					A2(
						$elm$core$Basics$composeR,
						function ($) {
							return $.a_;
						},
						function ($) {
							return $.dJ;
						}),
					w.$7)));
		var selected = A2($elm$core$Dict$get, model.br.dK, metrics);
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
								return $.du;
							},
							selected)),
					model.am($author$project$Form$Goal$MetricId),
					'사용할 지표',
					false,
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2('', '새 지표 만들기 · ID 자동 생성'),
						A2(
							$elm$core$List$map,
							function (metric) {
								return _Utils_Tuple2(metric.du, metric.dO + (' · ' + metric.eT));
							},
							$elm$core$Dict$values(metrics)))),
					$author$project$Ui$Common$note('같은 지표를 공유하는 목표는 기존 지표를 선택하세요. 동일 지표의 책임 관계를 연결하는 데 사용합니다. 이름이 같아도 정의가 다르면 새 지표를 만드세요.'),
					function () {
					if (!selected.$) {
						var metric = selected.a;
						return $author$project$Ui$Common$note(
							'선택한 지표: ' + (metric.dO + (' / ' + (metric.eT + (' / ' + ((metric.c4 === 'HigherIsBetter') ? '높을수록 좋음' : '낮을수록 좋음'))))));
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
			model.w,
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
									model.br,
									$author$project$Form$Goal$Permission(key));
							},
							function (key) {
								return model.am(
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
									$elm$html$Html$Attributes$id('goal-' + g.a_.du),
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
													$elm$html$Html$text(g.a_.c1)
												])),
											A2(
											$elm$html$Html$small,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text(
													g.a_.dJ.dO + (' · ' + ((g.a_.dJ.c4 === 'HigherIsBetter') ? '↑ 증가' : '↓ 감소')))
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
													g.a3)))
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
												A2($elm$core$Maybe$map, $author$project$Ui$Label$formatNumber, g.bu.dC)) + (' / ' + ($author$project$Ui$Label$formatNumber(g.a_.eI) + (' ' + g.a_.dJ.eT))))
										])),
									A2(
									$elm$html$Html$td,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(
											$elm$core$String$fromInt(
												$elm$core$Basics$round(g.bu.d8 * 100)) + '%'),
											A2(
											$elm$html$Html$small,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text(
													'기준 ' + $author$project$Ui$Label$formatNumber(g.a_.cE))
												]))
										])),
									A2(
									$elm$html$Html$td,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(
											A2($elm$core$String$left, 10, g.a_.cW))
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
											model.aF,
											$elm$core$Maybe$Just(g.a_.du))))
								]),
							g.a_.c1 + ' · 책임 · 권한 · 전략 관리',
							A2(
								$elm$core$List$cons,
								A2($author$project$Page$Goals$resultLink, model, g),
								A3($author$project$Page$Goals$goalManagement, model, w, g)))
						]);
				},
				w.$7));
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
								$elm$core$List$length(w.$7),
								'측정 가능한 결과'),
								_Utils_Tuple3(
								'활성 목표',
								$elm$core$List$length(
									A2(
										$elm$core$List$filter,
										function ($) {
											return $.bc;
										},
										w.$7)),
								'책임과 권한 검증 완료'),
								_Utils_Tuple3('구조 진단', w.cR.I + w.cR.cl, '확인이 필요한 항목'),
								_Utils_Tuple3(
								'누적 학습',
								$elm$core$List$sum(
									A2(
										$elm$core$List$map,
										A2(
											$elm$core$Basics$composeR,
											function ($) {
												return $.dD;
											},
											$elm$core$List$length),
										w.el)),
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
					$elm$core$List$isEmpty(w.$7) ? A2($author$project$Ui$Common$emptyState, '현재 관리 중인 목표가 있나요?', '확인된 측정 기준이 있다면 아래에서 목표 초안을 만드세요. 모르는 내용은 조직 진단에 미확인으로 남길 수 있습니다.') : ((mode === 1) ? A2($author$project$Page$Goals$goalTable, model, w) : A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('grid')
						]),
					A2(
						$elm$core$List$map,
						A2($author$project$Page$Goals$goalCard, model, w),
						w.$7))),
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
var $author$project$Form$Action$AddPerson = {$: 3};
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
									return _Utils_Tuple2(p.du, p.dO + (' · ' + p.eo));
								},
								A2(
									$elm$core$List$filter,
									function (p) {
										return p.bc && (!_Utils_eq(
											$elm$core$Maybe$Just(p.du),
											personId));
									},
									w.d4))))
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
					return $.ef;
				},
				$elm$core$Basics$eq(
					$elm$core$Maybe$Just(person.du))),
			w.d4);
		var goals = A2(
			$elm$core$List$filter,
			A2(
				$elm$core$Basics$composeR,
				function ($) {
					return $.a3;
				},
				$elm$core$Basics$eq(
					$elm$core$Maybe$Just(person.du))),
			w.$7);
		var requiresSuccessor = !($elm$core$List$isEmpty(goals) && $elm$core$List$isEmpty(reports));
		var action = $author$project$Form$Action$DeactivatePerson(person.du);
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
							A2($author$project$Ui$Label$personName, w, person.du) + ' · 상세')
						])),
					$author$project$Ui$Common$note('구성원 ID: ' + person.du),
					$author$project$Ui$Common$note(
					'보고 대상: ' + A2(
						$elm$core$Maybe$withDefault,
						'없음',
						A2(
							$elm$core$Maybe$map,
							$author$project$Ui$Label$personName(w),
							person.ef))),
					$author$project$Ui$Common$note(
					'직속 보고자: ' + ($elm$core$List$isEmpty(reports) ? '없음' : A2(
						$elm$core$String$join,
						', ',
						A2(
							$elm$core$List$map,
							function (p) {
								return A2($author$project$Ui$Label$personName, w, p.du);
							},
							reports)))),
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('secondary'),
							$elm$html$Html$Attributes$disabled(model.w.aE || (!model.w.ao)),
							$elm$html$Html$Events$onClick(
							model.a5(person.du))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('최신 정보로 다시 불러오기')
						])),
					$author$project$Ui$Common$note('다시 불러오면 이 구성원의 저장하지 않은 기본정보와 인계 입력이 초기화됩니다.'),
					A4(
					$author$project$Ui$Form$formView,
					model.w,
					$author$project$Form$Action$UpdatePerson(person.du),
					'기본정보 저장',
					A4(
						$author$project$Page$People$profileFields,
						model.w,
						w,
						$author$project$Form$Action$UpdatePerson(person.du),
						$elm$core$Maybe$Just(person.du))),
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
										$elm$html$Html$text(g.a_.c1)
									]));
						},
						goals)),
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('secondary'),
							$elm$html$Html$Attributes$disabled(model.w.aE),
							$elm$html$Html$Events$onClick(model.$7)
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('목표 관리 →')
						])),
					person.bc ? A2(
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
							model.w,
							action,
							'비활성화 및 인계 확정',
							_List_fromArray(
								[
									A6(
									$author$project$Ui$Form$selectField,
									model.w,
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
												return _Utils_Tuple2(p.du, p.dO + (' · ' + p.eo));
											},
											A2(
												$elm$core$List$filter,
												function (p) {
													return p.bc && (!_Utils_eq(p.du, person.du));
												},
												w.d4))))
								]))
						])) : $author$project$Ui$Common$note('비활성 구성원입니다. 기본정보를 수정하고 과거 기록을 조회할 수 있으며 새 업무를 배정할 수 없습니다.')
				]));
	});
var $author$project$Page$People$matches = F3(
	function (query, status, person) {
		return ((status === 'all') || (((status === 'active') && person.bc) || ((status === 'inactive') && (!person.bc)))) && A2(
			$elm$core$String$contains,
			$elm$core$String$toLower(
				$elm$core$String$trim(query)),
			$elm$core$String$toLower(
				A2(
					$elm$core$String$join,
					' ',
					_List_fromArray(
						[
							person.dO,
							person.eo,
							A2($elm$core$Maybe$withDefault, '', person.c0),
							A2($elm$core$Maybe$withDefault, '', person.da)
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
										$elm$html$Html$text(person.dO)
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(person.eo)
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										A2($elm$core$Maybe$withDefault, '부서 미입력', person.c0))
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										A2($elm$core$Maybe$withDefault, '이메일 미입력', person.da))
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										person.bc ? '재직' : '비활성')
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
															return $.a3;
														},
														$elm$core$Basics$eq(
															$elm$core$Maybe$Just(person.du))),
													w.$7))) + '개')
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
												$elm$html$Html$Attributes$disabled(model.w.aE),
												$elm$html$Html$Events$onClick(
												model.bP(person.du))
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
							$elm$html$Html$text(person.dO)
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
							person.bc ? '재직' : '비활성')
						])),
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							person.eo + (' · ' + A2($elm$core$Maybe$withDefault, '부서 미입력', person.c0)))
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
							A2($elm$core$Maybe$withDefault, '이메일 미입력', person.da))
						])),
					$author$project$Ui$Common$note(
					'담당 목표 ' + ($elm$core$String$fromInt(
						$elm$core$List$length(
							A2(
								$elm$core$List$filter,
								A2(
									$elm$core$Basics$composeR,
									function ($) {
										return $.a3;
									},
									$elm$core$Basics$eq(
										$elm$core$Maybe$Just(person.du))),
								w.$7))) + '개')),
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('secondary'),
							$elm$html$Html$Attributes$disabled(model.w.aE),
							$elm$html$Html$Events$onClick(
							model.bP(person.du))
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
						$elm$core$Maybe$Just(p.du),
						model.es);
				},
				w.d4));
		var people = A2(
			$elm$core$List$filter,
			A2($author$project$Page$People$matches, model.d9, model.ez),
			w.d4);
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
									A6($author$project$Ui$Form$inputValue, 'people-search', model.d9, model.er, '이름 · 역할 · 부서 · 이메일 검색', 'search', false),
									A6(
									$author$project$Ui$Form$selectValue,
									'people-status',
									model.ez,
									model.dk,
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
										$elm$core$List$length(w.d4)) + '명'))))
								])),
							$elm$core$List$isEmpty(people) ? A2($author$project$Ui$Common$emptyState, '표시할 구성원이 없습니다', '아래에서 구성원을 등록하거나 검색어와 재직 상태 필터를 변경하세요.') : ((mode === 1) ? A3($author$project$Page$People$peopleTable, model, w, people) : A2(
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
					if (!selected.$) {
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
							model.w,
							$author$project$Form$Action$AddPerson,
							'구성원 등록',
							A4($author$project$Page$People$profileFields, model.w, w, $author$project$Form$Action$AddPerson, $elm$core$Maybe$Nothing))
						]))
				]));
	});
var $author$project$Form$Action$Evaluate = function (a) {
	return {$: 13, a: a};
};
var $author$project$Form$Action$Report = function (a) {
	return {$: 9, a: a};
};
var $author$project$Page$Results$resultContent = F3(
	function (model, w, g) {
		return _List_fromArray(
			[
				$author$project$Ui$Common$note('입력할 지표: ' + (g.a_.dJ.dO + (' / 단위: ' + (g.a_.dJ.eT + '. 목표값이나 예상값 대신 실제 측정값을 입력하세요.')))),
				$author$project$Ui$Common$note(g.cu.d7),
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
				model.w,
				$author$project$Form$Action$Report(g.a_.du),
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
								model.w,
								$author$project$Form$Action$Report(g.a_.du),
								'실측값',
								'value',
								'number',
								true),
								A6(
								$author$project$Ui$Form$selectField,
								model.w,
								$author$project$Form$Action$Report(g.a_.du),
								'보고자',
								'reportedBy',
								true,
								$author$project$Ui$Form$peopleOptions(w))
							])),
						A6(
						$author$project$Ui$Form$inputField,
						model.w,
						$author$project$Form$Action$Report(g.a_.du),
						'결과 설명',
						'note',
						'text',
						true)
					])),
				$elm$core$List$isEmpty(g.ei) ? $author$project$Ui$Common$note('아직 결과가 없습니다.') : A2(
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
															$elm$html$Html$text(r.ed)
														])),
													A2(
													$elm$html$Html$td,
													_List_Nil,
													_List_fromArray(
														[
															$elm$html$Html$text(
															$author$project$Ui$Label$formatNumber(r.ba))
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
																	r.ee)))
														])),
													A2(
													$elm$html$Html$td,
													_List_Nil,
													_List_fromArray(
														[
															$elm$html$Html$text(r.dY)
														]))
												]));
									},
									g.ei))
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
								$elm$html$Html$Attributes$disabled(model.w.aE || (!model.w.ao)),
								$elm$html$Html$Events$onClick(
								model.w.eE(
									$author$project$Form$Action$Evaluate(g.a_.du)))
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
								$elm$html$Html$Attributes$disabled(model.w.aE),
								$elm$html$Html$Events$onClick(
								model.$7('goal-' + g.a_.du))
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
									$elm$html$Html$Attributes$id('goal-' + g.a_.du),
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
													$elm$html$Html$text(g.a_.c1)
												])),
											A2(
											$elm$html$Html$small,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text(
													g.a_.dJ.dO + (' · ' + ((g.a_.dJ.c4 === 'HigherIsBetter') ? '↑ 증가' : '↓ 감소')))
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
													g.a3)))
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
												A2($elm$core$Maybe$map, $author$project$Ui$Label$formatNumber, g.bu.dC)) + (' / ' + ($author$project$Ui$Label$formatNumber(g.a_.eI) + (' ' + g.a_.dJ.eT))))
										])),
									A2(
									$elm$html$Html$td,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(
											$elm$core$String$fromInt(
												$elm$core$Basics$round(g.bu.d8 * 100)) + '%'),
											A2(
											$elm$html$Html$small,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text(
													'기준 ' + $author$project$Ui$Label$formatNumber(g.a_.cE))
												]))
										])),
									A2(
									$elm$html$Html$td,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(
											A2($elm$core$String$left, 10, g.a_.cW))
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
							g.a_.c1 + ' · 결과 보고 · 평가 · 이력',
							A3($author$project$Page$Results$resultContent, model, w, g))
						]);
				},
				w.$7));
	});
var $author$project$Page$Results$resultCard = F3(
	function (model, w, g) {
		return A2(
			$elm$html$Html$article,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('goal-card'),
					$elm$html$Html$Attributes$id('goal-' + g.a_.du),
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
					$elm$core$List$isEmpty(w.$7) ? A2($author$project$Ui$Common$emptyState, '아직 측정할 목표가 없습니다', '목표 메뉴에서 목표를 만든 뒤 결과를 기록하세요.') : ((mode === 1) ? A2($author$project$Page$Results$goalTable, model, w) : A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('grid')
						]),
					A2(
						$elm$core$List$map,
						A2($author$project$Page$Results$resultCard, model, w),
						w.$7)))
				]));
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
			A2($author$project$Form$Review$value, model.br, field),
			model.am(field),
			label_,
			kind,
			required_);
	});
var $author$project$Page$Learning$formSelect = F5(
	function (model, label_, field, required_, options) {
		return A6(
			$author$project$Ui$Form$selectValue,
			$author$project$Form$Review$fieldName(field),
			A2($author$project$Form$Review$value, model.br, field),
			model.am(field),
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
							A2($elm$core$String$left, 10, r.ds) + (' · ' + $author$project$Ui$Label$statusName(r.bu.ez)))
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
							A2($author$project$Ui$Label$goalName, w, r.a_))
						])),
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(r.dY)
						])),
					A2($author$project$Page$Learning$activityLink, activity, r.du),
					A2(
					$elm$html$Html$h3,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('학습')
						])),
					$elm$core$List$isEmpty(r.dD) ? $author$project$Ui$Common$note('기록된 학습 없음') : A2(
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
						r.dD)),
					A2(
					$elm$html$Html$h3,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('다음 결정')
						])),
					$elm$core$List$isEmpty(r.cZ) ? $author$project$Ui$Common$note('기록된 결정 없음') : A2(
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
										$elm$html$Html$text(d.eK),
										A2($elm$html$Html$br, _List_Nil, _List_Nil),
										A2(
										$elm$html$Html$small,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text(
												A2($author$project$Ui$Label$personName, w, d.a3) + (' · ' + A2(
													$elm$core$Maybe$withDefault,
													'기한 미정',
													A2(
														$elm$core$Maybe$map,
														$elm$core$String$left(10),
														d.cW))))
											]))
									]));
						},
						r.cZ)),
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
								return $.cl;
							},
							A2(
								$elm$core$List$filter,
								A2(
									$elm$core$Basics$composeR,
									function ($) {
										return $.du;
									},
									$elm$core$Basics$eq(r.du)),
								w.ek))))
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
										A2($elm$core$String$left, 10, r.ds))
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
										A2($author$project$Ui$Label$goalName, w, r.a_))
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										$author$project$Ui$Label$statusName(r.bu.ez))
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(r.dY)
									])),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								$elm$core$List$isEmpty(r.dD) ? _List_fromArray(
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
									r.dD)),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								$elm$core$List$isEmpty(r.cZ) ? _List_fromArray(
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
													$elm$html$Html$text(d.eK),
													A2($elm$html$Html$br, _List_Nil, _List_Nil),
													A2(
													$elm$html$Html$small,
													_List_Nil,
													_List_fromArray(
														[
															$elm$html$Html$text(
															A2($author$project$Ui$Label$personName, w, d.a3) + (' · ' + A2(
																$elm$core$Maybe$withDefault,
																'기한 미정',
																A2(
																	$elm$core$Maybe$map,
																	$elm$core$String$left(10),
																	d.cW))))
														]))
												]));
									},
									r.cZ)),
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
											return $.cl;
										},
										A2(
											$elm$core$List$filter,
											A2(
												$elm$core$Basics$composeR,
												function ($) {
													return $.du;
												},
												$elm$core$Basics$eq(r.du)),
											w.ek)))),
								A2(
								$elm$html$Html$td,
								_List_Nil,
								_List_fromArray(
									[
										A2($author$project$Page$Learning$activityLink, activity, r.du)
									]))
							]));
				},
				w.el));
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
							model.w,
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
													A2($author$project$Form$Review$value, model.br, 2)),
													$elm$html$Html$Events$onInput(
													model.am(2))
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
					$elm$core$List$isEmpty(w.el) ? A2($author$project$Ui$Common$emptyState, '아직 회고 기록이 없습니다', '위에서 회고를 기록해 학습과 다음 결정을 남기세요.') : ((mode === 1) ? A2($author$project$Page$Learning$reviewTable, activity, w) : A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('grid')
						]),
					A2(
						$elm$core$List$map,
						A2($author$project$Page$Learning$reviewCard, activity, w),
						w.el)))
				]));
	});
var $author$project$Main$workspaceView = function (model) {
	return A2(
		$author$project$Remote$view,
		model.ad.bb,
		function (w) {
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						(w.bp && (model.bR.ac !== 12)) ? A2(
						$author$project$Ui$Guide$view,
						{
							aE: $author$project$Main$busy(model),
							aZ: $author$project$App$Update$Guide,
							aI: model.bR.aI,
							eO: $author$project$App$Update$ToggleGuide
						},
						w) : $elm$html$Html$text(''),
						((!w.bp) && (model.bR.ac !== 12)) ? A4(
						$author$project$Page$Discovery$guide,
						model.bR.aI,
						$author$project$App$Update$ToggleGuide,
						function (page) {
							return A2($author$project$App$Update$Navigate, page, model.ad.aL);
						},
						A2(
							$elm$core$Maybe$withDefault,
							$author$project$Domain$Discovery$empty,
							A2(
								$elm$core$Maybe$map,
								function ($) {
									return $.Z;
								},
								A2($author$project$App$Discovery$saved, w.bQ.du, model.Z)))) : $elm$html$Html$text(''),
						function () {
						var _v0 = model.bR.ac;
						switch (_v0) {
							case 1:
								return A3($author$project$Main$discoveryPage, 1, model, w);
							case 2:
								return A3($author$project$Main$discoveryPage, 2, model, w);
							case 3:
								return A2(
									$author$project$Page$Agents$view,
									{
										aE: $author$project$Main$busy(model),
										am: $author$project$App$Update$EditAgents,
										dh: A2($author$project$Api$Path$orgPath, w.bQ.du, 'agents/export'),
										aZ: function (target) {
											return A2($author$project$App$Update$Navigate, target, model.ad.aL);
										},
										dv: $author$project$App$Update$ImportAgentDrafts,
										aL: w.bQ.du,
										bZ: $author$project$App$Update$RebaseAgents,
										a5: $author$project$App$Update$ResetAgents,
										b1: A3($author$project$Main$discoveryPage, 3, model, w),
										b4: $author$project$App$Update$SubmitAgents,
										cb: model.N
									},
									w);
							case 4:
								return A3($author$project$Page$Agents$graph, model.N, w.bQ.du, w);
							case 5:
								return A3(
									$author$project$Page$People$viewWith,
									$author$project$Main$listMode(model),
									{
										dk: $author$project$App$Update$FilterPeople,
										w: $author$project$Main$formConfig(model),
										$7: A2($author$project$App$Update$Navigate, 6, model.ad.aL),
										bP: $author$project$App$Update$OpenPerson,
										d9: model.bR.aN,
										a5: $author$project$App$Update$ResetPerson,
										er: $author$project$App$Update$SearchPeople,
										es: model.bR.aU,
										ez: model.bR.aO
									},
									w);
							case 6:
								return A3(
									$author$project$Page$Goals$viewWith,
									$author$project$Main$listMode(model),
									{
										br: $author$project$App$Drafts$goalDraft(model),
										am: $author$project$App$Update$EditGoal,
										aF: model.bR.aF,
										w: $author$project$Main$formConfig(model),
										ei: $author$project$App$Update$Guide(9)
									},
									w);
							case 7:
								return A6(
									$author$project$Page$Responsibility$viewInteractive,
									$author$project$Main$listMode(model),
									model.bR.ap,
									$elm$core$Maybe$Just($author$project$App$Update$GraphMsg),
									$elm$core$Maybe$Just($author$project$App$Update$GraphGo),
									{
										w: $author$project$Main$formConfig(model)
									},
									w);
							case 8:
								return A3(
									$author$project$Page$Authorities$viewWith,
									$author$project$Main$listMode(model),
									{
										w: $author$project$Main$formConfig(model)
									},
									w);
							case 9:
								return A3(
									$author$project$Page$Results$viewWith,
									$author$project$Main$listMode(model),
									{
										w: $author$project$Main$formConfig(model),
										$7: $author$project$App$Update$Guide(6)
									},
									w);
							case 10:
								return A4(
									$author$project$Page$Learning$viewWithActivity,
									$elm$core$Maybe$Just($author$project$App$Update$OpenReviewActivity),
									$author$project$Main$listMode(model),
									{
										br: $author$project$App$Drafts$reviewDraft(model),
										am: $author$project$App$Update$EditReview,
										w: $author$project$Main$formConfig(model)
									},
									w);
							case 11:
								return A4(
									$author$project$Page$Activity$view,
									$author$project$Main$listMode(model),
									model.bR.X,
									$author$project$App$Update$ActivityChange,
									w);
							case 12:
								return A2(
									$author$project$Page$Settings$view,
									{
										cP: $author$project$App$Update$CloseDelete,
										cS: $author$project$App$Update$ConfirmDelete,
										bo: model.w.bo,
										w: $author$project$Main$formConfig(model),
										$7: A2($author$project$App$Update$Navigate, 6, model.ad.aL),
										dQ: $author$project$App$Update$NoOp,
										d0: $author$project$App$Update$OpenDelete
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
										$elm$html$Html$text('ORGANIZATION → AGENT ROLES')
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
									var _v0 = model.ad.bb;
									if (_v0.$ === 1) {
										var w = _v0.a;
										return w.bQ.dO;
									} else {
										return '조직 워크스페이스';
									}
								}()),
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$type_('button'),
										$elm$html$Html$Attributes$classList(
										_List_fromArray(
											[
												_Utils_Tuple2('workspace-switch', true),
												_Utils_Tuple2('selected', !model.bR.ac)
											])),
										A2(
										$elm$html$Html$Attributes$attribute,
										'aria-current',
										(!model.bR.ac) ? 'page' : 'false'),
										$elm$html$Html$Attributes$disabled(
										$author$project$Main$busy(model)),
										$elm$html$Html$Events$onClick(
										A2($author$project$App$Update$Navigate, 0, $elm$core$Maybe$Nothing))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(
										_Utils_eq(model.ad.aL, $elm$core$Maybe$Nothing) ? '조직 선택 →' : '조직 전환 →')
									]))
							])),
						A2(
						$elm$html$Html$nav,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$attribute, 'aria-label', '주요 화면')
							]),
						A2(
							$elm$core$List$map,
							function (_v1) {
								var title = _v1.a;
								var pages = _v1.b;
								return A2(
									$elm$html$Html$section,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('nav-group'),
											A2($elm$html$Html$Attributes$attribute, 'aria-label', title)
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$h2,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$class('nav-group-title')
												]),
											_List_fromArray(
												[
													$elm$html$Html$text(title)
												])),
											A2(
											$elm$html$Html$div,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$class('nav-group-items')
												]),
											A2(
												$elm$core$List$map,
												$author$project$Main$navigationButton(model),
												pages))
										]));
							},
							_List_fromArray(
								[
									_Utils_Tuple2(
									'조직 운영',
									_List_fromArray(
										[5, 6, 7, 8])),
									_Utils_Tuple2(
									'운영과 개선',
									_List_fromArray(
										[9, 10, 11])),
									_Utils_Tuple2(
									'조직 분석',
									_List_fromArray(
										[1, 2])),
									_Utils_Tuple2(
									'에이전트 설계',
									_List_fromArray(
										[3, 4]))
								])))
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
												$author$project$Page$pageName(model.bR.ac))
											])),
										A2(
										$elm$html$Html$p,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('현재 조직의 역할과 업무를 기록하고, 근거를 검토하며 멀티 AI 에이전트 구조를 설계합니다.')
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
										(!_Utils_eq(model.ad.aL, $elm$core$Maybe$Nothing)) ? A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('secondary'),
												$elm$html$Html$Attributes$disabled(
												$author$project$Main$busy(model)),
												$elm$html$Html$Events$onClick(
												A2($author$project$App$Update$Navigate, 12, model.ad.aL))
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
												$elm$html$Html$Events$onClick($author$project$App$Update$Refresh)
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
										_Utils_Tuple2('error', model.bt)
									])),
								A2(
								$elm$html$Html$Attributes$attribute,
								'role',
								model.bt ? 'alert' : 'status'),
								A2($elm$html$Html$Attributes$attribute, 'aria-live', 'polite')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(model.bN)
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
								$author$project$Main$busy(model) ? '저장 중 · 완료 후 다음 작업을 진행하세요' : (model.ad.ao ? '최신 상태 · 입력은 화면을 이동해도 유지됩니다' : (model.ad.aw ? '최신 상태를 확인하고 있습니다…' : '최신 상태 확인 실패 · 새로고침해 주세요')))
							])),
						(!A2(
						$elm$core$List$member,
						model.bR.ac,
						_List_fromArray(
							[12, 1, 2, 3, 4]))) ? A2(
						$author$project$Ui$ListView$controls,
						$author$project$Main$listMode(model),
						$author$project$App$Update$SetListMode(model.bR.ac)) : $elm$html$Html$text(''),
						(!model.bR.ac) ? A2(
						$author$project$Page$Organizations$viewWith,
						$author$project$Main$listMode(model),
						{
							w: $author$project$Main$formConfig(model),
							bP: function (org) {
								return A2(
									$author$project$App$Update$Navigate,
									1,
									$elm$core$Maybe$Just(org));
							},
							aM: model.ad.aM,
							ev: function (org) {
								return A2(
									$author$project$App$Update$Navigate,
									12,
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
		dy: A2($elm$core$Basics$composeR, $author$project$App$Update$init, $author$project$Main$runEffects),
		eF: $elm$core$Basics$always($elm$core$Platform$Sub$none),
		eV: F2(
			function (msg, model) {
				return $author$project$Main$runEffects(
					A2($author$project$App$Update$update, msg, model));
			}),
		eW: $author$project$Main$view
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
								{cW: deadline, b6: seed, eN: today});
						},
						A2($elm$json$Json$Decode$field, 'deadline', $elm$json$Json$Decode$string));
				},
				A2($elm$json$Json$Decode$field, 'seed', $elm$json$Json$Decode$string));
		},
		A2($elm$json$Json$Decode$field, 'today', $elm$json$Json$Decode$string)))(0)}});}(this));