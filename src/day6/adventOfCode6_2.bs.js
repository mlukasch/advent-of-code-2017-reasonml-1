// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var List                    = require("bs-platform/lib/js/list.js");
var $$Array                 = require("bs-platform/lib/js/array.js");
var Caml_obj                = require("bs-platform/lib/js/caml_obj.js");
var Caml_array              = require("bs-platform/lib/js/caml_array.js");
var Caml_int32              = require("bs-platform/lib/js/caml_int32.js");
var Pervasives              = require("bs-platform/lib/js/pervasives.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

var myInput = /* :: */[
  11,
  /* :: */[
    11,
    /* :: */[
      13,
      /* :: */[
        7,
        /* :: */[
          0,
          /* :: */[
            15,
            /* :: */[
              5,
              /* :: */[
                5,
                /* :: */[
                  4,
                  /* :: */[
                    4,
                    /* :: */[
                      1,
                      /* :: */[
                        1,
                        /* :: */[
                          7,
                          /* :: */[
                            1,
                            /* :: */[
                              15,
                              /* :: */[
                                11,
                                /* [] */0
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]
];

var length = List.length(myInput);

function withIndex(arr) {
  return List.mapi((function (i, a) {
                return /* tuple */[
                        i,
                        a
                      ];
              }), arr);
}

var initial = withIndex(myInput);

function max(param) {
  if (param) {
    return List.fold_left((function (param, param$1) {
                  var b = param$1[1];
                  var a = param[1];
                  if (Caml_obj.caml_greaterequal(a, b)) {
                    return /* tuple */[
                            param[0],
                            a
                          ];
                  } else {
                    return /* tuple */[
                            param$1[0],
                            b
                          ];
                  }
                }), param[0], param[1]);
  } else {
    return Pervasives.invalid_arg("Empty list");
  }
}

function newList(bank, biggest) {
  var arr = $$Array.of_list(bank);
  var startIndex = biggest[0];
  Caml_array.caml_array_set(arr, startIndex, 0);
  for(var x = startIndex + 1 | 0 ,x_finish = startIndex + biggest[1] | 0; x <= x_finish; ++x){
    var index = Caml_int32.mod_(x, length);
    Caml_array.caml_array_set(arr, index, Caml_array.caml_array_get(arr, index) + 1 | 0);
  }
  return $$Array.to_list(arr);
}

function foo(_bank, _acc, _biggest, _steps, _found) {
  while(true) {
    var found = _found;
    var steps = _steps;
    var biggest = _biggest;
    var acc = _acc;
    var bank = _bank;
    var exit = 0;
    var val;
    try {
      val = List.find((function(bank){
          return function (a) {
            return Caml_obj.caml_equal(a, bank);
          }
          }(bank)), acc);
      exit = 1;
    }
    catch (exn){
      if (exn === Caml_builtin_exceptions.not_found) {
        var newList$1 = newList(bank, biggest);
        _steps = steps + 1 | 0;
        _biggest = max(withIndex(newList$1));
        _acc = /* :: */[
          bank,
          acc
        ];
        _bank = newList$1;
        continue ;
        
      } else {
        throw exn;
      }
    }
    if (exit === 1) {
      if (found > 0) {
        return steps - found | 0;
      } else {
        var newList$2 = newList(bank, biggest);
        _found = steps;
        _steps = steps + 1 | 0;
        _biggest = max(withIndex(newList$2));
        _acc = List.filter((function(bank){
              return function (a) {
                return Caml_obj.caml_equal(a, bank);
              }
              }(bank)))(acc);
        _bank = newList$2;
        continue ;
        
      }
    }
    
  };
}

console.log("start");

console.log(foo(myInput, /* [] */0, max(initial), 0, 0));

exports.myInput   = myInput;
exports.length    = length;
exports.withIndex = withIndex;
exports.initial   = initial;
exports.max       = max;
exports.newList   = newList;
exports.foo       = foo;
/* length Not a pure module */
