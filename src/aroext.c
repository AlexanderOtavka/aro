//
// aroext.c
//
// Aro standard library extern definitions
//

#include <arostd.h>

#define _ARO_STD_EXT_DEFINE_CLOSURE(name, return_type, argument) \
    _Aro_Closure _aro_hook__std__##name; \
    return_type _aro_std_ext_fn__##name(argument, _Aro_Object _captures)

#define _ARO_STD_EXT_BIND_CLOSURE(name) \
    _aro_hook__std__##name = malloc(sizeof(_Aro_Any)); \
    _aro_hook__std__##name->Void_Ptr = _aro_std_ext_fn__##name;


_ARO_STD_EXT_DEFINE_CLOSURE(math__floordiv, int, _Aro_Object tuple_arg) {
    double left  = tuple_arg[0].Float;
    double right = tuple_arg[1].Float;

    if ((int)right == 0) {
        fprintf(stderr, "Can't divide by your future (which is zero).\n");
        exit(EXIT_FAILURE);
    }

    return (int)left / (int)right;
}

_ARO_STD_EXT_DEFINE_CLOSURE(list__push, _Aro_Object, _Aro_Object tuple_arg) {
    _Aro_Any    element = tuple_arg[0];
    _Aro_Object list    = tuple_arg[1].Object;

    _Aro_Object new_node = malloc(sizeof(_Aro_Any) * 2);
    new_node[0] = element;
    new_node[1].Object = list;

    return new_node;
}

_ARO_STD_EXT_DEFINE_CLOSURE(list__is_empty, bool, _Aro_Object list) {
    return list == NULL;
}

_ARO_STD_EXT_DEFINE_CLOSURE(list__head, _Aro_Any, _Aro_Object list) {
    if (list == NULL) {
        fprintf(stderr, "As usual, you can't get head.\n"
                        "Especially not from an empty list.\n");
        exit(EXIT_FAILURE);
    }

    return list[0];
}

_ARO_STD_EXT_DEFINE_CLOSURE(list__tail, _Aro_Object, _Aro_Object list) {
    if (list == NULL) {
        fprintf(stderr, "You spent your whole life chasing your own tail.\n"
                        "You can't even find the tail of a list.\n"
                        "The list is empty, dumbass.");
        exit(EXIT_FAILURE);
    }

    return list[1].Object;
}

_ARO_STD_EXT_DEFINE_CLOSURE(ref__new__, _Aro_Any*, _Aro_Any value) {
    _Aro_Any* ref = malloc(sizeof(value));
    *ref = value;
    return ref;
}

_ARO_STD_EXT_DEFINE_CLOSURE(ref__get__, _Aro_Any, _Aro_Any* ref) {
    return *ref;
}

_ARO_STD_EXT_DEFINE_CLOSURE(ref__set__, _Aro_Any*, _Aro_Object tuple_arg) {
    _Aro_Any* ref       = tuple_arg[0].Ref;
    _Aro_Any  new_value = tuple_arg[1];

    *ref = new_value;
    return ref;
}

void _aro_std_ext_init(void) {
    // std.math.*
    _ARO_STD_EXT_BIND_CLOSURE(math__floordiv);

    // std.list.*
    _ARO_STD_EXT_BIND_CLOSURE(list__push);
    _ARO_STD_EXT_BIND_CLOSURE(list__is_empty);
    _ARO_STD_EXT_BIND_CLOSURE(list__head);
    _ARO_STD_EXT_BIND_CLOSURE(list__tail);

    // std.ref.*
    _ARO_STD_EXT_BIND_CLOSURE(ref__new__);
    _ARO_STD_EXT_BIND_CLOSURE(ref__get__);
    _ARO_STD_EXT_BIND_CLOSURE(ref__set__);
}
