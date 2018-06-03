//
// aroext.c
//
// Aro standard library extern definitions
//

#include <arostd.h>

_Aro_Closure _aro_hook__std__math__floordiv;
_Aro_Closure _aro_hook__std__list__push;
_Aro_Closure _aro_hook__std__list__is_empty;
_Aro_Closure _aro_hook__std__list__head;
_Aro_Closure _aro_hook__std__list__tail;

int _aro_std_ext__math__floordiv(_Aro_Object tuple_arg, _Aro_Object captures) {
    double left  = tuple_arg[0].Float;
    double right = tuple_arg[1].Float;
    return (int)left / (int)right;
}

_Aro_Object _aro_std_ext__list__push(_Aro_Object tuple_arg, _Aro_Object captures) {
    _Aro_Any    element = tuple_arg[0];
    _Aro_Object list    = tuple_arg[1].Object;

    _Aro_Object new_node = malloc(sizeof(_Aro_Any) * 2);
    new_node[0] = element;
    new_node[1].Object = list;

    return new_node;
}

bool _aro_std_ext__list__is_empty(_Aro_Object list, _Aro_Object captures) {
    return list == NULL;
}

_Aro_Any _aro_std_ext__list__head(_Aro_Object list, _Aro_Object captures) {
    return list[0];
}

_Aro_Object _aro_std_ext__list__tail(_Aro_Object list, _Aro_Object captures) {
    return list[1].Object;
}

void _aro_std_ext_init(void) {
    // std.math.floordiv
    _aro_hook__std__math__floordiv = malloc(sizeof(_Aro_Any));
    _aro_hook__std__math__floordiv->Void_Ptr = _aro_std_ext__math__floordiv;

    // std.list.push
    _aro_hook__std__list__push = malloc(sizeof(_Aro_Any));
    _aro_hook__std__list__push->Void_Ptr = _aro_std_ext__list__push;

    // std.list.is_empty
    _aro_hook__std__list__is_empty = malloc(sizeof(_Aro_Any));
    _aro_hook__std__list__is_empty->Void_Ptr = _aro_std_ext__list__is_empty;

    // std.list.head
    _aro_hook__std__list__head = malloc(sizeof(_Aro_Any));
    _aro_hook__std__list__head->Void_Ptr = _aro_std_ext__list__head;

    // std.list.tail
    _aro_hook__std__list__tail = malloc(sizeof(_Aro_Any));
    _aro_hook__std__list__tail->Void_Ptr = _aro_std_ext__list__tail;
}
