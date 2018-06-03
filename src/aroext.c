//
// aroext.c
//
// Aro standard library extern definitions
//

#include <arostd.h>

_Aro_Closure _aro_hook__std__math__floordiv;

int _aro_std_ext__math_floordiv(_Aro_Object arg, _Aro_Object captures) {
    double left = arg[0].Float;
    double right = arg[1].Float;
    return (int)left / (int)right;
}

void _aro_std_ext_init(void) {
    // std.math.floordiv
    _aro_hook__std__math__floordiv = malloc(sizeof(_Aro_Any));
    _aro_hook__std__math__floordiv->Void_Ptr = _aro_std_ext__math_floordiv;
}
