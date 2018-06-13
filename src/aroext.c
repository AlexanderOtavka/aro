//
// aroext.c
//
// Aro standard library extern definitions
//

#include <arostd.h>

#include <limits.h>

ARO_DEFINE_CLOSURE_HOOK(std__math__floordiv, int, _Aro_Object tuple_arg) {
  double left = tuple_arg[0].Float;
  double right = tuple_arg[1].Float;

  double quotient = left / right;
  if (quotient > INT_MAX || quotient < INT_MIN) {
    fprintf(stderr,
            "The result of the division is too big, just like your ego.\n");
    exit(EXIT_FAILURE);
  }

  return (int)quotient;
}

ARO_DEFINE_CLOSURE_HOOK(std__list__push, _Aro_Object, _Aro_Object tuple_arg) {
  _Aro_Any element = tuple_arg[0];
  _Aro_Object list = tuple_arg[1].Object;

  _Aro_Object new_node = malloc(sizeof(_Aro_Any) * 2);
  new_node[0] = element;
  new_node[1].Object = list;

  return new_node;
}

ARO_DEFINE_CLOSURE_HOOK(std__list__is_empty, bool, _Aro_Object list) {
  return list == NULL;
}

ARO_DEFINE_CLOSURE_HOOK(std__list__head, _Aro_Any, _Aro_Object list) {
  if (list == NULL) {
    fprintf(stderr,
            "As usual, you can't get head.\n"
            "Especially not from an empty list.\n");
    exit(EXIT_FAILURE);
  }

  return list[0];
}

ARO_DEFINE_CLOSURE_HOOK(std__list__tail, _Aro_Object, _Aro_Object list) {
  if (list == NULL) {
    fprintf(stderr,
            "You spent your whole life chasing your own tail.\n"
            "You can't even find the tail of a list.\n"
            "The list is empty, dumbass.");
    exit(EXIT_FAILURE);
  }

  return list[1].Object;
}

void _aro_std_ext_init(void) {
  // std.math.*
  ARO_BIND_CLOSURE_HOOK(std__math__floordiv);

  // std.list.*
  ARO_BIND_CLOSURE_HOOK(std__list__push);
  ARO_BIND_CLOSURE_HOOK(std__list__is_empty);
  ARO_BIND_CLOSURE_HOOK(std__list__head);
  ARO_BIND_CLOSURE_HOOK(std__list__tail);
}
