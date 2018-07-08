//
// aroext.c
//
// Aro standard library extern definitions
//

#include <arostd.h>

#include <limits.h>

int _aro_hook__std__math_floordiv(double left, double right) {
  double quotient = left / right;
  if (quotient > INT_MAX || quotient < INT_MIN) {
    fprintf(stderr,
            "The result of the division is too big, just like your ego.\n");
    exit(EXIT_FAILURE);
  }

  return (int)quotient;
}

_Aro_Object _aro_hook__std__list_push(_Aro_Any element, _Aro_Object list) {
  _Aro_Object new_node = malloc(sizeof(_Aro_Any) * 2);
  new_node[0] = element;
  new_node[1].Object = list;

  return new_node;
}

bool _aro_hook__std__list_is_empty(_Aro_Object list) { return list == NULL; }

_Aro_Any _aro_hook__std__list_head(_Aro_Object list) {
  if (list == NULL) {
    fprintf(stderr,
            "As usual, you can't get head.\n"
            "Especially not from an empty list.\n");
    exit(EXIT_FAILURE);
  }

  return list[0];
}

_Aro_Object _aro_hook__std__list_tail(_Aro_Object list) {
  if (list == NULL) {
    fprintf(stderr,
            "You spent your whole life chasing your own tail.\n"
            "You can't even find the tail of a list.\n"
            "The list is empty, dumbass.");
    exit(EXIT_FAILURE);
  }

  return list[1].Object;
}
