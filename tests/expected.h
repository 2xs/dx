#ifndef GENERATED_H
#define GENERATED_H

struct info_s;
struct info_s {
  _Bool info_a;
  _Bool info_b;
};

extern _Bool id(_Bool);

extern unsigned int axiom;

_Bool id(_Bool b)
{
  return b;
}

extern unsigned int axiom;

extern _Bool neg(_Bool);

extern _Bool testId(_Bool);

extern void emptyUnitM(void);

extern _Bool ready(void);

extern void getReady(void);

extern void prepare(unsigned int);

extern _Bool getInfoA(struct info_s);


#endif
