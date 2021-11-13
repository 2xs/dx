extern _Bool id(_Bool);

extern unsigned int const axiom;

extern _Bool neg(_Bool);

extern _Bool testId(_Bool);

extern void prepare(unsigned int);

_Bool id(_Bool b)
{
  return b;
}

extern unsigned int const axiom;

_Bool neg(_Bool b$20)
{
  if (b$20) {
    return 0;
  } else {
    return 1;
  }
}

_Bool testId(_Bool b$22)
{
  return b$22;
}

extern void emptyUnitM(void);

extern _Bool ready(void);

extern void getReady(void);

void prepare(unsigned int recBound)
{
  unsigned int b$26;
  _Bool r;
  if (recBound == 0U) {
    emptyUnitM();
    return;
  } else {
    b$26 = recBound - 1U;
    r = ready();
    if (r) {
      return;
    } else {
      getReady();
      prepare(b$26);
      return;
    }
  }
}


