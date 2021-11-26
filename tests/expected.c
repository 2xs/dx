struct info_s;
struct info_s {
  _Bool info_a;
  _Bool info_b;
};

extern _Bool id(_Bool);

extern unsigned int axiom;

extern _Bool neg(_Bool);

extern _Bool testId(_Bool);

extern void prepare(unsigned int);

extern _Bool getInfoA(struct info_s);

_Bool id(_Bool b)
{
  return b;
}

extern unsigned int axiom;

_Bool neg(_Bool b$22)
{
  if (b$22) {
    return 0;
  } else {
    return 1;
  }
}

_Bool testId(_Bool b$24)
{
  return b$24;
}

extern void emptyUnitM(void);

extern _Bool ready(void);

extern void getReady(void);

void prepare(unsigned int recBound)
{
  unsigned int b$28;
  _Bool r;
  if (recBound == 0U) {
    emptyUnitM();
    return;
  } else {
    b$28 = recBound - 1U;
    r = ready();
    if (r) {
      return;
    } else {
      getReady();
      prepare(b$28);
      return;
    }
  }
}

_Bool getInfoA(struct info_s i)
{
  return i.info_a;
}


