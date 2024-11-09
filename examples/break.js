var a = 5;

for (let i = 0; i < 5; i++) {
  if (i == 2) {
    break;
  }
  a = a + 1;
}
__numberLog__(a);
