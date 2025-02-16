const UnicodeToLower = (codePoint: number) => {

}

const UTF16SurrogatePairToCodePoint = (lead: number, trail: number) => {
  return (lead - 0xd800) * 0x400 + (trail - 0xdc00) + 0x10000;
};

const StringCharAt = (str: string, position: number) => {
  let size = str.length;
  if (position < 0 || position >= size) {
    return '';
  }

  return str[position];
};

const StringCodePointAt = (str: string, position: number) => {
  let size = str.length;
  if (position < 0 || position >= size) {
    return undefined;
  }

  let first = Number(str[position]);
  let cp = first;

  // Check if the character is neither a high surrogate nor a low surrogate
  if (!((first >= 0xd800 && first <= 0xdfff) || (first >= 0xdc00 && first <= 0xdfff))) {
    return {
      codePoint: cp,
      codeUnitCount: 1,
      isUnpairedSurrogate: false
    };
  }

  // If first is a trailing surrogate or position + 1 = size, then
  // a. Return the Record { [[CodePoint]]: cp, [[CodeUnitCount]]: 1, [[IsUnpairedSurrogate]]: true }.

  if ((first >= 0xdc00 && first <= 0xdfff) || position + 1 === size) {
    return {
      codePoint: cp,
      codeUnitCount: 1,
      isUnpairedSurrogate: true
    };
  }

  // Let second be the code unit at index position + 1 within string.
  let second = Number(str[position + 1]);

  // If second is not in the range 0xDC00 to 0xDFFF, return the Record { [[CodePoint]]: cp, [[CodeUnitCount]]: 1, [[IsUnpairedSurrogate]]: true }.
  if (!(second >= 0xdc00 && second <= 0xdfff)) {
    return {
      codePoint: cp,
      codeUnitCount: 1,
      isUnpairedSurrogate: true
    };
  }

  cp = UTF16SurrogatePairToCodePoint(first, second);
  return {
    codePoint: cp,
    codeUnitCount: 2,
    isUnpairedSurrogate: false
  };
};

const StringToCodePoints = (str) => {
  let codePoints: number[] = [];
  let size = str.length;

  let position = 0;

  while (position < size) {
    let cp = StringCodePointAt(str, position);
    if (cp) {
      codePoints.push(cp.codePoint);
      position += cp.codeUnitCount;
    }
  }

  return codePoints;
};

const StringToLowerCase = (str) => {
  let sText = StringToCodePoints(str);
};
