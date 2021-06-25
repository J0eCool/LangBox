func toString*(chars: seq[char]): string =
  result = newString(chars.len)
  for i in 0..<chars.len:
    result[i] = chars[i]
