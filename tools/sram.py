#!/usr/bin/env python3
#
# Create an SRAM image containing files, for testing.
#

import sys

SIZE = 8 * 1024
NAME_LEN = 14
HDR_LEN = NAME_LEN + 2

buf = [0] * SIZE

free = SIZE - 5


def conv(asc):
  out = []
  for c in asc:
    c = c.upper()
    if c >= '0' and c <= '9':
      out.append(ord(c) - ord('0'))
    elif c >= 'A' and c <= 'Z':
      out.append(ord(c) - ord('A') + 10)
    elif c == '/':
      out.append(0x25)
    elif c == '-':
      out.append(0x26)
    elif c == '.':
      out.append(0x27)
    else:
      out.append(0x24)

  if len(out) > NAME_LEN:
    out = out[:NAME_LEN]

  while len(out) < NAME_LEN:
    out.append(0)

  return out


count = 0
offset = 5
for filename in sys.argv[1:]:
  f = open(filename, 'rb')
  data = f.read()
  f.close()
  size = len(data)
  name = conv(filename)

  if size > free:
    print("No space for '" + filename + "' (" + str(size) + ")")
  else:
    print("Adding '" + filename + "' (" + str(size) + ")")
    count = count + 1
    free = free - (size + HDR_LEN)

    # name
    for i in range(NAME_LEN):
      buf[offset] = name[i]
      offset = offset + 1

    # size
    buf[offset] = (size & 0x00ff)
    offset = offset + 1
    buf[offset] = (size & 0xff00) >> 8
    offset = offset + 1

    # data
    for i in range(size):
      buf[offset] = data[i]
      offset = offset + 1

buf[0] = (free & 0x00ff)
buf[1] = (free & 0xff00) >> 8
buf[2] = count

checksum = 0
for i in range(SIZE):
  checksum = (checksum + buf[i]) & 0xff

checksum = checksum ^ 0xff

buf[3] = checksum & 0xff

print("Free: " + str(free) + ", Checksum: 0x" + format(checksum, '02x'))

outbin = open("bios.srm", "wb")
outbin.write(bytes(buf))
outbin.close()
