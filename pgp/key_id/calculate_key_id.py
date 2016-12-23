# Example of calculating the PGP Key ID for a V4 public key packet.
#
# Carlo Hamalainen (2016)

import hashlib
import hexdump

###    # off=3038 ctb=b9 tag=14 hlen=3 plen=269
###    :public sub key packet:
###    version 4, algo 1, created 1481611279, expires 0
###    pkey[0]: A516862F512D2B1C36268DABBD9CD2844FF75F0EC24680840E9A5528475D3D2EB9F204514E4749ECAA74317D95C4A13B22AAB78DDF731CA07385B84F2252C0F505FE453401A31DD98EBB8936B3079C3A7D9EFD6E5A4ABF8E652ACC1A2A570582FD06F813A9DB52A6676C4057B7ED8304A6839FE14E7019A33C9A941866371A02234943DF2B81A43EA1A1EB51746FE466300939AE56E8DABC336FD92C4325DB105813C6B1C82786F1A3A8B5190A86A46C408F8A10F2617814F0DBEDB13AA6A3B3C6CE0550DACBEE2A1A0D398F2B5F547C1ABF69CB70CE47B381454335E12CE8478CBCB02C71452487DC9DF2CBDB16B76C84FDA7C787B14758F05B656753669D2F
###    pkey[1]: 010001
###    keyid: 17118623766D56F8

def to_str(x):
    return ''.join(x)

f = open('carlohamalainen-publickey-2016-12-23.bin', 'rb')
xs = []
try:
    byte = f.read(1)
    while byte != '':
        byte = f.read(1)
        xs.append(byte)
finally:
    f.close()

OFFSET = 3038
PLEN   = 269

block = xs[OFFSET:OFFSET+PLEN+2] # +2 for the length bytes?...

# Version 4 packet.
# https://tools.ietf.org/html/rfc4880#section-5.5.2
assert block[2] == '\x04'

# Key IDs: https://tools.ietf.org/html/rfc4880#section-12.2
# 
#     A V4 fingerprint is the 160-bit SHA-1 hash of the octet 0x99,
#     followed by the two-octet packet length, followed by the entire
#     Public-Key packet starting with the version field.  The Key ID is the
#     low-order 64 bits of the fingerprint.

for_fingerprint = ['\x99'] + block

h = hashlib.sha1(to_str(for_fingerprint))

# The SHA1 digest is 20 bytes, so we get the low-order 64 bits (8 bytes)
# by dropping the first 20 - 8 == 12 bytes of the digest:
assert h.digest_size == 20
key_id = hexdump.dump(to_str(h.digest()[12:]), sep='')

assert key_id == '17118623766D56F8'

print 'block:', hexdump.dump(to_str(block), sep='')
print
print 'for_fingerprint:', hexdump.dump(to_str(for_fingerprint), sep='')
print
print 'Key ID:', key_id
