# Example of calculating the PGP Key ID for a V4 public key packet.
#
# Example of checking the hash of the subkey signing signature.
#
# Carlo Hamalainen (2016)

"""

$ gpg2 -a --export carlo@carlo-hamalainen.net | gpg2 --list-packets --verbose  

# off=0 ctb=99 tag=6 hlen=3 plen=525
:public key packet:
   version 4, algo 1, created 1373439395, expires 0
   pkey[0]: C35E579D722847E70515382572B28200DC23E66E584E48F310B428B9ED3D77B9078659AD72FFF49DB415B543910A80088174A6E42D03734D69A9EFD21ABEFD70B689EFBA50A773382760E47EA3C8D23E4622B9EEFC562B1EED6438E97B8FADC3F2FB3CC7B2F29180622ECC6FF9D8D06ED2B30C759631E499DF23DFF4D20E40BFA12077FC2322CFC075A207DE18A62CEBAEEE65BF926208484A9E0831A89A935A5A27FC9F263E5422C67E588C7D3B9A6621580D3D31DB9C83324F8B794EB7F6DABEE0A4ADFF48330761FBCAD5211506D4FEED0F65A41D4FD8B1484043BD0EAE69DDD1EC8C61BE16A8A0B444035C722F50C884E7D9D4418120EB658D7353109DF3717AFE837BA2EBF094B91D3A3B898E748FA1EF6A9CF896423D72E805C09D433FFE67D60F622B36385F67371A7C347CDF9269D1777EEE4B61E9B9F5FACDC76C0966B10744BABEB7B741881E6993188223493E1396EBD73B9477D245CE721809F3BA7CB2D116D1CA9E1C2F44E07E615869E39F497D533B9B5D5487410C76987A4C84EB6C6073C04B0A416A3337DE117A628610F9B1B8CF8E8230A36245E08C83FB7B2BB71DFCBED9781EA978FF60318D80AC748FBB6AFC8AFC7E80287D52443A68DE1F2991DF9CE6F53D65DED0D548402CECFDF2CCF27CB2BDFA3E8955A94DAB260DA744D630EB52D004A74DC1DF0DF14D3BD2162909626919469A2CDC10C56A61
   pkey[1]: 010001
   keyid: 269E0DC4E3E4A5B8

# off=3038 ctb=b9 tag=14 hlen=3 plen=269
:public sub key packet:
   version 4, algo 1, created 1481611279, expires 0
   pkey[0]: A516862F512D2B1C36268DABBD9CD2844FF75F0EC24680840E9A5528475D3D2EB9F204514E4749ECAA74317D95C4A13B22AAB78DDF731CA07385B84F2252C0F505FE453401A31DD98EBB8936B3079C3A7D9EFD6E5A4ABF8E652ACC1A2A570582FD06F813A9DB52A6676C4057B7ED8304A6839FE14E7019A33C9A941866371A02234943DF2B81A43EA1A1EB51746FE466300939AE56E8DABC336FD92C4325DB105813C6B1C82786F1A3A8B5190A86A46C408F8A10F2617814F0DBEDB13AA6A3B3C6CE0550DACBEE2A1A0D398F2B5F547C1ABF69CB70CE47B381454335E12CE8478CBCB02C71452487DC9DF2CBDB16B76C84FDA7C787B14758F05B656753669D2F
   pkey[1]: 010001
   keyid: 17118623766D56F8

# off=3310 ctb=89 tag=2 hlen=3 plen=543
:signature packet: algo 1, keyid 269E0DC4E3E4A5B8
   version 4, created 1481611279, md5len 0, sigclass 0x18
   digest algo 8, begin of digest a2 63
   hashed subpkt 2 len 4 (sig created 2016-12-13)
   hashed subpkt 27 len 1 (key flags: 0C)
   subpkt 16 len 8 (issuer key ID 269E0DC4E3E4A5B8)
   data: 19B7A4FA200639CEC300C01097AB9DCAAE15422067E6A2DE75F0A9C6AFE9C8BE4A5FF110761C981ABDEC7AC5D974BFB7BDB820AC42BB1A237FF88A5856393D314D86BC0D22F882091661F62DBC9F2AF65AB27272BF7B1A87C031BE492E8C626709148086020FA8879F92E3CB2F2A3FA8DF22E6A24790F7C045BABCFEAD259620F673EED0AC31544E7C6A601073A7877F4D18D7D7D5805C8053AF16FD42617F6919BF59129AFEB9DB3DD41908C074FA30E6948C809E02FB06AC238E543C50CAC5489429E51569D2FE478A48DBEFAE62392221D7FE991C7A4D6CCDEA4FD3ADD3D8D430D580F638E6FAF2C779B5FE3A52848E71ECBD51C0031CFB7882BB9B6E977879E6AC77422071DD06005206D675345B84687D21094D8700717689601EC10756DB7199C7FA014E40CB992B23226BB850A56A44F46C5FF1B4E26FF77EA0306ABD6BD793ACF158553C27DA76DBB420B8E60E49ADB229ABB4B3EAA65D7223763EDC2ECDF8D551F03EF6358C5F24708EDECA15489D1AAFC9781937319966DB1EC05B380CABFAF3C2D618EAEC4068B89DD86D43B220C659D0708AC36451FC70532032AD1DB6907F0AE9F7D40DE1BD6FE1BE69C671BFF6EC833D34DCF211C1E57EFE510C6AD72B88D293CD7ABE6A8AA1F858F01220E743B4B8A68BCC39A04819B6B3828519242B676D372CFE85017D543F438D2F89EEEF74B23D083F15AFB0727E4CA7

"""






import hashlib
import hexdump

def to_hexdump(x, s=' '):
    return hexdump.dump(to_str(x), sep=s)

def to_str(x):
    return ''.join(x)

def read_binary_public_key():
    f = open('carlohamalainen-publickey-2016-12-23.bin', 'rb')
    xs = []
    try:
        byte = f.read(1)
        while byte != '':
            byte = f.read(1)
            xs.append(byte)
    finally:
        f.close()
    
    return xs

"""

First question: confirm the fingerprint of the subkey packet
that starts at offset 3038. The packet dump shows the expected
answer:

    keyid: 17118623766D56F8

"""

xs = read_binary_public_key()

SUBKEY_OFFSET = 3038
SUBKEY_PLEN   = 269

subkey_packet = xs[SUBKEY_OFFSET:SUBKEY_OFFSET+SUBKEY_PLEN+2] # +2 because the PLEN is short by one?

# Version 4 packet.
# https://tools.ietf.org/html/rfc4880#section-5.5.2
assert subkey_packet[2] == '\x04'

# Key IDs: https://tools.ietf.org/html/rfc4880#section-12.2
# 
#     A V4 fingerprint is the 160-bit SHA-1 hash of the octet 0x99,
#     followed by the two-octet packet length, followed by the entire
#     Public-Key packet starting with the version field.  The Key ID is the
#     low-order 64 bits of the fingerprint.

for_fingerprint = ['\x99'] + subkey_packet

h = hashlib.sha1(to_str(for_fingerprint))

print 'Subkey data (from 3038 packet dump):                               A5 16 86 2F ...'
print 'Start of subkey_packet:             ', to_hexdump(subkey_packet[:14], s=' '), '...'
print
print 'for_fingerprint:', hexdump.dump(to_str(for_fingerprint), sep=' ')

# The SHA1 digest is 20 bytes, so we get the low-order 64 bits (8 bytes)
# by dropping the first 20 - 8 == 12 bytes of the digest:
assert h.digest_size == 20
key_id = to_hexdump(h.digest()[12:], s='')

assert key_id == '17118623766D56F8'

print
print 'Computed Key ID matches what we expected:', key_id
print

"""

Second question: the packet at off=3038 defines the subkey 17118623766D56F8. Use
the subkey binding signature in the next packet (off=3310) to verify that the
key 17118623766D56F8 is attached to our main public key 269E0DC4E3E4A5B8.

The packet dump mentions

    digest algo 8, begin of digest a2 63

which means that algorithm 8 (SHA256) is used to create a digest
that has the left-most 16 bits A2 63.

First, let's pull out these left 16 bits by walking past the
hashed and unhashed parts of the signature.

"""

signature_block = xs[3310:3310+543+2]

# Starts off with two bytes for the length.
assert 543 == (ord(signature_block[0]) << 8) + ord(signature_block[1])

# Version 4 packet.
assert signature_block[2] == '\x04'

# This is a subkey binding signature.
assert signature_block[3] == '\x18'

print 'Subkey binding sig: public key algo:', '%02X' % ord(signature_block[4])
print 'Subkey binding sig: hash       algo:', '%02X' % ord(signature_block[5])

hash_subpacket_length = (ord(signature_block[6]) << 8) + ord(signature_block[7])
assert hash_subpacket_length == 9

start_of_hashed_part   = 8
start_of_unhashed_part = 7 + hash_subpacket_length + 1

unhash_subpacket_length = ( (ord(signature_block[start_of_unhashed_part]) << 8)
                          +  ord(signature_block[start_of_unhashed_part+1])
                          )

start_of_left_16 = start_of_unhashed_part + unhash_subpacket_length + 2

left_16 = signature_block[start_of_left_16:start_of_left_16+2]

assert left_16 == ['\xA2', '\x63']
print
print 'Left 16 bits of digest:', ('%02X %02X' % (ord(left_16[0]), ord(left_16[1])))
print

# Later we will need the hashed part of the signature for
# computing the final digest.

hashed_part_of_sig = signature_block[start_of_hashed_part:start_of_hashed_part+hash_subpacket_length]
assert len(hashed_part_of_sig) == hash_subpacket_length

print 'Hashed part of signature:', hexdump.dump(to_str(hashed_part_of_sig), sep=' ')
print '(Without this hashed part we can\'t compute the final digest.'
print ' I don\'t see this data in the output of'
print ' "gpg2 --list-packets --verbose" or pgpdump.)'
print

public_key_block = xs[0:0+525+2] # +2 for what? 
assert public_key_block[2] == '\x04'

"""

Next, we hash the public key, the subkey signing key,
a magic trailer, and we should get the left 16 bits that we
found before.

"""

header1     = ['\x99'] + public_key_block[:8]
pubkey_body = public_key_block[8:]

header2     = ['\x99'] + subkey_packet[:8]
subsig_body = subkey_packet[8:]

# Version, class, pub key algo, digest algo.
version_class_algos = [ '\x04', '\x18',
                        signature_block[4],
                        signature_block[5]
                      ]

m = hash_subpacket_length
assert m == 9
hash_chunk_length = [chr(m >> 8), chr(m)]

"""
According to https://tools.ietf.org/html/rfc4880#section-5.2.4
the final bit of data is a trailer of six octets:

   V4 signatures also hash in a final trailer of six octets: the
   version of the Signature packet, i.e., 0x04; 0xFF; and a four-octet,
   big-endian number that is the length of the hashed data from the
   Signature packet (note that this number does not include these final
   six octets).

But in gnupg-2.1.11, we see the following in g10/sig-check.c:

410     else {
411     byte buf[6];
412     size_t n;
413     gcry_md_putc( digest, sig->pubkey_algo );
414     gcry_md_putc( digest, sig->digest_algo );
415     if( sig->hashed ) {
416         n = sig->hashed->len;
417             gcry_md_putc (digest, (n >> 8) );
418             gcry_md_putc (digest,  n       );
419         gcry_md_write (digest, sig->hashed->data, n);
420         n += 6;
421     }
422     else {
423       /* Two octets for the (empty) length of the hashed
424              section. */
425           gcry_md_putc (digest, 0);
426       gcry_md_putc (digest, 0);
427       n = 6;
428     }
429     /* add some magic per Section 5.2.4 of RFC 4880.  */
430     buf[0] = sig->version;
431     buf[1] = 0xff;
432     buf[2] = n >> 24;
433     buf[3] = n >> 16;
434     buf[4] = n >>  8;
435     buf[5] = n;
436     gcry_md_write( digest, buf, 6 );
437     }
438     gcry_md_final( digest );

Line 420 adds 6, so we'll do the same even though it
seems to disagree with the RFC.

"""

n = m + 6
assert n == 15

magic = ['\x04', '\xff',
         chr(n >> 24),
         chr(n >> 16),
         chr(n >>  8),
         chr(n)]

for_digest = []

for_digest += header1
for_digest += pubkey_body

for_digest += header2
for_digest += subsig_body 

for_digest += version_class_algos 

for_digest += hash_chunk_length 
for_digest += hashed_part_of_sig

for_digest += magic

# According to https://tools.ietf.org/html/rfc4880#section-9.4
# the hash algo 8 is SHA256.
assert '\x08' == signature_block[5]
digest = hashlib.sha256(to_str(for_digest)).digest()

assert 'A2 63' == to_hexdump(digest[:2])
print 'Digest begins with "A2 63" as expected.'
print
print 'Full digest:', to_hexdump(digest)
print
print 'To complete the verification, we need to convert'
print 'the digest to an MPI and then verify it using the'
print 'public key algorithm.'
print

"""

gnupg-2.1.11/g10/sig-check.c:

440     /* Convert the digest to an MPI.  */
441     result = encode_md_value (pk, digest, sig->digest_algo );
442     if (!result)
443         return GPG_ERR_GENERAL;
444 
445     /* Verify the signature.  */
446     rc = pk_verify( pk->pubkey_algo, result, sig->data, pk->pkey );



"""
