Export my public key, then dump the packets:

    $ gpg2 -a --export carlo@carlo-hamalainen.net | gpg2 --list-packets --verbose

    # off=0 ctb=99 tag=6 hlen=3 plen=525
    :public key packet:
        version 4, algo 1, created 1373439395, expires 0
        pkey[0]: C35E579D722847E70515382572B28200 (very long line, snipped)
        pkey[1]: 010001
        keyid: 269E0DC4E3E4A5B8
    # off=528 ctb=b4 tag=13 hlen=2 plen=45
    :user ID packet: "Carlo Hamalainen <carlo@carlo-hamalainen.net>"
    # off=575 ctb=89 tag=2 hlen=3 plen=568
    :signature packet: algo 1, keyid 269E0DC4E3E4A5B8
        version 4, created 1373439395, md5len 0, sigclass 0x13
        digest algo 2, begin of digest 05 44
        hashed subpkt 2 len 4 (sig created 2013-07-10)
        hashed subpkt 27 len 1 (key flags: 03)
        hashed subpkt 11 len 5 (pref-sym-algos: 9 8 7 3 2)
        hashed subpkt 21 len 5 (pref-hash-algos: 8 2 9 10 11)
        hashed subpkt 22 len 3 (pref-zip-algos: 2 3 1)
        hashed subpkt 30 len 1 (features: 01)
        hashed subpkt 23 len 1 (key server preferences: 80)
        subpkt 16 len 8 (issuer key ID 269E0DC4E3E4A5B8)
        data: REDACTED001
    # off=1146 ctb=b9 tag=14 hlen=3 plen=525
    :public sub key packet:
        version 4, algo 1, created 1373439395, expires 0
        pkey[0]: REDACTED002
        pkey[1]: 010001
        keyid: EF86D47281E07A3C
    # off=1674 ctb=89 tag=2 hlen=3 plen=543
    :signature packet: algo 1, keyid 269E0DC4E3E4A5B8
        version 4, created 1373439395, md5len 0, sigclass 0x18
        digest algo 2, begin of digest b9 63
        hashed subpkt 2 len 4 (sig created 2013-07-10)
        hashed subpkt 27 len 1 (key flags: 0C)
        subpkt 16 len 8 (issuer key ID 269E0DC4E3E4A5B8)
        data: REDACTED003
    # off=2220 ctb=b9 tag=14 hlen=3 plen=269
    :public sub key packet:
        version 4, algo 1, created 1479618635, expires 0
        pkey[0]: REDACTED004
        pkey[1]: 010001
        keyid: 6D682AD2BE8897FA
    # off=2492 ctb=89 tag=2 hlen=3 plen=543
    :signature packet: algo 1, keyid 269E0DC4E3E4A5B8
        version 4, created 1479618635, md5len 0, sigclass 0x18
        digest algo 8, begin of digest bc 2c
        hashed subpkt 2 len 4 (sig created 2016-11-20)
        hashed subpkt 27 len 1 (key flags: 0C)
        subpkt 16 len 8 (issuer key ID 269E0DC4E3E4A5B8)
        data: REDACTED005
    # off=3038 ctb=b9 tag=14 hlen=3 plen=269
    :public sub key packet:
        version 4, algo 1, created 1481611279, expires 0
        pkey[0]: REDACTED006
        pkey[1]: 010001
        keyid: 17118623766D56F8
    # off=3310 ctb=89 tag=2 hlen=3 plen=543
    :signature packet: algo 1, keyid 269E0DC4E3E4A5B8
        version 4, created 1481611279, md5len 0, sigclass 0x18
        digest algo 8, begin of digest a2 63
        hashed subpkt 2 len 4 (sig created 2016-12-13)
        hashed subpkt 27 len 1 (key flags: 0C)
        subpkt 16 len 8 (issuer key ID 269E0DC4E3E4A5B8)
        data: REDACTED007

For the block at offset ```3038``` we see the Key ID of ```17118623766D56F8```:

    # off=3038 ctb=b9 tag=14 hlen=3 plen=269
    :public sub key packet:
        version 4, algo 1, created 1481611279, expires 0
        pkey[0]: REDACTED006
        pkey[1]: 010001
        keyid: 17118623766D56F8

Alternatively, using ```pgpdump```:

	$ gpg2 -a --export carlo@carlo-hamalainen.net | pgpdump 

	Old: Public Key Packet(tag 6)(525 bytes)
		Ver 4 - new
		Public key creation time - Wed Jul 10 16:56:35 AEST 2013
		Pub alg - RSA Encrypt or Sign(pub 1)
		RSA n(4096 bits) - ...
		RSA e(17 bits) - ...
	Old: User ID Packet(tag 13)(45 bytes)
		User ID - Carlo Hamalainen <carlo@carlo-hamalainen.net>
	Old: Signature Packet(tag 2)(568 bytes)
		Ver 4 - new
		Sig type - Positive certification of a User ID and Public Key packet(0x13).
		Pub alg - RSA Encrypt or Sign(pub 1)
		Hash alg - SHA1(hash 2)
		Hashed Sub: signature creation time(sub 2)(4 bytes)
			Time - Wed Jul 10 16:56:35 AEST 2013
		Hashed Sub: key flags(sub 27)(1 bytes)
			Flag - This key may be used to certify other keys
			Flag - This key may be used to sign data
		Hashed Sub: preferred symmetric algorithms(sub 11)(5 bytes)
			Sym alg - AES with 256-bit key(sym 9)
			Sym alg - AES with 192-bit key(sym 8)
			Sym alg - AES with 128-bit key(sym 7)
			Sym alg - CAST5(sym 3)
			Sym alg - Triple-DES(sym 2)
		Hashed Sub: preferred hash algorithms(sub 21)(5 bytes)
			Hash alg - SHA256(hash 8)
			Hash alg - SHA1(hash 2)
			Hash alg - SHA384(hash 9)
			Hash alg - SHA512(hash 10)
			Hash alg - SHA224(hash 11)
		Hashed Sub: preferred compression algorithms(sub 22)(3 bytes)
			Comp alg - ZLIB <RFC1950>(comp 2)
			Comp alg - BZip2(comp 3)
			Comp alg - ZIP <RFC1951>(comp 1)
		Hashed Sub: features(sub 30)(1 bytes)
			Flag - Modification detection (packets 18 and 19)
		Hashed Sub: key server preferences(sub 23)(1 bytes)
			Flag - No-modify
		Sub: issuer key ID(sub 16)(8 bytes)
			Key ID - 0x269E0DC4E3E4A5B8
		Hash left 2 bytes - 05 44 
		RSA m^d mod n(4095 bits) - ...
			-> PKCS-1
	Old: Public Subkey Packet(tag 14)(525 bytes)
		Ver 4 - new
		Public key creation time - Wed Jul 10 16:56:35 AEST 2013
		Pub alg - RSA Encrypt or Sign(pub 1)
		RSA n(4096 bits) - ...
		RSA e(17 bits) - ...
	Old: Signature Packet(tag 2)(543 bytes)
		Ver 4 - new
		Sig type - Subkey Binding Signature(0x18).
		Pub alg - RSA Encrypt or Sign(pub 1)
		Hash alg - SHA1(hash 2)
		Hashed Sub: signature creation time(sub 2)(4 bytes)
			Time - Wed Jul 10 16:56:35 AEST 2013
		Hashed Sub: key flags(sub 27)(1 bytes)
			Flag - This key may be used to encrypt communications
			Flag - This key may be used to encrypt storage
		Sub: issuer key ID(sub 16)(8 bytes)
			Key ID - 0x269E0DC4E3E4A5B8
		Hash left 2 bytes - b9 63 
		RSA m^d mod n(4095 bits) - ...
			-> PKCS-1
	Old: Public Subkey Packet(tag 14)(269 bytes)
		Ver 4 - new
		Public key creation time - Sun Nov 20 15:10:35 AEST 2016
		Pub alg - RSA Encrypt or Sign(pub 1)
		RSA n(2048 bits) - ...
		RSA e(17 bits) - ...
	Old: Signature Packet(tag 2)(543 bytes)
		Ver 4 - new
		Sig type - Subkey Binding Signature(0x18).
		Pub alg - RSA Encrypt or Sign(pub 1)
		Hash alg - SHA256(hash 8)
		Hashed Sub: signature creation time(sub 2)(4 bytes)
			Time - Sun Nov 20 15:10:35 AEST 2016
		Hashed Sub: key flags(sub 27)(1 bytes)
			Flag - This key may be used to encrypt communications
			Flag - This key may be used to encrypt storage
		Sub: issuer key ID(sub 16)(8 bytes)
			Key ID - 0x269E0DC4E3E4A5B8
		Hash left 2 bytes - bc 2c 
		RSA m^d mod n(4096 bits) - ...
			-> PKCS-1
	Old: Public Subkey Packet(tag 14)(269 bytes)
		Ver 4 - new
		Public key creation time - Tue Dec 13 16:41:19 AEST 2016
		Pub alg - RSA Encrypt or Sign(pub 1)
		RSA n(2048 bits) - ...
		RSA e(17 bits) - ...
	Old: Signature Packet(tag 2)(543 bytes)
		Ver 4 - new
		Sig type - Subkey Binding Signature(0x18).
		Pub alg - RSA Encrypt or Sign(pub 1)
		Hash alg - SHA256(hash 8)
		Hashed Sub: signature creation time(sub 2)(4 bytes)
			Time - Tue Dec 13 16:41:19 AEST 2016
		Hashed Sub: key flags(sub 27)(1 bytes)
			Flag - This key may be used to encrypt communications
			Flag - This key may be used to encrypt storage
		Sub: issuer key ID(sub 16)(8 bytes)
			Key ID - 0x269E0DC4E3E4A5B8
		Hash left 2 bytes - a2 63 
		RSA m^d mod n(4093 bits) - ...
			-> PKCS-1

I noticed that this block doesn't have the Key ID ```17118623766D56F8```:

	Old: Public Subkey Packet(tag 14)(269 bytes)
		Ver 4 - new
		Public key creation time - Tue Dec 13 16:41:19 AEST 2016
		Pub alg - RSA Encrypt or Sign(pub 1)
		RSA n(2048 bits) - ...
		RSA e(17 bits) - ...

Turns out the Key ID for this packet has to be computed on the fly.

The [RFC Section 12.2](https://tools.ietf.org/html/rfc4880#section-12.2) specifies
how to compute the Key ID for a Version 4 packet:

	A V4 fingerprint is the 160-bit SHA-1 hash of the octet 0x99,
	followed by the two-octet packet length, followed by the entire
	Public-Key packet starting with the version field.  The Key ID is the
	low-order 64 bits of the fingerprint.

See [calculate_key_id.py](calculate_key_id.py) for how to calculate this.

    $ python calculate_key_id.py 
    Subkey data (from 3038 packet dump):                               A5 16 86 2F ...
    Start of subkey_packet:              01 0D 04 58 4F 98 0F 01 08 00 A5 16 86 2F ...

    for_fingerprint: 99 01 0D 04 58 4F 98 0F 01 08 00 A5 16 86 2F 51 2D 2B 1C 36 26 8D AB BD 9C D2 84 4F F7 5F 0E C2 46 80 84 0E 9A 55 28 47 5D 3D 2E B9 F2 04 51 4E 47 49 EC AA 74 31 7D 95 C4 A1 3B 22 AA B7 8D DF 73 1C A0 73 85 B8 4F 22 52 C0 F5 05 FE 45 34 01 A3 1D D9 8E BB 89 36 B3 07 9C 3A 7D 9E FD 6E 5A 4A BF 8E 65 2A CC 1A 2A 57 05 82 FD 06 F8 13 A9 DB 52 A6 67 6C 40 57 B7 ED 83 04 A6 83 9F E1 4E 70 19 A3 3C 9A 94 18 66 37 1A 02 23 49 43 DF 2B 81 A4 3E A1 A1 EB 51 74 6F E4 66 30 09 39 AE 56 E8 DA BC 33 6F D9 2C 43 25 DB 10 58 13 C6 B1 C8 27 86 F1 A3 A8 B5 19 0A 86 A4 6C 40 8F 8A 10 F2 61 78 14 F0 DB ED B1 3A A6 A3 B3 C6 CE 05 50 DA CB EE 2A 1A 0D 39 8F 2B 5F 54 7C 1A BF 69 CB 70 CE 47 B3 81 45 43 35 E1 2C E8 47 8C BC B0 2C 71 45 24 87 DC 9D F2 CB DB 16 B7 6C 84 FD A7 C7 87 B1 47 58 F0 5B 65 67 53 66 9D 2F 00 11 01 00 01

    Computed Key ID matches what we expected: 17118623766D56F8

    Subkey binding sig: public key algo: 01
    Subkey binding sig: hash       algo: 08

    Left 16 bits of digest: A2 63

    Hashed part of signature: 05 02 58 4F 98 0F 02 1B 0C
    (Without this hashed part we can't compute the final digest.
     I don't see this data in the output of
     "gpg2 --list-packets --verbose" or pgpdump.)

    Digest begins with "A2 63" as expected.

    Full digest: A2 63 1B 7B 8F FE 87 C4 6E 77 97 95 5B 3C 2E CE 71 55 E6 5D 8F 12 2B 05 62 B6 71 96 A9 0D A0 6C

    To complete the verification, we need to convert
    the digest to an MPI and then verify it using the
    public key algorithm.
