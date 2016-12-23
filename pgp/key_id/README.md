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
    block: 010D04584F980F010800A516862F512D2B1C36268DABBD9CD2844FF75F0EC24680840E9A5528475D3D2EB9F204514E4749ECAA74317D95C4A13B22AAB78DDF731CA07385B84F2252C0F505FE453401A31DD98EBB8936B3079C3A7D9EFD6E5A4ABF8E652ACC1A2A570582FD06F813A9DB52A6676C4057B7ED8304A6839FE14E7019A33C9A941866371A02234943DF2B81A43EA1A1EB51746FE466300939AE56E8DABC336FD92C4325DB105813C6B1C82786F1A3A8B5190A86A46C408F8A10F2617814F0DBEDB13AA6A3B3C6CE0550DACBEE2A1A0D398F2B5F547C1ABF69CB70CE47B381454335E12CE8478CBCB02C71452487DC9DF2CBDB16B76C84FDA7C787B14758F05B656753669D2F0011010001

    for_fingerprint: 99010D04584F980F010800A516862F512D2B1C36268DABBD9CD2844FF75F0EC24680840E9A5528475D3D2EB9F204514E4749ECAA74317D95C4A13B22AAB78DDF731CA07385B84F2252C0F505FE453401A31DD98EBB8936B3079C3A7D9EFD6E5A4ABF8E652ACC1A2A570582FD06F813A9DB52A6676C4057B7ED8304A6839FE14E7019A33C9A941866371A02234943DF2B81A43EA1A1EB51746FE466300939AE56E8DABC336FD92C4325DB105813C6B1C82786F1A3A8B5190A86A46C408F8A10F2617814F0DBEDB13AA6A3B3C6CE0550DACBEE2A1A0D398F2B5F547C1ABF69CB70CE47B381454335E12CE8478CBCB02C71452487DC9DF2CBDB16B76C84FDA7C787B14758F05B656753669D2F0011010001

    Key ID: 17118623766D56F8
