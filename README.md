# eglot-signature.el (WIP)

Signature support for Eglot, displaying LSP signature information at a given cursor position.

TODO: https://www.google.com/search?smstk=ChhoVHF2K0pTTXRVNWZJcUIvdWIrL25Gcz0QAQ%3D%3D&smstidx=3&q=%E4%BD%A0%E6%98%AF%E4%B8%80%E4%BD%8Demacs%E4%B8%93%E5%AE%B6%EF%BC%8C%E5%AE%9E%E7%8E%B0%E8%BF%99%E4%B9%88%E4%B8%80%E4%B8%AA%E5%8A%9F%E8%83%BD%EF%BC%9A%0Astate+S1+-%3E+call+function+A+-%3E+state+S2+%26+some+key+bindings+-%3E+call+function+B+-%3E+state+S1+%26+restore+key+bindings%0A%0A%E4%B8%8D%E8%A6%81%E7%94%A8overlay&udm=50&csuir=1&aep=34&kgs=2eefe03e4a2aac22&shndl=37&shmd=H4sIAAAAAAAA_3WQv0vDQBzFKW75E5xuchDSGAcHEUH_BcE1XJPzGkzuwt3V2k2pYCxWEVvRKq0uUh2i2FJSbHT378ivrf-CCTgoxeXL4z0-PL5PGs5J2_HdSfz2kN2o0Y0H7dR74kjIgkHCTUSEbEMn9T6SwEs6R5kO_YPkdpi0enHbnU5OM50eD2L3IvTfo0Y_Or-KR27k1efXykI4fFVRqtVqEXMBhakXdWorHEGml2WHUZsq0LQ1XoYMaaJcsUsEmlbRIXhxXAiD-_j6JWsLgyayoc5D_zLyRlll5HWTs9f08yYcu3nuP0eNXloPppOOlPcgsKUCeR3o0LLAToXowqQEbOTWT7wMFgCnNgK7qAZKJjFMgvkssvkLUTOEIS4o-0tJUug308fDpNWne4hZsPZV0P573bQhRlwpZdvmsIIpxRbCGmbQyLdW1P0ZT8t4YkBmaOrKkuHk63wDta8K9bcBAAA&shmds=v1_ATWGeeNjPOCQrsRoIyTBtkGs4lVjgoQsdAUoMkMiIhfVQlEjkA&source=sh/x/aim/m1/1

(add-hook 'eglot-server-initialized-hook 'eglot-signature-mode)
(remove-hook 'eglot-server-initialized-hook 'eglot-signature-mode)
