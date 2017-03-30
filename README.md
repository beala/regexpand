# regexpand

## Install

```
$ stack install
```

## Examples
```
$ regexpand "(\([:num:]{3}\)-[:num:]{3}-[:num:]{4} ){2}" | head -10
(000)-000-0000 (000)-000-0000 
(000)-000-0000 (000)-000-0001 
(000)-000-0000 (000)-000-0002 
(000)-000-0000 (000)-000-0003 
(000)-000-0000 (000)-000-0004 
(000)-000-0000 (000)-000-0005 
(000)-000-0000 (000)-000-0006 
(000)-000-0000 (000)-000-0007 
(000)-000-0000 (000)-000-0008 
(000)-000-0000 (000)-000-0009
```

```
$ regexpand "(a|b)*c+"
c
cc
ac
acc
bc
bcc
aac
aacc
abc
abcc
bac
bacc
bbc
bbcc
aaac
aaacc
aabc
aabcc
abac
abacc
abbc
abbcc
baac
baacc
babc
babcc
bbac
bbacc
bbbc
bbbcc

```
