#!/usr/bin/env python3

import sys

ops = {
    "nop": 0x00,
    "vtaskdelay": 0x01,
    "iprint": 0x04,
    "fprint": 0x05,
    "ftoi": 0x06,
    "itof": 0x07,
    "iadd": 0x08,
    "isub": 0x09,
    "imul": 0x0a,
    "idiv": 0x0b,
    "fadd": 0x0c,
    "fsub": 0x0d,
    "fmul": 0x0e,
    "fdiv": 0x0f,
    "pop": 0x10,
    "swap": 0x11,
    "dup": 0x19,
    "and": 0x1a,
    "or": 0x1b,
    "xor": 0x1c,
    "not": 0x1d
}

branches = {
    "beq": 0x12,
    "bgt": 0x13,
    "blt": 0x14,
    "bge": 0x15,
    "ble": 0x16,
    "bne": 0x17,
    "jmp": 0x18
}

numbytes = {
    "nop": 0x01,
    "vtaskdelay": 0x01,
    "pushw": 0x03,
    "push": 0x03,
    "iprint": 0x01,
    "fprint": 0x01,
    "ftoi": 0x01,
    "itof": 0x01,
    "iadd": 0x01,
    "isub": 0x01,
    "imul": 0x01,
    "idiv": 0x01,
    "fadd": 0x01,
    "fsub": 0x01,
    "fmul": 0x01,
    "fdiv": 0x01,
    "pop": 0x01,
    "swap": 0x01,
    "dup": 0x01,
    "and": 0x01,
    "or": 0x01,
    "xor": 0x01,
    "not": 0x01,
    "beq": 0x2,
    "bgt": 0x2,
    "ble": 0x2,
    "bne": 0x2,
    "bge": 0x2,
    "blt": 0x2,
    "jmp": 0x2
}

def parse(x):
    k = 0
    i = 0
    out = []
    labels = {}
    for k, word in enumerate(x):
        if(word[-1] == ':'):
            labels[word] = i
        else:
            i+=numbytes.get(word, 0)
    for i,v in enumerate(x):
        if(v[-1] == ":"):
            x.pop(i)
    i = 0
    k=0
    while(i < len(x)):
        keyword = x[i]
        if(keyword == "push"):
            out.append(0x03)
            i+=1
            r = parseint(x[i])
            out.append(r & 0xff)
            out.append((r & 0xff00) >> 8)
            k+=3
        elif(keyword == "pushw"):
            out.append(0x02)
            i+=1
            k+=3
            r = parseint(x[i])
            out.append(r & 0xff)
            out.append((r & 0xff00) >> 8)
            out.append((r & 0xff0000) >> 16)
            out.append((r & 0xff000000) >> 24)
        elif(keyword in ["beq", "bgt", "ble", "bne", "bge", "blt", "jmp"]):
            k+=3
            out.append(branches[keyword])
            i+=1
            if(k - labels[x[i] + ":"] >= 0):
                off = 0xffff - (k - labels[x[i] + ":"]) + 1
                out.append(off & 0xff)
                out.append((off & 0xff00) >> 8)
            else:
                off = labels[x[i] + ":"] - k + 1
                out.append(off & 0xff)
                out.append((off & 0xff00) >> 8)
        elif(keyword == "store"):
            out.append(0x1e)
            i+=1
            out.append(parseint(x[i]) & 0xff)
        elif(keyword == "load"):
            out.append(0x1f)
            i+=1
            out.append(parseint(x[i]) & 0xff)
        else:
            out.append(ops[keyword])
            k+=1
        i = i + 1
    return out

def parseint(num):
    return int(num, 16) if num[0:2]=="0x" else int(num)

if __name__ == '__main__':
    if(len(sys.argv) != 3 and len(sys.argv) != 2):
        print("usage: ./asmstack [file.asm] [?outputfile]")
        exit(1)
    inf = open(sys.argv[1], 'r')
    fl = inf.read()
    inf.close()
    p = parse(fl.split())
    outf = open(sys.argv[2] if len(sys.argv) == 3 else sys.argv[1] + ".out", 'wb')
    outf.write(bytes(p))
    outf.close()

