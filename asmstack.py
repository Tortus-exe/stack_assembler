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
    "not": 0x1d,
    "phpc": 0x20,
    "wrpc": 0x21,
    "phsp": 0x22,
    "wrsp": 0x23,
    "jsrs": 0x25,
    "pusha": 0x26,
    "halt": 0xff
}

branches = {
    "beq": 0x12,
    "bgt": 0x13,
    "blt": 0x14,
    "bge": 0x15,
    "ble": 0x16,
    "bne": 0x17,
    "jmp": 0x18,
    "jsr": 0x24
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
    "beq": 0x3,
    "bgt": 0x3,
    "ble": 0x3,
    "bne": 0x3,
    "bge": 0x3,
    "blt": 0x3,
    "jmp": 0x3,
    "load": 0x2,
    "store": 0x2,
    "phpc": 0x1,
    "wrpc": 0x1,
    "phsp": 0x1,
    "wrsp": 0x1, 
    "jsr": 0x3,
    "jsrs": 0x1,
    "halt": 0x1,
    "pusha": 0x1
}

def resolveLabels(x, labels):
    i = 0
    for n, word in enumerate(x):
        if(word[-1] == ':'):
            labels[word] = i
        elif(word == ".db"):
            r = 1
            while(x[n+r] not in numbytes.keys() and x[n+r][-1] != ':'):
                stuff = [ch.replace(" ", "") for ch in x[n+r].split(",")]
                length = len([ch for ch in stuff if ch != ""])
                i += length
                r += 1
                print(x[n+r] + ": len " + str(length) + " so i " + str(i))
        else:
            i+=numbytes.get(word, 0)
    for i,v in enumerate(x):
        if(v[-1] == ":"):
            x.pop(i)

def parse(x):
    k = 0
    out = []
    labels = {}
    resolveLabels(x, labels)
    print(x)
    # print(labels.items())
    i = 0
    byteOffset=0
    while(i < len(x)):
        keyword = x[i]
        if(keyword == "push"):
            out.append(0x03)
            i+=1
            r = parseint(x[i], labels)
            out.append(r & 0xff)
            out.append((r & 0xff00) >> 8)
            # print("push at " + str(byteOffset))
            byteOffset+=3
        elif(keyword == "pushw"):
            out.append(0x02)
            i+=1
            r = parseint(x[i], labels)
            out.append(r & 0xff)
            out.append((r & 0xff00) >> 8)
            out.append((r & 0xff0000) >> 16)
            out.append((r & 0xff000000) >> 24)
            # print("pushw at " + str(byteOffset))
            byteOffset+=3
        elif(keyword in branches.keys()):
            out.append(branches[keyword])
            i+=1
            byteOffset+=3
            if(byteOffset - labels[x[i] + ":"] >= 0):
                # print("branch backward at " + str(byteOffset))
                off = 0xffff - (byteOffset - labels[x[i] + ":"]) + 1
                out.append(off & 0xff)
                out.append((off & 0xff00) >> 8)
            else:
                # print("branch forward at " + str(byteOffset))
                off = labels[x[i] + ":"] - byteOffset
                out.append(off & 0xff)
                out.append((off & 0xff00) >> 8)
        elif(keyword == "store"):
            i+=1
            out.append(0x1e)
            out.append(parseint(x[i], labels) & 0xff)
            # print("store at " + str(byteOffset))
            byteOffset += 2
        elif(keyword == "load"):
            i+=1
            out.append(0x1f)
            out.append(parseint(x[i], labels) & 0xff)
            # print("load at " + str(byteOffset))
            byteOffset += 2
        elif(keyword == ".db"):
            i+=1
            while(x[i] not in numbytes.keys()):
                out.extend(defineBytes(x[i], labels))
                i+=1
            i-=1
        else:
            # print(keyword + " at " + str(byteOffset))
            out.append(ops[keyword])
            byteOffset+=1
        i = i + 1
    return out

def defineBytes(nums, labels):
    split = [a.replace(" ", "") for a in nums.split(",")]
    return [parseint(a, labels) for a in split if a != ""]

def parseint(num, labels):
    if(num[0:2]=="0x"):
        return int(num, 16)
    elif(num.isnumeric()):
        return int(num)
    elif(num[0:2] == ">>"):
        return labels[num + ":"] & 0xff
    elif(num[0:2] == "<<"):
        return (labels[num + ":"] & 0xff000000) >> 24
    elif(num[0] == "<"):
        return (labels[num + ":"] & 0xff0000) >> 16
    elif(num[0] == ">"):
        return (labels[num + ":"] & 0xff0000) >> 8
    else:
        return labels[num + ":"]

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
