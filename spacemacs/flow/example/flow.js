// @flow

interface Serializable {
    serialize(): string;
}

class Foo {
    serialize() { return '[Foo]'; }
}

class Bar {
    serialize() { return '[Bar]'; }
}

const foo: Serializable = new Foo(); // Works!
const bar: Serializable = new Bar(); // Works!

type Type1<A,B,C> = Array<[number, A]>
let fooz: Type1

var obj: { [user_id: number]: string } = {};
obj[1] = "Julia";
obj[2] = "Camille";
obj[3] = "Justin";
obj[4] = "Mark";

class A { constructor() { }}

type genFunc = <T>(param: T) => T
let Func: genFunc
let ff = <T>(a: T) => {}
ff

function fff<T>(a: T): T { return a }
fff

type D = typeof fff
let d:D


opaque type StringAlias = string;
opaque type ObjectAlias = {
    property: string,
    method(): number,
};
opaque type UnionAlias = 1 | 2 | 3;
opaque type AliasAlias: ObjectAlias = ObjectAlias;
opaque type VeryOpaque: AliasAlias = ObjectAlias;

let aa: VeryOpaque

