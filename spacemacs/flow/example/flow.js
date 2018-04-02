// @flow



const AA = {a: 'hello', b: [1, 2, 3]}
function foo(): typeof AA { return AA }

let bar = foo()
const b = bar.b.map(e => e)

let a: string = bar.b

const f = (x: number, y: number): number => x * y
f(1, 2)

type Test<RS> = Array<RS>
let what: Test<string> = ["hello"]
type Test2 = (Test<number> & Array<Array<string>>) | boolean
type Test3 = Test2 | number
type Test4 = [number, string, Array<string>]

let what2: Test2

interface Serializable {
    serialize(): string;
}

class Foo implements Serializable {
    serialize() { return '[Foo]' }
}

class Bar implements Serializable {
    serialize() { return 'Bar' }
}

const yay1 = new Foo()
const yay = new Bar()

let aaaa: typeof yay1

let val = (value: number);
let obj = { prop: (value: Type) };
let arr = ([(value: Type), (value: Type)]: Array<Type>);


export default {a, b}
