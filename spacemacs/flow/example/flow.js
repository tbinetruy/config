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
type Test2 = Test & Array<string>

what

let what2: Test2

export default {a, b}
