// @flow



const AA = {a: 'hello', b: [1, 2, 3]}
function foo(): typeof AA { return AA }

let bar = foo()
const b = bar.b.map(e => e)

let a: string = bar.a

const f = (x: number, y: number): number => x * y
f(1, 2)

export default {a, b}
