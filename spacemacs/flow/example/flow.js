// @flow



function foo(): typeof AA { return AA }

const AA = {a: 'hello', b: [1, 2, 3]}





let bar = foo()
const b = bar.b.map(e => e)

let a: string = bar.a

export default {a, b}
