// @flow



function foo(): typeof AA { return AA }

const AA = {a: 'hello', b: [1, 2, 3]}


let bar = foo()
let a: string = bar.a


const b = bar.b.map(e => e)

export default {a, b}
