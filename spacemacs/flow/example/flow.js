// @flow

import React from 'react'
import zzz, {
    C,
} from './extra.js'

interface Serializable {
    serialize(): string;
}

class Foo {
    serialize() { return '[Foo]'; }
}

class Bar {
    serialize() { return '[Bar]'; }
}

const b = new Bar()
const f = (a = true) => false

let foo: Array<{
    a: number,
    a1: number,
    a2: number,
    a3: number,
    a4: number,
    a5: number,
    a6: number,
    a7: number,
    a8: number,
    b: () => void
}>

foo = false
console.log('yo')
console.log('yo')
console.log('yo')
console.log('yo')
console.log('yo')
console.log('yo')
console.log('yo')
console.log('yo')
console.log('yo')
console.log('yo')
console.log('yo')

type F$C<T> = T

let A = {
    foo: 1,
    bar: true,
}

const hey = props => <div>{ 1 + 1}</div>;
let _sleeping = zzz

const sleeping: string = {
    a: zzz
}

let foooo = (a) => console.log(a)
foooo("1")

console.log()

let c = new C()
