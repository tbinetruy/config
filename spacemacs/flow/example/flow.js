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

const b = new Bar()
