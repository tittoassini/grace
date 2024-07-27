/*
    The runtime of the QQ compiler.

    not = {F -> T, T -> F}
    o = not F
*/
export function not(x) {
    // const f = { F: "T", T: "F" };
    // return f[x];
    // if (x === "F") return "T";
    // if (x === "T") return "F";
    switch (x) {
        case "F": return "T";
        case "T": return "F";
        default: throw "unhandled case" + x;
    }
}

/*
{
    1 -> 1
    2 -> 2
    n -> n*2
}
*/
op = (x) => {
    if (x === 1) return 1;
    if (x === 2) return 2;
    return ((n) => n * 2)(x);
}

const op1 = op(5);

/*
    inc = \x -> x+1
*/
const inc = (x) => x + 1;
/*
len = {Nil -> 0, Cons _ t -> 1 + len t}

Nil = ["Nil"]
*/
function len(l) {
    if (l[0] === "Nil") return 0;
    if (l[0] === "Cons") return 1 + len(l[2]);
}

const l2 = len(["Cons", "a", ["Cons", "b", ["Nil"]]]);

function addIntInt(n2, n1) { return n1 + n2; }

function minusIntInt(n1,n2) { return n1 - n2; }

const o = not("F")

const r = 1


