/*
 * Copyright 2020 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


mod RoseTree {

    use RoseTree.Tree
    use RoseTree.Tree.{Node}

    pub type alias Forest[a] = List[Tree[a]]

    pub enum Tree[a] {
        case Node(a, List[Tree[a]])
    }

    instance Eq[Tree[a]] with Eq[a] {
        pub def eq(t1: Tree[a], t2: Tree[a]): Bool = equals(t1, t2)
    }

    instance Functor[Tree] {
        pub def map(f: a -> b \ ef, t: Tree[a]): Tree[b] \ ef = map(f, t)
    }

    instance Applicative[Tree] {
        pub def point(x: a): Tree[a] = singleton(x)
        pub def ap(f: Tree[a -> b \ ef], x: Tree[a]): Tree[b] \ ef = ap(f, x)
    }

    instance Monad[Tree] {
        pub def flatMap(f: a -> Tree[b] \ ef, x: Tree[a]): Tree[b] \ ef = flatMap(f, x)
    }




    pub def singleton(x: a): Tree[a] = Node(x, Nil)

    ///
    /// Returns `true` if and only if `c1` and `c2` and equal.
    ///
    pub def equals(t1: Tree[a], t2: Tree[a]): Bool with Eq[a] = 
        def loop(xs, ys) = match (xs, ys) {
            case (Nil, Nil)          => true
            case (a :: rs1, b:: rs2) => if (a == b) loop(rs1, rs2) else false
            case _                   => false
        };
        match (t1, t2) {
            case (Node(a, xs), Node(b, ys)) => if (a == b) loop(xs, ys) else false
        }



    pub def ap(f: Tree[a -> b \ ef], t: Tree[a]): Tree[b] \ ef =
        apHelper(f, t, eidentity)

    def apHelper(f: Tree[a -> b \ ef], t: Tree[a], k: Tree[b] -> Tree[b] \ ef): Tree[b] \ ef =
        let Node(f1, fs) = f;
        let Node(x1, ts) = t;
        apHelperList1(f1, ts, ks1 ->
            apHelperList2(t, fs, ks2 -> k(Node(f1(x1), ks1 ::: ks2))))

    def apHelperList1(f1: a -> b \ ef, ts: List[Tree[a]], k: List[Tree[b]] -> Tree[b] \ ef): Tree[b] \ ef =
        match ts {
            case Nil => k(Nil)
            case t1 :: rs => apHelperList1(f1, rs, ks -> k(map(f1, t1) :: ks))
        }

    def apHelperList2(t: Tree[a], fs: List[Tree[a -> b \ ef]], k: List[Tree[b]] -> Tree[b] \ ef): Tree[b] \ ef =
        match fs {
            case Nil => k(Nil)
            case f1 :: rs => apHelperList2(t, rs, ks ->
                                apHelper(f1, t, k1 ->
                                    k(k1 :: ks)))
        }

    pub def bind(t: Tree[a], f: a -> Tree[b] \ ef): Tree[b] \ ef =
        bindHelper(t, f, eidentity)

    def bindHelper(t: Tree[a], f: a -> Tree[b] \ ef, k: Tree[b] -> Tree[b] \ ef): Tree[b] \ ef =
        let Node(x1, ts) = t;
        match f(x1) {
            case Node(x2, ts2) => bindHelperList(ts, f, ks -> k(Node(x2, ts2 ::: ks)))
        }

    def bindHelperList(l: List[Tree[a]], f: a -> Tree[b] \ ef, k: List[Tree[b]] -> Tree[b] \ ef): Tree[b] \ ef =
        match l {
            case Nil => k(Nil)
            case t1 :: rs => bindHelper(t1, f, k1 ->
                                bindHelperList(rs, f, ks ->
                                    k(k1 :: ks)))
        }

    pub def flatMap(f: a -> Tree[b] \ ef, t: Tree[a]) : Tree[b] \ ef = bind(t, f)

    pub def drawTree(x: Tree[String]): String = drawTree1(x, xs -> xs) |> String.unlines

    pub def drawForest(x: Forest[String]): String = String.unlines(List.map(drawTree, x))

    def shift1(first: String, other: String, xs: List[String], k: List[String] -> List[String]): List[String] =
        match xs {
            case Nil => k(Nil)
            case x :: rs => {
                let s1 = first + x;
                shift2(other, rs, ss -> k(s1 :: ss))
            }
        }

    def shift2(other: String, xs: List[String], k: List[String] -> List[String]): List[String] =
        match xs {
            case Nil => k(Nil)
            case x :: rs => {
                let s1 = other + x;
                shift2(other, rs, ss -> k(s1 :: ss))
            }
        }


    def drawTree1(x: Tree[String], k: List[String] -> List[String]): List[String] =
        match x {
            case Node(label, kids) => {
                let xs = String.lines(label);
                drawKids(kids, ys -> k(xs ::: ys))
            }
        }

    def drawKids(xs: List[Tree[String]], k: List[String] -> List[String]): List[String] =
        match xs {
            case Nil       => k(Nil)
            case t1 :: Nil => drawTree1(t1, ac1 -> shift1("`- ", "   ", ac1, ac2 -> k("|" :: ac2)))
            case t1 :: rs  => drawTree1(t1, ac1 -> shift1("+- ", "|  ", ac1, ac2 -> drawKids(rs, ac3 -> k("|" :: (ac2 ::: ac3)))))
        }



    pub def flatten(x: Tree[a]): List[a] = preorder(x)

    pub def preorder(x: Tree[a]): List[a] =
        def loopTree(x1, ktree) = {
            def loopList(ts, klist) = match ts {
                case Nil     => klist(Chain.empty())
                case t :: rs => loopTree(t, vs1 -> loopList(rs, vs2 -> klist(Chain.append(vs1, vs2))))
            };
            match x1 {
                case Node(a, Nil) => ktree(Chain.singleton(a))
                case Node(a, xs)  => loopList(xs, vs -> ktree(Chain.cons(a, vs)))
            }
        };
        loopTree(x, eidentity) |> Chain.toList


    pub def postorder(x: Tree[a]): List[a] =
        def loopTree(x1, ktree) = {
            def loopList(ts, klist) = match ts {
                case Nil => klist(Chain.empty())
                case t1 :: rs => loopTree(t1, vs1 -> loopList(rs, vs2 -> klist(Chain.append(vs1, vs2))))
            };
            match x1 {
                case Node(a, Nil) => ktree(Chain.singleton(a))
                case Node(a, xs)  => loopList(xs, vs -> ktree(Chain.snoc(vs, a)))
            }
        };
        loopTree(x, eidentity) |> Chain.toList


    ///
    /// Tree catamorphism
    ///
    pub def foldTree(f: (a, List[b]) -> b, t: Tree[a]): b =
        foldTreeHelper(f, t, eidentity)

    ///
    /// Helper for `foldTree` in CPS.
    ///
    def foldTreeHelper(f: (a, List[b]) -> b, t: Tree[a], k: b -> b): b = match t {
        case Node(x,kids) => foldForest(f, kids, vs -> k(f(x,vs)))
    }

    ///
    /// Helper for `foldTree` in CPS.
    ///
    def foldForest(f: (a, List[b]) -> b, ts: Forest[a], k: List[b] -> b): b = match ts {
        case Nil => k(Nil)
        case x :: rs => foldTreeHelper(f, x, v -> foldForest(f, rs, vs -> k(v :: vs)))
    }

    pub def map(f: a -> b \ ef, t: Tree[a]): Tree[b] \ ef =
        def loopTree(t1, ktree) = {
            def loopList(ts, klist) = match ts {
                case Nil     => klist(Nil)
                case x :: rs => loopTree(x, v1 -> loopList(rs, vs -> klist(v1 :: vs)))
            };
            let Node(a, xs) = t1;
            loopList(xs, vs -> ktree(Node(f(a), vs)))
        };
        loopTree(t, eidentity)



    /// Traversal order is preorder (check...)
    pub def foreachTree(f: a -> Unit \ ef, t: Tree[a]): Unit \ ef =
        let Node(a,xs) = t;
        f(a);
        foreachForest(f, xs)

    def foreachForest(f: a -> Unit \ ef, ts: Forest[a]): Unit \ ef = match ts {
        case Nil => ()
        case x :: rs => { foreachTree(f, x); foreachForest(f, rs) }
    }

    pub def unfoldTree(f: b -> (a, List[b]) \ ef, st: b): Tree[a] \ ef =
        def loopTree(st1, ktree) = {
            def loopList(ss, klist) = match ss {
                case Nil => klist(Nil)
                case s :: rs => loopTree(s, k1 ->
                                    loopList(rs, ks -> klist(k1 :: ks)))
                };
            let (a, bs) = f(st1);
            loopList(bs, ks -> ktree(Node(a, ks)))
        };
        loopTree(st, eidentity)


    pub def unfoldForest(f: b -> (a, List[b]) \ ef, ss: List[b]): Forest[a] \ ef =
        List.map(unfoldTree(f), ss)


    pub def sequence(t: Tree[m[a]]): m[Tree[a]] with Applicative[m] =
        traverse(eidentity, t)

    pub def traverse(f: a -> m[b] \ ef, t: Tree[a]): m[Tree[b]] \ ef with Applicative[m] =
        traverseTree(f, t, eidentity)


    /// TODO check the cps transform...
    /// Particulary, is `f(x)` performed at the right time.
    def traverseTree(f: a -> m[b] \ ef, t: Tree[a], k: m[Tree[b]] -> m[Tree[b]] \ ef): m[Tree[b]] \ ef with Applicative[m] =
        match t {
            case Node(x, xs) => traverseForest(f, xs, ks -> k(nodeA(f(x), ks)))
        }


    def nodeA(mx: f[a], mxs: f[Forest[a]]): f[Tree[a]] with Applicative[f] =
        (((x, xs) -> Node(x, xs)) `Functor.map` mx) `Applicative.ap` mxs

    def consA(mx: f[a], mxs: f[List[a]]): f[List[a]] with Applicative[f] =
        (((x, xs) -> x :: xs) `Functor.map` mx) `Applicative.ap` mxs

    /// TODO check the cps transform...
    def traverseForest(f: a -> m[b] \ ef, l: Forest[a], k: m[Forest[b]] -> m[Tree[b]] \ ef): m[Tree[b]] \ ef with Applicative[m] =
        match l {
            case Nil     => k(Applicative.point(Nil))
            case x :: rs => traverseTree(f, x, k1 ->
                                traverseForest(f, rs, ks ->
                                    k(consA(k1, ks))))
        }

}
