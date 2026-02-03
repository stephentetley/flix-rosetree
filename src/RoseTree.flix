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



    ///
    /// Create a tree with a single node.
    ///
    pub def singleton(x: a): Tree[a] = Node(x, Nil)

    ///
    /// Returns `true` if and only if `t1` and `t2` and equal.
    ///
    pub def equals(t1: Tree[a], t2: Tree[a]): Bool with Eq[a] = 
        def loopTree(x1, y1, ktree) = {
            def loopList(xs, ys, klist) = match (xs, ys) {
                case (a1 :: rs1, a2 :: rs2) => 
                    loopTree(a1, a2, kb -> 
                        if (kb) loopList(rs1, rs2, klist) else klist(false))
                case (Nil, Nil)             => klist(true)
                case (_, _)                 => klist(false)
            };
            let Node(a, ks1) = x1;
            let Node(b, ks2) = y1;
            if (a == b) loopList(ks1, ks2, ktree) else ktree(false)
        };
        loopTree(t1, t2, eidentity)
        



    pub def ap(f: Tree[a -> b \ ef], t: Tree[a]): Tree[b] \ ef =
        def loopTree(tf, tx, ktree) = {
            def loopList1(f1, ts, klist1) = {
                match ts {
                    case Nil => klist1(Nil)
                    case t1 :: rs => loopList1(f1, rs, ks -> klist1(map(f1, t1) :: ks))
                }
            };
            def loopList2(t1, fs, klist2) = match fs {
                case Nil => klist2(Nil)
                case fn1 :: rs => loopList2(t1, rs, ks ->
                                    loopTree(fn1, t1, k1 ->
                                        klist2(k1 :: ks)))
            };
            let Node(f1, fs) = tf;
            let Node(x1, ts) = tx;
            loopList1(f1, ts, ks1 ->
                loopList2(tx, fs, ks2 -> ktree(Node(f1(x1), ks1 ::: ks2))))
        };
        loopTree(f, t, eidentity)



    pub def flatMap(f: a -> Tree[b] \ ef, t: Tree[a]): Tree[b] \ ef =
        def loopTree(t1, ktree) = {
            def loopList(xs, klist) = match xs {
                case Nil => klist(Nil)
                case x1 :: rs => loopTree(x1, k1 ->
                                    loopList(rs, ks ->
                                        klist(k1 :: ks)))
            };
            let Node(x1, ts) = t1;
            let Node(x2, ts2)  = f(x1);
            loopList(ts, ks -> ktree(Node(x2, ts2 ::: ks)))
        };
        loopTree(t, eidentity)


    pub def drawTree(t: Tree[String]): String = 
        def shift1(first1, other1, xs1, kshift1) = {
            def shift2(other2, xs2, kshift2) = match xs2 {
                case Nil        => kshift2(Nil)
                case x1 :: rs => {
                    let s1 = other2 + x1;
                    shift2(other2, rs, ss -> kshift2(s1 :: ss))
                }
            };
            match xs1 {
               case Nil => kshift1(Nil)
                case x1 :: rs => {
                    let s1 = first1 + x1;
                    shift2(other1, rs, ss -> kshift1(s1 :: ss))
                }
            }
        };
        def loopTree(x, ktree) = {
            def loopList(xs, klist) =
                match xs {
                    case Nil       => klist(Nil)
                    case t1 :: Nil => 
                        loopTree(t1, ac1 -> shift1("`- ", "   ", ac1, ac2 -> klist("|" :: ac2)))

                    case t1 :: rs  => 
                        loopTree(t1, ac1 -> shift1("+- ", "|  ", ac1, ac2 -> 
                            loopList(rs, ac3 -> klist("|" :: (ac2 ::: ac3)))))
                };
            match x {
                case Node(label, kids) => {
                    let xs = String.lines(label);
                    loopList(kids, ys -> ktree(xs ::: ys))
                }
            }
        };
        loopTree(t, xs -> xs) |> String.unlines

    pub def drawForest(x: Forest[String]): String = String.unlines(List.map(drawTree, x))

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
        def loopTree(x, ktree) = {
            def loopList(ts, klist) = match ts {
                case Nil => klist(Nil)
                case t1 :: rs => loopTree(t1, v -> loopList(rs, vs -> klist(v :: vs)))
            };
            let Node(x1, kids) = x;
            loopList(kids, vs -> ktree(f(x1,vs)))
        };
        loopTree(t, eidentity)


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

    pub def foreachForest(f: a -> Unit \ ef, ts: Forest[a]): Unit \ ef = match ts {
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
