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



mod TestRoseTree {


    use RoseTree.Tree;
    use RoseTree.Tree.{Node};

    /////////////////////////////////////////////////////////////////////////////
    // equals                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def equals01(): Unit \ Assert  =
        let t1 = Node("a1", Nil);
        let t2 = Node("a1", Nil);
        Assert.assertEq(expected = true, t1 == t2)

    @Test
    def equals02(): Unit \ Assert  =
        let t1 = Node("a1", Nil);
        let t2 = Node("a1", Node("b2", Nil) :: Nil);
        Assert.assertEq(expected = false, t1 == t2)

    @Test
    def equals03(): Unit \ Assert  =
        let t1 = Node("a1", Node("b2", Nil) :: Nil);
        let t2 = Node("a1", Node("b2", Nil) :: Nil);
        Assert.assertEq(expected = true, t1 == t2)


    /////////////////////////////////////////////////////////////////////////////
    // drawTree                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def drawTree01(): Unit \ Assert =
        use RoseTree.{drawTree};
        let t1 = Node("a", Nil);
        Assert.assertEq(expected = "a", drawTree(t1) |> String.trimRight)

    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatten01(): Unit \ Assert  =
        use RoseTree.{flatten};
        let t1 = Node("a1", Nil);
        Assert.assertEq(expected = List#{"a1"}, flatten(t1))

    @Test
    def flatten02(): Unit \ Assert  =
        use RoseTree.{flatten};
        let t1 = Node("a1", Node("b2", Nil) :: Nil);
        Assert.assertEq(expected = List#{"a1", "b2"}, flatten(t1))

    @Test
    def flatten03(): Unit \ Assert  =
        use RoseTree.{flatten};
        let t1 = Node("a1", Node("b2", Nil) :: Node("c2", Nil) :: Nil);
        Assert.assertEq(expected = List#{"a1", "b2", "c2"}, flatten(t1))

    @Test
    def flatten04(): Unit \ Assert  =
        use RoseTree.{flatten};
        let t1 = Node("a1", Node("b2", Node("c3", Nil) :: Nil) :: Node("d2", Nil) :: Nil);
        Assert.assertEq(expected = List#{"a1", "b2", "c3", "d2"}, flatten(t1))

    /////////////////////////////////////////////////////////////////////////////
    // preorder                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def preorder01(): Unit \ Assert  =
        use RoseTree.{preorder};
        let t1 = Node(1, Node(2, Node(4, Nil) :: Node(5, Nil) :: Nil) :: Node(3, Nil) :: Nil);
        Assert.assertEq(expected = List#{1, 2, 4, 5, 3}, preorder(t1))

    /////////////////////////////////////////////////////////////////////////////
    // postorder                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def postorder01(): Unit \ Assert  =
        use RoseTree.{postorder};
        let t1 = Node(1, Node(2, Node(4, Nil) :: Node(5, Nil) :: Nil) :: Node(3, Nil) :: Nil);
        Assert.assertEq(expected = List#{4, 5, 2, 3, 1}, postorder(t1))

    /////////////////////////////////////////////////////////////////////////////
    // foldTree                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldTree01(): Unit \ Assert =
        use RoseTree.{foldTree};
        let t1 = Node(1, Nil);
        Assert.assertEq(expected = 1, foldTree((x,xs) -> x + Foldable.sum(xs), t1))

    @Test
    def foldTree02(): Unit \ Assert =
        use RoseTree.{foldTree};
        let t1 = Node(1, Node(2, Nil) :: Nil);
        Assert.assertEq(expected = 3, foldTree((x,xs) -> x + Foldable.sum(xs), t1))

    @Test
    def foldTree03(): Unit \ Assert =
        use RoseTree.{foldTree};
        let t1 = Node(1, Node(2, Nil) :: Node(3, Nil) :: Nil);
        Assert.assertEq(expected = 6, foldTree((x,xs) -> x + Foldable.sum(xs), t1))

    @Test
    def foldTree04(): Unit \ Assert =
        use RoseTree.{foldTree};
        let t1 = Node(1, Node(2, Node(3, Nil) :: Nil) :: Node(4, Nil) :: Nil);
        Assert.assertEq(expected = 10, foldTree((x,xs) -> x + Foldable.sum(xs), t1))

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map01(): Unit \ Assert =
        use RoseTree.{map, foldTree};
        let t1 = Node(1, Nil) |> map(x -> x + 1);
        Assert.assertEq(expected = 2, foldTree((x,xs) -> x + Foldable.sum(xs), t1))

    @Test
    def map02(): Unit \ Assert =
        use RoseTree.{map, foldTree};
        let t1 = Node(1, Node(2, Nil) :: Nil) |> map(x -> x + 1);
        Assert.assertEq(expected = 5, foldTree((x,xs) -> x + Foldable.sum(xs), t1))


    @Test
    def map03(): Unit \ Assert =
        use RoseTree.{map, foldTree};
        let t1 = Node(1, Node(2, Nil) :: Node(3, Nil) :: Nil) |> map(x -> x + 1);
        Assert.assertEq(expected = 9, foldTree((x,xs) -> x + Foldable.sum(xs), t1))

    @Test
    def map04(): Unit \ Assert =
        use RoseTree.{map, foldTree};
        let t1 = Node(1, Node(2, Node(3, Nil) :: Nil) :: Node(4, Nil) :: Nil) |> map(x -> x + 1);
        Assert.assertEq(expected = 14, foldTree((x,xs) -> x + Foldable.sum(xs), t1))


}
