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
    // drawTree                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def drawTree01(): String =
        use RoseTree.{drawTree};
        let t1 = Node("a", Nil);
        drawTree(t1)

    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatten01(): Bool =
        use RoseTree.{flatten};
        let t1 = Node("a1", Nil);
        flatten(t1) == "a1" :: Nil

    @test
    def flatten02(): Bool =
        use RoseTree.{flatten};
        let t1 = Node("a1", Node("b2", Nil) :: Nil);
        flatten(t1) == "a1" :: "b2" :: Nil

    @test
    def flatten03(): Bool =
        use RoseTree.{flatten};
        let t1 = Node("a1", Node("b2", Nil) :: Node("c2", Nil) :: Nil);
        flatten(t1) == "a1" :: "b2" :: "c2" :: Nil

    @test
    def flatten04(): Bool =
        use RoseTree.{flatten};
        let t1 = Node("a1", Node("b2", Node("c3", Nil) :: Nil) :: Node("d2", Nil) :: Nil);
        flatten(t1) == "a1" :: "b2" :: "c3" :: "d2" :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // preorder                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def preorder01(): Bool =
        use RoseTree.{preorder};
        let t1 = Node(1, Node(2, Node(4, Nil) :: Node(5, Nil) :: Nil) :: Node(3, Nil) :: Nil);
        preorder(t1) == 1 :: 2 :: 4 :: 5 :: 3 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // postorder                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def postorder01(): Bool =
        use RoseTree.{postorder};
        let t1 = Node(1, Node(2, Node(4, Nil) :: Node(5, Nil) :: Nil) :: Node(3, Nil) :: Nil);
        postorder(t1) == 4 :: 5 :: 2 :: 3 :: 1 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // foldTree                                                            //
    /////////////////////////////////////////////////////////////////////////////

    def sum(xs: List[Int32]): Int32 = List.foldRightWithCont((x, fy) -> x + fy(), 0, xs)

    @test
    def foldTree01(): Bool =
        use RoseTree.{foldTree};
        let t1 = Node(1, Nil);
        foldTree((x,xs) -> x + sum(xs), t1) == 1

    @test
    def foldTree02(): Bool =
        use RoseTree.{foldTree};
        let t1 = Node(1, Node(2, Nil) :: Nil);
        foldTree((x,xs) -> x + sum(xs), t1) == 3

    @test
    def foldTree03(): Bool =
        use RoseTree.{foldTree};
        let t1 = Node(1, Node(2, Nil) :: Node(3, Nil) :: Nil);
        foldTree((x,xs) -> x + sum(xs), t1) == 6

    @test
    def foldTree04(): Bool =
        use RoseTree.{foldTree};
        let t1 = Node(1, Node(2, Node(3, Nil) :: Nil) :: Node(4, Nil) :: Nil);
        foldTree((x,xs) -> x + sum(xs), t1) == 10

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool =
        use RoseTree.{map, foldTree};
        let t1 = Node(1, Nil) |> map(x -> x + 1);
        foldTree((x,xs) -> x + sum(xs), t1) == 2

    @test
    def map02(): Bool =
        use RoseTree.{map, foldTree};
        let t1 = Node(1, Node(2, Nil) :: Nil) |> map(x -> x + 1);
        foldTree((x,xs) -> x + sum(xs), t1) == 5


    @test
    def map03(): Bool =
        use RoseTree.{map, foldTree};
        let t1 = Node(1, Node(2, Nil) :: Node(3, Nil) :: Nil) |> map(x -> x + 1);
        foldTree((x,xs) -> x + sum(xs), t1) == 9

    @test
    def map04(): Bool =
        use RoseTree.{map, foldTree};
        let t1 = Node(1, Node(2, Node(3, Nil) :: Nil) :: Node(4, Nil) :: Nil) |> map(x -> x + 1);
        foldTree((x,xs) -> x + sum(xs), t1) == 14


}
