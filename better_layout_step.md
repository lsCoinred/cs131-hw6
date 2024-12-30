### Complete Register Allocation Algorithm
1. Build interference graph (precolor nodes as necessary).
    - Add move related edges
2. Reduce the graph (building a stack of nodes to color).
    1. Simplify the graph as much as possible without removing nodes that are move related (i.e. have a move-related neighbor). Remaining nodes are high degree or move-related.
        1. Find a node with degree < K and cut it out of the graph.
            – Remove the nodes and edges.
            – This is called simplifying the graph
        2. Recursively K-color the remaining subgraph
        3. When remaining graph is colored, there must be at least one free color available for the deleted node (since its degree was < K). 
            - Pick such a color.
    2. Coalesce move-related nodes using Brigg’s or George’s strategy.
        - Brigg’s strategy: It's safe to coalesce x & y if the resulting node will have fewer than k neighbors (with degree ≥ k).
        - George’s strategy: We can safely coalesce x & y if for every neighbor t of x, either t already interferes with y or t has degree < k.
    3. Coalescing can reveal more nodes that can be simplified, so repeat 2.1 and 2.2 until no node can be simplified or coalesced.
    4. If no nodes can be coalesced freeze (remove) a move-related edge and keep trying to simplify/coalesce.
3. If there are non-precolored nodes left, mark one for spilling, remove it from the graph and continue doing step 2.
4. When only pre-colored node remain, start coloring (popping simplified nodes off the top of the stack).
    1. If a node must be spilled, insert spill code as on slide 14 and rerun the whole register allocation algorithm starting at step 1.
    2. Option 1: Reserve registers specifically for moving to/from memory.

--------------------------------------------------

翻译后的注释如下：

#### 更好的寄存器分配 

任务：实现一个（正确的）寄存器分配策略，其性能优于上面提到的贪心布局策略，前提是活跃性信息使用来自 liveness.ml 的数据流分析计算。

你的实现_不必一定_实现讲座中描述的完全图着色合并。你可以选择一个更简单的策略。特别地，使用一些简单的偏好启发式的非合并图着色算法 (non-coalescing graph coloring algorithm that uses some simple preference heuristics) 应该能够击败贪心算法。

为了衡量你的策略的有效性，我们的测试基础设施使用一个简单的启发式方法，将其与上面的“贪心”策略进行比较。

质量启发式：
    一个 x86 程序的寄存器分配的“质量分数”基于两件事：

    - 总的内存访问次数，计算为：
        - Ind2 和 Ind3 操作数的数量
        - Push 和 Pop 指令的数量

    - size(p)，即 x86 程序中的总指令数

你在寄存器分配中的目标应该是最小化内存操作次数，其次是最小化程序的总体大小。

registers.ml 提供了一些辅助函数，你可以用来获取程序中内存操作的大小和总数。它还提供了一个计算寄存器使用情况的直方图的函数，这在测试你的寄存器分配器时可能会有帮助。

为了检查你的寄存器分配是否优于贪心算法，我们会检查：
    如果 #mem_ops(你的分配) < #mem_ops(贪心分配) 则你的分配更好
    否则如果 size(你的分配) < size(贪心分配) 则你的分配更好
    否则贪心算法胜。

提示：
    - Datastructures 文件提供了一个 UidMap，可以用来创建你的干涉图。

    - 理解这个版本的编译器如何处理函数调用（参见 compile_pmov）以及贪心分配器如何工作可能很有帮助。

    - 编译器在代码生成中使用了 Rax 和 Rcx，所以它们_通常_不能用于你的分配器。

        - 其他调用者保存的寄存器可以自由使用

        - 如果你想使用被调用者保存的寄存器，你可能需要调整 compile_fdecl 生成的代码来保存/恢复它们。