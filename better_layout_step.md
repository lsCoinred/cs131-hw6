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