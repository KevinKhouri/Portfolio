import java.util.*;

public class Kruskal{

    public static WGraph kruskal(WGraph g){
    	WGraph mst = new WGraph(); //initialize empty MST.
    	ArrayList<Edge> edges = g.listOfEdgesSorted(); //get all edged in original graph.
    	DisjointSets p = new DisjointSets(g.getNbNodes()); //Make-Set(x) on all nodes in original graph. i.e. make n disjoint sets/components
    	int counter = g.getNbNodes(); //counts if |V| - 1 edges were added.
    	for(Edge e: edges){
    		if (counter == 1) break; //|V| - 1 edges were added.
    		if (IsSafe(p,e)) { //check if edge e crosses a cut between components.
    			mst.addEdge(e); //since e safe, add to MST.
    			counter--;
    			p.union(e.nodes[0], e.nodes[1]); //union what was the two disjoint components.
    		}
    	}
    	return mst;
     }

    //simply checks if both representatives are the same (which means not safe).
    public static Boolean IsSafe(DisjointSets p, Edge e){
    	return (p.find(e.nodes[0]) != p.find(e.nodes[1]));
    }

    public static void main(String[] args){

        String file = args[0];
        WGraph g = new WGraph(file);
        WGraph t = kruskal(g);
        System.out.println(t);

   } 
}
