import java.util.*;

public class BellmanFord{

    private int[] distances = null;
    private int[] predecessors = null;
    private int source;

    class BellmanFordException extends Exception{
        public BellmanFordException(String str){
            super(str);
        }
    }

    class NegativeWeightException extends BellmanFordException{
        public NegativeWeightException(String str){
            super(str);
        }
    }

    class PathDoesNotExistException extends BellmanFordException{
        public PathDoesNotExistException(String str){
            super(str);
        }
    }

    BellmanFord(WGraph g, int source) throws NegativeWeightException{
        /* Constructor, input a graph and a source
         * Computes the Bellman Ford algorithm to populate the
         * attributes 
         *  distances - at position "n" the distance of node "n" to the source is kept
         *  predecessors - at position "n" the predecessor of node "n" on the path
         *                 to the source is kept
         *  source - the source node
         *
         *  If the node is not reachable from the source, the
         *  distance value must be Integer.MAX_VALUE
         */
    	this.source = source;
    	initializeSingleSource(g,source);
    	ArrayList<Edge> edges = g.getEdges();
    	for (int i = 1; i <= g.getNbNodes()-1;i++) { //iterate |V| - 1 times
    		for (Edge e: edges) {
    			relax(e.nodes[0],e.nodes[1],e.weight); //we relax all edges
    		}
    	}
    	for (Edge e: edges) { //checks if we had a negative weight cycle
    		if (distances[e.nodes[1]] > distances[e.nodes[0]] + e.weight) {
    			throw new NegativeWeightException("Found a negative cycle!");
    		}
    	}

    }
    
    private void initializeSingleSource (WGraph g, int source) {
    	distances = new int[g.getNbNodes()];
    	predecessors = new int[g.getNbNodes()];
    	for (int i=0; i<g.getNbNodes(); i++) {
    		distances[i] = Integer.MAX_VALUE;
    		predecessors[i] = -1;
    	}
    	distances[source] = 0;
    }
    
    private void relax(int u, int v, int weight) {
    	if (distances[v] > distances[u] + weight) {
    		distances[v] = distances[u] + weight;
    		predecessors[v] = u;
    	}
    }

    public int[] shortestPath(int destination) throws PathDoesNotExistException{
        /*Returns the list of nodes along the shortest path from 
         * the object source to the input destination
         * If not path exists an Error is thrown
         */
    	int counter = 1;
    	int node = destination;
        while(predecessors[node] != -1) { //finding number of nodes in shortest path
        	counter++;
        	node = predecessors[node];
        }
        if (source != node) { //checking if shortest path exists
        	throw new PathDoesNotExistException("No path exists from source to destination.");
        }
        int[] path = new int[counter];
        node = destination;
        for (int i = counter-1; i>=0;i--) { //adding nodes in path to array.
        	path[i] = node;
        	node = predecessors[node];
        }
        return path;
    }

    public void printPath(int destination){
        /*Print the path in the format s->n1->n2->destination
         *if the path exists, else catch the Error and 
         *prints it
         */
        try {
            int[] path = this.shortestPath(destination);
            for (int i = 0; i < path.length; i++){
                int next = path[i];
                if (next == destination){
                    System.out.println(destination);
                }
                else {
                    System.out.print(next + "-->");
                }
            }
        }
        catch (Exception e){
            System.out.println(e);
        }
    }

    public static void main(String[] args){

        String file = args[0];
        WGraph g = new WGraph(file);
        try{
            BellmanFord bf = new BellmanFord(g, g.getSource());
            bf.printPath(g.getDestination());
        }
        catch (Exception e){
            System.out.println(e);
        }

   } 
}

