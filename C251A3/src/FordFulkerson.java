import java.util.*;
import java.io.File;

public class FordFulkerson {

	public static ArrayList<Integer> pathDFS(Integer source, Integer destination, WGraph graph){
		ArrayList<Integer> path = new ArrayList<Integer>();
		/* YOUR CODE GOES HERE*/
		boolean visited[] = new boolean[graph.getNbNodes()]; //we assume the nodes are numbered in order from 0 to |V|.
		path.add(source);
		if (source==destination) return path;
		if (!DFSVisit(graph,source,destination,path,visited)) path.remove(path.size()-1);
		return path;
	}
	
	private static boolean DFSVisit(WGraph G, Integer u, Integer destination, ArrayList<Integer> path, boolean[] visited) {
		visited[u] = true;
		for (int i: getAdjNodes(G,u)) {
			if (!visited[i]) {
				path.add(i);
				if (i == destination) {
					return true; // we found destination in this path. So return true to signify not to edit path.
				}else {
					boolean found = DFSVisit(G,i,destination,path,visited);
					if (!found) {
						path.remove(path.size()-1);
					} else {
						return true;
					}
				}
			}
		}
		return false;
	}
	
	private static ArrayList<Integer> pathDFSWeighted(Integer source, Integer destination, WGraph graph){
		ArrayList<Integer> path = new ArrayList<Integer>();
		/* YOUR CODE GOES HERE*/
		boolean visited[] = new boolean[graph.getNbNodes()]; //we assume the nodes are numbered in order from 0 to |V|.
		path.add(source);
		if (source==destination) return path;
		if (!DFSVisitWeighted(graph,source,destination,path,visited)) path.remove(path.size()-1);
		return path;
	}
	
	private static boolean DFSVisitWeighted(WGraph G, Integer u, Integer destination, ArrayList<Integer> path, boolean[] visited) {
		visited[u] = true;
		for (int i: getAdjNodes(G,u)) {
			if (!visited[i] && (G.getEdge(u, i).weight != 0)) {
				path.add(i);
				if (i == destination) {
					return true; // we found destination in this path. So return true to signify not to edit path.
				}else {
					boolean found = DFSVisitWeighted(G,i,destination,path,visited);
					if (!found) {
						path.remove(path.size()-1);
					} else {
						return true;
					}
				}
			}
		}
		return false;
	}


	public static String fordfulkerson(WGraph graph){
		String answer="";
		int maxFlow = 0;
		
		/* YOUR CODE GOES HERE		*/
		if(graph.getEdges().size() == 0) {
			answer += maxFlow + "\n" + graph.toString();	
			return answer;
		}
		
		WGraph G = graph;
		WGraph Gf = new WGraph(graph);
		for (Edge edge: G.getEdges()) {
			edge.weight = 0;
		}
		ArrayList<Integer> augPath = pathDFSWeighted(Gf.getSource(),Gf.getDestination(),Gf);
		while (!augPath.isEmpty()) {
			//get a list of the edges on this path
			ArrayList<Edge> aPath = new ArrayList<Edge>(augPath.size()-1);
			Edge firstEdge = Gf.getEdge(augPath.get(0), augPath.get(1));
			aPath.add(firstEdge);
			int pathResCap = firstEdge.weight; //beta
			for (int i = 1; i<augPath.size()-1;i++) {
				Edge e = Gf.getEdge(augPath.get(i), augPath.get(i+1));
				aPath.add(e);
				if (e.weight < pathResCap) {
					pathResCap = e.weight;
				}
			}
			//let beta = min residualCapacity of edge on this path^^^
			
			//add beta to maxFlow.
			maxFlow += pathResCap;
			//copy lines 5 to 7 having .f be the edge weight
			for (Edge e: aPath) {
				Edge temp = G.getEdge(e.nodes[0], e.nodes[1]);
				Edge temp2 = G.getEdge(e.nodes[1], e.nodes[0]);
				if (temp != null) {
					temp.weight += pathResCap;
				} else {
					temp2.weight -= pathResCap;
				}
			}
			//update Gf by updating/adding edges in reverse direction of path
			//with weight beta.
			for (Edge e: aPath) {
				try {
				Gf.addEdge(new Edge(e.nodes[1],e.nodes[0], pathResCap));
				} catch(Exception ex) {
					int temp = Gf.getEdge(e.nodes[1], e.nodes[0]).weight;
					Gf.setEdge(e.nodes[1],e.nodes[0],pathResCap + temp);
					}
			}
			//Then, subtracting beta from each edge in original augPath,
			//if weight equals zero, remove this edge from the set of edges.
			for (Edge e: aPath) {
				e.weight -= pathResCap;
			}
			//update augPath
			augPath = pathDFSWeighted(Gf.getSource(),Gf.getDestination(),Gf);
		}
		answer += maxFlow + "\n" + graph.toString();	
		return answer;
	}
	
	private static ArrayList<Integer> getAdjNodes(WGraph g, int n){ //gets all nodes adjacent to n
    	ArrayList<Integer> adj = new ArrayList<Integer>();
    	for (Edge edge: g.getEdges()) {
    		if (edge.nodes[0] == n) {
    			adj.add(edge.nodes[1]);
    		}
    	}
    	return adj;
    }
	
	//public void removeEdge(Edge e) {///
    //	this.edges.remove(e);
   // }

	 public static void main(String[] args){
		 String file = args[0];
		 File f = new File(file);
		 WGraph g = new WGraph(file);
	         System.out.println(fordfulkerson(g));
	         }
}

