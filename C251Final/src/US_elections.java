import java.util.*;
import java.io.File;
import java.io.FileNotFoundException;

public class US_elections {

	public static int solution(int num_states, int[] delegates, int[] votes_Biden, int[] votes_Trump, int[] votes_Undecided){
		//First we preprocess the data.
		int trumpsSecuredDelegates = 0;
		int bidensSecuredDelegates = 0;
		int availableDelegates = 0; 
		// Below at index i is -1 if Trump already won the state, 0 if Biden already won the state. > 0 if Biden could win the state and represents the minimum.
		int[] votesNeededForBidenToWinTheState = new int[num_states];
		int numOfContestedStates = 0;
		for (int i = 0; i < num_states; i++) {
			if ((votes_Trump[i] - (votes_Biden[i] + votes_Undecided[i])) >= 0) { 		// If Trump wins state no matter what.
				trumpsSecuredDelegates += delegates[i];
				votesNeededForBidenToWinTheState[i] = -1;
			} else if ((votes_Biden[i] - (votes_Trump[i] + votes_Undecided[i])) > 0) { 	// If Biden wins state no matter what.
				bidensSecuredDelegates += delegates[i];
				votesNeededForBidenToWinTheState[i] = 0;
			} else {																	// If either party could win the state.
				availableDelegates += delegates[i];
				votesNeededForBidenToWinTheState[i] = (votes_Trump[i] - votes_Biden[i] + 1) 
						+ (int) Math.ceil((0.5*(votes_Undecided[i] - 1 - (votes_Trump[i] - votes_Biden[i]))));
				numOfContestedStates += 1;
			}
		}
		
		if (trumpsSecuredDelegates >= (bidensSecuredDelegates + availableDelegates)) { 			// If Trump already won the presidency.
			return -1;
		} else if (bidensSecuredDelegates > (trumpsSecuredDelegates + availableDelegates)) {	// If Biden already won the presidency.
			return 0;
		} else {																				// If either party could win the presidency.
			//Compute the min-cost Knapsack problem.
			int threshold = (trumpsSecuredDelegates - bidensSecuredDelegates + 1) + 
					(int) (Math.ceil(0.5*(availableDelegates - 1  - (trumpsSecuredDelegates - bidensSecuredDelegates))));
			int[] contestedDelegatesOfState = new int[numOfContestedStates];
			int[] minVotesNeededToWinState = new int[numOfContestedStates]; //min number of votes for Biden to win the contested delegate.
			int counter = 0;
			for (int i = 0; i < num_states; i++) { //We create two arrays which only consider the states that Biden could possibly need to win.
				if (votesNeededForBidenToWinTheState[i] != -1 && votesNeededForBidenToWinTheState[i] != 0) { //We don't include states that Trump or Biden already won. 
					contestedDelegatesOfState[counter] = delegates[i];
					minVotesNeededToWinState[counter] = votesNeededForBidenToWinTheState[i];
					counter++;
				}
			}
			// At this point, all preprocessing is complete. We now have the following problem:
			// Given n items (n = numOfContestedStates), with each item having a value (value = minVotesNeededToWinTheState)
			// and a weight (weight = contestedDelegatesOfState), we want to find the minimum value needed to achieve a minimum weight boundary.
			// i.e. We want the smallest number of votes needed for the delegates to be above a given threshold.
			
			// Variables needed: availableDelegates (the max capacity of the knapsack), contestedDelegatesOfState (array containing number of delegates/weight of each state/item), 
			// and minVotesNeededToWinState (array containing number of votes/value of each state/item).
			
			int[][] minVotesMatrix = new int[numOfContestedStates + 1][availableDelegates + 1];
			
			for (int i = 0; i <= availableDelegates; i++) { // initialize row 0 with inf.
				minVotesMatrix[0][i] = Integer.MAX_VALUE;
			}
			
			for (int i = 0; i <= numOfContestedStates; i++) { // initialize column 0 with 0.
				minVotesMatrix[i][0] = 0;
			}
			
			for (int i = 1; i <= numOfContestedStates; i++) {
				for (int j = 1; j <= availableDelegates; j++) {
					if (contestedDelegatesOfState[i-1] > j) {
						minVotesMatrix[i][j] = minVotesMatrix[i-1][j];
					} else {
						if (minVotesMatrix[i-1][j-contestedDelegatesOfState[i-1]] == Integer.MAX_VALUE) {
							minVotesMatrix[i][j] = minVotesMatrix[i-1][j];
						} else {
							minVotesMatrix[i][j] = Math.min(minVotesMatrix[i-1][j], minVotesNeededToWinState[i-1] + minVotesMatrix[i-1][j-contestedDelegatesOfState[i-1]]);
						}
						
					}
				}
			}
			// Now we must find the minimum votes needed to meet the threshold.
			int minVotes = Integer.MAX_VALUE;
			for (int j = threshold; j <= availableDelegates; j++) {
				if (minVotesMatrix[numOfContestedStates][j] < minVotes) {
					minVotes = minVotesMatrix[numOfContestedStates][j];
				}
			}
			
			return minVotes;
		}
		
		
	}

	public static void main(String[] args) {
	 try {
			String path = args[0];
      File myFile = new File(path);
      Scanner sc = new Scanner(myFile);
      int num_states = sc.nextInt();
      int[] delegates = new int[num_states];
      int[] votes_Biden = new int[num_states];
      int[] votes_Trump = new int[num_states];
 			int[] votes_Undecided = new int[num_states];	
      for (int state = 0; state<num_states; state++){
			  delegates[state] =sc.nextInt();
				votes_Biden[state] = sc.nextInt();
				votes_Trump[state] = sc.nextInt();
				votes_Undecided[state] = sc.nextInt();
      }
      sc.close();
      int answer = solution(num_states, delegates, votes_Biden, votes_Trump, votes_Undecided);
      	System.out.println(answer);
    	} catch (FileNotFoundException e) {
      	System.out.println("An error occurred.");
      	e.printStackTrace();
    	}
  	}

}