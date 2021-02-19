import java.util.*;

class Assignment implements Comparator<Assignment>{
	int number;
	int weight;
	int deadline;
	
	
	protected Assignment() {
	}
	
	protected Assignment(int number, int weight, int deadline) {
		this.number = number;
		this.weight = weight;
		this.deadline = deadline;
	}
	
	
	
	/**
	 * This method is used to sort to compare assignment objects for sorting. 
	 */
	//1) Sort homeworks in decreasing order of weight.
	//If tie, put first the one that finishes last.
	@Override
	public int compare(Assignment a1, Assignment a2) {
		// TODO Implement this
		if (a1.weight < a2.weight) {
			return 1;
		} else if (a1.weight > a2.weight) {
			return -1;
		} else {
			if (a1.deadline < a2.deadline) { //tiebreaker
				return 1;
			} else if (a1.deadline > a2.deadline) {
				return -1;
			} else {
				return 0;
			}
		}
	}
}

public class HW_Sched {
	ArrayList<Assignment> Assignments = new ArrayList<Assignment>();
	int m;
	int lastDeadline = 0;
	
	protected HW_Sched(int[] weights, int[] deadlines, int size) {
		for (int i=0; i<size; i++) {
			Assignment homework = new Assignment(i, weights[i], deadlines[i]);
			this.Assignments.add(homework);
			if (homework.deadline > lastDeadline) {
				lastDeadline = homework.deadline;
			}
		}
		m =size;
	}
	
	
	/**
	 * 
	 * @return Array where output[i] corresponds to the assignment 
	 * that will be done at time i.
	 */
	public int[] SelectAssignments() {
		//TODO Implement this
		
		//Sort assignments
		//Order will depend on how compare function is implemented
		Collections.sort(Assignments, new Assignment());
		
		// If homeworkPlan[i] has a value -1, it indicates that the 
		// i'th timeslot in the homeworkPlan is empty
		//homeworkPlan contains the homework schedule between now and the last deadline
		int[] homeworkPlan = new int[lastDeadline];
		for (int i=0; i < homeworkPlan.length; ++i) {
			homeworkPlan[i] = -1;
		}
		//2)In order, schedule the homeworks to be done on the last possible day they
		//could be completed. If it clashes with an already scheduled one, then try an earlier time.
		//3)If no compatible slots are left, that homework can't be done. Which is ok I guess lol.
		int counter = 0;
		for (Assignment a: Assignments) {
			if (counter == lastDeadline) break;
			int index = a.deadline - 1;
			while (index >= 0 && homeworkPlan[index] != -1  ) {
				index--;
			}
			if (index < 0) continue;
			homeworkPlan[index] = a.number;
			counter++;
		}
		
		return homeworkPlan;
	}
}
	



