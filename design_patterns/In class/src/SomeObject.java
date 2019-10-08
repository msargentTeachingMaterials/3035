import java.util.ArrayList;

public class SomeObject {	
	
	public void changeObject(ArrayList<String> a){
		a.add("Next ");		
		a = new ArrayList<String>();
		a.add("Last ");
	}
}
