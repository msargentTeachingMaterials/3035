
public class RubberDuck extends Duck{
	
	public RubberDuck(){
		flyBehavior = new FlyNoWay();
		quackBehavior = new Squeak();
	}
	
	public void performFly() {
		flyBehavior.fly();
	}
	
	public void performQuack() {
		quackBehavior.quack();
	}
	
	@Override
	public void display() {
		System.out.println("I'm yellow and made of rubber!");		
	}

}
