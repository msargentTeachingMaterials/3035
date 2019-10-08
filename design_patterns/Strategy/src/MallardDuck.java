
public class MallardDuck extends Duck{

	public MallardDuck() {
		quackBehavior = new Quack();
		flyBehavior = new FlyWithWings();
	}
	
	public void performFly() {
		flyBehavior.fly();
	}
	
	public void performQuack() {
		quackBehavior.quack();
	}
	
	@Override
	public void display() {
		System.out.println("I'm a real Mallard duck");
	}
	
}
