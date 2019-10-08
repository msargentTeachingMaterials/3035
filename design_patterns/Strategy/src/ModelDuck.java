
public class ModelDuck extends Duck{
	
	public ModelDuck() {
		flyBehavior = new FlyNoWay();
		quackBehavior = new MuteQuack();
	}
	
	public void performFly() {
		flyBehavior.fly();
	}
	
	public void performQuack() {
		quackBehavior.quack();
	}
	

	@Override
	public void display() {
		System.out.println("I'm a model duck.");		
	}

}
