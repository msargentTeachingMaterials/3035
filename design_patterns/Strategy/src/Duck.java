
public abstract class Duck {
	
	protected FlyBehavior flyBehavior;
	protected QuackBehavior quackBehavior;
	
	public Duck() {}
	
	public abstract void display();
	
	public abstract void performFly();
	
	public abstract void performQuack();
	
	
	public void swim() {
		System.out.println("All ducks float, even decoys!");
	}

	public QuackBehavior getQuackBehavior() {
		return quackBehavior;
	}

	public void setQuackBehavior(QuackBehavior quackBehavior) {
		this.quackBehavior = quackBehavior;
	}

	public FlyBehavior getFlyBehavior() {
		return flyBehavior;
	}

	public void setFlyBehavior(FlyBehavior flyBehavior) {
		this.flyBehavior = flyBehavior;
	}

	
}
