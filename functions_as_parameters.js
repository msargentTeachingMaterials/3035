function sub1(){
	var x; //deep binding
	function sub2(){
		console.log(x);//which x is being printed here?
	};
	function sub3(){
		var x; //ad hoc binding
		x = 3;
		sub4(sub2);
	};
	function sub4(subx){
		var x; //shallow binding
		x = 4;
		subx();
	};
	x = 1;
	sub3();
};

sub1();