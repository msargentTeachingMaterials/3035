function main(){ 
	var x;
	function bigsub() { 
		var a, b, c; 
		function sub1 {
			var a, d; 
			a = b + c;
			
		} // end of sub1 
		function sub2(x) {
			var b, e;
			function sub3() { 
				var c, e;
				sub1(); 
				e = b + a;  
			} // end of sub3 ...
			sub3(); 
			a = d + e;  
		} // end of sub2
		sub2(7);		
	} // end of bigsub ...
	bigsub();
} // end of main