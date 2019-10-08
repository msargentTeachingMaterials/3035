// function addX(x){
// 	return function(y){
// 		return x + y;
// 	};
// };

// var add10 = addX(10);
// console.log(add10(3));

// for(var i = 0; i < 5; i++){
// 	console.log(add10(i));
// }


function addXandY(x, y){
	return x + y;
};

function makeCounter(){
	var counter = 0;
	return function(){
		return counter++;
	};
};

var advanceCounter = makeCounter();




