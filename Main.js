// function fibonacci(n){
// 	if(n<=1){
// 		return 1;
// 	}else{
// 		var result = fibonacci(n-1) + fibonacci(n-2);
// 		return (result);
// 	}
// }
// fibonacci(6);

// function fatorial(n){
//   if ((n == 0) || (n == 1))
//     return 1;
//   else
//     return (n * fatorial(n - 1));
// }


// function concatSorted(x, y)
// {
// 	var temp = [];
// 	while(true)
// 	{
// 		if((x.len) == 0 && (y.len) == 0) break;
		
// 		if((x.len) == 0)
// 		{
// 			temp = temp.concat(y);
// 			break;
// 		}
		
// 		if((y.len) == 0)
// 		{
// 			temp = temp.concat(x);
// 			break;
// 		}
		
// 		if((x.head) < (y.head))
// 		{
// 			temp = temp.concat(x.head);
// 			x = x.tail;
// 		}
// 		else 
// 		{
// 			temp = temp.concat(y.head);
// 			y = y.tail;
// 		}
// 	}
	
// 	return temp;
// }

// function mergesort(x, s, e)
// {
// 	if(s == e) return [x[s]];

// 	var half = (s + e) / 2;
// 	var left = mergesort(x, s, half);
// 	var right = mergesort(x, half + 1, e);

// 	return concatSorted(left, right);
// }

// var x = [2, 1, 4, 3, 9, 5, 0, 7, 8];
// mergesort(x, 0, x.len-1);


// function dobreXvezes(x){
// 	var i = 0;
// 	var result = x;
// 	while(i<x){
//  		result = result + result;
//  		i = i + 1;
// 	}
// 	return result;
// }
// teste(3);

var result = 0;
var x = 1;
var y = 0;
	switch(x){
		case 1:
			result = x;
			break;
		case 2:
			result = y;
			break;
	}

var teste = comparar(1,0);
