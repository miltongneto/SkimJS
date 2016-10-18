function coutLen (array, i) {
    if (array.head == null) return i;
    return coutLen(array.tail, i+1);
}
function len (array) {
    return coutLen(array, 0);
}
var a = [1,2,3,4,3,1,5];

function concatSorted(x, y)
{
	var temp = [];
	while(true)
	{
		if(len(x) == 0 && len(y) == 0) break;
		
		if(len(x) == 0)
		{
			temp = temp.concat(y);
			break;
		}
		
		if(len(y) == 0)
		{
			temp = temp.concat(x);
			break;
		}
		
		if(x.head < y.head)
		{
			temp = temp.concat(x.head);
			x = x.tail;
		}
		else 
		{
			temp = temp.concat(y.head);
			y = y.tail;
		}
	}
	
	return temp;
}

function mergesort(x, s, e)
{
	if(s == e) return [x[s]];

	var half = (s + e) / 2;
	var left = mergesort(x, s, half);
	var right = mergesort(x, half + 1, e);

	return concatSorted(left, right);
}
var x = [2, 1, 4, 3, 9, 5, 0, 7, 8];
var tam = len(x)-1;
mergesort(x, 0, tam);