object sudo {
	val ar = Array.fill(9,9)(0)
	def fill(a:Array[Array[Int]],column:Int,row:Int,Input:String):String={
		if(Input.length==0)
			""
		else{
		a(column)(row)=Input.charAt(0).toString.toInt
		if(column==8)
			if(row==8)
				""
			else
				fill(a,0,row+1,Input.tail)
		else
			fill(a,column+1,row,Input.tail)
		}
	}                                         //> fill: (a: Array[Array[Int]], column: Int, row: Int, Input: String)String
	def loop(a:Array[Array[Int]],column:Int,row:Int,count:Int,loopCount:Int):Array[Array[Int]]={
			if(a(column)(row)==0){
				if(checkPossibleAnswer(a,column,row,1,0)>0)
					println(checkPossibleAnswer(a,column,row,1,0)+" "+column+" "+row)
				a(column)(row)=checkPossibleAnswer(a,column,row,1,0)
			}
			if(loopCount==4000000)
				a
			else if(row==8)
				if(column==8)
					if(sum(ar,0,0,0)==405)
						a
					else
						loop(a,0,0,0,loopCount+1)
				else
				loop(a,column+1,0,count+1,loopCount+1)
			else
				loop(a,column,row+1,count+1,loopCount+1)
	}
	def checkPossibleAnswer(a:Array[Array[Int]],column:Int,row:Int,n:Int,possibleCount:Int):Int={
		if (nExistsInRow(a,0,row,n) || nExistsInColumn(a,column,0,n) || nExistsInBox(a,column,row,n,1,column/3*3,row/3*3))
			if(n==9)
				if(possibleCount==1)
					giveOnlyPossibleAnswer(a,column,row,1)
				else 0
			else checkPossibleAnswer(a,column,row,n+1,possibleCount)
		else if (checkIfOnlyPossibleAnswer(a,column,row,n)==0)
			if(n==9){
				if(possibleCount==0)
					giveOnlyPossibleAnswer(a,column,row,1)
				else 0
			}
			else checkPossibleAnswer(a,column,row,n+1,possibleCount+1)
		else
			checkIfOnlyPossibleAnswer(a,column,row,n)
	}
	def giveOnlyPossibleAnswer(a:Array[Array[Int]],column:Int,row:Int,n:Int):Int={
		if (nExistsInRow(a,0,row,n) || nExistsInColumn(a,column,0,n) || nExistsInBox(a,column,row,n,1,column/3*3,row/3*3))
			giveOnlyPossibleAnswer(a,column,row,n+1)
		else
			n
	}
	def checkIfOnlyPossibleAnswer(a:Array[Array[Int]],column:Int,row:Int,n:Int):Int={
		if(checkRow(a,0,row,n,column) || checkColumn(a,column,0,n,row) || checkBox(a,column,row,n,1,column/3*3,row/3*3))n
		else 0
	}
	def checkBox(a:Array[Array[Int]],column:Int,row:Int,n:Int,box:Int,targetColumn:Int,targetRow:Int):Boolean={
		if (a(targetColumn)(targetRow)>0 || nExistsInRow(a,0,targetRow,n) || nExistsInColumn(a,targetColumn,0,n) || (targetColumn==column && targetRow==row))
			if (box==9)
				true
			else if(box%3==0)
				checkBox(a,column,row,n,box+1,targetColumn-2,targetRow+1)
			else
				checkBox(a,column,row,n,box+1,targetColumn+1,targetRow)
		else
			false
	}
	def checkRow(a:Array[Array[Int]],column:Int,row:Int,n:Int,origColumn:Int):Boolean={
		if(a(column)(row)>0 || nExistsInColumn(a,column,0,n) || column==origColumn || nExistsInBox(a,column,row,n,1,column/3*3,row/3*3))
			if(column==8)
				true
			else
				checkRow(a,column+1,row,n,origColumn)
		else
			false
	}
	def checkColumn(a:Array[Array[Int]],column:Int,row:Int,n:Int,origRow:Int):Boolean={
		if(a(column)(row)>0 || nExistsInRow(a,0,row,n) || row==origRow || nExistsInBox(a,column,row,n,1,column/3*3,row/3*3))
			if(row==8)
				true
			else
				checkColumn(a,column,row+1,n,origRow)
		else
			false
	}
	def nExistsInRow(a:Array[Array[Int]],column:Int,row:Int,n:Int):Boolean={
		if(a(column)(row)==n)
			true
		else if(column==8)
			false
		else
			nExistsInRow(a,column+1,row,n)
	}
	def nExistsInColumn(a:Array[Array[Int]],column:Int,row:Int,n:Int):Boolean={
		if(a(column)(row)==n)
			true
		else if(row==8)
			false
		else
			nExistsInColumn(a,column,row+1,n)
	}
	def nExistsInBox(a:Array[Array[Int]],column:Int,row:Int,n:Int,box:Int,targetColumn:Int,targetRow:Int):Boolean={
		if (a(targetColumn)(targetRow)==n)
			true
		else box match{
		case 1 => nExistsInBox(a,column,row,n,box+1,targetColumn+1,targetRow)
		case 2 => nExistsInBox(a,column,row,n,box+1,targetColumn+1,targetRow)
		case 3 => nExistsInBox(a,column,row,n,box+1,targetColumn-2,targetRow+1)
		case 4 => nExistsInBox(a,column,row,n,box+1,targetColumn+1,targetRow)
		case 5 => nExistsInBox(a,column,row,n,box+1,targetColumn+1,targetRow)
		case 6 => nExistsInBox(a,column,row,n,box+1,targetColumn-2,targetRow+1)
		case 7 => nExistsInBox(a,column,row,n,box+1,targetColumn+1,targetRow)
		case 8 => nExistsInBox(a,column,row,n,box+1,targetColumn+1,targetRow)
		case 9 => false
		}
	}
	def sum(a:Array[Array[Int]],column:Int,row:Int,acc:Int):Int={
   	if(column==8 && row==8)
   			acc+a(column)(row)
   	else if(column==8)
   		 sum(a,0,row+1,acc+a(column)(row))
   	else
   		sum(a,column+1,row,acc+a(column)(row))
   }
	def printer(a:Array[Array[Int]],column:Int,row:Int):String={
   	if(a(column)(row)==0)
   		print("* ")
   	else
   		print(a(column)(row)+" ")
   	if(column==8){
   		if (row==8)
   			""
   		else{
   		 println()
   		 printer(a,0,row+1)
   		}
   	}
   	else
   		printer(a,column+1,row)
   }
	// 006704020050600001200008004409507003000000000800201906600100007300005060020306100"590006010001254709000001400003715008100000004200648100002500000708463900050100047"
	fill(ar,0,0,"006704020050600001200008004409507003000000000800201906600100007300005060020306100")
  printer(ar,0,0)
	printer(loop(ar,0,0,0,0),0,0)
}