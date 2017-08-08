import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

public class hw4 {
	
	public static void hw4(String input, String output) throws IOException{
	
		PrintStream myconsole=new PrintStream(new File(output));
		System.setOut(myconsole);
		
		BufferedReader in = new BufferedReader(new FileReader(input));
		
		ArrayList stack = new ArrayList();
		HashMap hm = new HashMap();
		String line = in.readLine();
		while(line!=null){

			if(line.equals("let")) {
				stack.add(0,parseLet(in,hm,myconsole));
			}
			else if(Character.isLetter(line.charAt(0))){
				stack = parsePrimitive(line, stack, hm, myconsole);
			}
			else if(line.charAt(0)==':'){
				stack = parseBooleanOrError(line, stack, hm);
			}
			else{
				myconsole.println("Error command!");
			}
			
			line = in.readLine();
		}
		in.close();
	}
	
	public static Object parseLet(BufferedReader in, HashMap hm_parent, PrintStream myconsole) throws IOException {
		ArrayList stack_let = new ArrayList();
		HashMap hm_let = new HashMap(hm_parent);
		String line = in.readLine();
		while(!line.equals("end")){
			
			if(line.equals("let")) {
				stack_let.add(0,parseLet(in,hm_let,myconsole));
			}
			else if(Character.isLetter(line.charAt(0))){
				stack_let = parsePrimitive(line, stack_let, hm_let, myconsole);
			}
			else if(line.charAt(0)==':'){
				stack_let = parseBooleanOrError(line, stack_let, hm_let);
			}
			else{
				myconsole.println("Error command!");
			}
			
			line = in.readLine();
		}
		return stack_let.get(0);
	}
	
	public static ArrayList parseBooleanOrError(String line, ArrayList stack, HashMap hm) {
		if (line.startsWith(":e")){
			stack.add(0, ":error:");
		}
		else if (line.startsWith(":t")){
			stack.add(0, ":true:");
		}
		else if (line.startsWith(":f")){
			stack.add(0, ":false:");
		}
		
		return stack;
	}

	public static ArrayList doMul(ArrayList stack, HashMap hm) {
		if (stack.size()<2){
			stack.add(0, ":error:");
		}
		else if (((String) stack.get(0)).charAt(0) == ':' || ((String) stack.get(1)).charAt(0) == ':'){
			stack.add(0, ":error:");
		}
		else{
			String s1 = (String) stack.get(1);
			String s0 = (String) stack.get(0);
			int x,y;
			if(s1.matches("[0-9]+")) x = Integer.parseInt(s1);
			else {
				String s1_1 = (String) hm.get(s1);
				if(s1_1 == null || !s1_1.matches("[0-9]+")) {
					stack.add(0, ":error:");
					return stack;
				}
				x = Integer.parseInt(s1_1);
			}
			if(s0.matches("[0-9]+")) y = Integer.parseInt(s0);
			else {
				String s0_1 = (String) hm.get(s0);
				if(s0_1 == null || !s0_1.matches("[0-9]+")) {
					stack.add(0, ":error:");
					return stack;
				}
				y = Integer.parseInt(s0_1);
			}
			stack.remove(0);
			stack.remove(0);
			Integer newTop = x*y;
			stack.add(0, newTop.toString());
		}
		return stack;
	}

	public static ArrayList doSub(ArrayList stack, HashMap hm) {
		if (stack.size()<2){
			stack.add(0, ":error:");
		}
		else if (((String) stack.get(0)).charAt(0) == ':' || ((String) stack.get(1)).charAt(0) == ':'){
			stack.add(0, ":error:");
		}
		else{
			String s1 = (String) stack.get(1);
			String s0 = (String) stack.get(0);
			int x,y;
			if(s1.matches("[0-9]+")) x = Integer.parseInt(s1);
			else {
				String s1_1 = (String) hm.get(s1);
				if(s1_1 == null || !s1_1.matches("[0-9]+")) {
					stack.add(0, ":error:");
					return stack;
				}
				x = Integer.parseInt(s1_1);
			}
			if(s0.matches("[0-9]+")) y = Integer.parseInt(s0);
			else {
				String s0_1 = (String) hm.get(s0);
				if(s0_1 == null || !s0_1.matches("[0-9]+")) {
					stack.add(0, ":error:");
					return stack;
				}
				y = Integer.parseInt(s0_1);
			}
			stack.remove(0);
			stack.remove(0);
			Integer newTop = x-y;
			stack.add(0, newTop.toString());
		}
		return stack;
	}

	public static ArrayList doAdd(ArrayList stack, HashMap hm) {
		if (stack.size()<2){
			stack.add(0, ":error:");
		}
		else if (((String) stack.get(0)).charAt(0) == ':' || ((String) stack.get(1)).charAt(0) == ':'){
			stack.add(0, ":error:");
		}
		else{
			String s1 = (String) stack.get(1);
			String s0 = (String) stack.get(0);
			int x,y;
			if(s1.matches("(.*)[0-9]+")) x = Integer.parseInt(s1);
			else {
				String s1_1 = (String) hm.get(s1);
				if(s1_1 == null || !s1_1.matches("[0-9]+")) {
					stack.add(0, ":error:");
					return stack;
				}
				x = Integer.parseInt(s1_1);
			}
			if(s0.matches("(.*)[0-9]+")) y = Integer.parseInt(s0);
			else {
				String s0_1 = (String) hm.get(s0);
				if(s0_1 == null || !s0_1.matches("[0-9]+")) {
					stack.add(0, ":error:");
					return stack;
				}
				y = Integer.parseInt(s0_1);
			}
			stack.remove(0);
			stack.remove(0);
			Integer newTop = x+y;
			stack.add(0, newTop.toString());
		}
		return stack;
	}

	public static ArrayList parsePrimitive(String line, ArrayList stack, HashMap hm, PrintStream myconsole){
		if (line.startsWith("add")){
			stack = doAdd(stack,hm);
		}
		else if (line.startsWith("sub")){
			stack = doSub(stack,hm);
		}
		else if (line.startsWith("mul")){
			stack = doMul(stack,hm);
		}
		else if (line.startsWith("div")){
			stack = doDiv(stack,hm);
		}
		else if (line.startsWith("rem")){
			stack = doRem(stack, hm);
		}
		else if (line.startsWith("pop")){
			stack = doPop(stack);
		}
		else if (line.startsWith("push")){
			stack = doPush(stack, line);
		}
		else if (line.startsWith("swap")){
			stack = doSwap(stack);
		}
		else if (line.startsWith("neg")){
			stack = doNeg(stack,hm);
		}
		else if (line.startsWith("quit")){
			doQuit(stack, myconsole);
		}
		else if (line.startsWith("if")) {
			doIf(stack,hm);
		}
		else if (line.startsWith("not")) {
			doNot(stack,hm);
		}
		else if (line.startsWith("and")) {
			doAnd(stack,hm);
		}
		else if (line.startsWith("or")) {
			doOr(stack,hm);
		}			
		else if (line.startsWith("equal")) {
			doEqual(stack,hm);
		}
		else if (line.startsWith("lessThan")) {
			doLessThan(stack,hm);
		}
		else if (line.startsWith("bind")) {
			doBind(stack,hm);
		}
		
		return stack;
	}

	public static void doQuit(ArrayList stack, PrintStream myconsole) {
		for (int i = 0; i < stack.size(); i++){
			String s = (String) stack.get(i);
			myconsole.println(s.replace("\"",""));
		}
		myconsole.close();
	}

	public static ArrayList doNeg(ArrayList stack, HashMap hm) {
		if (stack.isEmpty()){
			stack.add(0, ":error:");
		}
		else if (((String) stack.get(0)).charAt(0) == ':'){
			stack.add(0, ":error:");
		}
		else{
            int x;
            String s0 = (String) stack.get(0);
            if(s0.matches("(.*)[0-9]+")) x = Integer.parseInt(s0);
			else {
				String s0_1 = (String) hm.get(s0);
				if(s0_1 == null || !s0_1.matches("[0-9]+")) {
                    stack.add(0, ":error:");
					return stack;
				}
				x = Integer.parseInt(s0_1);
            }
			Integer newTop = -1*x;
			stack.remove(0);
			stack.add(0, newTop.toString());
        }
		return stack;
	}

	private static ArrayList doSwap(ArrayList stack) {
		if (stack.size() < 2){
			stack.add(0, ":error:");
		}
		else{
			String x = (String) stack.get(1);
			String y = (String) stack.get(0);
			stack.remove(0);
			stack.remove(0);
			stack.add(0, y);
			stack.add(0, x);
		}
		return stack;
	}

	public static ArrayList doPush(ArrayList stack, String line) {

		String getNum = line.substring(5);
		
		if (getNum.charAt(0) == '-'){
			if (getNum.substring(1).equals("0")){
				stack.add("0");
			}
			else if (getNum.substring(1).matches("[0-9]+")){
				stack.add(0, getNum);
			}
			else{
				stack.add(0, ":error:");
			}
		}
		else if (getNum.matches("[0-9]+")){
			stack.add(0, getNum);
		}
		else if (getNum.matches("^[a-zA-Z].*")){
			stack.add(0, getNum);
		}
		else if (getNum.matches("^\".+\"$")){
			stack.add(0, getNum);
		}
		else{
			stack.add(0, ":error:");
		}
		return stack;
	}

	public static ArrayList doPop(ArrayList stack) {
		if (stack.size() < 1){
			stack.add(0, ":error:");
		}
		else{
			stack.remove(0);
		}
		return stack;
	}

	public static ArrayList doRem(ArrayList stack, HashMap hm) {
		if (stack.size()<2){
			stack.add(0, ":error:");
		}
		else if (((String) stack.get(0)).charAt(0) == ':' || ((String) stack.get(1)).charAt(0) == ':'){
			stack.add(0, ":error:");
		}
		else{
			String s1 = (String) stack.get(1);
			String s0 = (String) stack.get(0);
			int x,y;
			if(s1.matches("[0-9]+")) x = Integer.parseInt(s1);
			else {
				String s1_1 = (String) hm.get(s1);
				if(s1_1 == null || !s1_1.matches("[0-9]+")) {
					stack.add(0, ":error:");
					return stack;
				}
				x = Integer.parseInt(s1_1);
			}
			if(s0.matches("[0-9]+")) y = Integer.parseInt(s0);
			else {
				String s0_1 = (String) hm.get(s0);
				if(s0_1 == null || !s0_1.matches("[0-9]+")) {
					stack.add(0, ":error:");
					return stack;
				}
				y = Integer.parseInt(s0_1);
			}
			if (y == 0){
				stack.add(0, ":error:");
			}
			else{
				stack.remove(0);
				stack.remove(0);
				Integer newTop = x%y;
				stack.add(0, newTop.toString());
			}
		}
		return stack;
	}

	public static ArrayList doDiv(ArrayList stack, HashMap hm) {
		if (stack.size()<2){
			stack.add(0, ":error:");
		}
		else if (((String) stack.get(0)).charAt(0) == ':' || ((String) stack.get(1)).charAt(0) == ':'){
			stack.add(0, ":error:");
		}
		else{
			String s1 = (String) stack.get(1);
			String s0 = (String) stack.get(0);
			int x,y;
			if(s1.matches("[0-9]+")) x = Integer.parseInt(s1);
			else {
				String s1_1 = (String) hm.get(s1);
				if(s1_1 == null || !s1_1.matches("[0-9]+")) {
					stack.add(0, ":error:");
					return stack;
				}
				x = Integer.parseInt(s1_1);
			}
			if(s0.matches("[0-9]+")) y = Integer.parseInt(s0);
			else {
				String s0_1 = (String) hm.get(s0);
				if(s0_1 == null || !s0_1.matches("[0-9]+")) {
					stack.add(0, ":error:");
					return stack;
				}
				y = Integer.parseInt(s0_1);
			}
			if (y == 0){
				stack.add(0, ":error:");
			}
			else{
				stack.remove(0);
				stack.remove(0);
				Integer newTop = x/y;
				stack.add(0, newTop.toString());
			}
		}
		return stack;
	}
	
	public static ArrayList doIf(ArrayList stack, HashMap hm) {
		if (stack.size()<3){
			stack.add(0, ":error:");
		}
		else {
            String s2 = (String) stack.get(2);
            
			if(!s2.equals(":true:") && !s2.equals(":false:")) s2 = (String) hm.get(s2);
            if (s2 != null && s2.equals(":true:")) {
                stack.remove(2);
                stack.remove(1);
				
            }
            else if (s2 != null && s2.equals(":false:")) {
                stack.remove(2);
                stack.remove(0);
				
            }
            else{
                stack.add(0, ":error:");
            }
        }
		return stack;
	}
	
	public static ArrayList doNot(ArrayList stack, HashMap hm) {
		if (stack.size()<1){
			stack.add(0, ":error:");
		}
        else {
            String s0 = (String) stack.get(0);
            if(!s0.equals(":true:") && !s0.equals(":false:")) s0 = (String) hm.get(s0);
            if(s0 == null){
                stack.add(0, ":error:");
            }
            else if (s0.equals(":true:")) {
                stack.remove(0);
                stack.add(0, ":false:");
            }
            else if (s0.equals(":false:")) {
                stack.remove(0);
                stack.add(0, ":true:");
            }
            else{
                stack.add(0, ":error:");
            }
        }
		return stack;
	}
	
	public static ArrayList doAnd(ArrayList stack, HashMap hm) {
		if (stack.size()<2){
			stack.add(0, ":error:");
		}
		else {
			String s0 = (String) stack.get(0);
            if(!s0.equals(":true:") && !s0.equals(":false:")) s0 = (String) hm.get(s0);
			String s1 = (String) stack.get(1);
            if(!s1.equals(":true:") && !s1.equals(":false:")) s1 = (String) hm.get(s1);
            if(s0 == null || s1 == null) {
                stack.add(0, ":error:");
            }
			else if (s0.equals(":false:") && s1.equals(":false:")) {
				stack.remove(0);
				stack.remove(0);
				stack.add(0, ":false:");
			}
			else if (s0.equals(":false:") && s1.equals(":true:")) {
				stack.remove(0);
				stack.remove(0);
				stack.add(0, ":false:");
			}
			else if (s0.equals(":true:") && s1.equals(":false:")) {
				stack.remove(0);
				stack.remove(0);
				stack.add(0, ":false:");
			}
			else if (s0.equals(":true:") && s1.equals(":true:")) {
				stack.remove(0);
				stack.remove(0);
				stack.add(0, ":true:");
			}
			else{
				stack.add(0, ":error:");
			}
		}
		return stack;
	}
	
	public static ArrayList doOr(ArrayList stack, HashMap hm) {
		if (stack.size()<2){
			stack.add(0, ":error:");
		}
		else {
			String s0 = (String) stack.get(0);
            if(!s0.equals(":true:") && !s0.equals(":false:")) s0 = (String) hm.get(s0);
			String s1 = (String) stack.get(1);
            if(!s1.equals(":true:") && !s1.equals(":false:")) s1 = (String) hm.get(s1);
            if(s0 == null || s1 == null) {
                stack.add(0, ":error:");
            }
			else if (s0.equals(":false:") && s1.equals(":false:")) {
				stack.remove(0);
				stack.remove(0);
				stack.add(0, ":false:");
			}
			else if (s0.equals(":false:") && s1.equals(":true:")) {
				stack.remove(0);
				stack.remove(0);
				stack.add(0, ":true:");
			}
			else if (s0.equals(":true:") && s1.equals(":false:")) {
				stack.remove(0);
				stack.remove(0);
				stack.add(0, ":true:");
			}
			else if (s0.equals(":true:") && s1.equals(":true:")) {
				stack.remove(0);
				stack.remove(0);
				stack.add(0, ":true:");
			}
			else{
				stack.add(0, ":error:");
			}
		}
		return stack;
	}
	
	public static ArrayList doEqual(ArrayList stack, HashMap hm) {
		if (stack.size()<2){
			stack.add(0, ":error:");
		}
		else if (((String) stack.get(0)).charAt(0) == ':' || ((String) stack.get(1)).charAt(0) == ':'){
			stack.add(0, ":error:");
		}
		else{
			String s1 = (String) stack.get(1);
			String s0 = (String) stack.get(0);
			int x,y;
			if(s1.matches("[0-9]+")) x = Integer.parseInt(s1);
			else {
				String s1_1 = (String) hm.get(s1);
				if(s1_1 == null || !s1_1.matches("[0-9]+")) {
					stack.add(0, ":error:");
					return stack;
				}
				x = Integer.parseInt(s1_1);
			}
			if(s0.matches("[0-9]+")) y = Integer.parseInt(s0);
			else {
				String s0_1 = (String) hm.get(s0);
				if(s0_1 == null || !s0_1.matches("[0-9]+")) {
					stack.add(0, ":error:");
					return stack;
				}
				y = Integer.parseInt(s0_1);
			}
			stack.remove(0);
			stack.remove(0);
            if(x == y) stack.add(0, ":true:");
            else stack.add(0, ":false:");
		}
		return stack;
	}
	
	public static ArrayList doLessThan(ArrayList stack, HashMap hm) {
		if (stack.size()<2){
			stack.add(0, ":error:");
		}
		else if (((String) stack.get(0)).charAt(0) == ':' || ((String) stack.get(1)).charAt(0) == ':'){
			stack.add(0, ":error:");
		}
		else{
			String s1 = (String) stack.get(1);
			String s0 = (String) stack.get(0);
			int x,y;
			if(s1.matches("[0-9]+")) x = Integer.parseInt(s1);
			else {
				String s1_1 = (String) hm.get(s1);
				if(s1_1 == null || !s1_1.matches("[0-9]+")) {
					stack.add(0, ":error:");
					return stack;
				}
				x = Integer.parseInt(s1_1);
			}
			if(s0.matches("[0-9]+")) y = Integer.parseInt(s0);
			else {
				String s0_1 = (String) hm.get(s0);
				if(s0_1 == null || !s0_1.matches("[0-9]+")) {
					stack.add(0, ":error:");
					return stack;
				}
				y = Integer.parseInt(s0_1);
			}
			stack.remove(0);
			stack.remove(0);
            if(x < y) stack.add(0, ":true:");
            else stack.add(0, ":false:");
		}
		return stack;
	}
	
	public static ArrayList doBind(ArrayList stack, HashMap hm) {
		if (stack.size()<2){
			stack.add(0, ":error:");
		}
		else {
			String s0 = (String) stack.get(0);
			String s1 = (String) stack.get(1);
			if(s1.matches("^[a-zA-Z].*") && !s0.equals(":error:")) {
				stack.remove(0);
				stack.remove(0);
				Object o = hm.get(s0);
				if(o!=null) hm.put(s1,o);
				else hm.put(s1,s0);
				stack.add(0, ":unit:");
			}
			else {
				stack.add(0, ":error:");
			}
		}
		return stack;
	}
}
