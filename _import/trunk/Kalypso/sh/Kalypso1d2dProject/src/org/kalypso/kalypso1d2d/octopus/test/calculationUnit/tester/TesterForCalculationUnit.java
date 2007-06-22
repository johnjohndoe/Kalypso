package org.kalypso.kalypso1d2d.octopus.test.calculationUnit.tester;

import org.kalypso.kalypso1d2d.octopus.test.calculationUnit.BoundaryCondition;
import org.kalypso.kalypso1d2d.octopus.test.calculationUnit.BoundaryLine;
import org.kalypso.kalypso1d2d.octopus.test.calculationUnit.CalculationUnit;

public class TesterForCalculationUnit {
	public static void main(String args[]){
		BoundaryLine bline = new BoundaryLine();
		BoundaryLine bline1 = new BoundaryLine();
		BoundaryLine bline2 = new BoundaryLine();
		
		bline.addToBoundaryCondition(new BoundaryCondition());
		bline.addToBoundaryCondition(new BoundaryCondition());
		bline.addToBoundaryCondition(new BoundaryCondition());
		
		CalculationUnit calc = new CalculationUnit();
		calc.addToBoundaryLine(bline);
		//calc.addToBoundaryLine(bline1);
		//calc.addToBoundaryLine(bline2);
		
		System.out.println(calc.checkAllInvariants());
		System.out.println(bline.checkAllInvariants());
		
	}
}
