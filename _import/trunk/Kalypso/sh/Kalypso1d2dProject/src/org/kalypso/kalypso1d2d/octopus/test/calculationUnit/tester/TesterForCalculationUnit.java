package org.kalypso.kalypso1d2d.octopus.test.calculationUnit.tester;

import org.kalypso.kalypso1d2d.octopus.test.calculationUnit.MergeBoundaryCondition;
import org.kalypso.kalypso1d2d.octopus.test.calculationUnit.MergeBoundaryLine;
import org.kalypso.kalypso1d2d.octopus.test.calculationUnit.MergeCalculationUnit;



public class TesterForCalculationUnit {
	public static void main(String args[]){
		MergeBoundaryLine bline = new MergeBoundaryLine();
		MergeBoundaryLine bline1 = new MergeBoundaryLine();
		MergeBoundaryLine bline2 = new MergeBoundaryLine();
		
		MergeCalculationUnit calc = new MergeCalculationUnit();
		bline.addToBoundaryCondition(calc,new MergeBoundaryCondition());
		//bline.addToBoundaryCondition(new BoundaryCondition());
		//bline.addToBoundaryCondition(new BoundaryCondition());
		
		calc.addToBoundaryLine(bline);
		calc.addToBoundaryLine(bline1);
		//calc.addToBoundaryLine(bline2);
		
		System.out.println(calc.checkAllInvariants());
		System.out.println(bline.checkAllInvariants());
		
	}
}
