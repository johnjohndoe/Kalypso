package de.tuhh.wb.javagis.AutomateCalibration;

public class CalContext {
	
	private double upperBound;
	private double lowerBound;
	private double paramValue;
	private double synteticValue;
	private String mode;
	private String[] xPath;
	
	public CalContext() {

	}
	
	public void setUpperbound(double value){
		upperBound = value;
	}
	public double getUpperBound(){
		return upperBound;
	}
	public void setLowerbound(double value){
		lowerBound = value;
	}
	public double getLowerBound(){
		return lowerBound;
	}
	public void setParamValue(double value){
		paramValue = value;
	}
	public double getParamValue(){
		return paramValue;
	}
	public void setSynteticValue(double value){
		synteticValue = value;
	}
	public double getSynteticValue(){
		return synteticValue;
	}
	public void setMode(String value){
		mode = value;
	}
	public String getMode(){
		return mode;
	}
	public void setXPath(String[] xPaths){
		xPath = new String[xPaths.length];
		xPath = xPaths;
	}
	public String[] getxPaths(){
		return xPath;
	}
	
}
