package org.kalypso.ui.editor.styleeditor.dialogs.filterdialog;

public class FilterDialogError 
{
	private FilterDialogTreeNode node = null;
	private String faultCode = null;
		
	public static final String INCOMPLETE = "Input incomplete!";
	public static final String NUMERIC_VALUE = "needs to be numeric!";
	public static final String LOWERBOUNDARY_EXCEEDS_UPPERBOUNDARY = "Lower boundary cannot exceed upper boundary!";
	public static final String DATA_NULL = "No data input given or possible";
	
	public FilterDialogError(FilterDialogTreeNode node, String faultCode)
	{
		this.node = node;
		this.faultCode = faultCode;
	}
	
	public String getFaultCode() {
		return faultCode;
	}
	public void setFaultCode(String faultCode) {
		this.faultCode = faultCode;
	}
	public FilterDialogTreeNode getNode() {
		return node;
	}
	public void setNode(FilterDialogTreeNode node) {
		this.node = node;
	}
}
