package org.kalypso.editor.styleeditor.dialogs.filterdialog;

public class FilterDialogException extends Exception
{
	private FilterDialogError error = null;
	
	public FilterDialogException(){}
	
	public FilterDialogException(FilterDialogError error)
	{
		super(error.getFaultCode());
		this.error = error;
	}	
	
	public FilterDialogError getError() {
		return error;
	}
}
