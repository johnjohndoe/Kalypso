/*
 * Created on 03.08.2004
 *  
 */
package org.kalypso.editor.styleeditor.dialogs.filterdialog;


public class BinaryComparisonData extends AbstractComparisonData
{
	private String literal = null;
	public String getLiteral() {
		return literal;
	}
	public void setLiteral(String literal) {
		this.literal = literal.trim();
	}
	
	public boolean verify() throws FilterDialogException 
	{
		if(literal == null || literal.trim().length() == 0 || propertyName == null)
		{
			throw new FilterDialogException(new FilterDialogError(null,FilterDialogError.INCOMPLETE));
		}
		else 
			return true;		
	}	
}