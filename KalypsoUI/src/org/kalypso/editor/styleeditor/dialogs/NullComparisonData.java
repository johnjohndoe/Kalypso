/*
 * Created on 03.08.2004
 *  
 */
package org.kalypso.editor.styleeditor.dialogs;


public class NullComparisonData extends AbstractComparisonData
{
	
	public boolean verify() throws FilterDialogException 
	{
		if(propertyName == null)		
			throw new FilterDialogException(new FilterDialogError(null,FilterDialogError.INCOMPLETE));
		else
			return true;
	}	
	
}