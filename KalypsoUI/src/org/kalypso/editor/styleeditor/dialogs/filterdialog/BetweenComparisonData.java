/*
 * Created on 03.08.2004
 *  
 */
package org.kalypso.editor.styleeditor.dialogs.filterdialog;


public class BetweenComparisonData extends AbstractComparisonData
{
	private String lower = null;
	private String upper = null;
	
	public String getLower() {
		return lower;
	}
	public void setLower(String lower) {
		this.lower = lower.trim();
	}
	public String getUpper() {
		return upper;
	}
	public void setUpper(String upper) {
		this.upper = upper.trim();
	}

	public boolean verify() throws FilterDialogException 
	{
		if(lower == null || lower.trim().length() == 0 || upper == null || upper.trim().length() == 0 || propertyName == null)
		{
			throw new FilterDialogException(new FilterDialogError(null,FilterDialogError.INCOMPLETE));
		}
		else 
		{	
			try
			{
				double lowerDouble = Double.parseDouble(lower);
				double upperDouble = Double.parseDouble(upper);
				if(lowerDouble>upperDouble)
					throw new FilterDialogException(new FilterDialogError(null,FilterDialogError.LOWERBOUNDARY_EXCEEDS_UPPERBOUNDARY));
			}
			catch(NumberFormatException e)
			{
				throw new FilterDialogException(new FilterDialogError(null,"Lower- and UpperBound input " +FilterDialogError.NUMERIC_VALUE));
			}			
			return true;
		}			
	}
}