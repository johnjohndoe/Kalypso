/*
 * Created on 03.08.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.dialogs.filterdialog;

import org.kalypso.ui.editor.styleeditor.MessageBundle;

public class BetweenComparisonData extends AbstractComparisonData
{
  private String lower = null;

  private String upper = null;

  public String getLower()
  {
    return lower;
  }

  public void setLower( String m_lower )
  {
    this.lower = m_lower.trim();
  }

  public String getUpper()
  {
    return upper;
  }

  public void setUpper( String m_upper )
  {
    this.upper = m_upper.trim();
  }

  public boolean verify() throws FilterDialogException
  {
    if( lower == null || lower.trim().length() == 0 || upper == null || upper.trim().length() == 0
        || propertyName == null )
    {
      throw new FilterDialogException( new FilterDialogError( null,
          MessageBundle.STYLE_EDITOR_FILTER_ERROR_INCOMPLETE ) );
    }
    try
    {
      double lowerDouble = Double.parseDouble( lower );
      double upperDouble = Double.parseDouble( upper );
      if( lowerDouble > upperDouble )
        throw new FilterDialogException( new FilterDialogError( null,
            MessageBundle.STYLE_EDITOR_FILTER_ERROR_LOWERBOUNDARY_EXCEEDS_UPPERBOUNDARY ) );
    }
    catch( NumberFormatException e )
    {
      throw new FilterDialogException( new FilterDialogError( null,
          MessageBundle.STYLE_EDITOR_FILTER_ERROR_LOWER_UPPER
              + MessageBundle.STYLE_EDITOR_ERROR_NUMBER ) );
    }
    return true;

  }
}