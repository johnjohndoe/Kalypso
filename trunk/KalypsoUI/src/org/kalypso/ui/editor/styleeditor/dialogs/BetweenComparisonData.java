/*
 * Created on 03.08.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.dialogs;

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
    this.lower = m_lower;
  }

  public String getUpper()
  {
    return upper;
  }

  public void setUpper( String m_upper )
  {
    this.upper = m_upper;
  }

  public boolean verify()
  {
    if( lower != null && upper != null && propertyName != null )
      return true;
    return false;
  }
}