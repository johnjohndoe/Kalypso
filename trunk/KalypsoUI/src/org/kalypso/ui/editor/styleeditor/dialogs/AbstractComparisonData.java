/*
 * Created on 03.08.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.dialogs;

public abstract class AbstractComparisonData extends AbstractData
{
  protected String propertyName = null;

  public String getPropertyName()
  {
    return propertyName;
  }

  public void setPropertyName( String m_propertyName )
  {
    this.propertyName = m_propertyName;
  }

  public abstract boolean verify();
}