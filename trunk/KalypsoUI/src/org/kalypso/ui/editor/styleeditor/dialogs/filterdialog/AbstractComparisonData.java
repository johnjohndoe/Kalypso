/*
 * Created on 03.08.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.dialogs.filterdialog;

public abstract class AbstractComparisonData extends AbstractData
{
  protected String propertyName = null;

  public String getPropertyName()
  {
    return propertyName;
  }

  public void setPropertyName( String m_propertyName )
  {
    this.propertyName = m_propertyName.trim();
  }

  public abstract boolean verify() throws FilterDialogException;
}