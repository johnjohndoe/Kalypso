/*
 * Created on 03.08.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.dialogs;

public class NullComparisonData extends AbstractComparisonData
{

  public boolean verify()
  {
    if( propertyName != null )
      return true;
    return false;
  }
}