/*
 * Created on 03.08.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.dialogs.filterdialog;

import org.kalypso.ui.editor.styleeditor.MessageBundle;

public class FeatureIDData extends AbstractData
{
  private String featureId = null;

  public String getFeatureId()
  {
    return featureId;
  }

  public void setFeatureId( String m_featureId )
  {
    this.featureId = m_featureId.trim();
  }

  public boolean verify() throws FilterDialogException
  {
    if( featureId == null || featureId.trim().length() == 0 )
      throw new FilterDialogException( new FilterDialogError( null,
          MessageBundle.STYLE_EDITOR_FILTER_ERROR_INCOMPLETE ) );

    return true;
  }
}