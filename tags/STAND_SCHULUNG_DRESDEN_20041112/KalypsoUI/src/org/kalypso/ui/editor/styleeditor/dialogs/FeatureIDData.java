/*
 * Created on 03.08.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.dialogs;

public class FeatureIDData extends AbstractData
{
  private String featureId = null;

  public String getFeatureId()
  {
    return featureId;
  }

  public void setFeatureId( String m_featureId )
  {
    this.featureId = m_featureId;
  }

  public boolean verify()
  {
    if( featureId != null )
      return true;
    return false;
  }
}