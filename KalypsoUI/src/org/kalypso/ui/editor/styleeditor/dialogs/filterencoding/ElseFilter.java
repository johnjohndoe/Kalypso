package org.kalypso.ui.editor.styleeditor.dialogs.filterencoding;

import org.deegree.model.feature.Feature;
import org.deegree_impl.services.wfs.filterencoding.AbstractFilter;

public class ElseFilter extends AbstractFilter
{

  public StringBuffer toXML()
  {
    return null;
  }

  public boolean evaluate( Feature feature )
  {
    return false;
  }

}