package org.kalypso.ogc.gml.widgets;

import org.kalypso.ogc.gml.command.JMSelector;

public class SelectWidget extends AbstractSelectWidget
{
  protected int getSelectionMode()
  {
     return JMSelector.MODE_SELECT;        
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.AbstractSelectWidget#allowOnlyOneSelectedFeature()
   */
  boolean allowOnlyOneSelectedFeature()
  {
    return false;
  }  
}