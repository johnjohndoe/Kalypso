package org.kalypso.ogc.gml.widgets;

import org.kalypso.ogc.gml.command.JMSelector;

public class UnSelectWidget extends AbstractSelectWidget
{
  protected int getSelectionMode()
  {
     return JMSelector.MODE_UNSELECT;        
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.AbstractSelectWidget#allowOnlyOneSelectedFeature()
   */
  boolean allowOnlyOneSelectedFeature()
  {
    return false;
  }  
}