package org.kalypso.ogc.widgets;

import org.kalypso.ogc.command.JMSelector;

public class SelectWidget extends AbstractSelectWidget
{
  protected int getSelectionMode()
  {
     return JMSelector.MODE_SELECT;        
  }

  /**
   * @see org.kalypso.ogc.widgets.AbstractSelectWidget#allowOnlyOneSelectedFeature()
   */
  boolean allowOnlyOneSelectedFeature()
  {
    return false;
  }  
}