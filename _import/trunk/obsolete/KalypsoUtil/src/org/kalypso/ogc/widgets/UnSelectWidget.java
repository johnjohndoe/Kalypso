package org.kalypso.ogc.widgets;

import org.kalypso.ogc.command.JMSelector;

public class UnSelectWidget extends AbstractSelectWidget
{
  protected int getSelectionMode()
  {
     return JMSelector.MODE_UNSELECT;        
  }

  /**
   * @see org.kalypso.ogc.widgets.AbstractSelectWidget#allowOnlyOneSelectedFeature()
   */
  boolean allowOnlyOneSelectedFeature()
  {
    return false;
  }  
}