package org.kalypso.ogc.widgets;

import org.kalypso.ogc.command.JMSelector;

public class ToggleSelectWidget extends AbstractSelectWidget
{
  protected int getSelectionMode()
  {
    return JMSelector.MODE_TOGGLE;
  }

  /**
   * @see org.kalypso.ogc.widgets.AbstractSelectWidget#allowOnlyOneSelectedFeature()
   */
  boolean allowOnlyOneSelectedFeature()
  {
    return false;
  }
}