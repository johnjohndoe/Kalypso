package org.kalypso.ogc.widgets;

import org.kalypso.ogc.command.JMSelector;

public class SingleElementSelectWidget extends AbstractSelectWidget
{
  protected int getSelectionMode()
  {
    return JMSelector.MODE_SELECT;
  }

  protected boolean allowOnlyOneSelectedFeature()
  {
    return true;
  }
}