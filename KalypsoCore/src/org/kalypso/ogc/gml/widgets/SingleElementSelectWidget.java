package org.kalypso.ogc.gml.widgets;

import org.kalypso.ogc.gml.command.JMSelector;

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