package org.kalypso.ogc.widgets;

import org.kalypso.ogc.command.JMSelector;

public class SelectWidget extends AbstractSelectWidget
{
  protected int getSelectionMode()
  {
     return JMSelector.MODE_SELECT;        
  }  
}