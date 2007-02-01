package org.kalypso.kalypsomodel1d2d.ui.map;

import org.kalypso.ogc.gml.map.widgets.SelectWidgetHandler;
import org.kalypso.ogc.gml.widgets.IWidget;

public class EdgeWidgetHandler extends SelectWidgetHandler
{
  /**
   * @see org.kalypso.ogc.gml.map.widgets.SelectWidgetHandler#getWidgetForName(java.lang.String)
   */
  @Override
  protected IWidget getWidgetForName( String widgetName )
  {
    IWidget widget = null;
    try
    {
      final Class<IWidget> widgetClass = (Class<IWidget>) Class.forName( widgetName );
      widget = widgetClass.newInstance();
    }
    catch( ClassNotFoundException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( InstantiationException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( IllegalAccessException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return widget;
  }
}
