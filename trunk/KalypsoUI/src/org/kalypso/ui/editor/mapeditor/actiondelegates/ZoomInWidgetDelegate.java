package org.kalypso.ui.editor.mapeditor.actiondelegates;

import org.kalypso.ogc.gml.widgets.ZoomInWidget;


/**
 * @author belger
 */
public class ZoomInWidgetDelegate extends AbstractWidgetActionDelegate
{
  public ZoomInWidgetDelegate(  )
  {
    super( new ZoomInWidget() );
  }
}
