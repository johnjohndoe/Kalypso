package org.kalypso.editor.mapeditor.actiondelegates;

import org.kalypso.ogc.widgets.ZoomInWidget;


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
