package org.kalypso.editor.mapeditor.actiondelegates;

import org.kalypso.ogc.widgets.ZoomInByRectWidget;


/**
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp</a>
 */
public class ZoomInByRectWidgetDelegate extends GisMapEditorWidgetActionDelegate
{
  public ZoomInByRectWidgetDelegate(  )
  {
    super( new ZoomInByRectWidget() );
  }
}
