package org.kalypso.editor.mapeditor.actiondelegates;

import org.kalypso.ogc.widgets.PanToWidget;


/**
 * @author belger
 */
public class PanToWidgetDelegate extends AbstractWidgetActionDelegate
{
  public PanToWidgetDelegate(  )
  {
    super( new PanToWidget(  ) );
  }
}
