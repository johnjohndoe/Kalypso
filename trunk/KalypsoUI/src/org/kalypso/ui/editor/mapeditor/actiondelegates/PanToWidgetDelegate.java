package org.kalypso.ui.editor.mapeditor.actiondelegates;

import org.kalypso.ogc.gml.widgets.PanToWidget;


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
