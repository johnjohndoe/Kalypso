package org.kalypso.editor.mapeditor.actiondelegates;

import org.kalypso.ogc.widgets.PanToWidget;


/**
 * @author belger
 */
public class PanToWidgetDelegate extends GisMapEditorWidgetActionDelegate
{
  public PanToWidgetDelegate(  )
  {
    super( new PanToWidget(  ) );
  }
}
