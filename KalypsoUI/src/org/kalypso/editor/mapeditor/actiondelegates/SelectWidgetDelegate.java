package org.kalypso.editor.mapeditor.actiondelegates;

import org.kalypso.ogc.widgets.SelectWidget;


/**
 * @author belger
 */
public class SelectWidgetDelegate extends GisMapEditorWidgetActionDelegate
{
  public SelectWidgetDelegate(  )
  {
    super( new SelectWidget() );
  }
}
