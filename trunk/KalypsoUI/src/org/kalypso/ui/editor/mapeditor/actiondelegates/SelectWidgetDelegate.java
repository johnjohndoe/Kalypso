package org.kalypso.ui.editor.mapeditor.actiondelegates;

import org.kalypso.ogc.gml.widgets.SelectWidget;


/**
 * @author belger
 */
public class SelectWidgetDelegate extends AbstractWidgetActionDelegate
{
  public SelectWidgetDelegate()
  {
    super( new SelectWidget( ) );
  }
}
