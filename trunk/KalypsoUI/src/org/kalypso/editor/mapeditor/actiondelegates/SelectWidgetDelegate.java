package org.kalypso.editor.mapeditor.actiondelegates;

import org.kalypso.ogc.widgets.SelectWidget;


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
