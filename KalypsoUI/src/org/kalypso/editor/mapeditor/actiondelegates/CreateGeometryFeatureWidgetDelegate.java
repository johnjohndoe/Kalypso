package org.kalypso.editor.mapeditor.actiondelegates;

import org.kalypso.ogc.widgets.CreateGeometryFeatureWidget;


/**
 * @author belger
 */
public class CreateGeometryFeatureWidgetDelegate extends AbstractWidgetActionDelegate
{
  public CreateGeometryFeatureWidgetDelegate(  )
  {
    super( new CreateGeometryFeatureWidget() );
  }
}
