package org.kalypso.ui.editor.mapeditor.actiondelegates;

import org.kalypso.ogc.gml.widgets.CreateGeometryFeatureWidget;


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
