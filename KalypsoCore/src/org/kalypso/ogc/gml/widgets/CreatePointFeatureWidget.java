/**
 * TODO: license definieren
 */

package org.kalypso.ogc.gml.widgets;

import java.awt.Point;

import org.deegree.model.feature.FeatureTypeProperty;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.util.command.ICommand;

/**
 * @author vDoemming
 */
public class CreatePointFeatureWidget extends AbstractWidget
{
//  private Point myPoint = null;

//  private final FeatureTypeProperty myFtp;

  private final CreateGeometryFeatureWidget myParentWidget;

//  private final IKalypsoFeatureTheme m_theme;

  public CreatePointFeatureWidget( CreateGeometryFeatureWidget parentWidget,
      final IKalypsoFeatureTheme theme, FeatureTypeProperty ftp )
  {
    myParentWidget = parentWidget;
    theme.getClass();
    ftp.getClass();
//    m_theme = theme;
//    myFtp = ftp;
  }

  public void leftPressed( Point p )
  {
//    myPoint = p;
    myParentWidget.perform();
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.AbstractWidget#performIntern()
   */
  public ICommand performIntern()
  {
//    if( myPoint != null )
//    {
//      final Feature feature = FeatureFactory.createFeature( "x", m_theme.getFeatureType() );
//      final GM_Position position = myParentWidget.getPosition( myPoint );
//      final CS_CoordinateSystem coordinatesSystem = myLayer.getCoordinatesSystem();
//      final GM_Object geometry = GeometryFactory.createGM_Point( position, coordinatesSystem );
//      final FeatureProperty fp = FeatureFactory.createFeatureProperty( myFtp.getName(), geometry );
//      feature.setProperty( fp );
//      return new CreateFeatureCommand( myLayer, new Feature[]
//      { feature } );
//    }
    return null;
  }
}