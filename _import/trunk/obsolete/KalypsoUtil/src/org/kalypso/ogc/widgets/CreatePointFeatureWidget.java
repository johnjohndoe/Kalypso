/**
 * TODO: license definieren
 */

package org.kalypso.ogc.widgets;

import java.awt.Point;

import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Position;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.kalypso.ogc.command.CreateFeatureCommand;
import org.kalypso.ogc.gml.KalypsoFeature;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.util.command.ICommand;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author vDoemming
 */
public class CreatePointFeatureWidget extends AbstractWidget
{
  private Point myPoint = null;

  private final KalypsoFeatureLayer myLayer;

  private final FeatureType myFt;

  private final FeatureTypeProperty myFtp;

  private final CreateGeometryFeatureWidget myParentWidget;
  
  public CreatePointFeatureWidget(CreateGeometryFeatureWidget parentWidget, KalypsoFeatureLayer layer, FeatureType ft, FeatureTypeProperty ftp )
  {
  myParentWidget=parentWidget;
  myLayer=layer;
  myFt=ft;
  myFtp=ftp;
  }

  public void leftPressed( Point p )
  {
    myPoint = p;
    myParentWidget.perform();
  }

  /**
   * @see org.kalypso.ogc.widgets.AbstractWidget#performIntern()
   */
  public ICommand performIntern()
  {
    if( myPoint != null)
    {
      final Object[] properties = new Object[myFt.getProperties().length];
      final KalypsoFeature feature = new KalypsoFeature( FeatureFactory.createFeature( "x",
          myFt, properties ) );
      final GM_Position position = myParentWidget.getPosition(myPoint);
      final CS_CoordinateSystem coordinatesSystem = myLayer.getCoordinatesSystem();
      final GM_Object geometry=GeometryFactory.createGM_Point(position, coordinatesSystem);
      final FeatureProperty fp=FeatureFactory.createFeatureProperty(myFtp.getName(),geometry);
      feature.setProperty(fp);
      return new CreateFeatureCommand( myLayer, new KalypsoFeature[]{feature} );
    }
    return null;
  }
}