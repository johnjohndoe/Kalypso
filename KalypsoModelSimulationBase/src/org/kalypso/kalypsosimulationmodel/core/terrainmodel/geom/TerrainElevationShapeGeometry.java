package org.kalypso.kalypsosimulationmodel.core.terrainmodel.geom;

import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.NativeTerrainElevationModelWrapper;
import org.kalypso.kalypsosimulationmodel.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Function property to provide shape for native terrain elevation
 * 
 * @author Manadagopal
 * @author Patrice Congo
 */
public class TerrainElevationShapeGeometry extends FeaturePropertyFunction
{
  /**
   * @see org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction#init(java.util.Map)
   */
  @Override
  public void init( Map<String, String> properties )
  {
    // nothing to do
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  @Override
  public Object getValue( final Feature feature, final IPropertyType pt, final Object currentValue )
  {
    // System.out.println("getting shape:"+feature);
    try
    {
      final IFeatureType featureType = feature.getFeatureType();
      QName featureQName = featureType.getQName();
      if( NativeTerrainElevationModelWrapper.SIM_BASE_F_NATIVE_TERRAIN_ELE_WRAPPER.equals( featureQName ) )
      {
        // transform the bounding box into a curve and return it
        ITerrainElevationModel terrainElevationModel = (ITerrainElevationModel) feature.getAdapter( ITerrainElevationModel.class );
        GM_Envelope bBox = terrainElevationModel.getBoundingBox();
        // GM_Position[] positions = new GM_Position[4];
        GM_Position min = bBox.getMin();
        GM_Position max = bBox.getMax();
        double minx = min.getX();
        double miny = min.getY();

        double maxx = max.getX();
        double maxy = max.getY();

        double[] coords = new double[] { minx, miny, maxx, miny, maxx, maxy, minx, maxy, minx, miny, };
        GM_Curve curve = GeometryFactory.createGM_Curve( coords, 2, terrainElevationModel.getCoordinateSystem() );

        return curve;
      }
      else
      {
        return null;
      }
    }
    catch( final Throwable e )
    {
      // final IStatus status = StatusUtilities.statusFromThrowable( e );
      // KalypsoModelSimulationBaseConsts.getDefault().getLog().log( status );
      e.printStackTrace();
      return null;
    }
  }

  public static final Object toGM_Curve( GM_Envelope bBox, String crs )
  {
    // System.out.println("getting shape:"+feature);
    try
    {
      GM_Position min = bBox.getMin();
      GM_Position max = bBox.getMax();

      double minx = min.getX();
      double miny = min.getY();

      double maxx = max.getX();
      double maxy = max.getY();

      double[] coords = new double[] { minx, miny, maxx, miny, maxx, maxy, minx, maxy, minx, miny, };
      GM_Curve curve = GeometryFactory.createGM_Curve( coords, 2, crs );

      return curve;
    }
    catch( final Throwable e )
    {
      throw new RuntimeException( Messages.getString( "org.kalypso.kalypsosimulationmodel.core.terrainmodel.geom.TerrainElevationShapeGeometry.0" ), e ); //$NON-NLS-1$
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#setValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  @Override
  public Object setValue( final Feature feature, final IPropertyType pt, final Object valueToSet )
  {
    // TODO: change underlying node geometry?
    return valueToSet;
  }

}
