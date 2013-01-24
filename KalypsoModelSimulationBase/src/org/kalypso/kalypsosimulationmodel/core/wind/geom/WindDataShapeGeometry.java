package org.kalypso.kalypsosimulationmodel.core.wind.geom;

import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindDataModel;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindDataModelSystem;
import org.kalypso.kalypsosimulationmodel.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Function property to provide shape for native wind data
 * 
 * @author ig
 */
public class WindDataShapeGeometry extends FeaturePropertyFunction
{
  /**
   * @see org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction#init(java.util.Map)
   */
  @Override
  public void init( Map<String, String> properties )
  {
    // nothing to do
    // System.out.println( "init: " + IWindModel.class );
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object getValue( final Feature feature, final IPropertyType pt, final Object currentValue )
  {
    try
    {
      final IFeatureType featureType = feature.getFeatureType();
      QName featureQName = featureType.getQName();
      if( KalypsoModelSimulationBaseConsts.SIM_BASE_F_WIND_ELE_SYS.equals( featureQName ) )
      {
        // System.out.println( "WindDataShapeGeometry::getting shape: " + KalypsoModelSimulationBaseConsts.SIM_BASE_F_WIND_ELE_SYS + "::" + feature );
        // transform the bounding box into a curve and return it
        IWindDataModelSystem lWindDataModelSystem = (IWindDataModelSystem) feature.getAdapter( IWindDataModelSystem.class );
        GM_Envelope bBox = lWindDataModelSystem.getGridDescriptor().getGM_Envelope( lWindDataModelSystem.getGridDescriptor().getCoordinateSystem() );// getBoundingBox();
        GM_Position min = bBox.getMin();
        GM_Position max = bBox.getMax();
        double minx = min.getX();
        double miny = min.getY();

        double maxx = max.getX();
        double maxy = max.getY();

        double[] coords = new double[] { minx, miny, maxx, miny, maxx, maxy, minx, maxy, minx, miny, };
        GM_Curve curve = GeometryFactory.createGM_Curve( coords, 2, lWindDataModelSystem.getGridDescriptor().getCoordinateSystem() );

        return curve;
      }
      else if( KalypsoModelSimulationBaseConsts.SIM_BASE_F_NATIVE_WIND_ELE_WRAPPER.equals( featureQName ) )
      {
        // transform the bounding box into a curve and return it
        IWindDataModel lWindDataModel = (IWindDataModel) feature.getAdapter( IWindDataModel.class );
        IWindDataModelSystem lWindDataModelSystem = (IWindDataModelSystem) lWindDataModel.getFeature().getParent().getAdapter( IWindDataModelSystem.class );

        GM_Envelope bBox = lWindDataModelSystem.getGridDescriptor().getGM_Envelope( lWindDataModelSystem.getGridDescriptor().getCoordinateSystem() );// getBoundingBox();
        GM_Position min = bBox.getMin();
        GM_Position max = bBox.getMax();
        double minx = min.getX();
        double miny = min.getY();

        double maxx = max.getX();
        double maxy = max.getY();

        double[] coords = new double[] { minx, miny, maxx, miny, maxx, maxy, minx, maxy, minx, miny, };
        GM_Curve curve = GeometryFactory.createGM_Curve( coords, 2, lWindDataModelSystem.getGridDescriptor().getCoordinateSystem() );

        return curve;

      }
      else
      {
        return null;
      }
    }
    catch( final Throwable e )
    {
       KalypsoCorePlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
//      e.printStackTrace();
      return null;
    }
  }

  public static final Object toGM_Curve( GM_Envelope bBox, String crs )
  {
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
  public Object setValue( final Feature feature, final IPropertyType pt, final Object valueToSet )
  {
    // TODO: change underlying node geometry?
    return valueToSet;
  }

}
