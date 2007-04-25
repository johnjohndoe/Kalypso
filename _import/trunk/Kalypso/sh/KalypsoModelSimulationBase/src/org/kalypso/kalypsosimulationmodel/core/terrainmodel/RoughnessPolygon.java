/**
 * 
 */
package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelRoughnessConsts;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfaceInterpolation;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * {@link AbstractFeatureBinder}  based, default implementation
 * of {@link IRoughnessPolygon}
 * 
 * @author Dejan Antanaskovic, Patrice Congo
 */
public class RoughnessPolygon 
                      extends AbstractFeatureBinder 
                      implements IRoughnessPolygon
{
//  // TODO: please comment! What is the reason for that implementation here?
//  // Why are you not just using GM_PolygonImpl?
//  class GM_PolygonImpl implements GM_Polygon
//  {
//    private final GM_SurfacePatch surfacePatch;
//
//    public GM_PolygonImpl( GM_SurfacePatch gmSurfacePatch )
//    {
//      this.surfacePatch = gmSurfacePatch;
//    }
//
//    /**
//     * @param gmo
//     * @return
//     * @see org.kalypsodeegree.model.geometry.GM_SurfacePatch#contains(org.kalypsodeegree.model.geometry.GM_Object)
//     */
//    public boolean contains( GM_Object gmo )
//    {
//      return surfacePatch.contains( gmo );
//    }
//
//    /**
//     * @return
//     * @see org.kalypsodeegree.model.geometry.GM_SurfacePatch#getArea()
//     */
//    public double getArea( )
//    {
//      return surfacePatch.getArea();
//    }
//
//    /**
//     * @return
//     * @see org.kalypsodeegree.model.geometry.GM_SurfacePatch#getCentroid()
//     */
//    public GM_Point getCentroid( )
//    {
//      return surfacePatch.getCentroid();
//    }
//
//    /**
//     * @return
//     * @see org.kalypsodeegree.model.geometry.GM_SurfacePatch#getCoordinateSystem()
//     */
//    public CS_CoordinateSystem getCoordinateSystem( )
//    {
//      return surfacePatch.getCoordinateSystem();
//    }
//
//    /**
//     * @return
//     * @see org.kalypsodeegree.model.geometry.GM_GenericSurface#getEnvelope()
//     */
//    public GM_Envelope getEnvelope( )
//    {
//      return surfacePatch.getEnvelope();
//    }
//
//    /**
//     * @return
//     * @see org.kalypsodeegree.model.geometry.GM_SurfacePatch#getExteriorRing()
//     */
//    public GM_Position[] getExteriorRing( )
//    {
//      return surfacePatch.getExteriorRing();
//    }
//
//    /**
//     * @return
//     * @see org.kalypsodeegree.model.geometry.GM_SurfacePatch#getInteriorRings()
//     */
//    public GM_Position[][] getInteriorRings( )
//    {
//      return surfacePatch.getInteriorRings();
//    }
//
//    /**
//     * @return
//     * @see org.kalypsodeegree.model.geometry.GM_SurfacePatch#getInterpolation()
//     */
//    public GM_SurfaceInterpolation getInterpolation( )
//    {
//      return surfacePatch.getInterpolation();
//    }
//
//    /**
//     * @return
//     * @see org.kalypsodeegree.model.geometry.GM_GenericSurface#getPerimeter()
//     */
//    public double getPerimeter( )
//    {
//      return surfacePatch.getPerimeter();
//    }
//
//    /**
//     * @param gmo
//     * @return
//     * @see org.kalypsodeegree.model.geometry.GM_SurfacePatch#intersects(org.kalypsodeegree.model.geometry.GM_Object)
//     */
//    public boolean intersects( GM_Object gmo )
//    {
//      return surfacePatch.intersects( gmo );
//    }
//
//    /**
//     * @see org.kalypsodeegree.model.geometry.GM_GenericSurface#invalidate()
//     */
//    public void invalidate( )
//    {
//      surfacePatch.invalidate();
//    }
//
//  }

//  private final Feature feature;

  /**
   * Create a RoughnessPolygon object base on an existing feature
   * 
   * @param feature
   * @throws IllegalArgumentException
   *           if feature is null and not of the appopriate type
   */
  public RoughnessPolygon( Feature featureToBind )
  {
//    Assert.throwIAEOnNull( aFeature, "Param feature must not be null" );
//    Assert.throwIAEOnNotDirectInstanceOf( aFeature, KalypsoModelSimulationBaseConsts.SIM_BASE_F_ROUGHNESS_POLYGON );
//    this.feature = aFeature;
    super(  featureToBind,
            KalypsoModelSimulationBaseConsts.SIM_BASE_F_ROUGHNESS_POLYGON );
  }

  /**
   * Creates a new RoughnessPolygon for the passed work space. if the work space has root element of the type
   * RoughnessPolynomCollection than the a new Roughness polygon should be created and added to that one otherwise an
   * illegal argument exception should be thrown
   * 
   * @param workspace
   */
  public RoughnessPolygon( Feature parentFeature, QName linkPropQName )
  {
//    Assert.throwIAEOnNull( parentFeature, "Param parentFeature must not be null" );
//    Assert.throwIAEOnNull( linkPropQName, "Parameter linkPropQName must not be null" );
//    this.feature = (Feature) parentFeature.getProperty( linkPropQName );
    super( 
        FeatureHelper.resolveLink( parentFeature, linkPropQName ),
        KalypsoModelSimulationBaseConsts.SIM_BASE_F_ROUGHNESS_POLYGON);
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#getLinestring()
   */
  public GM_MultiSurface getSurface( )
  {
    final Feature feature = getFeature();
    final Object object = feature.getProperty( KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_ROUGHNESS_POLYGON );
    if( object instanceof GM_MultiSurface )
    {
      return (GM_MultiSurface) object;
    }
    else if( object instanceof GM_Surface )
    {
      if( ((GM_Surface) object).getNumberOfSurfacePatches() <= 0 )
      {
        return null;
      }
      else
      {
        final GM_Surface surface = (GM_Surface) object;
        final GM_Surface[] surfaces = new GM_Surface[] { surface };
        return GeometryFactory.createGM_MultiSurface( surfaces, surface.getCoordinateSystem() );
      }
    }
    else if( object == null )
    {
      return null;
    }
    else
    {
      throw new RuntimeException( "GM_MultiSurface expected but got:" + "\n\ttype=" + object.getClass() + "\n\tvalue=" + object );
    }

  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#getRoughnessID()
   */
  public String getRoughnessStyle( )
  {
    final Feature feature = getFeature();
    final Object style = feature.getProperty( KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_ROUGHNESS_STYLE );
    if( style instanceof String )
    {
      return (String) style;
    }
    else if( style == null )
    {
      return null;
    }
    else
    {
      throw new RuntimeException( "String expected but got:" + "\n\ttype=" + style.getClass() + "\n\tvalue=" + style );
    }
  }

  public void setSurface( GM_MultiSurface polygon )
  {
    Assert.throwIAEOnNull( polygon, "Parameter cannot be null." );
    final Feature feature = getFeature();
    feature.setProperty( KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_ROUGHNESS_POLYGON, polygon );
  }

  public void setSurface( GM_Object object ) throws IllegalArgumentException
  {
    Assert.throwIAEOnNull( object, "Parameter cannot be null." );
    if( object instanceof GM_MultiSurface )
    {
      setSurface( (GM_MultiSurface) object );
    }
    else if( object instanceof GM_Surface )
    {
      final GM_Surface surface = (GM_Surface) object;
      final GM_Surface[] surfaces = new GM_Surface[] { surface };
      setSurface( GeometryFactory.createGM_MultiSurface( surfaces, surface.getCoordinateSystem() ) );
    }
    else
    {
      throw new IllegalArgumentException( "Type not supported: " + object.getClass().getName() );
    }
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#getRoughnessCls()
   */
  public IRoughnessCls getRoughnessCls( )
  {
    final IRoughnessCls rCls = FeatureHelper.resolveLink( 
        getFeature(), 
        KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_ROUGHNESS_CLASS_MEMBER,
        IRoughnessCls.class );
    
    return rCls;
  }
  
  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#setRoughnessClassMember(org.kalypso.gmlschema.property.relation.RelationType)
   */
  public void setRoughnessClassMember( Feature linkedFeature ) throws IllegalArgumentException
  {
    final Feature feature = getFeature();
    feature.setProperty( 
          KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_ROUGHNESS_CLASS_MEMBER, 
          linkedFeature );
  }

  @Override
  public String toString( )
  {
    final Feature feature = getFeature();
    StringBuffer buf = new StringBuffer( 128 );
    buf.append( "RoughnessPolygon" );
    String id = feature.getId();
    if( id != null )
    {
      buf.append( '.' );
      buf.append( id );
    }

    buf.append( "[ roughnessID=" );
    buf.append( getRoughnessStyle() );
    buf.append( ", polygonProperty=" );
    buf.append( getSurface() );
    buf.append( ']' );
    return buf.toString();
  }

  @Override
  public boolean equals( Object obj )
  {
    if( obj instanceof IRoughnessPolygon )
    {
      String style1 = getRoughnessStyle();
      String style2 = ((IRoughnessPolygon) obj).getRoughnessStyle();
      if( style1 != null && style2 == null )
      {
        return false;
      }
      else if( style1 == null && style2 != null )
      {
        return false;
      }
      else if( style1 == style2 )
      {
        // all null of the same or the same internalized
        // can match
      }
      else if( style1.equals( style2 ) )
      {
        // can match
      }
      else
      {
        return false;
      }

      GM_MultiSurface pol1 = getSurface();
      GM_MultiSurface pol2 = ((IRoughnessPolygon) obj).getSurface();
      if( pol1 == pol2 )
      {
        return true;
      }
      else if( pol1 != null && pol2 == null )
      {
        return false;
      }
      else if( pol1 == null && pol2 != null )
      {
        return false;
      }
      else
      {
        return pol1.equals( pol2 );
      }
    }
    else
    {
      return super.equals( obj );
    }
  }

//  public Feature getWrappedFeature( )
//  {
//    return getFeature();
//  }

//  /**
//   * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapper#getGmlID()
//   */
//  public String getGmlID( )
//  {
//    return feature.getId();
//  }

//  /**
//   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getDescription()
//   */
//  public String getDescription( )
//  {
//    // TODO Auto-generated method stub
//    return null;
//  }

//  /**
//   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getName()
//   */
//  public String getName( )
//  {
//    // TODO Auto-generated method stub
//    return null;
//  }

//  /**
//   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#setDescription(java.lang.String)
//   */
//  public void setDescription( String desc )
//  {
//    // TODO Auto-generated method stub
//    
//  }

//  /**
//   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#setName(java.lang.String)
//   */
//  public void setName( String name )
//  {
//    // TODO Auto-generated method stub
//    
//  }

}
