/**
 * 
 */
package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * {@link AbstractFeatureBinder} based, default implementation of {@link IRoughnessPolygon}
 * 
 * @author Dejan Antanaskovic, Patrice Congo
 */
public class RoughnessPolygon extends AbstractFeatureBinder implements IRoughnessPolygon
{
  /**
   * Create a RoughnessPolygon object base on an existing feature
   * 
   * @param feature
   * @throws IllegalArgumentException
   *             if feature is null and not of the appopriate type
   */
  public RoughnessPolygon( final Feature featureToBind )
  {
    super( featureToBind, KalypsoModelSimulationBaseConsts.SIM_BASE_F_ROUGHNESS_POLYGON );
  }

  /**
   * Creates a new RoughnessPolygon for the passed workspace. If the workspace has root element of the type
   * RoughnessPolynomCollection than the a new Roughness polygon should be created and added to that one otherwise an
   * illegal argument exception should be thrown
   * 
   * @param workspace
   */
  public RoughnessPolygon( final Feature parentFeature, final QName linkPropQName )
  {
    super( FeatureHelper.resolveLink( parentFeature, linkPropQName ), KalypsoModelSimulationBaseConsts.SIM_BASE_F_ROUGHNESS_POLYGON );
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
      if( ((GM_Surface) object).size() <= 0 )
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
      throw new RuntimeException( Messages.getString("RoughnessPolygon.0") + "\n\ttype=" + object.getClass() + "\n\tvalue=" + object ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
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
    final Object style = feature.getProperty( IRoughnessPolygon.PROP_ROUGHNESS_STYLE );
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
      throw new RuntimeException( Messages.getString("RoughnessPolygon.3") + "\n\ttype=" + style.getClass() + "\n\tvalue=" + style ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    }
  }

  public void setSurface( final GM_MultiSurface polygon )
  {
    Assert.throwIAEOnNull( polygon, Messages.getString("RoughnessPolygon.6") ); //$NON-NLS-1$
    final Feature feature = getFeature();
    feature.setProperty( KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_ROUGHNESS_POLYGON, polygon );
  }

  public void setSurface( final GM_Object object ) throws IllegalArgumentException
  {
    Assert.throwIAEOnNull( object, Messages.getString("RoughnessPolygon.7") ); //$NON-NLS-1$
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
      throw new IllegalArgumentException( Messages.getString("RoughnessPolygon.8") + object.getClass().getName() ); //$NON-NLS-1$
    }
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#getRoughnessCls()
   */
  public IRoughnessCls getRoughnessCls( )
  {
    final IRoughnessCls rCls = FeatureHelper.resolveLink( getFeature(), KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_ROUGHNESS_CLASS_MEMBER, IRoughnessCls.class );

    return rCls;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#setRoughnessClassMember(org.kalypso.gmlschema.property.relation.RelationType)
   */
  public void setRoughnessClassMember( final Feature linkedFeature ) throws IllegalArgumentException
  {
    final Feature feature = getFeature();
    feature.setProperty( KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_ROUGHNESS_CLASS_MEMBER, linkedFeature );
  }

  @Override
  public String toString( )
  {
    final Feature feature = getFeature();
    final StringBuffer buf = new StringBuffer( 128 );
    buf.append( "RoughnessPolygon" ); //$NON-NLS-1$
    final String id = feature.getId();
    if( id != null )
    {
      buf.append( '.' );
      buf.append( id );
    }

    buf.append( "[ roughnessID=" ); //$NON-NLS-1$
    buf.append( getRoughnessStyle() );
    buf.append( ", polygonProperty=" ); //$NON-NLS-1$
    buf.append( getSurface() );
    buf.append( ']' );
    return buf.toString();
  }

  @Override
  public boolean equals( final Object obj )
  {
    if( obj instanceof IRoughnessPolygon )
    {
      final String style1 = getRoughnessStyle();
      final String style2 = ((IRoughnessPolygon) obj).getRoughnessStyle();
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

      final GM_MultiSurface pol1 = getSurface();
      final GM_MultiSurface pol2 = ((IRoughnessPolygon) obj).getSurface();
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

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#getCorrectionParameterAxAy()
   */
  public Double getCorrectionParameterAxAy( )
  {
    return (Double) getFeature().getProperty( IRoughnessPolygon.PROP_CORRECTION_AXAY );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#getCorrectionParameterDP()
   */
  public Double getCorrectionParameterDP( )
  {
    return (Double) getFeature().getProperty( IRoughnessPolygon.PROP_CORRECTION_DP );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#getCorrectionParameterKS()
   */
  public Double getCorrectionParameterKS( )
  {
    return (Double) getFeature().getProperty( IRoughnessPolygon.PROP_CORRECTION_KS );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#setCorrectionParameterAxAy(double)
   */
  public void setCorrectionParameterAxAy( double value )
  {
    getFeature().setProperty( IRoughnessPolygon.PROP_CORRECTION_AXAY, value );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#setCorrectionParameterDP(double)
   */
  public void setCorrectionParameterDP( double value )
  {
    getFeature().setProperty( IRoughnessPolygon.PROP_CORRECTION_DP, value );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#setCorrectionParameterKS(double)
   */
  public void setCorrectionParameterKS( double value )
  {
    getFeature().setProperty( IRoughnessPolygon.PROP_CORRECTION_KS, value );
  }
}
