/**
 * 
 */
package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypso.kalypsosimulationmodel.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * {@link AbstractFeatureBinder} based, default implementation of {@link IRoughnessPolygon}
 * 
 * @author Dejan Antanaskovic, Patrice Congo
 */
public class RoughnessPolygon extends AbstractFeatureBinder implements IRoughnessPolygon
{
  public static final QName SIM_BASE_F_ROUGHNESS_POLYGON = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "RoughnessPolygon" ); //$NON-NLS-1$

  public static final QName SIM_BASE_PROP_ROUGHNESS_CLASS_MEMBER = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "roughnessClassMember" ); //$NON-NLS-1$

  /**
   * Create a RoughnessPolygon object base on an existing feature
   * 
   * @param feature
   * @throws IllegalArgumentException
   *           if feature is null and not of the appopriate type
   */
  public RoughnessPolygon( final Feature featureToBind )
  {
    super( featureToBind, SIM_BASE_F_ROUGHNESS_POLYGON );
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
    super( FeatureHelper.resolveLink( parentFeature, linkPropQName ), SIM_BASE_F_ROUGHNESS_POLYGON );
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#getLinestring()
   */
  @Override
  public GM_Surface< ? > getSurface( )
  {
    final GM_Object defaultGeometryProperty = getFeature().getDefaultGeometryPropertyValue();
    if( defaultGeometryProperty instanceof GM_Surface< ? > )
      return (GM_Surface< ? >) defaultGeometryProperty;
    return null;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#getRoughnessID()
   */
  @Override
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
      throw new RuntimeException( Messages.getString( "org.kalypso.kalypsosimulationmodel.core.terrainmodel.RoughnessPolygon.3" ) + "\n\ttype=" + style.getClass() + "\n\tvalue=" + style ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    }
  }

  @Override
  public void setSurface( final GM_Surface< ? > surface ) throws IllegalArgumentException
  {
    Assert.throwIAEOnNull( surface, Messages.getString( "org.kalypso.kalypsosimulationmodel.core.terrainmodel.RoughnessPolygon.7" ) ); //$NON-NLS-1$

    final Feature feature = getFeature();
    final IPropertyType geometryProperty = feature.getFeatureType().getDefaultGeometryProperty();
    // final IPropertyType geometryProperty = feature.getFeatureType().getProperty( IRoughnessPolygon.PROP_GEOMETRY );
    feature.setProperty( geometryProperty, surface );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#getRoughnessCls()
   */
  @Override
  public IRoughnessCls getRoughnessCls( )
  {
    try
    {
      final IRoughnessCls rCls = FeatureHelper.resolveLink( getFeature(), IRoughnessPolygon.PROP_ROUGHNESS_CLASS_MEMBER, IRoughnessCls.class );
      return rCls;
    }
    catch( IllegalStateException e )
    {
      // If this happens, it is because xlink is broken (possible after deleting class from roughness database)
      getFeature().setProperty( IRoughnessPolygon.PROP_ROUGHNESS_CLASS_MEMBER, null );
      return null;
    }
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#setRoughnessClassMember(org.kalypso.gmlschema.property.relation.RelationType)
   */
  @Override
  public void setRoughnessClassMember( final Feature linkedFeature ) throws IllegalArgumentException
  {
    final Feature feature = getFeature();
    feature.setProperty( SIM_BASE_PROP_ROUGHNESS_CLASS_MEMBER, linkedFeature );
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
    buf.append( ", location=" ); //$NON-NLS-1$
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

      final GM_Surface< ? > pol1 = getSurface();
      final GM_Surface< ? > pol2 = ((IRoughnessPolygon) obj).getSurface();
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
  @Override
  public Double getCorrectionParameterAxAy( )
  {
    return (Double) getFeature().getProperty( IRoughnessPolygon.PROP_CORRECTION_AXAY );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#getCorrectionParameterDP()
   */
  @Override
  public Double getCorrectionParameterDP( )
  {
    return (Double) getFeature().getProperty( IRoughnessPolygon.PROP_CORRECTION_DP );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#getCorrectionParameterKS()
   */
  @Override
  public Double getCorrectionParameterKS( )
  {
    return (Double) getFeature().getProperty( IRoughnessPolygon.PROP_CORRECTION_KS );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#setCorrectionParameterAxAy(double)
   */
  @Override
  public void setCorrectionParameterAxAy( double value )
  {
    getFeature().setProperty( IRoughnessPolygon.PROP_CORRECTION_AXAY, value );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#setCorrectionParameterDP(double)
   */
  @Override
  public void setCorrectionParameterDP( double value )
  {
    getFeature().setProperty( IRoughnessPolygon.PROP_CORRECTION_DP, value );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#setCorrectionParameterKS(double)
   */
  @Override
  public void setCorrectionParameterKS( double value )
  {
    getFeature().setProperty( IRoughnessPolygon.PROP_CORRECTION_KS, value );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#resetRoughnessClassMemberXLink()
   */
  @Override
  public FeatureChange[] resetRoughnessClassMemberXLink( )
  {
    final Feature feature = getFeature();

    setRoughnessClassMember( null );
    feature.setProperty( PROP_ROUGHNESS_STYLE, null );

    final IFeatureType featureType = feature.getFeatureType();

    final FeatureChange[] changes = new FeatureChange[2];
    changes[0] = new FeatureChange( feature, featureType.getProperty( PROP_ROUGHNESS_CLASS_MEMBER ), null );
    changes[1] = new FeatureChange( feature, featureType.getProperty( PROP_ROUGHNESS_STYLE ), null );

    return changes;
  }
}
