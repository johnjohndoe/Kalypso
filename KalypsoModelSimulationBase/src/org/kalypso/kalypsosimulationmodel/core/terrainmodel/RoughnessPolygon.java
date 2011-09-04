/**
 * 
 */
package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypso.kalypsosimulationmodel.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

/**
 * {@link AbstractFeatureBinder} based, default implementation of {@link IRoughnessPolygon}
 * 
 * @author Dejan Antanaskovic, Patrice Congo
 */
public class RoughnessPolygon extends Feature_Impl implements IRoughnessPolygon
{

  public static final QName SIM_BASE_F_ROUGHNESS_POLYGON = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "RoughnessPolygon" ); //$NON-NLS-1$

  public static final QName SIM_BASE_PROP_ROUGHNESS_CLASS_MEMBER = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "roughnessClassMember" ); //$NON-NLS-1$

  // TODO: not a good place, move to constant in 1d2d
  public static final String DATA_LOCATION = "/.metadata/roughness.gml"; //$NON-NLS-1$

  public RoughnessPolygon( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#getLinestring()
   */
  @Override
  public GM_Surface< ? > getSurface( )
  {
    final GM_Object defaultGeometryProperty = getDefaultGeometryPropertyValue();
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
    final Feature feature = this;
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

    final Feature feature = this;
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
      final IRoughnessCls rCls = FeatureHelper.resolveLink( this, IRoughnessPolygon.PROP_ROUGHNESS_CLASS_MEMBER, IRoughnessCls.class );
      return rCls;
    }
    catch( final IllegalStateException e )
    {
      // If this happens, it is because xlink is broken (possible after deleting class from roughness database)
      setProperty( IRoughnessPolygon.PROP_ROUGHNESS_CLASS_MEMBER, null );
      return null;
    }
  }

  @Override
  public void setRoughnessClass( final IRoughnessCls reference ) throws IllegalArgumentException
  {
    if( reference == null )
      setProperty( SIM_BASE_PROP_ROUGHNESS_CLASS_MEMBER, null );
    else
    {
      final XLinkedFeature_Impl xlink = RoughnessPolygon.createClassLink( this, reference );
      setProperty( SIM_BASE_PROP_ROUGHNESS_CLASS_MEMBER, xlink );
    }
  }

  @Override
  public String toString( )
  {
    final Feature feature = this;
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
    return (Double) getProperty( IRoughnessPolygon.PROP_CORRECTION_AXAY );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#getCorrectionParameterDP()
   */
  @Override
  public Double getCorrectionParameterDP( )
  {
    return (Double) getProperty( IRoughnessPolygon.PROP_CORRECTION_DP );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#getCorrectionParameterKS()
   */
  @Override
  public Double getCorrectionParameterKS( )
  {
    return (Double) getProperty( IRoughnessPolygon.PROP_CORRECTION_KS );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#setCorrectionParameterAxAy(double)
   */
  @Override
  public void setCorrectionParameterAxAy( final double value )
  {
    setProperty( IRoughnessPolygon.PROP_CORRECTION_AXAY, value );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#setCorrectionParameterDP(double)
   */
  @Override
  public void setCorrectionParameterDP( final double value )
  {
    setProperty( IRoughnessPolygon.PROP_CORRECTION_DP, value );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#setCorrectionParameterKS(double)
   */
  @Override
  public void setCorrectionParameterKS( final double value )
  {
    setProperty( IRoughnessPolygon.PROP_CORRECTION_KS, value );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon#resetRoughnessClassMemberXLink()
   */
  @Override
  public FeatureChange[] resetRoughnessClassMemberXLink( )
  {
    final Feature feature = this;

    setRoughnessClass( null );
    feature.setProperty( PROP_ROUGHNESS_STYLE, null );

    final IFeatureType featureType = feature.getFeatureType();

    final FeatureChange[] changes = new FeatureChange[2];
    changes[0] = new FeatureChange( feature, featureType.getProperty( PROP_ROUGHNESS_CLASS_MEMBER ), null );
    changes[1] = new FeatureChange( feature, featureType.getProperty( PROP_ROUGHNESS_STYLE ), null );

    return changes;
  }

  public static XLinkedFeature_Impl createClassLink( final Feature polygonFeature, final Feature classFeature )
  {
    final StringBuffer xlinkBuffer = new StringBuffer( 50 );
    xlinkBuffer.append( "project:" ).append( DATA_LOCATION ).append( "#" ).append( classFeature.getId() ).trimToSize(); //$NON-NLS-1$ //$NON-NLS-2$
    return new XLinkedFeature_Impl( polygonFeature, classFeature.getParentRelation(), classFeature.getFeatureType(), xlinkBuffer.toString(), "", "", "", "", "" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
  }
}
