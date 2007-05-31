package org.kalypsodeegree_impl.model.feature;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.gmlschema.property.virtual.IVirtualFunctionPropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeaturePropertyHandler;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.gml.schema.virtual.VirtualFeatureTypeProperty;
import org.kalypsodeegree_impl.gml.schema.virtual.VirtualPropertyUtilities;
import org.kalypsodeegree_impl.model.geometry.GM_Envelope_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author doemming implementation of ogc feature that supports different cardinalities of properties, but not "unbound"
 *         cardinalities (use FeatureCollections for unbound cardinalities)
 */
public class Feature_Impl extends AbstractFeature implements Feature
{
  private final static GM_Envelope INVALID_ENV = new GM_Envelope_Impl();

  /**
   * all property-values are stored here in sequencial order (as defined in applicationschema) properties with
   * maxOccurency = 1 are stored direct properties with maxOccurency > 1 are stored in a list properties with
   * maxOccurency = "unbounded" should use FeatureCollections
   */
  private final Object[] m_properties;

  private final IFeatureType m_featureType;

  private final String m_id;

  private Object m_parent = null;

  private final IRelationType m_parentRelation;

  private GM_Envelope m_envelope = Feature_Impl.INVALID_ENV;

  protected Feature_Impl( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    if( ft == null )
    {
      throw new UnsupportedOperationException( "must provide a featuretype" );
    }

    m_parent = parent;
    m_parentRelation = parentRelation;
    m_featureType = ft;
    m_id = id;
    m_properties = propValues;
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getId()
   */
  public String getId( )
  {
    return m_id;
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getFeatureType()
   */
  public IFeatureType getFeatureType( )
  {
    return m_featureType;
  }

  /**
   * @return array of properties, properties with maxoccurency>0 (as defined in applicationschema) will be embedded in
   *         java.util.List-objects
   * @see org.kalypsodeegree.model.feature.Feature#getProperties()
   */
  public Object[] getProperties( )
  {
    return m_properties;
  }

  /**
   * format of name if "namespace:name" or just "name" - both will work
   * 
   * @return array of properties, properties with maxoccurency>0 (as defined in applicationschema) will be embedded in
   *         java.util.List-objects
   * @see org.kalypsodeegree.model.feature.Feature#getProperty(java.lang.String)
   */
  public Object getProperty( final IPropertyType pt )
  {
    if( pt == null )
    {
      throw new IllegalArgumentException( "pt may not null" );
    }

    final int pos = m_featureType.getPropertyPosition( pt );
    if( pos == -1 )
    {
      if( m_featureType.isVirtualProperty( pt ) )
      {
        final IFeaturePropertyHandler fsh = getPropertyHandler();
        return fsh.getValue( this, pt, null );
      }
      final String msg = String.format( "Unknown property (%s) for type: %s", pt, m_featureType );
      throw new IllegalArgumentException( msg );
    }

    final IFeaturePropertyHandler fsh = getPropertyHandler();

    final Object currentValue = m_properties[pos];

    return fsh.getValue( this, pt, currentValue );
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getGeometryProperties()
   */
  public GM_Object[] getGeometryProperties( )
  {
    final List<GM_Object> result = new ArrayList<GM_Object>();
    final IPropertyType[] ftp = m_featureType.getProperties();
    for( final IPropertyType element : ftp )
    {
      if( GeometryUtilities.isGeometry( element ) )
      {
        final Object o = getProperty( element );
        if( o == null )
        {
          continue;
        }
        if( o instanceof List )
        {
          result.addAll( (List) o );
        }
        else
        {
          result.add( (GM_Object) o );
        }
      }
    }

    // add virtual function properties
    final IVirtualFunctionPropertyType[] virtualFuncGeo = m_featureType.getVirtualGeometryProperties();
    for( final IVirtualFunctionPropertyType pt : virtualFuncGeo )
    {
      final Object virGeoProp = getProperty( pt );
      if( virGeoProp == null )
      {
        continue;
      }
      else if( virGeoProp instanceof List )
      {
        result.addAll( (List) virGeoProp );
      }
      else
      {
        result.add( (GM_Object) virGeoProp );
      }
    }

    // TODO allways use virtual ftp to calculate bbox ??
    final VirtualFeatureTypeProperty[] vftp = VirtualPropertyUtilities.getVirtualProperties( m_featureType );
    for( int p = 0; p < vftp.length; p++ )
    {
      if( GeometryUtilities.isGeometry( vftp[p] ) )
      {
        final Object o = getVirtuelProperty( vftp[p], null );
        if( o == null )
        {
          continue;
        }
        if( o instanceof List )
        {
          result.addAll( (List) o );
        }
        else
        {
          result.add( (GM_Object) o );
        }
      }
    }

    return result.toArray( new GM_Object[result.size()] );
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getDefaultGeometryProperty()
   */
  public GM_Object getDefaultGeometryProperty( )
  {
    final int pos = m_featureType.getDefaultGeometryPropertyPosition();
    if( pos < 0 )
    {
      final IVirtualFunctionPropertyType[] virtualGeometryProperties = m_featureType.getVirtualGeometryProperties();
      if( virtualGeometryProperties == null )
      {
        return null;
      }
      else if( virtualGeometryProperties.length > 0 )
      {
        return (GM_Object) getProperty( virtualGeometryProperties[0] );
      }
      else
      {
        return null;
      }
    }

    final IPropertyType property = m_featureType.getProperties( pos );
    final Object prop = getProperty( property );
    if( prop instanceof List )
    {
      final List props = (List) prop;
      return (GM_Object) (props.size() > 0 ? props.get( 0 ) : null);
    }
    if( !((prop == null) || (prop instanceof GM_Object)) )
    {
      throw new UnsupportedOperationException( "wrong geometry type" );
    }
    return (GM_Object) prop;
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getEnvelope()
   */
  public GM_Envelope getEnvelope( )
  {
    if( m_envelope == Feature_Impl.INVALID_ENV )
    {
      calculateEnv();
    }
    return m_envelope;
  }

  private void calculateEnv( )
  {
    GM_Envelope env = null;
    final GM_Object[] geoms = getGeometryProperties();
    for( int i = 0; i < geoms.length; i++ )
    {
      if( !(geoms[i] instanceof GM_Point) )
      {
        if( env == null )
        {
          env = geoms[i].getEnvelope();
        }
        else
        {
          env = env.getMerged( geoms[i].getEnvelope() );
        }
      }
      else
      {
        final GM_Position pos = ((GM_Point) geoms[i]).getPosition();
// System.out.println(geoms[i].getClass().getName());
// System.out.println("POS:"+pos);
        final GM_Envelope env2 = GeometryFactory.createGM_Envelope( pos, pos );
        if( env == null )
        {
          env = env2;
        }
        else
        {
          env = env.getMerged( env2 );
        }
      }
    }
    m_envelope = env;
  }

  // TODO: make it private again?
  public void invalidEnvelope( )
  {
    m_envelope = Feature_Impl.INVALID_ENV;

    /* Invalidate geo-index of all feature-list which contains this feature. */
    // TODO: At the moment, only the owning list is invalidated. Lists who link to this feature are invald but not
    // invalidated.
    // TODO: This code is probably not very performant. How to improve this?
    // Alternative: instead of invalidating: before every query we check if any feature-envelope is invalid
    final Feature parent = getParent();
    if( parent == null )
    {
      return;
    }

    final IRelationType rt = getParentRelation();
    if( (rt != null) && rt.isList() )
    {
      // rt relation type and this relation type can differ (differnt feature workspaces!)
      final IRelationType relation = (IRelationType) parent.getFeatureType().getProperty( rt.getQName() );
      final FeatureList list = (FeatureList) parent.getProperty( relation );
      list.invalidate( this );
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getVirtuelProperty(java.lang.String,
   *      org.kalypsodeegree.model.feature.GMLWorkspace)
   */
  public Object getVirtuelProperty( final VirtualFeatureTypeProperty vpt, final GMLWorkspace workspace )
  {
    return vpt.getVirtuelValue( this, workspace );
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#setProperty(java.lang.String, java.lang.Object)
   */
  public void setProperty( final IPropertyType pt, final Object value )
  {
    final int pos = m_featureType.getPropertyPosition( pt );
    if( pos == -1 )
    {
      // if there is not regula property look for virtual
      // function property
      if( m_featureType.isVirtualProperty( pt ) )
      {
        final IFeaturePropertyHandler fsh = getPropertyHandler();
        fsh.setValue( this, pt, null );
        if( fsh.invalidateEnvelope( pt ) )
        {
          invalidEnvelope();
        }
      }
      else
      {
        final String message = String.format( "Feature[%s] does not know this property %s", toString(), pt.getQName().toString() );
        throw new RuntimeException( new GMLSchemaException( message ) );
      }
    }
    else
    {
      final IFeaturePropertyHandler fsh = getPropertyHandler();
      m_properties[pos] = fsh.setValue( this, pt, value );

      if( fsh.invalidateEnvelope( pt ) )
      {
        invalidEnvelope();
      }
    }
  }

  /**
   * @deprecated use getProperty(IPropertyType)
   * @see org.kalypsodeegree.model.feature.Feature#getProperty(java.lang.String)
   */
  @Deprecated
  public Object getProperty( final String propNameLocalPart )
  {
    if( propNameLocalPart.indexOf( ':' ) > 0 )
    {
      throw new UnsupportedOperationException( propNameLocalPart + " is not a localPart" );
    }

    final IPropertyType pt = m_featureType.getProperty( propNameLocalPart );
    if( pt == null )
    {
      throw new IllegalArgumentException( "unknown local part: " + propNameLocalPart );
    }

    return getProperty( pt );
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getProperty(javax.xml.namespace.QName)
   */
  public Object getProperty( final QName propQName )
  {
    /* final */final IPropertyType pt = m_featureType.getProperty( propQName );
    if( pt == null )
    {
// pt = m_featureType.getVirtualProperty(propQName);
// if(pt==null)
// {
      final String message = String.format( "Unknow property:\n\tfeatureType=%s\n\tprop QName=%s", getFeatureType().getQName(), propQName );
      throw new IllegalArgumentException( message );
// }
    }
    return getProperty( pt );
  }

  /**
   * @deprecated
   * @see org.kalypsodeegree.model.feature.Feature#setProperty(java.lang.String, java.lang.Object)
   */
  @Deprecated
  public void setProperty( final String propLocalName, final Object value )
  {
    final IPropertyType pt = FeatureHelper.getPT( this, propLocalName );
    setProperty( pt, value );
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getWorkspace()
   */
  public GMLWorkspace getWorkspace( )
  {
    if( m_parent instanceof GMLWorkspace )
    {
      return (GMLWorkspace) m_parent;
    }
    if( m_parent instanceof Feature )
    {
      return ((Feature) m_parent).getWorkspace();
    }
    return null;
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getParent()
   */
  public Feature getParent( )
  {
    if( m_parent instanceof Feature )
    {
      return (Feature) m_parent;
    }
    return null;
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getParentRelation()
   */
  public IRelationType getParentRelation( )
  {
    return m_parentRelation;
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#setWorkspace(org.kalypsodeegree.model.feature.GMLWorkspace)
   */
  public void setWorkspace( final GMLWorkspace workspace )
  {
    if( (m_parent == null) || (m_parent == workspace) )
    {
      m_parent = workspace;
    }
    else
    {
      throw new UnsupportedOperationException( "is not a root feature" );
    }
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    final StringBuffer buffer = new StringBuffer( "Feature " );
    if( m_featureType != null )
    {
      buffer.append( m_featureType.getQName().getLocalPart() );
    }
    if( m_id != null )
    {
      buffer.append( "#" + m_id );
    }
    return buffer.toString();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#setProperty(javax.xml.namespace.QName, java.lang.Object)
   */
  public void setProperty( final QName propQName, final Object value )
  {
    final IFeatureType featureType = getFeatureType();

    final IPropertyType prop = featureType.getProperty( propQName );
    if( prop == null )
    {
      throw new IllegalArgumentException( "Property not found: " + propQName );
    }

    setProperty( prop, value );
  }

  private IFeaturePropertyHandler getPropertyHandler( )
  {
    return FeaturePropertyHandlerFactory.getInstance().getHandler( getFeatureType() );
  }

}