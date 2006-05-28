package org.kalypsodeegree_impl.model.feature;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
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
  private Object m_parent = null;

  private final static GM_Envelope INVALID_ENV = new GM_Envelope_Impl();

  private GM_Envelope m_envelope = INVALID_ENV;

  /**
   * all property-values are stored here in sequencial order (as defined in applicationschema) properties with
   * maxOccurency = 1 are stored direct properties with maxOccurency > 1 are stored in a list properties with
   * maxOccurency = "unbounded" should use FeatureCollections
   */
  private final Object[] m_properties;

  private final IFeatureType m_featureType;

  private final String m_id;

  /**
   * Erzeugt ein Feature mit gesetzter ID und füllt das Feature mit Standardwerten.
   * 
   * @param initializeWithDefaults
   *          set <code>true</code> when generating from UserInterface <br>
   *          set <code>false</code> when generating from GML or so.
   */
  public Feature_Impl( final Object parent, final IFeatureType ft, final String id, final boolean initializeWithDefaults )
  {
    if( ft == null )
      throw new UnsupportedOperationException( "must provide a featuretype" );

    m_parent = parent;
    m_featureType = ft;
    m_id = id;

    // initialize
    final IPropertyType[] ftp = ft.getProperties();
    m_properties = new Object[ftp.length];
    for( int i = 0; i < ftp.length; i++ )
    {
      if( m_featureType.getProperties( i ).isList() )
      {
        if( ftp[i] instanceof IRelationType )
          m_properties[i] = FeatureFactory.createFeatureList( this, (IRelationType) ftp[i] );
        else
          m_properties[i] = new ArrayList();
      }
      else
        m_properties[i] = null;
    }

    if( initializeWithDefaults )
    {
      final Map<IPropertyType, Object> properties = FeatureFactory.createDefaultFeatureProperty( ftp, false );
      for( final Map.Entry<IPropertyType, Object> entry : properties.entrySet() )
      {
        final IPropertyType pt = entry.getKey();
        final Object value = entry.getValue();

        if( value != null && pt.getMaxOccurs() == 1 )
          setProperty( pt, value );
      }
    }
  }

  protected Feature_Impl( final Object parent, final IFeatureType ft, final String id, final Object[] propValues )
  {
    if( ft == null )
      throw new UnsupportedOperationException( "must provide a featuretype" );

    m_parent = parent;
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
      throw new IllegalArgumentException( "pt may not null" );

    final int pos = m_featureType.getPropertyPosition( pt );
    return getProperty( pos );
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getProperty(int)
   * @return array of properties, properties with maxoccurency>0 (as defined in applicationschema) will be embedded in
   *         java.util.List-objects
   */
  public Object getProperty( int index )
  {
    return m_properties[index];
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getGeometryProperties()
   */
  public GM_Object[] getGeometryProperties( )
  {
    final List<GM_Object> result = new ArrayList<GM_Object>();
    final IPropertyType[] ftp = m_featureType.getProperties();
    for( int p = 0; p < ftp.length; p++ )
    {
      if( GeometryUtilities.isGeometry( ftp[p] ) )
      {
        final Object o = getProperty( p );
        if( o == null )
          continue;
        if( o instanceof List )
          result.addAll( (List) o );
        else
          result.add( (GM_Object) o );
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
          continue;
        if( o instanceof List )
          result.addAll( (List) o );
        else
          result.add( (GM_Object) o );
      }
    }

    return result.toArray( new GM_Object[result.size()] );
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getDefaultGeometryProperty()
   */
  public GM_Object getDefaultGeometryProperty( )
  {
    int pos = m_featureType.getDefaultGeometryPropertyPosition();
    if( pos < 0 )
      return null;
    Object prop = m_properties[pos];
    if( prop instanceof List )
    {
      List props = (List) prop;
      return (GM_Object) (props.size() > 0 ? props.get( 0 ) : null);
    }
    if( !(prop == null || prop instanceof GM_Object) )
      throw new UnsupportedOperationException( "wrong geometry type" );
    return (GM_Object) prop;
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getEnvelope()
   */
  public GM_Envelope getEnvelope( )
  {
    if( m_envelope == INVALID_ENV )
      calculateEnv();
    return m_envelope;
  }

  private void calculateEnv( )
  {
    GM_Envelope env = null;
    GM_Object[] geoms = getGeometryProperties();
    for( int i = 0; i < geoms.length; i++ )
    {
      if( !(geoms[i] instanceof GM_Point) )
      {
        if( env == null )
          env = geoms[i].getEnvelope();
        else
          env = env.getMerged( geoms[i].getEnvelope() );
      }
      else
      {
        GM_Position pos = ((GM_Point) geoms[i]).getPosition();
        GM_Envelope env2 = GeometryFactory.createGM_Envelope( pos, pos );
        if( env == null )
          env = env2;
        else
          env = env.getMerged( env2 );
      }
    }
    m_envelope = env;
  }

  private void invalidEnvelope( )
  {
    m_envelope = INVALID_ENV;
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getVirtuelProperty(java.lang.String,
   *      org.kalypsodeegree.model.feature.GMLWorkspace)
   */
  public Object getVirtuelProperty( VirtualFeatureTypeProperty vpt, GMLWorkspace workspace )
  {
    return vpt.getVirtuelValue( this, workspace );
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#setProperty(java.lang.String, java.lang.Object)
   */
  public void setProperty( final IPropertyType pt, final Object value )
  {
    if( pt == null )
      return;

    // Check if value fits to property
    // Example1: QName is xs:double -> value must be Double
    // Exmaple2: property is list -> value must be a list
    if( pt.isList() && !(value instanceof List) )
      throw new IllegalArgumentException( "Value must be a list for qname: " + pt.getQName() );

    if( !pt.isList() )
    {
      if( pt instanceof IValuePropertyType )
      {
      //TODO:This doesn´t work - what to do?
      // REMARK: WHAT? does nto work?? This is a test if thee value fits to
      // the type of the property. Test here is necessary because if
      // we do not test here we will get later ClassCastExceptions
      // and there we do not know why.
      // Next: please contact me instead of just commenting ist out. Gernot
        // final Class< ? > valueClass = ((IValuePropertyType) pt).getTypeHandler().getValueClass();
        // if( value != null && !valueClass.isAssignableFrom( value.getClass() ) )
        // throw new IllegalArgumentException( "Wrong type of value (" + value.getClass() + ") for qname: " +
        // pt.getQName() );
      }
      else if( pt instanceof IRelationType )
      {
        if( value != null && !(value instanceof Feature) && !(value instanceof String) )
          throw new IllegalArgumentException( "Wrong type of value (" + value.getClass() + ") for qname: " + pt.getQName() );
      }
    }

    final int pos = m_featureType.getPropertyPosition( pt );
    m_properties[pos] = value;
    if( GeometryUtilities.isGeometry( pt ) )
      invalidEnvelope();
  }

  /**
   * @deprecated use getProperty(IPropertyType)
   * @see org.kalypsodeegree.model.feature.Feature#getProperty(java.lang.String)
   */
  @Deprecated
  public Object getProperty( final String propNameLocalPart )
  {
    if( propNameLocalPart.indexOf( ':' ) > 0 )
      throw new UnsupportedOperationException( propNameLocalPart + " is not a localPart" );

    final IPropertyType pt = m_featureType.getProperty( propNameLocalPart );
    if( pt == null )
      throw new IllegalArgumentException( "unknown local part: " + propNameLocalPart );

    return getProperty( pt );
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getProperty(javax.xml.namespace.QName)
   */
  public Object getProperty( QName propQName )
  {
    final IPropertyType pt = m_featureType.getProperty( propQName );
    return getProperty( pt );
  }

  /**
   * @deprecated
   * @see org.kalypsodeegree.model.feature.Feature#setProperty(java.lang.String, java.lang.Object)
   */
  @Deprecated
  public void setProperty( String propLocalName, Object value )
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
      return (GMLWorkspace) m_parent;
    if( m_parent instanceof Feature )
      return ((Feature) m_parent).getWorkspace();
    return null;
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getParent()
   */
  public Feature getParent( )
  {
    if( m_parent instanceof Feature )
      return (Feature) m_parent;
    return null;
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#setWorkspace(org.kalypsodeegree.model.feature.GMLWorkspace)
   */
  public void setWorkspace( GMLWorkspace workspace )
  {
    if( m_parent == null || m_parent == workspace )
      m_parent = workspace;
    else
      throw new UnsupportedOperationException( "is not a root feature" );
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    final StringBuffer buffer = new StringBuffer( "Feature " );
    if( m_featureType != null )
      buffer.append( m_featureType.getQName().getLocalPart() );
    if( m_id != null )
      buffer.append( "#" + m_id );
    return buffer.toString();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#setProperty(javax.xml.namespace.QName, java.lang.Object)
   */
  public void setProperty( final QName propQName, final Object value )
  {
    final IFeatureType featureType = getFeatureType();

    final IPropertyType prop = featureType.getProperty( propQName );

    setProperty( prop, value );
  }
}