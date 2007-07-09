package org.kalypsodeegree_impl.model.feature;

import javax.xml.namespace.QName;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureProvider;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.gml.schema.virtual.VirtualFeatureTypeProperty;

/**
 * A Feature implementation wich delegates all calls to another feature, proved by a feature provider.
 * <p>
 * Everything is delegated to the provided feature, except #getParent (because this freature stil lives in its own
 * structure).
 * </p>
 * <p>
 * Cannot be used as workspace root.
 * </p>
 * 
 * @author Gernot Belger
 */
public class XLinkedFeature_Impl extends AbstractFeature implements Feature
{
  private final Feature m_parentFeature;

  private final IRelationType m_parentRelation;

  private final String m_uri;

  private final String m_featureId;

  private final String m_role;

  private final String m_arcrole;

  private final String m_title;

  private final String m_show;

  private final String m_actuate;

  private IFeatureProvider m_provider;

  private final IFeatureType m_basicFeatureType;

  private IFeatureType m_featureType;

  public XLinkedFeature_Impl( final Feature parentFeature, final IRelationType parentRelation, final IFeatureType featureType, final String href, final String role, final String arcrole, final String title, final String show, final String actuate )
  {
    m_parentFeature = parentFeature;
    m_parentRelation = parentRelation;
    m_basicFeatureType = featureType;
    m_role = role;
    m_arcrole = arcrole;
    m_title = title;
    m_show = show;
    m_actuate = actuate;

    if( !href.contains( "#" ) )
    {
      m_uri = null;
      m_featureId = null;
    }
    else if( href.startsWith( "#" ) )
    {
      m_uri = null;
      m_featureId = href.substring( 1 );
    }
    else
    {
      final String[] hrefParts = href.split( "#" );
      if( hrefParts.length == 2 )
      {
        m_uri = hrefParts[0];
        m_featureId = hrefParts[1];
      }
      else
      {
        m_uri = null;
        m_featureId = null;
      }
    }

    if( m_parentFeature == null )
      throw new IllegalArgumentException( "XLinked Feature must have parent feature: " + m_parentFeature );
  }

  /** Returns the linked feature. */
  public final Feature getFeature( )
  {
    final GMLWorkspace workspace = m_parentFeature.getWorkspace();
    if( workspace == null || m_featureId == null )
    {
      // REMARK: This may happen while loading the gml, so we ignore it and all access to getFeature() should chek for
      // null
      return null;
    }

    final IFeatureProvider provider = getProvider( workspace );
    final Feature feature = provider == null ? null : provider.getFeature( m_featureId );

    if( feature == null )
      throw new IllegalStateException( "No feature found at: " + m_uri + "#" + m_featureId );

    /* The first time we access the real feature, get our real feature type. */
    if( m_featureType == null )
      m_featureType = feature.getFeatureType();

    return feature;
  }

  // TODO: this is potentially not thread-safe! Synchronize
  private IFeatureProvider getProvider( final GMLWorkspace workspace )
  {
    if( m_provider != null )
      return m_provider;

    final IFeatureProviderFactory featureProviderFactory = workspace.getFeatureProviderFactory();
    m_provider = featureProviderFactory.createFeatureProvider( m_parentFeature, m_uri, m_role, m_arcrole, m_title, m_show, m_actuate );
    return m_provider;
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getId()
   */
  public String getId( )
  {
    // return null in order to let the workspace generate internal ids
    return null;

    // do not access the provider/feature, because else we already acess the remote workspace while loading
    // the old one, this leading to dead-locks
    // final IFeatureProvider provider = getProvider( getWorkspace() );
    // return provider.getId();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getFeatureType()
   */
  public IFeatureType getFeatureType( )
  {
    /* As long as the feature was not accessed, we only know the target feature type of our defining property. */

    if( m_featureType != null )
      return m_featureType;

    return m_basicFeatureType;
  }

  /**
   * @return array of properties, properties with maxoccurency>0 (as defined in applicationschema) will be embedded in
   *         java.util.List-objects
   * @see org.kalypsodeegree.model.feature.Feature#getProperties()
   */
  @Deprecated
  public Object[] getProperties( )
  {
    final Feature feature = getFeature();
    return feature.getProperties();
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
    final Feature feature = getFeature();
    if( feature == null )
      return null;

    return feature.getProperty( pt );
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getGeometryProperties()
   */
  public GM_Object[] getGeometryProperties( )
  {
    return getFeature().getGeometryProperties();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getDefaultGeometryProperty()
   */
  public GM_Object getDefaultGeometryProperty( )
  {
    return getFeature().getDefaultGeometryProperty();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getEnvelope()
   */
  public GM_Envelope getEnvelope( )
  {
    final Feature feature = getFeature();
    if( feature == null )
      return null;

    return feature.getEnvelope();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getVirtuelProperty(java.lang.String,
   *      org.kalypsodeegree.model.feature.GMLWorkspace)
   */
  public Object getVirtuelProperty( final VirtualFeatureTypeProperty vpt, final GMLWorkspace workspace )
  {
    final Feature feature = getFeature();
    if( feature == null )
      return null;
    return feature.getVirtuelProperty( vpt, workspace );
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#setProperty(java.lang.String, java.lang.Object)
   */
  public void setProperty( final IPropertyType pt, final Object value )
  {
    final Feature feature = getFeature();
    if( feature != null )
      feature.setProperty( pt, value );
  }

  /**
   * @deprecated use getProperty(IPropertyType)
   * @see org.kalypsodeegree.model.feature.Feature#getProperty(java.lang.String)
   */
  @Deprecated
  public Object getProperty( final String propNameLocalPart )
  {
    final Feature feature = getFeature();
    if( feature == null )
      return null;
    return feature.getProperty( propNameLocalPart );
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getProperty(javax.xml.namespace.QName)
   */
  public Object getProperty( final QName propQName )
  {
    final Feature feature = getFeature();
    if( feature == null )
      return null;
    return feature.getProperty( propQName );
  }

  /**
   * @deprecated
   * @see org.kalypsodeegree.model.feature.Feature#setProperty(java.lang.String, java.lang.Object)
   */
  @Deprecated
  public void setProperty( final String propLocalName, final Object value )
  {
    final Feature feature = getFeature();
    if( feature != null )
      feature.setProperty( propLocalName, value );
  }

  /**
   * Returns the workspace of the linked feature.
   * 
   * @see org.kalypsodeegree.model.feature.Feature#getWorkspace()
   */
  public GMLWorkspace getWorkspace( )
  {
    return m_parentFeature.getWorkspace();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getParent()
   */
  public Feature getParent( )
  {
    return m_parentFeature;
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#setWorkspace(org.kalypsodeegree.model.feature.GMLWorkspace)
   */
  public void setWorkspace( final GMLWorkspace workspace )
  {
    // nothing to do, a delegated feature is never a root feature
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    final Feature feature = getFeature();
    return feature == null ? "null" : FeatureHelper.getAnnotationValue( feature, IAnnotation.ANNO_LABEL );
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#setProperty(javax.xml.namespace.QName, java.lang.Object)
   */
  public void setProperty( final QName propQName, final Object value )
  {
    final Feature feature = getFeature();
    if( feature != null )
      feature.setProperty( propQName, value );
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#invalidEnvelope()
   */
  public void invalidEnvelope( )
  {
    final Feature feature = getFeature();
    if( feature != null )
      feature.invalidEnvelope();
  }

  public String getHref( )
  {
    if( m_uri == null )
      return "#" + m_featureId;

    return m_uri + "#" + m_featureId;
  }

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals( final Object obj )
  {
    if( !(obj instanceof XLinkedFeature_Impl) )
      return false;

    final XLinkedFeature_Impl other = (XLinkedFeature_Impl) obj;
    return new EqualsBuilder().append( m_uri, other.m_uri ).append( m_featureId, other.m_featureId ).isEquals();

  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode( )
  {
    return new HashCodeBuilder().append( m_uri ).append( m_featureId ).toHashCode();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getParentRelation()
   */
  public IRelationType getParentRelation( )
  {
    return m_parentRelation;
  }

  public String getActuate( )
  {
    return m_actuate;
  }

  public String getArcrole( )
  {
    return m_arcrole;
  }

  public String getRole( )
  {
    return m_role;
  }

  public String getShow( )
  {
    return m_show;
  }

  public String getTitle( )
  {
    return m_title;
  }

  public String getUri( )
  {
    return m_uri;
  }

  public String getFeatureId( )
  {
    return m_featureId;
  }
}