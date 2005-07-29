package org.kalypsodeegree_impl.model.feature;

import java.util.ArrayList;
import java.util.List;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeaturePropertyVisitor;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.gml.schema.virtual.VirtualFeatureTypeProperty;
import org.kalypsodeegree_impl.model.geometry.GM_Envelope_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author doemming
 * 
 * implementation of ogc feature that supports different cardinalities of properties, but not "unbound" cardinalities
 * (use FeatureCollections for unbound cardinalities)
 */
public class Feature_Impl implements Feature
{
  private final static GM_Envelope INVALID_ENV = new GM_Envelope_Impl();

  private GM_Envelope m_envelope = INVALID_ENV;

  /**
   * all property-values are stored here in sequencial order (as defined in applicationschema) properties with
   * maxOccurency = 1 are stored direct properties with maxOccurency > 1 are stored in a list properties with
   * maxOccurency = "unbounded" should use FeatureCollections
   */
  private final Object[] m_properties;

  private final FeatureType m_featureType;

  private final String m_id;

  // fields from old KalypsoFeature
  private int mySelection = 0;

  /**
   * Erzeugt ein Feature mit gesetzter ID und f�llt das Feature mit Standardwerten
   * 
   * @deprecated use Constructor:
   *             <code>Feature_Impl( final FeatureType ft, final String id, boolean initializeWithDefaults )</code>
   *             instead.
   */
  protected Feature_Impl( final FeatureType ft, final String id )
  {
    if( ft == null )
      throw new UnsupportedOperationException( "must provide a featuretype" );
    m_featureType = ft;
    m_id = id;
    // initialize
    final FeatureTypeProperty[] ftp = ft.getProperties();
    m_properties = new Object[ftp.length];
    for( int i = 0; i < ftp.length; i++ )
    {
      if( m_featureType.getMaxOccurs( i ) != 1 )
      {
        if( ftp[i] instanceof FeatureAssociationTypeProperty )
          m_properties[i] = FeatureFactory.createFeatureList( this, ftp[i] );
        else
          m_properties[i] = new ArrayList();
      }
      else
        m_properties[i] = null;
    }

    final FeatureProperty[] properties = FeatureFactory.createDefaultFeatureProperty( ftp, false );
    for( int i = 0; i < properties.length; i++ )
    {
      if( properties[i].getValue() != null && ft.getMaxOccurs( properties[i].getName() ) == 1 )
        setProperty( properties[i] );
    }
  }

  /**
   * Erzeugt ein Feature mit gesetzter ID und f�llt das Feature mit Standardwerten
   * 
   * @param initializeWithDefaults
   *          set <code>true</code> when generating from UserInterface <br>
   *          set <code>false</code> when generating from GML or so.
   */
  protected Feature_Impl( final FeatureType ft, final String id, boolean initializeWithDefaults )
  {
    if( ft == null )
      throw new UnsupportedOperationException( "must provide a featuretype" );
    m_featureType = ft;
    m_id = id;
    // initialize
    final FeatureTypeProperty[] ftp = ft.getProperties();
    m_properties = new Object[ftp.length];
    for( int i = 0; i < ftp.length; i++ )
    {
      if( m_featureType.getMaxOccurs( i ) != 1 )
      {
        if( ftp[i] instanceof FeatureAssociationTypeProperty )
          m_properties[i] = FeatureFactory.createFeatureList( this, ftp[i] );
        else
          m_properties[i] = new ArrayList();
      }
      else
        m_properties[i] = null;
    }
    if( initializeWithDefaults )
    {
      final FeatureProperty[] properties = FeatureFactory.createDefaultFeatureProperty( ftp, false );
      for( int i = 0; i < properties.length; i++ )
      {
        if( properties[i].getValue() != null && ft.getMaxOccurs( properties[i].getName() ) == 1 )
          setProperty( properties[i] );
      }
    }
  }

  protected Feature_Impl( FeatureType ft, String id, Object[] propValues )
  {
    if( ft == null )
      throw new UnsupportedOperationException( "must provide a featuretype" );

    m_featureType = ft;
    m_id = id;
    m_properties = propValues;
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getId()
   */
  public String getId()
  {
    return m_id;
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getFeatureType()
   */
  public FeatureType getFeatureType()
  {
    return m_featureType;
  }

  /**
   * @return array of properties, properties with maxoccurency>0 (as defined in applicationschema) will be embedded in
   *         java.util.List-objects
   * @see org.kalypsodeegree.model.feature.Feature#getProperties()
   */
  public Object[] getProperties()
  {
    return m_properties;
  }

  /**
   * format of name if "namespace:name" or just "name" - both will work
   * 
   * @return array of properties, properties with maxoccurency>0 (as defined in applicationschema) will be embedded in
   *         java.util.List-objects
   * 
   * @see org.kalypsodeegree.model.feature.Feature#getProperty(java.lang.String)
   */
  public Object getProperty( String name )
  {
    final int pos = m_featureType.getPropertyPosition( name );
    if( pos == -1 )
      return null;

    return m_properties[pos];
  }

  /**
   * 
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
  public GM_Object[] getGeometryProperties()
  {
    final List result = new ArrayList();
    final FeatureTypeProperty[] ftp = m_featureType.getProperties();
    for( int p = 0; p < ftp.length; p++ )
    {
      if( ftp[p].isGeometryProperty() )
      {
        Object o = getProperty( p );
        if( o == null )
          continue;
        if( o instanceof List )
        {
          result.addAll( (List)o );
        }
        else
          result.add( o );
      }
    }
    // TODO allways use virtual ftp to calculate bbox ??
    final FeatureTypeProperty[] vftp = m_featureType.getVirtuelFeatureTypeProperty();
    for( int p = 0; p < vftp.length; p++ )
    {
      if( vftp[p].isGeometryProperty() )
      {
        Object o = getVirtuelProperty( vftp[p].getName(), null );
        if( o == null )
          continue;
        if( o instanceof List )
        {
          result.addAll( (List)o );
        }
        else
          result.add( o );
      }
    }

    return (GM_Object[])result.toArray( new GM_Object[result.size()] );
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getDefaultGeometryProperty()
   */
  public GM_Object getDefaultGeometryProperty()
  {
    int pos = m_featureType.getDefaultGeometryPropertyPosition();
    if( pos < 0 )
      return null;
    Object prop = m_properties[pos];
    if( prop instanceof List )
    {
      List props = (List)prop;
      return (GM_Object)( props.size() > 0 ? props.get( 0 ) : null );
    }
    if( !( prop == null || prop instanceof GM_Object ) )
      throw new UnsupportedOperationException( "wrong geometry type" );
    return (GM_Object)prop;
  }

  /**
   * set defaulproperty (occurenceposition=0)
   * 
   * @see org.kalypsodeegree.model.feature.Feature#setProperty(org.kalypsodeegree.model.feature.FeatureProperty)
   */
  public void setProperty( FeatureProperty property )
  {
    if( property == null )
      return;
    FeatureType ft = getFeatureType();
    if( ft == null )
      return;
    FeatureTypeProperty ftp = ft.getProperty( property.getName() );
    if( ftp == null )
    {
      return;
    }
    if( ftp.isGeometryProperty() )
      invalidEnvelope();

    int pos = m_featureType.getPropertyPosition( property.getName() );
    m_properties[pos] = property.getValue();
  }

  /**
   * 
   * @see org.kalypsodeegree.model.feature.Feature#addProperty(org.kalypsodeegree.model.feature.FeatureProperty)
   */
  public void addProperty( FeatureProperty property )
  {
    // to handle boundingbox if geometryproperty
    int pos = m_featureType.getPropertyPosition( property.getName() );
    Object newValue = property.getValue();
    Object oldValue = m_properties[pos];
    if( oldValue instanceof List )
    {
      if( newValue instanceof List )
      {
        ( (List)oldValue ).addAll( (List)newValue );
      }
      else
      {
        ( (List)oldValue ).add( newValue );
      }
    }
    else
    {
      m_properties[pos] = newValue;
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getEnvelope()
   */
  public GM_Envelope getEnvelope()
  {
    if( m_envelope == INVALID_ENV )
      calculateEnv();
    return m_envelope;
  }

  private void calculateEnv()
  {
    GM_Envelope env = null;
    GM_Object[] geoms = getGeometryProperties();
    for( int i = 0; i < geoms.length; i++ )
    {
      if( !( geoms[i] instanceof GM_Point ) )
      {
        if( env == null )
          env = geoms[i].getEnvelope();
        else
          env = env.getMerged( geoms[i].getEnvelope() );
      }
      else
      {
        GM_Position pos = ( (GM_Point)geoms[i] ).getPosition();
        GM_Envelope env2 = GeometryFactory.createGM_Envelope( pos, pos );
        if( env == null )
          env = env2;
        else
          env = env.getMerged( env2 );
      }
    }
    m_envelope = env;
  }

  private void invalidEnvelope()
  {
    m_envelope = INVALID_ENV;
  }

  public void debugOut( int indent )
  {
    System.out.println( getIndent( indent ) + "Name:      " + m_featureType.getName() );
    System.out.println( getIndent( indent ) + "NameSpace: " + m_featureType.getNamespace() );
    System.out.println( getIndent( indent ) + " TYPE:      Feature" );
    System.out.println( getIndent( indent ) + " props:" );
    final FeatureTypeProperty[] ftps = m_featureType.getProperties();
    indent++;
    for( int i = 0; i < ftps.length; i++ )
    {
      System.out.println( getIndent( indent ) + "Name:      " + ftps[i].getName() );
      System.out.println( getIndent( indent ) + "NameSpace: " + ftps[i].getNamespace() );
      System.out.println( getIndent( indent ) + " TYPE:     " + ftps[i].getType() );
      Object value = m_properties[i];
      if( value == null )
        System.out.println( getIndent( indent ) + "null" );
      else if( value instanceof List )
      {
        List vList = (List)value;
        for( int j = 0; j < vList.size(); j++ )
        {
          Object lValue = vList.get( j );
          debugOutProperty( indent, lValue );
        }
      }
      else
        debugOutProperty( indent, value );

    }
  }

  private void debugOutProperty( int indent, Object value )
  {
    if( value instanceof Feature )
      ( (Feature_Impl)value ).debugOut( indent + 1 );
    else
      System.out.println( getIndent( indent ) + value.toString() );
  }

  private String getIndent( int indent )
  {
    return "                                                  ".substring( 0, indent * 4 );
  }

  public boolean select( int selectID )
  {
    if( isSelected( selectID ) )
      return false;

    mySelection |= selectID;
    return true;

  }

  public boolean unselect( int selectID )
  {
    if( !isSelected( selectID ) )
      return false;
    mySelection &= ~selectID;
    return true;
  }

  public boolean toggle( int selectID )
  {
    mySelection ^= selectID;
    return true;
  }

  public boolean isSelected( int selectID )
  {
    return selectID == ( mySelection & selectID );
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getSelection()
   */
  public int getSelection()
  {
    return mySelection;
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#setSelection(int)
   */
  public void setSelection( final int selection )
  {
    mySelection = selection;
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getVirtuelProperty(java.lang.String,
   *      org.kalypsodeegree.model.feature.GMLWorkspace)
   */
  public Object getVirtuelProperty( String propertyName, GMLWorkspace workspace )
  {
    VirtualFeatureTypeProperty virtuelFeatureTypeProperty = (VirtualFeatureTypeProperty)m_featureType
        .getVirtuelFeatureTypeProperty( propertyName );
    return virtuelFeatureTypeProperty.getVirtuelValue( this, workspace );
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#accept(org.kalypsodeegree.model.feature.IFeaturePropertyVisitor)
   */
  public void accept( final IFeaturePropertyVisitor visitor )
  {
    final FeatureType featureType = getFeatureType();
    final FeatureTypeProperty[] ftps = featureType.getProperties();
    final Object[] properties = getProperties();

    for( int i = 0; i < ftps.length; i++ )
      FeatureFactory.accept( visitor, i, ftps[i], properties[i] );
  }
}