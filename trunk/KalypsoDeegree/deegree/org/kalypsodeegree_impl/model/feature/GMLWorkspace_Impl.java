package org.kalypsodeegree_impl.model.feature;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree_impl.model.feature.visitors.CollectorVisitor;
import org.kalypsodeegree_impl.model.feature.visitors.FeatureTypeVisitor;

/**
 * @author doemming
 */
public class GMLWorkspace_Impl implements GMLWorkspace
{
  private final Feature m_rootFeature;

  private URL m_context;

  private final String m_schemaLocation;

  private final String m_schemaNamespace;

  /** id -> feature */
  final Map m_indexMap = new HashMap();

  private final FeatureType[] m_featureTypes;

  /** xmlns -> namespaceURI */
  private final Map m_nsMap;

  /**
   * 
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#getFeature(java.lang.String)
   */
  public Feature getFeature( final String id )
  {
    return (Feature)m_indexMap.get( id );
  }

  public GMLWorkspace_Impl( final FeatureType[] featureTypes, final Feature feature, final URL context,
      final String schemaLocation, final String schemaNamespace, final Map nsMap )
  {
    m_featureTypes = featureTypes;
    m_context = context;
    m_schemaLocation = schemaLocation;
    m_schemaNamespace = schemaNamespace;
    m_nsMap = nsMap;

    m_rootFeature = feature;

    try
    {
      accept( new RegisterVisitor(), m_rootFeature, FeatureVisitor.DEPTH_INFINITE );
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
    }
  }

  public Feature resolveLink( Feature srcFeature, String linkPropertyName )
  {
    Object linkValue = srcFeature.getProperty( linkPropertyName );
    if( linkValue == null )
      return null;
    if( linkValue instanceof Feature )
      return (Feature)linkValue;
    // must be a reference
    String linkID = (String)linkValue;
    FeatureAssociationTypeProperty ftp = (FeatureAssociationTypeProperty)srcFeature.getFeatureType().getProperty(
        linkPropertyName );
    FeatureType[] linkFTs = ftp.getAssociationFeatureTypes();
    for( int _ft = 0; _ft < linkFTs.length; _ft++ )
    {
      Feature[] features = getFeatures( linkFTs[_ft] );
      // todo: performance-todo: todo oben aufloesen und hier das feature
      // aus dem hash holen:
      for( int i = 0; i < features.length; i++ )
      {
        if( linkID.equals( features[i].getId() ) )
          return features[i];
      }
    }
    // broken Link
    return null;
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#resolveLinks(org.kalypsodeegree.model.feature.Feature,
   *      java.lang.String)
   */
  public Feature[] resolveLinks( Feature srcFeature, String linkPropertyName )
  {
    if( srcFeature.getFeatureType().getMaxOccurs( linkPropertyName ) == 1 )
    {
      final Feature feature = resolveLink( srcFeature, linkPropertyName );
      if( feature != null )
        return new Feature[]
        { feature };
      return new Feature[] {};
    }
    final List result = new ArrayList();
    final List linkList = (List)srcFeature.getProperty( linkPropertyName );

    for( Iterator iter = linkList.iterator(); iter.hasNext(); )
    {
      Object linkValue = iter.next();
      if( linkValue instanceof Feature )
      {
        result.add( linkValue );
        continue;
      }
      // must be a reference
      String linkID = (String)linkValue;
      FeatureAssociationTypeProperty ftp = (FeatureAssociationTypeProperty)srcFeature.getFeatureType().getProperty(
          linkPropertyName );
      FeatureType[] linkFTs = ftp.getAssociationFeatureTypes();
      for( int _ft = 0; _ft < linkFTs.length; _ft++ )
      {
        Feature[] features = getFeatures( linkFTs[_ft] );
        // todo performance-todo: todo oben aufloesen und hier das feature
        // aus dem hash holen:
        for( int i = 0; i < features.length; i++ )
        {
          if( linkID.equals( features[i].getId() ) )
          {
            result.add( features[i] );
            continue;
          }
        }
      }
    }
    // broken Link
    return (Feature[])result.toArray( new Feature[result.size()] );
  }

  public Feature getRootFeature()
  {
    return m_rootFeature;
  }

  /**
   * 
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#getFeatureTypes()
   */
  public FeatureType[] getFeatureTypes()
  {
    return m_featureTypes;
  }

  /**
   * Findet alle Features eines Typs. Der Typ muss genau stimmen, substitution gilt nicht
   */
  public Feature[] getFeatures( final FeatureType ft )
  {
    final CollectorVisitor collector = new CollectorVisitor();
    final FeatureTypeVisitor visitor = new FeatureTypeVisitor( collector, ft, false );
    try
    {
      accept( visitor, getRootFeature(), FeatureVisitor.DEPTH_INFINITE );
    }
    catch( Throwable e )
    {
      e.printStackTrace();
    }

    return collector.getResults( true );
  }

  private final Collection m_listener = new ArrayList();

  /**
   * 
   * @see org.kalypsodeegree.model.feature.event.ModellEventProvider#addModellListener(org.kalypsodeegree.model.feature.event.ModellEventListener)
   */
  public void addModellListener( final ModellEventListener listener )
  {
    m_listener.add( listener );
  }

  /**
   * 
   * @see org.kalypsodeegree.model.feature.event.ModellEventProvider#removeModellListener(org.kalypsodeegree.model.feature.event.ModellEventListener)
   */
  public void removeModellListener( final ModellEventListener listener )
  {
    m_listener.remove( listener );
  }

  /**
   * 
   * @see org.kalypsodeegree.model.feature.event.ModellEventProvider#fireModellEvent(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void fireModellEvent( final ModellEvent event )
  {
    // use array instead of iterator, because the listener list may change in
    // response to this event (lead to a ConcurrentodificationException)
    final ModellEventListener[] objects = (ModellEventListener[])m_listener.toArray( new ModellEventListener[m_listener
        .size()] );
    for( int i = 0; i < objects.length; i++ )
      objects[i].onModellChange( event );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#resolveWhoLinksTo(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypsodeegree.model.feature.FeatureType, java.lang.String)
   */
  public Feature[] resolveWhoLinksTo( Feature linkTargetfeature, FeatureType linkSrcFeatureType, String linkPropertyName )
  {
    if( linkTargetfeature == null )
      return new Feature[0];

    final List result = new ArrayList();
    final Feature[] features = getFeatures( linkSrcFeatureType );
    for( int i = 0; i < features.length; i++ )
    {
      Object prop = features[i].getProperty( linkPropertyName );
      if( prop == linkTargetfeature )
        result.add( features[i] );
      if( linkTargetfeature.getId().equals( prop ) )
        result.add( features[i] );
    }

    FeatureType[] substiFTs = GMLHelper.getResolveSubstitutionGroup( linkSrcFeatureType, getFeatureTypes() );
    for( int _ft = 0; _ft < substiFTs.length; _ft++ )
    {
      final Feature[] substiFeatures = getFeatures( substiFTs[_ft] );

      for( int i = 0; i < features.length; i++ )
      {
        Object prop = substiFeatures[i].getProperty( linkPropertyName );
        if( prop == linkTargetfeature )
          result.add( substiFeatures[i] );
        if( linkTargetfeature.getId().equals( prop ) )
          result.add( substiFeatures[i] );
      }
    }

    return (Feature[])result.toArray( new Feature[result.size()] );
  }

  /**
   * 
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#getContext()
   */
  public URL getContext()
  {
    return m_context;
  }

  public void setContext( URL context )
  {
    m_context = context;
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#accept(org.kalypsodeegree.model.feature.FeatureVisitor,
   *      org.kalypsodeegree.model.feature.FeatureType, int)
   */
  public void accept( final FeatureVisitor fv, final FeatureType ft, final int depth )
  {
    final Feature[] features = getFeatures( ft );

    for( int i = 0; i < features.length; i++ )
    {
      final Feature feature = features[i];
      accept( fv, feature, depth );
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#accept(org.kalypsodeegree.model.feature.FeatureVisitor,
   *      org.kalypsodeegree.model.feature.Feature, int)
   */
  public void accept( final FeatureVisitor fv, final Feature feature, final int depth )
  {
    final FeatureType ft = feature.getFeatureType();
    final FeatureTypeProperty[] ftps = ft.getProperties();

    final boolean recurse = fv.visit( feature );

    if( recurse && depth != FeatureVisitor.DEPTH_ZERO )
    {
      for( int j = 0; j < ftps.length; j++ )
      {
        if( ftps[j] instanceof FeatureAssociationTypeProperty )
        {
          Object value = feature.getProperty( ftps[j].getName() );
          if( value == null )
            continue;

          if( value instanceof Feature )
          {
            final Feature f = (Feature)value;
            accept( fv, f, depth );
          }
          else if( value instanceof List )
            accept( fv, (List)value, depth );
          else if( value instanceof String && depth == FeatureVisitor.DEPTH_INFINITE_LINKS )
          {
            final Feature f = getFeature( (String)value );
            accept( fv, f, depth );
          }
        }
      }
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#accept(org.kalypsodeegree.model.feature.FeatureVisitor,
   *      java.util.List, int)
   */
  public void accept( final FeatureVisitor fv, final List features, final int depth )
  {
    for( Iterator iter = features.iterator(); iter.hasNext(); )
    {
      final Object next = iter.next();

      if( next instanceof String )
      {
        // ACHTUNG LINK!
        if( depth == FeatureVisitor.DEPTH_INFINITE_LINKS )
        {
          final Feature f = getFeature( (String)next );
          accept( fv, f, depth );
        }
      }
      else if( next instanceof Feature )
        accept( fv, (Feature)next, depth );
    }
  }

  private final class RegisterVisitor implements FeatureVisitor
  {
    /**
     * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
     */
    public boolean visit( final Feature f )
    {
      final String id = f.getId();
      // ACHTUNG!!! bitte BEIDE Zeilen ein- und auskommentieren!
      //      if( m_indexMap.containsKey( id ) )
      //        System.out.println( "Workspace already contains a feature with id: " +
      // id );
      m_indexMap.put( id, f );
      return true;
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#getFeatureType(java.lang.String)
   */
  public FeatureType getFeatureType( final String featureName )
  {
    for( int i = 0; i < m_featureTypes.length; i++ )
    {
      final FeatureType ft = m_featureTypes[i];
      if( ft.getName().equals( featureName ) )
        return ft;
    }

    return null;
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#getFeatureFromPath(java.lang.String)
   */
  public Object getFeatureFromPath( final String featurePath )
  {
    return new FeaturePath( featurePath ).getFeature( this );
  }

  /**
   * Holt den durch den FeaturePath angegebenen Typ Systax des FeaturePath:
   * <code> <propertyName>/.../<propertyName>[featureTypeName] </code> Wobei der featureTypeName optional ist
   * 
   * 
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#getFeatureTypeFromPath(java.lang.String)
   */
  public FeatureType getFeatureTypeFromPath( final String featurePath )
  {
    return new FeaturePath( featurePath ).getFeatureType( this );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#getFeaturepathForFeature(org.kalypsodeegree.model.feature.Feature)
   */
  public FeaturePath getFeaturepathForFeature( final Feature feature )
  {
    return new FeaturePath( feature );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#getSchemaLocation()
   */
  public String getSchemaLocation()
  {
    return m_schemaLocation;
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#getSchemaNamespace()
   */
  public String getSchemaNamespace()
  {
    return m_schemaNamespace;
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#createFeature(org.kalypsodeegree.model.feature.FeatureType)
   */
  public Feature createFeature( FeatureType type )
  {
    String newId = createFeatureId( type );
    return FeatureFactory.createFeature( newId, type, false );
  }

  private String createFeatureId( FeatureType type )
  {
    String id = type.getName();
    int no = 0;
    while( m_indexMap.containsKey( id + Integer.toString( no ) ) )
      no++;
    return id + Integer.toString( no );
  }

  /**
   * 
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#addFeatureAsComposition(org.kalypsodeegree.model.feature.Feature,
   *      java.lang.String, int, org.kalypsodeegree.model.feature.Feature)
   */
  public void addFeatureAsComposition( Feature parent, String propName, int pos, Feature newFeature ) throws Exception
  {
    final Object prop = parent.getProperty( propName );

    if( prop instanceof List )
    {
      ( (List)prop ).add( pos, newFeature );
      m_indexMap.put( newFeature.getId(), newFeature );
      return;
    }
    else if( prop == null ) // element not set
    {
      final FeatureProperty newProp = FeatureFactory.createFeatureProperty( propName, newFeature );
      parent.setProperty( newProp );
      m_indexMap.put( newFeature.getId(), newFeature );
      return;
    }
    // TODO eigene exception entwerfen
    throw new Exception( "New Feature violates maxOccurs" );
  }

  /**
   * 
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#addFeatureAsAggregation(org.kalypsodeegree.model.feature.Feature,
   *      java.lang.String, int, java.lang.String)
   */
  public void addFeatureAsAggregation( Feature srcFE, String propName, int pos, String featureID ) throws Exception
  {
    if( srcFE.getFeatureType().isListProperty( propName ) )
    {
      int maxOccurs = srcFE.getFeatureType().getMaxOccurs( propName );
      final List list = (List)srcFE.getProperty( propName );
      if( list.size() < maxOccurs || maxOccurs == FeatureType.UNBOUND_OCCURENCY )
        list.add( pos, featureID );
      else
        throw new Exception( "New Feature violates maxOccurs" );
    }
    else if( srcFE.getProperty( propName ) == null )
    {
      srcFE.setProperty( FeatureFactory.createFeatureProperty( propName, featureID ) );
    }
    else
      throw new Exception( "New Feature as allready set" );
  }

  /**
   * 
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#removeLinkedAsAggregationFeature(org.kalypsodeegree.model.feature.Feature,
   *      java.lang.String, java.lang.String)
   */
  public boolean removeLinkedAsAggregationFeature( Feature parentFeature, String propName, String childFeatureId )
  {
    final Object prop = parentFeature.getProperty( propName );
    if( parentFeature.getFeatureType().isListProperty( propName ) )
    {
      List list = (List)prop;
      return list.remove( childFeatureId );
    }
    if( childFeatureId.equals( parentFeature.getProperty( propName ) ) )
    {
      parentFeature.setProperty( FeatureFactory.createFeatureProperty( propName, null ) );
      return true;
    }
    return false;
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#removeLinkedAsCompositionFeature(org.kalypsodeegree.model.feature.Feature,
   *      java.lang.String, org.kalypsodeegree.model.feature.Feature)
   */
  public boolean removeLinkedAsCompositionFeature( Feature parentFeature, String propName, Feature childFeature )
  {
    boolean ok = false;
    final Object prop = parentFeature.getProperty( propName );
    int maxOccurs = parentFeature.getFeatureType().getMaxOccurs( propName );
    switch( maxOccurs )
    {
    case 1:
      if( parentFeature.getProperty( propName ) == childFeature )
      {
        parentFeature.setProperty( FeatureFactory.createFeatureProperty( propName, null ) );
        ok = true;
      }
      break;
    default:
      List list = (List)prop;
      ok = list.remove( childFeature );
    }
    if( ok )
      unregisterFeature( childFeature );
    return ok;
  }

  private void unregisterFeature( Feature childFeature )
  {
    m_indexMap.remove( childFeature.getId() );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#getNamespaceMap()
   */
  public Map getNamespaceMap()
  {
    return m_nsMap;
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#accept(org.kalypsodeegree.model.feature.FeatureVisitor,
   *      java.lang.String, int)
   */
  public void accept( final FeatureVisitor fv, final String featurePath, final int depth )
  {
    final Object featureFromPath = getFeatureFromPath( featurePath );
    if( featureFromPath instanceof Feature )
      fv.visit( (Feature)featureFromPath );
    else if( featureFromPath instanceof FeatureList )
      accept( fv, (FeatureList)featureFromPath, depth );
    else
      throw new IllegalArgumentException( "FeaturePath is neither Feature nor FeatureList: " + featurePath );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#isExistingRelation(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypsodeegree.model.feature.Feature, java.lang.String)
   */
  public boolean isExistingRelation( Feature srcFE, Feature destFE, String relationPropName )
  {
    Feature[] features = resolveLinks( srcFE, relationPropName );
    for( int i = 0; i < features.length; i++ )
    {
      if( features[i] == destFE )
        return true;
    }
    return false;
  }
}