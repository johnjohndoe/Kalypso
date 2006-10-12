package org.kalypsodeegree_impl.model.feature;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree_impl.model.feature.visitors.CollectorVisitor;
import org.kalypsodeegree_impl.model.feature.visitors.FeatureTypeVisitor;

/**
 * In order to use this workspace with support of xlinks, a
 * {@link org.kalypsodeegree_impl.model.feature.IFeatureProviderFactory} must be set.
 * 
 * @see #setFeatureProviderFactory(IFeatureProviderFactory)
 * @author doemming
 */
public class GMLWorkspace_Impl implements GMLWorkspace
{
  private final Feature m_rootFeature;

  private URL m_context;

  private final String m_schemaLocation;

  /** id -> feature */
  final Map<String, Feature> m_indexMap = new HashMap<String, Feature>();

  private final IFeatureType[] m_featureTypes;

  private final IGMLSchema m_schema;

  private final IFeatureProviderFactory m_factory;

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#getFeature(java.lang.String)
   */
  public Feature getFeature( final String id )
  {
    return m_indexMap.get( id );
  }

  public GMLWorkspace_Impl( final IGMLSchema schema, final IFeatureType[] featureTypes, final Feature feature, final URL context, final String schemaLocation, final IFeatureProviderFactory factory )
  {
    m_schema = schema;
    m_featureTypes = featureTypes;
    m_context = context;
    m_schemaLocation = schemaLocation;
    m_rootFeature = feature;
    m_factory = factory;

    if( m_rootFeature != null )
      m_rootFeature.setWorkspace( this );
    try
    {
      accept( new RegisterVisitor(), m_rootFeature, FeatureVisitor.DEPTH_INFINITE );
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventProvider#dispose()
   */
  public void dispose( )
  {
    m_listener.clear();
    // release other references?
  }

  public Feature resolveLink( Feature srcFeature, IRelationType linkProperty )
  {
    return resolveLink( srcFeature, linkProperty, RESOLVE_ALL );
  }

  public Feature resolveLink( Feature srcFeature, IRelationType linkProperty, final int resolveMode )
  {
    final Object linkValue = srcFeature.getProperty( linkProperty );
    if( linkValue == null )
      return null;
    if( linkValue instanceof Feature )
    {
      if( resolveMode != RESOLVE_LINK )
        return (Feature) linkValue;
      return null;
    }
    // must be a reference
    final String linkID = (String) linkValue;
    if( resolveMode != RESOLVE_COMPOSITION )
      return getFeature( linkID );
    return null;
  }

  public Feature[] resolveLinks( Feature srcFeature, IRelationType linkProperty )
  {
    return resolveLinks( srcFeature, linkProperty, RESOLVE_ALL );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#resolveLinks(org.kalypsodeegree.model.feature.Feature,
   *      java.lang.String)
   */
  public Feature[] resolveLinks( Feature srcFeature, IRelationType linkProperty, final int resolveMode )
  {
    if( !linkProperty.isList() )
    {
      final Feature feature = resolveLink( srcFeature, linkProperty, resolveMode );
      if( feature != null )
        return new Feature[] { feature };
      return new Feature[] {};
    }
    final List<Feature> result = new ArrayList<Feature>();
    final List linkList = (List) srcFeature.getProperty( linkProperty );

    for( final Object linkValue : linkList )
    {
      if( linkValue instanceof Feature )
      {
        if( !(resolveMode == RESOLVE_LINK) )
          result.add( (Feature) linkValue );
        continue;
      }
      // must be a reference
      if( !(resolveMode == RESOLVE_COMPOSITION) )
      {
        final String linkID = (String) linkValue;
        result.add( getFeature( linkID ) );
      }
    }

    return result.toArray( new Feature[result.size()] );
  }

  public Feature getRootFeature( )
  {
    return m_rootFeature;
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#getFeatureTypes()
   */
  public IFeatureType[] getFeatureTypes( )
  {
    return m_featureTypes;
  }

  /**
   * Findet alle Features eines Typs. Der Typ muss genau stimmen, substitution gilt nicht
   */
  public Feature[] getFeatures( final IFeatureType ft )
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

  private final Collection<ModellEventListener> m_listener = new HashSet<ModellEventListener>();

  /**
   * Every listener is registered only once.
   * 
   * @see org.kalypsodeegree.model.feature.event.ModellEventProvider#addModellListener(org.kalypsodeegree.model.feature.event.ModellEventListener)
   */
  public void addModellListener( final ModellEventListener listener )
  {
    m_listener.add( listener );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventProvider#removeModellListener(org.kalypsodeegree.model.feature.event.ModellEventListener)
   */
  public void removeModellListener( final ModellEventListener listener )
  {
    m_listener.remove( listener );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventProvider#fireModellEvent(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void fireModellEvent( final ModellEvent event )
  {
    // use array instead of iterator, because the listener list may change in
    // response to this event (lead to a ConcurrentodificationException)
    final ModellEventListener[] objects = m_listener.toArray( new ModellEventListener[m_listener.size()] );
    for( int i = 0; i < objects.length; i++ )
      objects[i].onModellChange( event );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#resolveWhoLinksTo(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypsodeegree.model.feature.IFeatureType, java.lang.String)
   */
  public Feature[] resolveWhoLinksTo( final Feature linkTargetfeature, final IFeatureType linkSrcFeatureType, final IRelationType linkProperty )
  {
    if( linkTargetfeature == null )
      return new Feature[0];

    final List<Feature> result = new ArrayList<Feature>();
    final Feature[] features = getFeatures( linkSrcFeatureType );
    for( int i = 0; i < features.length; i++ )
    {
      final Object prop = features[i].getProperty( linkProperty );
      if( prop == linkTargetfeature )
        result.add( features[i] );
      if( linkTargetfeature.getId().equals( prop ) )
        result.add( features[i] );
    }

    // final IFeatureType[] substiFTs =
    // GMLHelper.getResolveSubstitutionGroup( linkSrcFeatureType, getFeatureTypes() );
    final IFeatureType[] substiFTs = GMLSchemaUtilities.getSubstituts( linkSrcFeatureType, m_schema, false, true );

    for( int _ft = 0; _ft < substiFTs.length; _ft++ )
    {
      final Feature[] substiFeatures = getFeatures( substiFTs[_ft] );

      for( int i = 0; i < substiFeatures.length; i++ )
      {
        Object prop = substiFeatures[i].getProperty( linkProperty );
        if( prop == linkTargetfeature )
          result.add( substiFeatures[i] );
        if( linkTargetfeature.getId().equals( prop ) )
          result.add( substiFeatures[i] );
      }
    }
    return result.toArray( new Feature[result.size()] );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#getContext()
   */
  public URL getContext( )
  {
    return m_context;
  }

  public void setContext( URL context )
  {
    m_context = context;
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#accept(org.kalypsodeegree.model.feature.FeatureVisitor,
   *      org.kalypsodeegree.model.feature.IFeatureType, int)
   */
  public void accept( final FeatureVisitor fv, final IFeatureType ft, final int depth )
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
    accept( fv, feature, depth, feature.getFeatureType().getProperties() );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#accept(org.kalypsodeegree_impl.model.feature.visitors.CloneFeatureVisitor,
   *      org.kalypsodeegree.model.feature.Feature, int, org.kalypsodeegree.model.feature.IPropertyType[])
   */
  public void accept( final FeatureVisitor fv, final Feature feature, final int depth, final IPropertyType[] ftps )
  {
    final boolean recurse = fv.visit( feature );

    if( recurse && depth != FeatureVisitor.DEPTH_ZERO )
    {
      for( int j = 0; j < ftps.length; j++ )
      {
        if( ftps[j] instanceof IRelationType )
        {
          Object value = feature.getProperty( ftps[j] );
          if( value == null )
            continue;

          if( value instanceof XLinkedFeature_Impl )
          {
            if( depth == FeatureVisitor.DEPTH_INFINITE_LINKS )
            {
              final Feature f = getFeature( (String) value );
              accept( fv, f, depth );
            }
          }
          else if( value instanceof Feature )
          {
            final Feature f = (Feature) value;
            accept( fv, f, depth );
          }
          else if( value instanceof List )
            accept( fv, (List) value, depth );
          else if( value instanceof String && depth == FeatureVisitor.DEPTH_INFINITE_LINKS )
          {
            final Feature f = getFeature( (String) value );
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
    for( final Iterator iter = features.iterator(); iter.hasNext(); )
    {
      final Object next = iter.next();

      if( next instanceof String )
      {
        // ACHTUNG LINK!
        if( depth == FeatureVisitor.DEPTH_INFINITE_LINKS )
        {
          final Feature f = getFeature( (String) next );
          accept( fv, f, depth );
        }
      }
      else if( next instanceof Feature )
        accept( fv, (Feature) next, depth );
    }
  }

  protected final class RegisterVisitor implements FeatureVisitor
  {
    /**
     * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
     */
    public boolean visit( final Feature f )
    {
      String id = f.getId();
      // ACHTUNG!!! bitte BEIDE Zeilen ein- und auskommentieren!
      // if( m_indexMap.containsKey( id ) )
      // System.out.println( "Workspace already contains a feature with id: " +
      // id );
      // if( id != null && id.equals( "a816c00e010a02356c64000000086e20" ) )
      // System.out.println( "found" );

      if( id == null || id.length() == 0 )
      {
        id = createFeatureId( f.getFeatureType() );
        // System.out.println( "Feature has no id: " + f );
      }

      // TODO: better generate new ids and remember wich ones are generated (because
      // we dont want to write the gernerated ones)
      // IDEA: put an prefix before the generated id (a 'strong' prefix which will not be in any other id)
      // When writing the gml, we then can quickly determine if the id is generated

      // TODO: do not put null-ids into this map ? What sideeffects do we expect
      m_indexMap.put( id, f );
      return true;
    }
  }

  protected final class UnRegisterVisitor implements FeatureVisitor
  {
    /**
     * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
     */
    public boolean visit( final Feature f )
    {
      final String id = f.getId();
      m_indexMap.remove( id );
      return true;
    }
  }

  /**
   * @deprecated
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#getFeatureType(java.lang.String)
   */
  @Deprecated
  public IFeatureType getFeatureType( final String nameLocalPart )
  {
    for( int i = 0; i < m_featureTypes.length; i++ )
    {
      final IFeatureType ft = m_featureTypes[i];
      if( ft.getQName().getLocalPart().equals( nameLocalPart ) )
        return ft;
    }
    return null;
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#getFeatureType(javax.xml.namespace.QName)
   */
  public IFeatureType getFeatureType( final QName featureQName )
  {
    for( int i = 0; i < m_featureTypes.length; i++ )
    {
      final IFeatureType ft = m_featureTypes[i];
      if( ft.getQName().equals( featureQName ) )
        return ft;
    }

    // HACK: because a workspace has only its own feature types
    // maybe allways use this method?
    return m_schema.getFeatureType( featureQName );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#getFeatureFromPath(java.lang.String)
   */
  public Object getFeatureFromPath( final String featurePath )
  {
    try
    {
      final FeaturePath fPath = new FeaturePath( featurePath );
      return fPath.getFeature( this );
    }
    catch( Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
      return null;
    }
  }

  /**
   * Holt den durch den FeaturePath angegebenen Typ Systax des FeaturePath:
   * <code> <propertyName>/.../<propertyName>[featureTypeName] </code> Wobei der featureTypeName optional ist
   * 
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#getFeatureTypeFromPath(java.lang.String)
   */
  public IFeatureType getFeatureTypeFromPath( final String featurePath )
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
  public String getSchemaLocationString( )
  {
    return m_schemaLocation;
  }

  /**
   * Creates a new feature and registers it with this workspace.
   * 
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#createFeature(org.kalypsodeegree.model.feature.IFeatureType)
   */
  public Feature createFeature( final Feature parent, final IFeatureType type )
  {
    return createFeature( parent, type, 0 );
  }

  /**
   * Creates a new feature and registers it with this workspace.
   * 
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#createFeature(org.kalypsodeegree.model.feature.IFeatureType)
   */
  public Feature createFeature( final Feature parent, final IFeatureType type, final int depth )
  {
    if( type.isAbstract() )
    {
      // TODO: throw an exception?
      // throw new IllegalArgumentException( "Cannot create feature from abstract type: " + type );
      System.out.println( "Creating feature from abstract type: " + type );
    }

    // TODO: @andreas: merge createFeature method with the addFeatureAsComposite method (add the IRelationType )
    final String newId = createFeatureId( type );
    final Feature newFeature = FeatureFactory.createFeature( parent, newId, type, true, depth );
    // TODO: because we nowadys recurse, another feature might be created meanwhile
    // so there is a chance, that an id is used twice
    m_indexMap.put( newId, newFeature );
    return newFeature;
  }

  private String createFeatureId( final IFeatureType type )
  {
    // REMARK: Performance Bufix
    // The commented code (see below) caused a serious performance
    // problem to long lists of homogenous features.

    // SLOW: do not comment in!
    // int no = 0;
    // while( m_indexMap.containsKey( id + Integer.toString( no ) ) )
    // no++;
    // return id + Integer.toString( no );

    // We now create random numbered feature ids,
    // which normally should lead to only one try for finding a new id
    final String name = type.getQName().getLocalPart();
    while( true )
    {
      final long rnd = Math.round( Math.random() * m_indexMap.size() );
      final long time = System.currentTimeMillis();
      final String id = name + time + rnd;
      if( !m_indexMap.containsKey( id ) )
        return id;
    }

  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#addFeatureAsComposition(org.kalypsodeegree.model.feature.Feature,
   *      java.lang.String, int, org.kalypsodeegree.model.feature.Feature)
   */
  public void addFeatureAsComposition( final Feature parent, final IRelationType propName, final int pos, final Feature newFeature ) throws Exception
  {
    final Object prop = parent.getProperty( propName );

    if( prop instanceof List )
    {
      final List list = (List) prop;
      if( pos == -1 )
        list.add( newFeature );
      else
        list.add( pos, newFeature );
    }
    else if( prop == null ) // element not set
      parent.setProperty( propName, newFeature );
    else
      throw new Exception( "New Feature violates maxOccurs" );

    m_indexMap.put( newFeature.getId(), newFeature );

    // register also features in subtree of new feature
    accept( new RegisterVisitor(), newFeature, FeatureVisitor.DEPTH_INFINITE );
    return;

    // TODO eigene exception entwerfen
  }

  /**
   * @throws Exception
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#setFeatureAsComposition(org.kalypsodeegree.model.feature.Feature,
   *      java.lang.String, org.kalypsodeegree.model.feature.Feature, boolean)
   */
  public void setFeatureAsComposition( final Feature parentFE, final IRelationType linkProp, final Feature linkedFE, final boolean overwrite ) throws Exception
  {
    final Object value = parentFE.getProperty( linkProp );
    if( linkProp.isList() )
      throw new Exception( "can not set feature with maxoccurs > 1, use addFeatureAsComposition instead" );
    if( value == null | overwrite )
    {
      // TODO check if value is allready a feature, then remove it from gmlworkspace
      parentFE.setProperty( linkProp, linkedFE );
      m_indexMap.put( linkedFE.getId(), linkedFE );
      // accept all subfeatures
      accept( new RegisterVisitor(), linkedFE, FeatureVisitor.DEPTH_INFINITE );
      return;
    }
    throw new Exception( "New Feature violates maxOccurs" );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#addFeatureAsAggregation(org.kalypsodeegree.model.feature.Feature,
   *      java.lang.String, int, java.lang.String)
   */
  public void addFeatureAsAggregation( Feature srcFE, IRelationType linkProp, int pos, String featureID ) throws Exception
  {
    // test if feature exists in workspace
    final GMLWorkspace workspace = srcFE.getWorkspace();
    if( workspace.getFeature( featureID ) == null )
      throw new Exception( "tried to link a feature that does not exist in the workspace" );
    if( linkProp.isList() )
    {
      int maxOccurs = linkProp.getMaxOccurs();
      final List list = (List) srcFE.getProperty( linkProp );
      if( list.size() < maxOccurs || maxOccurs == IPropertyType.UNBOUND_OCCURENCY )
        list.add( pos, featureID );
      else
        throw new Exception( "New Feature violates maxOccurs" );
    }
    else if( srcFE.getProperty( linkProp ) == null )
      srcFE.setProperty( linkProp, featureID );
    else
      throw new Exception( "New Feature as already set" );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#addFeatureAsAggregation(org.kalypsodeegree.model.feature.Feature,
   *      java.lang.String, int, java.lang.String)
   */
  public void setFeatureAsAggregation( final Feature srcFE, final IRelationType linkProp, final int pos, final String featureID ) throws Exception
  {
    if( linkProp.isList() )
    {
      // TODO check remove existing correctly
      final int maxOccurs = linkProp.getMaxOccurs();
      final List list = (List) srcFE.getProperty( linkProp );
      if( list.size() < maxOccurs || maxOccurs == IPropertyType.UNBOUND_OCCURENCY )
        list.set( pos, featureID );
      else
        throw new Exception( "New Feature violates maxOccurs" );
    }
    else if( srcFE.getProperty( linkProp ) == null )
    {
      // TODO check remove existing correctly
      srcFE.setProperty( linkProp, featureID );
    }
    else
      throw new Exception( "New Feature as allready set" );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#setFeatureAsAggregation(org.kalypsodeegree.model.feature.Feature,
   *      java.lang.String, java.lang.String, boolean)
   */
  public void setFeatureAsAggregation( Feature srcFE, IRelationType linkProp, String featureID, boolean overwrite ) throws Exception
  {
    // TODO remove existing link correctly
    if( srcFE.getProperty( linkProp ) == null || overwrite )
      srcFE.setProperty( linkProp, featureID );
    else
      throw new Exception( "feature is allready set" );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#removeLinkedAsAggregationFeature(org.kalypsodeegree.model.feature.Feature,
   *      java.lang.String, java.lang.String)
   */
  public boolean removeLinkedAsAggregationFeature( Feature parentFeature, IRelationType linkProp, String childFeatureId )
  {
    final Object prop = parentFeature.getProperty( linkProp );
    if( linkProp.isList() )
    {
      List list = (List) prop;
      return list.remove( childFeatureId );
    }
    if( childFeatureId.equals( parentFeature.getProperty( linkProp ) ) )
    {
      parentFeature.setProperty( linkProp, null );
      return true;
    }
    return false;
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#removeLinkedAsCompositionFeature(org.kalypsodeegree.model.feature.Feature,
   *      java.lang.String, org.kalypsodeegree.model.feature.Feature)
   */
  public boolean removeLinkedAsCompositionFeature( Feature parentFeature, IRelationType linkProp, Feature childFeature )
  {
    boolean result = false;
    final Object prop = parentFeature.getProperty( linkProp );
    if( linkProp.isList() )
    {
      final List list = (List) prop;
      result = list.remove( childFeature );
    }
    else
    {
      if( parentFeature.getProperty( linkProp ) == childFeature )
      {
        parentFeature.setProperty( linkProp, null );
        result = true;
      }
    }
    if( result )
    {
      accept( new UnRegisterVisitor(), childFeature, FeatureVisitor.DEPTH_INFINITE );
      unregisterFeature( childFeature );
    }
    return result;
  }

  private void unregisterFeature( final Feature childFeature )
  {
    m_indexMap.remove( childFeature.getId() );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#accept(org.kalypsodeegree.model.feature.FeatureVisitor,
   *      java.lang.String, int)
   */
  public void accept( final FeatureVisitor fv, final String featurePath, final int depth )
  {
    final Object featureFromPath = getFeatureFromPath( featurePath );
    if( featureFromPath instanceof Feature )
      accept( fv, (Feature) featureFromPath, depth );
    else if( featureFromPath instanceof FeatureList )
      accept( fv, (FeatureList) featureFromPath, depth );
    else
      throw new IllegalArgumentException( "FeaturePath is neither Feature nor FeatureList: " + featurePath );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#isExistingRelation(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypsodeegree.model.feature.Feature, java.lang.String)
   */
  public boolean isExistingRelation( Feature srcFE, Feature destFE, IRelationType relationProp )
  {
    Feature[] features = resolveLinks( srcFE, relationProp );
    for( int i = 0; i < features.length; i++ )
    {
      if( features[i] == destFE )
        return true;
    }
    return false;
  }

  /**
   * TODO: this method does not use any members of this class and so does not depends on this specific implementation.
   * Move it into a utility class.
   * 
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#isAggrigatedLink(org.kalypsodeegree.model.feature.Feature,
   *      java.lang.String, int)
   */
  public boolean isAggregatedLink( Feature parent, IRelationType linkProp, int pos )
  {
    final boolean undefined = false;
    final Object value = parent.getProperty( linkProp );
    if( linkProp.isList() )
    {
      // else must be a list
      final List list = (List) value;
      // TODO: test for 0 does not suffice, test also if length < pos
      if( list.size() == 0 )
        return false;
      final Object object = list.get( pos );
      if( object instanceof Feature )
        return false;
      if( object instanceof String )
        return true;
      return undefined;
    }
    if( value instanceof Feature )
      return false;
    if( value instanceof String )
      return true;
    return undefined;
  }

  /**
   * Finds the parent feature of a feature
   * 
   * @param toFindParentFrom
   *          the feature to find the parent from
   * @return this method returns the parent feature, if the parent feature is the root feature null is returned.
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#getParentFeature(org.kalypsodeegree.model.feature.Feature)
   */
  public Feature getParentFeature( Feature toFindParentFrom )
  {
    // skip root feature
    if( getRootFeature().equals( toFindParentFrom ) )
      return null;
    final Collection collection = m_indexMap.values();
    final Iterator iterator = collection.iterator();
    while( iterator.hasNext() )
    {
      final Feature f = (Feature) iterator.next();
      // skips itself
      if( f.equals( toFindParentFrom ) )
        continue;
      final IFeatureType featureType = f.getFeatureType();
      final IPropertyType[] ftp = featureType.getProperties();
      for( int i = 0; i < ftp.length; i++ )
      {
        final IPropertyType property = ftp[i];
        if( property instanceof IRelationType )
        {
          if( property.isList() )
          {
            final List list = (List) f.getProperty( property );
            for( int j = 0; j < list.size(); j++ )
            {
              final Object childFromList = list.get( j );
              if( childFromList != null && childFromList.equals( toFindParentFrom ) )
              {
                // String substitutionGroup = ((Feature)childFromList).getFeatureType().getSubstitutionGroup();
                return f;
              }
            }
          }
          if( property.equals( toFindParentFrom ) )
            return f;
        }
        if( property.equals( toFindParentFrom ) )
          return f;
      }
    }
    Feature rootFeature = getRootFeature();
    Object[] properties = rootFeature.getProperties();
    for( int i = 0; i < properties.length; i++ )
    {
      Object property = properties[i];
      if( property != null && property.equals( toFindParentFrom ) )
        return rootFeature;
    }
    // TODO throw exception instead of returning null
    return null;
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#contains(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean contains( final Feature feature )
  {
    if( feature == null )
      return false;
    return m_indexMap.containsKey( feature.getId() );
  }

  /**
   * @param parentFeature
   * @param ftp
   * @param pos
   */
  public boolean isBrokenLink( final Feature parentFeature, final IPropertyType ftp, final int pos )
  {
    final Object property = parentFeature.getProperty( ftp );
    if( property == null )
      return false;
    if( property instanceof List )
    {
      Object object = ((List) property).get( pos );
      if( object instanceof Feature )
        return false;
      return !m_indexMap.containsKey( object );
    }
    if( property instanceof Feature )
      return false;
    return !m_indexMap.containsKey( property );
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#getGMLSchema()
   */
  public IGMLSchema getGMLSchema( )
  {
    return m_schema;
  }

  /**
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#getFeatureProviderFactory()
   */
  public IFeatureProviderFactory getFeatureProviderFactory( )
  {
    return m_factory;
  }
}