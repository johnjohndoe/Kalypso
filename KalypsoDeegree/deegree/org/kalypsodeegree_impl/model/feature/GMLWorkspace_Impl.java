package org.deegree_impl.model.feature;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureAssociationTypeProperty;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.feature.FeatureVisitor;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.deegree_impl.model.feature.visitors.FeatureTypeVisitor;

/**
 * @author doemming
 */
public class GMLWorkspace_Impl implements GMLWorkspace
{
  private final Feature m_rootFeature;

  private final URL m_context;

  /** id -> feature */
  private final Map m_indexMap = new HashMap();

  private final FeatureType[] m_featureTypes;

  /**
   * 
   * @see org.deegree.model.feature.GMLWorkspace#getFeature(org.deegree.model.feature.FeatureType,
   *      java.lang.String)
   */
  public Feature getFeature( final String id )
  {
    return (Feature)m_indexMap.get( id );
  }

  public GMLWorkspace_Impl( final FeatureType[] featureTypes, final Feature feature, final URL context )
  {
    m_featureTypes = featureTypes;
    m_context = context;

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
    FeatureAssociationTypeProperty ftp = (FeatureAssociationTypeProperty)srcFeature
        .getFeatureType().getProperty( linkPropertyName );
    FeatureType[] linkFTs = ftp.getAssociationFeatureTypes();
    for( int _ft = 0; _ft < linkFTs.length; _ft++ )
    {
      Feature[] features = getFeatures( linkFTs[_ft] );
      // TODO performance-todo: todo oben aufloesen und hier das feature
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
   * @see org.deegree.model.feature.GMLWorkspace#resolveLinks(org.deegree.model.feature.Feature,
   *      java.lang.String)
   */
  public Feature[] resolveLinks( Feature srcFeature, String linkPropertyName )
  {
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
      FeatureAssociationTypeProperty ftp = (FeatureAssociationTypeProperty)srcFeature
          .getFeatureType().getProperty( linkPropertyName );
      FeatureType[] linkFTs = ftp.getAssociationFeatureTypes();
      for( int _ft = 0; _ft < linkFTs.length; _ft++ )
      {
        Feature[] features = getFeatures( linkFTs[_ft] );
        // TODO performance-todo: todo oben aufloesen und hier das feature
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

  public FeatureType[] getFeatureTypes()
  {
    return m_featureTypes;
  }

  public Feature[] getFeatures( final FeatureType ft )
  {
    final FeatureTypeVisitor visitor = new FeatureTypeVisitor( ft );
    try
    {
      accept( visitor, getRootFeature(), FeatureVisitor.DEPTH_INFINITE );
    }
    catch( Throwable e )
    {
      e.printStackTrace();
    }
      
    final Collection results = visitor.getResults();
    return (Feature[])results.toArray( new Feature[results.size()] );
  }

  private final Collection m_listener = new ArrayList();

  private boolean m_editing;

  /**
   * 
   * @see org.deegree.model.feature.event.ModellEventProvider#addModellListener(org.deegree.model.feature.event.ModellEventListener)
   */
  public void addModellListener( final ModellEventListener listener )
  {
    m_listener.add( listener );
  }

  /**
   * 
   * @see org.deegree.model.feature.event.ModellEventProvider#removeModellListener(org.deegree.model.feature.event.ModellEventListener)
   */
  public void removeModellListener( final ModellEventListener listener )
  {
    m_listener.remove( listener );
  }

  /**
   * 
   * @see org.deegree.model.feature.event.ModellEventProvider#fireModellEvent(org.deegree.model.feature.event.ModellEvent)
   */
  public void fireModellEvent( final ModellEvent event )
  {
    for( final Iterator iter = m_listener.iterator(); iter.hasNext(); )
      ( (ModellEventListener)iter.next() ).onModellChange( event );
  }

  /**
   * @see org.deegree.model.feature.GMLWorkspace#resolveWhoLinksTo(org.deegree.model.feature.Feature,
   *      org.deegree.model.feature.FeatureType, java.lang.String)
   */
  public Feature[] resolveWhoLinksTo( Feature linkTargetfeature, FeatureType linkSrcFeatureType,
      String linkPropertyName )
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
   * @see org.deegree.model.feature.GMLWorkspace#setEditing(boolean)
   */
  public void setEditing( boolean edit )
  {
    m_editing = edit;

    fireModellEvent( new ModellEvent( this, ModellEvent.EDITING_CHANGED ) );
  }

  /**
   * @see org.deegree.model.feature.GMLWorkspace#isEditing()
   */
  public boolean isEditing()
  {
    return m_editing;
  }

  /**
   * @see org.deegree.model.feature.GMLWorkspace#getModelUrl()
   */
  public URL getContext()
  {
    return m_context;
  }

  /**
   * @see org.deegree.model.feature.GMLWorkspace#accept(org.deegree.model.feature.FeatureVisitor,
   *      org.deegree.model.feature.FeatureType, int)
   */
  public void accept( final FeatureVisitor fv, final FeatureType ft, final int depth )
      throws Throwable
  {
    final Feature[] features = getFeatures( ft );

    for( int i = 0; i < features.length; i++ )
    {
      final Feature feature = features[i];
      accept( fv, feature, depth );
    }
  }

  /**
   * @see org.deegree.model.feature.GMLWorkspace#accept(org.deegree.model.feature.FeatureVisitor,
   *      org.deegree.model.feature.Feature, int)
   */
  public void accept( final FeatureVisitor fv, final Feature feature, final int depth )
      throws Throwable
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
   * @see org.deegree.model.feature.GMLWorkspace#accept(org.deegree.model.feature.FeatureVisitor,
   *      java.util.List, int)
   */
  public void accept( final FeatureVisitor fv, final List features, final int depth )
      throws Throwable
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
     * @see org.deegree.model.feature.FeatureVisitor#visit(org.deegree.model.feature.Feature)
     */
    public boolean visit( final Feature f ) throws Throwable
    {
      final FeatureType featureType = f.getFeatureType();

      final String id = f.getId();
      if( m_indexMap.containsKey( id ) )
        System.out.println( "Workspace already contains a feature with id: " + id );
      m_indexMap.put( id, f );
      
      return true;
    }
  }

  /**
   * @see org.deegree.model.feature.GMLWorkspace#getFeatureType(java.lang.String)
   */
  public FeatureType getFeatureType( final String featureName )
  {
    for( int i = 0; i < m_featureTypes.length; i++ )
    {
      final FeatureType ft = m_featureTypes[i];
      if( ft.getName().equals(  featureName ) )
        return ft;
    }
    
    return null;
  }

  /**
   * @see org.deegree.model.feature.GMLWorkspace#getFeatureFromPath(java.lang.String)
   */
  public Object getFeatureFromPath( final String featurePath )
  {
    final String[] segments = featurePath.split( "/" );
    
    Feature aktuFeature = getRootFeature();
    for( int i = 0; i < segments.length; i++ )
    {
      final Object value = aktuFeature.getProperty( segments[i] );
      if( value instanceof Feature )
      {
        aktuFeature = (Feature)value;
        continue;
      }
      else if( value instanceof String )
      {
        aktuFeature = getFeature( (String)value );
        continue;
      }
      else if( value instanceof List )
      {
        // TODO: check if i == segemnt.length - 1
        return value;
      }
    }

    return aktuFeature;
  }

  /**
   * @see org.deegree.model.feature.GMLWorkspace#getFeatureTypeFromPath(java.lang.String)
   */
  public FeatureType getFeatureTypeFromPath( final String featurePath )
  {
    final String[] segments = featurePath.split( "/" );
    
    FeatureType aktuType = getRootFeature().getFeatureType();
    for( int i = 0; i < segments.length; i++ )
    {
      final FeatureAssociationTypeProperty property = (FeatureAssociationTypeProperty)aktuType.getProperty( segments[i] );
      aktuType = property.getAssociationFeatureType();
    }
    
    return aktuType;
  }

}