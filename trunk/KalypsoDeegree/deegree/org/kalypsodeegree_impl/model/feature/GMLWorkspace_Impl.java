package org.deegree_impl.model.feature;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureAssociationTypeProperty;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.deegree_impl.gml.schema.GMLSchema;

/**
 * @author doemming
 */
public class GMLWorkspace_Impl implements GMLWorkspace
{
  private final GMLSchema m_schema;

  private final Feature m_rootFeature;
  private final URL m_modelURL;

  // final UndoManager ??

  /**
   * HashMap(featuretype,List(feature)) TODO:
   * HashMap(featuretype,HashMap(id,feature))
   */
  private final HashMap m_featureMap = new HashMap();

  /**
   * 
   * @see org.deegree.model.feature.GMLWorkspace#getFeature(org.deegree.model.feature.FeatureType, java.lang.String)
   */
  public Feature getFeature( FeatureType ft, String id )
  {
    List list = (List)m_featureMap.get( ft );
    for( Iterator iter = list.iterator(); iter.hasNext(); )
    {
      Feature feature = (Feature)iter.next();
      if( id.equals( feature.getId() ) )
        return feature;
    }
    return null;
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

  public GMLWorkspace_Impl( GMLSchema schema, Feature feature,URL gmlURL )
  {
    m_schema = schema;
    m_modelURL=gmlURL;
    FeatureType[] featureTypes = m_schema.getFeatureTypes();
    for( int i = 0; i < featureTypes.length; i++ )
    {
      m_featureMap.put( featureTypes[i], new ArrayList() );
    }
    m_rootFeature = feature;
    registerFeature( feature );
  }

  private void addFeature( Feature feature )
  {
    List list = (List)m_featureMap.get( feature.getFeatureType() );
    list.add( feature );
  }

  private void registerFeature( Feature feature )
  {
    if( feature == null )
      return;
    addFeature( feature );
    FeatureType featureType = feature.getFeatureType();
    FeatureTypeProperty[] ftps = featureType.getProperties();
    for( int i = 0; i < ftps.length; i++ )
    {
      if( ftps[i] instanceof FeatureAssociationTypeProperty )
      {
        Object value = feature.getProperty( ftps[i].getName() );
        if( value instanceof Feature )
          registerFeature( (Feature)value );
        if( value instanceof List )
          registerFeature( (List)value );
      }
    }
  }

  private void registerFeature( List list )
  {
    if( list == null )
      return;
    for( Iterator iter = list.iterator(); iter.hasNext(); )
    {
      Object value = iter.next();
      if( value instanceof Feature )
        registerFeature( (Feature)value );
    }
  }

  public Feature getRootFeature()
  {
    return m_rootFeature;
  }

  public GMLSchema getSchema()
  {
    return m_schema;
  }

  public FeatureType[] getFeatureTypes()
  {
    return m_schema.getFeatureTypes();
  }

  public Feature[] getFeatures( FeatureType ft )
  {
    List list = (List)m_featureMap.get( ft );
    return (Feature[])list.toArray( new Feature[list.size()] );
  }

  private final Collection m_listener = new ArrayList();

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

    FeatureType[] substiFTs = m_schema.getResolveSubstitutionGroup( linkSrcFeatureType );
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
   * @see org.deegree.model.feature.GMLWorkspace#getModelUrl()
   */
  public URL getModelUrl()
  {
    return m_modelURL;
  }

}