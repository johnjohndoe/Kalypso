package org.deegree.model.feature;

import java.net.URL;
import java.util.List;

import org.deegree.model.feature.event.ModellEventProvider;

/**
 * @author doemming
 */
public interface GMLWorkspace extends ModellEventProvider
{
  public Feature getRootFeature();

  public FeatureType[] getFeatureTypes();

  public Feature[] getFeatures( final FeatureType ft );

  public Feature getFeature( final String id );

  /**
   * resolves the associationlink to a feature, maxOccurs =1
   */
  public Feature resolveLink( final Feature srcFeature, final String linkPropertyName );

  /**
   * resolves the associationlink to a feature, maxOccurs >1
   */
  public Feature[] resolveLinks( final Feature srcFeature, final String linkPropertyName );

  /**
   * returns all Features that that link to the linkTargetFeature, with the
   * specified linkPropertyname and are type of linkSourceFeatureType or do substitue it
   */
  public Feature[] resolveWhoLinksTo( Feature linkTargetfeature, FeatureType linkSrcFeatureType,
      String linkPropertyName );

  public URL getContext();
  
  /** Visit all Features of the given FeatureType */
  public void accept( final FeatureVisitor fv, final FeatureType ft, final int depth ) throws Throwable;

  /** Visit the given feature */
  public void accept( final FeatureVisitor fv, final Feature feature, final int depth ) throws Throwable;

  /** Visit alle features in the given list */
  public void accept( final FeatureVisitor fv, final List features, final int depth ) throws Throwable;

  public FeatureType getFeatureType( String featureName );

  public Object getFeatureFromPath( final String featurePath );
  
  public FeatureType getFeatureTypeFromPath( final String featurePath );
}