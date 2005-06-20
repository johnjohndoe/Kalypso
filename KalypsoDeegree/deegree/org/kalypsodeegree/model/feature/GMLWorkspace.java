package org.kalypsodeegree.model.feature;

import java.net.URL;
import java.util.List;
import java.util.Map;

import org.kalypsodeegree.model.feature.event.ModellEventProvider;
import org.kalypsodeegree_impl.model.feature.FeaturePath;

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
   * returns all Features that that link to the linkTargetFeature, with the specified linkPropertyname and are type of
   * linkSourceFeatureType or do substitue it
   */
  public Feature[] resolveWhoLinksTo( Feature linkTargetfeature, FeatureType linkSrcFeatureType, String linkPropertyName );

  public URL getContext();

  /** Visit all Features of the given FeatureType */
  public void accept( final FeatureVisitor fv, final FeatureType ft, final int depth );

  /** Visit the given feature */
  public void accept( final FeatureVisitor fv, final Feature feature, final int depth );

  /** Visit alle features in the given list */
  public void accept( final FeatureVisitor fv, final List features, final int depth );

  /** Visit alle features denoted by this path */
  public void accept( final FeatureVisitor fv, final String featurePath, final int depth );

  public FeatureType getFeatureType( String featureName );

  public Object getFeatureFromPath( final String featurePath );

  public FeatureType getFeatureTypeFromPath( final String featurePath );

  public FeaturePath getFeaturepathForFeature( final Feature feature );

  public String getSchemaLocation();

  public String getSchemaNamespace();

  public Feature createFeature( FeatureType type );

  public void addFeatureAsComposition( Feature parent, String propName, int pos, Feature newFeature ) throws Exception;

  public void addFeatureAsAggregation( Feature parent, String propName, int pos, String featureID ) throws Exception;

  /**
   * removes a related feature from the parent. Works only if the child is linked <br>
   * <i>and the relation is not a composition </i> see also
   * 
   * @see org.kalypsodeegree.model.feature.GMLWorkspace#removeLinkedAsCompositionFeature(org.kalypsodeegree.model.feature.Feature,
   *      java.lang.String, org.kalypsodeegree.model.feature.Feature)
   */
  public boolean removeLinkedAsAggregationFeature( Feature parentFeature, String propName, String childFeatureID );

  /**
   * removes a related feature from the parent. Works only if the child is a composition <br>
   * <i>and the relation is not linked </i>
   */
  public boolean removeLinkedAsCompositionFeature( Feature parentFeature, String propName, Feature childFeature );

  public Map getNamespaceMap();

  /**
   * return true if these feature are related
   */
  public boolean isExistingRelation( Feature f1, Feature f2, String relationPropertyName );
}