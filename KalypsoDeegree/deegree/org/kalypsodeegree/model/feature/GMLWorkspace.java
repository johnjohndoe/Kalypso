package org.deegree.model.feature;

import org.deegree.model.feature.event.ModellEventListener;
import org.deegree.model.feature.event.ModellEventProvider;
import org.deegree_impl.clients.wcasclient.model.ModelList;
import org.deegree_impl.gml.schema.GMLSchema;

/**
 * @author doemming
 */
public interface GMLWorkspace extends ModellEventProvider
{
  public Feature getRootFeature();

  public GMLSchema getSchema();

  public FeatureType[] getFeatureTypes();

  public Feature[] getFeatures( FeatureType ft );
/**
 * resolves the associationlink to a feature, maxOccurs =1
 */
  public Feature resolveLink( Feature srcFeature, String linkPropertyName );
  /**
   * resolves the associationlink to a feature, maxOccurs >1
   */
  public Feature[] resolveLinks( Feature srcFeature, String linkPropertyName );

  public Feature getFeature( FeatureType ft,String id );

}