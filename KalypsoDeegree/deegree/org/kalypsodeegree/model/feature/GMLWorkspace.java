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

  public Feature resolveLink( Feature srcFeature, String linkPropertyName );
}