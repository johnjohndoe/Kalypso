package org.kalypsodeegree.model.feature;

import java.util.List;

import org.kalypsodeegree.model.sort.JMSpatialIndex;

/**
 * @author belger
 */
public interface FeatureList extends List, JMSpatialIndex
{
  public Feature[] toFeatures();
  
  /** Visit all Features in the list. */
  public void accept( final FeatureVisitor visitor );
}
