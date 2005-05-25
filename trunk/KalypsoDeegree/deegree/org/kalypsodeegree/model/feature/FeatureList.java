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

  /**
   * @return the parent feature or null it is not known or list is allready some
   *         kind of rootelement
   */
  public Feature getParentFeature();

  /**
   * @return property of parent feature that has this list or null if it is not
   *         known or list is allready some kind of rootelement
   */
  public FeatureTypeProperty getParentFeatureTypeProperty();
}
