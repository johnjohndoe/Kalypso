package org.kalypsodeegree_impl.model.feature.visitors;

import java.util.ArrayList;
import java.util.Collection;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;

/**
 * <p>
 * Simply collects all visited features
 * </p>
 * 
 * @author belger
 */
public class CollectorVisitor implements FeatureVisitor
{
  private final Collection<Feature> m_results = new ArrayList<Feature>();

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public synchronized boolean visit( final Feature f )
  {
    m_results.add( f );
    return true;
  }

  /**
   * Returns alle visited features.
   * <p>
   * IMPORTANT: this method has been synchronized since toggel between radio buttons cause a
   * ArrayIndexOutOfBoundsException in the first line.
   * </p>
   * 
   * @param reset
   *          if true, resets the inner result set, so next call to getResults results in empty array.
   */
  public synchronized Feature[] getResults( final boolean reset )
  {
    final Feature[] features = m_results.toArray( new Feature[0] );
    if( reset )
      m_results.clear();
    return features;
  }
}
