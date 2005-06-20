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
  private final Collection m_results = new ArrayList();

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    m_results.add( f );
    return true;
  }

  /**
   * Returns alle visited features.
   * 
   * @param reset
   *          if true, resets the inner result set, so next call to getResults results in empty array.
   */
  public Feature[] getResults( final boolean reset )
  {
    final Feature[] features = (Feature[])m_results.toArray( new Feature[m_results.size()] );
    if( reset )
      m_results.clear();
    return features;
  }
}
