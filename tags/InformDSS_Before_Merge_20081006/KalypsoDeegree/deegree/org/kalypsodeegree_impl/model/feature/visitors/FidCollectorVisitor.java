package org.kalypsodeegree_impl.model.feature.visitors;

import java.util.ArrayList;
import java.util.Collection;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;

/**
 * <p>
 * Simply collects all Ids of visited features
 * </p>
 * 
 * @author belger
 */
public class FidCollectorVisitor implements FeatureVisitor
{
  private final Collection<String> m_results = new ArrayList<String>();

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public synchronized boolean visit( final Feature f )
  {
    m_results.add( f.getId() );
    return true;
  }

  /**
   * Returns alle visited features ids.
   * 
   * @param reset
   *          if true, resets the inner result set, so next call to getResults results in empty array.
   */
  public String[] getResults( final boolean reset )
  {
    final String[] result = m_results.toArray( new String[m_results.size()] );
    if( reset )
      m_results.clear();
    return result;
  }
}
