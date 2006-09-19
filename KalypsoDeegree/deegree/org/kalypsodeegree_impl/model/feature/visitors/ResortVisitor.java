package org.kalypsodeegree_impl.model.feature.visitors;

import org.kalypsodeegree.model.feature.FeatureList;

/**
 * @deprecated Because of the new {@link org.kalypsodeegree.model.sort.JMSpatialIndex#invalidate()} method, this visitor
 *             is now obsolete. Every change to a geometry property results in invalidating of the corresponding
 *             feature-list. The next call to query resorts it automatically. So You just have to remove all uses of the
 *             ResortVisitor and test if everything still works as it should.
 * @author belger
 */
public class ResortVisitor extends FeatureListsVisitor
{
  /**
   * @see org.kalypsodeegree_impl.model.feature.visitors.FeatureListsVisitor#visitList(org.kalypsodeegree.model.feature.FeatureList)
   */
  @Override
  protected void visitList( final FeatureList fl )
  {
    fl.invalidate();
  }
}
