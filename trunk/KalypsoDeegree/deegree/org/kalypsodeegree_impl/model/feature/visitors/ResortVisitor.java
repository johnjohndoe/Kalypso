package org.kalypsodeegree_impl.model.feature.visitors;

import org.kalypsodeegree.model.feature.FeatureList;

/**
 * @author belger
 */
public class ResortVisitor extends FeatureListsVisitor
{
  /**
   * @see org.kalypsodeegree_impl.model.feature.visitors.FeatureListsVisitor#visitList(org.kalypsodeegree.model.feature.FeatureList)
   */
  protected void visitList( final FeatureList fl )
  {
    fl.resort();
  }
}
