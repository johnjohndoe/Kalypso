package org.deegree_impl.model.feature.visitors;

import org.deegree.model.feature.FeatureList;

/**
 * @author belger
 */
public class ResortVisitor extends FeatureListVisitor
{
  /**
   * @see org.deegree_impl.model.feature.visitors.FeatureListVisitor#visitList(org.deegree.model.feature.FeatureList)
   */
  protected void visitList( final FeatureList fl )
  {
    fl.resort();
  }
}
