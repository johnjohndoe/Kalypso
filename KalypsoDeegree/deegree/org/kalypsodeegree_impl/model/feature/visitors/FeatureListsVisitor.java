package org.kalypsodeegree_impl.model.feature.visitors;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureVisitor;

/**
 * Visits all FeatureLists in a Feature-Hirarchy
 * 
 * @author belger
 */
public abstract class FeatureListsVisitor implements FeatureVisitor
{

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    final Object[] properties = f.getProperties();
    for( int i = 0; i < properties.length; i++ )
    {
      final Object p = properties[i];
      
      if( p instanceof FeatureList )
        visitList( (FeatureList)p );
    }
    
    return true;
  }

  protected abstract void visitList( final FeatureList fl );
}
