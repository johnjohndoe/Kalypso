package org.deegree_impl.model.feature.visitors;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureList;
import org.deegree.model.feature.FeatureVisitor;

/**
 * Visits all FeatureLists in a Feature-Hirarchy
 * 
 * @author belger
 */
public abstract class FeatureListsVisitor implements FeatureVisitor
{

  /**
   * @see org.deegree.model.feature.FeatureVisitor#visit(org.deegree.model.feature.Feature)
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
