package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import org.kalypsodeegree.model.feature.Feature;

/**
 * Tagging interface for all kinds of mesh items
 * 
 * @author Stefan Kurzbach
 */
public interface IFENetItem extends Feature
{
  void addLinkedComplexElement( IFE1D2DComplexElement element );

  void removeLinkedComplexElement( IFE1D2DComplexElement element );

  IFE1D2DComplexElement[] getLinkedElements( );
}
