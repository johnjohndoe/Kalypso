package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * @author Stefan Kurzbach
 */
public abstract class FE1D2DElement extends Feature_Impl implements IFE1D2DElement
{
  public FE1D2DElement( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  private FeatureList complexElementsInternal( )
  {
    return (FeatureList)getProperty( WB1D2D_PROP_ELEMENT_CONTAINERS );
  }

  @Override
  public void addLinkedComplexElement( IFE1D2DComplexElement element )
  {
    Assert.throwIAEOnNullParam( element, "element" ); //$NON-NLS-1$
    if( !complexElementsInternal().containsLinkTo( element ) )
      complexElementsInternal().addLink( element );
  }

  @Override
  public void removeLinkedComplexElement( IFE1D2DComplexElement element )
  {
    Assert.throwIAEOnNullParam( element, "element" ); //$NON-NLS-1$
    if( complexElementsInternal().containsLinkTo( element ) )
      complexElementsInternal().removeLink( element );
  }

  @Override
  public IFE1D2DComplexElement[] getLinkedElements( )
  {
    return complexElementsInternal().toFeatures( new IFE1D2DComplexElement[complexElementsInternal().size()] );
  }
}