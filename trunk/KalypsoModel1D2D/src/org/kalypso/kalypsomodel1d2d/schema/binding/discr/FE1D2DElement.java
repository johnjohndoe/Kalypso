package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * Provide the default implementation for {@link org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEElement}. Those
 * classes kann be used as java abtract for the subtituable of wb1d2d:FE1D2D_2DElement: wb1d2d:FE1D2DQuadriElement,
 * wb1d2d:FE1D2DTriElement and wb1d2d:FE1D2DContinuityLine
 *
 *
 * @author Gernot Belger, Patrice Congo
 * @see IFE1D2DContinuityLine
 * @see FE1D2DContinuityLine
 */
public abstract class FE1D2DElement extends Feature_Impl implements IFE1D2DElement
{
  private final IFeatureBindingCollection<IFE1D2DComplexElement> containers = new FeatureBindingCollection<>( this, IFE1D2DComplexElement.class, WB1D2D_PROP_ELEMENT_CONTAINERS );

  public FE1D2DElement( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  @Override
  public IFeatureBindingCollection<IFE1D2DComplexElement> getContainers( )
  {
    return containers;
  }
}