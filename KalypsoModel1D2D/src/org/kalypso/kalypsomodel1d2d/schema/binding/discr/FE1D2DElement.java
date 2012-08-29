package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.command.FeatureChange;
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
  private final IFeatureBindingCollection<IFE1D2DComplexElement> containers = new FeatureBindingCollection<IFE1D2DComplexElement>( this, IFE1D2DComplexElement.class, WB1D2D_PROP_ELEMENT_CONTAINERS );

  public FE1D2DElement( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  @Override
  public IFeatureBindingCollection<IFE1D2DComplexElement> getContainers( )
  {
    return containers;
  }

  @Override
  public FeatureChange[] assignRoughness( final String roughnessID, final Double correctionParameterKS, final Double correctionParameterAxAy, final Double correctionParameterDP, final String roughnessStyle )
  {
    setRoughnessClsID( roughnessID );
    setRoughnessCorrectionKS( correctionParameterKS );
    setRoughnessCorrectionAxAy( correctionParameterAxAy );
    setRoughnessCorrectionDP( correctionParameterDP );
    setRoughnessStyle( roughnessStyle );

    final IFeatureType featureType = this.getFeatureType();

    final FeatureChange[] changes = new FeatureChange[5];
    changes[0] = new FeatureChange( this, featureType.getProperty( PROP_ROUGHNESS_CLS_ID ), roughnessID );
    changes[1] = new FeatureChange( this, featureType.getProperty( PROP_ROUGHNESS_CORRECTION_KS ), correctionParameterKS );
    changes[2] = new FeatureChange( this, featureType.getProperty( PROP_ROUGHNESS_CORRECTION_AXAY ), correctionParameterAxAy );
    changes[3] = new FeatureChange( this, featureType.getProperty( PROP_ROUGHNESS_CORRECTION_DP ), correctionParameterDP );
    changes[4] = new FeatureChange( this, featureType.getProperty( PROP_ROUGHNESS_STYLE ), roughnessStyle );

    return changes;
  }
}