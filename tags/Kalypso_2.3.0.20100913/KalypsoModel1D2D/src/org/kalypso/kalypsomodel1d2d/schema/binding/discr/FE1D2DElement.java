package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;

/**
 * Provide the default implementation for {@link org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEElement}.
 * Those classes kann be used as java abtract for the subtituable of wb1d2d:FE1D2D_2DElement:
 * wb1d2d:FE1D2DQuadriElement, wb1d2d:FE1D2DTriElement and wb1d2d:FE1D2DContinuityLine
 *
 *
 * @author Gernot Belger, Patrice Congo
 * @see IFE1D2DContinuityLine
 * @see FE1D2DContinuityLine
 */
public abstract class FE1D2DElement<CT extends IFE1D2DComplexElement, ET extends IFE1D2DEdge> extends AbstractFeatureBinder implements IFE1D2DElement<CT, ET>
{
  private final IFeatureWrapperCollection<CT> containers;

  public FE1D2DElement( final Feature featureToBind, final QName featureQName, final Class<CT> complexElementClass )
  {
    super( featureToBind, featureQName );
    //
    Object prop = null;
    try
    {
      prop = featureToBind.getProperty( IFE1D2DElement.WB1D2D_PROP_ELEMENT_CONTAINERS );
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
    }

    if( prop == null )
    {
      // create the property that is still missing
      // TODO: remove this stuff
      containers = new FeatureWrapperCollection<CT>( featureToBind, IFE1D2DElement.WB1D2D_PROP_ELEMENT_CONTAINERS, IFE1D2DElement.WB1D2D_PROP_ELEMENT_CONTAINERS, complexElementClass );
    }
    else
    {
      // just wrapped the existing one
      containers = new FeatureWrapperCollection<CT>( featureToBind, complexElementClass, IFE1D2DElement.WB1D2D_PROP_ELEMENT_CONTAINERS );
    }
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEElement#getContainers()
   */
  @Override
  public IFeatureWrapperCollection<CT> getContainers( )
  {
    return containers;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#assignRoughness(java.lang.String,
   *      java.lang.Double, java.lang.Double, java.lang.Double, java.lang.String)
   */
  @Override
  public FeatureChange[] assignRoughness( final String roughnessID, final Double correctionParameterKS, final Double correctionParameterAxAy, final Double correctionParameterDP, final String roughnessStyle )
  {
    final Feature elementFeature = getFeature();

    setRoughnessClsID( roughnessID );
    setRoughnessCorrectionKS( correctionParameterKS );
    setRoughnessCorrectionAxAy( correctionParameterAxAy );
    setRoughnessCorrectionDP( correctionParameterDP );
    setRoughnessStyle( roughnessStyle );

    final IFeatureType featureType = elementFeature.getFeatureType();

    final FeatureChange[] changes = new FeatureChange[5];
    changes[0] = new FeatureChange( elementFeature, featureType.getProperty( PROP_ROUGHNESS_CLS_ID ), roughnessID );
    changes[1] = new FeatureChange( elementFeature, featureType.getProperty( PROP_ROUGHNESS_CORRECTION_KS ), correctionParameterKS );
    changes[2] = new FeatureChange( elementFeature, featureType.getProperty( PROP_ROUGHNESS_CORRECTION_AXAY ), correctionParameterAxAy );
    changes[3] = new FeatureChange( elementFeature, featureType.getProperty( PROP_ROUGHNESS_CORRECTION_DP ), correctionParameterDP );
    changes[4] = new FeatureChange( elementFeature, featureType.getProperty( PROP_ROUGHNESS_STYLE ), roughnessStyle );

    return changes;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement#getNodes()
   */
  @Override
  public abstract List<IFE1D2DNode> getNodes( );
}
