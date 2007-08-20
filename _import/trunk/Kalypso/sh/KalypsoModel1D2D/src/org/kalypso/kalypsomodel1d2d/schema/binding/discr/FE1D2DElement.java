package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.Util;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

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

  public FE1D2DElement( final Feature featureToBind )
  {
    this( featureToBind, Kalypso1D2DSchemaConstants.WB1D2D_F_ELEMENT, (Class<CT>) IFE1D2DComplexElement.class );
  }

  public FE1D2DElement( final Feature featureToBind, final QName featureQName, final Class<CT> complexElementClass )
  {
    super( featureToBind, featureQName );
    //
    Object prop = null;
    try
    {
      prop = featureToBind.getProperty( Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENT_CONTAINERS );
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
    }

    if( prop == null )
    {
      // create the property that is still missing
      containers = new FeatureWrapperCollection<CT>( featureToBind,
      // TODO: problem here?
      Kalypso1D2DSchemaConstants.WB1D2D_F_COMPLEX_ELE_2D, Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENT_CONTAINERS, complexElementClass );
    }
    else
    {
      // just wrapped the existing one
      containers = new FeatureWrapperCollection<CT>( featureToBind, complexElementClass, Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENT_CONTAINERS );
    }
  }

  /**
   * This constructor creates {@link FE1D2D_2DElement} based on a feature which is created as child of the given parent
   * feaure and linked to it by the property of the type specified by the argument propQName. The Type of the feature is
   * also specified by the given element. This constructor is typicaly used to construct feature like
   * wb1d2d:FE1D2DQuadriElement, wb1d2d:FE1D2DTriElement and wb1d2d:FE1D2DContinuityLine
   * 
   * @param parentFeature
   *            the parent feature for the new wbr:Roughness class
   * @param propQName
   *            the Q-name of the linking property type
   * @param newFeatureQName
   *            the Q-Name denoting the type of the new feature
   * @throws IllegalArgumentException
   *             if workspace is null or the roughness collection is not part of the workspace
   */
  public FE1D2DElement( final Feature parentFeature, final QName propQName, final QName newFeatureQName ) throws IllegalArgumentException
  {
    this( Util.createFeatureAsProperty( parentFeature, propQName, newFeatureQName ) );
  }

  public FE1D2DElement( final Feature parentFeature, final QName propQName, final String gmlID )
  {
    this( FeatureHelper.createFeatureWithId( Kalypso1D2DSchemaConstants.WB1D2D_F_ELEMENT, parentFeature, propQName, gmlID ) );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEElement#getContainers()
   */
  public IFeatureWrapperCollection<CT> getContainers( )
  {
    return containers;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#assignRoughness(java.lang.String,
   *      java.lang.Double, java.lang.Double, java.lang.Double, java.lang.String)
   */
  public FeatureChange[] assignRoughness( final String roughnessID, final Double correctionParameterKS, final Double correctionParameterAxAy, final Double correctionParameterDP, final String roughnessStyle )
  {
    final Feature elementFeature = getWrappedFeature();

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
  public abstract List<IFE1D2DNode> getNodes( );
}
