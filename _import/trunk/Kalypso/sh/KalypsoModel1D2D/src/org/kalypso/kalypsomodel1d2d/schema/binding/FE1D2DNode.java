package org.kalypso.kalypsomodel1d2d.schema.binding;


import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsosimulationmodel.core.FeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

/**
 * @author Gernot Belger
 */
public class FE1D2DNode extends AbstractFeatureBinder implements IFE1D2DNode<IFE1D2DEdge>
{
  public final static QName QNAME_FE1D2DNode = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "FE1D2DNode" );

  public final static QName QNAME_PROP_POINT = new QName( NS.GML3, "pointProperty" );
  
  public final static QName QNAME_PROP_NODE_CONTAINERS=
    new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "fe1d2dNodeContainer" );
 
  //TODO check remove of discretisation model
  //private final FE1D2DDiscretisationModel m_discretisationModel;
  private final FeatureWrapperCollection<IFE1D2DEdge> containers;

  public FE1D2DNode( final Feature featureToBind )
  {
    super( featureToBind, QNAME_FE1D2DNode );

    //m_discretisationModel = new FE1D2DDiscretisationModel( getFeature().getParent() );
    //
    Object prop=featureToBind.getProperty(QNAME_PROP_NODE_CONTAINERS);
   
    if(prop==null)
    {
      //create the property tha is still missing
      containers= 
        new FeatureWrapperCollection<IFE1D2DEdge>(
                            featureToBind,
                            Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2DNODE,
                            QNAME_PROP_NODE_CONTAINERS,
                            IFE1D2DEdge.class);
    }
    else
    {
      
      //just wrapped the existing one
      containers= 
        new FeatureWrapperCollection<IFE1D2DEdge>(
                            featureToBind,
                            IFE1D2DEdge.class,//<IFE1D2DElement,IFE1D2DNode<IFE1D2DEdge>>.class,
                            QNAME_PROP_NODE_CONTAINERS);
    }
  }
  
  /**
     * This constructor creates {@link FE1D2DNode} based on a
     * wb1d2d:FE1D2DNode feature which is created as child of the
     * given parent feaure and linked to it by the property of the 
     * type specified by the argument propQName.
     * 
     * @param parentFeature the parent feature for the new wbr:Roughness class
     * @param propQName  the Q-name of the linking property type
     * @throws IllegalArgumentException if workspace is null
     *  or the roughness collection is not part of the workspace
     */
  public FE1D2DNode(
        Feature parentFeature,
        QName propQName)
        throws IllegalArgumentException
  {
    this(Util.createFeatureAsProperty(
                parentFeature, 
                propQName,
                Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2DNODE));   
  }
  
  

  public GM_Point getPoint( )
  {
    return (GM_Point) getFeature().getProperty( QNAME_PROP_POINT );
  }

  public void setPoint( final GM_Point point )
  {
    getFeature().setProperty( QNAME_PROP_POINT, point );
  }
  
  public static FE1D2DNode createNode( final FE1D2DDiscretisationModel discModel )
  {
    final Feature parentFeature = discModel.getFeature();
    final IFeatureType nodeType = parentFeature.getFeatureType().getGMLSchema().getFeatureType( QNAME_FE1D2DNode );
    final Feature nodeFeature = parentFeature.getWorkspace().createFeature( parentFeature, nodeType );
    return new FE1D2DNode( nodeFeature );
  }

//  public FE1D2DDiscretisationModel getDiscretisationModel( )
//  {
//    return m_discretisationModel;
//  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFENode#getContainers()
   */
  public IFeatureWrapperCollection<IFE1D2DEdge> getContainers( )
  {
    return containers;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapper#getWrappedFeature()
   */
  public Feature getWrappedFeature( )
  {
    return getFeature();
  }
  
  
}
