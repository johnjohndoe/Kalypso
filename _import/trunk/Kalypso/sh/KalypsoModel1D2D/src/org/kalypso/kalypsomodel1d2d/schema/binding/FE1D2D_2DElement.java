package org.kalypso.kalypsomodel1d2d.schema.binding;

import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsosimulationmodel.core.FeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfaceInterpolation;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.geometry.GM_SurfaceInterpolation_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Provide the default implementation for 
 * {@link org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEElement}.
 * Those classes kann be used as java abtract for the subtituable of 
 * wb1d2d:FE1D2D_2DElement:  wb1d2d:FE1D2DQuadriElement, 
 *  wb1d2d:FE1D2DTriElement and 
 * wb1d2d:FE1D2DContinuityLine 
 *  
 * 
 * @author Gernot Belger, Patrice Congo
 * @see IFE1D2DContinuityLine
 * @see FE1D2DContinuityLine
 */
public class FE1D2D_2DElement extends AbstractFeatureBinder implements IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge>
{
  private final IFeatureWrapperCollection<IFE1D2DComplexElement> containers;

  private final IFeatureWrapperCollection<IFE1D2DEdge> edges;

  public FE1D2D_2DElement( final Feature featureToBind )
  {
    super( featureToBind, Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2D_2DElement );
    //
    Object prop = 
        featureToBind.getProperty( 
            Kalypso1D2DSchemaConstants.WB1D2D_PROP_DIRECTEDEDGE );

    if( prop == null )
    {
      // create the property tha is still missing
      containers = 
          new FeatureWrapperCollection<IFE1D2DComplexElement>( 
                featureToBind, 
                Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2D_2DElement, 
                Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENT_CONTAINERS, 
                IFE1D2DComplexElement.class );
    }
    else
    {
      // just wrapped the existing one
      containers = 
        new FeatureWrapperCollection<IFE1D2DComplexElement>( 
                featureToBind, 
                IFE1D2DComplexElement.class,// <IFE1D2DElement,IFE1D2DNode<IFE1D2DEdge>>.class,
                Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENT_CONTAINERS );
    }

    // edges
    prop = featureToBind.getProperty( Kalypso1D2DSchemaConstants.WB1D2D_PROP_DIRECTEDEDGE );

    if( prop == null )
    {
      // create the property that is still missing
      edges = new FeatureWrapperCollection<IFE1D2DEdge>( featureToBind, Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2D_2DElement, Kalypso1D2DSchemaConstants.WB1D2D_PROP_DIRECTEDEDGE, IFE1D2DEdge.class );
    }
    else
    {
      // just wrapped the existing one
      edges = 
        new FeatureWrapperCollection<IFE1D2DEdge>( 
                  featureToBind, 
                  IFE1D2DEdge.class,// <IFE1D2DElement,IFE1D2DNode<IFE1D2DEdge>>.class,
                  Kalypso1D2DSchemaConstants.WB1D2D_PROP_DIRECTEDEDGE );
    }
  }

  /**
   * This constructor creates {@link FE1D2D_2DElement} based on a
   * feature which is created as child of the given parent feaure and 
   * linked to it by the property of the type specified by the 
   * argument propQName. The Type of the feature is also specified by the given
   * element.
   * This constructor is typicaly used to construct feature like 
   * wb1d2d:FE1D2DQuadriElement, wb1d2d:FE1D2DTriElement and 
   * wb1d2d:FE1D2DContinuityLine 
   * 
   * @param parentFeature the parent feature for the new wbr:Roughness class
   * @param propQName  the Q-name of the linking property type
   * @param newFeatureQName the Q-Name denoting the type of the
   *            new feature
   * @throws IllegalArgumentException if workspace is null
   *  or the roughness collection is not part of the workspace
   */
  public FE1D2D_2DElement(
        Feature parentFeature,
        QName propQName,
        QName newFeatureQName
        )
        throws IllegalArgumentException
  {
    this(Util.createFeatureAsProperty(
                parentFeature, 
                propQName,
                newFeatureQName));   
  }
  
  /**
   * Returns the (dereferenced) nodes of this egde. Elements of the array may be null.
   */
  public FE1D2DEdge[] getEdgesAsArray( )
  {
    final Feature feature = getFeature();
    final GMLWorkspace workspace = feature.getWorkspace();
    final List edgeList = 
        (List) feature.getProperty( 
                Kalypso1D2DSchemaConstants.WB1D2D_PROP_DIRECTEDEDGE );

    final FE1D2DEdge[] edges = new FE1D2DEdge[edgeList.size()];
    for( int i = 0; i < edges.length; i++ )
    {
      /*
       * Accessing the list via index is ok here, because we should never have edges with more than 2 nodes.
       */
      final String ref = (String) edgeList.get( i );
      if( ref == null )
        edges[i] = null;
      else
        edges[i] = new FE1D2DEdge( workspace.getFeature( ref ) );
    }

    return edges;
  }

  @SuppressWarnings("unchecked")
  public void setEdges( final FE1D2DEdge[] edges )
  {
    final Feature feature = getFeature();
    final List edgeList = (List) feature.getProperty( Kalypso1D2DSchemaConstants.WB1D2D_PROP_DIRECTEDEDGE );

    edgeList.clear();

    for( final FE1D2DEdge edge : edges )
      edgeList.add( edge.getFeature().getId() );
  }

  /**
   * Recalculates the geometry of this element. Used by the corresponding property function.
   */
  public GM_Object recalculateElementGeometry( ) throws GM_Exception
  {
    final FE1D2DEdge[] edges = getEdgesAsArray();

    if( edges.length < 3 )
      return null;

    final FE1D2DNode[] nodes = new FE1D2DNode[edges.length + 1];
    for( int i = 0; i < edges.length; i++ )
    {
      final FE1D2DEdge edge0 = edges[i];
      final FE1D2DEdge edge1 = edges[(i + 1) % edges.length];

      final FE1D2DNode[] edge0Nodes = edge0.getNodesAsArray();
      final FE1D2DNode[] edge1Nodes = edge1.getNodesAsArray();

      final FE1D2DNode edge0node0 = edge0Nodes[0];
      final FE1D2DNode edge0node1 = edge0Nodes[1];
      final FE1D2DNode edge1node0 = edge1Nodes[0];
      final FE1D2DNode edge1node1 = edge1Nodes[1];

      /* Always take the node which does not fit to the next edge */
      if( edge0node1.equals( edge1node0 ) )
        nodes[i] = edge0node0;
      else if( edge0node1.equals( edge1node1 ) )
        nodes[i] = edge0node0;
      else if( edge0node0.equals( edge1node0 ) )
        nodes[i] = edge0node1;
      else if( edge0node0.equals( edge1node1 ) )
        nodes[i] = edge0node1;
    }

    nodes[edges.length] = nodes[0];

    /* Positions from nodes */
    final GM_Position[] poses = new GM_Position[nodes.length];

    if( nodes.length < 2 )
      return null;

    // REMARK: we assume here, that all nodes live in the same coordinate
    // system.
    final CS_CoordinateSystem crs = nodes[0].getPoint().getCoordinateSystem();

    for( int i = 0; i < poses.length; i++ )
    {
      final GM_Point point = nodes[i].getPoint();
      final GM_Position position = point.getPosition();
      poses[i] = GeometryFactory.createGM_Position( position.getX(), position.getY() );
    }
    
    return GeometryFactory.createGM_Surface( 
                    poses, 
                    new GM_Position[0][], 
                    new GM_SurfaceInterpolation_Impl( 
                                  GM_SurfaceInterpolation.PLANAR ), 
                    crs );
      
  }

  public static FE1D2D_2DElement createPolyElement( 
                          final FE1D2DDiscretisationModel discModel )
  {
    final Feature parentFeature = discModel.getFeature();
    final IFeatureType parentFT = parentFeature.getFeatureType();
    final IRelationType parentElementProperty = 
            (IRelationType) parentFT.getProperty( 
                Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENTS );
    
    final IFeatureType polyType = 
          parentFT.getGMLSchema().getFeatureType( 
              Kalypso1D2DSchemaConstants.WB1D2D_F_POLY_ELEMENT );
    
    final Feature edgeFeature = 
          parentFeature.getWorkspace().createFeature( 
                  parentFeature, parentElementProperty, polyType );
    
    return new FE1D2D_2DElement( edgeFeature );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapper#getWrappedFeature()
   */
  public Feature getWrappedFeature( )
  {
    return getFeature();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEElement#getContainers()
   */
  public IFeatureWrapperCollection<IFE1D2DComplexElement> getContainers( )
  {
    return containers;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEElement#getEdges()
   */
  public IFeatureWrapperCollection<IFE1D2DEdge> getEdges( )
  {
    return edges;
  }

  public void setSurface(GM_Surface surface)
  {
    getFeature().setProperty( 
            Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENT_GEOM, 
            surface);
  }
  public void resetGeometry()
  {
    try
    {
      setSurface( (GM_Surface)recalculateElementGeometry() );
    }
    catch( GM_Exception e )
    {
      e.printStackTrace();
    }
  }
}
