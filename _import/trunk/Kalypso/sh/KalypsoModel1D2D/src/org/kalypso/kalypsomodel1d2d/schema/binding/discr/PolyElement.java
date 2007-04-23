package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.ops.ModelOps;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.Util;
import org.kalypso.kalypsosimulationmodel.core.FeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper;
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
 * TODO: also make interface for this element
 * 
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
@SuppressWarnings("hiding")
public class PolyElement extends AbstractFeatureBinder 
                              implements IPolyElement
{
  private final IFeatureWrapperCollection<IFE1D2DComplexElement> containers;

  private final IFeatureWrapperCollection<IFE1D2DEdge> edges;

  public PolyElement( final Feature featureToBind )
  {
    this(featureToBind, Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2D_2DElement );
  }
  
  public PolyElement( final Feature featureToBind, QName featureQName )
  {
    super( featureToBind, featureQName );
    //
    Object prop =null;
    try
    {
        prop=featureToBind.getProperty( 
            Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENT_CONTAINERS );
    }
    catch(Throwable th)
    {
      th.printStackTrace();
    }

    if( prop == null )
    {
      // create the property tha is still missing
      containers = 
          new FeatureWrapperCollection<IFE1D2DComplexElement>( 
                featureToBind, 
                Kalypso1D2DSchemaConstants.WB1D2D_F_COMPLEX_ELE_2D, 
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
    try
    {
      prop = featureToBind.getProperty( 
                  Kalypso1D2DSchemaConstants.WB1D2D_PROP_DIRECTEDEDGE );
    }
    catch (Throwable th) 
    {
      th.printStackTrace();
      prop=null;
    }

    if( prop == null )
    {
      // create the property that is still missing
      edges = new FeatureWrapperCollection<IFE1D2DEdge>( 
                          featureToBind, 
                          Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2D_2DElement, 
                          Kalypso1D2DSchemaConstants.WB1D2D_PROP_DIRECTEDEDGE, 
                          IFE1D2DEdge.class );
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
  public PolyElement(
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
  
  public PolyElement(
      Feature parentFeature,
      QName propQName,
      String gmlID)
  {
    this(
      org.kalypso.kalypsosimulationmodel.core.Util.createFeatureWithId( 
          Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2D_2DElement,
          parentFeature, 
          propQName, 
          gmlID ));
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
  public void setEdges( final IFE1D2DEdge[] edges )
  {
    final Feature feature = getFeature();
    final List edgeList = (List) feature.getProperty( Kalypso1D2DSchemaConstants.WB1D2D_PROP_DIRECTEDEDGE );

    edgeList.clear();
   Feature edgeInvFeature;
    
    for( final IFE1D2DEdge edge : edges )
    {
      edgeList.add( edge.getGmlID() );
    }
    ModelOps.sortElementEdges( this );
    
    getWrappedFeature().invalidEnvelope();
  }

  /**
   * Recalculates the geometry of this element. Used by the corresponding property function.
   */
  public GM_Object recalculateElementGeometry( ) throws GM_Exception
  {
    //TODO Patrice use ModelGeometryBuilder createSurface
      List<IFE1D2DNode> nodes=getNodes();
      final int SIZE=nodes.size();
      /* Positions from nodes */
      final GM_Position[] poses = new GM_Position[SIZE];

      if( SIZE <= 3 )
      {
        return null;
      }

      final CS_CoordinateSystem crs = 
        nodes.get(0).getPoint().getCoordinateSystem();

      for( int i = 0; i < poses.length; i++ )
      {
        final GM_Point point = nodes.get( i ).getPoint();
        poses[i] = point.getPosition();
      }
      
      return GeometryFactory.createGM_Surface( 
                      poses, 
                      new GM_Position[0][], 
                      new GM_SurfaceInterpolation_Impl( 
                                    GM_SurfaceInterpolation.PLANAR ), 
                      crs );
  }

  public static IPolyElement createPolyElement( 
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
    
    final Feature eleFeature = 
          parentFeature.getWorkspace().createFeature( 
                  parentFeature, parentElementProperty, polyType );
    
    return new PolyElement( eleFeature );
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

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement#getNodes()
   */
  public List<IFE1D2DNode> getNodes( )
  {
    
    List<IFE1D2DNode> nodes= new ArrayList<IFE1D2DNode>(edges.size()+1);
    IFE1D2DNode lasAddedNode=null;
//    IFE1D2DNode actualNode;
//    IFE1D2DNode nextNode;
    
    for(IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> edge:edges)
    {
      if(edge instanceof IEdgeInv)
      {
        IFE1D2DEdge invertedEdge=((IEdgeInv)edge).getInverted();
        List<IFE1D2DNode> edgeNodes=invertedEdge.getNodes();
        IFE1D2DNode<IFE1D2DEdge> node;
        for(int i=edgeNodes.size()-1;i>=0;i--)
        {
          node=edgeNodes.get( i );
          if(node!=null)
          {
            if(!node.equals( lasAddedNode ))
            {
              nodes.add( node );
              lasAddedNode=node;
            }
          }
        }
        
      }
      else
      {
        for(IFE1D2DNode<IFE1D2DEdge> node : edge.getNodes())
        {
          if(node!=null)
          {
            if(!node.equals( lasAddedNode ))
            {
              nodes.add( node );
              lasAddedNode=node;
            }
          }
        }
      }
    }
    if(lasAddedNode!=null && nodes.size()>0)
    {
      if(!lasAddedNode.equals( nodes.get( 0 ) ))
      {
        nodes.add( nodes.get( 0 ) );
      }
    }
    return nodes;
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement#addEdge(java.lang.String)
   */
  public void addEdge( String edgeID )
  {
    if(edgeID==null)
    {
      throw new IllegalArgumentException("edge ID must not be null");
    }
    FeatureList edgeFeatureList=edges.getWrappedList();
    if(edgeFeatureList.contains( edgeID ))
    {
      return;
    }
    edgeFeatureList.add( edgeID );
    
    getWrappedFeature().invalidEnvelope();
  }
  
  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    StringBuffer buf= new StringBuffer(128);
    buf.append( "FE1D2D_Element[" );
    for(IFeatureWrapper featureWrapper:edges)
    {
      buf.append( featureWrapper );
    }
    buf.append( ']' );
    return buf.toString();
  }

  public GM_Surface getSurface( )
  {
    return (GM_Surface) getFeature().getProperty( 
        Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENT_GEOM );
  }
}
