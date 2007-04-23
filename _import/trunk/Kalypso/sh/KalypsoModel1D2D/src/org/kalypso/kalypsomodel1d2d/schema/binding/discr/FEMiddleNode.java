package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import javax.xml.namespace.QName;

import org.kalypsodeegree.model.feature.Feature;

/**
 * The default implementation of {@link IFEMiddleNode} based on 
 * {@link FE1D2DNode} to bind wb1d2d:MiddleNode elements
 * 
 * @author Patrice Congo
 */
public class FEMiddleNode 
                extends FE1D2DNode 
                implements IFEMiddleNode<IFE1D2DEdge>
{
  public FEMiddleNode(Feature middleNodeFeature )
  {
    super(middleNodeFeature);
  }

  public FEMiddleNode( Feature parentFeature, QName propQName, String gmlID )
  {
    super(parentFeature, propQName, gmlID);
  }

  public FEMiddleNode( Feature parentFeature, QName propQName ) throws IllegalArgumentException
  {
    super(parentFeature, propQName);
  }
  
  
}
