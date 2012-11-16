/**
 *
 */
package org.kalypso.kalypsomodel1d2d.schema.functions;

import java.util.Map;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Creates the edge geometry from the two referenced nodes.
 * 
 * @author Gernot Belger
 */
public class FE1D2DEdgeTypeGeometryFunction extends FeaturePropertyFunction
{
  @Override
  public void init( final Map<String, String> properties )
  {
    // nothing to do
  }

  @Override
  public Object getValue( final Feature feature, final IPropertyType pt, final Object currentValue )
  {
    if( !GeometryCalcControl.doCalcEdge )
      return null;

    final IFE1D2DNode[] nodes = ((IFE1D2DEdge)feature).getNodes();
    if( nodes.length != 2 )
      return null;

    final IFE1D2DNode node0 = nodes[0];
    final IFE1D2DNode node1 = nodes[1];

    if( node0 == null || node1 == null )
      return null;

    final GM_Point point0 = node0.getPoint();
    final GM_Position positions[] = new GM_Position[] { point0.getPosition(), node1.getPoint().getPosition() };
    try
    {
      return GeometryFactory.createGM_Curve( positions, point0.getCoordinateSystem() );
    }
    catch( final GM_Exception e )
    {
      throw new IllegalStateException( e );
    }
  }

  @Override
  public Object setValue( final Feature feature, final IPropertyType pt, final Object valueToSet )
  {
    return valueToSet;
  }
}