package org.kalypso.kalypsomodel1d2d.schema.functions;

import java.util.Map;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_AbstractSurfacePatch;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_PolygonPatch;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Gernot Belger
 */
public class FE1D2D_2DElementTypeGeometry extends FeaturePropertyFunction
{
  @Override
  public void init( final Map<String, String> properties )
  {
    // nothing to do
  }

  @Override
  public Object getValue( final Feature feature, final IPropertyType pt, final Object currentValue )
  {
    if( !GeometryCalcControl.doCalcElement )
      return null;

    final IFE1D2DElement element = (IFE1D2DElement)feature.getAdapter( IFE1D2DElement.class );
    final IFE1D2DNode[] nodes = element.getNodes();
    final int nodeCount = nodes.length;

    if( nodeCount < 4 )
      return null;
    
    /* Positions from nodes */
    final GM_Position[] pos = new GM_Position[nodeCount];

    final String crs = nodes[0].getPoint().getCoordinateSystem();
    for( int i = 0; i < nodeCount; i++ )
    {
      final GM_Point point = nodes[i].getPoint();
      pos[i] = point.getPosition();
    }

    try
    {
      final GM_PolygonPatch patch;
      if( nodeCount == 4 )
        patch = GeometryFactory.createGM_Triangle( pos[0], pos[1], pos[2], crs );
      else if( nodeCount == 5 )
        patch = GeometryFactory.createGM_Rectangle( pos[0], pos[1], pos[2], pos[3], crs );
      else //if( nodeCount > 5 )
        patch = GeometryFactory.createGM_PolygonPatch( pos, null, crs );
      return GeometryFactory.createGM_Surface( patch );
    }
    catch( final GM_Exception e )
    {
      throw new IllegalArgumentException( e );
    }
  }

  @Override
  public Object setValue( final Feature feature, final IPropertyType pt, final Object valueToSet )
  {
    return valueToSet;
  }
}