/**
 * 
 */
package org.kalypso.kalypsomodel1d2d.schema.functions;

import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_LineString;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;

/**
 * Creates the edge geometry from the two referenced nodes.
 * 
 * @author Gernot Belger
 */
@SuppressWarnings("unchecked")
public class FE1D2DEdgeTypeGeometryFunction extends FeaturePropertyFunction
{

  /**
   * @see org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction#init(java.util.Map)
   */
  @Override
  public void init( final Map<String, String> properties )
  {
    // nothing to do
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  @Override
  public Object getValue( final Feature feature, final IPropertyType pt, final Object currentValue )
  {
    if( !GeometryCalcControl.doCalcEdge )
      return null;

    final QName featureQName = feature.getFeatureType().getQName();

    if( IFE1D2DEdge.QNAME.equals( featureQName ) )
    {
      final FE1D2DEdge edge = new FE1D2DEdge( feature );
      try
      {
        return edge.recalculateEgdeGeometry();
      }
      catch( final GM_Exception e )
      {
        e.printStackTrace();
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
        return null;
      }
    }
    else
    {
      return null;
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#setValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */

  @Override
  public Object setValue( final Feature feature, final IPropertyType pt, final Object valueToSet )
  {
    // System.out.println("New Edge geometry="+valueToSet.getClass());
    if( true )
    {
      return valueToSet;
    }
    if( valueToSet instanceof GM_Curve )
    {

      try
      {
        final GM_LineString lineString = ((GM_Curve) valueToSet).getAsLineString();
        if( lineString.getNumberOfPoints() == 2 )
        {
          final IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> edge = (IFE1D2DEdge) feature.getAdapter( IFE1D2DEdge.class );
          if( edge != null )
          {
            final IFeatureWrapperCollection<IFE1D2DNode> nodes = edge.getNodes();
            if( nodes.size() == 2 )
            {
              System.out.println( "Node set:" + lineString.getStartPoint().distance( nodes.get( 0 ).getPoint() ) ); //$NON-NLS-1$
              nodes.get( 0 ).setPoint( lineString.getStartPoint() );
              nodes.get( 1 ).setPoint( lineString.getEndPoint() );

              return valueToSet;
            }

          }

        }
        System.out.println( "Not sets:" + lineString ); //$NON-NLS-1$
        return null;
      }
      catch( final GM_Exception e )
      {
        e.printStackTrace();
        return null;
      }
    }

    // TODO: move the corresponding node
    return null;
  }

}
