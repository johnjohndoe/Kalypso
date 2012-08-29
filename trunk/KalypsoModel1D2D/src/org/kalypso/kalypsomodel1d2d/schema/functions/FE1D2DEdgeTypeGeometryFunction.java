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
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;

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

    final QName featureQName = feature.getFeatureType().getQName();

    if( IFE1D2DEdge.QNAME.equals( featureQName ) )
    {
      final FE1D2DEdge edge = (FE1D2DEdge) feature;
      try
      {
        return edge.recalculateElementGeometry();
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

  @Override
  public Object setValue( final Feature feature, final IPropertyType pt, final Object valueToSet )
  {
    return valueToSet;
  }
}