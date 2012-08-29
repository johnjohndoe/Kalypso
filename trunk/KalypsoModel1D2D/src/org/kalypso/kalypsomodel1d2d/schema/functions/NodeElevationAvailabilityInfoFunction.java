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
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;

/**
 * Function Property that answers whether a 1D2D node has an elevation or not.
 *
 * @author Patrice Congo
 */
public class NodeElevationAvailabilityInfoFunction extends FeaturePropertyFunction
{
  /**
   * @see org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction#init(java.util.Map)
   */
  @Override
  public void init( final Map<String, String> properties )
  {
    // yes empty
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  @Override
  public Object getValue( final Feature feature, final IPropertyType pt, final Object currentValue )
  {
    final QName featureQName = feature.getFeatureType().getQName();

    if( Kalypso1D2DSchemaConstants.WB1D2D_F_NODE.equals( featureQName ) )
    {
      // TODO: Sometimes here is a NPE when first displaying the map. Race condition?
      final IFE1D2DNode node = (IFE1D2DNode) feature.getAdapter( IFE1D2DNode.class );
      try
      {
        final GM_Point point = node == null ? null : node.getPoint();
        if( point == null )
          return Boolean.FALSE;

        if( point.getCoordinateDimension() == 3 )
          return !Double.isNaN( point.getZ() );

        return Boolean.FALSE;
      }
      catch( final Throwable th )
      {
        th.printStackTrace();
        final IStatus status = StatusUtilities.statusFromThrowable( th );
        KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
        return null;
      }
    }
    else
    {
      System.out.println( "Cannot get value for:" + "\n\tfeature=" + feature + "\n\tvalue=" + pt.getQName() + "\n\tcurrentValue=" + currentValue ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
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
    return null;
  }

}
