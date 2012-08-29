package org.kalypso.kalypsomodel1d2d.ui.map.temsys;

import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypsodeegree.model.geometry.GM_Point;

public class FENodeHeightProvider extends ColumnLabelProvider
{
  @Override
  public String getText( final Object element )
  {
    if( element instanceof IFE1D2DNode )
      return getElevationString( element );

    return super.getText( element );
  }

  static final String getElevationString( final Object node )
  {
    final double z = getElevation( node );
    if( Double.isNaN( z ) )
      return Messages.getString( "FENodeHeightProvider_0" ); //$NON-NLS-1$

    return formatElevation( z );
  }

  static double getElevation( final Object node )
  {
    if( !(node instanceof IFE1D2DNode) )
      return Double.NaN;

    final GM_Point point = ((IFE1D2DNode) node).getPoint();
    return point.getZ();
  }

  static String formatElevation( final double elevation )
  {
    return String.format( "%.2f", elevation ); //$NON-NLS-1$
  }
}