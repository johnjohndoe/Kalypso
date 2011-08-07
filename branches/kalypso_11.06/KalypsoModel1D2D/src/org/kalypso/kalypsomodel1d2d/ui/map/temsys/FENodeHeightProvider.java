package org.kalypso.kalypsomodel1d2d.ui.map.temsys;

import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypsodeegree.model.geometry.GM_Point;

public class FENodeHeightProvider extends ColumnLabelProvider
{
  @Override
  public String getText( final Object element )
  {
    if( element instanceof IFE1D2DNode )
      return getElevationString( (IFE1D2DNode< ? >) element );

    return super.getText( element );
  }

  public static final String getElevationString( final IFE1D2DNode< ? > node )
  {
    final GM_Point point = node.getPoint();

    final double z = point.getZ();
    if( Double.isNaN( z ) )
      return "<Not Set>";

    return String.format( "%.2f", z );
  }
}