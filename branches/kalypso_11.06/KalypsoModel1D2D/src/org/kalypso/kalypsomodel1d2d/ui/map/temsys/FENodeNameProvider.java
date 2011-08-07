package org.kalypso.kalypsomodel1d2d.ui.map.temsys;

import org.apache.commons.lang.StringUtils;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;

public class FENodeNameProvider extends ColumnLabelProvider
{
  @Override
  public String getText( final Object element )
  {
    if( element instanceof IFE1D2DNode )
      return getNameOrID( (IFE1D2DNode) element );

    return super.getText( element );
  }

  public static final String getNameOrID( final IFE1D2DNode node )
  {
    final String name = node.getName();
    if( StringUtils.isBlank( name ) )
      return "<Not Set>";

    return name;
  }
}