package org.kalypso.kalypsomodel1d2d.ui.map.temsys;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;

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
      return Messages.getString("FENodeNameProvider_0"); //$NON-NLS-1$

    return name;
  }
}