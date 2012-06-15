package org.kalypso.kalypsomodel1d2d.ui.map.temsys;

import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.swt.graphics.Image;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypsodeegree.model.geometry.GM_Point;

public class FENodeLabelProvider implements ITableLabelProvider {

	@Override
  public Image getColumnImage(Object element, int columnIndex) {
		return null;
	}

	@Override
  public String getColumnText(Object element, int index) {
		if(element instanceof IFE1D2DNode)
        {
		  return getNodeColumnText((IFE1D2DNode)element, index);
        }
        else
        {
          return element==null?"null."+index:element.toString(); //$NON-NLS-1$
        }
      
	}

  private String  getNodeColumnText( IFE1D2DNode node, int index )
  {
    switch( index )
    {
      case 0:
      {
        return getNameOrID( node );
      }
      case 1:
      {
        return getElevationString( node );
      }
      default:
      {
        return "ColText for"+node+" "+index; //$NON-NLS-1$ //$NON-NLS-2$
      }
    }
    
  }

  @Override
  public void addListener(ILabelProviderListener listener) {
		
	}

	@Override
  public void dispose() {
		
	}

	@Override
  public boolean isLabelProperty(Object element, String property) {
		return false;
	}

	@Override
  public void removeListener(ILabelProviderListener listener) {
		
	}
    
    
    
    public static final String getNameOrID(IFE1D2DNode node)
    {
      String name = node.getName();
      if(name!=null)
      {
        name=name.trim();
        if(name.length()==0)
        {
          name=node.getGmlID();
        }
        return name;
      }
      else
      {
        return node.getGmlID();
      }
    }
   
    
    public static final String getElevationString(IFE1D2DNode node)
    {
      GM_Point point = node.getPoint();
      if(point.getCoordinateDimension()<=2)
      {
        return String.valueOf( Double.NaN );
      }
      else
      {
        return String.valueOf( point.getZ() );
      }
    }
}
