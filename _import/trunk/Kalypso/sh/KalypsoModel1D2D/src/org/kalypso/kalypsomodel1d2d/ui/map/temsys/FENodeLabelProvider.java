package org.kalypso.kalypsomodel1d2d.ui.map.temsys;

import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.swt.graphics.Image;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;
import org.kalypsodeegree.model.geometry.GM_Point;

public class FENodeLabelProvider implements ITableLabelProvider {

	public Image getColumnImage(Object element, int columnIndex) {
		return null;
	}

	public String getColumnText(Object element, int index) {
		if(element instanceof IFE1D2DNode)
        {
		  return getNodeColumnText((IFE1D2DNode)element, index);
        }
        else
        {
          return element==null?"null."+index:element.toString();
        }
      
	}

  private String  getNodeColumnText( IFE1D2DNode node, int index )
  {
    switch( index )
    {
      case 0:
      {
        String name = node.getName();
        if(name!=null)
        {
          name=name.trim();
          System.out.println("name: "+name );
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
      case 1:
      {
        GM_Point point = node.getPoint();
        if(point.getCoordinateDimension()<=2)
        {
          return String.valueOf( Double.NaN );
        }
        else
        {
          point.getZ();
        }
      }
      default:
      {
        return "ColText for"+node+" "+index;
      }
    }
    
  }

  public void addListener(ILabelProviderListener listener) {
		
	}

	public void dispose() {
		
	}

	public boolean isLabelProperty(Object element, String property) {
		return false;
	}

	public void removeListener(ILabelProviderListener listener) {
		
	}
}
