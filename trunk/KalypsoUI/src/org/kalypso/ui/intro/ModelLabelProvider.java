package org.kalypso.ui.intro;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.kalypso.model.xml.ModellistType;
import org.kalypso.model.xml.ModellistType.ModelType;
import org.kalypso.ui.ImageProvider;

/**
 * @author Belger
 */
public class ModelLabelProvider extends LabelProvider
{
  private Map m_imageMap = new HashMap();

  /**
   * @see org.eclipse.jface.viewers.LabelProvider#dispose()
   */
  public void dispose()
  {
    for( Iterator iter = m_imageMap.values().iterator(); iter.hasNext(); )
      ((Image)iter.next()).dispose();
  }
  
  public Image getImage( final Object element )
  {
    final ModellistType.ModelType model = (ModelType)element;
   
    // images hashen
    Image image = (Image)m_imageMap.get( model );
    if( image == null )
    {
      image = ImageProvider.id( model.getIcon() ).createImage();
      m_imageMap.put( model, image );
    }
    
    return image;
  }

  public String getText( final Object element )
  {
    final ModellistType.ModelType model = (ModelType)element;
    
    return model.getLabel();
  }
}