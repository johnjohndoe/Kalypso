package org.kalypso.ui.editor.gmleditor.ui;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.editor.gmleditor.util.model.FeatureElement;
import org.kalypso.ui.editor.gmleditor.util.model.LinkedFeatureElement;
import org.kalypso.ui.editor.gmleditor.util.model.Model;
import org.kalypso.ui.editor.gmleditor.util.model.PropertyElement;

public class GMLEditorLabelProvider extends LabelProvider
{
  private Map imageCache = new HashMap( 11 );

  /*
   * @see ILabelProvider#getImage(Object)
   */
  public Image getImage( Object element )
  {
    ImageDescriptor descriptor = null;
    if( element instanceof FeatureElement )
    {
      descriptor = ImageProvider.IMAGE_FEATURE;
    }
    else if( element instanceof PropertyElement )
    {
      descriptor = ImageProvider.IMAGE_FEATURE_RELATION_COMPOSITION;
    }
    else if( element instanceof LinkedFeatureElement )
    {
      descriptor = ImageProvider.IMAGE_FEATURE_LINKED;
    }
    else
    {
      throw unknownElement( element );
    }

    //obtain the cached image corresponding to the descriptor
    Image image = (Image)imageCache.get( descriptor );
    if( image == null )
    {
      image = descriptor.createImage();
      imageCache.put( descriptor, image );
    }
    return image;
  }

  /*
   * @see ILabelProvider#getText(Object)
   */
  public String getText( Object element )
  {
    if( element instanceof Model )
    {
      return ( (Model)element ).getName();
    }
    throw unknownElement( element );
  }

  public void dispose()
  {
    for( Iterator i = imageCache.values().iterator(); i.hasNext(); )
    {
      ( (Image)i.next() ).dispose();
    }
    imageCache.clear();
  }

  protected RuntimeException unknownElement( Object element )
  {
    return new RuntimeException( "Unknown type of element in tree of type " + element.getClass().getName() );
  }

}