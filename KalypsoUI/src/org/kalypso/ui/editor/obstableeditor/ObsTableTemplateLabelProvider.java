package org.kalypso.ui.editor.obstableeditor;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.kalypso.ogc.sensor.tableview.ITableViewTheme;
import org.kalypso.ui.ImageProvider;

/**
 * ObsDiagTemplateLabelProvider
 * 
 * @author schlienger
 */
public class ObsTableTemplateLabelProvider extends LabelProvider
{
  // lazy loading
  private Image m_themeImage = null;
  // lazy loading
  private Image m_image = null;

  /**
   * @see org.eclipse.jface.viewers.LabelProvider#dispose()
   */
  public void dispose( )
  {
    if( m_themeImage != null )
      m_themeImage.dispose();
    
    if( m_image != null )
      m_image.dispose();

    super.dispose();
  }
  
  /**
   * @see org.eclipse.jface.viewers.LabelProvider#getImage(java.lang.Object)
   */
  public Image getImage( Object element )
  {
    if( element instanceof ITableViewTheme)
      return getThemeImage();
    
    return getDefaultImage();
  }
  
  private Image getDefaultImage( )
  {
    if( m_image == null )
      m_image = ImageProvider.IMAGE_UTIL_POINT_GREEN.createImage();
    
    return m_image;
  }

  private Image getThemeImage()
  {
    if( m_themeImage == null )
      m_themeImage = ImageProvider.IMAGE_ZML_FILE.createImage();
    
    return m_themeImage;
  }
}
