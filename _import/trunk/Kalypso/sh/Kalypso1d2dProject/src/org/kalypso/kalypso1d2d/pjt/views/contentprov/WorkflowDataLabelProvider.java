/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt.views.contentprov;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.scenarios.Scenario;

/**
 * @author congo
 */
public class WorkflowDataLabelProvider extends LabelProvider
{
  private static final Logger logger = Logger.getLogger( WorkflowDataLabelProvider.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.kalypso1d2d.pjt/debug" ) );

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  /**
   * @see org.eclipse.jface.viewers.ILabelProvider#getImage(java.lang.Object)
   */
  @Override
  public Image getImage( final Object element )
  {
    try
    {
      return Kalypso1d2dProjectPlugin.getImageDescriptor( Kalypso1d2dProjectPlugin.KEY_ICON_SIM_MODEL );
    }
    catch( Throwable th )
    {
      logger.log( Level.SEVERE, "Error getting image", th );
      return null;
    }
  }

  /**
   * @see org.eclipse.jface.viewers.ILabelProvider#getText(java.lang.Object)
   */
  @Override
  public String getText( final Object element )
  {
    if( element instanceof Scenario )
    {
      return ((Scenario) element).getName();
    }
    return "";
  }
}
