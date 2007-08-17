package org.kalypso.kalypso1d2d.pjt.views.contentprov;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.kalypso.afgui.scenarios.Scenario;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;

public class ScenarioLabelProvider extends LabelProvider
{
  private static final Logger logger = Logger.getLogger( ScenarioLabelProvider.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.kalypso1d2d.pjt/debug" ) );  //$NON-NLS-1$

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
    if( element instanceof Scenario )
    {
      try
      {
        return Kalypso1d2dProjectPlugin.getImageDescriptor( Kalypso1d2dProjectPlugin.KEY_ICON_SIM_MODEL );
      }
      catch( Throwable th )
      {
        logger.log( Level.SEVERE, Messages.getString("ScenarioLabelProvider.1"), th ); //$NON-NLS-1$
        return null;
      }
    }
    return null;
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
    return "";  //$NON-NLS-1$
  }
}
