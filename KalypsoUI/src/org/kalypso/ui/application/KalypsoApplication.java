package org.kalypso.ui.application;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import org.eclipse.core.runtime.IPlatformRunnable;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.application.WorkbenchAdvisor;
import org.kalypso.eclipse.jface.dialogs.PasswordDialog;
import org.kalypso.services.user.UserServiceConstants;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * @author belger
 */
public class KalypsoApplication implements IPlatformRunnable
{
  private Logger m_logger = Logger.getLogger( KalypsoApplication.class.getName() );
  
  /**
   * @see org.eclipse.core.runtime.IPlatformRunnable#run(java.lang.Object)
   */
  public Object run( final Object args ) throws Exception
  {
    final String[] userRights = KalypsoGisPlugin.getDefault()
        .getUserRights();

    final StringBuffer userMsg = new StringBuffer( "Rights from server:" );
    if( userRights != null )
    {
      for( int i = 0; i < userRights.length; i++ )
        userMsg.append( "\n'" + userRights[i] + "'" );
    }
    m_logger.info( userMsg.toString() );

    final String[] rights = chooseRight( userRights );

    if( rights == null )
      return null;

    final StringBuffer chooseMsg = new StringBuffer( "Chosen rights:" );
    for( int i = 0; i < rights.length; i++ )
      chooseMsg.append( "\n'" + rights[i] + "'" );
    m_logger.info( chooseMsg.toString() );

    return startWorkbench( new KalypsoWorkbenchAdvisor( rights ) );
  }

  private String[] chooseRight( final String[] givenrights /*
                                                            * , final String
                                                            * username
                                                            */)
  {
    final List rights = new ArrayList();

    if( givenrights != null )
    {
      // leere Rechte rauschmeissen
      for( int i = 0; i < givenrights.length; i++ )
      {
        String right = givenrights[i];
        if( right != null )
        {
          right = right.trim();

          if( right.length() != 0
              && UserServiceConstants.isValidUserRight( right ) )
            rights.add( right.trim() );
        }
      }
    }

    String[] choosenRights = null;
    final Display display = new Display();
    final Shell shell = new Shell( display );

    final ImageDescriptor id = ImageProvider.IMAGE_KALYPSO_ICON;

    shell.setImage( id.createImage() );

    if( rights.size() == 0 )
    {
      while( true )
      {
        final PasswordDialog dialog = new PasswordDialog(
            shell,
            "Passworteingabe",
            "Es konnten keine Benutzerrechte vom Server ermittelt werden. Geben Sie das Administrator-Passwort ein, um im Administrator-Modus zu starten." );

        if( dialog.open() != Window.OK )
          break;

        if( "hochwasser".equals( dialog.getValue() ) )
        {
          choosenRights = new String[] { UserServiceConstants.RIGHT_ADMIN };
          break;
        }
      }
    }
    else
      /* if( rights.length == 1 ) */
      choosenRights = (String[]) rights.toArray( new String[] {} );

    shell.dispose();
    display.dispose();
    return choosenRights;
  }

  private Object startWorkbench( final WorkbenchAdvisor advisor )
  {
    final Display display = PlatformUI.createDisplay();
    final int returnCode = PlatformUI.createAndRunWorkbench( display, advisor );

    return returnCode == PlatformUI.RETURN_RESTART ? IPlatformRunnable.EXIT_RESTART
        : IPlatformRunnable.EXIT_OK;
  }

}