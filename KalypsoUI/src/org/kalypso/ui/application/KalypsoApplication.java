package org.kalypso.ui.application;

import java.util.ArrayList;
import java.util.List;

import javax.xml.rpc.ServiceException;

import org.eclipse.core.runtime.IPlatformRunnable;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.application.WorkbenchAdvisor;
import org.kalypso.eclipse.jface.dialogs.PasswordDialog;
import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.services.ProxyFactory;
import org.kalypso.services.proxy.IUserService;
import org.kalypso.services.user.common.IUserServiceConstants;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * @author belger
 */
public class KalypsoApplication implements IPlatformRunnable
{
  /**
   * @see org.eclipse.core.runtime.IPlatformRunnable#run(java.lang.Object)
   */
  public Object run( final Object args ) throws Exception
  {
    String[] rights = new String[] {};
    final String username = System.getProperty( "user.name" ).toLowerCase();
    try
    {
      final IUserService service = prepareService();
      rights = service == null ? null : service.getRights( username ); // todo avoid nullpointerexception
    }
    catch( final Throwable e1 )
    {
      e1.printStackTrace();
    }
    
    rights = chooseRight( rights /*, username */);
    
    if( rights == null )
      return null;
    
    for( int i = 0; i < rights.length; i++ )
      System.out.println( "Rights dump: '" + rights[i] + "'" );
    
//    rights = new String[]{ IUserServiceConstants.RIGHT_ADMIN };
    
    return startWorkbench( new KalypsoWorkbenchAdvisor( rights ) );
  }

  private String[] chooseRight( final String[] givenrights /*, final String username */)
  {
    // leere Rechte rauschmeissen
    final List rights = new ArrayList( givenrights.length );
    for( int i = 0; i < givenrights.length; i++ )
    {
      final String right = givenrights[i];
      if( right != null && right.trim().length() != 0 )
        rights.add( right.trim() );
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
            "Es konnten keine Benutzerrechte vom Server ermittelt erden. Geben Sie das Administrator-Passwort ein, um im Administrator-Modus zu starten." );

        if( dialog.open() != Window.OK )
          break;

        if( "hochwasser".equals( dialog.getValue() ) )
        {
          choosenRights = new String[] { IUserServiceConstants.RIGHT_ADMIN };
          break;
        }
      }
    }
    else /* if( rights.length == 1 ) */
      choosenRights = (String[])rights.toArray( new String[] {} );

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

  private IUserService prepareService()
  {
    try
    {
      final ProxyFactory serviceProxyFactory = KalypsoGisPlugin.getDefault()
          .getServiceProxyFactory();
      return (IUserService)serviceProxyFactory.getProxy( "Kalypso_UserService", ClassUtilities
          .getOnlyClassName( IUserService.class ) );
    }
    catch( final ServiceException e )
    {
      e.printStackTrace();

      return null;
    }
  }

}