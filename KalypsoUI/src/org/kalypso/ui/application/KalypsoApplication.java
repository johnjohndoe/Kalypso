package org.kalypso.ui.application;

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
      System.out.println( "'" + rights[i] + "'" );
    
    return startWorkbench( new KalypsoWorkbenchAdvisor( rights ) );
  }

  private String[] chooseRight( final String[] rights /*, final String username */)
  {
    String[] choosenRights = null;
    final Display display = new Display();
    final Shell shell = new Shell( display );
    
    final ImageDescriptor id = ImageProvider.IMAGE_KALYPSO_ICON;
    
    shell.setImage( id.createImage() );
    
    if( rights == null || rights.length == 0 )
    {
      while( true )
      {
        final PasswordDialog dialog = new PasswordDialog(
            shell,
            "Passworteingabe",
            "Es konnten keine Benutzerrechte vom Server ermittelt erden. Geben Sie das Administrator-Passwort ein, um im Administrator-Modus zu starten." );

        if( dialog.open() != Window.OK )
          break;

        if( "arglgargl".equals( dialog.getValue() ) )
        {
          choosenRights = new String[] { IUserServiceConstants.RIGHT_ADMIN };
          break;
        }
      }
    }
//    else if( rights.length == 0 )
//    {
//      MessageDialog.openInformation( shell, "Benutzerrechte",
//          "Es konnten keine Benutzerrechte für Benutzer '" + username
//              + "' ermittelt werden. Bitte wenden Sie sich an den System-Administrator" );
//    }
    else /* if( rights.length == 1 ) */
    {
//      choosenRights = rights[0];
//    }
//    else
//    {
//      // auswahldialog
//      final ListDialog dialog = new ListDialog( shell );
//      dialog.setTitle( "Kalypso" );
//      dialog
//          .setMessage( "Bitte wählen Sie den Modus, in welchem Sie Kalypso starten möchten." );
//      dialog.setContentProvider( new ArrayContentProvider() );
//      dialog.setLabelProvider( new LabelProvider() );
//      dialog.setInput( rights );
//      dialog.setInitialSelections( new Object[]
//      { rights[0] } );
//
//      if( dialog.open() == Window.OK )
//        choosenRights = (String)dialog.getResult()[0];
      choosenRights = rights;
    }

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