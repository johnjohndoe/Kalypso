package org.kalypso.ui.application;

import java.net.URL;

import javax.xml.rpc.ServiceException;

import org.eclipse.core.runtime.IPlatformRunnable;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.application.WorkbenchAdvisor;
import org.eclipse.ui.dialogs.ListDialog;
import org.eclipse.ui.internal.ide.IDEWorkbenchAdvisor;
import org.kalypso.eclipse.jface.dialogs.PasswordDialog;
import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.services.ProxyFactory;
import org.kalypso.services.proxy.IUserService;
import org.kalypso.services.user.common.IUserServiceConstants;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.view.prognose.PrognosePanel;

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
      rights = service.getRights( username );
    }
    catch( final Throwable e1 )
    {
      e1.printStackTrace();
    }

    final String choosenRight = chooseRight( rights, username );

    // start application
    if( IUserServiceConstants.RIGHT_PROGNOSE.equals( choosenRight ) )
      return startPrognose();
    else if( IUserServiceConstants.RIGHT_EXPERT.equals( choosenRight ) )
      return startWorkbench( new KalypsoWorkbenchAdvisor() );
    if( IUserServiceConstants.RIGHT_ADMIN.equals( choosenRight ) )
      return startWorkbench( new IDEWorkbenchAdvisor()
      {
        //
        } );

    return null;
  }

  private String chooseRight( final String[] rights, final String username )
  {
    String choosenRight = null;
    final Display display = new Display();
    final Shell shell = new Shell( display );
    if( rights == null )
    {
      while( true )
      {
        final PasswordDialog dialog = new PasswordDialog(
            shell,
            "Passworteingabe",
            "Es konnten keine Benutzerrechte vom Server ermittelt erden. Geben Sie das Administrator-Passwort ein, um im Administrator-Modus zu starten." );

        if( dialog.open() != Window.OK )
          break;

        if( !"arglgargl".equals( dialog.getValue() ) )
        {
          choosenRight = IUserServiceConstants.RIGHT_ADMIN;
          break;
        }
      }
    }
    else if( rights.length == 0 )
    {
      MessageDialog.openInformation( shell, "Benutzerrechte",
          "Es konnten keine Benutzerrechte für Benutzer '" + username
              + "' ermittelt werden. Bitte wenden Sie sich an den System-Administrator" );
    }
    else if( rights.length == 1 )
    {
      choosenRight = rights[0];
    }
    else
    {
      // auswahldialog
      final ListDialog dialog = new ListDialog( shell );
      dialog.setTitle( "Kalypso" );
      dialog
          .setMessage( "Bitte wählen Sie den Modus, in welchem Sie Kalypso starten möchten." );
      dialog.setContentProvider( new ArrayContentProvider() );
      dialog.setLabelProvider( new LabelProvider() );
      dialog.setInput( rights );
      dialog.setInitialSelections( new Object[]
      { rights[0] } );

      if( dialog.open() == Window.OK )
        choosenRight = (String)dialog.getResult()[0];
    }

    shell.dispose();
    display.dispose();
    return choosenRight;
  }

  private Object startWorkbench( final WorkbenchAdvisor advisor )
  {
    final Display display = PlatformUI.createDisplay();
    final int returnCode = PlatformUI.createAndRunWorkbench( display, advisor );

    return returnCode == PlatformUI.RETURN_RESTART ? IPlatformRunnable.EXIT_RESTART
        : IPlatformRunnable.EXIT_OK;
  }

  private Object startPrognose()
  {
//    final IProject[] projects = ResourcesPlugin.getWorkspace().getRoot().getProjects();

    final Display display = new Display();
    final Shell shell = new Shell( display );

    // first, choose project
    final URL location = KalypsoGisPlugin.getDefault().getModellistLocation();
    if( location == null )
    {
      MessageDialog.openError( shell, "Hochwasser Vorhersage", "Die Liste der Vorhersage Modelle konnten nicht geladen werden. Bitte wenden Sie sich an den System Administrator." );
      return null;
    }
    
    final PrognosePanel prognosePanel = new PrognosePanel( location );
    
    final Dialog dialog = new Dialog( shell ) 
    {
      /**
       * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
       */
      protected Control createDialogArea( final Composite parent )
      {
        return prognosePanel.createControl( parent );
      }
    };

    dialog.open();
    
    // now start calculation wizard
//    final CalcWizard wizard = new CalcWizard( projects[0] );
//    final WizardDialog dialog = new WizardDialog( shell, wizard );
//    dialog.open();

    return null;
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