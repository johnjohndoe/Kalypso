package org.kalypso.ui.application;

import javax.xml.rpc.ServiceException;

import org.eclipse.core.runtime.IPlatformRunnable;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.application.WorkbenchAdvisor;
import org.eclipse.ui.internal.ide.IDEWorkbenchAdvisor;
import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.services.ProxyFactory;
import org.kalypso.services.proxy.IUserService;
import org.kalypso.services.user.common.IUserServiceConstants;
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
    final IUserService service = prepareService();

    final String username = System.getProperty( "user.name" );

    try
    {
      final String[] fakeRights = service.getFakeRights( username );
      System.out.println( "Fake rights:" );
      for( int i = 0; i < fakeRights.length; i++ )
        System.out.println( fakeRights[i] );
      System.out.println( "Fake rights END" );
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
    }

    try
    {
      final String[] rights = service.getRights( username );
      System.out.println( "Rights:" );
      for( int i = 0; i < rights.length; i++ )
        System.out.println( rights[i] );
      System.out.println( "Rights END" );
    }
    catch( final Throwable e1 )
    {
      e1.printStackTrace();
    }

    final String choosenRight = IUserServiceConstants.RIGHT_ADMIN;
    // TODO: choose from righs

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

    // dann BerechnungsWizard

    return null;

    //    for( int i = 0; i < projects.length; i++ )
    //    {
    //      System.out.println( projects[i].getName() );
    //
    //      // wir können direkt den PrognoseWizard laufen lassen!!!
    //      
    //      // JUHU!
    //      
    //      
    //    }
    //    
    //    boolean bPrognose = false;
    //    if( args instanceof String[] )
    //    {
    //      final String[] strgargs = (String[])args;
    //      for( int i = 0; i < strgargs.length; i++ )
    //      {
    //        if( "-prognose".equals( strgargs[i] ) )
    //        {
    //          bPrognose = true;
    //          break;
    //        }
    //      }
    //    }
    //    
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

//    final Display display = new Display();
//    final Shell shell = new Shell( display );

    // TODO!
//    final CalcWizard wizard = new CalcWizard( projects[0] );
//
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