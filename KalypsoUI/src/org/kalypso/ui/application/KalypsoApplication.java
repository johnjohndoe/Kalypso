package org.kalypso.ui.application;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPlatformRunnable;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.ui.calcwizard.CalcWizard;

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
    // TODO: zuerst Modellauswahl,
    
    // dann BerechnungsWizard
    
    final IProject[] projects = ResourcesPlugin.getWorkspace().getRoot().getProjects();


    final Display display = new Display( );
    final Shell shell = new Shell( display );
    
    final CalcWizard wizard = new CalcWizard( projects[0] );
    
    final WizardDialog dialog = new WizardDialog( shell, wizard );
    dialog.open();

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
//    final WorkbenchAdvisor advisor = bPrognose ? (WorkbenchAdvisor)new PrognoseWorkbenchAdvisor() : new KalypsoWorkbenchAdvisor();
//    final Display display = PlatformUI.createDisplay();
//
//    final int returnCode = PlatformUI.createAndRunWorkbench( display, advisor );
//
//    return returnCode == PlatformUI.RETURN_RESTART ? IPlatformRunnable.EXIT_RESTART
//        : IPlatformRunnable.EXIT_OK;
  }
}