package org.kalypso.ui.launcher;

import java.util.logging.Logger;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.debug.ui.ILaunchShortcut;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IEditorPart;
import org.kalypso.ui.nature.ModelNature;

/**
 * @author belger
 */
public class CalcCaseLaunchShortcut implements ILaunchShortcut
{
  private static final Logger LOGGER = Logger.getLogger( CalcCaseLaunchShortcut.class.getName() );

  /**
   * @see org.eclipse.debug.ui.ILaunchShortcut#launch(org.eclipse.jface.viewers.ISelection,
   *      java.lang.String)
   */
  public void launch( final ISelection selection, final String mode )
  {
    // test, ob die selection ein rechenfall ist
    if( selection.isEmpty() || !( selection instanceof IStructuredSelection )
        || ( (IStructuredSelection)selection ).size() > 1 )
    {
      LOGGER.info( "Could not launch calculation: not exactly 1 element selected" );
      return;
    }

    final Object element = ( (IStructuredSelection)selection ).getFirstElement();
    if( element == null || !( element instanceof IFolder ) )
    {
      LOGGER.info( "Could not launch calculation: selected element must be a folder" );
      return;
    }

    final IFolder folder = (IFolder)element;
    if( !ModelNature.isCalcCalseFolder( folder ) )
    {
      LOGGER.info( "Could not launch calculation: selected element must be a calculation case" );
      return;
    }

    // launchen!
    try
    {
      final ILaunchManager lm = DebugPlugin.getDefault().getLaunchManager();
      final ILaunchConfigurationType configType = lm
          .getLaunchConfigurationType( "org.kalypso.ui.launch.CalcCaseType" );

      final ILaunchConfigurationWorkingCopy wc = configType.newInstance( null, lm
          .generateUniqueLaunchConfigurationNameFrom( folder.getName() ) );
//      wc.setAttribute( IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME, mainType );
//      wc.setAttribute( IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, project.getElementName() );
//      wc.setAttribute( JUnitBaseLaunchConfiguration.ATTR_KEEPRUNNING, false );
//      wc.setAttribute( JUnitBaseLaunchConfiguration.LAUNCH_CONTAINER_ATTR, container );

      final ILaunchConfiguration config = wc.doSave();

      DebugUITools.launch( config, mode );
    }
    catch( final CoreException ce )
    {
      ce.printStackTrace();
    }
  }

  /**
   * @see org.eclipse.debug.ui.ILaunchShortcut#launch(org.eclipse.ui.IEditorPart,
   *      java.lang.String)
   */
  public void launch( final IEditorPart editor, final String mode )
  {
    throw new UnsupportedOperationException();
  }

}