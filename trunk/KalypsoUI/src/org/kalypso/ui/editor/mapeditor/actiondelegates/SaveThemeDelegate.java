package org.kalypso.ui.editor.mapeditor.actiondelegates;

import java.lang.reflect.InvocationTargetException;

import org.deegree.model.feature.event.ModellEventListener;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.progress.IProgressService;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ui.editor.mapeditor.GisMapEditor;

/**
 * @author belger
 */
public class SaveThemeDelegate extends AbstractThemeDelegate implements ModellEventListener
{
  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    final GisMapEditor editor = getEditor();
    if( editor == null )
      return;

    final Shell shell = editor.getSite().getShell();
    if( !MessageDialog.openConfirm( shell, "Themen speichern",
        "Sollen die Daten des aktiven Themas gespeichert werden?" ) )
      return;

    final IMapModell mapModell = editor.getMapPanel().getMapModell();
    if( mapModell != null )
    {
      final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
      if( activeTheme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme)activeTheme;

        final IProgressService progressService = PlatformUI.getWorkbench().getProgressService();

        final WorkspaceModifyOperation op = new WorkspaceModifyOperation( theme.getSchedulingRule() )
        {
          protected void execute( final IProgressMonitor monitor ) throws CoreException
          {
            editor.saveTheme( theme, monitor );
          }
        };

        try
        {
          progressService.busyCursorWhile( op );
        }
        catch( final InvocationTargetException e )
        {
          e.printStackTrace();

          final CoreException ce = (CoreException)e.getTargetException();
          ErrorDialog.openError( shell, "Fehler", "Fehler beim Speichern", ce.getStatus() );
        }
        catch( final InterruptedException e )
        {
          e.printStackTrace();
        }

      }
    }

    refreshAction();
  }

  protected void refreshAction( )
  {
    boolean bEnabled = false;

    final GisMapEditor editor = getEditor();
    if( editor != null )
    {
      final IMapModell mapModell = editor.getMapPanel().getMapModell();
      if( mapModell != null )
      {
        final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
        if( activeTheme != null && activeTheme instanceof IKalypsoFeatureTheme )
        {
          final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme)activeTheme;
          final CommandableWorkspace workspace = theme.getWorkspace();
          if( workspace != null )
            bEnabled = workspace.isDirty();
        }
      }
    }

    if( getAction() != null )
      getAction().setEnabled( bEnabled );
  }
}