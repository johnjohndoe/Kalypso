package org.kalypso.ui.action;

import java.util.Iterator;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.ListSelectionDialog;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.kalypso.java.util.Arrays;
import org.kalypso.ui.nature.CalcCaseCollector;

/**
 * Helper Klasse für die Calc-Case Actions
 * 
 * @author belger
 */
public class CalcCaseHelper
{
  private CalcCaseHelper()
  {
  // wird nicht instantiiert
  }

  /**
   * Lässt den Benutzer aus einer Liste von Rechenfällen auswählen Es werden
   * alle Rechenfälle angezeigt, welche sich in oder unterhalb der angegebenen
   * Selection von Resourcen befinden.
   * 
   * @param message
   * @return null bei Abbruch
   */
  public static IFolder[] chooseCalcCases( final Shell shell, final ISelection selection,
      final String title, final String message )
  {
    // rausfinden, ob selection ok ist
    if( !( selection instanceof IStructuredSelection ) )
      return null;

    final CalcCaseCollector visitor = new CalcCaseCollector();
    try
    {
      final IStructuredSelection structsel = (IStructuredSelection)selection;
      for( final Iterator sIt = structsel.iterator(); sIt.hasNext(); )
      {
        final Object sel = sIt.next();
        if( sel instanceof IContainer )
          ( (IContainer)sel ).accept( visitor );
      }
    }
    catch( CoreException e )
    {
      e.printStackTrace();
      
      ErrorDialog.openError( shell, title,
          "Fehler beim Ermitteln der Rechenfälle", e.getStatus() );
    }

    final IFolder[] calcCases = visitor.getCalcCases();
    if( calcCases.length == 0 )
    {
      MessageDialog.openInformation( shell, title,
          "Es sind keine Rechenfälle im Navigator selektiert." );
      return null;
    }

    final ListSelectionDialog dlg = new ListSelectionDialog( shell, calcCases,
        new ArrayContentProvider(), new WorkbenchLabelProvider(), message );
    dlg.setInitialSelections( calcCases );
    if( dlg.open() == Window.CANCEL )
      return null;

    final Object[] calcCasesToCalc = dlg.getResult();
    return (IFolder[])Arrays.castArray( calcCasesToCalc, new IFolder[calcCasesToCalc.length] );
  }
}