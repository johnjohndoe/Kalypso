/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
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
import org.kalypso.contribs.java.util.Arrays;
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
   * Lässt den Benutzer aus einer Liste von Rechenfällen auswählen Es werden alle Rechenfälle angezeigt, welche sich in
   * oder unterhalb der angegebenen Selection von Resourcen befinden.
   * 
   * @param shell
   * @param selection
   * @param title
   * 
   * @param message
   * @return null bei Abbruch
   */
  public static IFolder[] chooseCalcCases( final Shell shell, final ISelection selection, final String title,
      final String message )
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

      ErrorDialog.openError( shell, title, "Fehler beim Ermitteln der Rechenfälle", e.getStatus() );
    }

    final IFolder[] calcCases = visitor.getCalcCases();
    if( calcCases.length == 0 )
    {
      MessageDialog.openInformation( shell, title, "Es sind keine Rechenvarianten im Navigator selektiert." );
      return null;
    }

    final ListSelectionDialog dlg = new ListSelectionDialog( shell, calcCases, new ArrayContentProvider(),
        new WorkbenchLabelProvider(), message );
    dlg.setInitialSelections( calcCases );
    if( dlg.open() == Window.CANCEL )
      return null;

    final Object[] calcCasesToCalc = dlg.getResult();
    return (IFolder[])Arrays.castArray( calcCasesToCalc, new IFolder[calcCasesToCalc.length] );
  }
}