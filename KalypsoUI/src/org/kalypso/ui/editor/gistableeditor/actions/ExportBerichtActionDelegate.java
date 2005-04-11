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
package org.kalypso.ui.editor.gistableeditor.actions;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.kalypso.ogc.gml.table.wizard.ExportableLayerTable;
import org.kalypso.services.proxy.DocBean;
import org.kalypso.ui.editor.AbstractGisEditorActionDelegate;
import org.kalypso.ui.editor.gistableeditor.GisTableEditor;
import org.kalypso.ui.metadoc.table.ExportTableBerichtWizard;
import org.kalypso.ui.metadoc.util.MetadocServiceWrapper;

/**
 * @author Belger
 */
public class ExportBerichtActionDelegate extends AbstractGisEditorActionDelegate
{
  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    GisTableEditor editor = (GisTableEditor)getEditor();
    try
    {
      final String username = System.getProperty( "user.name" );
      final MetadocServiceWrapper service = new MetadocServiceWrapper(  );
      final DocBean doc = service.prepareDocument( ".csv", username );
      
      final ExportableLayerTable exp = new ExportableLayerTable( editor.getLayerTable() );
      
      final Wizard exportWizard = new ExportTableBerichtWizard( exp , doc );

      final WizardDialog dialog = new WizardDialog( editor.getSite().getShell(), exportWizard );
      final int ok = dialog.open();

      final Job job = new Job( "Berichtsablage" ) {
        protected IStatus run( IProgressMonitor monitor )
        { 
          try
          {
            if( ok == Window.OK )
              service.commitData( doc );
            else
              service.cancelData( doc );
          }
          catch( CoreException e )
          {
            return e.getStatus();
          }
          
          return Status.OK_STATUS;
        }};
        job.setUser( true );
        job.schedule();
    }
    catch( final CoreException e )
    {
      e.printStackTrace();

      ErrorDialog.openError( editor.getSite().getShell(), "Berichtsablage",
          "Berichtsablagedienst konnte nicht initialisiert werden", e.getStatus() );
    }
  }

  /**
   * @see org.kalypso.ui.editor.AbstractGisEditorActionDelegate#refreshAction()
   */
  protected void refreshAction()
  {
    // nix tun, immer Aktiv
  }
}