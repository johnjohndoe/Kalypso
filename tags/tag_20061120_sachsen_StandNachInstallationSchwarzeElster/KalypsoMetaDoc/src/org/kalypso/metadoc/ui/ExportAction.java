/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.metadoc.ui;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPart;
import org.kalypso.metadoc.IExportTarget;
import org.kalypso.metadoc.IExportableObjectFactory;

/**
 * The default export action which takes care of opening the wizards for the configuration and performing the export.
 * 
 * @author schlienger
 */
public class ExportAction extends Action
{
  private final IExportTarget m_target;
  private IWorkbenchPart m_part;

  public ExportAction( final IExportTarget target, final IWorkbenchPart part )
  {
    super( target.getName(), target.getImage() );
    setToolTipText( target.getDescription() );

    m_part = part;
    m_target = target;
  }

  public void run()
  {
    final IExportableObjectFactory factory = (IExportableObjectFactory)m_part
        .getAdapter( IExportableObjectFactory.class );
    if( factory == null )
      return;

    final Shell shell = m_part.getSite().getShell();

    try
    {
      // TODO: define a default image for the export wizard (maybe target should yield one)
      final Wizard wizard = new ExportWizard( m_target, factory, shell, null, "Exportdialog" );
      final WizardDialog dlg = new WizardDialog( shell, wizard );
      dlg.open();
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      ErrorDialog.openError( shell, "Exportdialog", "Fehler beim Erstellen des Exportdialog", e.getStatus() );
    }
  }

  public void setActivePart( final IWorkbenchPart part )
  {
    m_part = part;
  }
}
