/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.google.earth.export.wizard;

import java.io.File;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.google.earth.export.constants.IGoogleEarthExportSettings;
import org.kalypso.ui.views.map.MapView;

/**
 * @author kuch
 */
public class WizardGoogleExport extends Wizard implements INewWizard
{

  private PageGoogleExport m_page;

  private final MapView m_mapView;

  private final File m_targetFile;

  public WizardGoogleExport( final MapView mapView, final File targetFile )
  {
    m_mapView = mapView;
    m_targetFile = targetFile;
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    setWindowTitle( "Google Earth (TM) export" );

    m_page = new PageGoogleExport( m_targetFile );
    addPage( m_page );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {

  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    final GoogleEarthExporter googleEarthExporter = new GoogleEarthExporter( m_mapView, m_page );

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, googleEarthExporter );
    ErrorDialog.openError( getShell(), getWindowTitle(), "Error adding geodata set.", status );

    if( status.isOK() )
      return true;

    return false;
  }

  public IGoogleEarthExportSettings getExportedSettings( )
  {
    return m_page;
  }
}
