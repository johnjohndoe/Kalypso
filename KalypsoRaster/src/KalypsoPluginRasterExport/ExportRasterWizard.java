package KalypsoPluginRasterExport;

import java.io.File;
import java.net.URL;
import java.util.List;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IImportWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.ide.IDE;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage;

import KalypsoPluginRasterImport.GridUtils;

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

public class ExportRasterWizard extends Wizard implements IImportWizard
{
  private ExportRasterSelectionWizardPage m_page1 = null;

  private IStructuredSelection m_selection;

  public ExportRasterWizard()
  {
    super();
    setHelpAvailable( false );
    setNeedsProgressMonitor( false );
    setWindowTitle( "Import Raster" );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( IWorkbench workbench, IStructuredSelection currentSelection )
  {
    m_selection = currentSelection;
    final List selectedResources = IDE.computeSelectedResources( currentSelection );
    if( !selectedResources.isEmpty() )
    {
      m_selection = new StructuredSelection( selectedResources );
    }

    setWindowTitle( "Title" );
    setNeedsProgressMonitor( true );
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#addPages()
   */
  public void addPages()
  {
    super.addPages();

    m_page1 = new ExportRasterSelectionWizardPage( "Dateien waehlen" );
    addPage( m_page1 );

    m_page1.setSelection( m_selection );
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#performCancel()
   */
  public boolean performCancel()
  {
    return true;
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  public boolean performFinish()
  {
    try
    {
      final RasterExportSelection selection = (RasterExportSelection)m_page1.getSelection();
      final File fileSource = selection.getSourceFile();
      final File fileTarget = selection.getFileTarget();
      final String format = selection.getTargetFormat();

      if( fileSource.exists() )
      {
        if( format.equals( "Ascii" ) )
        {
          Dialog monitor = new ProgressMonitorDialog( this.getShell() );
          monitor.open();
          URL schemaUrl = getClass().getResource( "../schema/RasterDataModel.xsd" );
          RectifiedGridCoverage grid = GridUtils.readRasterData( fileSource, schemaUrl );
          GridUtils.exportGridArc( fileTarget, grid );
          monitor.close();
        }
        else
        {
          MessageDialog.openConfirm( this.getShell(), "Information",
              "Export-Function not implemented" );
          return false;
        }
      }
      else
      {
        System.out.println( "fileSource does not exist" );
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return false;
    }
    return true;
  }

}