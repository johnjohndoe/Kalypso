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
package org.kalypso.ogc.gml.table.wizard;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Map;

import org.bce.eclipse.jface.wizard.SaveFileWizardPage;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.ui.internal.UIPlugin;
import org.kalypso.ogc.gml.table.LayerTableViewer;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.metadoc.table.ExportTableOptionsPage;
import org.kalypso.util.io.CSV;

/**
 * @author belger
 */
public class ExportTableWizard extends Wizard
{
  private final ExportTableOptionsPage m_optionPage = new ExportTableOptionsPage( "tableExport", "Tabelle exportieren",
      ImageProvider.IMAGE_ICON_GTT );

  private final SaveFileWizardPage m_filePage;

  private final LayerTableViewer m_layerTable;

  public ExportTableWizard( final LayerTableViewer layerTable )
  {
    final Map formats = new HashMap();
    formats.put( "Comma Separated Values (CSV)", "csv" );

    m_filePage = new SaveFileWizardPage( "tableExport", "Tabelle exportieren", ImageProvider.IMAGE_ICON_GTT,
        "Export Ziel", formats );

    final IDialogSettings workbenchSettings = UIPlugin.getDefault().getDialogSettings();
    IDialogSettings section = workbenchSettings.getSection( "ExportTableWizard" );//$NON-NLS-1$
    if( section == null )
      section = workbenchSettings.addNewSection( "ExportTableWizard" );//$NON-NLS-1$
    setDialogSettings( section );

    setWindowTitle( "Export" );

    m_layerTable = layerTable;
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  public void addPages()
  {
    super.addPages();

    addPage( m_filePage );
    addPage( m_optionPage );
  }

  public boolean performFinish()
  {
    final SaveFileWizardPage filePage = m_filePage;
    final ExportTableOptionsPage optionPage = m_optionPage;

    final LayerTableViewer layerTable = m_layerTable;

    final IRunnableWithProgress runnable = new IRunnableWithProgress()
    {
      public void run( IProgressMonitor monitor ) throws InvocationTargetException
      {
        try
        {
          final File destinationFile = new File( filePage.getDestinationValue() );
          final boolean onlySelected = optionPage.getOnlySelected();

          final String[][] csv = layerTable.exportTable( onlySelected );
          final PrintWriter pw = new PrintWriter( new FileWriter( destinationFile ) );
          CSV.writeCSV( csv, pw );
          pw.close();
        }
        catch( final IOException e )
        {
          throw new InvocationTargetException( e, "Fehler beim Export\n" + e.getLocalizedMessage() );
        }
      }
    };

    try
    {
      getContainer().run( false, false, runnable );

      m_filePage.saveWidgetValues();
      m_optionPage.saveWidgetValues();
    }
    catch( final InvocationTargetException e )
    {
      e.printStackTrace();

      final MessageBox mb = new MessageBox( getContainer().getShell(), SWT.ICON_ERROR | SWT.OK );
      mb.setMessage( e.getLocalizedMessage() );
      mb.open();

      return false;
    }
    catch( final InterruptedException e )
    {
      // should never occur
      e.printStackTrace();
      return false;
    }

    return true;
  }
}