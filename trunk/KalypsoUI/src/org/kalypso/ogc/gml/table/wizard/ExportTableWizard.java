package org.kalypso.ogc.gml.table.wizard;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.ui.internal.UIPlugin;
import org.kalypso.ogc.gml.table.LayerTableViewer;
import org.kalypso.ui.ImageProvider;

/**
 * @author belger
 */
public class ExportTableWizard extends Wizard
{
  private final ExportTableWizardPage m_exportTableWizardPage = new ExportTableWizardPage(
      "tableExport", "Tabelle exportieren", ImageProvider.IMAGE_ICON_GTT );

  private final LayerTableViewer m_layerTable;

  public ExportTableWizard( final LayerTableViewer layerTable )
  {
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

    addPage( m_exportTableWizardPage );
  }

  public boolean performFinish()
  {
    final ExportTableWizardPage exportTableWizardPage = m_exportTableWizardPage;
    final LayerTableViewer layerTable = m_layerTable;

    final IRunnableWithProgress runnable = new IRunnableWithProgress()
    {
      public void run( IProgressMonitor monitor ) throws InvocationTargetException
      {
        try
        {
          final File destinationFile = new File( exportTableWizardPage.getDestinationValue() );
          final boolean onlySelected = exportTableWizardPage.getOnlySelected();

          final String[][] csv = layerTable.exportTable( onlySelected );

          final PrintWriter pw = new PrintWriter( new FileWriter( destinationFile ) );

          for( int i = 0; i < csv.length; i++ )
          {
            final String[] line = csv[i];

            for( int j = 0; j < line.length; j++ )
            {
              if( j != 0 )
                pw.print( "," );

              pw.print( "\"" );
              pw.print( line[j] );
              pw.print( "\"" );
            }

            pw.println();
          }

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
      m_exportTableWizardPage.saveWidgetValues();
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