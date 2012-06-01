package org.kalypso.statistics.gui.wizards.importNodesFromSHP;

import java.io.File;
import java.nio.charset.Charset;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWizard;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.statistics.gui.PartManager;
import org.kalypso.statistics.gui.views.nodes.NodesListView;
import org.kalypso.statistics.gui.wizards.importNodesFromSHP.NodeImportOperation.InputDescriptor;
import org.kalypso.statistics.utils.AppUtils;

public class ImportNodesFromShapeWizard extends Wizard implements IWorkbenchWizard
{
  public static final String ID = ImportNodesFromShapeWizard.class.getCanonicalName();

  private final static String PROPERTY_NODE_LABEL = "Node/Station name field";

  private NodeShapeImportWizardPage m_wizardPage;

  public ImportNodesFromShapeWizard( )
  {
    setNeedsProgressMonitor( true );
    setWindowTitle( AppUtils.APPLICATION_TITLE );
  }

  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    final String[] properties = new String[] { PROPERTY_NODE_LABEL };
    m_wizardPage = new NodeShapeImportWizardPage( "shapePage", properties ); //$NON-NLS-1$
    m_wizardPage.setDescription( "Import Nodes / Stations from Shape data" );
    addPage( m_wizardPage );
  }

  /**
   * This method is called by the wizard framework when the user presses the Finish button.
   */
  @Override
  public boolean performFinish( )
  {
    final String nodeLabelProperty = m_wizardPage.getProperty( PROPERTY_NODE_LABEL );

    final File shapeFile = m_wizardPage.getShapeFile();
    final String crs = m_wizardPage.getSelectedCRS();
    final Charset charset = m_wizardPage.getSelectedCharset();

    final InputDescriptor inputDescriptor = new NodeShapeInputDescriptor( shapeFile, nodeLabelProperty, crs, charset );

    try
    {
      // call importer
      final NodeImportOperation op = new NodeImportOperation( inputDescriptor, m_wizardPage.getImportSelectionType() );
      final IStatus execute = RunnableContextHelper.execute( getContainer(), true, true, op );
      new StatusDialog( getShell(), execute, getWindowTitle() ).open();
      if( execute.matches( IStatus.ERROR ) )
        return false;

      // TODO refresh view
      final IWorkbenchPart part = PartManager.getInstance().getActivePart();
      if( part instanceof NodesListView )
      {
        final NodesListView view = (NodesListView) part;
        view.refresh();
      }

    }
    catch( final Exception e )
    {
      Display.getDefault().asyncExec( new Runnable()
      {
        @Override
        public void run( )
        {
          MessageDialog.openError( getShell(), "Error 1", e.getLocalizedMessage() ); //$NON-NLS-1$
        }
      } );
      return false;
    }
    return true;
  }

}
