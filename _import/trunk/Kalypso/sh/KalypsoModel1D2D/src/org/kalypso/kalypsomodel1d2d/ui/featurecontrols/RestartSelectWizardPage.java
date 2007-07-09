package org.kalypso.kalypsomodel1d2d.ui.featurecontrols;

import java.io.File;

import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.kalypso.kalypsomodel1d2d.ui.map.flowrel.wizardPageZmlImportWithPreview.FileTreeContentProvider;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class RestartSelectWizardPage extends WizardPage
{
  private class ResultFilter extends ViewerFilter
  {
    @Override
    public boolean select( final Viewer viewer, final Object parentElement, final Object element )
    {
      if( element instanceof File )
        return ((File) element).getName().endsWith( ".gml" );
      return true;
    }
  }

  protected RestartSelectWizardPage( )
  {
    super( "Example" );
    setTitle( "Select restart file(s)" );
    setDescription( "Please select one or more restart files that you want to use for your next calculation" );
  }

  public void createControl( Composite parent )
  {
    final Composite topComposite = new Composite( parent, SWT.NONE );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 4;
    gridLayout.makeColumnsEqualWidth = true;
    topComposite.setLayout( gridLayout );

    final Composite innerComposite = new SashForm( topComposite, SWT.HORIZONTAL | SWT.NULL );
    GridData data = new GridData();
    data.horizontalAlignment = GridData.FILL;
    data.verticalAlignment = GridData.FILL;
    data.horizontalSpan = 4;
    data.grabExcessHorizontalSpace = true;
    data.grabExcessVerticalSpace = true;
    innerComposite.setLayoutData( data );

    final TreeViewer treeViewer = new TreeViewer( innerComposite, SWT.BORDER );

    // TODO: do NOT use this file stuff! do NOT work against eclipse!
    // TODO: either use WorkbenchLabelProvider and so on on IFiles/IFolders
    // TODO: or even use ProjectExlorer techniques
    treeViewer.setContentProvider( new FileTreeContentProvider() );
    treeViewer.setLabelProvider( new WorkbenchLabelProvider() );
    treeViewer.setInput( new File( "../.." ) );
    treeViewer.addFilter( new ResultFilter() );

//    final Button loadZmlBtn = new Button( topComposite, SWT.PUSH );
//    data = new GridData();
//    data.horizontalAlignment = GridData.FILL;
//    data.horizontalSpan = 1;
//    data.grabExcessHorizontalSpace = false;
//    loadZmlBtn.setText( "Importieren" );
//    loadZmlBtn.setLayoutData( data );
//    loadZmlBtn.addSelectionListener( new SelectionAdapter()
//    {
//      /**
//       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
//       */
//      @Override
//      public void widgetSelected( final SelectionEvent e )
//      {
//        // TODO: ths is very strange. Where does the wizard put the files??
//        final IWizardDescriptor wizardDesc = PlatformUI.getWorkbench().getNewWizardRegistry().findWizard( "org.kalypso.ui.wizards.imports.observation.ImportObservationWizard" );
//        final Shell shell = topComposite.getShell();
//        try
//        {
//          final IWorkbenchWizard wizard = wizardDesc.createWizard();
//          wizard.init( PlatformUI.getWorkbench(), new StructuredSelection( importFolder ) );
//          final WizardDialog dialog = new WizardDialog( shell, wizard );
//          dialog.open();
//        }
//        catch( final CoreException e1 )
//        {
//          final IStatus status = e1.getStatus();
//          ErrorDialog.openError( shell, "Fehler", "Konnte den Assistenten zum Import von Zeitreihen nicht starten", status );
//          KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
//        }
//        treeViewer.refresh();
//      }
//    } );
//
    setControl( topComposite );

//    topComposite.getShell().setMinimumSize( 400, 300 );
  }

}
