package org.kalypso.ui.view.prognose;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.calcwizard.CalcWizard;
import org.kalypso.ui.calcwizard.CalcWizardDialog;
import org.kalypso.util.synchronize.ModelSynchronizer;

/**
 * @author belger
 */
public class PrognoseView extends ViewPart
{
  private Button m_button;

  private final PrognosePanel m_panel;
  
  public PrognoseView()
  {
    final URL location = KalypsoGisPlugin.getDefault().getModellistLocation();
    m_panel = new PrognosePanel( location );
  }
  
  public void dispose()
  {
    m_panel.dispose();
  }

  /**
   * @see org.eclipse.ui.intro.IIntroPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( final Composite parent )
  {
    final Display display = parent.getDisplay();
    final FormToolkit toolkit = new FormToolkit( display );
    final Form form = toolkit.createForm( parent );
    form.setBackground( display.getSystemColor( SWT.COLOR_WHITE ) );

    final GridLayout gridLayout = new GridLayout( 1, false );
    gridLayout.horizontalSpacing = 20;
    gridLayout.verticalSpacing = 20;
    gridLayout.marginHeight = 20;
    gridLayout.marginWidth = 20;
    form.getBody().setLayout( gridLayout );
    form.getBody().setBackground( display.getSystemColor( SWT.COLOR_WHITE ) );

    final GridData formGridData = new GridData( GridData.FILL_BOTH );
    formGridData.horizontalAlignment = GridData.CENTER;
    form.setLayoutData( formGridData );
    
    final Composite panelControl = m_panel.createControl( form.getBody() );
    panelControl.setBackground( display.getSystemColor( SWT.COLOR_WHITE ) );
    panelControl.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    m_button = toolkit.createButton( form.getBody(), "Hochwasser Vorhersage starten", SWT.PUSH );

    final PrognosePanel panel = m_panel;
    m_button.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( final SelectionEvent e )
      {
        startModel( panel.getModel() );
      }
    } );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#setFocus()
   */
  public void setFocus()
  {
    // mir doch egal
  }

  protected void startModel( final String projectName )
  {
    final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
    
    // Projekt updaten
    final File serverRoot = KalypsoGisPlugin.getDefault().getServerModelRoot();

    if( serverRoot == null )
    {
      // TODO: error handling
      return;
    }

    // TODO: progress monitor
    // am besten busyCursorWhile

    final IProject project = root.getProject( projectName );
    final File serverProject = new File( serverRoot, projectName );
    
    if( !serverProject.exists() )
    {
      // TODO: error message
      System.out.println( "Servermodel does not exist! Cannot start Prognose." );
      return;
    }
    
    
    final IRunnableWithProgress op = new IRunnableWithProgress()
    {
      public void run( IProgressMonitor monitor ) throws InvocationTargetException
      {
        try
        {
          final ModelSynchronizer synchronizer = new ModelSynchronizer( project, serverProject );
          synchronizer.updateLocal( monitor );
        }
        catch( final CoreException ce )
        {
          ce.printStackTrace();
          throw new InvocationTargetException( ce );
        }
      }
    };
    
    final IWorkbench workbench = PlatformUI.getWorkbench();
    try
    {
      workbench.getProgressService().busyCursorWhile( op );
    }
    catch( final InvocationTargetException e )
    {
      e.printStackTrace();
      
      final CoreException ce = (CoreException)e.getTargetException();
      ErrorDialog.openError( workbench.getDisplay().getActiveShell(), "Vorhersage starten", "Modell konnte nicht aktualisiert werden", ce.getStatus() );
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
    }
    
    final CalcWizard wizard = new CalcWizard( project );

    final WizardDialog dialog = new CalcWizardDialog( getSite().getShell(), wizard ); 
    dialog.open();
  }}
