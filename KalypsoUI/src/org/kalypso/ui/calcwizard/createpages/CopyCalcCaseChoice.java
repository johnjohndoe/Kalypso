package org.kalypso.ui.calcwizard.createpages;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.nature.CalcCaseCollector;
import org.kalypso.ui.nature.ModelNature;

/**
 * Diese Implementierung erzeugt einen völlig neuen Rechenfall im
 * Prognoseverzeichnis
 * 
 * @author belger
 */
public class CopyCalcCaseChoice implements IAddCalcCaseChoice
{
  private Control m_control;

  private IFolder m_folder;

  private final String m_label;

  private final IProject m_project;

  private ListViewer m_viewer;

  private Collection m_oldCalcCases = new LinkedList();

  private final AddCalcCasePage m_page;

  private String m_name;

  public CopyCalcCaseChoice( final String label, final IProject project, final AddCalcCasePage page )
  {
    m_label = label;
    m_project = project;
    m_page = page;
  }

  /**
   * @see org.kalypso.ui.calcwizard.createpages.IAddCalcCaseChoice#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout() );

    final Label label = new Label( panel, SWT.NONE );
    label.setText( "wählen Sie eine der vorhandenen Hochwasser-Vorhersagen:" );

    final ListViewer viewer = new ListViewer( panel, SWT.BORDER );
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setLabelProvider( new WorkbenchLabelProvider() );
    viewer.setInput( m_oldCalcCases );
    final GridData viewerData = new GridData( GridData.FILL_BOTH );
    viewerData.horizontalSpan = 2;
    viewer.getControl().setLayoutData( viewerData );

    viewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection)viewer.getSelection();
        if( selection.isEmpty() )
          setFolder( null );
        else
          setFolder( (IFolder)selection.getFirstElement() );
      }
    } );
    m_viewer = viewer;

    final Label nameLabel = new Label( panel, SWT.NONE );
    nameLabel.setLayoutData( new GridData() );
    nameLabel.setText( "Bezeichnung:" );
    nameLabel.setToolTipText( AddNewCalcCaseChoice.TOOLTIP );

    final Text edit = new Text( panel, SWT.BORDER );
    edit.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    edit.setToolTipText( AddNewCalcCaseChoice.TOOLTIP );
    edit.addModifyListener( new ModifyListener()
    {
      public void modifyText( ModifyEvent e )
      {
        setName( edit.getText() );
      }
    } );

    m_control = panel;

    try
    {
      refresh( new NullProgressMonitor() );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }
  }

  protected void setName( final String text )
  {
    m_name = text;
    
    validateChoice();
  }

  protected void setFolder( final IFolder folder )
  {
    m_folder = folder;
    
    validateChoice();
  }

  public void refresh( final IProgressMonitor monitor ) throws CoreException
  {
    final ModelNature nature = (ModelNature)m_project.getNature( ModelNature.ID );
    final IFolder prognoseFolder = nature.getPrognoseFolder();

    // alle Prognosen finden
    final CalcCaseCollector calcCaseCollector = new CalcCaseCollector();
    prognoseFolder.accept( calcCaseCollector );
    final IFolder[] calcCases = calcCaseCollector.getCalcCases();

    final List usedCalcCases = m_page.getCalcCases();

    m_oldCalcCases.clear();

    IFolder newSelect = null;
    if( usedCalcCases != null )
    {
      for( int i = 0; i < calcCases.length; i++ )
      {
        final IFolder folder = calcCases[i];

        if( !usedCalcCases.contains( folder ) )
        {
          m_oldCalcCases.add( folder );
          if( newSelect == null )
            newSelect = folder;
        }
      }
    }

    final IFolder newSelectFinal = newSelect;
    final Viewer viewer = m_viewer;
    if( viewer != null )
    {
      viewer.getControl().getDisplay().syncExec( new Runnable()
      {
        public void run()
        {
          viewer.refresh();

          if( newSelectFinal == null )
            viewer.setSelection( StructuredSelection.EMPTY );
          else
            viewer.setSelection( new StructuredSelection( newSelectFinal ) );
        }
      } );
    }

    m_page.getWizard().getContainer().updateButtons();
  }

  /**
   * @throws CoreException
   * @see org.kalypso.ui.calcwizard.createpages.IAddCalcCaseChoice#perform(org.eclipse.core.runtime.IProgressMonitor)
   */
  public IFolder perform( final IProgressMonitor monitor ) throws CoreException
  {
    final ModelNature nature = (ModelNature)m_project.getNature( ModelNature.ID );

    final IFolder folder = nature.getPrognoseFolder();
    if( m_name.length() == 0 )
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Geben Sie einen Namen für die Vorhersage ein", null ) );

    final IFolder calcCaseFolder = folder.getFolder( m_name );
    if( calcCaseFolder.exists() )
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Eine Vorhersage mit diesem namen existiert bereits: " + m_name, null ) );

    if( m_folder == null )
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Es muss eine vorhandene Berechnung ausgewählt werden", null ) );

    // quellverzeichnis holen
    m_folder.copy( calcCaseFolder.getFullPath(), false, monitor );

    return calcCaseFolder;
  }

  /**
   * @see org.kalypso.ui.calcwizard.createpages.IAddCalcCaseChoice#getControl()
   */
  public Control getControl()
  {
    return m_control;
  }

  /**
   * @see org.kalypso.ui.calcwizard.createpages.IAddCalcCaseChoice#toString()
   */
  public String toString()
  {
    return m_label;
  }

  /**
   * @see org.kalypso.ui.calcwizard.createpages.IAddCalcCaseChoice#shouldUpdate()
   */
  public boolean shouldUpdate()
  {
    return true;
  }

  /**
   * @see org.kalypso.ui.calcwizard.createpages.IAddCalcCaseChoice#validateChoice()
   */
  public void validateChoice()
  {
    if( m_folder == null )
    {
      m_page.setErrorMessage( "Es muss ein vorhandener Rechenfall ausgewählt werden." );
      m_page.setMessage( null );
      m_page.setPageComplete( false );
    }
    
    final IStatus status = m_project.getWorkspace().validateName( m_name, IResource.FOLDER );
    if( status.getSeverity() == IStatus.OK )
    {
      m_page.setErrorMessage( null );
      m_page.setMessage( null );
      m_page.setPageComplete( true );
    }
    else
    {
      m_page.setErrorMessage( status.getMessage() );
      m_page.setMessage( null );
      m_page.setPageComplete( false );
    }
  }
}