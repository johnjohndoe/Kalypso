package org.kalypso.ui.calcwizard.createpages;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.kalypso.ui.calcwizard.ICalcWizardPage;

/**
 * @author belger
 */
public class AddCalcCasePage extends WizardPage implements ICalcWizardPage
{
  private final List m_choices = new LinkedList();

  private ComboViewer m_chooserViewer;

  private List m_calcCases = new LinkedList();

  private IFolder m_currentCalcCase = null;

  private IAddCalcCaseChoice m_choice;

  private ListViewer m_caseViewer;

  public AddCalcCasePage()
  {
    super( "addPrognoseWizardPage", "Vorhersage hinzufügen", null );

    setTitle( "Vorhersage hinzufügen" );
  }
  
  public void addChoice( final IAddCalcCaseChoice choice )
  {
    m_choices.add( choice );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout() );

    createExistingGroup( panel );

    final Label chooserLabel = new Label( panel, SWT.NONE );
    chooserLabel.setText( "Bitte wählen Sie aus, was Sie tun möchten:" );

    m_chooserViewer = new ComboViewer( panel, SWT.READ_ONLY | SWT.DROP_DOWN );
    m_chooserViewer.getControl().setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    //m_chooserViewer.add( m_choices );
    
    m_chooserViewer.setContentProvider( new ArrayContentProvider( ) );
    m_chooserViewer.setInput( m_choices );

    final Group choiceGroup = new Group( panel, SWT.NONE );
    choiceGroup.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    final StackLayout choiceLayout = new StackLayout();
    choiceGroup.setLayout( choiceLayout );

    for( final Iterator iter = m_choices.iterator(); iter.hasNext(); )
      ((IAddCalcCaseChoice)iter.next()).createControl( choiceGroup );

    m_chooserViewer.addPostSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final ISelection selection = event.getSelection();
        if( !selection.isEmpty() )
        {
          final IAddCalcCaseChoice choice = getChoosen( selection );
          setChoice( choice );
          choiceLayout.topControl = choice.getControl();
          choiceGroup.setText( choice.toString() );
  
          choiceGroup.layout( true );
        }
      }

    } );

    setControl( panel );

    if( m_choices.size() > 0 )
      m_chooserViewer.setSelection( new StructuredSelection( m_choices.get( 0 ) ) );
  }

  protected void setChoice( final IAddCalcCaseChoice choice )
  {
    m_choice = choice;
  }

  public IFolder getCurrentCalcCase()
  {
    return m_currentCalcCase;
  }

  public IAddCalcCaseChoice getChoosen( final ISelection selection )
  {
    return (IAddCalcCaseChoice)( (IStructuredSelection)selection ).getFirstElement();
  }

  private void createExistingGroup( final Composite parent )
  {
    final Group existingGroup = new Group( parent, SWT.NONE );
    existingGroup.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    existingGroup.setLayout( new GridLayout() );
    existingGroup.setText( "In dieser Sitzung durchgeführte Vorhersagen" );

    m_caseViewer = new ListViewer( existingGroup );
    final GridData viewerData = new GridData();
    viewerData.grabExcessHorizontalSpace = true;
    viewerData.grabExcessVerticalSpace = true;
    viewerData.horizontalAlignment = GridData.FILL;
    viewerData.verticalAlignment = GridData.FILL;
    m_caseViewer.getControl().setLayoutData( viewerData );

    m_caseViewer.setContentProvider( new ArrayContentProvider() );

    m_caseViewer.setInput( m_calcCases );
    m_caseViewer.setLabelProvider( new WorkbenchLabelProvider() );
  }

  /**
   * @see org.kalypso.ui.calcwizard.ICalcWizardPage#doNext(org.eclipse.core.runtime.IProgressMonitor)
   */
  public void doNext( final IProgressMonitor monitor ) throws CoreException
  {
    final IFolder folder = m_choice.perform( monitor );
    m_calcCases.add( folder );
    m_currentCalcCase = folder;

    final ListViewer viewer = m_caseViewer;
    viewer.getControl().getDisplay().asyncExec( new Runnable()
    {
      public void run()
      {
        viewer.refresh();
      }
    } );
  }
  
  public void update( final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "Seite wird aktualisiert", m_choices.size() );
    for( Iterator iter = m_choices.iterator(); iter.hasNext(); )
    {
      ((IAddCalcCaseChoice)iter.next()).update( new NullProgressMonitor() );
      monitor.worked( 1 );
    }
    
    monitor.done();
  }

  /**
   * @see org.kalypso.ui.calcwizard.ICalcWizardPage#clean(org.eclipse.core.runtime.IProgressMonitor)
   */
  public void clean( final IProgressMonitor monitor )
  {
    // todo: neuen Rechenfall wieder löschen?
  }

  public boolean isUpdateCalcCase()
  {
    return m_choice.isUpdateCalcCase();
  }

  public List getCalcCases()
  {
    return m_calcCases;
  }
}