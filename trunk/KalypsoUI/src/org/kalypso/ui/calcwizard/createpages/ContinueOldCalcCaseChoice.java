package org.kalypso.ui.calcwizard.createpages;

import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.kalypso.ui.nature.CalcCaseCollector;
import org.kalypso.ui.nature.ModelNature;

/**
 * Die Implementierung erzeugt einen v�llig neuen Rechenfall im
 * Prognoseverzeichnis
 * 
 * @author belger
 */
public class ContinueOldCalcCaseChoice implements IAddCalcCaseChoice
{
  private Control m_control;
  
  private IFolder m_folder;

  private final String m_label;

  private final IProject m_project;

  private ListViewer m_viewer;

  private Collection m_usedCalcCases = null;

  private Button m_checkbox;

  private final AddCalcCasePage m_page;
  
  private boolean m_isUpdateCalcCase = false;

  public ContinueOldCalcCaseChoice( final String label, final IProject project, final AddCalcCasePage page )
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
    label.setText( "w�hlen Sie eine der vorhandenen Hochwasser-Vorhersagen:" );

    final ListViewer viewer = new ListViewer( panel, SWT.BORDER ); 
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setLabelProvider( new WorkbenchLabelProvider() );
    viewer.setInput( m_page.getCalcCases() );
    viewer.getControl().setLayoutData( new GridData( GridData.FILL_BOTH ) );

    final Button checkbox = new Button( panel, SWT.CHECK );
    checkbox.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    checkbox.setText( "Zeitreihen aktualisieren" );
    checkbox.setSelection( m_isUpdateCalcCase );
    m_checkbox = checkbox;
    
    m_checkbox.addSelectionListener( new SelectionAdapter() 
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      public void widgetSelected( final SelectionEvent e )
      {
        if( e.detail == SWT.CHECK )
        {
          setUpdate( checkbox.getSelection() );
        }
      }
      
    } );
    
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

    m_control = panel;

    try
    {
      update( new NullProgressMonitor() );
    }
    catch( CoreException e )
    {
      e.printStackTrace();
    }
  }

  protected void setUpdate( final boolean update )
  {
    m_isUpdateCalcCase = update;
  }

  protected void setFolder( final IFolder folder )
  {
    m_folder = folder;
  }
  
  public boolean canFlipToNextPage()
  {
    return m_folder != null;
  }

  public void update( final IProgressMonitor monitor ) throws CoreException
  {
    final ModelNature nature = (ModelNature)m_project.getNature( ModelNature.ID );
    final IFolder prognoseFolder = nature.getPrognoseFolder();

    // alle Prognosen finden
    final CalcCaseCollector calcCaseCollector = new CalcCaseCollector();
    prognoseFolder.accept( calcCaseCollector );
    final IFolder[] calcCases = calcCaseCollector.getCalcCases();

    final List oldCases = m_page.getCalcCases();
    oldCases.clear();

    IFolder newSelect = null;
    if( m_usedCalcCases != null )
    {
      for( int i = 0; i < calcCases.length; i++ )
      {
        final IFolder folder = calcCases[i];
        
        if( !m_usedCalcCases.contains( folder ) )
        {
          oldCases.add( folder );
          if( newSelect == null )
            newSelect = folder;
        }
      }
    }

    final IFolder newSelectFinal = newSelect;
    final Viewer viewer = m_viewer;
    if( viewer != null )
    {
      viewer.getControl().getDisplay().asyncExec( new Runnable()
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
  }

  /**
   * @see org.kalypso.ui.calcwizard.createpages.IAddCalcCaseChoice#perform(org.eclipse.core.runtime.IProgressMonitor)
   */
  public IFolder perform( final IProgressMonitor monitor )
  {
    return m_folder;
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
   * @see org.kalypso.ui.calcwizard.createpages.IAddCalcCaseChoice#isUpdateCalcCase()
   */
  public boolean isUpdateCalcCase()
  {
    return m_isUpdateCalcCase;
  }

}