package org.kalypso.kalypso1d2d.pjt.wizards;

import java.util.Vector;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;

public class RestartSelectWizardPage2 extends WizardPage
{
  private static final ImageData IMAGEDATA_ARROW_UP = KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/list_up.gif" ).getImageData(); //$NON-NLS-1$

  private static final ImageData IMAGEDATA_ARROW_DOWN = KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/list_down.gif" ).getImageData(); //$NON-NLS-1$

  private ListViewer m_listViewer;

  private final Vector<IStepResultMeta> m_sortedResults = new Vector<>();

  private Button m_btnUp;

  private Button m_btnDown;

  private IStepResultMeta m_selectedInput;

  public RestartSelectWizardPage2( final String pageName, final String title, final ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );
    setDescription( Messages.getString( "org.kalypso.kalypso1d2d.pjt.wizards.RestartSelectWizardPage2.0" ) ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    /* set a fixed size to the Wizard */
    final Object layoutData = parent.getLayoutData();
    if( layoutData instanceof GridData )
    {
      final GridData pLayout = (GridData) layoutData;
      pLayout.widthHint = 700;
      pLayout.heightHint = 300;
      parent.layout();
    }
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout( 2, false ) );
    final Composite panelLeft = new Composite( panel, SWT.NONE );
    final GridData gridData = new GridData();
    gridData.verticalAlignment = GridData.FILL;
    gridData.grabExcessVerticalSpace = true;
    gridData.horizontalAlignment = GridData.FILL;
    gridData.grabExcessHorizontalSpace = true;
    panelLeft.setLayout( new FillLayout( SWT.FILL ) );
    panelLeft.setLayoutData( gridData );

    m_listViewer = new ListViewer( panelLeft, SWT.BORDER );
    m_listViewer.setContentProvider( new IStructuredContentProvider()
    {
      @Override
      public Object[] getElements( final Object inputElement )
      {
        final Vector< ? > list = (Vector< ? >) inputElement;
        return list.toArray();
      }

      @Override
      public void dispose( )
      {
      }

      @Override
      public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
      {
        m_btnUp.setEnabled( m_selectedInput != null );
        m_btnDown.setEnabled( m_selectedInput != null );
      }
    } );
    m_listViewer.setLabelProvider( new LabelProvider()
    {
      @Override
      public Image getImage( final Object element )
      {
        return null;
      }

      @Override
      public String getText( final Object object )
      {
        final IResultMeta resultMeta = (IResultMeta) object;
        return resultMeta.getOwner().getName() + ", " + resultMeta.getName(); //$NON-NLS-1$
      }
    } );
    m_listViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      @SuppressWarnings("synthetic-access")
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final ISelection selection = event.getSelection();
        if( selection.isEmpty() )
        {
          m_btnUp.setEnabled( false );
          m_btnDown.setEnabled( false );
        }
        else
        {
          m_btnUp.setEnabled( true );
          m_btnDown.setEnabled( true );
          m_selectedInput = (IStepResultMeta) ((IStructuredSelection) selection).getFirstElement();
        }
      }
    } );

    final Composite panelRight = new Composite( panel, SWT.NONE );
    panelRight.setLayout( new GridLayout( 1, true ) );
    panelRight.setLayoutData( new GridData( GridData.END ) );
    m_btnUp = new Button( panelRight, SWT.PUSH );
    m_btnUp.setImage( new Image( parent.getDisplay(), IMAGEDATA_ARROW_UP ) );
    m_btnUp.setEnabled( false );
    m_btnUp.addSelectionListener( new SelectionListener()
    {
      @Override
      public void widgetDefaultSelected( final SelectionEvent e )
      {
      }

      @Override
      @SuppressWarnings("synthetic-access")
      public void widgetSelected( final SelectionEvent e )
      {
        final int position = m_sortedResults.indexOf( m_selectedInput );
        if( position == 0 )
          return;
        m_sortedResults.remove( position );
        m_sortedResults.insertElementAt( m_selectedInput, position - 1 );
        final ISelection selection = m_listViewer.getSelection();
        m_listViewer.setInput( m_sortedResults );
        m_listViewer.setSelection( selection );
      }
    } );
    m_btnDown = new Button( panelRight, SWT.PUSH );
    m_btnDown.setImage( new Image( parent.getDisplay(), IMAGEDATA_ARROW_DOWN ) );
    m_btnDown.setEnabled( false );
    m_btnDown.addSelectionListener( new SelectionListener()
    {
      @Override
      public void widgetDefaultSelected( final SelectionEvent e )
      {
      }

      @Override
      @SuppressWarnings("synthetic-access")
      public void widgetSelected( final SelectionEvent e )
      {
        final int position = m_sortedResults.indexOf( m_selectedInput );
        if( position == m_sortedResults.size() - 1 )
          return;
        m_sortedResults.remove( position );
        m_sortedResults.insertElementAt( m_selectedInput, position + 1 );
        final ISelection selection = m_listViewer.getSelection();
        m_listViewer.setInput( m_sortedResults );
        m_listViewer.setSelection( selection );
      }
    } );
    setControl( panel );
  }

  public void initializeResults( final IResultMeta[] resultMetas )
  {
    m_sortedResults.clear();
    for( final IResultMeta element : resultMetas )
    {
      if( element instanceof IStepResultMeta )
        m_sortedResults.add( (IStepResultMeta) element );
    }
    m_listViewer.setInput( m_sortedResults );
  }

  public IResultMeta[] getSortedResults( )
  {
    return m_sortedResults.toArray( new IResultMeta[0] );
  }
}
