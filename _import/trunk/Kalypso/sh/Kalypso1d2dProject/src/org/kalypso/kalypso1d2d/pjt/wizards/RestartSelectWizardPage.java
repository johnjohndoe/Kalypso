package org.kalypso.kalypso1d2d.pjt.wizards;

import java.io.File;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.kalypso.kalypso1d2d.pjt.views.contentprov.ResultNavigatorContentProvider;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.metadata.IResultModelDescriptor;
import org.kalypso.kalypsomodel1d2d.schema.binding.metadata.ISimulationDescriptor;
import org.kalypso.kalypsomodel1d2d.schema.binding.metadata.ResultDB;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class RestartSelectWizardPage extends WizardPage
{
  private Text m_selectedResultNames;

  private final String m_initialSelection;

  private final Set<IResultModelDescriptor> m_selectedResults = new HashSet<IResultModelDescriptor>();

  private final IResultModelDescriptor[] m_initialDescriptors;

  public RestartSelectWizardPage( final String pageName, final String initialSelection )
  {
    this( pageName, initialSelection, new IResultModelDescriptor[0] );
  }

  public RestartSelectWizardPage( final String pageName, final IResultModelDescriptor[] existingResultDescriptors )
  {
    this( pageName, "", existingResultDescriptors );
  }

  public RestartSelectWizardPage( final String pageName, final String initialSelection, final IResultModelDescriptor[] initialDescriptors )
  {
    super( pageName );

    m_initialSelection = initialSelection;
    m_initialDescriptors = initialDescriptors;
  }

  public void createControl( final Composite parent )
  {
    final Composite topComposite = new Composite( parent, SWT.NONE );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 4;
    gridLayout.makeColumnsEqualWidth = true;
    topComposite.setLayout( gridLayout );
    setControl( topComposite );

    final Composite innerComposite = new SashForm( topComposite, SWT.HORIZONTAL | SWT.NULL );
    final GridData data = new GridData();
    data.horizontalAlignment = GridData.FILL;
    data.verticalAlignment = GridData.FILL;
    data.horizontalSpan = 4;
    data.grabExcessHorizontalSpace = true;
    data.grabExcessVerticalSpace = true;
    innerComposite.setLayoutData( data );

    final TreeViewer allResultsViewer = new TreeViewer( innerComposite, SWT.BORDER );

    allResultsViewer.setContentProvider( new ResultNavigatorContentProvider() );
    allResultsViewer.setLabelProvider( new WorkbenchLabelProvider() );
    allResultsViewer.setInput( new File( "." ) );

    m_selectedResultNames = new Text( innerComposite, SWT.MULTI | SWT.BORDER );
    m_selectedResultNames.setEditable( false );
    m_selectedResultNames.setText( "" );

    loadInitialSelection();
    if( m_initialDescriptors != null )
      loadInitialDescriptors();

    allResultsViewer.addDoubleClickListener( new IDoubleClickListener()
    {
      public void doubleClick( final DoubleClickEvent event )
      {
        final StructuredSelection selection = (StructuredSelection) event.getSelection();
        final Object firstElement = selection.getFirstElement();
        if( firstElement instanceof IResultModelDescriptor )
          setSelection( (IResultModelDescriptor) firstElement );
      }
    } );
  }

  private void loadInitialSelection( )
  {
    final String[] selections = m_initialSelection.split( ";" );

    final ResultDB resultDB = KalypsoModel1D2DPlugin.getDefault().getResultDB();

    final IFeatureWrapperCollection<ISimulationDescriptor> simulationDescriptors = resultDB.getSimulationDescriptors();
    for( final ISimulationDescriptor descriptor : simulationDescriptors )
    {
      final IFeatureWrapperCollection<IResultModelDescriptor> resultModel = descriptor.getResultModel();
      for( int i = 0; i < resultModel.size(); i++ )
      {
        final String path = resultModel.get( i ).getWorkspacePath();
        for( final String element : selections )
          if( element.equals( path ) )
            setSelection( resultModel.get( i ) );
      }
    }
  }

  private void loadInitialDescriptors( )
  {
    for( final IResultModelDescriptor desc : m_initialDescriptors )
      setSelection( desc );
  }

  protected void setSelection( final IResultModelDescriptor descriptor )
  {
    if( m_selectedResults.contains( descriptor ) )
      m_selectedResults.remove( descriptor );
    else
      m_selectedResults.add( descriptor );

    // Update text-control
    String text = "";
    for( final IResultModelDescriptor key : m_selectedResults )
    {
      final String modelName = key.getModelName();
      text = text.concat( modelName ).concat( m_selectedResultNames.getLineDelimiter() );
    }
    if( text.length() == 0 )
      m_selectedResultNames.setText( "" );
    else
      m_selectedResultNames.setText( text.substring( 0, text.length() - m_selectedResultNames.getLineDelimiter().length() ) );
  }

  public String getSelectedPath( )
  {
    String text = "";
    for( final IResultModelDescriptor key : m_selectedResults )
    {
      text = text.concat( key.getWorkspacePath() ).concat( ";" );
    }
    if( text.length() == 0 )
      return "";
    else
      return text.substring( 0, text.length() - 1 );
  }

  public IResultModelDescriptor[] getSelectedResults( )
  {
    return m_selectedResults.toArray( new IResultModelDescriptor[m_selectedResults.size()] );
  }

}
