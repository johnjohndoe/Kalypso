package org.kalypso.kalypso1d2d.pjt.wizards;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
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

  private Map<String, String> m_selectedResultPaths; // map that contains <path, name> pairs of the result files

  private final String m_initialSelection;

  private final ViewerFilter m_resultFilter;

  public static class ResultFilter extends ViewerFilter
  {
    @Override
    public boolean select( final Viewer viewer, final Object parentElement, final Object element )
    {
      return true;
      // if( element instanceof File )
      // return ((File) element).getName().endsWith( ".gml" );
      // return true;
    }
  }

  public RestartSelectWizardPage( final String initialSelection )
  {
    this( initialSelection, "Example", new ResultFilter() );
  }

  public RestartSelectWizardPage( final String initialSelection, final String pageName, final ViewerFilter resultFilter )
  {
    super( pageName );

    m_initialSelection = initialSelection;
    m_resultFilter = resultFilter;
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

    allResultsViewer.addFilter( m_resultFilter );

    m_selectedResultPaths = new HashMap<String, String>();
    m_selectedResultNames = new Text( innerComposite, SWT.MULTI | SWT.BORDER );
    m_selectedResultNames.setEditable( false );
    m_selectedResultNames.setText( "" );

    loadInitialSelection();

    allResultsViewer.addDoubleClickListener( new IDoubleClickListener()
    {

      public void doubleClick( final DoubleClickEvent event )
      {
        final StructuredSelection selection = (StructuredSelection) event.getSelection();
        final Object firstElement = selection.getFirstElement();
        if( firstElement instanceof IResultModelDescriptor )
          setSelection( (IResultModelDescriptor) firstElement, false );
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
            setSelection( resultModel.get( i ), true );
      }
    }
  }

  private void setSelection( final IResultModelDescriptor descriptor, final boolean initial )
  {
    final String path = descriptor.getWorkspacePath();
    final String value = descriptor.getModelName();
    if( m_selectedResultPaths.containsKey( path ) )
    {
      if( initial )
        return;
      m_selectedResultPaths.remove( path );
    }
    else
      m_selectedResultPaths.put( path, value );

    String text = "";
    for( final String key : m_selectedResultPaths.keySet() )
    {
      text = text.concat( m_selectedResultPaths.get( key ) ).concat( m_selectedResultNames.getLineDelimiter() );
    }
    if( text.length() == 0 )
      m_selectedResultNames.setText( "" );
    else
      m_selectedResultNames.setText( text.substring( 0, text.length() - m_selectedResultNames.getLineDelimiter().length() ) );
  }

  public String getSelectedPath( )
  {
    String text = "";
    for( final String key : m_selectedResultPaths.keySet() )
    {
      text = text.concat( key ).concat( ";" );
    }
    if( text.length() == 0 )
      return "";
    else
      return text.substring( 0, text.length() - 1 );
  }

}
