package org.kalypso.kalypsomodel1d2d.ui.map.flowrel.wizardPageZmlImportWithPreview;

import java.awt.Font;
import java.awt.Frame;
import java.io.File;
import java.net.MalformedURLException;
import java.util.Date;

import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.widgets.Composite;
import org.jfree.chart.title.TextTitle;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.DiagView;
import org.kalypso.ogc.sensor.diagview.jfreechart.ChartFactory;
import org.kalypso.ogc.sensor.diagview.jfreechart.ObservationChart;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.template.ObsView;
import org.kalypso.ogc.sensor.template.ObsViewUtils;
import org.kalypso.ogc.sensor.template.PlainObsProvider;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.util.swtcalendar.SWTCalendarDialog;

public class WizardPageZmlChooser extends WizardPage
{
  public class ZmlFilter extends ViewerFilter
  {

    public ZmlFilter( )
    {
      super();
    }

    @Override
    public boolean select( Viewer viewer, Object parentElement, Object element )
    {
      if( element instanceof File )
        return ((File) element).getName().endsWith( ".zml" );
      return true;
    }
  }

  protected DiagView m_diagView;

  protected TextTitle m_subTitle;

  private String m_parentFolder = "";

  public WizardPageZmlChooser( String title )
  {
    super( title );
  }

  public WizardPageZmlChooser( )
  {
    super( "Super title" );
    setTitle( "Some title that should be changed." );
    setDescription( "Some subtitle that should be changed." );
  }

  public void init( String parentFolder )
  {
    m_parentFolder = parentFolder;
    setPageComplete( false );
  }

  public void createControl( Composite parent )
  {
    SashForm sash_form = new SashForm( parent, SWT.HORIZONTAL | SWT.NULL );

    TreeViewer tv = new TreeViewer( sash_form );
    tv.setContentProvider( new FileTreeContentProvider() );
    tv.setLabelProvider( new FileTreeLabelProvider() );
    tv.setInput( new File( m_parentFolder ) );
    tv.addFilter( new ZmlFilter() );
    m_diagView = new DiagView( true );
    ObservationChart m_chart = null;
    try
    {
      m_chart = new ObservationChart( m_diagView );
    }
    catch( SensorException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    m_subTitle = new TextTitle( "", new Font( "Default", Font.PLAIN, 12 ) );
    m_chart.addSubtitle( m_subTitle );
    final Frame vFrame = SWT_AWT.new_Frame( new Composite( sash_form, SWT.RIGHT | SWT.BORDER | SWT.EMBEDDED | SWT.Paint ) );

    vFrame.add( ChartFactory.createChartPanel( m_chart ) );

    tv.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        // always remove items first (we don't know which selection we get)
        m_diagView.removeAllItems();

        setPageComplete( false );

        final StructuredSelection selection = (StructuredSelection) event.getSelection();

        File selectedFile = (File) selection.getFirstElement();

        IObservation obs = null;

        try
        {
          obs = ZmlFactory.parseXML( selectedFile.toURL(), selectedFile.getAbsolutePath() );
        }
        catch( MalformedURLException e )
        {
        }
        catch( SensorException e )
        {
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }

        m_subTitle.setText( "" );
        if( obs != null )
        {
          DateRange dateRange = null;
          try
          {
            final ITuppleModel values = obs.getValues( null );
            final IAxis[] axisList = values.getAxisList();
            final IAxis axis = ObservationUtilities.findAxisByType( axisList, "date" );
            Date minDate = (Date) values.getElement( 0, axis );
            Date maxDate = (Date) values.getElement( values.getCount() - 1, axis );
            SWTCalendarDialog calendarDialog = new SWTCalendarDialog(getShell(), minDate);
            calendarDialog.open();
            dateRange = new DateRange( minDate, maxDate );
          }
          catch( SensorException e )
          {
            e.printStackTrace();
            dateRange = new DateRange( new Date( 1, 1, 1 ), null );
          }
          m_subTitle.setText(dateRange.toString());
          m_diagView.addObservation( new PlainObsProvider( obs, new ObservationRequest( dateRange ) ), ObsViewUtils.DEFAULT_ITEM_NAME, new ObsView.ItemData( false, null, null ) );
          setPageComplete( true );
          
        }
      }
    } );
    sash_form.getShell().setMinimumSize( 400, 300 );
    setControl( sash_form );

    parent.pack();
    parent.layout();
  }

}
