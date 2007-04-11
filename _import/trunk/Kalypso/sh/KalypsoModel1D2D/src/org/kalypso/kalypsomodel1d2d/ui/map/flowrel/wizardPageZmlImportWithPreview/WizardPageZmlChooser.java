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
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.widgets.Composite;
import org.jfree.chart.title.TextTitle;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.DiagView;
import org.kalypso.ogc.sensor.diagview.jfreechart.ChartFactory;
import org.kalypso.ogc.sensor.diagview.jfreechart.ObservationChart;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.template.ObsView;
import org.kalypso.ogc.sensor.template.ObsViewUtils;
import org.kalypso.ogc.sensor.template.PlainObsProvider;
import org.kalypso.ogc.sensor.zml.ZmlFactory;

public class WizardPageZmlChooser extends WizardPage
{
  protected DiagView m_diagView;

  protected TextTitle m_subTitle;
  
  private String m_parentFolder = "";

  public WizardPageZmlChooser( String pageName )
  {
    super( pageName );
  }

  public void init( String parentFolder)
  {
    m_parentFolder = parentFolder;
  }

  public void createControl( Composite parent )
  {
    SashForm sash_form = new SashForm( parent, SWT.HORIZONTAL | SWT.NULL );

    TreeViewer tv = new TreeViewer( sash_form );
    tv.setContentProvider( new FileTreeContentProvider() );
    tv.setLabelProvider( new FileTreeLabelProvider() );
    tv.setInput( new File( m_parentFolder ) );

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
    final Frame vFrame = SWT_AWT.new_Frame( new Composite( sash_form, SWT.RIGHT | SWT.BORDER | SWT.EMBEDDED | SWT.Paint) );
    
    vFrame.add( ChartFactory.createChartPanel( m_chart ) );

    tv.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        // always remove items first (we don't know which selection we get)
        m_diagView.removeAllItems();

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
          final DateRange dra = new DateRange( new Date( 1, 1, 1 ), null );

          m_diagView.addObservation( new PlainObsProvider( obs, new ObservationRequest( dra ) ), ObsViewUtils.DEFAULT_ITEM_NAME, new ObsView.ItemData( false, null, null ) );

          // sub title of diagram contains date-range info
          // if (dra != null) m_subTitle.setText(dra.toString());
        }
      }
    } );
    setControl( sash_form );

    parent.pack();
    parent.layout();
  }
}
