package org.kalypso.model.wspm.ui.view;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.contribs.eclipse.swt.custom.ScrolledCompositeCreator;
import org.kalypso.contribs.eclipse.ui.partlistener.AdapterPartListener;
import org.kalypso.contribs.eclipse.ui.partlistener.EditorFirstAdapterFinder;
import org.kalypso.contribs.eclipse.ui.partlistener.IAdapterEater;
import org.kalypso.model.wspm.ui.profil.view.IProfilView;
import org.kalypso.model.wspm.ui.profil.view.chart.ProfilChartView;
import org.kalypso.model.wspm.ui.profil.view.chart.layer.IProfilChartLayer;

import de.belger.swtchart.layer.IActiveLayerChangeListener;
import de.belger.swtchart.layer.IActiveLayerProvider;
import de.belger.swtchart.layer.IChartLayer;

/**
 * @author Gernot Belger
 */
public class LayerView extends ViewPart implements IAdapterEater, IActiveLayerChangeListener
{
  private final ScrolledCompositeCreator m_creator = new ScrolledCompositeCreator( null )
  {
    @Override
    protected Control createContents( final Composite sc, final int style )
    {
      final Group group = new Group( sc, style );
      group.setLayoutData( new GridData( GridData.FILL_BOTH ) );
      group.setLayout( new GridLayout() );

      return group;
    }
  };

  private final AdapterPartListener m_providerListener = new AdapterPartListener( IActiveLayerProvider.class, this, EditorFirstAdapterFinder.instance(), EditorFirstAdapterFinder.instance() );

  private IActiveLayerProvider m_provider;

  private IProfilChartLayer m_activeLayer;

  /**
   * @see org.eclipse.ui.part.ViewPart#init(org.eclipse.ui.IViewSite)
   */
  @Override
  public void init( IViewSite site ) throws PartInitException
  {
    super.init( site );

    m_providerListener.init( site.getPage() );
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#dispose()
   */
  @Override
  public void dispose( )
  {
    unhookProvider();

    m_providerListener.dispose();

    super.dispose();
  }

  private void unhookProvider( )
  {
    if( m_provider != null )
    {
      m_provider.removeActiveLayerChangeListener( this );
      m_provider = null;
    }
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#setFocus()
   */
  @Override
  public void setFocus( )
  {
    final Control control = m_creator.getContentControl();
    if( control != null )
      control.setFocus();
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    m_creator.createControl( parent, SWT.NONE, SWT.NONE );

    onActiveLayerChanged( m_provider == null ? null : m_provider.getActiveLayer() );
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.partlistener.IAdapterEater#setAdapter(java.lang.Object)
   */
  public void setAdapter( final Object adapter )
  {
    final IActiveLayerProvider provider = (IActiveLayerProvider) adapter;

    if( m_provider == provider && provider != null )
      return;

    unhookProvider();

    m_provider = provider;

    if( m_provider != null )
      m_provider.addActiveLayerChangeListener( this );

    onActiveLayerChanged( m_provider == null ? null : m_provider.getActiveLayer() );
  }

  /**
   * @see de.belger.swtchart.layer.IActiveLayerChangeListener#onActiveLayerChanged(de.belger.swtchart.layer.IChartLayer)
   */
  public void onActiveLayerChanged( final IChartLayer activeLayer )
  {
    final IProfilChartLayer profilLayer = activeLayer instanceof IProfilChartLayer ? (IProfilChartLayer) activeLayer : null;
    if( m_activeLayer == profilLayer && profilLayer != null )
      return;
    final Group group = (Group) m_creator.getContentControl();
    if( group == null || group.isDisposed() )
      return;

    for( final Control c : group.getChildren() )
    {
      if( !c.isDisposed() )
        c.dispose();
    }
    group.setText( "" );

    if( profilLayer != null )
    {
      group.setText( profilLayer.toString() );

      final ProfilChartView profilChartView = profilLayer.getProfilChartView();
      final IProfilView panel = profilLayer.createLayerPanel( profilChartView.getProfilEventManager(), profilChartView.getViewData() );

      if( panel != null )
      {
        final Control control = panel.createControl( group, SWT.NONE );
        control.setLayoutData( new GridData( GridData.FILL_BOTH ) );
      }
      else
        new Label( group, SWT.NONE | SWT.WRAP );

      m_activeLayer = profilLayer;
    }
    else
    {
      // alles gescheitert -> Meldung
      final Label label = new Label( group, SWT.CENTER );
      final GridData gridData = new GridData(  );
      gridData.grabExcessHorizontalSpace = true;
      gridData.horizontalAlignment = SWT.FILL;
      gridData.horizontalIndent = 10;
      gridData.grabExcessVerticalSpace = true;
      gridData.verticalAlignment = SWT.CENTER;
      gridData.verticalIndent = 10;

      label.setLayoutData( gridData );
      label.setText( "Keine Legende vorhanden" );
    }

    group.layout();
    m_creator.updateControlSize( false );
  }

}
