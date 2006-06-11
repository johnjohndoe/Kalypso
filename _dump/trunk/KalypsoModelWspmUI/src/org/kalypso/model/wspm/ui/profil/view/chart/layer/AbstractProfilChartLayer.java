package org.kalypso.model.wspm.ui.profil.view.chart.layer;

import org.eclipse.swt.graphics.Point;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.ui.profil.view.IProfilView;
import org.kalypso.model.wspm.ui.profil.view.ProfilViewData;
import org.kalypso.model.wspm.ui.profil.view.chart.ProfilChartView;

import de.belger.swtchart.EditInfo;
import de.belger.swtchart.axis.AxisRange;
import de.belger.swtchart.layer.AbstractChartLayer;

public abstract class AbstractProfilChartLayer extends AbstractChartLayer implements IProfilChartLayer
{
  private final ProfilChartView m_chartView;

  public AbstractProfilChartLayer( final ProfilChartView chartView, final AxisRange domainRange, final AxisRange valueRange )
  {
    super( domainRange, valueRange );

    m_chartView = chartView;
  }

  public final void edit( final Point point, final Object data )
  {
    editProfil( point, data );

    getViewData().setActiveLayer( this );
  }

  /**
   * @return Returns the profil.
   */
  protected final IProfil getProfil( )
  {
    return getProfilEventManager().getProfil();
  }

  public IProfilEventManager getProfilEventManager( )
  {
    return m_chartView.getProfilEventManager();
  }

  /**
   * @return Returns the viewData.
   */
  protected final ProfilViewData getViewData( )
  {
    return m_chartView.getViewData();
  }

  protected abstract void editProfil( Point point, Object data );

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.chart.layer.IProfilChartLayer#createLayerPanel(org.kalypso.model.wspm.core.profil.IProfilEventManager,
   *      org.kalypso.model.wspm.ui.profil.view.ProfilViewData)
   */
  public IProfilView createLayerPanel( IProfilEventManager pem, ProfilViewData viewData )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getHoverInfo(org.eclipse.swt.graphics.Point)
   */
  public EditInfo getHoverInfo( Point point )
  {
    return null;
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#onProfilChanged(org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint,
   *      org.kalypso.model.wspm.core.profil.IProfilChange[])
   */
  public void onProfilChanged( ProfilChangeHint hint, IProfilChange[] changes )
  {
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paintDrag(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper,
   *      org.eclipse.swt.graphics.Point, java.lang.Object)
   */
  public void paintDrag( GCWrapper gc, Point editing, Object hoverData )
  {
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paintLegend(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  public void paintLegend( GCWrapper gc )
  {
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#setActivePoint(java.lang.Object)
   */
  public void setActivePoint( Object data )
  {
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.chart.layer.IProfilChartLayer#getProfilChartView()
   */
  public ProfilChartView getProfilChartView( )
  {
    return m_chartView;
  }
}
