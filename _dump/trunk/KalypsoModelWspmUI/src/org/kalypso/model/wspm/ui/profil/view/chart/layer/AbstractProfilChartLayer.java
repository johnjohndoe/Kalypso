package org.kalypso.model.wspm.ui.profil.view.chart.layer;

import java.awt.geom.Rectangle2D;

import org.eclipse.swt.graphics.Point;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.ui.profil.view.IProfilView;
import org.kalypso.model.wspm.ui.profil.view.IProfilViewProvider;
import org.kalypso.model.wspm.ui.profil.view.ProfilViewData;

import de.belger.swtchart.EditInfo;
import de.belger.swtchart.axis.AxisRange;
import de.belger.swtchart.layer.AbstractChartLayer;

public abstract class AbstractProfilChartLayer extends AbstractChartLayer implements IProfilChartLayer
{
  private final IProfilEventManager m_pem;

  private final ProfilViewData m_viewData;
  

  public AbstractProfilChartLayer( final IProfilViewProvider pvp, final AxisRange domainRange, final AxisRange valueRange )
  {
    super( domainRange, valueRange );

    m_pem = pvp.getProfilEventManager();
    m_viewData = pvp.getViewData();
  }

  public final void edit( final Point point, final Object data )
  {

    editProfil( point, data );

    m_viewData.setActiveLayer( this );
  }

  /**
   * @return Returns the profil.
   */
  protected final IProfil getProfil( )
  {
    return m_pem.getProfil();
  }

  public IProfilEventManager getProfilEventManager( )
  {
    return m_pem;
  }

  /**
   * @return Returns the viewData.
   */
  protected final ProfilViewData getViewData( )
  {
    return m_viewData;
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
   * @see org.kalypso.model.wspm.ui.profil.view.chart.layer.IProfilChartLayer#removeYourself()
   */
  @SuppressWarnings("unused")
  public void removeYourself( ) throws ProfilDataException
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getBounds()
   */
  public Rectangle2D getBounds( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getHoverInfo(org.eclipse.swt.graphics.Point)
   */
  public EditInfo getHoverInfo( Point point )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#onProfilChanged(org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint,
   *      org.kalypso.model.wspm.core.profil.IProfilChange[])
   */
  public void onProfilChanged( ProfilChangeHint hint, IProfilChange[] changes )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paint(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  public void paint( GCWrapper gc )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paintDrag(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper,
   *      org.eclipse.swt.graphics.Point, java.lang.Object)
   */
  public void paintDrag( GCWrapper gc, Point editing, Object hoverData )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paintLegend(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  public void paintLegend( GCWrapper gc )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#setActivePoint(org.eclipse.swt.graphics.Point)
   */
  public void setActivePoint( final Object data )
  {
    // TODO Auto-generated method stub
  }
}
