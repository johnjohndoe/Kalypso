/**
 * 
 */
package org.kalypso.model.wspm.ui.profil.view.chart.layer.buildings;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.model.wspm.core.profil.IProfilBuilding;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.changes.BuildingSet;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.profil.view.IProfilView;
import org.kalypso.model.wspm.ui.profil.view.ProfilViewData;
import org.kalypso.model.wspm.ui.profil.view.chart.ProfilChartView;
import org.kalypso.model.wspm.ui.profil.view.chart.layer.AbstractProfilChartLayer;
import org.kalypso.model.wspm.ui.profil.view.panel.BuildingPanel;

import de.belger.swtchart.EditInfo;
import de.belger.swtchart.axis.AxisRange;

/**
 * @author kimwerner
 */
public abstract class AbstractBuildingLayer extends AbstractProfilChartLayer
{
  private final Color m_color;

  public AbstractBuildingLayer( final ProfilChartView pvp, final AxisRange domainRange, final AxisRange valueRange, final Color color )
  {
    super( pvp, domainRange, valueRange );

    m_color = color;
  }

  protected final Color getColor( )
  {
    return m_color;
  }

  protected final IProfilBuilding getBuilding( )
  {
    return getProfil().getBuilding();
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paintDrag(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper,
   *      org.eclipse.swt.graphics.Point, java.lang.Object)
   */
  @Override
  public final void paintDrag( final GCWrapper gc, final Point editing, final Object hoverData )
  {
    // da nicht editiert wird, nichts zeichnen
  }

  @Override
  public IProfilView createLayerPanel( final IProfilEventManager pem, final ProfilViewData viewData )
  {
    return new BuildingPanel( pem, viewData );
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getHoverInfo(org.eclipse.swt.graphics.Point)
   */
  @Override
  public final EditInfo getHoverInfo( final Point point )
  {
    // immer null, weil nicht editiert wird
    return null;
  }

  public void removeYourself( )
  {
    final IProfilChange pc = new BuildingSet( getProfil(), null );
    final ProfilOperation operation = new ProfilOperation( "Bauwerk entfernen", getProfilEventManager(), pc, true );
    new ProfilOperationJob( operation ).schedule();
  }

}
