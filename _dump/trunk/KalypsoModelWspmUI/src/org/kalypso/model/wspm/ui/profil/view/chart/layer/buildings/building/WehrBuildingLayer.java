/**
 * 
 */
package org.kalypso.model.wspm.ui.profil.view.chart.layer.buildings.building;

import java.util.Arrays;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_TYP;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.changes.BuildingSet;
import org.kalypso.model.wspm.core.profil.changes.DeviderRemove;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.profil.view.IProfilView;
import org.kalypso.model.wspm.ui.profil.view.ProfilViewData;
import org.kalypso.model.wspm.ui.profil.view.chart.ProfilChartView;
import org.kalypso.model.wspm.ui.profil.view.chart.layer.AbstractPolyLineLayer;
import org.kalypso.model.wspm.ui.profil.view.panel.WehrPanel;

import de.belger.swtchart.axis.AxisRange;

/**
 * @author kimwerner
 */
public class WehrBuildingLayer extends AbstractPolyLineLayer
{
  @Override
  public List<IProfilPoint> getPoints( )
  {
    return ProfilUtil.getInnerPoints( getProfil(), DEVIDER_TYP.TRENNFLAECHE );
  }

  public WehrBuildingLayer( final ProfilChartView pvp, final AxisRange domainRange, final AxisRange valueRange, final List<Color> colors, final Color selectedcolor, final Color stationColor, final Color editColor )
  {
    super( pvp, domainRange, valueRange, colors, selectedcolor, stationColor, editColor, Arrays.asList( POINT_PROPERTY.OBERKANTEWEHR ), false, false, false );
  }

  @Override
  public String toString( )
  {
    return "Wehr";
  }

  @Override
  public void paintLegend( GCWrapper gc )
  {
    final Rectangle clipping = gc.getClipping();

    final int left = clipping.x;
    final int top = clipping.y;
    final int right = clipping.x + clipping.width;
    final int bottom = clipping.y + clipping.width;
    final int midx = (left + right) / 2;
    final int midy = (top + bottom) / 2;

    drawStationline( gc, midx, midy, midx, bottom );
    gc.setLineWidth( 1 );
    gc.setLineStyle( SWT.LINE_SOLID );
    gc.setForeground( m_colors.get( 0 ) );
    gc.drawOval( midx - 2, midy - 2, 4, 4 );
    gc.drawLine( left, top, midx, midy );
    gc.drawLine( midx, midy, right, midy );

  }

  @Override
  public IProfilView createLayerPanel( final IProfilEventManager pem, final ProfilViewData viewData )
  {
    return new WehrPanel( pem, viewData );
  }

  public void removeYourself( )
  {
    final IProfilEventManager pem = getProfilEventManager();
    final IProfilDevider[] deviders = pem.getProfil().getDevider( DEVIDER_TYP.WEHR );
    final int deviderCount = deviders == null ? 0 : deviders.length;
    final IProfilChange[] changes = new IProfilChange[deviderCount + 1];
    changes[0] = new BuildingSet( pem.getProfil(), null );
    for( int i = 0; i < deviderCount; i++ )
    {
      changes[i + 1] = new DeviderRemove( pem.getProfil(), deviders[i] );
    }
    final ProfilOperation operation = new ProfilOperation( "Wehr entfernen", pem, changes, true );
    new ProfilOperationJob( operation ).schedule();
  }

  /**
   * @see com.bce.eind.core.profil.IProfilListener#onProfilChanged(com.bce.eind.core.profil.changes.ProfilChangeHint,
   *      com.bce.eind.core.profil.IProfilChange[])
   */
  @Override
  public void onProfilChanged( ProfilChangeHint hint, IProfilChange[] changes )
  {
  }
}
