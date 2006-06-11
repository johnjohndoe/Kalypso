/**
 * 
 */
package org.kalypso.model.wspm.ui.profil.view.chart.layer.buildings.durchlass;

import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.model.wspm.core.profil.IProfilBuilding;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilBuilding.BUILDING_PROPERTY;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.ui.profil.view.chart.ProfilChartView;
import org.kalypso.model.wspm.ui.profil.view.chart.layer.buildings.AbstractBuildingLayer;

import de.belger.swtchart.axis.AxisRange;

/**
 * @author kimwerner
 */
public class TrapezBuildingLayer extends AbstractBuildingLayer
{
  public TrapezBuildingLayer( final ProfilChartView pvp, final AxisRange domainRange, final AxisRange valueRange, final Color color )
  {
    super( pvp, domainRange, valueRange, color );
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getBounds()
   */
  public Rectangle2D getBounds( )
  {
    try
    {
      final double[] trapezArray = createTrapez();
      return new Rectangle2D.Double( trapezArray[6], trapezArray[1], trapezArray[4] - trapezArray[6], trapezArray[5] - trapezArray[3] );
    }
    catch( Exception e )
    {
      return new Rectangle2D.Double( Double.NaN, Double.NaN, Double.NaN, Double.NaN );
    }
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paintLegend(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  @Override
  public void paintLegend( final GCWrapper gc )
  {
    final Rectangle clipping = gc.getClipping();

    final Color background = gc.getBackground();

    gc.setBackground( getColor() );
    final int[] trapez = { clipping.x + 6, clipping.height - 2, clipping.width - 6, clipping.height - 2, clipping.width - 2, clipping.y + 2, clipping.x + 2, clipping.y + 2 };
    gc.fillPolygon( trapez );
    gc.drawPolygon( trapez );
    gc.setBackground( background );
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paint(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  public void paint( final GCWrapper gc )
  {
    try
    {
      final Color background = gc.getBackground();
      gc.setBackground( getColor() );
      
      final double[] trapezArray = createTrapez();
      final Rectangle2D trapezRect = new Rectangle2D.Double( trapezArray[0], trapezArray[1], trapezArray[2] - trapezArray[0], trapezArray[5] - trapezArray[1] );
      final Rectangle trapezScreen = logical2screen( trapezRect );
      final Point2D topLeft = new Point2D.Double( trapezArray[6], trapezArray[7] );
      final Point2D topRight = new Point2D.Double( trapezArray[4], trapezArray[5] );
      final Point topLeftScreen = logical2screen( topLeft );
      final Point topRightScreen = logical2screen( topRight );
      final int[] trapezScreenArray = new int[8];
      trapezScreenArray[0] = trapezScreen.x;
      trapezScreenArray[1] = trapezScreen.y;

      trapezScreenArray[2] = trapezScreen.x + trapezScreen.width;
      trapezScreenArray[3] = trapezScreen.y;

      trapezScreenArray[4] = topRightScreen.x;
      trapezScreenArray[5] = topRightScreen.y;

      trapezScreenArray[6] = topLeftScreen.x;
      trapezScreenArray[7] = topLeftScreen.y;

      gc.fillPolygon( trapezScreenArray );
 
      gc.setBackground( background );
    }
    catch( final ProfilDataException e )
    {
      // sollte nie passieren
      e.printStackTrace();
      throw new IllegalStateException( e );
    }
  }

  private double[] createTrapez( ) throws ProfilDataException
  {
    final IProfilBuilding building = getBuilding();
    final double bezX = (Double) building.getValueFor( BUILDING_PROPERTY.BEZUGSPUNKT_X );
    final double bezY = (Double) building.getValueFor( BUILDING_PROPERTY.BEZUGSPUNKT_Y );
    final double lang = (Double) building.getValueFor( BUILDING_PROPERTY.BREITE );
    final double hoch = (Double) building.getValueFor( BUILDING_PROPERTY.HOEHE );
    final double dx = hoch * ((Double) building.getValueFor( BUILDING_PROPERTY.STEIGUNG ) / 100);

    final double[] trapezArray = new double[8];
    trapezArray[0] = bezX - (lang / 2 - dx);
    trapezArray[1] = bezY;

    trapezArray[2] = bezX + (lang / 2 - dx);
    trapezArray[3] = bezY;

    trapezArray[4] = bezX + lang / 2;
    trapezArray[5] = bezY + hoch;

    trapezArray[6] = bezX - lang / 2;
    trapezArray[7] = bezY + hoch;

    return trapezArray;
  }

  @Override
  public String toString( )
  {
    return "Trapez";
  }

  /**
   * @see com.bce.profil.ui.view.chart.layer.AbstractProfilChartLayer#editProfil(org.eclipse.swt.graphics.Point,
   *      java.lang.Object)
   */
  @Override
  protected void editProfil( Point point, Object data )
  {

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
