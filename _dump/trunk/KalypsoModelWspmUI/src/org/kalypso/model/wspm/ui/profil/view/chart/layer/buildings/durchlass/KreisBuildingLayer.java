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
import org.kalypso.model.wspm.ui.profil.view.IProfilViewProvider;
import org.kalypso.model.wspm.ui.profil.view.chart.layer.buildings.AbstractBuildingLayer;

import de.belger.swtchart.axis.AxisRange;

/**
 * @author kimwerner
 * 
 */
public class KreisBuildingLayer extends AbstractBuildingLayer
{
  public KreisBuildingLayer( final IProfilViewProvider pvp,
      final AxisRange domainRange, final AxisRange valueRange, final Color color )
  {
    super( pvp, domainRange, valueRange, color );
  }
 

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getBounds()
   */
  @Override
  public Rectangle2D getBounds( )
  {
    try
    {
      return createOval();
    }
    catch( Exception e )
    {
      return new Rectangle2D.Double( Double.NaN, Double.NaN, Double.NaN,
          Double.NaN );
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
    gc.fillOval( clipping.x + 2, clipping.y + 2, clipping.width - 4, clipping.height - 4 );
    gc.drawOval( clipping.x + 2, clipping.y + 2, clipping.width - 4, clipping.height - 4 );
    
    gc.setBackground( background );
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paint(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  @Override
  public void paint( final GCWrapper gc )
  {
    try
    {
      final Color background = gc.getBackground();
      gc.setBackground( getColor() );

      final Rectangle2D oval = createOval();

      final Rectangle ovalScreen = logical2screen( oval );
      gc.fillOval( ovalScreen.x, ovalScreen.y, ovalScreen.width,
          ovalScreen.height );
  //    gc.drawOval( ovalScreen.x, ovalScreen.y, ovalScreen.width,
  //        ovalScreen.height );

      gc.setBackground( background );
    }
    catch( final ProfilDataException e )
    {
      // sollte nie passieren
      e.printStackTrace();
      throw new IllegalStateException( e );
    }
  }

  private Rectangle2D createOval( ) throws ProfilDataException
  {
    final IProfilBuilding building = getBuilding();
    final double bezX = (Double)building
        .getValueFor( BUILDING_PROPERTY.BEZUGSPUNKT_X );
    final double bezY = (Double)building
        .getValueFor( BUILDING_PROPERTY.BEZUGSPUNKT_Y );
    final double durchmesser = (Double)building
        .getValueFor( BUILDING_PROPERTY.BREITE );
    final Point2D topLeft = new Point2D.Double( bezX - durchmesser / 2, bezY);
    final double w = durchmesser;
    final double h = durchmesser;
    final Rectangle2D oval = new Rectangle2D.Double( topLeft.getX(), topLeft
        .getY(), w, h );
    return oval;
  }
  @Override
  public String toString( )
  {
    return "Kreis";
  }


  /**
   * @see com.bce.profil.ui.view.chart.layer.AbstractProfilChartLayer#editProfil(org.eclipse.swt.graphics.Point, java.lang.Object)
   */
  @Override
  protected void editProfil( Point point, Object data )
  {
    
  }


  /**
   * @see com.bce.eind.core.profil.IProfilListener#onProfilChanged(com.bce.eind.core.profil.changes.ProfilChangeHint, com.bce.eind.core.profil.IProfilChange[])
   */
  @Override
  public void onProfilChanged( ProfilChangeHint hint, IProfilChange[] changes )
  {
    
  }
 
}
