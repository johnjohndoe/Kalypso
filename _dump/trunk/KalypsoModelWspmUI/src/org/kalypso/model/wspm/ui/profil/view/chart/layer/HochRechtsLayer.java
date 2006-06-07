package org.kalypso.model.wspm.ui.profil.view.chart.layer;

import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.profil.view.IProfilView;
import org.kalypso.model.wspm.ui.profil.view.IProfilViewProvider;
import org.kalypso.model.wspm.ui.profil.view.ProfilViewData;

import de.belger.swtchart.EditInfo;
import de.belger.swtchart.axis.AxisRange;

public class HochRechtsLayer extends AbstractProfilChartLayer implements IProfilChartLayer
{
  private IProfilEventManager m_pem;

  private Color m_color;

  public HochRechtsLayer( final IProfilViewProvider  pvp, final AxisRange domainRange, final AxisRange valueRange, final Color color )
  {
    super( pvp,domainRange, valueRange );

    m_pem = pvp.getProfilEventManager() ;
    m_color = color;
  }

  @Override
  public IProfilView createLayerPanel( IProfilEventManager pem, ProfilViewData viewData )
  {
    return null;
  }

  @Override
  public void removeYourself( )
  {
    final IProfilChange[] changes = new IProfilChange[2];
    changes[0] = new PointPropertyRemove( m_pem.getProfil(), POINT_PROPERTY.HOCHWERT );
    changes[1] = new PointPropertyRemove( m_pem.getProfil(), POINT_PROPERTY.RECHTSWERT );

    final ProfilOperation operation = new ProfilOperation( "Datensatz entfernen: " + toString(), m_pem, changes, true );
    new ProfilOperationJob( operation ).schedule();
  }

  @Override
  public Rectangle2D getBounds( )
  {
    try
    {
      final IProfilPoint p = m_pem.getProfil().getPoints().getFirst();
      final double x = p.getValueFor( POINT_PROPERTY.BREITE );
      final double y = p.getValueFor( POINT_PROPERTY.HOEHE );
      final Point2D p2 = new Point2D.Double( x, y );
      return new Rectangle2D.Double( p2.getX(), p2.getY(), 0, 0 );
    }
    catch( ProfilDataException e )
    {
      e.printStackTrace();
      return new Rectangle2D.Double( 0, 0, 0, 0 );

    }

  }

  @Override
  public String toString( )
  {
    return "Geokoordinaten RW/HW";
  }

  @Override
  public EditInfo getHoverInfo( Point point )
  {
    return null;
  }

  @Override
  public void paintDrag( GCWrapper gc, Point editing, Object hoverData )
  {
  }

  @Override
  public void paintLegend( GCWrapper gc )
  {
    final Rectangle clipping = gc.getClipping();

    final int left = clipping.x + 3;
    final int top = clipping.y + 3;
    final int right = clipping.x + clipping.width - 6;
    final int bottom = clipping.y + clipping.width - 6;
    final int midx = (left + right) / 2;
    final int midy = (top + bottom) / 2;
    gc.setLineWidth( 1 );
    gc.setForeground( m_color );
    gc.setLineStyle( SWT.LINE_SOLID );

    gc.drawLine( left, top, left, bottom + 2 );
    gc.drawLine( left - 2, bottom, right, bottom );

    gc.drawOval( midx, midy, 3, 3 );

  }
  

  @Override
  public boolean isNotPainting( )
  {
    return true;
  }

  /**
   * @see com.bce.profil.ui.view.chart.layer.AbstractProfilChartLayer#editProfil(org.eclipse.swt.graphics.Point, java.lang.Object)
   */
  @Override
  protected void editProfil( Point point, Object data )
  {
    // TODO Auto-generated method stub
    
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paint(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  @Override
  public void paint( GCWrapper gc )
  {
    // TODO Auto-generated method stub
    
  }

  /**
   * @see com.bce.eind.core.profil.IProfilListener#onProfilChanged(com.bce.eind.core.profil.changes.ProfilChangeHint, com.bce.eind.core.profil.IProfilChange[])
   */
  @Override
  public void onProfilChanged( ProfilChangeHint hint, IProfilChange[] changes )
  {
    // TODO Auto-generated method stub
    
  }
}
