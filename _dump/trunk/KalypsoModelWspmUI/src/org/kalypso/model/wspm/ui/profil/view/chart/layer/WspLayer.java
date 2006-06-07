package org.kalypso.model.wspm.ui.profil.view.chart.layer;

import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.graphics.Region;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.result.IResultSet;
import org.kalypso.model.wspm.core.result.IStationResult;
import org.kalypso.model.wspm.core.result.IResultSet.TYPE;
import org.kalypso.model.wspm.ui.profil.view.IProfilView;
import org.kalypso.model.wspm.ui.profil.view.IProfilViewProvider;
import org.kalypso.model.wspm.ui.profil.view.ProfilViewData;
import org.kalypso.model.wspm.ui.profil.view.panel.WspPanel;

import de.belger.swtchart.EditInfo;
import de.belger.swtchart.axis.AxisRange;

/**
 * Zeigt eine Konstante WSP Linie im Profil
 * 
 * @author gernot
 * 
 */
public class WspLayer extends AbstractProfilChartLayer implements IProfilChartLayer, IStationResult
{
  private final Color m_color;

  private final IProfil m_profil;

  private final IStationResult m_result;

  private double m_height;

  public WspLayer(final IProfilViewProvider pvp, final AxisRange domainRange, final AxisRange valueRange, final Color color, final IStationResult result )
  {
    super(pvp, domainRange, valueRange );

    m_profil = pvp.getProfil();
    m_color = color;
    m_result = result;
    m_height = result.getValue( IResultSet.TYPE.WSP );
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getBounds()
   */
  @Override
  public Rectangle2D getBounds( )
  {
    return new Rectangle2D.Double( Double.NaN, m_height, Double.NaN, 0.0 );
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paint(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  @Override
  public void paint( final GCWrapper gc )
  {
    final Rectangle clipping = gc.getClipping();

    final Point2D point = new Point2D.Double( 0.0, m_height );
    final Point location = logical2screen( point );

    final Region clipreg = new Region();
    final int[] points = getPoints();

    clipreg.add( points );
    clipreg.intersect( clipping );

    final Rectangle toprect = new Rectangle( clipping.x, location.y - 100000, clipping.width, 100000 );
    clipreg.subtract( toprect );

    // if not fill
    if( true )
    {
      final int linesize = 2;
      final Rectangle bottomrect = new Rectangle( clipping.x, location.y + linesize, clipping.width, 10000 );
      clipreg.subtract( bottomrect );
    }

    gc.setClipping( clipreg );

    final Color oldColor = gc.getBackground();
    gc.setBackground( m_color );

    gc.fillRectangle( clipping );

    gc.setBackground( oldColor );
    gc.setClipping( clipping );
  }

  private int[] getPoints( )
  {
    // ermittelt das Polygon oberhalb der geländelinie
    final List<IProfilPoint> ppoints = m_profil.getPoints();
    final List<Point> points = new ArrayList<Point>( (ppoints.size() + 2) * 2 );
    for( int i = 0; i < ppoints.size(); i++ )
    {
      final IProfilPoint p = ppoints.get( i );

      try
      {
        final double x = p.getValueFor( POINT_PROPERTY.BREITE );
        final double y = p.getValueFor( POINT_PROPERTY.HOEHE );

        final Point point = logical2screen( new Point2D.Double( x, y ) );

        if( i == 0 )
          points.add( new Point( point.x, -1000 ) );

        points.add( point );

        if( i == ppoints.size() - 1 )
          points.add( new Point( point.x, -1000 ) );
      }
      catch( Exception e )
      {
        // should never happen
      }
    }

    final int[] ps = new int[points.size() * 2];
    int count = 0;
    for( int i = 0; i < points.size(); i++ )
    {
      final Point p = points.get( i );
      ps[count++] = p.x;
      ps[count++] = p.y;
    }

    return ps;
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paintLegend(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  @Override
  public void paintLegend( final GCWrapper gc )
  {
    final Rectangle clipping = gc.getClipping();
    final int mid = clipping.y + clipping.width / 2;

    gc.setForeground( m_color );
    gc.drawLine( clipping.x, mid, clipping.x + clipping.width, mid );
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    return m_result.getName();
  }

 

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getHoverInfo(org.eclipse.swt.graphics.Point)
   */
  @Override
  public EditInfo getHoverInfo( Point point )
  {
    return null;
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paintDrag(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper,
   *      org.eclipse.swt.graphics.Point, java.lang.Object)
   */
  @Override
  public void paintDrag( final GCWrapper gc, final Point editing, final Object hoverData )
  {
  }

  @Override
  public IProfilView createLayerPanel( final IProfilEventManager pem, final ProfilViewData viewData )
  {
    return new WspPanel( pem, viewData, m_result );
  }

  @Override
  public void removeYourself( )
  {
    throw new UnsupportedOperationException();
  }

  public IStationResult getResult( )
  {
    return m_result;
  }

  public String getName( )
  {
    return m_result.getName();
  }

  public TYPE[] getTypes( )
  {
    return m_result.getTypes();
  }

  public Double getValue( final TYPE type )
  {
    return m_result.getValue( type );
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
