package org.kalypso.model.wspm.ui.profil.view.chart.layer;

import java.awt.geom.Rectangle2D;
import java.util.Iterator;
import java.util.List;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfil.PROFIL_PROPERTY;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_PROPERTY;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_TYP;
import org.kalypso.model.wspm.core.profil.IProfilPoint.PARAMETER;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.ui.profil.view.IProfilView;
import org.kalypso.model.wspm.ui.profil.view.ProfilViewData;
import org.kalypso.model.wspm.ui.profil.view.chart.ProfilChartView;
import org.kalypso.model.wspm.ui.profil.view.panel.RauheitenPanel;

import de.belger.swtchart.EditInfo;
import de.belger.swtchart.axis.AxisRange;
import de.belger.swtchart.util.LogicalRange;

/**
 * @author Gernot Belger
 */
public class RauheitLayer extends AbstractProfilChartLayer implements IProfilChartLayer
{
  private final Color m_color;

  private final IProfil m_profil;

  private final Color m_fillColor;

  public RauheitLayer( final ProfilChartView pvp, final AxisRange domainRange, final AxisRange valueRange, final Color color, final Color fillColor )
  {
    super( pvp, domainRange, valueRange );

    m_profil = pvp.getProfil();
    m_color = color;
    m_fillColor = fillColor;
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getBounds()
   */
  public Rectangle2D getBounds( )
  {
    final List<IProfilPoint> points = m_profil.getPoints();

    Rectangle2D bounds = null;

    try
    {
      for( final IProfilPoint p : points )
      {
        final double x = p.getValueFor( POINT_PROPERTY.BREITE );

        final double rauheit = p.getValueFor( POINT_PROPERTY.RAUHEIT );
        final Rectangle2D area = new Rectangle2D.Double( x, 0.0, 0.0, rauheit );

        if( bounds == null )
          bounds = area;
        else
          bounds.add( area );
      }
    }
    catch( final Exception e )
    {
      // should never happen
      e.printStackTrace();
    }

    return bounds;
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paint(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  public void paint( final GCWrapper gc )
  {
    final Color background = gc.getBackground();
    // final int alpha = gc.getAlpha();
    if( !(Boolean) POINT_PROPERTY.RAUHEIT.getParameter( PARAMETER.VISIBLE ) )
    {
      drawBlock( gc );

    }
    else
    {
      drawDetail( gc );
    }
    // gc.setAlpha( alpha );
    gc.setBackground( background );
  }

  private void drawDetail( final GCWrapper gc )
  {
    try
    {

      final List<IProfilPoint> points = m_profil.getPoints();

      IProfilPoint lastP = null;
      for( final Iterator<IProfilPoint> pIt = points.iterator(); pIt.hasNext(); )
      {
        final IProfilPoint p = pIt.next();

        if( lastP != null )
        {
          final double x1 = lastP.getValueFor( POINT_PROPERTY.BREITE );
          final double x2 = p.getValueFor( POINT_PROPERTY.BREITE );

          final double y1 = 0;
          final double y2 = lastP.getValueFor( POINT_PROPERTY.RAUHEIT );

          final Rectangle box = logical2screen( new Rectangle2D.Double( x1, y1, x2 - x1, y2 - y1 ) );
          box.width += 1;
          fillRectangle( gc, box );
        }

        lastP = p;
      }
    }
    catch( final Exception e )
    {
      // should never happen

      e.printStackTrace();
    }

  }

  private void drawBlock( final GCWrapper gc )
  {

    final IProfilDevider[] deviders = m_profil.getDevider( new DEVIDER_TYP[] { DEVIDER_TYP.TRENNFLAECHE, DEVIDER_TYP.DURCHSTROEMTE } );

    try
    {
      for( int i = 0; i < deviders.length - 1; i++ )
      {
        final IProfilPoint firstP = deviders[i].getPoint();
        final IProfilPoint lastP = deviders[i + 1].getPoint();
        final Double value = (Double) deviders[i].getValueFor( DEVIDER_PROPERTY.RAUHEIT );
        if( value == null )
          break;
        final double x1 = firstP.getValueFor( POINT_PROPERTY.BREITE );
        final double x2 = lastP.getValueFor( POINT_PROPERTY.BREITE );

        final double y1 = 0;
        final double y2 = value;

        final Rectangle box = logical2screen( new Rectangle2D.Double( x1, y1, x2 - x1, y2 - y1 ) );
        box.width += 1;
        fillRectangle( gc, box );
      }
    }
    catch( ProfilDataException e )
    {
      // should never happen
    }

  }

  private void fillRectangle( final GCWrapper gc, final Rectangle box )
  {
    // gc.setAlpha( 50 );
    gc.setForeground( m_color );
    gc.setBackground( m_fillColor );
    gc.fillRectangle( box );
    // gc.drawRectangle( box );
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getHoverInfo(org.eclipse.swt.graphics.Point)
   */
  @Override
  public EditInfo getHoverInfo( final Point point )
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

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    final Object rw = m_profil.getProperty( PROFIL_PROPERTY.RAUHEIT_TYP );
    return "Rauheit " + ((rw == null) ? "unbekannter Typ" : rw.toString());
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paintLegend(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  @Override
  public void paintLegend( final GCWrapper gc )
  {
    final Rectangle clipping = gc.getClipping();
    fillRectangle( gc, clipping );
  }

  @Override
  public IProfilView createLayerPanel( final IProfilEventManager pem, final ProfilViewData viewData )
  {
    return new RauheitenPanel( pem, viewData );
  }

  /**
   * @see IProfilChartLayer#removeYourself()
   */
  public void removeYourself( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.chart.layer.AbstractProfilChartLayer#editProfil(org.eclipse.swt.graphics.Point,
   *      java.lang.Object)
   */
  @Override
  protected void editProfil( Point point, Object data )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilListener#onProfilChanged(org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint,
   *      org.kalypso.model.wspm.core.profil.IProfilChange[])
   */
  @Override
  public void onProfilChanged( ProfilChangeHint hint, IProfilChange[] changes )
  {
    if( !hint.isPointValuesChanged() )
      return;
    final AxisRange valueRange = getValueRange();
    final double max = valueRange.getLogicalTo();
    double maxProfilValue = 0;
    for( IProfilChange change : changes )
    {
      if( change.getPointProperty() == POINT_PROPERTY.RAUHEIT )
      {
        final double newValue = change.getValue();
        if( newValue > maxProfilValue )
          maxProfilValue = newValue;
      }
    }
    final double newMax = 1.05 * maxProfilValue;
    if( Math.abs( maxProfilValue - max ) > max * 0.05 )
      valueRange.setLogicalRange( new LogicalRange( valueRange.getLogicalFrom(), newMax ) );

  }
}
