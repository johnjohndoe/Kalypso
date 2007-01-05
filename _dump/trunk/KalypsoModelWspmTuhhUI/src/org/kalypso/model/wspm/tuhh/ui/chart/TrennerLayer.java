/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.tuhh.ui.chart;

import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.resource.ColorRegistry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilConstants;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.changes.ActiveObjectEdit;
import org.kalypso.model.wspm.core.profil.changes.DeviderMove;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.view.IProfilView;
import org.kalypso.model.wspm.ui.view.ProfilViewData;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilChartLayer;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;
import org.kalypso.model.wspm.ui.view.chart.color.IProfilColorSet;

import de.belger.swtchart.EditInfo;
import de.belger.swtchart.axis.AxisRange;

/**
 * @author gernot
 */
public class TrennerLayer extends AbstractProfilChartLayer
{
  /**
   * @see org.kalypso.model.wspm.ui.profil.view.chart.layer.AbstractProfilChartLayer#setActivePoint(java.lang.Object)
   */
  @Override
  public void setActivePoint( Object data )
  {
    if( data instanceof IProfilDevider )
    {
      final IProfilDevider devider = (IProfilDevider) data;
      final IProfilPoint activePoint = devider.getPoint();
      final ProfilOperation operation = new ProfilOperation( "", getProfilEventManager(), new ActiveObjectEdit( getProfil(), activePoint, null ), true );
      final IStatus status = operation.execute( new NullProgressMonitor(), null );
      operation.dispose();
      if( !status.isOK() )
        KalypsoModelWspmUIPlugin.getDefault().getLog().log( status );
    }
  }

  private ColorRegistry m_colorRegistry;

  private enum DEVIDER
  {

    Durchstroemte(IProfilConstants.DEVIDER_TYP_DURCHSTROEMTE, IProfilColorSet.COLOUR_DURCHSTROEMTE_BEREICHE, true, 0, "Durchströmter Bereich"),
    Fliesszone(IProfilConstants.DEVIDER_TYP_TRENNFLAECHE, IProfilColorSet.COLOUR_TRENNFLAECHEN, false, 20, "Trennflächen"),
    Bordvoll(IProfilConstants.DEVIDER_TYP_BORDVOLL, IProfilColorSet.COLOUR_BORDVOLLPUNKTE, false, 40, "Bordvollpunkt"),
    Wehr(IProfilConstants.DEVIDER_TYP_WEHR, IProfilColorSet.COLOUR_WEHR, false, 60, "Wehrfeldtrenner");

    private DEVIDER( final String deviderTyp, final String colorKey, final boolean isclosed, final int topOffset, final String label )
    {
      m_colorKey = colorKey;

      m_isclosed = isclosed;
      m_label = label;
      m_topOffset = topOffset;
      m_deviderTyp = deviderTyp;

    }

    public final static DEVIDER getDevider( String typ )
    {
      if( IProfilConstants.DEVIDER_TYP_TRENNFLAECHE.equals( typ ) )
        return Fliesszone;
      else if( IProfilConstants.DEVIDER_TYP_WEHR.equals( typ ) )
        return Wehr;
      else if( IProfilConstants.DEVIDER_TYP_BORDVOLL.equals( typ ) )
        return Bordvoll;
      else if( IProfilConstants.DEVIDER_TYP_DURCHSTROEMTE.equals( typ ) )
        return Durchstroemte;
      else
        return null;
    }

    public final boolean isClosed( )
    {
      return m_isclosed;
    }

    public final int getTopOffset( )
    {
      return m_topOffset;
    }

    public final String getColorKey( )
    {
      return m_colorKey;
    }

    private final String m_colorKey;

    private final String m_deviderTyp;

    private final boolean m_isclosed;

    private final int m_topOffset;

    private final String m_label;

    public String getDeviderTyp( )
    {
      return m_deviderTyp;
    }

    public String getHoverInfo( IProfilDevider devider, int fieldNr )
    {
      try
      {

        if( m_deviderTyp.compareTo( IProfilConstants.DEVIDER_TYP_BORDVOLL ) == 0 )
          return String.format( "%s%n%10.4f [m]", new Object[] { m_label, devider.getPoint().getValueFor( IProfilPoint.POINT_PROPERTY.BREITE ) } );
        else if( m_deviderTyp.compareTo( IProfilConstants.DEVIDER_TYP_DURCHSTROEMTE ) == 0 )
          return String.format( "%s%n%10.4f [m]", new Object[] { m_label, devider.getPoint().getValueFor( IProfilPoint.POINT_PROPERTY.BREITE ) } );
        else if( m_deviderTyp.compareTo( IProfilConstants.DEVIDER_TYP_WEHR ) == 0 )
          return String.format( "%s%n%s%n%s: %10.4f", new Object[] { m_label, "Wehrparameter", "Feld " + Integer.toString( fieldNr + 1 ),
              (Double) devider.getValueFor( IProfilDevider.DEVIDER_PROPERTY.BEIWERT ) } );
        else if( m_deviderTyp.compareTo( IProfilConstants.DEVIDER_TYP_TRENNFLAECHE ) == 0 )
        {
          final Boolean position = (Boolean) devider.getValueFor( IProfilDevider.DEVIDER_PROPERTY.BOESCHUNG );
          final boolean pos = position == null ? false : position;

          return String.format( "%s%n%s%n%10.4f %s", new Object[] { m_label, pos ? "Böschungsfuss" : "Vorland", devider.getPoint().getValueFor( IProfilPoint.POINT_PROPERTY.BREITE ), "[m]" } );
        }

      }
      catch( ProfilDataException e )
      {
        return "";
      }
      return "";
    }

    public final String getLabel( )
    {
      return m_label;
    }
  }

  public TrennerLayer( final ProfilChartView pvp, final AxisRange domainRange, final AxisRange valueRange, final ColorRegistry colorRegistry )
  {
    super( pvp, domainRange, valueRange );
    m_colorRegistry = colorRegistry;

  }

  @Override
  public String toString( )
  {
    return "Fließzonen";
  }

  @Override
  public final IProfilView createLayerPanel( final IProfilEventManager pem, final ProfilViewData viewData )
  {
    return new TrennerPanel( pem, viewData );
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paint(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  public void paint( final GCWrapper gc )
  {
    try
    {
      final IProfil m_profil = getProfil();
      final int bottom = getValueRange().getScreenFrom() + getValueRange().getGapSpace();
      final int top = getValueRange().getScreenTo() + getValueRange().getGapSpace();// (int) maxscreen;
      gc.setLineWidth( 3 );
      gc.setLineStyle( SWT.LINE_SOLID );
      for( DEVIDER dev : DEVIDER.values() )
      {
        if( getViewData().getDeviderVisibility( dev.getDeviderTyp() ) )
        {
          final IProfilDevider[] deviders = m_profil.getDevider( dev.getDeviderTyp() );

          gc.setForeground( m_colorRegistry.get( dev.getColorKey() ) );
          for( IProfilDevider devider : deviders )
          {
            final IProfilPoint point = devider.getPoint();
            final double leftvalue = point.getValueFor( POINT_PROPERTY.BREITE );
            final int left = (int) getDomainRange().logical2screen( leftvalue );
            gc.drawLine( left, top + dev.getTopOffset(), left, bottom );
          }
          if( dev.isClosed() )
          {
            final int l = (int) getDomainRange().logical2screen( deviders[0].getPoint().getValueFor( POINT_PROPERTY.BREITE ) );
            final int r = (int) getDomainRange().logical2screen( deviders[deviders.length - 1].getPoint().getValueFor( POINT_PROPERTY.BREITE ) );
            gc.drawLine( l, top + dev.getTopOffset(), r, top + dev.getTopOffset() );
          }
        }
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getBounds()
   */
  public Rectangle2D getBounds( )
  {
    try
    {
      final IProfilDevider[] deviders = getProfil().getDevider();
      if( deviders.length < 2 )
        return MINIMAL_RECT;

      final double left = deviders[0].getPoint().getValueFor( POINT_PROPERTY.BREITE );
      final double right = deviders[deviders.length - 1].getPoint().getValueFor( POINT_PROPERTY.BREITE );
      final double top = deviders[0].getPoint().getValueFor( POINT_PROPERTY.HOEHE );

      return new Rectangle2D.Double( left, top, right - left, 0 );
    }
    catch( final ProfilDataException e )
    {
      e.printStackTrace();

      return null;
    }
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paintDrag(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper,
   *      org.eclipse.swt.graphics.Point, java.lang.Object)
   */
  @Override
  public void paintDrag( final GCWrapper gc, final Point editing, final Object hoverData )
  {
    gc.setLineStyle( SWT.LINE_DOT );
    gc.setLineWidth( 1 );
    gc.setForeground( m_colorRegistry.get( IProfilColorSet.COLOUR_AXIS_FOREGROUND ) );
    if( hoverData instanceof IProfilDevider )
    {
      final DEVIDER dev = DEVIDER.getDevider( ((IProfilDevider) hoverData).getTyp() );
      final AxisRange valueRange = getValueRange();
      final int bottom = valueRange.getScreenFrom() + valueRange.getGapSpace();
      final int top = valueRange.getScreenTo() + valueRange.getGapSpace() + dev.getTopOffset();
      try
      {
        final IProfilPoint destinationPoint = ProfilUtil.findNearestPoint( getProfil(), screen2logical( editing ).getX() );
        final Point destP = logical2screen( new Point2D.Double( destinationPoint.getValueFor( POINT_PROPERTY.BREITE ), destinationPoint.getValueFor( POINT_PROPERTY.HOEHE ) ) );
        gc.drawRectangle( destP.x - 5, top - 5, 10, bottom - top + 10 );
      }
      catch( ProfilDataException e )
      {
        e.printStackTrace();
      }
    }
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#edit(org.eclipse.swt.graphics.Point, java.lang.Object)
   */
  @Override
  public void editProfil( final Point point, final Object data )
  {
    final IProfilDevider activeDevider = (IProfilDevider) data;

    final IProfilPoint destinationPoint = ProfilUtil.findNearestPoint( getProfil(), screen2logical( point ).getX() );

    final IProfilPoint oldPos = activeDevider.getPoint();
    if( oldPos != destinationPoint )
    {
      final ProfilOperation operation = new ProfilOperation( activeDevider.toString() + " verschieben", getProfilEventManager(), true );
      operation.addChange( new DeviderMove( activeDevider, destinationPoint ) );
      operation.addChange( new ActiveObjectEdit( getProfil(), destinationPoint, (activeDevider.getTyp() == IProfilConstants.DEVIDER_TYP_WEHR ? POINT_PROPERTY.OBERKANTEWEHR : null) ) );
      new ProfilOperationJob( operation ).schedule();
    }
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paintLegend(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  @Override
  public void paintLegend( final GCWrapper gc )
  {
    final Rectangle clipping = gc.getClipping();

    final int left = clipping.x;
    final int top = clipping.y;
    final int right = clipping.x + clipping.width;
    final int bottom = clipping.y + clipping.width;
    final int midx = (left + right) / 2;

    gc.drawLine( midx, top, midx, bottom );
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getHoverInfo(org.eclipse.swt.graphics.Point)
   */
  @Override
  public EditInfo getHoverInfo( final Point point )
  {

    EditInfo info = null;
    final IProfil profil = getProfil();
    final DEVIDER[] devs = DEVIDER.values();
    for( int i = devs.length; i > 0; i-- )
    {
      final DEVIDER dev = devs[i - 1];
      final String devTyp = dev.getDeviderTyp();
      final IProfilDevider[] deviders = profil.getDevider( devTyp );

      if( getViewData().getDeviderVisibility( devTyp ) )
      {
        int pos = 0;
        for( IProfilDevider devider : deviders )
        {
          try
          {

            info = getDeviderInfo( point, devider, dev, pos );
            pos++;
            if( info != null )
              return info;
          }
          catch( ProfilDataException e )
          {
            return null;
          }
        }

      }
    }
    return null;
  }

  private EditInfo getDeviderInfo( final Point mousePoint, final IProfilDevider devider, final DEVIDER dev, int position ) throws ProfilDataException
  {

    if( devider == null )
      return null;
    final AxisRange valueRange = getValueRange();
    final int bottom = valueRange.getScreenFrom() + valueRange.getGapSpace();
    final int top = valueRange.getScreenTo() + valueRange.getGapSpace() + dev.getTopOffset();
    final IProfilPoint deviderPos = devider.getPoint();
    final double breite = deviderPos.getValueFor( POINT_PROPERTY.BREITE );
    final Point point = logical2screen( new Point2D.Double( breite, deviderPos.getValueFor( POINT_PROPERTY.HOEHE ) ) );
    final Rectangle devRect = new Rectangle( point.x - 5, top - 5, 10, bottom - top + 10 );
    final Rectangle pointRect = new Rectangle( point.x - 5, point.y - 5, 10, 10 );
    if( pointRect.contains( mousePoint.x, mousePoint.y ) )
      return null;
    if( devRect.contains( mousePoint.x, mousePoint.y ) )
    {
      return new EditInfo( this, devRect, devider, dev.getHoverInfo( devider, position + 1 ) );
    }
    return null;
  }

  /**
   * @see java.lang.Object#toString()
   */

  public void removeYourself( )
  {
    throw new UnsupportedOperationException();
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
