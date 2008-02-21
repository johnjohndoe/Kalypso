/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.changes.ActiveObjectEdit;
import org.kalypso.model.wspm.core.profil.changes.PointMarkerSetPoint;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.util.ProfilObsHelper;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.view.IProfilView;
import org.kalypso.model.wspm.ui.view.ProfilViewData;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilChartLayer;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;
import org.kalypso.model.wspm.ui.view.chart.color.IProfilColorSet;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

import de.belger.swtchart.EditInfo;
import de.belger.swtchart.axis.AxisRange;

/**
 * @author gernot
 */
public class TrennerLayer extends AbstractProfilChartLayer
{
  private final ColorRegistry m_colorRegistry;

  public TrennerLayer( final ProfilChartView pcv )
  {
    super( IWspmTuhhConstants.LAYER_DEVIDER, pcv, pcv.getDomainRange(), pcv.getValueRangeLeft(), "Flieﬂzonen" );
    m_colorRegistry = pcv.getColorRegistry();
    if( !m_colorRegistry.getKeySet().contains( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) )
      m_colorRegistry.put( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE, new RGB( 0, 180, 0 ) );
    if( !m_colorRegistry.getKeySet().contains( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) )
      m_colorRegistry.put( IWspmTuhhConstants.MARKER_TYP_BORDVOLL, new RGB( 200, 50, 0 ) );
    if( !m_colorRegistry.getKeySet().contains( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) )
      m_colorRegistry.put( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, new RGB( 0, 0, 255 ) );

  }

  @Override
  public final IProfilView createLayerPanel( final IProfil profile, final ProfilViewData viewData )
  {
    return new TrennerPanel( profile, viewData );
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#edit(org.eclipse.swt.graphics.Point, java.lang.Object)
   */
  @Override
  public void editProfil( final Point point, final Object data )
  {
    final IProfilPointMarker activeDevider = (IProfilPointMarker) data;

    final IRecord destinationPoint = ProfilUtil.findNearestPoint( getProfil(), screen2logical( point ).getX() );
    final int index = getProfil().indexOfProperty( activeDevider.getId() );

    final IRecord oldPos = activeDevider.getPoint();
    if( (oldPos != destinationPoint) && (destinationPoint.getValue( index ) == activeDevider.getId().getDefaultValue()) )
    {
      final ProfilOperation operation = new ProfilOperation( activeDevider.toString() + " verschieben", getProfil(), true );
      operation.addChange( new PointMarkerSetPoint( activeDevider, destinationPoint ) );

      final String id = activeDevider.getId().getId() == IWspmTuhhConstants.MARKER_TYP_WEHR ? IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR : null;

      if( id == IWspmTuhhConstants.MARKER_TYP_WEHR )
        operation.addChange( new ActiveObjectEdit( getProfil(), destinationPoint, ProfilObsHelper.getPropertyFromId( activeDevider.getPoint(), IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ) ) );
      else
        operation.addChange( new ActiveObjectEdit( getProfil(), destinationPoint, null ) );

      new ProfilOperationJob( operation ).schedule();
    }
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getBounds()
   */
  public Rectangle2D getBounds( )
  {
    try
    {
      final int iBreite = getProfil().indexOfProperty( IWspmTuhhConstants.POINT_PROPERTY_BREITE );
      final int iHoehe = getProfil().indexOfProperty( IWspmTuhhConstants.POINT_PROPERTY_HOEHE );
      final IComponent[] markerTypes = getProfil().getPointMarkerTypes();
      for( final IComponent markerType : markerTypes )
      {
        final IProfilPointMarker[] deviders = getProfil().getPointMarkerFor( markerType );
        if( deviders.length < 2 )
          return MINIMAL_RECT;

        final double left = (Double) deviders[0].getPoint().getValue( iBreite );
        final double right = (Double) deviders[deviders.length - 1].getPoint().getValue( iBreite );
        final double top = (Double) deviders[0].getPoint().getValue( iHoehe );

        return new Rectangle2D.Double( left, top, right - left, 0 );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return null;
    }
    return null;
  }

  public String getDeviderInfo( final IProfilPointMarker devider )
  {
    try
    {
      final int iBreite = getProfil().indexOfProperty( IWspmTuhhConstants.POINT_PROPERTY_BREITE );
      if( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE.equals( devider.getId().getId() ) )
      {
        final Boolean position = (Boolean) devider.getIntepretedValue();
        final boolean pos = position == null ? false : position;

        return String.format( "%s %s%n%10.4f [m]", new Object[] { devider.getId().getName(), pos ? "Bˆschungsfuss" : "Vorland", devider.getPoint().getValue( iBreite ) } );
      }
      else
        return String.format( "%s%n%10.4f [m]", new Object[] { devider.getId().getName(), devider.getPoint().getValue( iBreite ) } );
    }
    catch( final Exception e )
    {
      return "";
    }

  }

  private EditInfo getDeviderInfo( final Point mousePoint, final IProfilPointMarker[] deviders, final int topOffset )
  {
    final AxisRange valueRange = getValueRange();
    final int bottom = valueRange.getScreenFrom() + valueRange.getGapSpace();
    final int top = valueRange.getScreenTo() + valueRange.getGapSpace() + topOffset;
    final int iBreite = getProfil().indexOfProperty( IWspmTuhhConstants.POINT_PROPERTY_BREITE );
    for( final IProfilPointMarker devider : deviders )
    {

      final IRecord deviderPos = devider.getPoint();
      final double breite = (Double) deviderPos.getValue( iBreite );
      final Point point = logical2screen( new Point2D.Double( breite, (Double) deviderPos.getValue( iBreite ) ) );
      final Rectangle devRect = new Rectangle( point.x - 5, top - 5, 10, bottom - top + 10 );
      final Rectangle pointRect = new Rectangle( point.x - 5, point.y - 5, 10, 10 );
      if( pointRect.contains( mousePoint.x, mousePoint.y ) )
        return null;
      if( devRect.contains( mousePoint.x, mousePoint.y ) )
        return new EditInfo( this, devRect, devider, getDeviderInfo( devider ) );
    }
    return null;
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getHoverInfo(org.eclipse.swt.graphics.Point)
   */
  @Override
  public EditInfo getHoverInfo( final Point point )
  {
    EditInfo info = null;
    final IProfil profil = getProfil();

    if( getViewData().getMarkerVisibility( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) )
    {

      info = getDeviderInfo( point, profil.getPointMarkerFor( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) ), 20 );
      if( info != null )
        return info;
    }
    if( getViewData().getMarkerVisibility( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) )
    {

      info = getDeviderInfo( point, profil.getPointMarkerFor( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) ), 40 );
      if( info != null )
        return info;
    }
    if( getViewData().getMarkerVisibility( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) )
    {

      info = getDeviderInfo( point, profil.getPointMarkerFor( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) ), 0 );
      if( info != null )
        return info;
    }

    return null;
  }

  /**
   * @see com.bce.eind.core.profil.IProfilListener#onProfilChanged(com.bce.eind.core.profil.changes.ProfilChangeHint,
   *      com.bce.eind.core.profil.IProfilChange[])
   */
  @Override
  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {

  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paint(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  public void paint( final GCWrapper gc )
  {
    gc.setLineWidth( 3 );
    gc.setLineStyle( SWT.LINE_SOLID );
    final int top = getValueRange().getScreenTo() + getValueRange().getGapSpace();
    if( getViewData().getMarkerVisibility( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) )
      paintDevider( gc, ProfilObsHelper.getPropertyFromId( getProfil(), IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ), top, true );
    if( getViewData().getMarkerVisibility( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) )
      paintDevider( gc, ProfilObsHelper.getPropertyFromId( getProfil(), IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ), top + 20, false );

    if( getViewData().getMarkerVisibility( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) )
      paintDevider( gc, ProfilObsHelper.getPropertyFromId( getProfil(), IWspmTuhhConstants.MARKER_TYP_BORDVOLL ), top + 40, false );
  }

  public void paintDevider( final GCWrapper gc, final IComponent devider, final int top, final boolean isClosed )
  {
    if( devider == null )
      return;

    final IProfilPointMarker[] deviders = getProfil().getPointMarkerFor( devider );
    final int bottom = getValueRange().getScreenFrom() + getValueRange().getGapSpace();
    final int iBreite = getProfil().indexOfProperty( IWspmTuhhConstants.POINT_PROPERTY_BREITE );

    gc.setForeground( m_colorRegistry.get( devider.getId() ) );
    for( final IProfilPointMarker d : deviders )
    {
      final IRecord point = d.getPoint();
      final double leftvalue = (Double) point.getValue( iBreite );
      final int left = (int) getDomainRange().logical2screen( leftvalue );
      gc.drawLine( left, top, left, bottom );
    }
    if( isClosed && deviders.length > 1 )
    {
      final int l = (int) getDomainRange().logical2screen( (Double) deviders[0].getPoint().getValue( iBreite ) );
      final int r = (int) getDomainRange().logical2screen( (Double) deviders[deviders.length - 1].getPoint().getValue( iBreite ) );
      gc.drawLine( l, top, r, top );
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
    if( hoverData instanceof IProfilPointMarker )
    {
      final IComponent cDevider = ((IProfilPointMarker) hoverData).getId();
      final int iDevider = getProfil().indexOfProperty( cDevider );
      final AxisRange valueRange = getValueRange();
      final int bottom = valueRange.getScreenFrom() + valueRange.getGapSpace();
      int top = valueRange.getScreenTo() + valueRange.getGapSpace();
      if( cDevider.getId().equals( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) )
        top = top + 20;
      if( cDevider.getId().equals( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) )
        top = top + 40;
      try
      {
        final int iBreite = getProfil().indexOfProperty( IWspmTuhhConstants.POINT_PROPERTY_BREITE );
        final int iHoehe = getProfil().indexOfProperty( IWspmTuhhConstants.POINT_PROPERTY_HOEHE );
        final IRecord destinationPoint = ProfilUtil.findNearestPoint( getProfil(), screen2logical( editing ).getX() );
        if( destinationPoint.getValue( iDevider ) == cDevider.getDefaultValue() )
        {
          final Point destP = logical2screen( new Point2D.Double( (Double) destinationPoint.getValue( iBreite ), (Double) destinationPoint.getValue( iHoehe ) ) );
          gc.drawRectangle( destP.x - 5, top - 5, 10, bottom - top + 10 );
        }
        else
        {
          gc.drawRectangle( editing.x - 5, top - 5, 10, bottom - top + 10 );
        }
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
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
   * @see java.lang.Object#toString()
   */

  public void removeYourself( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.chart.layer.AbstractProfilChartLayer#setActivePoint(java.lang.Object)
   */
  @Override
  public void setActivePoint( final Object data )
  {
    if( data instanceof IProfilPointMarker )
    {
      final IProfilPointMarker devider = (IProfilPointMarker) data;
      final IRecord activePoint = devider.getPoint();
      final ProfilOperation operation = new ProfilOperation( "", getProfil(), new ActiveObjectEdit( getProfil(), activePoint, null ), true );
      final IStatus status = operation.execute( new NullProgressMonitor(), null );
      operation.dispose();
      if( !status.isOK() )
        KalypsoModelWspmUIPlugin.getDefault().getLog().log( status );
    }
  }

  @Override
  public String toString( )
  {
    return "Flieﬂzonen";
  }
}