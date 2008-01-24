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
import java.util.LinkedList;
import java.util.List;

import org.eclipse.jface.resource.ColorRegistry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.changes.ActiveObjectEdit;
import org.kalypso.model.wspm.core.profil.changes.PointMarkerEdit;
import org.kalypso.model.wspm.core.profil.changes.PointMarkerSetPoint;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.changes.ProfileObjectSet;
import org.kalypso.model.wspm.core.profil.util.ProfilObsHelper;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.panel.WehrPanel;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.view.IProfilView;
import org.kalypso.model.wspm.ui.view.ProfilViewData;
import org.kalypso.model.wspm.ui.view.chart.AbstractPolyLineLayer;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;
import org.kalypso.observation.result.IRecord;

import de.belger.swtchart.EditInfo;
import de.belger.swtchart.axis.AxisRange;

/**
 * @author kimwerner
 */
public class WehrBuildingLayer extends AbstractPolyLineLayer
{
  public WehrBuildingLayer( final ProfilChartView pcv )
  {
    super( IWspmTuhhConstants.LAYER_WEHR, "Wehr", pcv, pcv.getDomainRange(), pcv.getValueRangeLeft(), ProfilObsHelper.getPropertyFromId( pcv.getProfil(), new String[] { IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, } ), false, false, false );
    super.setColors( setColor( pcv.getColorRegistry() ) );
  }

  @Override
  public IProfilView createLayerPanel( final IProfilEventManager pem, final ProfilViewData viewData )
  {
    return new WehrPanel( pem, viewData );
  }

  public final void editDevider( final Point point, final IProfilPointMarker devider )
  {
    final IProfilPointMarker activeDevider = devider;

    final IRecord destinationPoint = ProfilUtil.findNearestPoint( getProfil(), screen2logical( point ).getX() );

    final IRecord oldPos = activeDevider.getPoint();
    if( oldPos != destinationPoint )
    {
      final ProfilOperation operation = new ProfilOperation( activeDevider.toString() + " verschieben", getProfilEventManager(), true );
      operation.addChange( new PointMarkerSetPoint( activeDevider, destinationPoint ) );
      operation.addChange( new ActiveObjectEdit( getProfil(), destinationPoint, ProfilObsHelper.getPropertyFromId( destinationPoint, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ) ) );
      new ProfilOperationJob( operation ).schedule();
    }
  }

  @Override
  public final void editProfil( final Point moveTo, final Object data )
  {
    if( data instanceof IProfilPointMarker )
    {
      editDevider( moveTo, (IProfilPointMarker) data );

    }
    else
      super.editProfil( moveTo, data );
  }

  private EditInfo getDeviderInfo( final Point mousePoint )
  {
    if( !getViewData().getMarkerVisibility( IWspmTuhhConstants.MARKER_TYP_WEHR ) )
      return null;
    final AxisRange valueRange = getValueRange();
    final int bottom = valueRange.getScreenFrom() + valueRange.getGapSpace();
    final int top = valueRange.getScreenTo() + valueRange.getGapSpace();
    final IProfilPointMarker[] deviders = getProfil().getPointMarkerFor( ProfilObsHelper.getPropertyFromId( getProfil(), IWspmTuhhConstants.MARKER_TYP_WEHR ) );
    int fieldNr = 0;

    // TODO IProfileObjects now returned as list from IProfile
    final IProfileObject building = getProfil().getProfileObject()[0];
    final Object value = building.getValue( ProfilObsHelper.getPropertyFromId( getProfil().getProfileObject()[0], IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT ) );
    Double leftValue = (Double) building.getValue( ProfilObsHelper.getPropertyFromId( getProfil().getProfileObject()[0], IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT ) );
    for( final IProfilPointMarker devider : deviders )
    {

      final IRecord deviderPos = devider.getPoint();
      final double breite = (Double) deviderPos.getValue( ProfilObsHelper.getPropertyFromId( deviderPos, IWspmTuhhConstants.POINT_PROPERTY_BREITE ) );
      final Point point = logical2screen( new Point2D.Double( breite, (Double) deviderPos.getValue( ProfilObsHelper.getPropertyFromId( deviderPos, IWspmTuhhConstants.POINT_PROPERTY_HOEHE ) ) ) );
      final Rectangle devRect = new Rectangle( point.x - 5, top - 5, 10, bottom - top + 10 );
      final Double rightValue = (Double) devider.getValue();
      if( devRect.contains( mousePoint.x, mousePoint.y ) )
      {
        final String text = String.format( "%s%n%s: %10.4f%n%s: %10.4f", new Object[] { "Wehrparameter", "Feld " + Integer.toString( fieldNr + 1 ), leftValue,
            "Feld " + Integer.toString( fieldNr + 2 ), rightValue } );

        return new EditInfo( this, devRect, devider, text );
      }
      fieldNr++;
      leftValue = rightValue;
    }
    return null;
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractPolyLineLayer#getHoverInfo(org.eclipse.swt.graphics.Point)
   */
  @Override
  public EditInfo getHoverInfo( final Point mousePos )
  {
    final EditInfo info = super.getHoverInfo( mousePos );
    return info == null ? getDeviderInfo( mousePos ) : info;
  }

  @Override
  public List<IRecord> getPoints( )
  {
    return ProfilUtil.getInnerPoints( getProfil(), ProfilObsHelper.getPropertyFromId( getProfil(), IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.chart.layer.AbstractPolyLineLayer#isPointVisible(org.kalypso.model.wspm.core.profil.IProfilPoint)
   */
  @Override
  protected boolean isPointVisible( final IRecord point )
  {
    final IProfil profil = getProfil();
    final LinkedList<IRecord> points = profil.getPoints();
    final int i = points.indexOf( point );
    final IProfilPointMarker[] deviders = profil.getPointMarkerFor( ProfilObsHelper.getPropertyFromId( getProfil(), IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
    if( i < points.indexOf( deviders[0].getPoint() ) )
      return false;
    if( i > points.indexOf( deviders[deviders.length - 1].getPoint() ) )
      return false;
    return true;
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
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractPolyLineLayer#paint(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  @Override
  public void paint( final GCWrapper gc )
  {
    super.paint( gc );
    if( getViewData().getMarkerVisibility( IWspmTuhhConstants.MARKER_TYP_WEHR ) )
      paintDevider( gc );
  }

  private void paintDevider( final GCWrapper gc )
  {
    final IProfilPointMarker[] deviders = getProfil().getPointMarkerFor( ProfilObsHelper.getPropertyFromId( getProfil(), IWspmTuhhConstants.MARKER_TYP_WEHR ) );
    final int bottom = getValueRange().getScreenFrom() + getValueRange().getGapSpace();
    final int top = getValueRange().getScreenTo() + getValueRange().getGapSpace();
    for( final IProfilPointMarker devider : deviders )
    {
      final IRecord point = devider.getPoint();
      final double leftvalue = (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmTuhhConstants.POINT_PROPERTY_BREITE ) );
      final int left = (int) getDomainRange().logical2screen( leftvalue );
      gc.drawLine( left, top, left, bottom );
    }

  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractPolyLineLayer#paintDrag(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper,
   *      org.eclipse.swt.graphics.Point, java.lang.Object)
   */
  @Override
  public void paintDrag( final GCWrapper gc, final Point editing, final Object data )
  {
    if( data instanceof IProfilPointMarker )
    {
      paintDragDevider( gc, editing );
    }
    else
      super.paintDrag( gc, editing, data );

  }

  private void paintDragDevider( final GCWrapper gc, final Point editing )
  {
    gc.setLineStyle( SWT.LINE_DOT );
    gc.setLineWidth( 1 );

    final AxisRange valueRange = getValueRange();
    final int bottom = valueRange.getScreenFrom() + valueRange.getGapSpace();
    final int top = valueRange.getScreenTo() + valueRange.getGapSpace();

    try
    {
      final IRecord destinationPoint = ProfilUtil.findNearestPoint( getProfil(), screen2logical( editing ).getX() );
      final Point destP = logical2screen( new Point2D.Double( (Double) destinationPoint.getValue( ProfilObsHelper.getPropertyFromId( destinationPoint, IWspmTuhhConstants.POINT_PROPERTY_BREITE ) ), (Double) destinationPoint.getValue( ProfilObsHelper.getPropertyFromId( destinationPoint, IWspmTuhhConstants.POINT_PROPERTY_HOEHE ) ) ) );
      gc.drawRectangle( destP.x - 5, top - 5, 10, bottom - top + 10 );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  @Override
  public void paintLegend( final GCWrapper gc )
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
    gc.setForeground( m_colors[0] );
    gc.drawOval( midx - 2, midy - 2, 4, 4 );
    gc.drawLine( left, top, midx, midy );
    gc.drawLine( midx, midy, right, midy );

  }

  public void removeYourself( )
  {
    final IProfilEventManager pem = getProfilEventManager();
    final IProfil profile = pem.getProfil();
    final IProfilPointMarker[] deviders = profile.getPointMarkerFor( ProfilObsHelper.getPropertyFromId( getProfil(), IWspmTuhhConstants.MARKER_TYP_WEHR ) );
    final IProfilChange[] changes = new IProfilChange[deviders.length + 2];
    changes[0] = new ProfileObjectSet( profile, new IProfileObject[] {} );
    changes[1] = new PointPropertyRemove( profile, ProfilObsHelper.getPropertyFromId( profile, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ) );
    for( int i = 0; i < deviders.length; i++ )
    {
      changes[i + 2] = new PointMarkerEdit( deviders[i], null );
    }
    final ProfilOperation operation = new ProfilOperation( "Wehr entfernen", pem, changes, true );
    new ProfilOperationJob( operation ).schedule();
  }

  private final Color[] setColor( final ColorRegistry cr )
  {
    if( !cr.getKeySet().contains( IWspmTuhhConstants.LAYER_WEHR ) )
    {
      cr.put( IWspmTuhhConstants.LAYER_WEHR, new RGB( 0, 128, 0 ) );
    }
    return new Color[] { cr.get( IWspmTuhhConstants.LAYER_WEHR ), cr.get( IWspmTuhhConstants.LAYER_WEHR ) };
  }

  @Override
  public String toString( )
  {
    return "Wehr";
  }
}