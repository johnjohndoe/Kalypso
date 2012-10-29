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

import org.eclipse.swt.graphics.RGB;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfilePointMarkerProvider;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.TuhhProfil;
import org.kalypso.model.wspm.ui.view.AbstractLayerStyleProvider;

import de.openali.odysseus.chart.framework.model.style.IAreaStyle;
import de.openali.odysseus.chart.framework.model.style.IFill;
import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.model.style.IStyleConstants.LINECAP;
import de.openali.odysseus.chart.framework.model.style.IStyleConstants.LINEJOIN;
import de.openali.odysseus.chart.framework.model.style.impl.ColorFill;
import de.openali.odysseus.chart.framework.model.style.impl.LineStyle;

/**
 * @author kimwerner
 */
public class LayerStyleProviderTuhh extends AbstractLayerStyleProvider
{
  @Override
  public void createStyles( )
  {
    createPointMarkerStyles();
    createCrossSectionStyles();
    createRoughnessStyles();
    createBridgeStyles();
    createWeirStyles();
    createVegetationStyle();
    createWasserspiegel2dStyle();
    createWasserspiegelStyle();
    createTubeStyle();
  }

  private void createWasserspiegel2dStyle( )
  {
    final ILineStyle ls = getStyleFor( IWspmConstants.LAYER_WASSERSPIEGEL2D + LINE, LineStyle.class ); //$NON-NLS-1$
    ls.setColor( new RGB( 153, 217, 234 ) );
  }

  private void createWasserspiegelStyle( )
  {
    final ILineStyle ls = getStyleFor( IWspmConstants.LAYER_WASSERSPIEGEL + LINE, LineStyle.class ); //$NON-NLS-1$
    ls.setColor( new RGB( 0x00, 0x00, 0xff ) );

    final ILineStyle fixation = getStyleFor( IWspmConstants.LAYER_WASSERSPIEGEL_FIXIERUNG + LINE, LineStyle.class ); //$NON-NLS-1$
    fixation.setColor( new RGB( 0x00, 0xCC, 0xFF ) );
  }

  private void createPointMarkerStyles( )
  {
    final IProfilePointMarkerProvider markerProvider = KalypsoModelWspmCoreExtensions.getMarkerProviders( TuhhProfil.PROFIL_TYPE );

    final ILineStyle lsT = getStyleFor( IWspmTuhhConstants.LAYER_DEVIDER + "_" + IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE + LINE, LineStyle.class ); //$NON-NLS-1$
    lsT.setColor( markerProvider.getColorFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
    lsT.setWidth( 2 );

    final ILineStyle lsD = lsT.clone();
    lsD.setColor( markerProvider.getColorFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) );
    addStyle( IWspmTuhhConstants.LAYER_DEVIDER + "_" + IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE + LINE, lsD ); //$NON-NLS-1$

    final ILineStyle lsB = lsT.clone();
    lsB.setColor( markerProvider.getColorFor( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) );
    addStyle( IWspmTuhhConstants.LAYER_DEVIDER + "_" + IWspmTuhhConstants.MARKER_TYP_BORDVOLL + LINE, lsB ); //$NON-NLS-1$
  }

  private void createWeirStyles( )
  {
    final IProfilePointMarkerProvider markerProvider = KalypsoModelWspmCoreExtensions.getMarkerProviders( TuhhProfil.PROFIL_TYPE );
    final RGB col = markerProvider.getColorFor( IWspmTuhhConstants.MARKER_TYP_WEHR );

    final ILineStyle ls = getStyleFor( IWspmTuhhConstants.LAYER_DEVIDER + "_" + IWspmTuhhConstants.MARKER_TYP_WEHR + LINE, LineStyle.class ); //$NON-NLS-1$
    ls.setColor( col );
    addStyle( IWspmTuhhConstants.LAYER_WEHR_OK + LINE, ls.clone() ); //$NON-NLS-1$
  }

  private void createCrossSectionStyles( )
  {
    final ILineStyle ls = getStyleFor( IWspmConstants.LAYER_GELAENDE + LINE, LineStyle.class ); //$NON-NLS-1$
    ls.setColor( new RGB( 255, 150, 0 ) );

    /* station lines */
    final ILineStyle stationLineStyle = getStyleFor( IWspmConstants.LAYER_STATION_LINES + LINE, LineStyle.class ); //$NON-NLS-1$
    stationLineStyle.setColor( new RGB( 0, 0, 0 ) );
    stationLineStyle.setWidth( 1 );
    stationLineStyle.setDash( 0, new float[] { 1, 1, 1 } );
  }

  private void createVegetationStyle( )
  {
    getStyleFor( IWspmTuhhConstants.LAYER_BEWUCHS + LINE, LineStyle.class ).setColor( new RGB( 0, 255, 0 ) ); //$NON-NLS-1$
  }

  private void createBridgeStyles( )
  {
    final ILineStyle lsO = getStyleFor( IWspmTuhhConstants.LAYER_BRUECKE_OK + LINE, LineStyle.class ); //$NON-NLS-1$
    lsO.setColor( new RGB( 0, 128, 0 ) );

    final ILineStyle lsU = lsO.clone();
    lsU.setColor( new RGB( 0, 128, 179 ) );
    addStyle( IWspmTuhhConstants.LAYER_BRUECKE_UK + LINE, lsU ); //$NON-NLS-1$
  }

  private void createRoughnessStyles( )
  {
    /* ks */
    final IAreaStyle ksStyle = getStyleFor( IWspmTuhhConstants.LAYER_RAUHEIT_KS + AREA, IAreaStyle.class );
    ksStyle.setAlpha( 50 );

    ksStyle.getStroke().setColor( new RGB( 0, 0, 0 ) );
    ksStyle.getStroke().setAlpha( 75 );

    ksStyle.setFill( new ColorFill( new RGB( 137, 62, 16 ) ) );

    /* kst */
    final IAreaStyle kstStyle = getStyleFor( IWspmTuhhConstants.LAYER_RAUHEIT_KST + AREA, IAreaStyle.class );
    kstStyle.setAlpha( 50 );

    kstStyle.getStroke().setColor( new RGB( 0, 0, 0 ) );
    kstStyle.getStroke().setAlpha( 75 );

    kstStyle.setFill( new ColorFill( new RGB( 137, 62, 16 ) ) );
  }

  private void createTubeStyle( )
  {
    final IAreaStyle style = getStyleFor( IWspmTuhhConstants.LAYER_TUBES + AREA, IAreaStyle.class ); //$NON-NLS-1$

    final IFill fill = new ColorFill( new RGB( 255, 255, 100 ) );
    final ILineStyle stroke = new LineStyle( 2, new RGB( 0, 0, 0 ), 255, 0f, null, LINEJOIN.ROUND, LINECAP.ROUND, 1, true );

    // / FIXME: ugly, styles should be immutable -> first create styles, then register
    style.setFill( fill );
    style.setStroke( stroke );
    style.setAlpha( 128 );
  }
}