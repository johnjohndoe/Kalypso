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

import org.eclipse.swt.graphics.RGB;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.TuhhProfil;
import org.kalypso.model.wspm.ui.view.AbstractLayerStyleProvider;

import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.model.style.IPointStyle;
import de.openali.odysseus.chart.framework.model.style.impl.LineStyle;
import de.openali.odysseus.chart.framework.model.style.impl.PointStyle;

/**
 * @author kimwerner
 */
public class LayerStyleProviderTuhh extends AbstractLayerStyleProvider
{
  private static final String POINT = "_POINT"; //$NON-NLS-1$

  private static final String LINE = "_LINE"; //$NON-NLS-1$

  @Override
  protected void createStyles( )
  {
    // TODO: read styles from *.kod file
    createPointMarkerStyles();
    createCrossSectionStyles();
    createRoughnessStyles();
    createBridgeStyles();
    createWeirStyles();
    createVegetationStyle();
    createWasserspiegel2dStyle();
    createWasserspiegelStyle();
  }

  private void createWasserspiegel2dStyle( )
  {
    final ILineStyle ls = getStyleFor( IWspmTuhhConstants.LAYER_WASSERSPIEGEL2D + LINE, LineStyle.class ); //$NON-NLS-1$
    ls.setColor( new RGB( 153, 217, 234 ) );
  }

  private void createWasserspiegelStyle( )
  {
    final ILineStyle ls = getStyleFor( IWspmTuhhConstants.LAYER_WASSERSPIEGEL + LINE, LineStyle.class ); //$NON-NLS-1$
    ls.setColor( new RGB( 153, 217, 234 ) );
  }

  private void createPointMarkerStyles( )
  {
    final IProfilPointMarkerProvider markerProvider = KalypsoModelWspmCoreExtensions.getMarkerProviders( TuhhProfil.PROFIL_TYPE );

    final ILineStyle lsT = getStyleFor( IWspmTuhhConstants.LAYER_DEVIDER + "_" + IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE + LINE, LineStyle.class ); //$NON-NLS-1$
    lsT.setColor( markerProvider.getColorFor(  IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
    lsT.setWidth( 2 );

    final ILineStyle lsD = lsT.clone();
    lsD.setColor( markerProvider.getColorFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) );
    addStyle( IWspmTuhhConstants.LAYER_DEVIDER + "_" + IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE + LINE, lsD ); //$NON-NLS-1$

    final ILineStyle lsB = lsT.clone();
    lsB.setColor( markerProvider.getColorFor(  IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) );
    addStyle( IWspmTuhhConstants.LAYER_DEVIDER + "_" + IWspmTuhhConstants.MARKER_TYP_BORDVOLL + LINE, lsB ); //$NON-NLS-1$
  }

  private void createWeirStyles( )
  {
    final IProfilPointMarkerProvider markerProvider = KalypsoModelWspmCoreExtensions.getMarkerProviders( TuhhProfil.PROFIL_TYPE );
    final RGB col = markerProvider.getColorFor( IWspmTuhhConstants.MARKER_TYP_WEHR );

    final ILineStyle ls = getStyleFor(  IWspmTuhhConstants.LAYER_DEVIDER + "_" + IWspmTuhhConstants.MARKER_TYP_WEHR + LINE, LineStyle.class ); //$NON-NLS-1$
    ls.setColor( col );
    addStyle( IWspmTuhhConstants.LAYER_WEHR + "_" + IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR + LINE, ls.clone() ); //$NON-NLS-1$
  }

  private void createCrossSectionStyles( )
  {
    final ILineStyle ls = getStyleFor( IWspmTuhhConstants.LAYER_GELAENDE + LINE, LineStyle.class ); //$NON-NLS-1$
    ls.setColor( new RGB( 255, 150, 0 ) );
  }

  private void createVegetationStyle( )
  {
    getStyleFor( IWspmTuhhConstants.LAYER_BEWUCHS + LINE, LineStyle.class ).setColor( new RGB( 0, 255, 0 ) ); //$NON-NLS-1$
  }

  private void createBridgeStyles( )
  {
    final ILineStyle lsO = getStyleFor( IWspmTuhhConstants.LAYER_BRUECKE + "_" + IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE + LINE, LineStyle.class ); //$NON-NLS-1$
    lsO.setColor( new RGB( 0, 128, 0 ) );

    final ILineStyle lsU = lsO.clone();
    lsU.setColor( new RGB( 0, 128, 179 ) );
    addStyle( IWspmTuhhConstants.LAYER_BRUECKE + "_" + IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE + LINE, lsU ); //$NON-NLS-1$
  }

  private void createRoughnessStyles( )
  {
    final IPointStyle psKS = getStyleFor( IWspmTuhhConstants.LAYER_RAUHEIT + POINT, PointStyle.class ); //$NON-NLS-1$
    psKS.getStroke().setColor( new RGB( 0, 0, 0 ) );
    psKS.setInlineColor( new RGB( 0, 0, 0 ) );
    psKS.setAlpha( 50 );

  }

}
